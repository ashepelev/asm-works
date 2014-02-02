.386
TERMINALSYMBOL  EQU '$'                 ;#define TERMINALSYMBOL '$'
DATA SEGMENT USE16 PUBLIC 'data'
string WORD 5 dup(0)
emptyString byte 0Dh,0Ah,TERMINALSYMBOL
inputm     DB 'Enter please M: $'
inputn     DB 'Enter please N: $'
allocres    DB 'Allocated mem: $'
noposstr DB 'No positive string$'
m       WORD 0 ; кол-во строк
n       WORD 0 ; кол-во столбцов
mptr WORD 0
DATA ENDS

STACK SEGMENT STACK 'stack'
 db  1024 dup (0)                       ;db - data byte; const
STACK ENDS


CODE SEGMENT USE16 PUBLIC 'code'

strlen PROC PASCAL, strBuffer:ptr byte
	push di
	mov di, strBuffer       ; address
	mov al, TERMINALSYMBOL  ; symbol to find
	mov cx, 0ffffh          ; max string lenght
	cld                     ; установить правильное направление : DF = 0
	repnz scasb             ; REPeat string operation. Scan string
	; scasb - element from es:edi/di - eax/ax/al and setting flag
	;         then
	;         (edi/di)++ if DF = 0
	;         (edi/di)-- if DF = 1
	; repnz - Analizing cx and zf
	;         cx <> 0 or zf = 0 then scasb and cx=cx-1
	;         cx = 0  or zf <> 0 exit from cycle
	jnz   bad_string        ; if not find goto bad_string
	neg cx                  ; inverse sign
	sub cx,2                ;
bad_string:
	mov ax, cx
	pop si
	ret
strlen ENDP

atoi PROC STDCALL,  strbuff:PTR BYTE
		LOCAL tmp:WORD, strsize:WORD
		push si         ; saving registrs
		push cx
		push bx

		push strbuff    ; push for function call
		call strlen
		mov strsize, ax ; strsize = strlen(strbuff)
		mov si, strbuff ; si = strbuff
		mov tmp, 10     ; tmp = 10
		xor bx,bx       ; bx = 0
		xor ax,ax       ; ax = 0

atoi_nextstep:
		cmp bx, strsize
		jge atoi_endloop                ; if (bx >= strsize) goto atoi_endloop
		movzx cx, byte ptr [si+bx]      ; cx = si[bx]
		cmp cl, 1Bh
		je end_input
		cmp cl , '0'
		jl atoi_endloop ; if (cl < '0') goto atoi_endloop
		cmp cl, '9'
		jg atoi_endloop ; if (cl > '9') goto atoi_endloop
		sub cl, '0'     ; cl -= '0'
		mul tmp         ; ax *= tmp
		add ax, cx      ; ax += cx
		inc bx          ; bx++
		jmp atoi_nextstep ;goto atoi_nextstep

end_input:
		mov ax, 1Bh
atoi_endloop:                   ; restoring registrs
		pop bx
		pop cx
		pop si
	ret
atoi ENDP
		
allocmem PROC STDCALL
		LOCAL tmp:WORD
		push dx
		push bx
		
		mov dx, offset inputm ; Выводим приветственное сообщение на ввод m
		mov ah, 09h ; прерывание вывода
		int 21h
		push 5 ; максимум длины 5
		push offset string 
		call readstring ; считываем строку
		push offset string
		call atoi ; приводим к числу
		mov ds:m, ax ; сохраняем в сегментный регистр в m
		cmp ax, 0h
		jz _exit
		
		call newline
		
		mov dx, offset inputn ; Выводим приветствие на ввод n
		mov ah, 09h ; прерывание вывода
		int 21h
		push 5
		push offset string
		call readstring ; считываем строку
		push offset string
		call atoi ; приводим к числу
		mov ds:n, ax ; сохраняем в сегментный регистр в n
		cmp ax, 0h
		jz _exit
				
		call newline
		
		mul ds:m ; в ax сидит n, умножаем на m
		shl ax, 1 ; столько байт нужно, т.к. в матрице word-ы
		shr ax, 4 ; столько 16-байтовых блоков нужно                        
		inc ax ; так как shr вернет целую часть от деления, увеличиваем на единицу
		mov bx, ax ; это количество 16-байтов блоков, которое нужно выделить
		mov ah, 48h ; прерывание выделения памяти
		int 21h
		jc _exit
		
		mov tmp, ax
		mov dx, offset allocres ; выводим начальное сообщение выделения ресурсов
		mov ah, 09h
		int 21h
		push bx
		call itoa ; выводим количество выделенной памяти
		
		call newline
		mov ax, tmp

		pop bx
		pop dx
		ret

_exit:
			xor al,al
			mov ax,4c00h
			int 21h

			ret	
allocmem ENDP

readmatr PROC STDCALL
		LOCAL strind, colind:WORD, isNeg:BYTE
		push dx
		push bx
		mov strind, 0h ; индекс строки
		mov colind, 0h ; индекс столбца
		readrow:
			push ax ; сохраняем ax
			mov ax, strind ; в ax заносим индекс строки
			cmp ax, ds:m ; проверям не прошли ли все строки
			jz endread
			readnumber:
						push ax ; запоминаем aх
						mov ax, colind ; заносим в ах индекс столбца
						cmp ax, ds:n ; сравниваем на конец строки
						jz endstring
						pop ax ; восстанавливаем х
						mov isNeg, 0 ; помечаем, что у нас неотрицательное число
						xor ax, ax ; обнулям ах
						push ax ; запоминаем
				readdigits:     
						mov ah, 01h ; прерывание чтения символа
						int 21h
						cmp al, '-' ; а вдруг это минус
						jz numIsNeg
				readdigcont:
						cmp al, '0' ; проверям, что код символа больше кода нуля
						jl rnumcomp
						cmp al, '9' ; проверям, что код символа меньше кода девятки
						jg rnumcomp
						cmp al, ' ' ; проверям, что это не пробел
						jz rnumcomp
						sub al, '0' ; приводим код к соответств числу
						movzx bx, al ; далее процедура накопления числа
						pop ax
						mov dl, 10
						mul dl
						add ax, bx
						push ax
						jmp readdigits
					rnumcomp:
						pop ax
						cmp isNeg, 0
						jz saveAsPos ; если мы считали положительное число
						jnz saveAsNeg ; если мы считали отрицательное число
					rnumj:
						inc colind ; увеличиваем индекс колонки
						jmp readnumber
			endstring: ; конец строки
				pop ax
				mov colind, 0h ; устанавливаем индекс колонки в ноль
				inc strind ; увеличиваем индекс строки
				call newline
				jmp readrow; метка чтения строки
		endread:                                      
			pop ax
			pop bx
			pop dx
			ret
		numIsNeg:
				mov isNeg, 1 ; говорим, что последнее число отрицательное
				jmp readdigits
		saveAsNeg:
				push ax ; в ах сидит число сохраним его
				mov ax, strind
				mul ds:n ; умножаем индекс строки на кол-во столбцов
				add ax, colind ; сдвигаем вправо на индекс стоблца
				mov bx, ax ; записываем текущий индекс в bx
				pop ax ; получаем число обратно
				neg ax ; делаем его отрицательным = xor ax  + 1
				shl bx, 1
				mov word ptr fs:[bx], ax ; сохраняем в массив
				jmp rnumj
		saveAsPos:
				push ax
				mov ax, strind
				mul ds:n
				add ax, colind
				mov bx, ax
				shl bx, 1
				pop ax ; те же действия но без отрицания
				mov word ptr fs:[bx], ax
				jmp rnumj
readmatr ENDP
; печать матрицы
printmatr PROC STDCALL
		LOCAL strind:WORD, colind:WORD
		mov strind, 0h ; инициализируем индексы в нули
		mov colind, 0h
		push dx
		push bx
		printstring:
			push ax
			mov ax, strind
			cmp ax, ds:m ; все вывели
			jz endread 
			printnumber:
				mov ax, colind
				cmp ax, ds:n ; вывели все строчку
				jz endstring
				mov ax, strind ; получаем текущий индекс элемента матрицы
				mul ds:n
				add ax, colind
				mov bx, ax
				shl bx, 1 ; так как элементы двухбайтовые
				mov ax, word ptr fs:[bx] ; считываем текущий элемент матрицы
				cmp ax, 0 ; если оно отрицательное - принимаем меры
				jl numberIsNeg
				printing:                               
					push ax ; кладем ах на стек
					call itoa ; переводим в число и выводим
					mov ah, 02h ; прерывание печати символа
					mov dl, ' ' ; выводить будем пробел
					int 21h
					inc colind ; увеличиваем индекс стоблца
					jmp printnumber                         
			endstring:
				pop ax
				int 3h
				mov word ptr colind, 0h ; обнуляем индекс столбца
				inc strind ; увеличиваем индекс строки
				call newline ; переходим на новую строчку
				jmp printstring
		endread:
			pop ax
			pop bx
			pop dx
			ret
		numberIsNeg:
			push ax ; сохраняем ах
			mov ah, 02h ; устанавливаем прерывание печати символа
			mov dl, '-' ; и печатем минус
			int 21h
			pop ax ; восстанавливаем ах
			neg ax ; и берем его отрицание
			jmp printing
printmatr ENDP

processing_matrix PROC STDCALL
		LOCAL strind:WORD, colind:WORD, tmp:WORD
		push bx
		mov ax, ds:m
		mov strind, ax ; будем искать с конца
		mov colind, 0h
		dec strind ; нумерация с нуля
		proc_matr:
				mov ax, strind
				cmp ax, 0
				jz no_pos_str
				check_str:
					mov ax, colind
					cmp ax, ds:n
					jz str_is_pos
					mov ax, strind ; получаем текущий индекс элемента матрицы
					mul ds:n
					add ax, colind
					mov bx, ax
					shl bx, 1 ; так как элементы двухбайтовые
					mov ax, word ptr fs:[bx] ; считываем текущий элемент матрицы
					cmp ax, 0
					jl str_not_pos
					inc colind
					jmp check_str
		no_pos_str:
				mov dx, offset noposstr ; Выводим приветствие на ввод n
				mov ah, 09h ; прерывание вывода
				int 21h
				call newline
				jmp end_swap
		str_not_pos:
			dec strind
			mov colind, 0h
			jmp proc_matr
		str_is_pos:
			mov colind, 0h
			swap_str:
				mov ax, colind
				cmp ax, ds:n
				jz end_swap
				mov ax, strind ; получаем текущий индекс элемента матрицы
				mul ds:n
				add ax, colind
				mov bx, ax
				shl bx, 1 ; так как элементы двухбайтовые
				mov ax, word ptr fs:[bx] ; считываем элемент последней позитивное строки                
				push bx
				push ax
				mov bx, colind
				shl bx, 1
				mov ax, word ptr fs:[bx]
				mov tmp, ax ; считываем элемент первой строки
				pop ax
				mov word ptr fs:[bx], ax ; записываем в первую строку элемент последней позитивной
				pop bx ; восстанавливаем указатель на последнюю позитивную
				mov ax, tmp
				mov word ptr fs:[bx], ax ; записываем в последнюю позитивную элемент первой
				inc colind ; сдвигаемся на следующий элемент
				jmp swap_str
		end_swap:
			pop bx                          
			ret                                
processing_matrix ENDP

memcpy PROC STDCALL, dest: PTR BYTE, src: PTR BYTE, memsize:WORD
       push di         ; saving rigisters
       push si
       push cx
       mov di, dest
       mov si, src
       mov cx, memsize
       rep movsb       ; move string byte cx size

       pop cx
       pop si
       pop di
       ret
memcpy ENDP

readstring  PROC STDCALL, strbuff: ptr byte, strsize: word
       push bx                  ; push into stack bx to save
       push si                  ; push into stack si to save
       xor  bx,bx               ; bx = 0
       mov  si, strbuff         ; putting string buffer
       mov  ah, 01              ; read char

nextchar:
       cmp bx, strsize          ; comparing bx and string size
       jge endloop              ; if bx >= strsize
       int 21h                  ; ёўшЄрЄ№ ё ъырт√
       cmp al, 0dh              ;яЁютхЁшЄ№, с√ы ыш эрцрЄ 'enter'
       jz  endloop              ; if al == '\n'
       cmp al, 08               ; яЁютхЁшЄ№, с√ы ыш эрцрЄ 'backspace'
       jnz _noback              ; if al != 'Backspace'
       test bx,bx       
       jz nextchar              ; if bx == 0, чэрўшЄ ёшьтюыют эх с√ыю
       dec bx                   ;bx--
       jmp nextchar

_noback:
       mov byte ptr [si+bx], al ; si[bx] = al
       inc bx                   ; bx++
       jmp nextchar
endloop:

       mov byte ptr [si+bx], '$' ; adding '\0'
       mov ax, bx                ; ax = strlen


       pop si           ; returning registry to it's places
       pop bx
       ret              ; return
readstring ENDP

newline PROC STDCALL
	push dx
	push ax

	mov ah, 9
		mov dx, offset emptyString
		int 21h

	pop ax
	pop dx
	ret
newline ENDP

itoa PROC STDCALL, i:WORD
				LOCAL tmp:BYTE
				push bx                 
				push dx
				push cx
				
				mov bx, 10 ; эр эхую сєфхь фхышЄ№
				mov ax, i
				xor cx, cx ; ёўхЄўшъ ЎшЇЁ
				stack_push:
					xor dx, dx ; юёЄрЄюъ
					div bx; dx = ax % 10, ax = ax / 10
					push dx ; ъырфхь эр ёЄхъ ёяЁртр эрыхтю
					inc cx
					cmp ax, 0 ; хёыш яюыєўшыш тёх ЎшЇЁ√
					jnz stack_push
					mov ah, 02h
				stack_pop:
					pop dx ; ёэшьрхь ёю ёЄхър ЎшЇЁ√ ўшёыр ёыхтр эряЁртю
					add dl, '0' ; яюыєўрхь ъюф ёшьтюыр ЎшЇЁ√
					int 21h ; т√тюфшь хую
					dec cx
					jnz stack_pop
				pop cx
				pop dx
				pop bx
				ret
itoa ENDP

start:
		push dx
	mov ax, data    ; initialising segments
	mov ds, ax
	mov es, ax
	assume ds:data, es:data, cs:code, ss:stack
		  
		call allocmem
		mov fs, ax
		call readmatr
		call newline
		call processing_matrix
		call printmatr
		
		
end_program:
		pop dx
	xor al,al
	mov ax,4c00h
	int 21h

	ret


CODE ENDS
END start
