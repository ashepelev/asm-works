.386
TERMINALSYMBOL  EQU '$'                 ;#define TERMINALSYMBOL '$'
DATA SEGMENT USE16 PUBLIC 'data'
string WORD 5 dup(0)
emptyString byte 0Dh,0Ah,TERMINALSYMBOL

allocres    DB 'Allocated mem: $'
ems_driver DB 'EMMXXXX0'
noems DB 'There is no EMS$'
emsver DB 'EMS version is: $'
listlength DB 'Enter list length: $'
difference DB 'Difference = $'

isnull DB 'NULL$'
emsid DW ?	; Здесь будет сохраняться идентификатор выделенной памяти
len WORD 0 ; длина списка

mptr WORD 0

; Структура список
list STRUCT ; 4 байта
	head dw ? ; 2 байта
	tail dw ? ; 2 байта
list ENDS
; Структура узел
node STRUCT ; 6 байта
	prev dw ? ; 2 байта
	value word 0 ; 2 байта
	next dw ? ; 2 байта
node ENDS

mylist list <0,0> ; Список, который хранится в основной памяти

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
	
; Выделение памяти	
; Для работы нужно использовать правильный образ
; С включенной EMS
; После активации - отладка невозможна
allocmem PROC STDCALL
		LOCAL tmp:WORD
		push dx
		push bx
		
		mov ah, 41h ; получим сегментный адрес окна
		int 67h
		mov fs, bx  ; Запоминаем его в сегмент fs

		mov ah, 43h ; Выделим ОДНУ 16-килобайтной страницу
		mov bx, 0001h	
		int 67h
		
		; отображение памяти
		mov ah, 44h
		mov al, 00h ; номер первой 16Кб страницы в 64Кб окне
		mov bx, 00h ; номер 16Кб страницы в EMS-памяти
		; в DX уже сидит идентификатор выделенной памяти в пред вызове
		int 67h

		mov ax, dx ; сохраним идентификатор нашей памяти и возвратим его

		pop bx
		pop dx
		ret
allocmem ENDP

; Функция считывает список
; На вход подается указатель на список
readlist PROC STDCALL, rlist: ptr list
	LOCAL ind:WORD	; Индекс в списке
	LOCAL isNeg:WORD	; Флаг отрицательного числа
	LOCAL elem:WORD		; Временная переменная
	push dx			; Сохраняем регистры
	push bx
	push cx
	mov dx, offset listlength ; Выводим приглашение к вводу длины списка
	mov ah, 09h
	mov cx, 6h				; Множитель, равный длине структуры
	int 21h 
	push 5
	push offset string
	call readstring			; Считываем
	call newline
	push offset string
	call atoi			; Парсим
	mov ds:len, ax		; Сохраняем
	mov ind, 0h			; Инициализируем
	readstart:
		mov isNeg, 0h
		mov ax, ind
		cmp ax, ds:len	; Конец списка?
		jz end_list
		xor ax, ax		; В ах будет накапливаться число
		push ax
		mov ah, 01h		; Считываем симол
		int 21h
		cmp al, '-'
		jz numIsNeg		; Если это отрицательное, помечаем и на readdigits
		jnz readdigcont	; Если положительное, то на readdigcont, символ уже считан
		readdigits:
			mov ah, 01h
			int 21h
		readdigcont:	; Стандартный алгоритм накопления числа
			cmp al, '0' ; проверям, что код символа больше кода нуля
			jl save
			cmp al, '9' ; проверям, что код символа меньше кода девятки
			jg save
			cmp al, ' ' ; проверям, что это не пробел
			jz save
			sub al, '0' ; приводим код к соответств числу
			movzx bx, al ; далее процедура накопления числа
			pop ax
			mov dl, 10
			mul dl
			add ax, bx
			push ax
			jmp readdigits       
		save:			; Сохраняем элемент в список
			mov ax, ind	; Число в ax лежит на стеке. Помещаем в ax индекс элемента в списке
			mul cx		; Умножаем на длину структуры
			mov bx, ax	; В bx помещаем указатель на структуру
			pop ax		; Снимаем со стека ах, в нем накопленное число
			cmp isNeg, 0	; Проверям не отрицательный ли
			jnz makeNeg
		saveFromNeg:
			mov word ptr fs:[bx+2], ax		; Сохраняем в структуру на позицию +2 байта
			cmp ind, 0h ; Если это не первый узел списка
			jnz linkleft ; То связываем с предыдущим
		save1:
			mov ax, ind
			cmp ax, ds:len-1 ; Если это не последний узел списка
			jnz linkright	; То связываем со следующим
		save2:                  
			inc ind			; Увеличиваем индекс списка
			jmp readstart	; Переходим к считыванию следующего числа
		linkleft:
			mov ax, ind ;Помещаем в ax индекс элемента в списке
			mul cx ; Умножаем на длину структуры
			mov bx, ax  ; Записываем индекс текущей структуры   
			sub ax, 6   ; Указатель на предыдущий элемент           
			mov word ptr fs:[bx], ax ; В текущий даем ссылку на предыдущий
			mov word ptr fs:[bx-2], bx ; В предыдущем даем ссылку на следующий
			jmp save1
		linkright:    
			mov ax, ind
			mul cx
			mov bx, ax   
			add ax, 6 ; указатель на следующий элемент                                   
			mov word ptr fs:[bx+4], ax ; В текущем даем ссылку на следующий
			mov word ptr fs:[bx+6], bx ; В следующем даем ссылку на предыдущий
			jmp save2               
	numIsNeg:
		mov isNeg, 1 ; говорим, что последнее число отрицательное
		jmp readdigits
	makeNeg:
		neg ax		; Делаем отрицательным
		jmp saveFromNeg
	end_list:
		mov bx, 0h	; Для первого элемента в качестве предыдущего указываем 8000h, что считается NULL
		mov word ptr fs:[bx], 8000h
		; Получаем указатель на последний элемент списка
		mov ax, ds:len
		dec ax
		mul cx
		mov bx, ax
		mov word ptr fs:[bx+4], 8000h  ; Для последнего элемента, следующий NULL
		assume bx: ptr list
		mov bx, rlist ; Заполняем поля списка
		mov [bx].head, 0h ; Голова указывает на первый
		mov ax, ds:len
		dec ax
		mul cx
		mov [bx].tail, ax ; Хвост на последний элемент
		pop cx
		pop bx
		pop dx
		ret
readlist ENDP

; Печать матрицы
; На входе указатель на список
printlist proc STDCALL, plist:PTR list
	LOCAL ind:WORD
	push bx
	push dx
	assume bx:ptr list
	mov bx, plist
	mov ax, [bx].head ; Получаем голову
	mov bx, ax
	printstart:
		printleftnull:
			mov dx, offset isnull	; В начале выводим NULL
			mov ah, 09h
			int 21h 
			mov ah, 02h
			mov dl, '='			; И знак "=" как разделитель
			int 21h
		printval:
			mov ax, word ptr fs:[bx+2] ; Выводим само значение
			cmp ax, 0					; Проверяем на отрицательность
			jl isNeg
		printval1:                      
			push ax						; Переводим число в строку
			call itoa					; И печатаем
			mov ah, 02h 
			mov dl, '='					; Выводим "="
			int 21h
		cmp word ptr fs:[bx+4], 8000h	; Проверям, что у нас не последний элемент
		jz lastelem
		mov ax, word ptr fs:[bx+4]		; В ах помещаем указатель на следующий элемент
		mov bx, ax						; И переводим его в bx
		jmp printval					; Обрабатываем следующий элемент
		isNeg:
			push ax						; Запоминаем ах
			mov ah, 02h
			mov dl, '-'					; Выводим минус
			int 21h
			pop ax						; Восстанавливаем ах
			neg ax						; Берем его отрицательное
			jmp printval1
		lastelem:
			mov dx, offset isnull		; Если это последний, то выводим "NULL"
			mov ah, 09h
			int 21h
			jmp endprint
		endprint:
			pop dx
			pop bx
			ret
printlist ENDP

; Моя функция которая находит разность элементов списка
; Считает как a[0] - sum[i:1-n](a[i])
findDifference PROC STDCALL, ilist: ptr list
	push bx
	push dx
	mov dx, offset difference
	mov ah, 09h
	int 21h
	assume bx:ptr list
	mov bx, ilist
	mov ax, [bx].head
	mov bx, ax      
	
	mov ax, word ptr fs:[bx+2]
	mov dx, word ptr fs:[bx+4]
	mov bx, dx
	printstart:
		printval:
			cmp bx, 8000h
			jz endprint
			mov dx, word ptr fs:[bx+2]
			sub ax, dx
			mov dx, word ptr fs:[bx+4]
			mov bx, dx
			jmp printval
		endprint:
			cmp ax, 0
			jl isNeg
		endprint1:
			push ax
			call itoa
			pop dx
			pop bx
			ret
		isNeg:
			push ax
			mov ah, 02h
			mov dl, '-'
			int 21h
			pop ax
			neg ax
			jmp endprint1
findDifference ENDP


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
				push ax                                

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
				pop ax
				pop cx
				pop dx
				pop bx
				ret
itoa ENDP

; Функция освобождает занятую память
freemem PROC STDCALL
	push dx
	mov ah, 45h
	mov dx, ds:emsid
	int 67h
	pop dx
	ret
freemem ENDP

start:
		push dx
	mov ax, data    ; initialising segments
	mov ds, ax
	mov es, ax
	assume ds:data, es:data, cs:code, ss:stack
		  
	call allocmem
	mov ds:emsid, ax    ; Сохраняем идентификатор выделенной EMS памяти
	push offset mylist
	call readlist
	call newline
	push offset mylist
	call printlist   
	call newline
	push offset mylist
	call findDifference
     ;   call newline       
	call freemem    
		
end_program:
		pop dx
	xor al,al
	mov ax,4c00h
	int 21h

	ret


CODE ENDS
END start
