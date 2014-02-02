.386
TERMINALSYMBOL  EQU '$'
DATA SEGMENT USE16 PUBLIC 'data'
membuf BYTE '4294967295$'
memarr BYTE 100 dup(0)
indseq BYTE 100 dup(0)
enterTime db 'Enter date (hh mm ss): $'
enterDate DB 'Enter date: $'
wrong db 'Wrong date or time$'
leap db 0
date STRUCT
    day word ?
    month word ?
    year word ?
date ENDS
mydate date <0,0,0> ; пустая структура даты
DATA ENDS

STACK SEGMENT STACK 'stack'
 db  1024 dup (0)
 
STACK ENDS


CODE SEGMENT USE16 PUBLIC 'code'
assume ds:data, es:data, cs:code, ss:stack

strlen PROC PASCAL, strBuffer:ptr byte ; функция для определения длины строки
	push di ;положили на стек старое значение регистра
	push cx ; запоминаем контекст
	mov di, strBuffer ; это наш буфер
	mov al, TERMINALSYMBOL ; это символ конца строки
	mov cx, 0ffffh ; столько в худшем случае будет "итераций"
	cld             ; установить правильное направление - происходит инкремент di
	repnz scasb ; проходим по строке в поисках символа конца
	jnz   bad_string ; если не нашли символ конца
	neg cx
	sub cx,2 ; нашли длину
bad_string:
	mov ax, cx ; запихнули длину в регистр, в котором ожидаем увидеть результат выполнения
	pop cx ; восстановили контекст
	pop di
	ret
strlen ENDP

atoi PROC STDCALL,  strbuf:PTR BYTE ; перевод строки в целое число
		LOCAL tmp:DWORD, strsize:WORD
		push si
		push ecx
		push bx
		push edx

		push strbuf ; кладем указатель на начало буфера-строки на стек
		call strlen ; узнаем её длину
		mov strsize, ax ; запоминаем в локальной переменной
		mov si, strbuf ; готовимся ходить по строке
		mov tmp, 10
		xor bx,bx ; обнуляем регистры
		xor eax,eax

atoi_nextstep:
		cmp bx, strsize ; не дошли ли до конца строки
		jge atoi_endloop ; если дошли - выходим
		movzx ecx, byte ptr [si+bx] ; иначе запоминаем в регистре cx текущий символ (задаётся 16-теричным кодом)
		cmp cl , '0'
		jl atoi_endloop
		cmp cl, '9'
		jg atoi_endloop ; если разность кодов символов меньше нуля или больше 9-ти - нарвались на конец строки, само число уже обработано
		sub cl, '0' ; иначе теперь в регистре цифра
		mul tmp ; умножаем на 10 формируемое число
		add eax, ecx ; прибавляем очередную цифру (идёт в младший разряд)
		inc bx ; готовимся работать со следующим символом строки
		jmp atoi_nextstep

atoi_endloop:
		pop edx
		pop bx
		pop ecx
		pop si
	ret
atoi ENDP

readDigit PROC STDCALL, num:WORD
	LOCAL ind:WORD
	push dx
	push bx
;        push cx
   ;     mov cx, num
	xor ax, ax
	push ax
	mov ind, 0h
iter:
	mov ax, num
	cmp ind, ax
	jz enditer
	mov ah, 01h
	int 21h
	sub al, '0' 
	movzx bx, al
	pop ax
	mov dl, 10
	mul dl
	add ax, bx
	push ax
	inc ind
	jmp iter
enditer:
	pop ax
 ;       pop cx
	pop bx
	pop dx
	ret
readDigit ENDP

readstring  PROC STDCALL, strbuff: ptr byte, strsize: word
       push bx
       push si
       xor  bx,bx
       mov  si, strbuff
       mov  ah, 01 ; при вызове прерывания будем читать символ
nextchar:
       cmp bx, strsize ; пока не превысили размер буфера
       jge endloop
       int 21h ; вызываем прерывание
       cmp al, 0dh ;проверяем что нажат символ перехода на новую строчку
       jz  endloop ; если размер буфера таки превысили, либо таки нажали Enter, считано всё возможное
       cmp al, 08 ; не решили ли чего-нибудь стереть
       jnz _noback ; если не решили - всё ok и символ добавляется в буфер
       test bx,bx ; если решили - смотрим, не имеется ли у нас в настоящий момент 0 символов
       jz nextchar ; если символов в буфере и правда 0, просто читаем следующий
       dec bx ; иначе выкинули один символ
       jmp nextchar ; и отправились читать следующий
_noback:
       mov byte ptr [si+bx], al
       inc bx ; строка увеличилась на 1 символ
       jmp nextchar
endloop:
       mov byte ptr [si+bx], TERMINALSYMBOL ; добавляем признак конца строки
       mov ax, bx ; запоминаем длину

       pop si
       pop bx
       ret
readstring ENDP

nextline PROC ; надо же иногда и на новую строчку переходить!
	push dx
	push ax

	mov ah, 2 ; будем выводить символы
	mov dl, 0Ah ; #10
	int 21h
	mov dl, 0Dh ; #13
	int 21h

	pop ax
	pop dx
	ret
nextline ENDP

readDate PROC STDCALL, d: ptr date
	push bx
	push dx

	mov ah, 9
    mov dx, offset enterDate    ; Выводим приглашение к вводу
    int 21h

	assume bx: ptr date
	mov bx, d
		
	push 2
	call readDigit                          ; Считываем число в 2 цифры
	mov [bx].day, ax
	
	mov ah, 01h                                     ; Любое разделитель в 1 символ
	int 21h
	
	push 2
	call readDigit                          ; Считываем число в 2 цифры
	mov [bx].month, ax

	mov ah, 01h                                     ; Любое разделитель в 1 символ
	int 21h

	push 4
	call readDigit                          ; Считываем число в 4 цифры
	mov [bx].year, ax

	pop dx
	pop bx
	ret
	
readDate ENDP

readAndParseByte PROC STDCALL array: ptr byte
	LOCAL buffer: ptr byte
	push cx
	push bx
	push di
	push si
	push dx
	push es
	mov ax, ds
	mov es, ax

	mov di, offset membuf
	mov si, array ; указатель на источник - массив
	push 100
	push di
	call readstring ; считываем строку в di
	mov cx, ax ; в ах - кол-во считанных символов
	xor bx, bx

	readone:
		mov al, ' '
		cld
		repe scasb
		dec di ; делает одну лишнюю операцию - уменьшаем
		inc cx 

		push di
		call atoi ; преобразовали в число
		mov byte ptr[si + bx], al ; сохранили в массив
		inc bx ; передвинулись на след элемент массива

		mov al, ' ' 
		cld
		repne scasb ; пока не пробел - сдвигаемся (если несколько пробелов между элементами)
		test cx, cx
		jnz readone
	mov ax, bx ; кол-во элементов маасива
	;mov bx, 1
	;xor dx, dx
	;div bx
	call nextline
	pop es
	pop dx
	pop si
	pop di
	pop bx
	pop cx
	ret
readAndParseByte endp

isLeap PROC STDCALL, d: ptr date
	push bx
	push dx
	push cx
	
	assume bx:ptr date
	mov bx, d

	mov ax, [bx].year ; считываем год      
	cwd
	mov cx, 400 ; проверям делится ли на 400
	div cx
	cmp dx, 0
	jz yearIsLeap
	
	mov ax, [bx].year ; проверям делится ли на 100
	cwd
	mov cx, 100
	div cx
	cmp dx, 0
	jz yearIsNotLeap

	mov ax, [bx].year ; проверям делится ли на 4
	cwd
	mov cx, 4
	div cx
	cmp dx, 0
	jz yearIsLeap
	
	jmp yearIsNotLeap ; во всех остальных случаях - невисокосный

yearIsLeap:
	mov ax, 1h
	jmp procend
yearIsNotLeap:
	mov ax, 0h
	jmp procend
procend:
	pop cx
	pop dx
	pop bx
	ret
isLeap ENDP

setCMOSParam proc; arguments al - index, ah - value
	push dx
	out 70h, al
	xchg ah, al
	mov ah, 0
	mov dl, 10
	div dl
	shl al, 4
	and ah, 0fh
	add al, ah
	
	out 71h, al
	pop dx
	ret
setCMOSParam endp

checkDay proc STDCALL, d:WORD, m:WORD
	int 3h
	;;;;;;;;;; CHECK DAY
	mov ax, d
	cmp al, 1
	jb _err
	cmp al, 31
	ja _err
	
	mov bx, m
	cmp bl, 1
	jz _mon31
	
	cmp bl, 2
	jz _feb
	
	cmp bl, 3
	jz _mon31
	
	cmp bl, 4
	jz _mon30
	
	cmp bl, 5
	jz _mon31
	
	cmp bl, 6
	jz _mon30
	
	cmp bl, 7
	jz _mon31
	
	cmp bl, 8
	jz _mon31
	
	cmp bl, 9
	jz _mon30
	
	cmp bl, 10
	jz _mon31
	
	cmp bl, 11
	jz _mon30
	
	cmp bl, 12
	jz _mon31
	ret
_err:
	call nextline
	mov dx, offset wrong
		mov ah, 09h
		int 21h
		mov ax, 4c00h
		int 21h

	_mon31:
		cmp al, 31
		jnz _err
		ret
	_mon30:
		cmp al, 30
		jnz _err
		ret
	_feb:
		cmp leap, 0
		jz _febnl
	_febl:
		cmp al, 29
		jnz _err
		ret
	_febnl:
		cmp al, 28
		jnz _err
		ret
checkDay endp

writeDate proc
	push bx
	push dx
	push cx
	push offset mydate
	call readDate
	
	assume bx:ptr date
    mov bx, offset mydate

	push offset mydate
	call isLeap
	mov leap, al
	
	;;;;;;;;;;; CHECK YEAR
	mov ax, [bx].year
	cmp ax, 0
	jb _err
	
	cmp ax, 9999
	ja _err
	
	;;;;;;;;;;;; CHECK MONTH
	mov ax, [bx].month
	cmp al, 1
	jb _err
	cmp al, 12
	ja _err
	xchg ah, al
	
	mov ax, [bx].month
	push ax
	mov ax, [bx].day
	push ax
	call checkDay
	
_end:   
	pop cx
	pop dx
	pop bx
	ret
	
_err:
	call nextline
	mov dx, offset wrong
		mov ah, 09h
		int 21h
		mov ax, 4c00h
		int 21h
writeDate endp

setCMOS proc

	push di
	push cx
			
	call writeDate  
	call nextline
	
	assume bx:ptr date
    mov bx, offset mydate
		
	mov di, offset indseq
	push di
		
	mov dx, offset enterTime
		mov ah, 09h
		int 21h
	call readAndParseByte  ; read ss mm hh
	mov cx, ax

	mov al, 0bh     ; CMOS-управляющий регистр (регистр состояния B)
	out 70h, al ; Пишем в индекс CMOS
	in al, 71h      ;
	or al, 00000010b ; формат времени - двоичный, 24-часовой
	out 71h, al  
	
	;;;;;;;;;;; WRITE YEAR
	xor dx, dx
	mov ax, [bx].year
	mov dx, 100
	div dx
;	xchg ah, al
	mov al, 09h
	mov ah, dl
	call setCMOSParam
	jcxz _end
	
	mov ax, [bx].year
	mov dx, 100
	div dx
	xchg ah, al
	mov al, 32h
	call setCMOSParam
	jcxz _end
	
	;;;;;;;;;;; WRITE MONTH
	mov dx, [bx].month
	mov ah, dl
	mov al, 08h
	call setCMOSParam
	jcxz _end
	
	;;;;;;;;;;; WRITE DAY
	mov dx, [bx].day
	mov ah, dl
	mov al, 07h          ;days
	call setCMOSParam
	jcxz _end

	mov al, 04h          ;hours
	mov ah, byte ptr[di]
	cmp ah, 00
	jb _err
	cmp ah, 23
	ja _err
	call setCMOSParam
	dec cx
	jcxz _end
		
		mov al, 02h          ;минуты
	mov ah, byte ptr[di+1]
	cmp ah, 00
	jb _err
	cmp ah, 59
	ja _err
	call setCMOSParam
	dec cx
	jcxz _end


	mov al, 00h          ;секунды
	mov ah, byte ptr[di+2]
	cmp ah, 00
	jb _err
	cmp ah, 59
	ja _err
	call setCMOSParam
	dec cx
	jcxz _end       


_end:   
	pop cx
	pop di
	ret
	
_err:
	call nextline
	mov dx, offset wrong
		mov ah, 09h
		int 21h
		mov ax, 4c00h
		int 21h

setCMOS endp

start:
	mov ax, data
	mov ds, ax
	mov es, ax

	call setCMOS
		
	mov ax, 4c00h
	int 21h

	ret

CODE ENDS
END start
