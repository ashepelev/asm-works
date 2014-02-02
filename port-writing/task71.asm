.386
TERMINALSYMBOL  EQU '$'
DATA SEGMENT USE16 PUBLIC 'data'

string BYTE 1000 dup(0h)
memarr BYTE 100 dup(0)
indseq BYTE 100 dup(0)

wrongNum db 'Wrong indicator number$'

DATA ENDS

STACK SEGMENT STACK 'stack'
 db  1024 dup (0)
STACK ENDS


CODE SEGMENT USE16 PUBLIC 'code'
assume ds:data, es:data, cs:code, ss:stack

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
	mov di, offset memarr
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

; Функция вместо стандартного прерывания. Вызывается 18 раз в секунду
; Обработчик прерывания 08h(IRQO)
myTimerHandler proc
	push ax; because interrupt could be really unexpected
	push bx;
	mov al, 00110000b   ; чтение/запись сначала младшего,а потом старшего байта
							; прерывание IRQO при достижении нуля
							; программируем канал 0
	out 43h, al ; управляющий регистр первого таймера

	movzx ax, byte ptr cs:counter
	inc ax
	mov bl, 19
	div bl                          ; делим - каждый
	mov byte ptr cs:counter, ah ; в ah - остаток (сколько раз (0-18) сработал таймер)
	add byte ptr cs:index, al ; в al - результат деления (номер текущего индикатора для загорания)

    mov al, 0ffh ;restore timer value to default:
    out 40h, al ;setting lower byte of timer value
    out 40h, al ;setting higher byte
	pop bx
	pop ax
	db 0eah ; jmp far
	defaultHandler dd 0
myTimerHandler endp



; Устанавливает дескриптор
; handler_seg - сегмент старого обработчика
; handler_offs 
; Установка другого обработчика
setHandler proc PASCAL interrupt:word, handler_seg:word , handler_offs:word
	push ds
	push dx
	cli ; отключают прерывания
		; в сегменте 0000 - ссылки на обработчики прерываний
	xor ax, ax  ;handlers offsets and segments placed in 0000:interrupt*4
	mov ds, ax
	movzx eax, interrupt ; eax = 0000:interrupt
	mov dx, handler_seg ; в dx - сегмент
	mov word ptr ds:[eax*4+2], dx ; в 0000:interrupt*4 + 2 сегмент нового обработчика
	mov dx, handler_offs
	mov word ptr ds:[eax*4], dx ; в 0000:interrupt*4 оффсет нового обработчика
	sti ; включают прерыванию
	pop dx
	pop ds
	ret
setHandler endp

indicatorPlay proc
	push es
    push ds
    push bx
    push dx
	mov ax, 3508h ; перехват прерываний
	int 21h ; считываем адрес обработчика прерываний - сохранится в es:bx
		; сохраняем стандартный обработчик
	mov word ptr defaultHandler, bx ; offset
	mov word ptr defaultHandler+2, es ; segment

	mov ax, offset indseq
	push ax 
	call readAndParseByte ; массив считался в seq
	mov cs:seq_len, al ; длина массива
	push 08h
	push cs
	push offset myTimerHandler
	call setHandler
;        int 3h
chgIndicator: ; ждем доступа к клавиатуре
	in al, 64h ; чтение состояния клавиатуры
	test al, 0010b ; проверяем, есть ли в буфере ввода данные
	jnz chgIndicator ; если буфер не свободен - ждем
	mov ah, 01h
	int 16h
	jnz keyPressed
notPressed:
	mov al, 0edh ; изменить состояние светоидов
	out 60h, al ; записываем в порт чтения клавиатуры
	movzx eax, byte ptr cs:index       ; в index - номер текущего индикатора (в последовательности)
	add ax, offset indseq   ; добавляем начало массива
	mov al, [eax]  ; записываем байт в al - команда для индикаторов
		cmp al, 0h
		jz isZeroN
		cmp al, 2h
		jg err_
		shr al, 1
	contWait:
	out 60h, al ; записываем байт в al - команда для индикаторов
	movzx ax, byte ptr cs:index        ; помещаем в аl текущий номер индикатора
	div byte ptr cs:seq_len ; делим на длину последовательности
	mov byte ptr cs:index, ah ; записываем в index остаток, чтобы не выйти за границы
	;;;;;;;;;;;;;
	jmp chgIndicator
	;;;;;;;;;;;
end_:		
	push 08h ; interrupt number
	push word ptr defaultHandler+2 ; segment
	push word ptr defaultHandler ; offset
	call setHandler
	
	pop dx
    pop bx
    pop ds
    pop es
	ret
	
	isZeroN:
		mov al, 1h
		jmp contWait
	err_:
		mov dx, offset wrongNum
		mov ah, 09h
		int 21h
		mov ax, 4c00h
		int 21h
	keyPressed:
		mov ah, 00h
		int 16h
		cmp ah, 02h
		jz end_
		jnz notPressed
    counter db 0
    index db 0
    seq_len db 0 
indicatorPlay endp



start:
	mov ax, data
	mov ds, ax
	mov es, ax
		
		call indicatorPlay
		mov ax, 4c00h
	int 21h

	ret
CODE ENDS
END start
