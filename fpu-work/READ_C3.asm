.386
TERMINALSYMBOL  EQU '$'
DATA SEGMENT USE16 PUBLIC 'data'
string BYTE 6 dup(0)
DATA ENDS


STACK SEGMENT STACK 'stack'
 db  1024 dup (0)
STACK ENDS


CODE SEGMENT USE16 PUBLIC 'code'
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


memcpy PROC STDCALL, dest: PTR BYTE, src: PTR BYTE, memsize:WORD ; копирование области памяти
       push di
       push si
       push cx
       mov di, dest
       mov si, src
       mov cx, memsize
       rep movsb ; копируем байты столько раз, каков размер области памяти

       pop cx ; восстанавливаем контекст
       pop si
       pop di
       ret
memcpy ENDP


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

atoi PROC STDCALL,  strbuf:PTR BYTE ; перевод строки в целое число
		LOCAL tmp:WORD, strsize:WORD
		push si
		push cx
		push bx

		push strbuf ; кладем указатель на начало буфера-строки на стек
		call strlen ; узнаем её длину
		mov strsize, ax ; запоминаем в локальной переменной
		mov si, strbuf ; готовимся ходить по строке
		mov tmp, 10
		xor bx,bx ; обнуляем регистры
		xor ax,ax

atoi_nextstep:
		cmp bx, strsize ; не дошли ли до конца строки
		jge atoi_endloop ; если дошли - выходим
		movzx cx, byte ptr [si+bx] ; иначе запоминаем в регистре cx текущий символ (задаётся 16-теричным кодом)
		cmp cl , '0'
		jl atoi_endloop
		cmp cl, '9'
		jg atoi_endloop ; если разность кодов символов меньше нуля или больше 9-ти - нарвались на конец строки, само число уже обработано
		sub cl, '0' ; иначе теперь в регистре цифра
		mul tmp ; умножаем на 10 формируемое число
		add ax, cx ; прибавляем очередную цифру (идёт в младший разряд)
		inc bx ; готовимся работать со следующим символом строки
		jmp atoi_nextstep

atoi_endloop:
		pop bx
		pop cx
		pop si
	ret
atoi ENDP

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


expr PROC STDCALL
				LOCAL A:WORD, B:WORD, CC:WORD
				
				push ax
				push dx
				push cx
				
				push 5
				push offset string
				call readstring
				push offset string
				call atoi
				mov A, ax
				call nextline
				push 5
				push offset string
				call readstring
				push offset string
				call atoi
				mov B, ax
				call nextline
				push 5
				push offset string
				call readstring
				push offset string
				call atoi
				mov CC, ax
				call nextline
				
				mov ax, B
				mov dx, 19
				mul dx
				push ax
				mov ax, A
				mov dx, CC
				mul dx
				mov cx, B
				div cx
				pop dx
				add ax, dx
				push ax
				call itoa
				
				pop cx
				pop dx
				pop ax
				ret
expr ENDP
				
					
				

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

start:

	mov ax, data
	mov ds, ax
	mov es, ax
	assume ds:data, es:data, cs:code, ss:stack
		
			
	call expr

	xor al, al
	mov ax, 4c00h
	int 21h

	ret

CODE ENDS
END start
