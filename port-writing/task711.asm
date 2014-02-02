.386
TERMINALSYMBOL  EQU '$'
DATA SEGMENT USE16 PUBLIC 'data'
testing BYTE '4294967295$'
memarr BYTE 100 dup(0)
indseq BYTE 100 dup(0)
wrongNum db 'Wrong indicator number$'
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
		mov si, array ; єърчрЄхы№ эр шёЄюўэшъ - ьрёёшт
	push 100
	push di
	call readstring ; ёўшЄ√трхь ёЄЁюъє т di
	mov cx, ax ; т рї - ъюы-тю ёўшЄрээ√ї ёшьтюыют
	xor bx, bx

	readone:
		mov al, ' '
		cld
		repe scasb
		dec di ; фхырхЄ юфэє ыш°э■■ юяхЁрЎш■ - єьхэ№°рхь
		inc cx

		push di
		call atoi
		mov byte ptr[si + bx], al ; ёюїЁрэшыш т ьрёёшт
		inc bx ; яхЁхфтшэєышё№ эр ёыхф ¤ыхьхэЄ ьрёёштр

		mov al, ' '
		cld
		repne scasb ; яюър эх яЁюсхы - ёфтшурхьё  (хёыш эхёъюы№ъю яЁюсхыют ьхцфє ¤ыхьхэЄрьш)
		test cx, cx
		jnz readone
	mov ax, bx ; ъюы-тю ¤ыхьхэЄют ьррёштр
	call nextline
	pop es
	pop dx
	pop si
	pop di
	pop bx
	pop cx
	ret
readAndParseByte endp

; ╘єэъЎш  тьхёЄю ёЄрэфрЁЄэюую яЁхЁ√трэш . ┬√ч√трхЄё  18 Ёрч т ёхъєэфє
; ╬сЁрсюЄўшъ яЁхЁ√трэш  08h(IRQO)
myHandler proc
	push ax
	push bx;
	mov al, 00110000b   ; ўЄхэшх/чряшё№ ёэрўрыр ьырф°хую,р яюЄюь ёЄрЁ°хую срщЄр
							; яЁхЁ√трэшх IRQO яЁш фюёЄшцхэшш эєы 
							; яЁюуЁрььшЁєхь ърэры 0
	out 43h, al             ; єяЁрты ■∙шщ ЁхушёЄЁ яхЁтюую ЄрщьхЁр

	movzx ax, byte ptr cs:counter
	inc ax
	mov bl, 19
	div bl                  ; фхышь - ърцф√щ Єшъ
	mov byte ptr cs:counter, ah ; т ah - юёЄрЄюъ (ёъюы№ъю Ёрч (0-18) ёЁрсюЄры ЄрщьхЁ)
	add byte ptr cs:index, al ; т al - Ёхчєы№ЄрЄ фхыхэш  (эюьхЁ Єхъє∙хую шэфшърЄюЁр фы  чруюЁрэш )
	mov al, 0ffh ; ╙ёЄрэртыштрхь ёЄрэфрЁЄэюх чэрўхэшх ЄрщьхЁр 0FFFFh
	out 40h, al
	out 40h, al
	pop bx
	pop ax
	db 0eah ; far jmp
	defaultHandler dd 0 ;..here
myHandler endp

; ╙ёЄрэртыштрхЄ фхёъЁшяЄюЁ
; handler_seg - ёхуьхэЄ ёЄрЁюую юсЁрсюЄўшър
; handler_offs 
; ╙ёЄрэютър фЁєуюую юсЁрсюЄўшър
setHandler proc PASCAL interrupt:word, handler_seg:word , handler_offs:word
	push ds
	push dx
	cli ; юЄъы■ўр■Є яЁхЁ√трэш 
		; т ёхуьхэЄх 0000 - ёё√ыъш эр юсЁрсюЄўшъш яЁхЁ√трэшщ
	xor ax, ax 
	mov ds, ax
	movzx eax, interrupt ; eax = 0000:interrupt
	mov dx, handler_seg     ; т dx - ёхуьхэЄ
	mov word ptr ds:[eax*4+2], dx   ; т 0000:interrupt*4 + 2 ёхуьхэЄ эютюую юсЁрсюЄўшър
	mov dx, handler_offs
	mov word ptr ds:[eax*4], dx ;  т 0000:interrupt*4 юЇЇёхЄ эютюую юсЁрсюЄўшър
	sti ;тъы■ўр■Є яЁхЁ√трэш■
	pop dx
	pop ds
	ret
setHandler endp

playIndicator proc
	push es
	push ds
	push bx
	push dx
	mov ax, 3508h   ; яхЁхїтрЄ яЁхЁ√трэшщ
	int 21h ; ёўшЄ√трхь рфЁхё юсЁрсюЄўшър яЁхЁ√трэшщ - ёюїЁрэшЄё  т es:bx
		; ёюїЁрэ хь ёЄрэфрЁЄэ√щ юсЁрсюЄўшъ
	mov word ptr defaultHandler, bx
	mov word ptr defaultHandler+2, es

	mov ax, offset indseq
	push ax
	call readAndParseByte   ; ьрёёшт ёўшЄрыё  т indseq
	mov cs:seq_len, al ; ; фышэр ьрёёштр
	push 08h
	push cs
	push offset myHandler
	call setHandler


chgIndicator: ; цфхь фюёЄєяр ъ ъыртшрЄєЁх
	in al, 64h
	cmp al, 0001b
	jz keyInBuffer
	in al, 64h ; ўЄхэшх ёюёЄю эш  ъыртшрЄєЁ√
	test al, 0010b ; яЁютхЁ хь, хёЄ№ ыш т сєЇхЁх ттюфр фрээ√х
	jnz chgIndicator ; хёыш сєЇхЁ эх ётюсюфхэ - цфхь
	mov al, 0edh ; шчьхэшЄ№ ёюёЄю эшх ётхЄюшфют
	out 60h, al ; чряшё√трхь т яюЁЄ ўЄхэш  ъыртшрЄєЁ√
	movzx eax, byte ptr cs:index    ; т index - эюьхЁ Єхъє∙хую шэфшърЄюЁр (т яюёыхфютрЄхы№эюёЄш)
	add ax, offset indseq     ; фюсрты хь эрўрыю ьрёёштр   
	mov al, [eax]	 ; чряшё√трхь срщЄ т al - ъюьрэфр фы  шэфшърЄюЁют
		cmp al, 0h
		jz isZeroN
		cmp al, 2h
		jg err_
		shl al, 1
	cont_:
     ;   add ax, offset indseq   
     ;   mov al, [eax]  ; чряшё√трхь срщЄ т al - ъюьрэфр фы  шэфшърЄюЁют
	out 60h, al     ; чряшё√трхь срщЄ т al - ъюьрэфр фы  шэфшърЄюЁют
	movzx ax, byte ptr cs:index     ; яюьх∙рхь т рl Єхъє∙шщ эюьхЁ 
	div byte ptr cs:seq_len ; фхышь эр фышэє яюёыхфютрЄхы№эюёЄш
	mov byte ptr cs:index, ah       ; чряшё√трхь т index юёЄрЄюъ, ўЄюс√ эх т√щЄш чр уЁрэшЎ√
	;;;;;;;;;;;;;
	jmp chgIndicator
	;;;;;;;;;;;
end_:
	push 08h
	push word ptr defaultHandler+2
	push word ptr defaultHandler
	call setHandler
	pop dx
	pop bx
	pop ds
	pop es
	ret
	counter db 0
	index db 0
	seq_len db 0
	isZeroN:
		mov al, 1h
		jmp cont_
	err_:
		mov dx, offset wrongNum
		mov ah, 09h
		int 21h
		mov ax, 4c00h
		int 21h
	keyInBuffer:
		in al, 60h
		cmp al, 2h
		je end_
		jmp chgIndicator
playIndicator endp

start:
	mov ax, data
	mov ds, ax
	mov es, ax

	call playIndicator

	mov ax, 4c00h
	int 21h

	ret

CODE ENDS
END start
