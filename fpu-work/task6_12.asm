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

exdoubletostr proc STDCALL
		LOCAL ten:WORD, temp:WORD
    ;   enter   4, 0            ; пролог - выделим в кадре стека 4 байта под локальные переменные
        mov     ten, 10		
        ftst                    ; определяем знак числа
        fstsw   ax				; Следующие две команды загружает в FLAGS флаги С0, С1, С3
        sahf					; Что позволяет выполняеть проверку carry, parity, zero флагов
								; C0 -> CF, C1 -> PF, C3 -> ZF
        jnc     @positiv
		mov ah, 02h
        mov     al, '-'         ; если число отрицательное - выводим минус
        int     21h
        fchs                    ; и получаем модуль числа
@positiv:        
		fld1                    ; загружаем единицу
        fld     st(1)           ; копируем число на вершину стека
        fprem                   ; выделим дробную часть
        fsub    st(2), st       ; отнимем ее от числа - получим целую часть
        fxch    st(2)           ; меняем местами целую и дробную части
        xor     cx, cx          ; обнуляем счетчик
; далее идет стандартный алгоритм вывода целого числа на экран
@1:     fidiv   ten             ; делим целую часть на десять
        fxch    st(1)           ; обменяем местами st и st(1) для команды fprem
        fld     st(1)           ; копируем результат на вершину стека 
        fprem                   ; выделим дробную часть (цифру справа от целой части)
        fsub    st(2), st       ; получим целую часть
        fimul   ten             ; *10
        fistp   temp            ; получаем очередную цифру и снимем его со стека
        push    temp            ; заталкиваем ее глубже в стек
        inc     cx              ; и увеличим счетчик
        fxch    st(1)           ; подготовим стек к следующему шагу цикла (полученное частное на вершину, в st(1) - 1)
        ftst                    ; проверим не получили ли в частном 0?
        fstsw   ax
        sahf
        jnz     @1              ; нет - продолжим цикл
@2:     pop     ax; извлекаем очередную цифру, переводим её в символ и выводим. 
		mov ah, 02h
        add     al, '0'
        int     21h
        loop    @2 				; В cx сидит длина целой части
; далее то же самое, только для дробной части. Алгоритм похож на вывод целого числа, только вместо деления умножение и проход по числу слева
        fstp    st              ; сначала проверим, есть ли дробная часть
        fxch    st(1)
        ftst
        fstsw   ax
        sahf
        jz      @quit           ; дробная часть отсутствует
        mov ah, 02h
		mov     al, '.'
        int     21h             ; если присутствует - выведем точку
        mov     cx, length_frac ; помещаем в счетчик длину дробной части
@3:        fimul   ten             ; умножим на 10
        fxch    st(1)           ; подготовка для fprem - меняем st и st(1) местами и
        fld     st(1)           ; копируем число на вершину
        fprem                   ; отделим дробную часть от целой
        fsub    st(2), st       ; и оставляем дробную
        fxch    st(2)
        fistp   temp            ; выталкиваем полученное число из стека в temp
        mov     ax, temp        ; по дробной части идем слева, значит число выводим сразу, без предварительного сохранения в стек
        mov 	ah, 02h
		add     al, 30h         ; перевод в ascii
        int     21h             ; на экран
        fxch    st(1)           ; подготовим стек к следующему шагу цикла (полученное частное на вершину, в st(1) - 1)
        ftst
        fstsw   ax
        sahf                    ; проверим на 0 остаток дробной части
        loopne  @3
@quit:        
		fstp                    ; готово. Чистим стек сопроцессора
        fstp    st                
        ret
exdoubletostr endp 
				

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
		
	finit		
	or cr, 0000000011000000b   ; Устанавливаем точность до 80 байт (расширенная двойная)
	push 5 ; строку такого размера можем считать (см. на размер буфера)
		push offset string ; передаём адрес начала буфера
		call readstring;
		mov si, offset string
	;	call strtoexdouble
	;	call newline
	;	call exdoubletostr
		

	xor al, al
	mov ax, 4c00h
	int 21h

	ret

CODE ENDS
END start
