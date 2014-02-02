.386
TERMINALSYMBOL  EQU '$'
DATA SEGMENT USE16 PUBLIC 'data'
string BYTE 20 dup(0)
cr dw 0abh
number dt 32.382139
entern db 'Enter float number: $'
wronginput db 'Wrong float format$'
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
					cmp ax, 0 ; хёыш яюыєўшыш тёх ЎшЇЁv
					jnz stack_push
					mov ah, 02h
				stack_pop:
					pop dx ; ёэшьрхь ёю ёЄхър ЎшЇЁv ўшёыр ёыхтр эряЁртю
					add dl, '0' ; яюыєўрхь ъюф ёшьтюыр ЎшЇЁv
					int 21h ; тvтюфшь хую
					dec cx
					jnz stack_pop
				pop ax
				pop cx
				pop dx
				pop bx
				ret
itoa ENDP


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

STRTOFLOAT      PROC STDCALL, strd:WORD
		LOCAL value:WORD
	pusha
	mov             value, 0                    ; обнуляем локальную переменную
	xor             bx, bx                          ; очищаем указатель позиции
	mov si, strd
		cmp     byte ptr [si], '-'              ; проверяем на отрицательность
	jne             ispositive           ; число не отрицательное
	inc             bx                                      ; число отрицательное, увеличиваем позицию
ispositive:
	mov             value, 10           ; загружаем число в переменную
	fild            value                       ; загружаем в стэк число 10
	fldz                                                    ; загружаем в стэк число 0
readint:
	mov             al, byte ptr [si+bx]             ; получаем символ
	cmp     al, '.'                        ; проверяем точка ли это
	je              pointfound  ; точка - идем дальше
	cmp     al, '$'                 ; проверяем конец ли строкиа
	je              isint               ; уже конец и хватит искать дроби
	cmp     al, '0'                         ; если текущий символ не цифра
	jc      _error                    ; то выход с ошибкой
	cmp     al,'9'
	ja      _error
	sub             al, 30h                         ; делаем из CHAR - INT
	mov             byte ptr value, al  ; копируем в память
	fiadd   value                       ; складываем из тем, что есть в стэке
	fmul    st(0), st(1)                    ; умножаем на 10
	inc             bx                                      ; увеличиваем указатель
	jmp             readint  ; повторяем
pointfound:
	inc             bx                                      ; увеличиваем указатель
	fdiv            st(0), st(1)                    ; делим число на 10, т.к. оно уже больше
	fxch            st(1)                           ; меняем местами регистры
	mov             al, '$'                 ; ищем символ конца строки
findend:
	cmp  byte ptr   [si+bx], al                              ; ищем конец строки
	je              @findend1                        ; нешел конец строки
	inc             bx                                      ; получаем следующий адрес символа
	jmp             findend                      ; не нашел, еще ищем
@findend1: 
	dec bx                                      ; переходим на предыдущий символ
	fldz                                                    ; загружаем в стэк число 0
readmantis:
	mov  al, byte ptr [si+bx]             ; получаем символ
	int 3h
	cmp     al, '.'                        ; проверяем точка ли это
	je              cleanfpu  ; точка - идем дальше
	cmp     al, '0'                 ; если текущий символ не цифра
	jc      _error            ; то выход с ошибкой
	cmp     al,'9'
	ja      _error
	mov ah, 0h
	sub             al, 30h                         ; делаем из CHAR - INT
	mov value, ax ; копируем в память
	fiadd   value                       ; складываем из тем, что есть в стэке
	fdiv            st(0), st(1)                    ; делим на 10
	dec             bx                                      ; декрементируем указатель
	loop    readmantis   ; повторяем
cleanfpu:
	fxch            st(1)                           ; меняем число 10 и остаток местами
	fxch            st(2)                           ; меняем целое и 10 местами
	faddp   st(1), st(0)                           ; складываем число до и после запятой
	fxch            st(1)                           ; меняем местами результат и 10
	fistp           value                       ; извлекаем из стэка 10
	jmp             _preexit                        ; полный конец процедуры
isint:
	fdiv            st(0), st(1)                    ; делим число на 10, т.к. оно уже больше
	fxch            st(1)                           ; меняем местами регистры
	fistp           value                       ; извлекаем из стэка 10
_preexit:
	cmp     byte ptr [si], '-'              ; проверяем на отрицательность
	jne             _exit                        ; число не отрицательное
	fchs                                                    ; число отрицательное, меняем знак
_exit:
	popa                                            ; выгружаем все регистры	
	ret
 
_error:
	popa                                            ; выгружаем все регистры
	mov dx, offset wronginput
	mov ah, 09h
	int 21h
	mov ax, 4c00h
	int 21h
	ret ; возврат из процедуры
STRTOFLOAT      ENDP

OutFloat proc  STDCALL, lmantis:WORD
		LOCAL temp:WORD, ten:WORD
	pusha
  ;      enter   4, 0            ; пролог - выделим в кадре стека 4 байта под локальные переменные
	mov     ten, 10
	ftst                    ; определяем знак числа
	fstsw   ax
	sahf
	jnc     ispositive
	mov     al, '-'         ; если число отрицательное - выводим минус
	int     29h
	fchs                    ; и получаем модуль числа
ispositive:        fld1                    ; загружаем единицу
	fld     st(1)           ; копируем число на вершину стека
	fprem                   ; выделим дробную часть
	fsub    st(2), st       ; отнимем ее от числа - получим целую часть
	fxch    st(2)           ; меняем местами целую и дробную части
	xor     cx, cx          ; обнуляем счетчик
; далее идет стандартный алгоритм вывода целого числа на экран
readint:        fidiv   ten             ; делим целую часть на десять
	fxch    st(1)           ; обменяем местами st и st(1) для команды fprem
	fld     st(1)           ; копируем результат на вершину стека 
	fprem                   ; выделим дробную часть (цифру справа от целой части)
	fsub    st(2), st       ; получим целую часть
	fimul   ten             ; *10
	fistp   temp            ; получаем очередную цифру      
	push    temp            ; заталкиваем ее глубже в стек
	inc     cx              ; и увеличим счетчик
	fxch    st(1)           ; подготовим стек к следующему шагу цикла (полученное частное на вершину, в st(1) - 1)
	ftst                    ; проверим не получили ли в частном 0?
	fstsw   ax
	sahf
	jnz     readint              ; нет - продолжим цикл
printint:     
	pop     ax; извлекаем очередную цифру, переводим её в символ и выводим.        
	add     al, '0'
	int     29h
	loop    printint
; далее то же самое, только для дробной части. Алгоритм похож на вывод целого числа, только вместо деления умножение и проход по числу слева
	fstp    st              ; сначала проверим, есть ли дробная часть
	fxch    st(1)
	ftst
	fstsw   ax
	sahf
	jz      @quit           ; дробная часть отсутствует
	mov     al, '.'
	int     29h             ; если присутствует - выведем точку
	mov     cx, lmantis ; помещаем в счетчик длину дробной части
printmantis:        
	fimul   ten             ; умножим на 10
	fxch    st(1)           ; подготовка для fprem - меняем st и st(1) местами и
	fld     st(1)           ; копируем число на вершину
	fprem                   ; отделим дробную часть от целой
	fsub    st(2), st       ; и оставляем дробную
	fxch    st(2)
	fistp   temp            ; выталкиваем полученное число из стека в temp
	mov     ax, temp        ; по дробной части идем слева, значит число выводим сразу, без предварительного сохранения в стек
	or      al, 30h         ; перевод в ascii
	int     29h             ; на экран
	fxch    st(1)           ; подготовим стек к следующему шагу цикла (полученное частное на вершину, в st(1) - 1)
	ftst
	fstsw   ax
	sahf                    ; проверим на 0 остаток дробной части
	loopne printmantis
@quit:        
		fstp st                   ; готово. Чистим стек сопроцессора
	fstp    st
;       leave                   ; эпилог
	popa
	ret 
OutFloat endp 

				

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
	fstcw cr
	or cr,  0000000011000000b
	fldcw cr
	mov dx, offset entern
	mov ah, 09h
	int 21h
       push 20 ; строку такого размера можем считать (см. на размер буфера)
		push offset string ; передаём адрес начала буфера
		call readstring;
	;       mov si, offset string
	       push offset string
	       call strtofloat
	       call nextline
	       push 5
	       call outfloat
		

	xor al, al
	mov ax, 4c00h
	int 21h

	ret

CODE ENDS
END start
