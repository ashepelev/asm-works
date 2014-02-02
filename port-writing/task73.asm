.386
TERMINALSYMBOL  EQU '$'
DATA SEGMENT USE16 PUBLIC 'data'
membuf BYTE '4294967295$'
memarr BYTE 100 dup(0)
indseq BYTE 100 dup(0)
DATA ENDS

STACK SEGMENT STACK 'stack'
 db  1024 dup (0)
STACK ENDS


CODE SEGMENT USE16 PUBLIC 'code'
assume ds:data, es:data, cs:code, ss:stack

atoi PROC STDCALL,  strbuf:PTR BYTE ; ЇҐаҐў®¤ бва®ЄЁ ў жҐ«®Ґ зЁб«®
                LOCAL tmp:DWORD, strsize:WORD
                push si
                push ecx
                push bx
                push edx

                push strbuf ; Є« ¤Ґ¬ гЄ § вҐ«м ­  ­ з «® ЎгдҐа -бва®ЄЁ ­  бвҐЄ
                call strlen ; г§­ Ґ¬ Ґс ¤«Ё­г
                mov strsize, ax ; § Ї®¬Ё­ Ґ¬ ў «®Є «м­®© ЇҐаҐ¬Ґ­­®©
                mov si, strbuf ; Ј®в®ўЁ¬бп е®¤Ёвм Ї® бва®ЄҐ
                mov tmp, 10
                xor bx,bx ; ®Ў­г«пҐ¬ аҐЈЁбвал
                xor eax,eax

atoi_nextstep:
                cmp bx, strsize ; ­Ґ ¤®и«Ё «Ё ¤® Є®­ж  бва®ЄЁ
                jge atoi_endloop ; Ґб«Ё ¤®и«Ё - ўле®¤Ё¬
                movzx ecx, byte ptr [si+bx] ; Ё­ зҐ § Ї®¬Ё­ Ґ¬ ў аҐЈЁбваҐ cx вҐЄгйЁ© бЁ¬ў®« (§ ¤ свбп 16-вҐаЁз­л¬ Є®¤®¬)
                cmp cl , '0'
                jl atoi_endloop
                cmp cl, '9'
                jg atoi_endloop ; Ґб«Ё а §­®бвм Є®¤®ў бЁ¬ў®«®ў ¬Ґ­миҐ ­г«п Ё«Ё Ў®«миҐ 9-вЁ - ­ аў «Ёбм ­  Є®­Ґж бва®ЄЁ, б ¬® зЁб«® г¦Ґ ®Ўа Ў®в ­®
                sub cl, '0' ; Ё­ зҐ вҐЇҐам ў аҐЈЁбваҐ жЁда 
                mul tmp ; г¬­®¦ Ґ¬ ­  10 д®а¬ЁагҐ¬®Ґ зЁб«®
                add eax, ecx ; ЇаЁЎ ў«пҐ¬ ®зҐаҐ¤­го жЁдаг (Ё¤св ў ¬« ¤иЁ© а §ап¤)
                inc bx ; Ј®в®ўЁ¬бп а Ў®в вм б® б«Ґ¤гойЁ¬ бЁ¬ў®«®¬ бва®ЄЁ
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


writer proc PASCAL com:word, mbegin:dword, mend:dword ;not including end
      ;  int 3
        pushad ; сохраняем регистры
        push com
        call setupPort

        push ds
        mov ecx, mend
        sub ecx, mbegin ; длина передаваемых данных в байтах
        mov ebx, mbegin	; в bx - начало диапозона
        mov esi, 0fh 
        and esi, ebx ; в esi - offset
        shr ebx, 4 ; в ebx - segment
        mov ds, bx ; адрес сегмента - 16 бит берем как сегмент данных
        mov dx, com  ; в dx - порт COM
        cld
send_byte:           ;send bytes untill ecx==0
        mov al, byte ptr[si] ; по-умолчанию segment - в ds
        out dx, al	; берем байт по offset и кидаем в COM порт
        inc si		; сдвигаемся по диапозону
        test si, 10h	; Когда offset = 16
        jz pass_inc_ds	; если не равен то
        mov ax, ds	; сдвигаемся
        inc ax		; на следующий
        mov ds, ax	; сегментный адрес
        mov si, 0h	;
pass_inc_ds:
        dec ecx		; уменьшаем кол-во пройденных байт
        test ecx, ecx	; не дошли ли до конца
        jnz send_byte
        pop ds
        popad
        ret

writer endp

setupPort proc near PASCAL com:word ;some hardly understandable magic
        push es
        mov ax, com
        mov cs:cur_com, ax ; сохраняем текущий СОМ-порт

        mov dx, com ; СОМ-порты 03F8h-03FFh
        ;configure frequency of com-port
        add dx, 3; com+3; LCR
        mov al, 10000011b ; 03F8h и 03F9h - делители частоты порта
						  ; Длина слова - 8 бит
        out dx, al
        mov dx, com ;DLL
        mov al, 2 ; Устанавливаем младший делитель частоты порта
        out dx, al	; Будет работать в 2 раза медленней макс скорости
        inc dx ; 03F9h
        mov al, 0 ; Устанавливаем старший делитель частоты порта
        out dx, al ; В итоге - скорость в 2 раза медленней
        ;
        add dx, 2 ; com+3; 
        mov al, 0011b ; Возвращаем 03F8 и 03F9 старое значение
						; т.е. 03F8 - регистр передачи данных
						; 03F9 - IER
        out dx, al
		
        sub dx, 2
        mov al, 0	; Выполнять прерывание - если пришли новые данные
        out dx, al

        pop es
        ret
        cur_com dw 0
setupPort endp


COM_Writer proc
		
		push 3
		push offset membuf
		call readstring
		push offset membuf
		call atoi
		
		call nextline
		
		cmp ax, 1
		jz com1
		cmp ax, 2
		jz com2
		cmp ax, 3
		jz com3
		cmp ax, 4
		jz com4
		
		com1:
			mov ax, 03f8h
			jmp cont_
		com2:
			mov ax, 02f8h
			jmp cont_
		com3:
			mov ax, 03e8h
			jmp cont_
		com4:
			mov ax, 02e8h
			jmp cont_
		
	cont_:
	
        push ax ; первый аргумент (порт СОМ1)

        push 11
        push offset membuf
        call readstring		; считываем начало диапозона

        push offset membuf
        call atoi			; парсим
		
		call nextline

        push eax ; второй аргумент

        push 11
        push offset membuf
        call readstring		; считываем конец диапозона
		
		
        push offset membuf
        call atoi	; парсим
		
		call nextline
		
        push eax ; третий аргумент

        call writer
        ret
COM_Writer endp

start:
        mov ax, data
        mov ds, ax
        mov es, ax

        call COM_Writer

        mov ax, 4c00h
        int 21h

        ret

CODE ENDS
END start