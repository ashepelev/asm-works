.386
TERMINALSYMBOL EQU '$'

DATA SEGMENT USE16 PUBLIC 'data'
emptyString byte 0Dh, 0Ah,TERMINALSYMBOL
string WORD 5 dup(0)
islp DB 'Year is leap$'
isnotlp DB 'Year is not leap$'
curDate DB 'Current date:$'
enterDate DB 'Enter date:$'
date STRUCT
    day word ?
    month word ?
    year word ?
date ENDS
mydate date <0,0,0> ; пустая структура даты

DATA ENDS

STACK SEGMENT STACK 'stack'
 db 1024 dup(0)
STACK ENDS

CODE SEGMENT USE16 PUBLIC 'code'


itoa PROC STDCALL, num: WORD
    LOCAL tmp: WORD ; основание системы счисления
    push cx
    push dx

    mov tmp,10
    mov ax,num
    xor cx,cx           ; кол-во цифр == 0
get_digit:
    xor dx,dx           ; хранит остаток от деления = 0
    div tmp             ; возьмем последнюю цифру
    push dx             ; и сохраним её на стеке
    inc cx

    test ax,ax
    jnz get_digit
    mov ah,02h          ; вывод
_next:
    pop dx
    add dl,'0'          ; перевод цифры в символ
    int 21h
    dec cx
    jnz _next
itoa_end:
    pop dx
    pop cx
    ret
itoa endp

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

; Считывает число длиной num
; Может считать ерунду а не число
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

; Считывает дату
; Может считать 45.14.2013
readDate PROC STDCALL, d: ptr date
	push bx
	push dx

	mov ah, 9
    mov dx, offset enterDate	; Выводим приглашение к вводу
    int 21h

	assume bx: ptr date
	mov bx, d
		
	push 2
	call readDigit				; Считываем число в 2 цифры
	mov [bx].day, ax
	
	mov ah, 01h					; Любое разделитель в 1 символ
	int 21h
	
	push 2
	call readDigit				; Считываем число в 2 цифры
	mov [bx].month, ax

	mov ah, 01h					; Любое разделитель в 1 символ
	int 21h

	push 4
	call readDigit				; Считываем число в 4 цифры
	mov [bx].year, ax

	pop dx
	pop bx
	ret
	
readDate ENDP

; Печатаем дату
writeDate PROC STDCALL, d: ptr date
    push ax
    push bx
    push dx

    assume bx:ptr date
    mov bx, d

    mov ax, [bx].day ; считываем день
    push ax
    call itoa
	mov ah,02h
	mov dl,'.'
	int 21h

	mov ax, [bx].month ; считываем месяц
	push ax
	call itoa
	mov ah,02h
	mov dl,'.'
	int 21h

	mov ax, [bx].year ; считываем год
	push ax
	call itoa

	pop dx
	pop bx
    pop ax
    ret
writeDate ENDP

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

printLeap PROC STDCALL, leap:WORD
	push dx
	mov ax, leap
	cmp ax, 0
	jz print_is_not_leap
	jnz print_is_leap
print_is_leap:
	mov dx, offset islp
	mov ah, 09h
	int 21h
	jmp procend
print_is_not_leap:
	mov dx, offset isnotlp
	mov ah, 09h
	int 21h
	jmp procend
procend:
	pop dx
	ret
printLeap ENDP

start:
    ; initialising segments
    mov ax, data
    mov ds, ax
    mov es, ax
    assume ds:data, es:data, cs:code, ss:stack

    push offset mydate
	call readDate
    call newline
	push offset mydate
    call writeDate ; печатаем текущую дату

    call newline
	; ПОСЛЕ ЭТОГО ИДЕТ ВАШ ОБРАБОТЧИК
	; ТЕКУЩИЙ МОЙ - ПРОВЕРКА НА ВИСОКОСНОСТЬ
    push offset mydate
    call isLeap ; проверям на високосность
    push ax
    call printLeap ; выводим сообщение

end_prog:
	xor al,al
	mov ax,4c00h
	int 21h
	ret
CODE ENDS
END start
