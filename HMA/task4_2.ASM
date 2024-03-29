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
mydate date <0,0,0> ; ����� ������� ����

DATA ENDS

STACK SEGMENT STACK 'stack'
 db 1024 dup(0)
STACK ENDS

CODE SEGMENT USE16 PUBLIC 'code'


itoa PROC STDCALL, num: WORD
    LOCAL tmp: WORD ; �᭮����� ��⥬� ��᫥���
    push cx
    push dx

    mov tmp,10
    mov ax,num
    xor cx,cx           ; ���-�� ��� == 0
get_digit:
    xor dx,dx           ; �࠭�� ���⮪ �� ������� = 0
    div tmp             ; ���쬥� ��᫥���� ����
    push dx             ; � ��࠭�� �� �� �⥪�
    inc cx

    test ax,ax
    jnz get_digit
    mov ah,02h          ; �뢮�
_next:
    pop dx
    add dl,'0'          ; ��ॢ�� ���� � ᨬ���
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
       int 21h                  ; ������� � �����
       cmp al, 0dh              ;���������, ��� �� ����� 'enter'
       jz  endloop              ; if al == '\n'
       cmp al, 08               ; ���������, ��� �� ����� 'backspace'
       jnz _noback              ; if al != 'Backspace'
       test bx,bx       
       jz nextchar              ; if bx == 0, ������ �������� �� ����
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

; ���뢠�� �᫮ ������ num
; ����� ����� ��㭤� � �� �᫮
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

; ���뢠�� ����
; ����� ����� 45.14.2013
readDate PROC STDCALL, d: ptr date
	push bx
	push dx

	mov ah, 9
    mov dx, offset enterDate	; �뢮��� �ਣ��襭�� � �����
    int 21h

	assume bx: ptr date
	mov bx, d
		
	push 2
	call readDigit				; ���뢠�� �᫮ � 2 ����
	mov [bx].day, ax
	
	mov ah, 01h					; �� ࠧ����⥫� � 1 ᨬ���
	int 21h
	
	push 2
	call readDigit				; ���뢠�� �᫮ � 2 ����
	mov [bx].month, ax

	mov ah, 01h					; �� ࠧ����⥫� � 1 ᨬ���
	int 21h

	push 4
	call readDigit				; ���뢠�� �᫮ � 4 ����
	mov [bx].year, ax

	pop dx
	pop bx
	ret
	
readDate ENDP

; ���⠥� ����
writeDate PROC STDCALL, d: ptr date
    push ax
    push bx
    push dx

    assume bx:ptr date
    mov bx, d

    mov ax, [bx].day ; ���뢠�� ����
    push ax
    call itoa
	mov ah,02h
	mov dl,'.'
	int 21h

	mov ax, [bx].month ; ���뢠�� �����
	push ax
	call itoa
	mov ah,02h
	mov dl,'.'
	int 21h

	mov ax, [bx].year ; ���뢠�� ���
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

	mov ax, [bx].year ; ���뢠�� ���      
	cwd
	mov cx, 400 ; �஢��� ������� �� �� 400
	div cx
	cmp dx, 0
	jz yearIsLeap
	
	mov ax, [bx].year ; �஢��� ������� �� �� 100
	cwd
	mov cx, 100
	div cx
	cmp dx, 0
	jz yearIsNotLeap

	mov ax, [bx].year ; �஢��� ������� �� �� 4
	cwd
	mov cx, 4
	div cx
	cmp dx, 0
	jz yearIsLeap
	
	jmp yearIsNotLeap ; �� ��� ��⠫��� ����� - ����᮪���

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
    call writeDate ; ���⠥� ⥪���� ����

    call newline
	; ����� ����� ���� ��� ����������
	; ������� ��� - �������� �� ������������
    push offset mydate
    call isLeap ; �஢��� �� ��᮪�᭮���
    push ax
    call printLeap ; �뢮��� ᮮ�饭��

end_prog:
	xor al,al
	mov ax,4c00h
	int 21h
	ret
CODE ENDS
END start
