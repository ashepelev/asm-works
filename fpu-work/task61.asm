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
strlen PROC PASCAL, strBuffer:ptr byte ; ������� ��� ����������� ����� ������
	push di ;�������� �� ���� ������ �������� ��������
	push cx ; ���������� ��������
	mov di, strBuffer ; ��� ��� �����
	mov al, TERMINALSYMBOL ; ��� ������ ����� ������
	mov cx, 0ffffh ; ������� � ������ ������ ����� "��������"
	cld             ; ���������� ���������� ����������� - ���������� ��������� di
	repnz scasb ; �������� �� ������ � ������� ������� �����
	jnz   bad_string ; ���� �� ����� ������ �����
	neg cx
	sub cx,2 ; ����� �����
bad_string:
	mov ax, cx ; ��������� ����� � �������, � ������� ������� ������� ��������� ����������
	pop cx ; ������������ ��������
	pop di
	ret
strlen ENDP

itoa PROC STDCALL, i:WORD
				LOCAL tmp:BYTE
				push bx                 
				push dx
				push cx
				push ax                                

				mov bx, 10 ; �� ���� ���� ������
				mov ax, i
				xor cx, cx ; ������� ����
				stack_push:
					xor dx, dx ; ������
					div bx; dx = ax % 10, ax = ax / 10
					push dx ; ������ �� ���� ������ ������
					inc cx
					cmp ax, 0 ; ���� �������� �� ����v
					jnz stack_push
					mov ah, 02h
				stack_pop:
					pop dx ; ������� �� ����� ����v ����� ����� �������
					add dl, '0' ; �������� ��� ������� ����v
					int 21h ; �v����� ���
					dec cx
					jnz stack_pop
				pop ax
				pop cx
				pop dx
				pop bx
				ret
itoa ENDP


memcpy PROC STDCALL, dest: PTR BYTE, src: PTR BYTE, memsize:WORD ; ����������� ������� ������
       push di
       push si
       push cx
       mov di, dest
       mov si, src
       mov cx, memsize
       rep movsb ; �������� ����� ������� ���, ����� ������ ������� ������

       pop cx ; ��������������� ��������
       pop si
       pop di
       ret
memcpy ENDP


readstring  PROC STDCALL, strbuff: ptr byte, strsize: word
       push bx
       push si
       xor  bx,bx
       mov  si, strbuff
       mov  ah, 01 ; ��� ������ ���������� ����� ������ ������

nextchar:
       cmp bx, strsize ; ���� �� ��������� ������ ������
       jge endloop
       int 21h ; �������� ����������
       cmp al, 0dh ;��������� ��� ����� ������ �������� �� ����� �������
       jz  endloop ; ���� ������ ������ ���� ���������, ���� ���� ������ Enter, ������� �� ���������
       cmp al, 08 ; �� ������ �� ����-������ �������
       jnz _noback ; ���� �� ������ - �� ok � ������ ����������� � �����
       test bx,bx ; ���� ������ - �������, �� ������� �� � ��� � ��������� ������ 0 ��������
       jz nextchar ; ���� �������� � ������ � ������ 0, ������ ������ ���������
       dec bx ; ����� �������� ���� ������
       jmp nextchar ; � ����������� ������ ���������
_noback:
       mov byte ptr [si+bx], al
       inc bx ; ������ ����������� �� 1 ������
       jmp nextchar
endloop:
       mov byte ptr [si+bx], TERMINALSYMBOL ; ��������� ������� ����� ������
       mov ax, bx ; ���������� �����

       pop si
       pop bx
       ret
readstring ENDP

STRTOFLOAT      PROC STDCALL, strd:WORD
		LOCAL value:WORD
	pusha
	mov             value, 0                    ; �������� ��������� ����������
	xor             bx, bx                          ; ������� ��������� �������
	mov si, strd
		cmp     byte ptr [si], '-'              ; ��������� �� ���������������
	jne             ispositive           ; ����� �� �������������
	inc             bx                                      ; ����� �������������, ����������� �������
ispositive:
	mov             value, 10           ; ��������� ����� � ����������
	fild            value                       ; ��������� � ���� ����� 10
	fldz                                                    ; ��������� � ���� ����� 0
readint:
	mov             al, byte ptr [si+bx]             ; �������� ������
	cmp     al, '.'                        ; ��������� ����� �� ���
	je              pointfound  ; ����� - ���� ������
	cmp     al, '$'                 ; ��������� ����� �� �������
	je              isint               ; ��� ����� � ������ ������ �����
	cmp     al, '0'                         ; ���� ������� ������ �� �����
	jc      _error                    ; �� ����� � �������
	cmp     al,'9'
	ja      _error
	sub             al, 30h                         ; ������ �� CHAR - INT
	mov             byte ptr value, al  ; �������� � ������
	fiadd   value                       ; ���������� �� ���, ��� ���� � �����
	fmul    st(0), st(1)                    ; �������� �� 10
	inc             bx                                      ; ����������� ���������
	jmp             readint  ; ���������
pointfound:
	inc             bx                                      ; ����������� ���������
	fdiv            st(0), st(1)                    ; ����� ����� �� 10, �.�. ��� ��� ������
	fxch            st(1)                           ; ������ ������� ��������
	mov             al, '$'                 ; ���� ������ ����� ������
findend:
	cmp  byte ptr   [si+bx], al                              ; ���� ����� ������
	je              @findend1                        ; ����� ����� ������
	inc             bx                                      ; �������� ��������� ����� �������
	jmp             findend                      ; �� �����, ��� ����
@findend1: 
	dec bx                                      ; ��������� �� ���������� ������
	fldz                                                    ; ��������� � ���� ����� 0
readmantis:
	mov  al, byte ptr [si+bx]             ; �������� ������
	int 3h
	cmp     al, '.'                        ; ��������� ����� �� ���
	je              cleanfpu  ; ����� - ���� ������
	cmp     al, '0'                 ; ���� ������� ������ �� �����
	jc      _error            ; �� ����� � �������
	cmp     al,'9'
	ja      _error
	mov ah, 0h
	sub             al, 30h                         ; ������ �� CHAR - INT
	mov value, ax ; �������� � ������
	fiadd   value                       ; ���������� �� ���, ��� ���� � �����
	fdiv            st(0), st(1)                    ; ����� �� 10
	dec             bx                                      ; �������������� ���������
	loop    readmantis   ; ���������
cleanfpu:
	fxch            st(1)                           ; ������ ����� 10 � ������� �������
	fxch            st(2)                           ; ������ ����� � 10 �������
	faddp   st(1), st(0)                           ; ���������� ����� �� � ����� �������
	fxch            st(1)                           ; ������ ������� ��������� � 10
	fistp           value                       ; ��������� �� ����� 10
	jmp             _preexit                        ; ������ ����� ���������
isint:
	fdiv            st(0), st(1)                    ; ����� ����� �� 10, �.�. ��� ��� ������
	fxch            st(1)                           ; ������ ������� ��������
	fistp           value                       ; ��������� �� ����� 10
_preexit:
	cmp     byte ptr [si], '-'              ; ��������� �� ���������������
	jne             _exit                        ; ����� �� �������������
	fchs                                                    ; ����� �������������, ������ ����
_exit:
	popa                                            ; ��������� ��� ��������	
	ret
 
_error:
	popa                                            ; ��������� ��� ��������
	mov dx, offset wronginput
	mov ah, 09h
	int 21h
	mov ax, 4c00h
	int 21h
	ret ; ������� �� ���������
STRTOFLOAT      ENDP

OutFloat proc  STDCALL, lmantis:WORD
		LOCAL temp:WORD, ten:WORD
	pusha
  ;      enter   4, 0            ; ������ - ������� � ����� ����� 4 ����� ��� ��������� ����������
	mov     ten, 10
	ftst                    ; ���������� ���� �����
	fstsw   ax
	sahf
	jnc     ispositive
	mov     al, '-'         ; ���� ����� ������������� - ������� �����
	int     29h
	fchs                    ; � �������� ������ �����
ispositive:        fld1                    ; ��������� �������
	fld     st(1)           ; �������� ����� �� ������� �����
	fprem                   ; ������� ������� �����
	fsub    st(2), st       ; ������� �� �� ����� - ������� ����� �����
	fxch    st(2)           ; ������ ������� ����� � ������� �����
	xor     cx, cx          ; �������� �������
; ����� ���� ����������� �������� ������ ������ ����� �� �����
readint:        fidiv   ten             ; ����� ����� ����� �� ������
	fxch    st(1)           ; �������� ������� st � st(1) ��� ������� fprem
	fld     st(1)           ; �������� ��������� �� ������� ����� 
	fprem                   ; ������� ������� ����� (����� ������ �� ����� �����)
	fsub    st(2), st       ; ������� ����� �����
	fimul   ten             ; *10
	fistp   temp            ; �������� ��������� �����      
	push    temp            ; ����������� �� ������ � ����
	inc     cx              ; � �������� �������
	fxch    st(1)           ; ���������� ���� � ���������� ���� ����� (���������� ������� �� �������, � st(1) - 1)
	ftst                    ; �������� �� �������� �� � ������� 0?
	fstsw   ax
	sahf
	jnz     readint              ; ��� - ��������� ����
printint:     
	pop     ax; ��������� ��������� �����, ��������� � � ������ � �������.        
	add     al, '0'
	int     29h
	loop    printint
; ����� �� �� �����, ������ ��� ������� �����. �������� ����� �� ����� ������ �����, ������ ������ ������� ��������� � ������ �� ����� �����
	fstp    st              ; ������� ��������, ���� �� ������� �����
	fxch    st(1)
	ftst
	fstsw   ax
	sahf
	jz      @quit           ; ������� ����� �����������
	mov     al, '.'
	int     29h             ; ���� ������������ - ������� �����
	mov     cx, lmantis ; �������� � ������� ����� ������� �����
printmantis:        
	fimul   ten             ; ������� �� 10
	fxch    st(1)           ; ���������� ��� fprem - ������ st � st(1) ������� �
	fld     st(1)           ; �������� ����� �� �������
	fprem                   ; ������� ������� ����� �� �����
	fsub    st(2), st       ; � ��������� �������
	fxch    st(2)
	fistp   temp            ; ����������� ���������� ����� �� ����� � temp
	mov     ax, temp        ; �� ������� ����� ���� �����, ������ ����� ������� �����, ��� ���������������� ���������� � ����
	or      al, 30h         ; ������� � ascii
	int     29h             ; �� �����
	fxch    st(1)           ; ���������� ���� � ���������� ���� ����� (���������� ������� �� �������, � st(1) - 1)
	ftst
	fstsw   ax
	sahf                    ; �������� �� 0 ������� ������� �����
	loopne printmantis
@quit:        
		fstp st                   ; ������. ������ ���� ������������
	fstp    st
;       leave                   ; ������
	popa
	ret 
OutFloat endp 

				

nextline PROC ; ���� �� ������ � �� ����� ������� ����������!
	push dx
	push ax

	mov ah, 2 ; ����� �������� �������
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
       push 20 ; ������ ������ ������� ����� ������� (��. �� ������ ������)
		push offset string ; ������� ����� ������ ������
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
