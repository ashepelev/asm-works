.386
TERMINALSYMBOL  EQU '$'
DATA SEGMENT USE16 PUBLIC 'data'
string BYTE 6 dup(0)
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

exdoubletostr proc STDCALL
		LOCAL ten:WORD, temp:WORD
    ;   enter   4, 0            ; ������ - ������� � ����� ����� 4 ����� ��� ��������� ����������
        mov     ten, 10		
        ftst                    ; ���������� ���� �����
        fstsw   ax				; ��������� ��� ������� ��������� � FLAGS ����� �0, �1, �3
        sahf					; ��� ��������� ���������� �������� carry, parity, zero ������
								; C0 -> CF, C1 -> PF, C3 -> ZF
        jnc     @positiv
		mov ah, 02h
        mov     al, '-'         ; ���� ����� ������������� - ������� �����
        int     21h
        fchs                    ; � �������� ������ �����
@positiv:        
		fld1                    ; ��������� �������
        fld     st(1)           ; �������� ����� �� ������� �����
        fprem                   ; ������� ������� �����
        fsub    st(2), st       ; ������� �� �� ����� - ������� ����� �����
        fxch    st(2)           ; ������ ������� ����� � ������� �����
        xor     cx, cx          ; �������� �������
; ����� ���� ����������� �������� ������ ������ ����� �� �����
@1:     fidiv   ten             ; ����� ����� ����� �� ������
        fxch    st(1)           ; �������� ������� st � st(1) ��� ������� fprem
        fld     st(1)           ; �������� ��������� �� ������� ����� 
        fprem                   ; ������� ������� ����� (����� ������ �� ����� �����)
        fsub    st(2), st       ; ������� ����� �����
        fimul   ten             ; *10
        fistp   temp            ; �������� ��������� ����� � ������ ��� �� �����
        push    temp            ; ����������� �� ������ � ����
        inc     cx              ; � �������� �������
        fxch    st(1)           ; ���������� ���� � ���������� ���� ����� (���������� ������� �� �������, � st(1) - 1)
        ftst                    ; �������� �� �������� �� � ������� 0?
        fstsw   ax
        sahf
        jnz     @1              ; ��� - ��������� ����
@2:     pop     ax; ��������� ��������� �����, ��������� � � ������ � �������. 
		mov ah, 02h
        add     al, '0'
        int     21h
        loop    @2 				; � cx ����� ����� ����� �����
; ����� �� �� �����, ������ ��� ������� �����. �������� ����� �� ����� ������ �����, ������ ������ ������� ��������� � ������ �� ����� �����
        fstp    st              ; ������� ��������, ���� �� ������� �����
        fxch    st(1)
        ftst
        fstsw   ax
        sahf
        jz      @quit           ; ������� ����� �����������
        mov ah, 02h
		mov     al, '.'
        int     21h             ; ���� ������������ - ������� �����
        mov     cx, length_frac ; �������� � ������� ����� ������� �����
@3:        fimul   ten             ; ������� �� 10
        fxch    st(1)           ; ���������� ��� fprem - ������ st � st(1) ������� �
        fld     st(1)           ; �������� ����� �� �������
        fprem                   ; ������� ������� ����� �� �����
        fsub    st(2), st       ; � ��������� �������
        fxch    st(2)
        fistp   temp            ; ����������� ���������� ����� �� ����� � temp
        mov     ax, temp        ; �� ������� ����� ���� �����, ������ ����� ������� �����, ��� ���������������� ���������� � ����
        mov 	ah, 02h
		add     al, 30h         ; ������� � ascii
        int     21h             ; �� �����
        fxch    st(1)           ; ���������� ���� � ���������� ���� ����� (���������� ������� �� �������, � st(1) - 1)
        ftst
        fstsw   ax
        sahf                    ; �������� �� 0 ������� ������� �����
        loopne  @3
@quit:        
		fstp                    ; ������. ������ ���� ������������
        fstp    st                
        ret
exdoubletostr endp 
				

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
	or cr, 0000000011000000b   ; ������������� �������� �� 80 ���� (����������� �������)
	push 5 ; ������ ������ ������� ����� ������� (��. �� ������ ������)
		push offset string ; ������� ����� ������ ������
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
