.386
TERMINALSYMBOL  EQU '$'
DATA SEGMENT USE16 PUBLIC 'data'
string BYTE 20 dup(0)
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

strtoexdouble PROC STDCALL
	LOCAL	ten:WORD, value:WORD
        pusha
        mov             value, 0                    ; �������� ��������� ����������
        xor             bx, bx                          ; ������� ��������� �������
        cmp     byte ptr [si], '-'              ; ��������� �� ���������������
        jne             @POSITIVE_S2F           ; ����� �� �������������
        inc             bx                                      ; ����� �������������, ����������� �������
@POSITIVE_S2F:
        mov             value, 10           ; ��������� ����� � ����������
        fild            value                       ; ��������� � ���� ����� 10
        fldz                                                    ; ��������� � ���� ����� 0
@REPEAT_BEFORE:
        mov     al, byte ptr [si+bx]             ; �������� ������
        cmp     al, byte ptr '.'                        ; ��������� ����� �� ���
        je              @ISPOINTBEFORE  ; ����� - ���� ������
        cmp     al, byte ptr 13                 ; ��������� ����� �� �������
        je              @ENDASINT               ; ��� ����� � ������ ������ �����
        cmp     al, '0'                         ; ���� ������� ������ �� �����
        jc      @END_S2F_ERR                    ; �� ����� � �������
        cmp     al,'9'
        ja      @END_S2F_ERR
        sub             al, 30h                         ; ������ �� CHAR - INT
        mov             byte ptr value, al  ; �������� � ������
        fiadd   value                       ; ���������� �� ���, ��� ���� � �����
        fmul    st(0), st(1)                    ; �������� �� 10
        inc             bx                                      ; ����������� ���������
        jmp             @REPEAT_BEFORE  ; ���������
@ISPOINTBEFORE:
        inc             bx                                      ; ����������� ���������
        fdiv            st(0), st(1)                    ; ����� ����� �� 10, �.�. ��� ��� ������
        fxch            st(1)                           ; ������ ������� ��������
     ;   mov             al, byte ptr 13                 ; ���� ������ ����� ������
@FINDNEXT:
        cmp byte ptr [si+bx], 0dh                            ; ���� ����� ������
        je              @FINDEND                        ; ����� ����� ������
        inc             bx                                      ; �������� ��������� ����� �������
        jmp             @FINDNEXT                       ; �� �����, ��� ����
@FINDEND: 
		dec bx                                      ; ��������� �� ���������� ������
        fldz                                           ; ��������� � ���� ����� 0
@REPEAT_AFTER:
        mov             ax, word ptr [si+bx]             ; �������� ������
        cmp     al, byte ptr '.'                        ; ��������� ����� �� ���
        je              @WASPOINTAFTER  ; ����� - ���� ������
        cmp     al, '0'                 ; ���� ������� ������ �� �����
        jc      @END_S2F_ERR            ; �� ����� � �������
        cmp     al,'9'
        ja      @END_S2F_ERR
        sub             al, 30h                         ; ������ �� CHAR - INT
        mov             byte ptr value, al  ; �������� � ������
        fiadd   value                       ; ���������� �� ���, ��� ���� � �����
        fdiv            st(0), st(1)                    ; ����� �� 10
        dec             bx                                      ; �������������� ���������
        loop    @REPEAT_AFTER   ; ���������
@WASPOINTAFTER:
        fxch            st(1)                           ; ������ ����� 10 � ������� �������
        fxch            st(2)                           ; ������ ����� � 10 �������
        faddp   st(1), st(0)                           ; ���������� ����� �� � ����� �������
        fxch            st(1)                           ; ������ ������� ��������� � 10
        fistp           value                       ; ��������� �� ����� 10
        jmp             @FULLEND                        ; ������ ����� ���������
@ENDASINT:
        fdiv            st(0), st(1)                    ; ����� ����� �� 10, �.�. ��� ��� ������
        fxch            st(1)                           ; ������ ������� ��������
        fistp           value                       ; ��������� �� ����� 10
@FULLEND:
        cmp     byte ptr [si], '-'              ; ��������� �� ���������������
        jne             @END_S2F                        ; ����� �� �������������
        fchs                                                    ; ����� �������������, ������ ����
@END_S2F:
        popa                                            ; ��������� ��� ��������
        clc                                             ;������ ��� cl = 0
        ret                                                     ; ������� �� ���������
 
@END_S2F_ERR:
        popa                                            ; ��������� ��� ��������                        ;������� st0
        stc  ;������ cl = 1
        ret ; ������� �� ���������
strtoexdouble ENDP

exdoubletostr proc STDCALL
		LOCAL ten:WORD, temp:WORD
		pusha
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
getint:     
		fidiv   ten             ; ����� ����� ����� �� ������
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
        jnz     getint              ; ��� - ��������� ����
printint:     
		pop     ax; ��������� ��������� �����, ��������� � � ������ � �������. 
		mov ah, 02h
        add     al, '0'
        int     21h
        loop    printint 				; � cx ����� ����� ����� �����
; ����� �� �� �����, ������ ��� ������� �����. �������� ����� �� ����� ������ �����, ������ ������ ������� ��������� � ������ �� ����� �����
        fstp    st              ; ������� ��������, ���� �� ������� �����
        fxch    st(1)
        ftst
        fstsw   ax
        sahf
        jz      endproc           ; ������� ����� �����������
        mov ah, 02h
		mov     al, '.'
        int     21h             ; ���� ������������ - ������� �����
;        mov     cx, length_frac ; �������� � ������� ����� ������� �����
printmant:        
		ftst
        fstsw   ax
        sahf
        jz      endproc         ; ������� ����� �����������
		fimul   ten             ; ������� �� 10
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
   ;     loopne  printmant
		jmp printmant
endproc:        		
		fstp                    ; ������. ������ ���� ������������
        fstp    st  
		popa              
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
;	or cr, 0000000011000000b   ; ������������� �������� �� 80 ���� (����������� �������)
;	push 5 ; ������ ������ ������� ����� ������� (��. �� ������ ������)
		push offset string ; ������� ����� ������ ������
		call readstring;
	;	mov si, offset string
	;	call strtoexdouble
	;	call nextline
	;	call exdoubletostr
		

	xor al, al
	mov ax, 4c00h
	int 21h

	ret

CODE ENDS
END start
