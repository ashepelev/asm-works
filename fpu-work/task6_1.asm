.386
TERMINALSYMBOL  EQU '$'
DATA SEGMENT USE16 PUBLIC 'data'
string BYTE 6 dup(0)
DATA ENDS

STACK SEGMENT STACK 'stack'
 db  1024 dup (0)
STACK ENDS


CODE SEGMENT USE16 PUBLIC 'code'
strlen PROC PASCAL, strBuffer:ptr byte ; �㭪�� ��� ��।������ ����� ��ப�
	push di ;�������� �� �⥪ ��஥ ���祭�� ॣ����
	push cx ; ���������� ���⥪��
	mov di, strBuffer ; �� ��� ����
	mov al, TERMINALSYMBOL ; �� ᨬ��� ���� ��ப�
	mov cx, 0ffffh ; �⮫쪮 � ��襬 ��砥 �㤥� "���権"
	cld             ; ��⠭����� �ࠢ��쭮� ���ࠢ����� - �ந�室�� ���६��� di
	repnz scasb ; ��室�� �� ��ப� � ���᪠� ᨬ���� ����
	jnz   bad_string ; �᫨ �� ��諨 ᨬ��� ����
	neg cx
	sub cx,2 ; ��諨 �����
bad_string:
	mov ax, cx ; �����㫨 ����� � ॣ����, � ���஬ ������� 㢨���� १���� �믮������
	pop cx ; ����⠭����� ���⥪��
	pop di
	ret
strlen ENDP


memcpy PROC STDCALL, dest: PTR BYTE, src: PTR BYTE, memsize:WORD ; ����஢���� ������ �����
       push di
       push si
       push cx
       mov di, dest
       mov si, src
       mov cx, memsize
       rep movsb ; �����㥬 ����� �⮫쪮 ࠧ, ����� ࠧ��� ������ �����

       pop cx ; ����⠭�������� ���⥪��
       pop si
       pop di
       ret
memcpy ENDP


readstring  PROC STDCALL, strbuff: ptr byte, strsize: word
       push bx
       push si
       xor  bx,bx
       mov  si, strbuff
       mov  ah, 01 ; �� �맮�� ���뢠��� �㤥� ���� ᨬ���

nextchar:
       cmp bx, strsize ; ���� �� �ॢ�ᨫ� ࠧ��� ����
       jge endloop
       int 21h ; ��뢠�� ���뢠���
       cmp al, 0dh ;�஢��塞 �� ����� ᨬ��� ���室� �� ����� �����
       jz  endloop ; �᫨ ࠧ��� ���� ⠪� �ॢ�ᨫ�, ���� ⠪� ������ Enter, ��⠭� ��� ���������
       cmp al, 08 ; �� �訫� �� 祣�-����� �����
       jnz _noback ; �᫨ �� �訫� - ��� ok � ᨬ��� ���������� � ����
       test bx,bx ; �᫨ �訫� - ᬮ�ਬ, �� ������� �� � ��� � �����騩 ������ 0 ᨬ�����
       jz nextchar ; �᫨ ᨬ����� � ���� � �ࠢ�� 0, ���� �⠥� ᫥���騩
       dec bx ; ���� �모�㫨 ���� ᨬ���
       jmp nextchar ; � ��ࠢ����� ���� ᫥���騩
_noback:
       mov byte ptr [si+bx], al
       inc bx ; ��ப� 㢥��稫��� �� 1 ᨬ���
       jmp nextchar
endloop:
       mov byte ptr [si+bx], TERMINALSYMBOL ; ������塞 �ਧ��� ���� ��ப�
       mov ax, bx ; ���������� �����

       pop si
       pop bx
       ret
readstring ENDP

strtoexdouble PROC STDCALL
	LOCAL	ten:WORD, value:WORD
        pusha
        mov             value, 0                    ; ����塞 �������� ��६�����
        xor             bx, bx                          ; ��頥� 㪠��⥫� ����樨
        cmp     byte ptr [si], '-'              ; �஢��塞 �� ����⥫쭮���
        jne             @POSITIVE_S2F           ; �᫮ �� ����⥫쭮�
        inc             bx                                      ; �᫮ ����⥫쭮�, 㢥��稢��� ������
@POSITIVE_S2F:
        mov             value, 10           ; ����㦠�� �᫮ � ��६�����
        fild            value                       ; ����㦠�� � ��� �᫮ 10
        fldz                                                    ; ����㦠�� � ��� �᫮ 0
@REPEAT_BEFORE:
        mov             al, byte ptr si[bx]             ; ����砥� ᨬ���
        cmp     al, byte ptr '.'                        ; �஢��塞 �窠 �� ��
        je              @ISPOINTBEFORE  ; �窠 - ���� �����
        cmp     al, byte ptr 13                 ; �஢��塞 ����� �� ��ப��
        je              @ENDASINT               ; 㦥 ����� � 墠�� �᪠�� �஡�
        cmp     al, '0'                         ; �᫨ ⥪�騩 ᨬ��� �� ���
        jc      @END_S2F_ERR                    ; � ��室 � �訡���
        cmp     al,'9'
        ja      @END_S2F_ERR
        sub             al, 30h                         ; ������ �� CHAR - INT
        mov             byte ptr value, al  ; �����㥬 � ������
        fiadd   value                       ; ᪫��뢠�� �� ⥬, �� ���� � ���
        fmul    st(0), st(1)                    ; 㬭����� �� 10
        inc             bx                                      ; 㢥��稢��� 㪠��⥫�
        jmp             @REPEAT_BEFORE  ; �����塞
@ISPOINTBEFORE:
        inc             bx                                      ; 㢥��稢��� 㪠��⥫�
        fdiv            st(0), st(1)                    ; ����� �᫮ �� 10, �.�. ��� 㦥 �����
        fxch            st(1)                           ; ���塞 ���⠬� ॣ�����
     ;   mov             al, byte ptr 13                 ; �饬 ᨬ��� ���� ��ப�
@FINDNEXT:
        cmp     si[bx], 0dh                            ; �饬 ����� ��ப�
        je              @FINDEND                        ; ��襫 ����� ��ப�
        inc             bx                                      ; ����砥� ᫥���騩 ���� ᨬ����
        jmp             @FINDNEXT                       ; �� ��襫, �� �饬
@FINDEND: 
		dec bx                                      ; ���室�� �� �।��騩 ᨬ���
        fldz                                           ; ����㦠�� � ��� �᫮ 0
@REPEAT_AFTER:
        mov             ax, word ptr si[bx]             ; ����砥� ᨬ���
        cmp     al, byte ptr '.'                        ; �஢��塞 �窠 �� ��
        je              @WASPOINTAFTER  ; �窠 - ���� �����
        cmp     al, '0'                 ; �᫨ ⥪�騩 ᨬ��� �� ���
        jc      @END_S2F_ERR            ; � ��室 � �訡���
        cmp     al,'9'
        ja      @END_S2F_ERR
        sub             al, 30h                         ; ������ �� CHAR - INT
        mov             byte ptr value, al  ; �����㥬 � ������
        fiadd   value                       ; ᪫��뢠�� �� ⥬, �� ���� � ���
        fdiv            st(0), st(1)                    ; ����� �� 10
        dec             bx                                      ; ���६����㥬 㪠��⥫�
        loop    @REPEAT_AFTER   ; �����塞
@WASPOINTAFTER:
        fxch            st(1)                           ; ���塞 �᫮ 10 � ���⮪ ���⠬�
        fxch            st(2)                           ; ���塞 楫�� � 10 ���⠬�
        faddp   st(1), st(0)                           ; ᪫��뢠�� �᫮ �� � ��᫥ ����⮩
        fxch            st(1)                           ; ���塞 ���⠬� १���� � 10
        fistp           value                       ; ��������� �� ��� 10
        jmp             @FULLEND                        ; ����� ����� ��楤���
@ENDASINT:
        fdiv            st(0), st(1)                    ; ����� �᫮ �� 10, �.�. ��� 㦥 �����
        fxch            st(1)                           ; ���塞 ���⠬� ॣ�����
        fistp           value                       ; ��������� �� ��� 10
@FULLEND:
        cmp     byte ptr [si], '-'              ; �஢��塞 �� ����⥫쭮���
        jne             @END_S2F                        ; �᫮ �� ����⥫쭮�
        fchs                                                    ; �᫮ ����⥫쭮�, ���塞 ����
@END_S2F:
        popa                                            ; ���㦠�� �� ॣ�����
        clc                                             ;�訡�� ��� cl = 0
        ret                                                     ; ������ �� ��楤���
 
@END_S2F_ERR:
        popa                                            ; ���㦠�� �� ॣ�����                        ;��頥� st0
        stc  ;�訡�� cl = 1
        ret ; ������ �� ��楤���
strtoexdouble ENDP

exdoubletostr proc STDCALL
		LOCAL ten:WORD, temp:WORD
		pusha
    ;   enter   4, 0            ; �஫�� - �뤥��� � ���� �⥪� 4 ���� ��� ������� ��६����
        mov     ten, 10		
        ftst                    ; ��।��塞 ���� �᫠
        fstsw   ax				; ������騥 ��� ������� ����㦠�� � FLAGS 䫠�� �0, �1, �3
        sahf					; �� �������� �믮����� �஢��� carry, parity, zero 䫠���
								; C0 -> CF, C1 -> PF, C3 -> ZF
        jnc     @positiv
		mov ah, 02h
        mov     al, '-'         ; �᫨ �᫮ ����⥫쭮� - �뢮��� �����
        int     21h
        fchs                    ; � ����砥� ����� �᫠
@positiv:        
		fld1                    ; ����㦠�� �������
        fld     st(1)           ; �����㥬 �᫮ �� ���設� �⥪�
        fprem                   ; �뤥��� �஡��� ����
        fsub    st(2), st       ; �⭨��� �� �� �᫠ - ����稬 楫�� ����
        fxch    st(2)           ; ���塞 ���⠬� 楫�� � �஡��� ���
        xor     cx, cx          ; ����塞 ���稪
; ����� ���� �⠭����� ������ �뢮�� 楫��� �᫠ �� �࠭
getint:     fidiv   ten             ; ����� 楫�� ���� �� ������
        fxch    st(1)           ; �����塞 ���⠬� st � st(1) ��� ������� fprem
        fld     st(1)           ; �����㥬 १���� �� ���設� �⥪� 
        fprem                   ; �뤥��� �஡��� ���� (���� �ࠢ� �� 楫�� ���)
        fsub    st(2), st       ; ����稬 楫�� ����
        fimul   ten             ; *10
        fistp   temp            ; ����砥� ��।��� ���� � ᭨��� ��� � �⥪�
        push    temp            ; ��⠫������ �� ��㡦� � �⥪
        inc     cx              ; � 㢥��稬 ���稪
        fxch    st(1)           ; �����⮢�� �⥪ � ᫥���饬� 蠣� 横�� (����祭��� ��⭮� �� ���設�, � st(1) - 1)
        ftst                    ; �஢�ਬ �� ����稫� �� � ��⭮� 0?
        fstsw   ax
        sahf
        jnz     getint              ; ��� - �த����� 横�
printint:     pop     ax; ��������� ��।��� ����, ��ॢ���� �� � ᨬ��� � �뢮���. 
		mov ah, 02h
        add     al, '0'
        int     21h
        loop    printint 				; � cx ᨤ�� ����� 楫�� ���
; ����� � �� ᠬ��, ⮫쪮 ��� �஡��� ���. ������ ��宦 �� �뢮� 楫��� �᫠, ⮫쪮 ����� ������� 㬭������ � ��室 �� ��� ᫥��
        fstp    st              ; ᭠砫� �஢�ਬ, ���� �� �஡��� ����
        fxch    st(1)
        ftst
        fstsw   ax
        sahf
        jz      endproc           ; �஡��� ���� ���������
        mov ah, 02h
		mov     al, '.'
        int     21h             ; �᫨ ��������� - �뢥��� ���
;        mov     cx, length_frac ; ����頥� � ���稪 ����� �஡��� ���
printmant:        
		ftst
        fstsw   ax
        sahf
        jz      endproc         ; �஡��� ���� ���������
		fimul   ten             ; 㬭���� �� 10
        fxch    st(1)           ; �����⮢�� ��� fprem - ���塞 st � st(1) ���⠬� �
        fld     st(1)           ; �����㥬 �᫮ �� ���設�
        fprem                   ; �⤥��� �஡��� ���� �� 楫��
        fsub    st(2), st       ; � ��⠢�塞 �஡���
        fxch    st(2)
        fistp   temp            ; ��⠫������ ����祭��� �᫮ �� �⥪� � temp
        mov     ax, temp        ; �� �஡��� ��� ���� ᫥��, ����� �᫮ �뢮��� �ࠧ�, ��� �।���⥫쭮�� ��࠭���� � �⥪
        mov 	ah, 02h
		add     al, 30h         ; ��ॢ�� � ascii
        int     21h             ; �� �࠭
        fxch    st(1)           ; �����⮢�� �⥪ � ᫥���饬� 蠣� 横�� (����祭��� ��⭮� �� ���設�, � st(1) - 1)
        ftst
        fstsw   ax
        sahf                    ; �஢�ਬ �� 0 ���⮪ �஡��� ���
   ;     loopne  printmant
		jmp printmant
endproc:        		
		fstp                    ; ��⮢�. ���⨬ �⥪ ᮯ�����
        fstp    st  
		popa              
        ret
exdoubletostr endp 
				

nextline PROC ; ���� �� ������ � �� ����� ����� ���室���!
	push dx
	push ax

	mov ah, 2 ; �㤥� �뢮���� ᨬ����
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
	or cr, 0000000011000000b   ; ��⠭�������� �筮��� �� 80 ���� (���७��� �������)
	push 5 ; ��ப� ⠪��� ࠧ��� ����� ����� (�. �� ࠧ��� ����)
		push offset string ; ��।�� ���� ��砫� ����
		call readstring;
		mov si, offset string
		call strtoexdouble
		call newline
		call exdoubletostr
		

	xor al, al
	mov ax, 4c00h
	int 21h

	ret

CODE ENDS
END start
