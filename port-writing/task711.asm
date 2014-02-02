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

atoi PROC STDCALL,  strbuf:PTR BYTE ; ��ॢ�� ��ப� � 楫�� �᫮
		LOCAL tmp:DWORD, strsize:WORD
		push si
		push ecx
		push bx
		push edx

		push strbuf ; ������ 㪠��⥫� �� ��砫� ����-��ப� �� �⥪
		call strlen ; 㧭��� �� �����
		mov strsize, ax ; ���������� � �����쭮� ��६�����
		mov si, strbuf ; ��⮢���� 室��� �� ��ப�
		mov tmp, 10
		xor bx,bx ; ����塞 ॣ�����
		xor eax,eax

atoi_nextstep:
		cmp bx, strsize ; �� ��諨 �� �� ���� ��ப�
		jge atoi_endloop ; �᫨ ��諨 - ��室��
		movzx ecx, byte ptr [si+bx] ; ���� ���������� � ॣ���� cx ⥪�騩 ᨬ��� (�������� 16-���� �����)
		cmp cl , '0'
		jl atoi_endloop
		cmp cl, '9'
		jg atoi_endloop ; �᫨ ࠧ����� ����� ᨬ����� ����� ��� ��� ����� 9-� - ��ࢠ���� �� ����� ��ப�, ᠬ� �᫮ 㦥 ��ࠡ�⠭�
		sub cl, '0' ; ���� ⥯��� � ॣ���� ���
		mul tmp ; 㬭����� �� 10 �ନ�㥬�� �᫮
		add eax, ecx ; �ਡ���塞 ��।��� ���� (���� � ����訩 ࠧ��)
		inc bx ; ��⮢���� ࠡ���� � ᫥���騬 ᨬ����� ��ப�
		jmp atoi_nextstep

atoi_endloop:
		pop edx
		pop bx
		pop ecx
		pop si
	ret
atoi ENDP

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
		mov si, array ; ��������� �� �������� - ������
	push 100
	push di
	call readstring ; ��������� ������ � di
	mov cx, ax ; � �� - ���-�� ��������� ��������
	xor bx, bx

	readone:
		mov al, ' '
		cld
		repe scasb
		dec di ; ������ ���� ������ �������� - ���������
		inc cx

		push di
		call atoi
		mov byte ptr[si + bx], al ; ��������� � ������
		inc bx ; ������������� �� ���� ������� �������

		mov al, ' '
		cld
		repne scasb ; ���� �� ������ - ���������� (���� ��������� �������� ����� ����������)
		test cx, cx
		jnz readone
	mov ax, bx ; ���-�� ��������� �������
	call nextline
	pop es
	pop dx
	pop si
	pop di
	pop bx
	pop cx
	ret
readAndParseByte endp

; ������� ������ ������������ ����������. ���������� 18 ��� � �������
; ���������� ���������� 08h(IRQO)
myHandler proc
	push ax
	push bx;
	mov al, 00110000b   ; ������/������ ������� ��������,� ����� �������� �����
							; ���������� IRQO ��� ���������� ����
							; ������������� ����� 0
	out 43h, al             ; ����������� ������� ������� �������

	movzx ax, byte ptr cs:counter
	inc ax
	mov bl, 19
	div bl                  ; ����� - ������ ���
	mov byte ptr cs:counter, ah ; � ah - ������� (������� ��� (0-18) �������� ������)
	add byte ptr cs:index, al ; � al - ��������� ������� (����� �������� ���������� ��� ���������)
	mov al, 0ffh ; ������������� ����������� �������� ������� 0FFFFh
	out 40h, al
	out 40h, al
	pop bx
	pop ax
	db 0eah ; far jmp
	defaultHandler dd 0 ;..here
myHandler endp

; ������������� ����������
; handler_seg - ������� ������� �����������
; handler_offs 
; ��������� ������� �����������
setHandler proc PASCAL interrupt:word, handler_seg:word , handler_offs:word
	push ds
	push dx
	cli ; ��������� ����������
		; � �������� 0000 - ������ �� ����������� ����������
	xor ax, ax 
	mov ds, ax
	movzx eax, interrupt ; eax = 0000:interrupt
	mov dx, handler_seg     ; � dx - �������
	mov word ptr ds:[eax*4+2], dx   ; � 0000:interrupt*4 + 2 ������� ������ �����������
	mov dx, handler_offs
	mov word ptr ds:[eax*4], dx ;  � 0000:interrupt*4 ������ ������ �����������
	sti ;�������� ����������
	pop dx
	pop ds
	ret
setHandler endp

playIndicator proc
	push es
	push ds
	push bx
	push dx
	mov ax, 3508h   ; �������� ����������
	int 21h ; ��������� ����� ����������� ���������� - ���������� � es:bx
		; ��������� ����������� ����������
	mov word ptr defaultHandler, bx
	mov word ptr defaultHandler+2, es

	mov ax, offset indseq
	push ax
	call readAndParseByte   ; ������ �������� � indseq
	mov cs:seq_len, al ; ; ����� �������
	push 08h
	push cs
	push offset myHandler
	call setHandler


chgIndicator: ; ���� ������� � ����������
	in al, 64h
	cmp al, 0001b
	jz keyInBuffer
	in al, 64h ; ������ ��������� ����������
	test al, 0010b ; ���������, ���� �� � ������ ����� ������
	jnz chgIndicator ; ���� ����� �� �������� - ����
	mov al, 0edh ; �������� ��������� ���������
	out 60h, al ; ���������� � ���� ������ ����������
	movzx eax, byte ptr cs:index    ; � index - ����� �������� ���������� (� ������������������)
	add ax, offset indseq     ; ��������� ������ �������   
	mov al, [eax]	 ; ���������� ���� � al - ������� ��� �����������
		cmp al, 0h
		jz isZeroN
		cmp al, 2h
		jg err_
		shl al, 1
	cont_:
     ;   add ax, offset indseq   
     ;   mov al, [eax]  ; ���������� ���� � al - ������� ��� �����������
	out 60h, al     ; ���������� ���� � al - ������� ��� �����������
	movzx ax, byte ptr cs:index     ; �������� � �l ������� ����� 
	div byte ptr cs:seq_len ; ����� �� ����� ������������������
	mov byte ptr cs:index, ah       ; ���������� � index �������, ����� �� ����� �� �������
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
