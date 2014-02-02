.386
TERMINALSYMBOL  EQU '$'
DATA SEGMENT USE16 PUBLIC 'data'

string BYTE 1000 dup(0h)
memarr BYTE 100 dup(0)
indseq BYTE 100 dup(0)

wrongNum db 'Wrong indicator number$'

DATA ENDS

STACK SEGMENT STACK 'stack'
 db  1024 dup (0)
STACK ENDS


CODE SEGMENT USE16 PUBLIC 'code'
assume ds:data, es:data, cs:code, ss:stack

atoi PROC STDCALL,  strbuf:PTR BYTE ; ������� ������ � ����� �����
		LOCAL tmp:DWORD, strsize:WORD
		push si
		push ecx
		push bx
		push edx

		push strbuf ; ������ ��������� �� ������ ������-������ �� ����
		call strlen ; ������ � �����
		mov strsize, ax ; ���������� � ��������� ����������
		mov si, strbuf ; ��������� ������ �� ������
		mov tmp, 10
		xor bx,bx ; �������� ��������
		xor eax,eax

atoi_nextstep:
		cmp bx, strsize ; �� ����� �� �� ����� ������
		jge atoi_endloop ; ���� ����� - �������
		movzx ecx, byte ptr [si+bx] ; ����� ���������� � �������� cx ������� ������ (������� 16-�������� �����)
		cmp cl , '0'
		jl atoi_endloop
		cmp cl, '9'
		jg atoi_endloop ; ���� �������� ����� �������� ������ ���� ��� ������ 9-�� - ��������� �� ����� ������, ���� ����� ��� ����������
		sub cl, '0' ; ����� ������ � �������� �����
		mul tmp ; �������� �� 10 ����������� �����
		add eax, ecx ; ���������� ��������� ����� (��� � ������� ������)
		inc bx ; ��������� �������� �� ��������� �������� ������
		jmp atoi_nextstep

atoi_endloop:
		pop edx
		pop bx
		pop ecx
		pop si
	ret
atoi ENDP

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
		call atoi ; ������������� � �����
		mov byte ptr[si + bx], al ; ��������� � ������
		inc bx ; ������������� �� ���� ������� �������

		mov al, ' ' 
		cld
		repne scasb ; ���� �� ������ - ���������� (���� ��������� �������� ����� ����������)
		test cx, cx
		jnz readone
	mov ax, bx ; ���-�� ��������� �������
	;mov bx, 1
	;xor dx, dx
	;div bx
	call nextline
	pop es
	pop dx
	pop si
	pop di
	pop bx
	pop cx
	ret
readAndParseByte endp

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

; ������� ������ ������������ ����������. ���������� 18 ��� � �������
; ���������� ���������� 08h(IRQO)
myTimerHandler proc
	push ax; because interrupt could be really unexpected
	push bx;
	mov al, 00110000b   ; ������/������ ������� ��������,� ����� �������� �����
							; ���������� IRQO ��� ���������� ����
							; ������������� ����� 0
	out 43h, al ; ����������� ������� ������� �������

	movzx ax, byte ptr cs:counter
	inc ax
	mov bl, 19
	div bl                          ; ����� - ������
	mov byte ptr cs:counter, ah ; � ah - ������� (������� ��� (0-18) �������� ������)
	add byte ptr cs:index, al ; � al - ��������� ������� (����� �������� ���������� ��� ���������)

    mov al, 0ffh ;restore timer value to default:
    out 40h, al ;setting lower byte of timer value
    out 40h, al ;setting higher byte
	pop bx
	pop ax
	db 0eah ; jmp far
	defaultHandler dd 0
myTimerHandler endp



; ������������� ����������
; handler_seg - ������� ������� �����������
; handler_offs 
; ��������� ������� �����������
setHandler proc PASCAL interrupt:word, handler_seg:word , handler_offs:word
	push ds
	push dx
	cli ; ��������� ����������
		; � �������� 0000 - ������ �� ����������� ����������
	xor ax, ax  ;handlers offsets and segments placed in 0000:interrupt*4
	mov ds, ax
	movzx eax, interrupt ; eax = 0000:interrupt
	mov dx, handler_seg ; � dx - �������
	mov word ptr ds:[eax*4+2], dx ; � 0000:interrupt*4 + 2 ������� ������ �����������
	mov dx, handler_offs
	mov word ptr ds:[eax*4], dx ; � 0000:interrupt*4 ������ ������ �����������
	sti ; �������� ����������
	pop dx
	pop ds
	ret
setHandler endp

indicatorPlay proc
	push es
    push ds
    push bx
    push dx
	mov ax, 3508h ; �������� ����������
	int 21h ; ��������� ����� ����������� ���������� - ���������� � es:bx
		; ��������� ����������� ����������
	mov word ptr defaultHandler, bx ; offset
	mov word ptr defaultHandler+2, es ; segment

	mov ax, offset indseq
	push ax 
	call readAndParseByte ; ������ �������� � seq
	mov cs:seq_len, al ; ����� �������
	push 08h
	push cs
	push offset myTimerHandler
	call setHandler
;        int 3h
chgIndicator: ; ���� ������� � ����������
	in al, 64h ; ������ ��������� ����������
	test al, 0010b ; ���������, ���� �� � ������ ����� ������
	jnz chgIndicator ; ���� ����� �� �������� - ����
	mov ah, 01h
	int 16h
	jnz keyPressed
notPressed:
	mov al, 0edh ; �������� ��������� ���������
	out 60h, al ; ���������� � ���� ������ ����������
	movzx eax, byte ptr cs:index       ; � index - ����� �������� ���������� (� ������������������)
	add ax, offset indseq   ; ��������� ������ �������
	mov al, [eax]  ; ���������� ���� � al - ������� ��� �����������
		cmp al, 0h
		jz isZeroN
		cmp al, 2h
		jg err_
		shr al, 1
	contWait:
	out 60h, al ; ���������� ���� � al - ������� ��� �����������
	movzx ax, byte ptr cs:index        ; �������� � �l ������� ����� ����������
	div byte ptr cs:seq_len ; ����� �� ����� ������������������
	mov byte ptr cs:index, ah ; ���������� � index �������, ����� �� ����� �� �������
	;;;;;;;;;;;;;
	jmp chgIndicator
	;;;;;;;;;;;
end_:		
	push 08h ; interrupt number
	push word ptr defaultHandler+2 ; segment
	push word ptr defaultHandler ; offset
	call setHandler
	
	pop dx
    pop bx
    pop ds
    pop es
	ret
	
	isZeroN:
		mov al, 1h
		jmp contWait
	err_:
		mov dx, offset wrongNum
		mov ah, 09h
		int 21h
		mov ax, 4c00h
		int 21h
	keyPressed:
		mov ah, 00h
		int 16h
		cmp ah, 02h
		jz end_
		jnz notPressed
    counter db 0
    index db 0
    seq_len db 0 
indicatorPlay endp



start:
	mov ax, data
	mov ds, ax
	mov es, ax
		
		call indicatorPlay
		mov ax, 4c00h
	int 21h

	ret
CODE ENDS
END start
