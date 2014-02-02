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

atoi PROC STDCALL,  strbuf:PTR BYTE ; ��ॢ�� ��ப� � 楫�� �᫮
		LOCAL tmp:WORD, strsize:WORD
		push si
		push cx
		push bx

		push strbuf ; ������ 㪠��⥫� �� ��砫� ����-��ப� �� �⥪
		call strlen ; 㧭��� �� �����
		mov strsize, ax ; ���������� � �����쭮� ��६�����
		mov si, strbuf ; ��⮢���� 室��� �� ��ப�
		mov tmp, 10
		xor bx,bx ; ����塞 ॣ�����
		xor ax,ax

atoi_nextstep:
		cmp bx, strsize ; �� ��諨 �� �� ���� ��ப�
		jge atoi_endloop ; �᫨ ��諨 - ��室��
		movzx cx, byte ptr [si+bx] ; ���� ���������� � ॣ���� cx ⥪�騩 ᨬ��� (�������� 16-���� �����)
		cmp cl , '0'
		jl atoi_endloop
		cmp cl, '9'
		jg atoi_endloop ; �᫨ ࠧ����� ����� ᨬ����� ����� ��� ��� ����� 9-� - ��ࢠ���� �� ����� ��ப�, ᠬ� �᫮ 㦥 ��ࠡ�⠭�
		sub cl, '0' ; ���� ⥯��� � ॣ���� ���
		mul tmp ; 㬭����� �� 10 �ନ�㥬�� �᫮
		add ax, cx ; �ਡ���塞 ��।��� ���� (���� � ����訩 ࠧ��)
		inc bx ; ��⮢���� ࠡ���� � ᫥���騬 ᨬ����� ��ப�
		jmp atoi_nextstep

atoi_endloop:
		pop bx
		pop cx
		pop si
	ret
atoi ENDP

itoa PROC STDCALL, i:WORD
				LOCAL tmp:BYTE
				push bx                 
				push dx
				push cx
				
				mov bx, 10 ; �� ���� ����� ������
				mov ax, i
				xor cx, cx ; ������� ����
				stack_push:
					xor dx, dx ; �������
					div bx; dx = ax % 10, ax = ax / 10
					push dx ; ������ �� ���� ������ ������
					inc cx
					cmp ax, 0 ; ���� �������� ��� �����
					jnz stack_push
					mov ah, 02h
				stack_pop:
					pop dx ; ������� �� ����� ����� ����� ����� �������
					add dl, '0' ; �������� ��� ������� �����
					int 21h ; ������� ���
					dec cx
					jnz stack_pop
				pop cx
				pop dx
				pop bx
				ret
itoa ENDP


expr PROC STDCALL
				LOCAL A:WORD, B:WORD, CC:WORD
				
				push ax
				push dx
				push cx
				
				push 5
				push offset string
				call readstring
				push offset string
				call atoi
				mov A, ax
				call nextline
				push 5
				push offset string
				call readstring
				push offset string
				call atoi
				mov B, ax
				call nextline
				push 5
				push offset string
				call readstring
				push offset string
				call atoi
				mov CC, ax
				call nextline
				
				mov ax, B
				mov dx, 19
				mul dx
				push ax
				mov ax, A
				mov dx, CC
				mul dx
				mov cx, B
				div cx
				pop dx
				add ax, dx
				push ax
				call itoa
				
				pop cx
				pop dx
				pop ax
				ret
expr ENDP
				
					
				

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
		
			
	call expr

	xor al, al
	mov ax, 4c00h
	int 21h

	ret

CODE ENDS
END start
