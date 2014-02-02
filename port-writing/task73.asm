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


writer proc PASCAL com:word, mbegin:dword, mend:dword ;not including end
      ;  int 3
        pushad ; ��������� ��������
        push com
        call setupPort

        push ds
        mov ecx, mend
        sub ecx, mbegin ; ����� ������������ ������ � ������
        mov ebx, mbegin	; � bx - ������ ���������
        mov esi, 0fh 
        and esi, ebx ; � esi - offset
        shr ebx, 4 ; � ebx - segment
        mov ds, bx ; ����� �������� - 16 ��� ����� ��� ������� ������
        mov dx, com  ; � dx - ���� COM
        cld
send_byte:           ;send bytes untill ecx==0
        mov al, byte ptr[si] ; ��-��������� segment - � ds
        out dx, al	; ����� ���� �� offset � ������ � COM ����
        inc si		; ���������� �� ���������
        test si, 10h	; ����� offset = 16
        jz pass_inc_ds	; ���� �� ����� ��
        mov ax, ds	; ����������
        inc ax		; �� ���������
        mov ds, ax	; ���������� �����
        mov si, 0h	;
pass_inc_ds:
        dec ecx		; ��������� ���-�� ���������� ����
        test ecx, ecx	; �� ����� �� �� �����
        jnz send_byte
        pop ds
        popad
        ret

writer endp

setupPort proc near PASCAL com:word ;some hardly understandable magic
        push es
        mov ax, com
        mov cs:cur_com, ax ; ��������� ������� ���-����

        mov dx, com ; ���-����� 03F8h-03FFh
        ;configure frequency of com-port
        add dx, 3; com+3; LCR
        mov al, 10000011b ; 03F8h � 03F9h - �������� ������� �����
						  ; ����� ����� - 8 ���
        out dx, al
        mov dx, com ;DLL
        mov al, 2 ; ������������� ������� �������� ������� �����
        out dx, al	; ����� �������� � 2 ���� ��������� ���� ��������
        inc dx ; 03F9h
        mov al, 0 ; ������������� ������� �������� ������� �����
        out dx, al ; � ����� - �������� � 2 ���� ���������
        ;
        add dx, 2 ; com+3; 
        mov al, 0011b ; ���������� 03F8 � 03F9 ������ ��������
						; �.�. 03F8 - ������� �������� ������
						; 03F9 - IER
        out dx, al
		
        sub dx, 2
        mov al, 0	; ��������� ���������� - ���� ������ ����� ������
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
	
        push ax ; ������ �������� (���� ���1)

        push 11
        push offset membuf
        call readstring		; ��������� ������ ���������

        push offset membuf
        call atoi			; ������
		
		call nextline

        push eax ; ������ ��������

        push 11
        push offset membuf
        call readstring		; ��������� ����� ���������
		
		
        push offset membuf
        call atoi	; ������
		
		call nextline
		
        push eax ; ������ ��������

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