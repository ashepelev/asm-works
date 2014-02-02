.386
TERMINALSYMBOL  EQU '$'                 ;#define TERMINALSYMBOL '$'
DATA SEGMENT USE16 PUBLIC 'data'
string WORD 5 dup(0)
emptyString byte 0Dh,0Ah,TERMINALSYMBOL

allocres    DB 'Allocated mem: $'
ems_driver DB 'EMMXXXX0'
noems DB 'There is no EMS$'
emsver DB 'EMS version is: $'
listlength DB 'Enter list length: $'
difference DB 'Difference = $'

isnull DB 'NULL$'
emsid DW ?	; ����� �㤥� ��࠭����� �����䨪��� �뤥������ �����
len WORD 0 ; ����� ᯨ᪠

mptr WORD 0

; ������� ᯨ᮪
list STRUCT ; 4 ����
	head dw ? ; 2 ����
	tail dw ? ; 2 ����
list ENDS
; ������� 㧥�
node STRUCT ; 6 ����
	prev dw ? ; 2 ����
	value word 0 ; 2 ����
	next dw ? ; 2 ����
node ENDS

mylist list <0,0> ; ���᮪, ����� �࠭���� � �᭮���� �����

DATA ENDS

STACK SEGMENT STACK 'stack'
 db  1024 dup (0)                       ;db - data byte; const
STACK ENDS


CODE SEGMENT USE16 PUBLIC 'code'

strlen PROC PASCAL, strBuffer:ptr byte
	push di
	mov di, strBuffer       ; address
	mov al, TERMINALSYMBOL  ; symbol to find
	mov cx, 0ffffh          ; max string lenght
	cld                     ; ��⠭����� �ࠢ��쭮� ���ࠢ����� : DF = 0
	repnz scasb             ; REPeat string operation. Scan string
	; scasb - element from es:edi/di - eax/ax/al and setting flag
	;         then
	;         (edi/di)++ if DF = 0
	;         (edi/di)-- if DF = 1
	; repnz - Analizing cx and zf
	;         cx <> 0 or zf = 0 then scasb and cx=cx-1
	;         cx = 0  or zf <> 0 exit from cycle
	jnz   bad_string        ; if not find goto bad_string
	neg cx                  ; inverse sign
	sub cx,2                ;
bad_string:
	mov ax, cx
	pop si
	ret
strlen ENDP

atoi PROC STDCALL,  strbuff:PTR BYTE
		LOCAL tmp:WORD, strsize:WORD
		push si         ; saving registrs
		push cx
		push bx

		push strbuff    ; push for function call
		call strlen
		mov strsize, ax ; strsize = strlen(strbuff)
		mov si, strbuff ; si = strbuff
		mov tmp, 10     ; tmp = 10
		xor bx,bx       ; bx = 0
		xor ax,ax       ; ax = 0

atoi_nextstep:
		cmp bx, strsize
		jge atoi_endloop                ; if (bx >= strsize) goto atoi_endloop
		movzx cx, byte ptr [si+bx]      ; cx = si[bx]
		cmp cl, 1Bh
		je end_input
		cmp cl , '0'
		jl atoi_endloop ; if (cl < '0') goto atoi_endloop
		cmp cl, '9'
		jg atoi_endloop ; if (cl > '9') goto atoi_endloop
		sub cl, '0'     ; cl -= '0'
		mul tmp         ; ax *= tmp
		add ax, cx      ; ax += cx
		inc bx          ; bx++
		jmp atoi_nextstep ;goto atoi_nextstep

end_input:
		mov ax, 1Bh
atoi_endloop:                   ; restoring registrs
		pop bx
		pop cx
		pop si
	ret
atoi ENDP
	
; �뤥����� �����	
; ��� ࠡ��� �㦭� �ᯮ�짮���� �ࠢ���� ��ࠧ
; � ����祭��� EMS
; ��᫥ ��⨢�樨 - �⫠��� ����������
allocmem PROC STDCALL
		LOCAL tmp:WORD
		push dx
		push bx
		
		mov ah, 41h ; ����稬 ᥣ����� ���� ����
		int 67h
		mov fs, bx  ; ���������� ��� � ᥣ���� fs

		mov ah, 43h ; �뤥��� ���� 16-�������⭮� ��࠭���
		mov bx, 0001h	
		int 67h
		
		; �⮡ࠦ���� �����
		mov ah, 44h
		mov al, 00h ; ����� ��ࢮ� 16�� ��࠭��� � 64�� ����
		mov bx, 00h ; ����� 16�� ��࠭��� � EMS-�����
		; � DX 㦥 ᨤ�� �����䨪��� �뤥������ ����� � �। �맮��
		int 67h

		mov ax, dx ; ��࠭�� �����䨪��� ��襩 ����� � �����⨬ ���

		pop bx
		pop dx
		ret
allocmem ENDP

; �㭪�� ���뢠�� ᯨ᮪
; �� �室 �������� 㪠��⥫� �� ᯨ᮪
readlist PROC STDCALL, rlist: ptr list
	LOCAL ind:WORD	; ������ � ᯨ᪥
	LOCAL isNeg:WORD	; ���� ����⥫쭮�� �᫠
	LOCAL elem:WORD		; �६����� ��६�����
	push dx			; ���࠭塞 ॣ�����
	push bx
	push cx
	mov dx, offset listlength ; �뢮��� �ਣ��襭�� � ����� ����� ᯨ᪠
	mov ah, 09h
	mov cx, 6h				; �����⥫�, ࠢ�� ����� ��������
	int 21h 
	push 5
	push offset string
	call readstring			; ���뢠��
	call newline
	push offset string
	call atoi			; ���ᨬ
	mov ds:len, ax		; ���࠭塞
	mov ind, 0h			; ���樠�����㥬
	readstart:
		mov isNeg, 0h
		mov ax, ind
		cmp ax, ds:len	; ����� ᯨ᪠?
		jz end_list
		xor ax, ax		; � �� �㤥� ������������� �᫮
		push ax
		mov ah, 01h		; ���뢠�� ᨬ��
		int 21h
		cmp al, '-'
		jz numIsNeg		; �᫨ �� ����⥫쭮�, ����砥� � �� readdigits
		jnz readdigcont	; �᫨ ������⥫쭮�, � �� readdigcont, ᨬ��� 㦥 ��⠭
		readdigits:
			mov ah, 01h
			int 21h
		readdigcont:	; �⠭����� ������ ���������� �᫠
			cmp al, '0' ; �஢���, �� ��� ᨬ���� ����� ���� ���
			jl save
			cmp al, '9' ; �஢���, �� ��� ᨬ���� ����� ���� ����⪨
			jg save
			cmp al, ' ' ; �஢���, �� �� �� �஡��
			jz save
			sub al, '0' ; �ਢ���� ��� � ᮮ⢥��� ���
			movzx bx, al ; ����� ��楤�� ���������� �᫠
			pop ax
			mov dl, 10
			mul dl
			add ax, bx
			push ax
			jmp readdigits       
		save:			; ���࠭塞 ����� � ᯨ᮪
			mov ax, ind	; ��᫮ � ax ����� �� �⥪�. ����頥� � ax ������ ����� � ᯨ᪥
			mul cx		; �������� �� ����� ��������
			mov bx, ax	; � bx ����頥� 㪠��⥫� �� ��������
			pop ax		; ������� � �⥪� ��, � ��� ����������� �᫮
			cmp isNeg, 0	; �஢��� �� ����⥫�� ��
			jnz makeNeg
		saveFromNeg:
			mov word ptr fs:[bx+2], ax		; ���࠭塞 � �������� �� ������ +2 ����
			cmp ind, 0h ; �᫨ �� �� ���� 㧥� ᯨ᪠
			jnz linkleft ; �� ��뢠�� � �।��騬
		save1:
			mov ax, ind
			cmp ax, ds:len-1 ; �᫨ �� �� ��᫥���� 㧥� ᯨ᪠
			jnz linkright	; �� ��뢠�� � ᫥���騬
		save2:                  
			inc ind			; �����稢��� ������ ᯨ᪠
			jmp readstart	; ���室�� � ���뢠��� ᫥���饣� �᫠
		linkleft:
			mov ax, ind ;����頥� � ax ������ ����� � ᯨ᪥
			mul cx ; �������� �� ����� ��������
			mov bx, ax  ; �����뢠�� ������ ⥪�饩 ��������   
			sub ax, 6   ; �����⥫� �� �।��騩 �����           
			mov word ptr fs:[bx], ax ; � ⥪�騩 ���� ��뫪� �� �।��騩
			mov word ptr fs:[bx-2], bx ; � �।��饬 ���� ��뫪� �� ᫥���騩
			jmp save1
		linkright:    
			mov ax, ind
			mul cx
			mov bx, ax   
			add ax, 6 ; 㪠��⥫� �� ᫥���騩 �����                                   
			mov word ptr fs:[bx+4], ax ; � ⥪�饬 ���� ��뫪� �� ᫥���騩
			mov word ptr fs:[bx+6], bx ; � ᫥���饬 ���� ��뫪� �� �।��騩
			jmp save2               
	numIsNeg:
		mov isNeg, 1 ; ����ਬ, �� ��᫥���� �᫮ ����⥫쭮�
		jmp readdigits
	makeNeg:
		neg ax		; ������ ����⥫��
		jmp saveFromNeg
	end_list:
		mov bx, 0h	; ��� ��ࢮ�� ����� � ����⢥ �।��饣� 㪠�뢠�� 8000h, �� ��⠥��� NULL
		mov word ptr fs:[bx], 8000h
		; ����砥� 㪠��⥫� �� ��᫥���� ����� ᯨ᪠
		mov ax, ds:len
		dec ax
		mul cx
		mov bx, ax
		mov word ptr fs:[bx+4], 8000h  ; ��� ��᫥����� �����, ᫥���騩 NULL
		assume bx: ptr list
		mov bx, rlist ; ������塞 ���� ᯨ᪠
		mov [bx].head, 0h ; ������ 㪠�뢠�� �� ����
		mov ax, ds:len
		dec ax
		mul cx
		mov [bx].tail, ax ; ����� �� ��᫥���� �����
		pop cx
		pop bx
		pop dx
		ret
readlist ENDP

; ����� ������
; �� �室� 㪠��⥫� �� ᯨ᮪
printlist proc STDCALL, plist:PTR list
	LOCAL ind:WORD
	push bx
	push dx
	assume bx:ptr list
	mov bx, plist
	mov ax, [bx].head ; ����砥� ������
	mov bx, ax
	printstart:
		printleftnull:
			mov dx, offset isnull	; � ��砫� �뢮��� NULL
			mov ah, 09h
			int 21h 
			mov ah, 02h
			mov dl, '='			; � ���� "=" ��� ࠧ����⥫�
			int 21h
		printval:
			mov ax, word ptr fs:[bx+2] ; �뢮��� ᠬ� ���祭��
			cmp ax, 0					; �஢��塞 �� ����⥫쭮���
			jl isNeg
		printval1:                      
			push ax						; ��ॢ���� �᫮ � ��ப�
			call itoa					; � ���⠥�
			mov ah, 02h 
			mov dl, '='					; �뢮��� "="
			int 21h
		cmp word ptr fs:[bx+4], 8000h	; �஢���, �� � ��� �� ��᫥���� �����
		jz lastelem
		mov ax, word ptr fs:[bx+4]		; � �� ����頥� 㪠��⥫� �� ᫥���騩 �����
		mov bx, ax						; � ��ॢ���� ��� � bx
		jmp printval					; ��ࠡ��뢠�� ᫥���騩 �����
		isNeg:
			push ax						; ���������� ��
			mov ah, 02h
			mov dl, '-'					; �뢮��� �����
			int 21h
			pop ax						; ����⠭�������� ��
			neg ax						; ��६ ��� ����⥫쭮�
			jmp printval1
		lastelem:
			mov dx, offset isnull		; �᫨ �� ��᫥����, � �뢮��� "NULL"
			mov ah, 09h
			int 21h
			jmp endprint
		endprint:
			pop dx
			pop bx
			ret
printlist ENDP

; ��� �㭪�� ����� ��室�� ࠧ����� ����⮢ ᯨ᪠
; ��⠥� ��� a[0] - sum[i:1-n](a[i])
findDifference PROC STDCALL, ilist: ptr list
	push bx
	push dx
	mov dx, offset difference
	mov ah, 09h
	int 21h
	assume bx:ptr list
	mov bx, ilist
	mov ax, [bx].head
	mov bx, ax      
	
	mov ax, word ptr fs:[bx+2]
	mov dx, word ptr fs:[bx+4]
	mov bx, dx
	printstart:
		printval:
			cmp bx, 8000h
			jz endprint
			mov dx, word ptr fs:[bx+2]
			sub ax, dx
			mov dx, word ptr fs:[bx+4]
			mov bx, dx
			jmp printval
		endprint:
			cmp ax, 0
			jl isNeg
		endprint1:
			push ax
			call itoa
			pop dx
			pop bx
			ret
		isNeg:
			push ax
			mov ah, 02h
			mov dl, '-'
			int 21h
			pop ax
			neg ax
			jmp endprint1
findDifference ENDP


memcpy PROC STDCALL, dest: PTR BYTE, src: PTR BYTE, memsize:WORD
       push di         ; saving rigisters
       push si
       push cx
       mov di, dest
       mov si, src
       mov cx, memsize
       rep movsb       ; move string byte cx size

       pop cx
       pop si
       pop di
       ret
memcpy ENDP

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

itoa PROC STDCALL, i:WORD
				LOCAL tmp:BYTE
				push bx                 
				push dx
				push cx
				push ax                                

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
				pop ax
				pop cx
				pop dx
				pop bx
				ret
itoa ENDP

; �㭪�� �᢮������� ������� ������
freemem PROC STDCALL
	push dx
	mov ah, 45h
	mov dx, ds:emsid
	int 67h
	pop dx
	ret
freemem ENDP

start:
		push dx
	mov ax, data    ; initialising segments
	mov ds, ax
	mov es, ax
	assume ds:data, es:data, cs:code, ss:stack
		  
	call allocmem
	mov ds:emsid, ax    ; ���࠭塞 �����䨪��� �뤥������ EMS �����
	push offset mylist
	call readlist
	call newline
	push offset mylist
	call printlist   
	call newline
	push offset mylist
	call findDifference
     ;   call newline       
	call freemem    
		
end_program:
		pop dx
	xor al,al
	mov ax,4c00h
	int 21h

	ret


CODE ENDS
END start
