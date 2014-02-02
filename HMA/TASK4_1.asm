.386
TERMINALSYMBOL  EQU '$'                 ;#define TERMINALSYMBOL '$'
DATA SEGMENT USE16 PUBLIC 'data'
string WORD 5 dup(0)
emptyString byte 0Dh,0Ah,TERMINALSYMBOL
inputm     DB 'Enter please M: $'
inputn     DB 'Enter please N: $'
allocres    DB 'Allocated mem: $'
noposstr DB 'No positive string$'
m       WORD 0 ; ���-�� ��ப
n       WORD 0 ; ���-�� �⮫�殢
mptr WORD 0
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
		
allocmem PROC STDCALL
		LOCAL tmp:WORD
		push dx
		push bx
		
		mov dx, offset inputm ; �뢮��� �ਢ���⢥���� ᮮ�饭�� �� ���� m
		mov ah, 09h ; ���뢠��� �뢮��
		int 21h
		push 5 ; ���ᨬ� ����� 5
		push offset string 
		call readstring ; ���뢠�� ��ப�
		push offset string
		call atoi ; �ਢ���� � ���
		mov ds:m, ax ; ��࠭塞 � ᥣ����� ॣ���� � m
		cmp ax, 0h
		jz _exit
		
		call newline
		
		mov dx, offset inputn ; �뢮��� �ਢ���⢨� �� ���� n
		mov ah, 09h ; ���뢠��� �뢮��
		int 21h
		push 5
		push offset string
		call readstring ; ���뢠�� ��ப�
		push offset string
		call atoi ; �ਢ���� � ���
		mov ds:n, ax ; ��࠭塞 � ᥣ����� ॣ���� � n
		cmp ax, 0h
		jz _exit
				
		call newline
		
		mul ds:m ; � ax ᨤ�� n, 㬭����� �� m
		shl ax, 1 ; �⮫쪮 ���� �㦭�, �.�. � ����� word-�
		shr ax, 4 ; �⮫쪮 16-���⮢�� ������ �㦭�                        
		inc ax ; ⠪ ��� shr ��୥� 楫�� ���� �� �������, 㢥��稢��� �� �������
		mov bx, ax ; �� ������⢮ 16-���⮢ ������, ���஥ �㦭� �뤥����
		mov ah, 48h ; ���뢠��� �뤥����� �����
		int 21h
		jc _exit
		
		mov tmp, ax
		mov dx, offset allocres ; �뢮��� ��砫쭮� ᮮ�饭�� �뤥����� ����ᮢ
		mov ah, 09h
		int 21h
		push bx
		call itoa ; �뢮��� ������⢮ �뤥������ �����
		
		call newline
		mov ax, tmp

		pop bx
		pop dx
		ret

_exit:
			xor al,al
			mov ax,4c00h
			int 21h

			ret	
allocmem ENDP

readmatr PROC STDCALL
		LOCAL strind, colind:WORD, isNeg:BYTE
		push dx
		push bx
		mov strind, 0h ; ������ ��ப�
		mov colind, 0h ; ������ �⮫��
		readrow:
			push ax ; ��࠭塞 ax
			mov ax, strind ; � ax ����ᨬ ������ ��ப�
			cmp ax, ds:m ; �஢��� �� ��諨 �� �� ��ப�
			jz endread
			readnumber:
						push ax ; ���������� a�
						mov ax, colind ; ����ᨬ � �� ������ �⮫��
						cmp ax, ds:n ; �ࠢ������ �� ����� ��ப�
						jz endstring
						pop ax ; ����⠭�������� �
						mov isNeg, 0 ; ����砥�, �� � ��� ������⥫쭮� �᫮
						xor ax, ax ; ����� ��
						push ax ; ����������
				readdigits:     
						mov ah, 01h ; ���뢠��� �⥭�� ᨬ����
						int 21h
						cmp al, '-' ; � ���� �� �����
						jz numIsNeg
				readdigcont:
						cmp al, '0' ; �஢���, �� ��� ᨬ���� ����� ���� ���
						jl rnumcomp
						cmp al, '9' ; �஢���, �� ��� ᨬ���� ����� ���� ����⪨
						jg rnumcomp
						cmp al, ' ' ; �஢���, �� �� �� �஡��
						jz rnumcomp
						sub al, '0' ; �ਢ���� ��� � ᮮ⢥��� ���
						movzx bx, al ; ����� ��楤�� ���������� �᫠
						pop ax
						mov dl, 10
						mul dl
						add ax, bx
						push ax
						jmp readdigits
					rnumcomp:
						pop ax
						cmp isNeg, 0
						jz saveAsPos ; �᫨ �� ��⠫� ������⥫쭮� �᫮
						jnz saveAsNeg ; �᫨ �� ��⠫� ����⥫쭮� �᫮
					rnumj:
						inc colind ; 㢥��稢��� ������ �������
						jmp readnumber
			endstring: ; ����� ��ப�
				pop ax
				mov colind, 0h ; ��⠭�������� ������ ������� � ����
				inc strind ; 㢥��稢��� ������ ��ப�
				call newline
				jmp readrow; ��⪠ �⥭�� ��ப�
		endread:                                      
			pop ax
			pop bx
			pop dx
			ret
		numIsNeg:
				mov isNeg, 1 ; ����ਬ, �� ��᫥���� �᫮ ����⥫쭮�
				jmp readdigits
		saveAsNeg:
				push ax ; � �� ᨤ�� �᫮ ��࠭�� ���
				mov ax, strind
				mul ds:n ; 㬭����� ������ ��ப� �� ���-�� �⮫�殢
				add ax, colind ; ᤢ����� ��ࠢ� �� ������ �⮡��
				mov bx, ax ; �����뢠�� ⥪�騩 ������ � bx
				pop ax ; ����砥� �᫮ ���⭮
				neg ax ; ������ ��� ����⥫�� = xor ax  + 1
				shl bx, 1
				mov word ptr fs:[bx], ax ; ��࠭塞 � ���ᨢ
				jmp rnumj
		saveAsPos:
				push ax
				mov ax, strind
				mul ds:n
				add ax, colind
				mov bx, ax
				shl bx, 1
				pop ax ; � �� ����⢨� �� ��� ���栭��
				mov word ptr fs:[bx], ax
				jmp rnumj
readmatr ENDP
; ����� ������
printmatr PROC STDCALL
		LOCAL strind:WORD, colind:WORD
		mov strind, 0h ; ���樠�����㥬 ������� � �㫨
		mov colind, 0h
		push dx
		push bx
		printstring:
			push ax
			mov ax, strind
			cmp ax, ds:m ; �� �뢥��
			jz endread 
			printnumber:
				mov ax, colind
				cmp ax, ds:n ; �뢥�� �� �����
				jz endstring
				mov ax, strind ; ����砥� ⥪�騩 ������ ����� ������
				mul ds:n
				add ax, colind
				mov bx, ax
				shl bx, 1 ; ⠪ ��� ������ ���塠�⮢�
				mov ax, word ptr fs:[bx] ; ���뢠�� ⥪�騩 ����� ������
				cmp ax, 0 ; �᫨ ��� ����⥫쭮� - �ਭ����� ����
				jl numberIsNeg
				printing:                               
					push ax ; ������ �� �� �⥪
					call itoa ; ��ॢ���� � �᫮ � �뢮���
					mov ah, 02h ; ���뢠��� ���� ᨬ����
					mov dl, ' ' ; �뢮���� �㤥� �஡��
					int 21h
					inc colind ; 㢥��稢��� ������ �⮡��
					jmp printnumber                         
			endstring:
				pop ax
				int 3h
				mov word ptr colind, 0h ; ����塞 ������ �⮫��
				inc strind ; 㢥��稢��� ������ ��ப�
				call newline ; ���室�� �� ����� �����
				jmp printstring
		endread:
			pop ax
			pop bx
			pop dx
			ret
		numberIsNeg:
			push ax ; ��࠭塞 ��
			mov ah, 02h ; ��⠭�������� ���뢠��� ���� ᨬ����
			mov dl, '-' ; � ���⥬ �����
			int 21h
			pop ax ; ����⠭�������� ��
			neg ax ; � ��६ ��� ���栭��
			jmp printing
printmatr ENDP

processing_matrix PROC STDCALL
		LOCAL strind:WORD, colind:WORD, tmp:WORD
		push bx
		mov ax, ds:m
		mov strind, ax ; �㤥� �᪠�� � ����
		mov colind, 0h
		dec strind ; �㬥��� � ���
		proc_matr:
				mov ax, strind
				cmp ax, 0
				jz no_pos_str
				check_str:
					mov ax, colind
					cmp ax, ds:n
					jz str_is_pos
					mov ax, strind ; ����砥� ⥪�騩 ������ ����� ������
					mul ds:n
					add ax, colind
					mov bx, ax
					shl bx, 1 ; ⠪ ��� ������ ���塠�⮢�
					mov ax, word ptr fs:[bx] ; ���뢠�� ⥪�騩 ����� ������
					cmp ax, 0
					jl str_not_pos
					inc colind
					jmp check_str
		no_pos_str:
				mov dx, offset noposstr ; �뢮��� �ਢ���⢨� �� ���� n
				mov ah, 09h ; ���뢠��� �뢮��
				int 21h
				call newline
				jmp end_swap
		str_not_pos:
			dec strind
			mov colind, 0h
			jmp proc_matr
		str_is_pos:
			mov colind, 0h
			swap_str:
				mov ax, colind
				cmp ax, ds:n
				jz end_swap
				mov ax, strind ; ����砥� ⥪�騩 ������ ����� ������
				mul ds:n
				add ax, colind
				mov bx, ax
				shl bx, 1 ; ⠪ ��� ������ ���塠�⮢�
				mov ax, word ptr fs:[bx] ; ���뢠�� ����� ��᫥���� ����⨢��� ��ப�                
				push bx
				push ax
				mov bx, colind
				shl bx, 1
				mov ax, word ptr fs:[bx]
				mov tmp, ax ; ���뢠�� ����� ��ࢮ� ��ப�
				pop ax
				mov word ptr fs:[bx], ax ; �����뢠�� � ����� ��ப� ����� ��᫥���� ����⨢���
				pop bx ; ����⠭�������� 㪠��⥫� �� ��᫥���� ����⨢���
				mov ax, tmp
				mov word ptr fs:[bx], ax ; �����뢠�� � ��᫥���� ����⨢��� ����� ��ࢮ�
				inc colind ; ᤢ������� �� ᫥���騩 �����
				jmp swap_str
		end_swap:
			pop bx                          
			ret                                
processing_matrix ENDP

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

start:
		push dx
	mov ax, data    ; initialising segments
	mov ds, ax
	mov es, ax
	assume ds:data, es:data, cs:code, ss:stack
		  
		call allocmem
		mov fs, ax
		call readmatr
		call newline
		call processing_matrix
		call printmatr
		
		
end_program:
		pop dx
	xor al,al
	mov ax,4c00h
	int 21h

	ret


CODE ENDS
END start
