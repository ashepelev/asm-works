; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
;    This example does not use the common control library as it only uses the standard controls
; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
	

    include \masm32\include\masm32rt.inc
    include \masm32\include\dialogs.inc

	
    authproc PROTO :DWORD,:DWORD,:DWORD,:DWORD
    authdialog PROTO :DWORD
	calcdialog PROTO :DWORD, :DWORD
    calcproc PROTO :DWORD,:DWORD,:DWORD,:DWORD

	
    .data
      hAuthInstance dd ?
		hCalcInstance dd ?
	  log db 'admin'
	  pas db 'password'
	loghash DWORD 3166067245
	inpass DWORD ?
	inpasslen DWORD ?
	wp db 'Wrong password'
	hRadio1   dd ?
    hRadio2   dd ?
    hRadio3   dd ?
    hRadio4   dd ?
	
	

	TERMINALSYMBOL EQU '$'
    .code

; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

start:
    
    call mainauth
    invoke ExitProcess,eax

; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

mainauth proc

    LOCAL ptxt  :DWORD
    LOCAL hIcon :DWORD
	LOCAL a
	mov hAuthInstance, rv(GetModuleHandle,NULL)

    invoke InitCommonControls

    mov ptxt, rv(authdialog,"Authentication")

    invoke GlobalFree,ptxt

    ret

mainauth endp

; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤


maincalc proc 
  	LOCAL rvl   :DWORD
    LOCAL arr[4]:DWORD          ; DWORD array
    LOCAL parr  :DWORD          ; array pointer
	LOCAL ptxt  :DWORD
    LOCAL icce:INITCOMMONCONTROLSEX

    mov hCalcInstance, rv(GetModuleHandle,NULL)

	invoke InitCommonControls
	
	mov ptxt, rv(calcdialog,hCalcInstance,"Super Asm Calculator")

	invoke GlobalFree,ptxt
     ret
maincalc endp

authdialog proc dgltxt:DWORD

    LOCAL arg1[4]:DWORD
    LOCAL parg  :DWORD

    lea eax, arg1
    mov parg, eax

  ; ---------------------------------------
  ; load the array with the stack arguments
  ; ---------------------------------------
    mov ecx, dgltxt
    mov [eax], ecx

    Dialog "Get User Text", \               ; caption
           "Arial",8, \                     ; font,pointsize
            WS_OVERLAPPED or \              ; styles for
            WS_SYSMENU or DS_CENTER, \      ; dialog window
            7, \                            ; number of controls
            50,50,292,120, \                 ; x y co-ordinates
            4096                            ; memory buffer size

    DlgIcon   0,250,12,299
    DlgGroup  "Login",8,4,231,31,300
    DlgEdit   ES_LEFT or WS_BORDER or WS_TABSTOP,17,16,212,11,301
	DlgGroup  "Password",8,40,231,31,302
	DlgEdit   ES_LEFT or WS_BORDER or WS_TABSTOP,17,52,212,11,303
    DlgButton "Enter",WS_TABSTOP,172,80,50,13,IDOK
    DlgButton "Cancel",WS_TABSTOP,225,80,50,13,IDCANCEL

    CallModalDialog hAuthInstance,0,authproc,parg

    ret

authdialog endp

; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

check proc login:DWORD, llen:DWORD, pass:DWORD, plen:DWORD
	LOCAL t:DWORD
	mov t, 0h
	mov eax, offset log
	dec plen
	dec llen
	push llen
	push eax
	push login
	call mycmpstr
	add t, eax
	mov eax, offset pas
	push plen
	push eax
	push pass
	call mycmpstr
	add t, eax
	mov eax, t
	cmp eax, 02h
	jz istrue
	jnz isbad
	istrue:
		mov eax, 1h
		ret
	isbad:
		mov eax, 0h
		ret	
check endp

hash proc login:DWORD, leng:DWORD
	LOCAL ind:DWORD
	LOCAL m:BYTE
	LOCAL res:DWORD
	LOCAL p:DWORD
	push ebx
	mov p, 31
	mov ind, 0h
	mov ebx, login
	add ebx, 5 
	mov al, byte ptr [ebx]
	mov m, al
	mov res, 0h
	make:
		mov eax, ind
		cmp eax, leng
		jz endhash
		mov ebx, login
		add ebx, ind
		mov al, byte ptr [ebx]
		movzx ebx, al
		movzx eax, m
		sub ebx, eax
		inc ebx
		mov eax, ebx
		mul p
		add res, eax
		mov eax, p
		mul p
		mov p, eax
		inc ind
		jmp make
	endhash:
		pop ebx

		mov eax, res
		ret
hash endp

mycmpstr proc string1:DWORD, string2:DWORD, length1:DWORD
	LOCAL ind:DWORD
	LOCAL t:BYTE
	push ebx
	mov ind, 0h
	
	mcmp:
		mov eax, ind
		cmp eax, length1
		jz end_cmp_true
		mov eax, string1
		mov ebx, ind
		add ebx, eax
		mov al, byte ptr [ebx]
		mov t, al
		mov eax, string2
		mov ebx, ind
		add ebx, eax
		mov al, byte ptr [ebx]
		cmp al, t
		jz cont_good
		jnz end_cmp_false
	cont_good:
		inc ind
		jmp mcmp
	end_cmp_true:
		pop ebx
		mov eax, 1
		ret
	end_cmp_false:
		pop ebx
		mov eax, 0
		ret		
mycmpstr endp

authproc proc hWin:DWORD,uMsg:DWORD,wParam:DWORD,lParam:DWORD

    LOCAL tlen1  :DWORD
    LOCAL hMem1  :DWORD
	LOCAL tlen2  :DWORD
    LOCAL hMem2  :DWORD
	LOCAL pbuf	:DWORD
    LOCAL hIcon :DWORD

    switch uMsg
      case WM_INITDIALOG
      ; -------------------------------------------------
      ; get the arguments from the array passed in lParam
      ; -------------------------------------------------
        push esi
        mov esi, lParam
        fn SetWindowText,hWin,[esi]                         ; title text address
       ; fn SetWindowText,rv(GetDlgItem,hWin,300),[esi+4]   
		; fn SetWindowText,rv(GetDlgItem,hWin,302),[esi+12] ; groupbox text address
        mov eax, [esi+8]                                    ; icon handle
        .if eax == 0
          mov hIcon, rv(LoadIcon,NULL,IDI_ASTERISK)         ; use default system icon
        .else
          mov hIcon, eax                                    ; load user icon
       .endif
        pop esi

     ;   fn SendMessage,hWin,WM_SETICON,1,hIcon
     ;   invoke SendMessage,rv(GetDlgItem,hWin,299),STM_SETIMAGE,IMAGE_ICON,hIcon
        xor eax, eax
        ret

      case WM_COMMAND
        switch wParam
          case IDOK
		;	invoke maincalc
		;	ret
            mov tlen1, rv(GetWindowTextLength,rv(GetDlgItem,hWin,301))
			mov tlen2, rv(GetWindowTextLength,rv(GetDlgItem,hWin,303))
            .if tlen1 == 0
              invoke SetFocus,rv(GetDlgItem,hWin,301)
              ret
            .endif
			.if tlen2 == 0
              invoke SetFocus,rv(GetDlgItem,hWin,303)
              ret
            .endif
            add tlen1, 1
			add tlen2, 1
            mov hMem1, alloc(tlen1)
			mov hMem2, alloc(tlen2)
            fn GetWindowText,rv(GetDlgItem,hWin,301),hMem1,tlen1
			fn GetWindowText,rv(GetDlgItem,hWin,303),hMem2,tlen2
			fn hash,hMem1,tlen1		
			cmp eax, loghash
			jz valid
			jnz invalid	
		;	fn check,hMem1,tlen1,hMem2,tlen2	
			valid:
				mov eax, hMem2
            	mov inpass, eax
				mov eax, tlen2
				mov inpasslen, eax
				invoke maincalc
				ret
			invalid:
				invoke MessageBox,hWin,SADD("Invalid password"), SADD("Error"),MB_OK
				jmp some
			some:
          case IDCANCEL
            invoke EndDialog,hWin,0
        endsw
      case WM_CLOSE
        invoke EndDialog,hWin,0
    endsw

    xor eax, eax
    ret

authproc endp

anf1 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+11]
	mov ebx, 04h
	div bx
	pop edx
	pop ebx
	ret
anf1 endp

anf2 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+8]
	mov ebx, 2h
	mul bx
	add ax, 16
	pop edx
	pop ebx
	ret
anf2 endp

anf3 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+12]
	mov ebx, 2h
	div bx
	pop edx
	pop ebx
	ret
anf3 endp

anf4 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+10]
	pop edx
	pop ebx
	ret
anf4 endp

anf5 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+5]
	sub ax, 2
	pop edx
	pop ebx
	ret
anf5 endp

anf6 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+7]
	add ax, 4
	pop edx
	pop ebx
	ret
anf6 endp

anf7 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+6]
	add ax, 7
	pop edx
	pop ebx
	ret
anf7 endp

anf8 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx]
	sub ax, 10
	pop edx
	pop ebx
	ret
anf8 endp

anf9 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+8]
	add ax, 1
	pop edx
	pop ebx
	ret
anf9 endp

anf10 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+9]
	add ax, 1
	pop edx
	pop ebx
	ret
anf10 endp

anf11 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+6]
	mov bx, 2
	div bx
	pop edx
	pop ebx
	ret
anf11 endp

anf12 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx]
	mov bx, 2
	div bx
	dec ax
	pop edx
	pop ebx
	ret
anf12 endp

anf13 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+13]
	sub ax, 6
	pop edx
	pop ebx
	ret
anf13 endp

anf14 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+7]
	add ax, 2
	pop edx
	pop ebx
	ret
anf14 endp

anf15 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+8]
	sub ax, 5
	pop edx
	pop ebx
	ret
anf15 endp

anf16 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+6]
	add ax, 6
	pop edx
	pop ebx
	ret
anf16 endp

anf17 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+5]
	pop edx
	pop ebx
	ret
anf17 endp

anf18 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+8]
	sub ax, 1
	pop edx
	pop ebx
	ret
anf18 endp

anf19 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+12]
	mov bx, 2
	mul bx
	pop edx
	pop ebx
	ret
anf19 endp

anf20 proc
	push edx
	push ebx
	xor eax, eax
	xor edx, edx
	mov ebx, inpass
	mov al, byte ptr [ebx+4]
	dec ax
	pop edx
	pop ebx
	ret
anf20 endp
; ============================= CALCULATOR ==============================================

calcdialog proc iinstance:DWORD,extra:DWORD

	LOCAL arg1[4]:DWORD
    LOCAL parg  :DWORD
	LOCAL t1:WORD
	LOCAL t2:DWORD
	LOCAL t3:DWORD
	LOCAL t4:DWORD
	push ecx

    lea eax, arg1
    mov parg, eax

  ; ---------------------------------------
  ; load the array with the stack arguments
  ; ---------------------------------------
    mov ecx, extra
    mov [eax], ecx	
	call anf1
	push ebx
	mov bx, ax
	;mov t1, ax
	;mov cx, t1
;	call anf1
;	mov t1, eax
  ; -----------------------------------------------------------------------
  ; scale the dialog size up or down by changing the point size of the font
  ; -----------------------------------------------------------------------
    Dialog "Super Asm Calculator", \            ; caption
           "MS Sans Serif",8, \             ; font,pointsize
            WS_OVERLAPPED or \              ; styles for
            WS_SYSMENU or DS_CENTER, \      ; dialog window
            bx, \                            ; number of controls
            50,50,300,140, \                ; x y co-ordinates
            1024                            ; memory buffer size
	pop ebx
  ; -----------------------------------------------------------------------
  ; ensure that the number of controls matches the count in the above macro
  ; -----------------------------------------------------------------------
	
;	push esi
 ;       push edi
   ;     invoke GlobalAlloc,GMEM_FIXED or GMEM_ZEROINIT,1024
  ;      mov esi, eax
 ;       mov edi, esi
 ;       mov DWORD PTR [edi+0],  DS_SETFONT or WS_OVERLAPPED or WS_SYSMENU or DS_CENTER
  ;      mov WORD  PTR [edi+8],  cx
  ;      mov WORD  PTR [edi+10], 50
 ;       mov WORD  PTR [edi+12], 50
  ;      mov WORD  PTR [edi+14], 300
 ;       mov WORD  PTR [edi+16], 140
  ;      add edi, 22
 ;       ustring " Super Asm C alculator"
  ;      mov WORD PTR [edi], 8
  ;      add edi, 2
  ;      ustring "MS Sans Serif"

	call anf2
	push ecx
	mov cx, ax	
    DlgButton "OK",WS_TABSTOP,cx,10,50,13,IDOK
	pop ecx
	call anf3
	push ecx
	mov cx, ax
    DlgButton "Cancel",WS_TABSTOP,230,cx,50,13,IDCANCEL
	pop ecx

    DlgGroup " Îïåðàöèè ",120,5,100,75,100
	
	call anf4
	push ecx
	mov cx, ax
    DlgRadio " Ñëîæèòü",BS_LEFT or WS_TABSTOP,130,15,60,12,cx
	pop ecx
	call anf5
	push ecx
	mov cx, ax
    DlgRadio " Âû÷åñòü",BS_LEFT or WS_TABSTOP,130,30,60,12,cx
	pop ecx
	call anf6
	push ecx
	mov cx, ax
    DlgRadio " Óìíîæèòü",BS_LEFT or WS_TABSTOP,130,45,60,12,cx
	pop ecx
	call anf7
	push ecx
	mov cx, ax
    DlgRadio " Ðàçäåëèòü",BS_LEFT or WS_TABSTOP,130,60,60,12,cx
	pop ecx
	DlgGroup  "Îïåðàíä 1",8,4,90,31,105
	call anf8
	push ecx
	mov cx, ax
	DlgEdit   ES_LEFT or WS_BORDER or WS_TABSTOP,17,16,70,11,cx
	pop ecx
	DlgGroup  "Îïåðàíä 2",8,40,90,31,107
	call anf9
	push ecx
	mov cx, ax
	DlgEdit   ES_LEFT or WS_BORDER or WS_TABSTOP,17,52,70,11,cx
	pop ecx
	DlgGroup  "Ðåçóëüòàò",8,76,90,31,109
	call anf10
	push ecx
	mov cx, ax
	DlgEdit   ES_LEFT or WS_BORDER or WS_TABSTOP,17,88,70,11,cx
	pop ecx

  ;  DlgIconEx 5,130,40,30,30,101

  ; ------------------------------------------------------------------------
  ; the argument "extra" is available in the DlgProc WM_INITDIALOG as lParam
  ; ------------------------------------------------------------------------
    CallModalDialog iinstance,0,calcproc,extra

  ; -------------------------------------------------------------------------
  ; the value in EAX is that set by the EndDialog() API called in the DlgProc
  ; -------------------------------------------------------------------------
    ret

calcdialog endp

strlen PROC STDCALL, strBuffer:ptr byte
		LOCAL t1:BYTE
        push edi
		push ecx
        mov edi, strBuffer
        mov al, TERMINALSYMBOL
        mov ecx, 0ffffffffh
        cld             ; óñòàíîâèòü ïðàâèëüíîå íàïðàâëåíèå
        repnz scasb
        jnz   bad_string
        neg ecx
        sub ecx,2
bad_string:
        mov eax, ecx
		pop ecx
        pop edi
        ret
strlen ENDP

atoi PROC STDCALL,  strbuff:PTR BYTE
                LOCAL tmp:DWORD, strsize:DWORD
				LOCAL t1:BYTE
				LOCAL t2:BYTE
				LOCAL t3:BYTE
                push esi
                push ecx
                push ebx
				push edx
				
				call anf11
				mov t1, al
				call anf12
				mov t2, al
				call anf13
				mov t3, al
                push strbuff
                call strlen
                mov strsize, eax
                mov esi, strbuff
                mov tmp, 10
                xor ebx,ebx
                xor eax,eax

atoi_nextstep:
                cmp ebx, strsize
                jge atoi_endloop
                movzx ecx, byte ptr [esi+ebx]			
                cmp cl , t1
                jl atoi_endloop			
                cmp cl ,t2
                jg atoi_endloop
                sub cl, t3
                mul tmp
                add eax, ecx
                inc ebx
                jmp atoi_nextstep

atoi_endloop:
				pop edx
                pop ebx
                pop ecx
                pop esi
        ret
atoi ENDP

calcproc proc hWin:DWORD,uMsg:DWORD,wParam:DWORD,lParam:DWORD 

    LOCAL flag  :DWORD
    LOCAL pbuf  :DWORD
	LOCAL tlen1  :DWORD
	LOCAL hMem1  :DWORD
	LOCAL tlen2  :DWORD
	LOCAL hMem2  :DWORD
	LOCAL res: DWORD
	LOCAL res1: DWORD
    LOCAL buffer[128]:BYTE
	

    Switch uMsg
      Case WM_INITDIALOG
      ; ----------------------------
      ; read items from passed array
      ; ----------------------------
        push esi
        mov esi, lParam
      ;  invoke SendMessage,hWin,WM_SETICON,1,[esi]
        invoke SetWindowText,hWin,[esi]
        pop esi
		call anf14
        mov hRadio1, rv(GetDlgItem,hWin,ax)
		call anf15
        mov hRadio2, rv(GetDlgItem,hWin,ax)
		call anf16
        mov hRadio3, rv(GetDlgItem,hWin,ax)
		call anf17
        mov hRadio4, rv(GetDlgItem,hWin,ax)

      ; -------------------------------------------
      ; set the selection to the first radio button
      ; -------------------------------------------
        fn SendMessage,hRadio1,BM_SETCHECK,BST_CHECKED,0

   ;     m2m hWnd, hWin
   ;     mov eax, 1
		xor eax, eax
        ret

      Case WM_COMMAND
        Switch wParam						
          Case IDOK
			call anf18
			mov tlen1, rv(GetWindowTextLength,rv(GetDlgItem,hWin,ax))
			call anf19
			mov tlen2, rv(GetWindowTextLength,rv(GetDlgItem,hWin,ax))
	        .if tlen1 == 0
	        	invoke SetFocus,rv(GetDlgItem,hWin,106)
	            ret
	        .endif
			.if tlen2 == 0
	        	invoke SetFocus,rv(GetDlgItem,hWin,108)
	            ret
	        .endif
			add tlen1, 1
			add tlen2, 1
			mov hMem1, alloc(tlen1)
			mov hMem2, alloc(tlen2)
			fn GetWindowText,rv(GetDlgItem,hWin,106),hMem1,tlen1
			fn GetWindowText,rv(GetDlgItem,hWin,108),hMem2,tlen2
			mov eax, hMem1
			mov ebx, tlen1
			dec ebx
			mov byte ptr [eax+ebx], TERMINALSYMBOL
			push eax
			call atoi
			push hMem1
			call atoi
            mov res, eax
			push hMem2
			call atoi
            mov res1, eax
            .if rv(SendMessage,hRadio1,BM_GETCHECK,0,0) == BST_CHECKED
				mov eax, res1
				add res, eax
				jmp showit
            .endif

            .if rv(SendMessage,hRadio2,BM_GETCHECK,0,0) == BST_CHECKED
                mov eax, res1
				sub res, eax
            	jmp showit
            .endif

            .if rv(SendMessage,hRadio3,BM_GETCHECK,0,0) == BST_CHECKED
            	mov eax, res1
				mul res
				mov res, eax
            	jmp showit
            .endif

            .if rv(SendMessage,hRadio4,BM_GETCHECK,0,0) == BST_CHECKED
            	mov eax, res
				div res1
				mov res, eax
            	jmp showit
            .endif

          showit:
          ; ------------------------------------------
          ; construct the string to display the choice
          ; ------------------------------------------
            mov pbuf, ptr$(buffer)
            mov pbuf, cat$(pbuf,str$(res))
	;		fn MsgBoxi,hInstance,hWnd,pbuf,"Result",MB_OK,5
			call anf20
			invoke SendMessage,FUNC(GetDlgItem,hWin,ax),WM_SETTEXT,0,pbuf
			; fn SetWindowText,
            ; fn MsgBoxi,hInstance,hWnd,pbuf,"Result",MB_OK,5
			;invoke EndDialog,hWin,hMem
                       
          Case IDCANCEL
         invoke EndDialog,hWin,1
        EndSw
      Case WM_CLOSE
        invoke EndDialog,hWin,0
    EndSw

    return 0

calcproc endp

; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤


; ===================================== ANFUSCATION =======================================


end start
