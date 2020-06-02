.286
.model small
.stack 100h

.data
    creatingItemFlag    dw 1 
    endBaseCycleFlag           dw 0
    blackSymbol         db ' ',0h 
    deleteSymbol        db ' ',00010000b      
	BorderBlock         db '#',11000111b     
	mainCharacter           db '$'
	redBlock       db 01100100b
	greenBlock     db 01100010b
	blueBlock      db 01100011b
    tempBlock         dw 0    
    rotatePos         dw 0
    itemHeight         dw 0
    itemWidth          dw 0
    itemX              dw 40
    itemY              dw 1
	startTime        dw 0
	rotateAllowedFlag        db 1
    stringScore         db "00000"
    wordScore           db "Score:"
    wordOver            db "GAME OVER"
    wordStart           db "TETRIS GAME"
    wordPowered         db "Powered by ASM"
    wordLevel           db "Level:"
    Level1              db "Chicken"
    Level2              db "Casual"   
    Level3              db "Profi" 
    Level4              db "Veteran"
    score               dw 0
    delay               dw 4
    incrementSpeedFlag      db 0
    startAttr       equ 10001111b
    powerAttr       equ 10001100b
    scoreAttr      equ 00000111b
    overAttr        equ 00000100b
    base                dw 10
    randomNum        dw 0
    currentBlockModel   dw 3
    justtwo          dw 2   
.code

StartState proc
    
    keyRight       equ 4Dh
    keyLeft        equ 4Bh
    keyUp          equ 48h
    keyDown        equ 50h
    screenWidth    equ 0A0h       ; width 160 
    screenHeight   equ 19h        ; height 25
    justvar          equ 2 
    leftBorder      equ 1
    rightBorder     equ 21
    downBorder      equ 24

    mov     ax, @data 
    mov     ds, ax

    mov     ah,00h 
	mov     al,3
	int     10h

	mov     ax,0B800h ;pointer start of videomemory
	mov     es,ax
    ret
endp

GetScreenOffset proc
    push    cx
    push    bx

    mov     cl, screenWidth             ;Screen Width = 80x2=160
    mul     cl                          ; ax = y * Screen Width
    mov     dx, ax                      ; dx = y * 80x2  

    mov     ax, bx
    mov     bx, justvar
    push    dx 
    mul     bx                          ; ax = x* 2
    pop     dx
    add     dx, ax                      ; dx = offset

    pop     bx
    pop     cx
    ret
endp

RandomNumber proc 
    pusha

    mov     ah, 00h                     ; system system time        
    int     1ah                         ; CX:DX number of clock ticks      	
    mov     ax, dx
    xor     dx, dx
    mov     cx, 3    
    div     cx                          ; dx from 0 to 2
    mov     randomNum, dx	
    popa
    
    ret
endp





; [bp + 4] = symbol
	; [bp + 6] = height
	; [bp + 8] = width
	; [bp + 10] = y
    ; [bp + 12] = x
DrawBlocks proc
    push     bp
    mov      bp, sp
    
    push     ax
	push     bx  
	push     cx
	push     dx
    push     di

    mov     ax, [bp + 10]
    mov     bx, [bp + 12]
    call    GetScreenOffset

    mov     di, dx
    mov     ax, [bp + 4] ; ax = symbol
	mov     cx, [bp + 6] ; cx = height

	DrawBlocksLoop:  
		push    cx
		mov     cx, [bp + 8] ; cx = width

		push    di
		rep     stosw
		pop     di

		add     di, screenWidth

		pop     cx
	    loop    DrawBlocksLoop

	pop     di
	pop     dx
	pop     cx
	pop     bx
	pop     ax
	pop     bp
    
    ret
endp

 
    ; [bp + 4] = height
    ; [bp + 6] = width
    ; [bp + 8] = y
    ; [bp + 10] = x
EraseBlocks proc
    push    bp
    mov     bp, sp

    push    ax
    push    bx  
    push    cx
    push    dx
    push    di

    mov     ax, [bp + 8]
    mov     bx, [bp + 10]
    call    GetScreenOffset                 ; ax = y bx = x dx = offset
    mov     di, dx

    mov     ax, word ptr deleteSymbol       ; ax = symbolattr
    mov     cx, [bp + 4]                    ; cx = height

    EraseBlocksLoop:  
        push    cx
        mov     cx, [bp + 6]                ; cx = width
    
        push    di
        rep     stosw
        pop     di

        add     di, screenWidth

        pop     cx
        loop    EraseBlocksLoop

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret
endp

CommandDrawBlocks macro x, y, width, height, symbol
    push    x                
    push    y                
    push    width             ;width of block
    push    height            ;height of block
    push    word ptr symbol   

    call    DrawBlocks
    pop     dx
    pop     dx
    pop     dx
    pop     dx
    pop     dx
endm

CommandEraseBlocks macro x, y, width, height
    push    x                
    push    y                 
    push    width             ;width of block
    push    height            ;height of block

    call    EraseBlocks
    pop     dx
    pop     dx
    pop     dx
    pop     dx
endm

InitialDraw proc

    CommandDrawBlocks    1, 0, rightBorder, 1, BorderBlock              ; top
    CommandDrawBlocks    0, 0, 1, downBorder, BorderBlock               ; left
    CommandDrawBlocks    rightBorder, 1, 1, downBorder, BorderBlock     ; right
    CommandDrawBlocks    0, downBorder, rightBorder, 1, BorderBlock     ; bottom
    CommandDrawBlocks   1,1,20,23,deleteSymbol                          ; main layout
    
    push    bx
    push    si
    push    di
    mov     di, offset wordScore    ;printing word score
	push    cx
    mov     cx, 1988
    mov     si, cx
    add     cx, 12

    RenderIteration:
		xor     bx, bx
		mov     bl, byte ptr [di]
		mov     bh, scoreAttr
		mov     word ptr es:[si], bx
		inc     di
		add     si, 2
		cmp     si,cx
		jne     RenderIteration

    mov     di, offset wordLevel    ;printing word score
	push    cx
    mov     cx, 2308
    mov     si, cx
    add     cx, 12

    RenderIteration2:
		xor     bx, bx
		mov     bl, byte ptr [di]
		mov     bh, scoreAttr
		mov     word ptr es:[si], bx
		inc     di
		add     si, 2
		cmp     si,cx
		jne     RenderIteration2
    call    printLevel
    pop     cx
    pop     di
    pop     si
    pop     bx
    
    ret
endp

printLevel proc
    pusha

    mov     cx, 2320
    mov     si, cx
    add     cx, 14

    RenderIterationLvl0:
		xor     bx, bx
		mov     bx, word ptr blackSymbol
		mov     word ptr es:[si], bx
		add     si, 2
		cmp     si,cx
		jne     RenderIterationLvl0

    cmp     delay, 4
    je      LevelPrint1
    cmp     delay, 3
    je      LevelPrint2
    cmp     delay, 2
    je      LevelPrint3
    cmp     delay, 1
    je      LevelPrint4
    LevelPrint1:
        mov     di, offset Level1    ;printing word score
	    push    cx
        mov     cx, 2320
        mov     si, cx
        add     cx, 14

    RenderIterationLvl1:
		xor     bx, bx
		mov     bl, byte ptr [di]
		mov     bh, scoreAttr
		mov     word ptr es:[si], bx
		inc     di
		add     si, 2
		cmp     si,cx
		jne     RenderIterationLvl1
        pop     cx
        popa
        ret

    LevelPrint2:
        mov     di, offset Level2    ;printing word score
	    push    cx
        mov     cx, 2320
        mov     si, cx
        add     cx, 12

    RenderIterationLvl2:
		xor     bx, bx
		mov     bl, byte ptr [di]
		mov     bh, scoreAttr
		mov     word ptr es:[si], bx
		inc     di
		add     si, 2
		cmp     si,cx
		jne     RenderIterationLvl2
         pop     cx
        popa
        ret

    LevelPrint3:
        mov     di, offset Level3    ;printing word score
	    push    cx
        mov     cx, 2320
        mov     si, cx
        add     cx, 10

    RenderIterationLvl3:
		xor     bx, bx
		mov     bl, byte ptr [di]
		mov     bh, scoreAttr
		mov     word ptr es:[si], bx
		inc     di
		add     si, 2
		cmp     si,cx
		jne     RenderIterationLvl3
         pop     cx
        popa
        ret

    LevelPrint4:
        mov     di, offset Level4    ;printing word score
	    push    cx
        mov     cx, 2320
        mov     si, cx
        add     cx, 14

    RenderIterationLvl4:
		xor     bx, bx
		mov     bl, byte ptr [di]
		mov     bh, scoreAttr
		mov     word ptr es:[si], bx
		inc     di
		add     si, 2
		cmp     si,cx
		jne     RenderIterationLvl4
         pop     cx
        popa
        ret
endp

toString proc
    pusha
    xor     di, di
    lea     di, stringScore
    mov     ax, score
    xor     bx, bx
    mov     bx, di
    xor     cx, cx
    mov     cx, 5

    setZeroString:
        mov     [di], byte ptr '0'
        loop    setZeroString
    
    lea     di, stringScore

    itoaLoop:
        xor     dx, dx
        div     base  
        add     dl, byte ptr '0'
        mov     [di], dl
        inc     di                   
        cmp     ax, 0
        ja      itoaLoop

    lea     di, stringScore
    add     di, 4
    lea     si, stringScore 

    reverseMini:
        xor     dx, dx
        xor     cx, cx  
        mov     dl, byte ptr [si]  
        mov     [di], dl
        mov     [si], byte ptr '0'
        inc     si
        dec     di
        cmp     si, di
        jb      reverseMini
    popa
    ret
endp

ComplitedRowCheck proc
    pusha
    
    mov     bp, downBorder
    dec     bp
    mov     al, mainCharacter
    mov     cx, bp

    ComplitedRowCheckLoop:
        push    cx
        mov     cx, rightBorder
        dec     cx
        push    ax
        mov     ax, bp
        mov     bx, 1
        call    GetScreenOffset ; ax = y bx = x  dx = screen offset
        pop     ax

        mov     bx, dx

        checkCurrentRowLoop:
            cmp     byte ptr es:[bx], al
            jne     checkNextRow
            add     bx, 2
            loop    checkCurrentRowLoop

        mov     bx, dx

        CommandEraseBlocks 1, bp, 20, 1
        
        inc     score
        mov     incrementSpeedFlag,1
        push    ds
        push    es
        pop     ds
        push    bp
        MoveRows:
            mov     di, bx              ; di on cleaning row
            sub     bx, screenWidth
            mov     si, bx              ; si on previous row

            mov     cx, 20
            rep     movsw               ; move down row

            dec     bp
            cmp     bp, 1
            ja      MoveRows
        
        pop     bp	
        pop     ds
        mov     bp,25                   ;for multiple rows moving
        checkNextRow:
            dec     bp                  ; y--
            pop     cx
        
        loop ComplitedRowCheckLoop

    popa
    ret
endp

printScore proc
    push    bx
    push    si
    push    di

    call    toString

    mov     di, offset stringScore
	push    cx
    mov     cx, 2000
    mov     si, cx
    add     cx, 10

    nextRenderIteration:
		xor     bx, bx
		mov     bl, byte ptr [di]
		mov     bh, scoreAttr
		mov     word ptr es:[si], bx
		inc     di
		add     si, 2
		cmp     si,cx
		jne     nextRenderIteration
    
    pop     cx
    pop     di
    pop     si
    pop     bx
    
    ret
endp

SidesCheck proc
    push    ax
    push    bx
    push    cx
    xor     ax, ax
    xor     bx, bx
    xor     cx, cx
    xor     dx, dx

    mov     ax, itemY 
    mov     bx, itemX 
    
    call    GetScreenOffset                 ; ax = y bx = x dx = screen offset

    mov     bx, dx
    sub     bx, 2

    mov     ax, itemWidth
    mov     cx, 2
    mul     cx
    add     ax, 2

    mov     cl, byte ptr itemHeight
    mov     dl, byte ptr mainCharacter

    SidesCheckLoop:
        cmp     byte ptr es:[bx], dl
        je      disableSideMove

        add     bx, ax

        cmp     byte ptr es:[bx], dl
        je      disableSideMove

        add     bx, screenWidth
        sub     bx, ax
        loop    SidesCheckLoop
    cmp     byte ptr es:[bx], dl
    je      disableSideMove
    add     bx, ax
    cmp     byte ptr es:[bx], dl
    je      disableSideMove

    mov     dx, 0
    jmp     endChecking

    disableSideMove:
        mov     dx, 1
    endChecking:
        pop     cx
        pop     bx
        pop     ax
    ret
endp

RotateAllowsCheck proc
    push    cx
    xor     cx, cx

    cmp     byte ptr ds:[rotatePos], 0
    je      initialRotate

    cmp     byte ptr ds:[rotatePos], 1
    je      firstStepRotate

    cmp     byte ptr ds:[rotatePos], 2
    je      secondStepRotate
    jmp     far ptr     thirdStepRotate

    initialRotate:
                ;bl = h2
        mov     bl, byte ptr itemWidth	   
                ;bl = h2 - h1
	    sub     bl, byte ptr itemHeight	
                ;cl = h2 - h1 iterations
	    mov     cl, bl 						
    
	    mov     al, byte ptr itemY		  
	    mov     bl, byte ptr itemX     
	    call    GetScreenOffset 	    ; ax = y bx = x dx = screen offset
	    mov     bx, dx

	    mov     dl, byte ptr mainCharacter
        mov     ax, word ptr BorderBlock
        
        initialRotateLoop:
            sub     bx, screenWidth 			
		    cmp     byte ptr es:[bx], dl
		    je      farJmpToBlockRotating
		    cmp     word ptr es:[bx], ax
		    je      farJmpToBlockRotating
	        loop    initialRotateLoop
        
        jmp     nonBlockRotating

        farJmpToBlockRotating:
            jmp     far ptr blockRotating

    firstStepRotate:
                ;bl = w2
        mov     bl, byte ptr itemHeight 	
                ;bl = w2 - w1
	    sub     bl, byte ptr itemWidth 	
                ;cl = w2 - w1 iterations 
	    mov     cl, bl						

	    mov     al, byte ptr itemY		
	    mov     bl, byte ptr itemX 	
	    call    GetScreenOffset	        ; ax = y  bx = x dx = screen offset
	    mov     bx, dx

	    push    cx
	    mov     cl, byte ptr itemHeight
        dec     cl

        firstStepRotateDown:
            add     bx, screenWidth 
	        loop    firstStepRotateDown
        
        pop     cx

        mov     dl, byte ptr mainCharacter
        mov     ax, word ptr BorderBlock
        
        firstStateRotateLoop:
            sub     bx, 2 						
		    cmp     byte ptr es:[bx], dl
		    je      blockRotatingjump
		    cmp     word ptr es:[bx], ax
		    je      blockRotatingjump
            loop    firstStateRotateLoop
        
        jmp     nonBlockRotating
        blockRotatingjump:
        jmp     far ptr blockRotating
    secondStepRotate:
                ;bl = h2
        mov     bl, byte ptr itemWidth	
                ;bl = h2 - h1
	    sub     bl, byte ptr itemHeight 	
                ;cl = h2 - h1 iterations
	    mov     cl, bl							

	    mov     bl, byte ptr itemX	
	    mov     al, byte ptr itemWidth
	    dec     al
	    mov     dl, 2
	    mul     dl

	    add     bx, ax
	    xor     ax, ax
	    mov     al, byte ptr itemY 		
	    call    GetScreenOffset 			; ax = y bx = x dx = screen offset
	    mov     bx, dx

	    mov     dl, byte ptr mainCharacter
        mov     ax, word ptr BorderBlock
        
	    secondStepRotateLoop:
	    	sub     bx, screenWidth 			
	    	cmp     byte ptr es:[bx], dl
	    	je      blockRotating
	    	cmp     word ptr es:[bx], ax
	    	je      blockRotating
            loop    secondStepRotateLoop
            
	    jmp     nonBlockRotating

    thirdStepRotate:
                ;bl = w2
        mov     bl, byte ptr itemHeight 
                ;bl = w2 - w1
	    sub     bl, byte ptr itemWidth 
                ;cl = w2 - w1 (iterations count)
	    mov     cl, bl							

	    mov     al, byte ptr itemY 		
	    mov     bl, byte ptr itemX 		
	    call    GetScreenOffset 				; ax = y bx = x dx = screen offset
	    mov     bx, dx

	    mov     dl, byte ptr mainCharacter
        mov     ax, word ptr BorderBlock
        
	    thirdStepRotateLoop:
            push    ax
            push    dx
            xor     ax,ax
            mov     ax, word ptr itemWidth
            mul     justtwo
            add     bx, ax
            pop     ax 
            pop     dx
	    	cmp     byte ptr es:[bx], dl
	    	je      blockRotating
	    	cmp     word ptr es:[bx], ax
	    	je      blockRotating
            loop    thirdStepRotateLoop
        
	    jmp     nonBlockRotating

    blockRotating:
	    mov     byte ptr ds:[rotateAllowedFlag], 1
	    jmp     endCheckRotating

	nonBlockRotating:
	    mov     byte ptr ds:[rotateAllowedFlag], 0

	endCheckRotating:
	    pop     cx
        ret
endp

ItemRotate proc
    pusha

    xor     ax, ax
	xor     bx, bx
	xor     dx, dx

    call    RotateAllowsCheck
    cmp     byte ptr rotateAllowedFlag, 1
	jne     continueRotating

    jmp     far ptr endItemRotate

    continueRotating:

    CommandEraseBlocks itemX, itemY, itemWidth, itemHeight
    
    cmp     byte ptr rotatePos, 0
    je      rotateInitial

    cmp     byte ptr rotatePos, 1
    je      firstRotate

    cmp     byte ptr rotatePos, 2
    je      secondRotate

    cmp     byte ptr rotatePos, 3
    je      thirdRotate
    
    rotateInitial:
                ;bl = h2
        mov     bl, byte ptr itemWidth 	
                ;bl = h2 - h1
	    sub     bl, byte ptr itemHeight	
                ;bl = y2 = y1 + (h2 - h1)
	    sub     byte ptr ds:itemY, bl		
    
	    ; swap width and height
	    mov     al, byte ptr itemWidth
	    xchg    al, byte ptr itemHeight
	    mov     byte ptr itemWidth, al
    
	    ;set current rotate state
	    inc     rotatePos		;set 1
        jmp     endItemRotate
        
    firstRotate:
                ;bl = w2
        mov     bl, byte ptr itemHeight 	
                ;bl = w2 - w1
	    sub     bl, byte ptr itemWidth 	
                ;x2 = x1 - (w2 - w1)
	    sub     byte ptr itemX, bl 		
                ;bl = h1
	    mov     bl, byte ptr itemHeight	
                ;bl = h1 - h2
	    sub     bl, byte ptr itemWidth 	
	    add     byte ptr itemY, bl

        ; swap width and height
	    mov     al, byte ptr itemWidth
	    xchg    al, byte ptr itemHeight
	    mov     byte ptr itemWidth, al
	
	    ;set current rotate state
	    inc     rotatePos		;set 2
		jmp     endItemRotate
	
    secondRotate:
                ;bl = w1
        mov     bl, byte ptr itemWidth 	
                ;bl = w1 - w2
	    sub     bl, byte ptr itemHeight 	
                ;x2 = x1 + (w1 - w2)
	    add     byte ptr itemX, bl 		

	    ; swap width and height
	    mov     al, byte ptr itemWidth
	    xchg    al, byte ptr itemHeight
	    mov     byte ptr itemWidth, al
    
	    ;set current rotate state
	    inc     rotatePos		;set 3
		jmp     endItemRotate

    thirdRotate:
        ; swap width and height 
	    mov     al, byte ptr itemWidth
	    xchg    al, byte ptr itemHeight
	    mov     byte ptr itemWidth, al

	    ;set current rotate state
	    mov     byte ptr rotatePos, 0	;set 0

    endItemRotate:
        popa
        ret
endp

ItemMove proc
    pusha

    call    BlockBordersCheck
    cmp     dx, 1
    je      createNewItem

    CommandEraseBlocks itemX, itemY, itemWidth, itemHeight

    cmp     ah, keyLeft
	je      moveLeft
	
	cmp     ah, keyRight
	je      moveRight

	jmp     moveDown

	moveLeft:
		cmp     itemX, leftBorder
		jbe     moveDown

		call    SidesCheck
		cmp     dx, 1
		je      moveDown

		sub     itemX, 1
		jmp     moveDown

    moveRight:
		xor     dx, dx
		xor     bx, bx

		mov     dl, byte ptr itemX
		mov     bl, byte ptr itemWidth
		add     dx, bx

		cmp     dx, rightBorder
		jae     moveDown

		call    SidesCheck
		cmp     dx, 1
		je      moveDown

		add     itemX, 1
		jmp     moveDown		

    createNewItem:
        pop     dx
	    pop     bx
	    pop     ax

	    mov     rotatePos, 0
	    mov     creatingItemFlag, 1
        call    ComplitedRowCheck
        jmp     exitFromMoving

    moveDown:
        add     itemY, 1
        xor     dx, dx
        CommandDrawBlocks    itemX, itemY, itemWidth, itemHeight, tempBlock
        
    exitFromMoving:

    popa
    ret
endp

ActionHandler proc
    cmp     ah, keyLeft 
    je      ItemMoveMark
    cmp     ah, keyRight
    je      ItemMoveMark
    cmp     ah, keyUp
    je      ItemRotateMark

    jne     ItemMoveMark

    ItemRotateMark:
		call    ItemRotate
		CommandDrawBlocks    itemX, itemY, itemWidth, itemHeight, tempBlock
	
    ItemMoveMark:
		call    ItemMove
		jmp     endAction

    endAction:
    ret
endp

CheckAction proc
    push    ax
    
    mov     ah, 1 	;check for key pressed
    int     16h 	;keyboard interrupt

    jz      endCheckInput 	

    mov     ah, 0 	;get key
    int     16h 	;read key
    
    endCheckInput:
    call    ActionHandler
    pop     ax
    
    ret
endp

RandomItemType proc
    push    dx
    xor     dx, dx

    cmp     byte ptr randomNum, 0
    je      setFirstType
    cmp     byte ptr randomNum, 1
    je      setSecondType
    cmp     byte ptr randomNum, 2
    je      setThirdType

    setFirstType:	
        mov     dh, redBlock
        mov     dl, mainCharacter
        mov     tempBlock, dx
        mov     itemWidth, 4
        mov     itemHeight, 2
    jmp     endSelectType

    setSecondType:	
        mov     dh, greenBlock
        mov     dl, mainCharacter
        mov     tempBlock, dx
        mov     itemWidth, 2
        mov     itemHeight, 2
    jmp     endSelectType

    setThirdType:	
        mov     dh, blueBlock
        mov     dl, mainCharacter
        mov     tempBlock, dx
        mov     itemWidth, 2
        mov     itemHeight, 1
    jmp     endSelectType

    endSelectType:
        pop     dx
        ret
endp

SkipDelay proc
    pusha    

    xor     cx,cx
    xor     dx, dx
    xor     ax, ax
    mov     ah, 86h
    mov     cx, 4
    int     15h    

    popa
    ret
endp

BlockBordersCheck proc 
    ;dx flag
    push    ax
    push    bx
    push    cx

    xor     ax, ax
    xor     bx, bx
    xor     cx, cx

    mov     ax, itemY
    mov     bx, itemX
    call    GetScreenOffset ;dx = screen offset

    mov     bx, dx
    xor     dx, dx

    mov     cx, itemHeight

	checkForBordersHeight:
		add     bx, screenWidth
	    loop    checkForBordersHeight

    mov     cx, itemWidth

	checkForBordersWidth:
		mov     ax, word ptr BorderBlock
        cmp     ax, word ptr es:[bx]
        
		je      CheckForBordersSuccess

		mov     al, mainCharacter
        cmp     al, byte ptr es:[bx]
    
		je      CheckForBordersSuccess

		add     bx, 2
        loop    checkForBordersWidth
    
	jmp     EndCheckForBorders

	CheckForBordersSuccess:
	    mov     dx, 1
	
	EndCheckForBorders:
	    pop     cx
	    pop     bx
	    pop     ax

    ret
endp

ItemCreate proc
    call    SkipDelay           ;for real random
    call    RandomNumber
    call    RandomItemType
    
    mov     itemX, 10           ;start falling
    mov     itemY, 1

    push    dx
	call    BlockBordersCheck
	cmp     dx, 1
	je      ItemCreateError
	pop     dx

    CommandDrawBlocks    itemX, itemY, itemWidth, itemHeight, tempBlock
    mov     creatingItemFlag, 0 
    ret

    ItemCreateError:
        pop     dx
	    mov     creatingItemFlag, 0 
        mov     endBaseCycleFlag, 1 
    ret
endp

BaseCycle proc
	
    mov     ah, 0               ;get timer count
    int     1Ah                 ;dx - lower byte
    call    printScore
    
    cmp     incrementSpeedFlag, 1   
    jne     frameRender
    cmp     delay, 1            ;min value
    je      frameRender
    dec     delay               ;increment speed
    dec     incrementSpeedFlag      
    call    printLevel
    frameRender:
        xor     cx, cx
	    cmp     dx, startTime   ;if not enough time
	    jb      skipIteration
        add     dx, delay
	    mov     startTime, dx   ;new delay
        cmp     creatingItemFlag, 0
	    je      skipCreating 
        call    ItemCreate
        cmp     endBaseCycleFlag, 1
	    je      BaseCycleOver

    skipCreating:
        call    CheckAction     
    
    skipIteration:
    ret
endp

exit proc
    mov ah, 0h
	mov al, 3h
	int 10h
    
    mov     ax, 4c00h
    int     21h
endp

start proc

    
    call    StartState  
     
    CommandDrawBlocks    0, 0, 30, 25, blackSymbol
        mov     di, offset wordStart    ;printing word start
	    push    cx
        mov     cx, 1940
        mov     si, cx
        add     cx, 22

    RenderIterationStart:

		xor     bx, bx
		mov     bl, byte ptr [di]
		mov     bh, startAttr
		mov     word ptr es:[si], bx
		inc     di
		add     si, 2
		cmp     si,cx
		jne     RenderIterationStart
        
        mov     di, offset wordPowered    ;printing word start
	    push    cx
        mov     cx, 2096
        mov     si, cx
        add     cx, 28

    RenderIterationStart2:

		xor     bx, bx
		mov     bl, byte ptr [di]
		mov     bh, powerAttr
		mov     word ptr es:[si], bx
		inc     di
		add     si, 2
		cmp     si,cx
		jne     RenderIterationStart2



    keyWaitStart:
    
        xor     ax,ax
        mov     ah, 1 	;check for key pressed
        int     16h 	;keyboard interrupt
        jz      keyWaitStart 	;if zf=1 - key not pressed



    call    InitialDraw
    
    BaseCycleLoop:
        call    BaseCycle
        jmp     BaseCycleLoop
    
    BaseCycleOver:

        CommandDrawBlocks    0, 0, 30, 25, blackSymbol
        mov     di, offset wordOver    ;printing word score
	    push    cx
        mov     cx, 1950
        mov     si, cx
        add     cx, 18

    RenderIterationOver:

		xor     bx, bx
		mov     bl, byte ptr [di]
		mov     bh, overAttr
		mov     word ptr es:[si], bx
		inc     di
		add     si, 2
		cmp     si,cx
		jne     RenderIterationOver

    keyWait:
    
        xor     ax,ax
        mov     ah, 1 	;check for key pressed
        int     16h 	;keyboard interrupt
        jz      keyWait 	;if zf=1 - key not pressed
        call    exit
endp

end     start