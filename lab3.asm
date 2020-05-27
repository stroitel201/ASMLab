data segment
    buffer db 30, 0, 30 dup(0)
    array dw 30 dup(0) 
    array_len   dw ?
    start_msg   db "Enter count of array (from 2 to 30): ", 0Dh, 0Ah, '$'  
    getLowRangeRequest  db 'Enter low range ', '$'
    getHighRange        db 0Dh, 0Ah, 'Enter high range ', '$'
    lowRange            dw      0
    highRange           dw      0
    minElem             dw      0
    maxElem             dw      0
    CrLf        db 0Dh, 0Ah, '$'
    input_msg   db "Enter array :", 0Dh, 0Ah, '$'   
    output_msg  db "New array :", 0Dh, 0Ah, '$'    
    error       db "Input error", 0Dh, 0Ah, '$'
    pkey db "press any key...$"
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
; set segment registers:
    mov     ax, data
    mov     ds, ax
    mov     es, ax    
    
    lea     dx, start_msg
    call    print  
    
    mov     ah, 0Ah
    lea     dx, buffer
    int     21h  
    
    lea     dx, CrLf
    call    print

    push    array_len  
    call    parse
    pop     array_len 
    cmp     array_len, 2
    jl      exception
    cmp     array_len, 30
    jg      exception
    
    mov     cx, array_len
    lea     si, array
    
    lea     dx, input_msg
    call    print   
    
input_of_array:
        
    mov     ah, 0Ah
    lea     dx, buffer
    int     21h   
    
    lea     dx, CrLf
    call    print 
    
    push    word ptr[si]
    call    parse
    pop     word ptr[si]
    add     si, 2
    
    loop    input_of_array  
    
    lea     dx,getLowRangeRequest
    call    print
    mov     ah, 0Ah
    lea     dx, buffer
    int     21h
    call    parse
    pop     lowrange 
    
    lea     dx,getHighRange
    call    print
    mov     ah, 0Ah
    lea     dx, buffer
    int     21h
    call    parse
    pop     highrange
    mov     ax,highrange
    mov     dx,lowrange
    cmp     ax, dx
    jl      exception
    
    
    lea     dx, CrLf
    call    print
    lea     dx, output_msg
    call    print        
    
    call    convert   
    
    lea     dx, CrLf
    call    print 
            
exit:
    lea     dx, pkey
    mov     ah, 9
    int     21h        
    
        
    mov     ah, 1
    int     21h
    
    mov     ax, 4c00h 
    int     21h  
    

convert proc
        
       
            mov    cx, array_len
            lea    bx, array 
            mov    ax, [bx]

maxvalue:   cmp ax, [bx]     
            jge maxloop      
            mov ax, [bx]    
maxloop:
            add bx, 2       
            loop maxvalue  
            
            mov maxElem, ax 
            
            mov    cx, array_len
            lea    bx, array 
            mov    ax, [bx]

minvalue:   cmp ax, [bx]    
            jle minloop     
            mov ax, [bx]    
minloop:
            add bx, 2       
            loop minvalue  
            
            mov minElem, ax  
    ;=== ax - element  ==================================
    ;======          A[i] - minElem                                       =======
    ;====== A[i] = ------------------ * (highRange - lowRange) + lowRange =======
    ;======        maxElem - minElem                                      =======            
            mov    cx, array_len
            lea    bx, array 
             
calculate:
      mov  ax, [bx]
      push bx           
      sub   ax, minElem
      jo    exception  
      mov   bx, highRange
      sub   bx, lowRange
      jo    exception  
      imul  bx 
      jo    exception  
      mov   bx, maxElem
      sub   bx, minElem
      jo    exception  
      mov   dx, 0
      idiv  bx
        
      add   ax, lowRange
      jo    exception     
      push  ax
      cmp   dx,0
      je   finish
      
      mov   ax,bx 
      mov   bx,dx 
      mov   dx,0
      div   bx
      cmp   ax, 2
      jg    finish
      pop   ax
      inc   ax
      push  ax
      
finish: 
      pop   ax
      pop   bx
      push  ax
      call  to_string
       
      lea   dx, CrLf
      call  print 
      lea   dx, buffer+2
      call  print 
      
      add   bx,2 
      loop calculate 
       
        ret 
convert endp
    
    
        
to_string proc 
        push    bp
        mov     bp, sp
        push    si
        push    di
        push    ax 
        push    bx
        push    cx
        push    dx
                       
        lea     si, buffer+2
        mov     di, si 
        mov     cx, 0 
        mov     ax, [bp+4]   
        push    ax
        mov     bx, 10 
        xor     dx, dx   
        and     ax, 8000h      
        cmp     ax, 0           
        pop     ax
        je      loop1
        not     ax
        inc     ax          
        mov     buffer+1, 1
        mov     buffer+2, 2Dh 
        inc     di        
          
    loop1:         
        mov     dx, 0
        idiv    bx 
        push    dx
        inc     cx
        cmp     ax, 0
        jnz     loop1
        
        mov     buffer+1, cl        
           
        
    loop2:
        pop     ax   
        add     ax, 30h
        stosb
        loop loop2 
        
        mov     byte ptr [di], '$' 
        
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        pop     di
        pop     si
        pop     bp
        ret 2
    to_string endp
    
    
parse proc
       push    bp
        mov     bp, sp
        push    ax
        push    bx
        push    cx
        push    si
       
        mov     dx, 0    
        push    dx
 
        xor     ax, ax
        mov     bx, 0
       
        mov     cx, 0
        mov     cl, [buffer+1]
       
        lea     si, buffer+2  
       
        cld
               
        lodsb  
        dec     si
        cmp     al, 2Dh        
        jne     For
        inc     si
        dec     cl  
        pop     dx
        mov     dl, 1
        push    dx
       
   
    For:
        lodsb  
        sub     al, '0'  
        cmp     al, 10
        jge     exception  
       
        push    ax
        mov     ax, bx
        mov     bx, 10
        mul     bx
       
        cmp     dx, 0
        jne     exception
       
        mov     bx, ax
        pop     ax
        add     bx, ax
       
        push    bx
        and     bh, 80h
        cmp     bh, 0
        jne     exception  
        pop     bx
       
        loop    For
                     
        pop     dx
        cmp     dl, 0
        jnz     negative
       
        mov     [bp+4], bx
        jmp     end_function
       
    negative:    
        sub     bx, 1
        not     bx
        mov     [bp+4], bx
        jmp     end_function
       
    exception:
        lea     dx, error
        call    print    
        jmp     exit
    end_function:      
        pop     si
        pop     cx
        pop     bx
        pop     ax
        pop     bp
       
        ret
    parse endp
        
             
    
    print       proc 
        
        push    ax
        mov     ah, 09h
        int     21h
        pop     ax
        ret
    print endp
ends

end start 