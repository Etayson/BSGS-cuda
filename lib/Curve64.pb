EnableExplicit
CompilerIf #PB_Compiler_Unicode
  Debug" switch to Ascii mode"
  End
CompilerEndIf
CompilerIf Not #PB_Compiler_Processor = #PB_Processor_x64
  Debug" only x64 processor support"
  End
CompilerEndIf

DeclareModule Curve
  Declare m_sethex32(*buffer, *string_pointer)
  Declare$ m_gethex32(*buffer)
  Declare m_serializeX64(*bufferFrom,offset,*bufferTo,counter=8)
  Declare.i m_IsInfinityX64(*buffer)
  Declare.i m_check_nonzeroX64(*buffer)
  Declare.i m_check_less_more_equilX64(*bufferA,*bufferB)
  Declare.i m_check_equilX64(*bufferA,*bufferB)
  Declare m_Ecc_TestBitX64(*buffer, testbit)
  Declare m_Ecc_AndX64(*bufferResult, *bufferA, *bufferB)
  Declare m_Ecc_OrX64(*bufferResult, *bufferA, *bufferB)
  Declare m_shrX64(*buffer)
  Declare m_shlX64(*buffer)
  Declare m_NegX64(*bufferResult,*bufferA)
  Declare m_NegModX64(*bufferResult,*bufferA,*p)
  Declare m_addX64(*bufferResult, *bufferA, *bufferB)
  Declare m_subX64(*bufferResult, *bufferA, *bufferB)  
  Declare m_subModX64(*bufferResult, *bufferA, *bufferB, *p)
  Declare m_addModX64(*bufferResult, *bufferA, *bufferB, *p)
  Declare m_mulModX64(*bufferResult, *bufferA, *bufferB, *p, *high)
  Declare m_squareModX64(*bufferResult,*bufferA,*p, *r512)
  Declare m_Ecc_modInvX64(*bufferResult, *bufferA, *p)
  Declare m_YfromX64(*bufferResult,*bufferA,*p)
  Declare m_DBLTX64(*resultX, *resultY, *PointX, *PointY, *p)  
  Declare m_ADDPTX64(*resultX,*resultY,*APointX,*APointY,*BPointX,*BPointY,*p)
  Declare m_PTMULX64(*resultX, *resultY, *APointX, *APointY, *buffer,*p)
  Declare m_PTDIVX64(*resultX, *resultY, *APointX, *APointY, *buffer,*p, *n)
  Declare m_EccPoint_multX64(*resultX, *resultY, *GPointX, *GPointY, *buffer)  
  Declare beginBatchAdd(*Invout, totalpoints, *apointX, *apointY,  *pointarr)
  Declare completeBatchAddWithDouble(*newpointarr, lenline, totalpoints, *apointX, *apointY,  *pointarr, *InvTotal)
  Declare fillarrayN(*pointarr, totalpoints, *apointX, *apointY)
  Declare.i m_getCurveValues()
  Declare m_deserializeX64(*a,b,*sptr,counter=8)
  Declare m_serializeX64(*a,b,*sptr,counter=8)
EndDeclareModule

Module Curve
  EnableExplicit 
  Define *CurveP, *CurveGx, *CurveGY, *Curveq1, *Curveq2, *Curveqn, *Curveqncut
  *CurveP = AllocateMemory(160)
  *CurveGx = *CurveP+32
  *CurveGY = *CurveP+64  
  *Curveqn = *CurveP+96
  *Curveqncut=*CurveP+128
  m_sethex32(*CurveP, @"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F")
  m_sethex32(*CurveGx, @"79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798")
  m_sethex32(*CurveGy, @"483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8")  
  m_sethex32(*Curveqn,@"fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141")
  m_sethex32(*Curveqncut,@"000000000000000000000000000000febaaedce6af48a03bbfd25e8cd0364141")
  
  
Macro move16b_1(offset_target_s,offset_target_d)  
  !movdqa xmm0,[rdx++offset_target_s]
  !movdqa [rcx+offset_target_d],xmm0
EndMacro

Macro move32b_(s,d,offset_target_s,offset_target_d)
  !mov rdx, [s]
  !mov rcx, [d]  
  move16b_1(0+offset_target_s,0+offset_target_d)
  move16b_1(16+offset_target_s,16+offset_target_d) 
EndMacro

Macro mul4blo(offset)  
  !mov rax,r10
  !mov rbx, [rdi+offset]
  !mul rbx ;edx:eax
  !mov [rsi+offset], rax
EndMacro

Macro madhi(offsetB, offsetC)  
 !mov rbx, [rdi+offsetB] ;rbx lo32b  =ebx
 !mov rax,r10
 !mul rbx;edx:eax 
 !add rdx,[rsi+offsetC]
 ;store carry
 !mov rcx,0
 !adc rcx, rcx
 
 !mov [rsi+offsetC], rdx
EndMacro

Macro madhicc(offsetB, offsetC) 
 !mov rbx, [rdi+offsetB] ;rbx lo32b  =ebx
 !mov rax,r10
 !mul rbx;edx:eax 
 ;add carry
 !add rdx,rcx
 !add rdx,[rsi+offsetC]
 
 ;store carry
 !mov rcx,0
 !adc rcx, rcx
 
 !mov [rsi+offsetC], rdx
EndMacro

Macro madlo(offsetB, offsetC)  
 !mov rbx, [rdi+offsetB] 
 !mov rax,r10
 !mul rbx 
 !add rax,[rsi+offsetC]
 ;store carry
 !mov rcx,0
 !adc rcx, rcx
 
 !mov [rsi+offsetC], rax
EndMacro

Macro madlocc(offsetB, offsetC)  
 !mov rbx, [rdi+offsetB] 
 !mov rax,r10
 !mul rbx 
 ;add carry
 !add rax,rcx
 !add rax,[rsi+offsetC]
 
 ;store carry
 !mov rcx,0
 !adc rcx, rcx
 
 !mov [rsi+offsetC], rax
EndMacro

Macro madhiccHIGH(offsetB, offsetHIGH) 
 !mov rbx, [rdi+offsetB] ;b
 !mov rax,r10;a
 !mul rbx;edx:eax 
 ;add carry
 !add rdx,rcx
 !add rdx,[r8+offsetHIGH];high
 
 ;store carry
 !mov rcx,0
 !adc rcx, rcx
 
 !mov [r8+offsetHIGH], rdx
EndMacro

Macro madloccHIGH(offsetB, offsetHIGH)  
 !mov rbx, [rdi+offsetB];b
 !mov rax,r10;a
 !mul rbx 
 !add rax,rcx
 ;store carry
 !add rax,[r8+offsetHIGH]
 ;add carry
 
 !mov rcx,0
 !adc rcx, rcx
 
 !mov [r8+offsetHIGH], rax
EndMacro

Macro madhiccHIGHZero(offsetB, offsetHIGH) 
 !mov rbx, [rdi+offsetB] ;b
 !mov rax,r10;a
 !mul rbx;edx:eax 
 ;add carry
 !add rdx,rcx
 ;!add rdx,[r8+offsetHIGH];high first time HIGH shold be zero
 
 ;store carry
 !mov rcx,0
 !adc rcx, rcx
 
 !mov [r8+offsetHIGH], rdx
EndMacro


Procedure m_getCurveValues()
  Shared *CurveP
 ProcedureReturn *CurveP
EndProcedure

Procedure m_deserializeX64(*a,b,*sptr,counter=8)
  Protected *ptr
    *ptr=*a+64*b  
  
  !mov rbx,[p.p_ptr] 
  !mov rdi,[p.p_sptr] 
  
  !mov rax,[p.v_counter]
  !cmp rax,0
  !jz llm_MyLabelfexit ;if counter is zero break
  !dec rax
  !shl rax,2 ;*4
  !add rbx,rax
  
  !xor cx,cx  
  !llm_MyLabelf:
  
  !push cx
  !mov rax,[rdi]
  !mov rcx,rax
  !xor edx,edx
  
   
  !sub al,48  
  !cmp al,15     
  !jb llm_MyLabelf1        
  !sub al,7
  
  !llm_MyLabelf1:
  !and al,15      ;1
  !or dl,al  
  !rol edx,4
  !ror rcx,8
  !mov al,cl
  
  !sub al,48  
  !cmp al,15     
  !jb llm_MyLabelf2        
  !sub al,7
  
  !llm_MyLabelf2:
  !and al,15      ;2
  !or dl,al  
  !rol edx,4
  !ror rcx,8
  !mov al,cl
  
  !sub al,48  
  !cmp al,15     
  !jb llm_MyLabelf3        
  !sub al,7
  
  !llm_MyLabelf3:
  !and al,15      ;3
  !or dl,al  
  !rol edx,4
  !ror rcx,8
  !mov al,cl
  
  !sub al,48  
  !cmp al,15     
  !jb llm_MyLabelf4        
  !sub al,7
  
  !llm_MyLabelf4:
  !and al,15      ;4
  !or dl,al  
  !rol edx,4
  !ror rcx,8
  !mov al,cl
  
  !sub al,48  
  !cmp al,15     
  !jb llm_MyLabelf5        
  !sub al,7
  
  !llm_MyLabelf5:
  !and al,15      ;5
  !or dl,al  
  !rol edx,4
  !ror rcx,8
  !mov al,cl
  
  !sub al,48  
  !cmp al,15     
  !jb llm_MyLabelf6        
  !sub al,7
  
  !llm_MyLabelf6:
  !and al,15      ;6
  !or dl,al  
  !rol edx,4
  !ror rcx,8
  !mov al,cl
  
  !sub al,48  
  !cmp al,15     
  !jb llm_MyLabelf7       
  !sub al,7
  
  !llm_MyLabelf7:
  !and al,15      ;7
  !or dl,al  
  !rol edx,4
  !ror rcx,8
  !mov al,cl
  
  !sub al,48  
  !cmp al,15     
  !jb llm_MyLabelf8        
  !sub al,7
  
  !llm_MyLabelf8:
  !and al,15      ;8
  !or dl,al  
  
  ;!ror rdx,8
  !mov [rbx],edx
  !add rdi,8
  !sub rbx,4
  
  
  !pop cx   
  !inc cx 
  !cmp cx,[p.v_counter]
  !jb llm_MyLabelf 
  
  !llm_MyLabelfexit:
  

EndProcedure

Procedure m_serializeX64(*a,b,*sptr,counter=8);>hex  
 Protected *ptr
  *ptr=*a+32*b  
  
  !mov rbx,[p.p_ptr] ;bin input
  !mov rdi,[p.p_sptr] ;hex output
  
  !mov rax,[p.v_counter]
  !cmp rax,0
  !jz llm_MyLabelexit ;if counter is zero break
  !dec rax
  !shl rax,3 ;*8
  !add rdi,rax
  
  !xor cx,cx
  !llm_MyLabel:
  
  !push cx
  
  !mov eax,[rbx]; get 4bytes>8hex digit
  !xor rdx,rdx ;4B each 2bhex = 8b total
  
  !mov ecx,eax
  
  !and ax,0fh
  !cmp al,10     ;1
  !jb llm_MyLabel1        
  !add al,39  
  !llm_MyLabel1:
  !add al,48   
  !or dx,ax
  !shl rdx,8  
  !ror ecx,4
  
  !mov ax,cx  
  !and ax,0fh
  !cmp al,10     ;2
  !jb llm_MyLabel2        
  !add al,39  
  !llm_MyLabel2:
  !add al,48   
  !or dx,ax
  !shl rdx,8  
  !ror ecx,4  
  
  !mov ax,cx  
  !and ax,0fh
  !cmp al,10     ;3
  !jb llm_MyLabel3        
  !add al,39  
  !llm_MyLabel3:
  !add al,48   
  !or dx,ax
  !shl rdx,8  
  !ror ecx,4
  
  !mov ax,cx  
  !and ax,0fh
  !cmp al,10     ;4
  !jb llm_MyLabel4        
  !add al,39  
  !llm_MyLabel4:
  !add al,48   
  !or dx,ax
  !shl rdx,8  
  !ror ecx,4
  
  !mov ax,cx  
  !and ax,0fh
  !cmp al,10     ;5
  !jb llm_MyLabel5        
  !add al,39  
  !llm_MyLabel5:
  !add al,48   
  !or dx,ax
  !shl rdx,8  
  !ror ecx,4
  
  !mov ax,cx  
  !and ax,0fh
  !cmp al,10     ;6
  !jb llm_MyLabel6        
  !add al,39  
  !llm_MyLabel6:
  !add al,48   
  !or dx,ax
  !shl rdx,8  
  !ror ecx,4
  
  !mov ax,cx  
  !and ax,0fh
  !cmp al,10     ;7
  !jb llm_MyLabel7        
  !add al,39
  !llm_MyLabel7:
  !add al,48   
  !or dx,ax
  !shl rdx,8  
  !ror ecx,4
  
  !mov ax,cx  
  !and ax,0fh
  !cmp al,10     ;8
  !jb llm_MyLabel8        
  !add al,39  
  !llm_MyLabel8:
  !add al,48   
  !or dx,ax
  ;!ror edx,16
  !mov [rdi],rdx
  !sub rdi,8
  !add rbx,4
  
  !pop cx
  !inc cx
  !cmp cx,[p.v_counter]; words
  !jb llm_MyLabel 
  
  !llm_MyLabelexit:
EndProcedure

Procedure.s m_cutHex(a$)
  a$=Trim(UCase(a$)) 
  If Left(a$,2)="0X" 
    a$=Mid(a$,3,Len(a$)-2)
  EndIf 
  If Len(a$)=1
    a$="0"+a$
  EndIf
ProcedureReturn LCase(a$)
EndProcedure

Procedure m_sethex32(*bin, *hash)
  Protected a$=PeekS(*hash), i
  ;************************************************************************
  ;Convert HEX string in BIG indian format to bytes in LITTLE indian format
  ;************************************************************************
  a$ = RSet(m_cutHex(a$),64,"0")
  If Len(a$)<>64
    Debug "Invalid argument length for m_sethex32"
    End
  EndIf
  m_deserializeX64(*bin,0,@a$,8)  
EndProcedure

Procedure.s m_gethex32(*bin)  
  Protected *sertemp=AllocateMemory(64, #PB_Memory_NoClear)
  Protected res$  
  ;************************************************************************
  ;Convert bytes in LITTLE indian format to HEX string in BIG indian format
  ;************************************************************************ 
  m_serializeX64(*bin,0,*sertemp,8)  
  res$=PeekS(*sertemp,64, #PB_Ascii)
  FreeMemory(*sertemp)
ProcedureReturn res$
EndProcedure

Procedure m_check_less_more_equilX64(*s,*t);len *8 byte 0 - s = t, 1- s < t, 2- s > t
  !mov rsi,[p.p_s]  
  !mov rdi,[p.p_t]
  
  !mov rax,24  
  !add rsi,rax
  !add rdi,rax
  
  !xor cx,cx
  !llm_check_less_continue:
  
  !mov rax,[rsi]
  !mov rbx,[rdi]
  !sub rsi,8
  !sub rdi,8 
  !cmp rax,rbx
  !jb llm_check_less_exit_less
  !ja llm_check_less_exit_more  
  !inc cx 
  !cmp cx,4
  !jb llm_check_less_continue
  
  !xor rax,rax
  !jmp llm_check_less_exit  
  
  !llm_check_less_exit_more:
  !mov rax,2
  !jmp llm_check_less_exit  
  
  !llm_check_less_exit_less:
  !mov rax,1
  !llm_check_less_exit:
ProcedureReturn  
EndProcedure

Procedure m_check_equilX64(*s,*t)
  !mov rsi,[p.p_s]  
  !mov rdi,[p.p_t]  
  
  !xor cx,cx
  !llm_check_equil_continue:
  
  !mov rax,[rsi]
  !mov rbx,[rdi]
  !add rsi,8
  !add rdi,8 
  !cmp rax,rbx
  !jne llm_check_equil_exit_noteqil
  !inc cx 
  !cmp cx,4
  !jb llm_check_equil_continue
  
  !mov rax,1
  !jmp llm_check_equil_exit  
  
  !llm_check_equil_exit_noteqil:
  !mov rax,0
  !llm_check_equil_exit:
ProcedureReturn  
EndProcedure

Procedure m_check_nonzeroX64(*a)
  !mov rdx, [p.p_a]
  !mov rax, [rdx]
  !mov rcx,1
  !cmp rax,0
  !jne llcheck_nonzeroIfNoExit
  !mov rax, [rdx+8]
  !cmp rax,0
  !jne llcheck_nonzeroIfNoExit
  !mov rax, [rdx+16]
  !cmp rax,0
  !jne llcheck_nonzeroIfNoExit
  !mov rax, [rdx+24]
  !cmp rax,0
  !jne llcheck_nonzeroIfNoExit
  !xor rcx,rcx
  !llcheck_nonzeroIfNoExit:
  !mov rax,rcx
ProcedureReturn  
EndProcedure

Procedure m_IsInfinityX64(*a)
  !mov rdx, [p.p_a]
  !mov rax, [rdx]
  !xor rcx,rcx
  !cmp rax,0xffffffffffffffff
  !jne ll_IfNoExit
  !mov rax, [rdx+8]
  !cmp rax,0xffffffffffffffff
  !jne ll_IfNoExit
  !mov rax, [rdx+16]
  !cmp rax,0xffffffffffffffff
  !jne ll_IfNoExit
  !mov rax, [rdx+24]
  !cmp rax,0xffffffffffffffff
  !jne ll_IfNoExit  
  !or ecx,1
  !ll_IfNoExit:
  !mov rax,rcx
ProcedureReturn
EndProcedure

Procedure m_Ecc_EvenX64(*a)
  !mov rsi,[p.p_a]
  !xor rax,rax
  !mov al,[rsi]
  !and al,1
  !xor al,1
  ProcedureReturn
EndProcedure

Procedure m_Ecc_TestBitX64(*a, testbit)
  !mov rsi,[p.p_a]
  !xor rcx, rcx
  !xor ebx, ebx
  !xor rdx, rdx
  !mov dl,[p.v_testbit]
  !mov cl,dl
  !shr cl,6;//64  
  !mov bl,cl  
  !shl cl,3  
  !add rsi,rcx
  !shl bl,6
  !sub dl,bl
  !mov rax,[rsi]
  !bt  rax,rdx
  !mov rax,0
  !adc rax,rax
  ProcedureReturn
EndProcedure

Procedure m_Ecc_AndX64(*c, *a, *b)
  !mov rsi,[p.p_a]
  !mov rdi,[p.p_b]
  !mov rbx,[p.p_c]
  
  !mov rax,[rsi]
  !and rax,[rdi]
  !mov [rbx],rax
  
  !mov rax,[rsi+8]
  !and rax,[rdi+8]
  !mov [rbx+8],rax
  
  !mov rax,[rsi+16]
  !and rax,[rdi+16]
  !mov [rbx+16],rax
  
  !mov rax,[rsi+24]
  !and rax,[rdi+24]
  !mov [rbx+24],rax  
EndProcedure

Procedure m_Ecc_OrX64(*c, *a, *b)
  !mov rsi,[p.p_a]
  !mov rdi,[p.p_b]
  !mov rbx,[p.p_c]
  
  !mov rax,[rsi]
  !or rax,[rdi]
  !mov [rbx],rax
  
  !mov rax,[rsi+8]
  !or rax,[rdi+8]
  !mov [rbx+8],rax
  
  !mov rax,[rsi+16]
  !or rax,[rdi+16]
  !mov [rbx+16],rax
  
  !mov rax,[rsi+24]
  !or rax,[rdi+24]
  !mov [rbx+24],rax  
EndProcedure

Procedure m_Ecc_ClearMX64(*a)
  !mov rsi,[p.p_a]
  !xor rax,rax
  !mov [rsi],rax
  !mov [rsi+8],rax
  !mov [rsi+16],rax
  !mov [rsi+24],rax
EndProcedure

Procedure m_shrX64(*a)
  !mov rdi,[p.p_a]
  !clc
  !mov rax,[rdi+24]
  !RCR rax,1
  !mov [rdi+24],rax
    
  !mov rax,[rdi+16]
  !RCR rax,1
  !mov [rdi+16],rax
  
  !mov rax,[rdi+8]
  !RCR rax,1
  !mov [rdi+8],rax
  
  !mov rax,[rdi]
  !RCR rax,1
  !mov [rdi],rax    
EndProcedure

Procedure m_shlX64(*a)
  !mov rdi,[p.p_a]
  !clc
  !mov rax,[rdi]
  !RCL rax,1
  !mov [rdi],rax
    
  !mov rax,[rdi+8]
  !RCL rax,1
  !mov [rdi+8],rax
  
  !mov rax,[rdi+16]
  !RCL rax,1
  !mov [rdi+16],rax
  
  !mov rax,[rdi+24]
  !RCL rax,1
  !mov [rdi+24],rax    
EndProcedure

Procedure m_NegX64(*c,*a)
!mov rsi,[p.p_a] 
!mov rdi,[p.p_c]

!mov rax,0
!sub rax, [rsi]
!mov [rdi],rax

!mov rax,0
!sbb rax, [rsi+8]
!mov [rdi+8],rax

!mov rax,0
!sbb rax, [rsi+16]
!mov [rdi+16],rax

!mov rax,0
!sbb rax, [rsi+24]
!mov [rdi+24],rax
EndProcedure


Procedure m_NegModX64(*c,*a,*p)
!mov rsi,[p.p_a] 
!mov rdi,[p.p_c]
!mov rdx,[p.p_p]

!mov rax,0
!mov rbx,[rsi]
!sub rax,rbx 
;store borrow
!mov rcx,0
!adc rcx,rcx

!add rax,[rdx];c0 + p0
;store carry
!mov r8,0
!adc r8,r8
!mov [rdi],rax


!mov rax,0
!mov rbx,[rsi+8]
;add borrow
!sub rax,rcx

;store borrow
!mov rcx,0
!adc rcx,rcx

!sub rax,rbx 



;add carry
!add rax,r8
!add rax,[rdx+8];c1 + p1
;store carry
!mov r8,0
!adc r8,r8
!mov [rdi+8],rax


!mov rax,0
!mov rbx,[rsi+16]
;add borrow
!sub rax,rcx

;store borrow
!mov rcx,0
!adc rcx,rcx

!sub rax,rbx 



;add carry
!add rax,r8
!add rax,[rdx+16];c2 + p2
;store carry
!mov r8,0
!adc r8,r8
!mov [rdi+16],rax


!mov rax,0
!mov rbx,[rsi+24]
;add borrow
!sub rax,rcx
!sub rax,rbx 

;add carry
!add rax,r8
!add rax,[rdx+24];c3 + p3

!mov [rdi+24],rax

EndProcedure

Procedure m_NegModX64Old(*c,*a,*p)
!mov rcx,[p.p_a] 
!mov rdi,[p.p_c]

!mov rax,0
!sub rax, [rcx]
!mov [rdi],rax

!mov rax,0
!sbb rax, [rcx+8]
!mov [rdi+8],rax

!mov rax,0
!sbb rax, [rcx+16]
!mov [rdi+16],rax

!mov rax,0
!sbb rax, [rcx+24]
!mov [rdi+24],rax

;add P
!mov rdx,[p.p_p]

!mov rax, [rdi]
!add rax, [rdx]
!mov [rdi],rax

!mov rax, [rdi+8]
!adc rax, [rdx+8]
!mov [rdi+8],rax

!mov rax, [rdi+16]
!adc rax, [rdx+16]
!mov [rdi+16],rax

!mov rax, [rdi+24]
!adc rax, [rdx+24]
!mov [rdi+24],rax

EndProcedure

Procedure m_addX64(*c,*a,*b)
!mov rcx,[p.p_a] 
!mov rdx,[p.p_b]
!mov rdi,[p.p_c]

!mov rax, [rcx]
!add rax, [rdx]
!mov [rdi],rax

!mov rax, [rcx+8]
!adc rax, [rdx+8]
!mov [rdi+8],rax

!mov rax, [rcx+16]
!adc rax, [rdx+16]
!mov [rdi+16],rax

!mov rax, [rcx+24]
!adc rax, [rdx+24]
!mov [rdi+24],rax

!mov rax,0
!adc rax, rax
ProcedureReturn
EndProcedure

Procedure m_subX64(*c,*a,*b)
!mov rcx,[p.p_a] 
!mov rdx,[p.p_b]
!mov rdi,[p.p_c]

!mov rax, [rcx]
!sub rax, [rdx]
!mov [rdi],rax

!mov rax, [rcx+8]
!sbb rax, [rdx+8]
!mov [rdi+8],rax

!mov rax, [rcx+16]
!sbb rax, [rdx+16]
!mov [rdi+16],rax

!mov rax, [rcx+24]
!sbb rax, [rdx+24]
!mov [rdi+24],rax

!mov rax,0
!sbb rax, rax
!and rax,0x01
ProcedureReturn
EndProcedure

Procedure m_subModX64(*c,*a,*b,*p)
  ;Protected borrow  
  ;borrow = m_subX64(*c,*a,*b)
  ;If borrow
   ; m_addX64(*c,*c,*p)
 ; EndIf  
  
  !mov rcx,[p.p_a] 
  !mov rdx,[p.p_b]
  !mov rdi,[p.p_c]
  
  !mov rax, [rcx]
  !sub rax, [rdx]
  !mov [rdi],rax
  
  !mov rax, [rcx+8]
  !sbb rax, [rdx+8]
  !mov [rdi+8],rax
  
  !mov rax, [rcx+16]
  !sbb rax, [rdx+16]
  !mov [rdi+16],rax
  
  !mov rax, [rcx+24]
  !sbb rax, [rdx+24]
  !mov [rdi+24],rax
  
  !jnc ll_subModExit
  ;if borrow m_addX64(*c,*c,*p)  
  
  !mov rdx,[p.p_p]; rdi>*c rdx>*p
  
  !mov rax, [rdi]
  !add rax, [rdx]
  !mov [rdi],rax
  
  !mov rax, [rdi+8]
  !adc rax, [rdx+8]
  !mov [rdi+8],rax
  
  !mov rax, [rdi+16]
  !adc rax, [rdx+16]
  !mov [rdi+16],rax
  
  !mov rax, [rdi+24]
  !adc rax, [rdx+24]
  !mov [rdi+24],rax

  
  !ll_subModExit:
EndProcedure

Procedure m_addModX64(*c,*a,*b,*p)
  ;Protected carry, gt
  ;carry = m_addX64(*c,*a,*b)
  ;gt = m_check_less_more_equilX64(*c,*p)>>1
  ;If(carry Or gt)
    ;m_subX64(*c,*c,*p)
  ;EndIf
  
  !mov rcx,[p.p_a] 
  !mov rdx,[p.p_b]
  !mov rsi,[p.p_c]
  
  !mov rax, [rcx]
  !add rax, [rdx]
  !mov [rsi],rax
  
  !mov rax, [rcx+8]
  !adc rax, [rdx+8]
  !mov [rsi+8],rax
  
  !mov rax, [rcx+16]
  !adc rax, [rdx+16]
  !mov [rsi+16],rax
  
  !mov rax, [rcx+24]
  !adc rax, [rdx+24]
  !mov [rsi+24],rax
  
  !jc ll_addMod_needsub ;if carry m_subX64(*c,*c,*p)
  
  ;check gt
  !mov rdi,[p.p_p]; rsi>*c rdi>*p
  
  !mov rax,24  
  !add rsi,rax
  !add rdi,rax
  
  !xor cx,cx
  !ll_addMod_check_less_continue:
  
  !mov rax,[rsi]
  !mov rbx,[rdi]
  !sub rsi,8
  !sub rdi,8 
  !cmp rax,rbx
  !jb ll_addMod_check_less_exit_less
  !ja ll_addMod_check_less_exit_more  
  !inc cx 
  !cmp cx,4
  !jb ll_addMod_check_less_continue
  
  !xor rax,rax
  !jmp ll_addMod_check_less_exit  
  
  !ll_addMod_check_less_exit_more:
  !mov rax,1
  !jmp ll_addMod_check_less_exit  
  
  !ll_addMod_check_less_exit_less:
  !xor rax,rax
  !ll_addMod_check_less_exit:
  
  !cmp rax,1
  !jne ll_addMod_Exit ;if not gt>exit
  ;else m_subX64(*c,*c,*p)
  !ll_addMod_needsub:
  
  
   
  !mov rdx,[p.p_p]
  !mov rdi,[p.p_c]
  !mov rcx, rdi
  
  !mov rax, [rcx]
  !sub rax, [rdx]
  !mov [rdi],rax
  
  !mov rax, [rcx+8]
  !sbb rax, [rdx+8]
  !mov [rdi+8],rax
  
  !mov rax, [rcx+16]
  !sbb rax, [rdx+16]
  !mov [rdi+16],rax
  
  !mov rax, [rcx+24]
  !sbb rax, [rdx+24]
  !mov [rdi+24],rax
  
  
  !ll_addMod_Exit:
EndProcedure

Procedure m_mulModX64(*res,*a,*b,*p, *r512)
  Protected *tt, overflow, borrow
  ;NOTE!!! *r512 buffer 64+40=104bytes!!!
  ;m_Ecc_ClearMX64(*r512+32)
  *tt=*r512+64
    
  !mov rbx,[p.p_tt]
  !mov rdi, [p.p_a]  
  !mov rsi,[p.p_r512]
 
  
  
  ;imm_umul A*B0
  !mov r8, [p.p_b];b
  !mov rax,[r8+0];b0
  !mov r8,rax;b0
  
  !mov rax,[rdi+0];a0
  !mul r8; a0*b0>r512[0]
  !mov [rsi+0], rax;r512[0]
  !mov rcx,rdx;carry
  
  !mov rax,[rdi+8];a1
  !mul r8; a1*b0>r512[1]
  !add rax, rcx
  ;cf=0
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rsi+8], rax;r512[1]
  !mov rcx,rdx;carry
  
  !mov rax,[rdi+16];a2
  !mul r8; a2*b0>r512[2]
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rsi+16], rax;r512[2]
  !mov rcx,rdx;carry  
  
  !mov rax,[rdi+24];a3
  !mul r8; a3*b0>r512[3]
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rsi+24], rax;r512[3]
  !mov rcx,rdx;carry
  
  !mov rax,0
  ;add cf
  !add rax,r9
  !adc rax, rcx  
  !mov [rsi+32], rax;r512[4]
  ;end imm_umul
  
  
  ;imm_umul A*B1
  !mov r8, [p.p_b];b
  !mov rax,[r8+8];b1
  !mov r8,rax;b1
  
  !mov rax,[rdi+0];a0
  !mul r8; a0*b1>t[0]
  !mov [rbx+0], rax;t[0]
  !mov rcx,rdx;carry
  
  !mov rax,[rdi+8];a1
  !mul r8; a1*b1>t[1]
  !add rax, rcx
  ;cf=0
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+8], rax;t[1]
  !mov rcx,rdx;carry
  
  !mov rax,[rdi+16];a2
  !mul r8; a2*b1>t[2]
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+16], rax;t[2]
  !mov rcx,rdx;carry  
  
  !mov rax,[rdi+24];a3
  !mul r8; a3*b1>t[3]
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+24], rax;t[3]
  !mov rcx,rdx;carry
  
  !mov rax,0
  ;add cf
  !add rax,r9
  !adc rax, rcx  
  !mov [rbx+32], rax;t[4]
  ;end imm_umul
  
  !mov rax,[rsi+8];r512[1]
  !add rax,[rbx+0]; r512[1]+t[0]>r512[1]
  !mov [rsi+8],rax
  
  !mov rax,[rsi+16];r512[2]
  !adc rax,[rbx+8]; r512[2]+t[1]>r512[2]
  !mov [rsi+16],rax
  
  !mov rax,[rsi+24];r512[3]
  !adc rax,[rbx+16]; r512[3]+t[2]>r512[3]
  !mov [rsi+24],rax
  
  !mov rax,[rsi+32];r512[4]
  !adc rax,[rbx+24]; r512[4]+t[3]>r512[4]
  !mov [rsi+32],rax
  
  ;!mov rax,[rsi+40];r512[5]
  !mov rax,0
  !adc rax,[rbx+32]; r512[5]+t[4]>r512[5]
  !mov [rsi+40],rax
  
  ;imm_umul A*B2
  !mov r8, [p.p_b];b
  !mov rax,[r8+16];b2
  !mov r8,rax;b2
  
  !mov rax,[rdi+0];a0
  !mul r8; a0*b2>t[0]
  !mov [rbx+0], rax;t[0]
  !mov rcx,rdx;carry
  
  !mov rax,[rdi+8];a1
  !mul r8; a1*b2>t[1]
  !add rax, rcx
  ;cf=0
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+8], rax;t[1]
  !mov rcx,rdx;carry
  
  !mov rax,[rdi+16];a2
  !mul r8; a2*b2>t[2]
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+16], rax;t[2]
  !mov rcx,rdx;carry  
  
  !mov rax,[rdi+24];a3
  !mul r8; a3*b2>t[3]
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+24], rax;t[3]
  !mov rcx,rdx;carry
  
  !mov rax,0
  ;add cf
  !add rax,r9
  !adc rax, rcx  
  !mov [rbx+32], rax;t[4]
  ;end imm_umul
  
  
  !mov rax,[rsi+16];r512[2]
  !add rax,[rbx+0]; r512[2]+t[0]>r512[2]
  !mov [rsi+16],rax
  
  !mov rax,[rsi+24];r512[3]
  !adc rax,[rbx+8]; r512[3]+t[1]>r512[3]
  !mov [rsi+24],rax
  
  !mov rax,[rsi+32];r512[4]
  !adc rax,[rbx+16]; r512[4]+t[2]>r512[4]
  !mov [rsi+32],rax
  
  !mov rax,[rsi+40];r512[5]
  !adc rax,[rbx+24]; r512[5]+t[3]>r512[5]
  !mov [rsi+40],rax
  
  ;!mov rax,[rsi+48];r512[6]
  !mov rax,0
  !adc rax,[rbx+32]; r512[6]+t[4]>r512[6]
  !mov [rsi+48],rax
  
  
  ;imm_umul A*B3
  !mov r8, [p.p_b];b
  !mov rax,[r8+24];b3
  !mov r8,rax;b3
  
  !mov rax,[rdi+0];a0
  !mul r8; a0*b3>t[0]
  !mov [rbx+0], rax;t[0]
  !mov rcx,rdx;carry
  
  !mov rax,[rdi+8];a1
  !mul r8; a1*b3>t[1]
  !add rax, rcx
  ;cf=0
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+8], rax;t[1]
  !mov rcx,rdx;carry
  
  !mov rax,[rdi+16];a2
  !mul r8; a2*b3>t[2]
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+16], rax;t[2]
  !mov rcx,rdx;carry  
  
  !mov rax,[rdi+24];a3
  !mul r8; a3*b3>t[3]
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+24], rax;t[3]
  !mov rcx,rdx;carry
  
  !mov rax,0
  ;add cf
  !add rax,r9
  !adc rax, rcx  
  !mov [rbx+32], rax;t[4]
  ;end imm_umul
  
  
  !mov rax,[rsi+24];r512[3]
  !add rax,[rbx+0]; r512[3]+t[0]>r512[3]
  !mov [rsi+24],rax
  
  !mov rax,[rsi+32];r512[4]
  !adc rax,[rbx+8]; r512[4]+t[1]>r512[4]
  !mov [rsi+32],rax
  
  !mov rax,[rsi+40];r512[5]
  !adc rax,[rbx+16]; r512[5]+t[2]>r512[5]
  !mov [rsi+40],rax
  
  !mov rax,[rsi+48];r512[6]
  !adc rax,[rbx+24]; r512[6]+t[3]>r512[6]
  !mov [rsi+48],rax
  
  ;!mov rax,[rsi+56];r512[7]
  !mov rax,0
  !adc rax,[rbx+32]; r512[7]+t[4]>r512[7]
  !mov [rsi+56],rax
  
  ; At this point we have 16 32-bit words representing a 512-bit value
  ; high[0 ... 7] And c[0 ... 7]  
 
  ;Debug "high:"+m_gethex32(*r512+32)
  ;Debug "low :"+m_gethex32(*r512)
  ;End
  ;****************************************************************
  ;   At this poin result correct!!!
  ; high: 225989dbbc349b6f319ca3eed777a46f55b1dc22e97af11261167d213c1f060d
  ; low : 29520a21508989b06ed1194129efb1517cee385a708abe44718bc509775ad540
  ;
  ;result:
  ;high:225989dbbc349b6f319ca3eed777a46f55b1dc22e97af11261167d213c1f060d
  ;low :29520a21508989b06ed1194129efb1517cee385a708abe44718bc509775ad540
  ;correct!!!
  ;****************************************************************

  ;Store high[6] And high[7] since they will be overwritten
  ;**Reduce from 512 to 320
  !mov r8, 0x1000003D1
  
  ;imm_umul
  
  !mov rax,[rsi+32]
  !mul r8; *1000003D1h>t[0]
  !mov [rbx+0], rax;
  !mov rcx,rdx;carry
  
  !mov rax,[rsi+40]
  !mul r8; *1000003D1h  
  !add rax, rcx
  ;cf=0
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+8], rax;t[1]
  !mov rcx,rdx;carry
  
  !mov rax,[rsi+48]
  !mul r8; *1000003D1h
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+16], rax;t[2]
  !mov rcx,rdx;carry  
  
  !mov rax,[rsi+56]
  !mul r8; *1000003D1h
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+24], rax;t[3]
  !mov rcx,rdx;carry
  
  !mov rax,0
  ;add cf
  !add rax,r9
  !adc rax, rcx  
  !mov [rbx+32], rax;t[4]
  ;end imm_umul
  
  
  !mov rax,[rsi+0];r512[0]
  !add rax, [rbx+0];512[0]+t[0]>512[0]
  !mov [rsi+0],rax;r512[0]
  
  !mov rax,[rsi+8];r512[1]
  !adc rax,[rbx+8];512[1]+t[1]>512[1]
  !mov [rsi+8],rax;r512[1]
  
  !mov rax,[rsi+16];r512[2]
  !adc rax,[rbx+16];512[2]+t[2]>512[2]
  !mov [rsi+16],rax;r512[2]
  
  !mov rax,[rsi+24];r512[3]
  !adc rax,[rbx+24];512[3]+t[2]>512[3]
  !mov [rsi+24],rax;r512[3]
  
  
  ;**Reduce from 320 to 256
  ;No overflow possible here t[4]+c<=0x1000003D1ULL  
  !mov rax,[rbx+32];t[4]+carry
  !adc rax,0
  !mul r8; (t[4]+carry)*1000003D1h>rax>u10 rdx>u11
  
  !add rax,[rsi+0];u10+512[0]>512[0]
  !mov [rsi+0],rax;r512[0]
  
  !adc rdx,[rsi+8];u11+512[1]>512[1]
  !mov [rsi+8],rdx;r512[1]
  
  !mov rax,0
  !adc rax,[rsi+16];0+512[2]>512[2]
  !mov [rsi+16],rax;r512[2]
  
  !mov rax,0
  !adc rax,[rsi+24];0+512[3]>512[3]
  !mov [rsi+24],rax;r512[3]
  
  !mov rax,0
  !adc rax,rax
  !mov [p.v_overflow],rax
  
  
    
  borrow = m_subX64(*r512,*r512,*p)  
  If overflow
    If Not borrow      
       m_subX64(*r512,*r512,*p) 
    EndIf
  Else
    If borrow      
      m_addX64(*r512,*r512,*p) 
    EndIf  
  EndIf  
  

  move32b_(p.p_r512,p.p_res,0,0)
EndProcedure

Procedure m_mulModX64v2(*res,*a,*b,*p, *high)
  Protected *c, overflow, borrow
  
  ;m_Ecc_ClearMX64(*high)
  *c=*high
  *high=*c+32
  
  !mov rdi,[p.p_b]  
  !mov rsi,[p.p_c]
  !mov r9,[p.p_a]  
  !mov r8, [p.p_high]
  
 
  ;a[7] * b (low) 
  !mov r10, [r9];a7
  mul4blo(0)  
  mul4blo(8)
  mul4blo(16)
  mul4blo(24)  
  
  ;a[7] * b (high)  
  
  madhi(0, 8)
  madhicc(8, 16)
  madhicc(16, 24)   
  madhiccHIGHZero(24, 0) ;b0;high7
    
  ;a[6] * b (low)  
  !mov r10, [r9+8];a6
  madlo(0, 8) 
  madlocc(8, 16)
  madlocc(16, 24)    
  madloccHIGH(24, 0)  ;b0;high7
  ;add carry  
  !mov [r8+8], rcx ;HIGH6
  
  ;a[6] * b (high)   
  madhi(0, 16)
  madhicc(8, 24)    
  madhiccHIGH(16, 0) ;b1;high7
  madhiccHIGH(24, 8) ;b0;high6
  
  ;a[5] * b (low)  
  !mov r10, [r9+16];a5
  madlo(0, 16) 
  madlocc(8, 24)   
  madloccHIGH(16, 0)  ;b1;high7;a5
  madloccHIGH(24, 8)  ;b0;high6;a5
  ;add carry  
  !mov [r8+16], rcx ;HIGH5
  
  ;a[5] * b (high) 
  madhi(0, 24)  
  madhiccHIGH(8, 0) ;b2;high7;a5
  madhiccHIGH(16, 8) ;b1;high6;a5
  madhiccHIGH(24, 16) ;b0;high5;a5
  
  ;a[4] * b (low) 
  !mov r10, [r9+24];a4
  madlo(0, 24)     
  madloccHIGH(8, 0)  ;b2;high7;a4
  madloccHIGH(16, 8)  ;b1;high6;a4
  madloccHIGH(24, 16)  ;b0;high5;a4
  ;add carry 
  !mov [r8+24], rcx ;HIGH4
  

 
  !xor ecx,ecx ;clear carry!
  madhiccHIGH(0, 0) ;b7;high7;a0
  madhiccHIGH(8, 8) ;b6;high6;a0
  madhiccHIGH(16, 16) ;b5;high5;a0
  madhiccHIGH(24, 24) ;b4;high4;a0 
  
  ; At this point we have 16 32-bit words representing a 512-bit value
	; high[0 ... 7] And c[0 ... 7]
  ;Debug "high:"+m_gethex32(*high)
  ;Debug "low :"+m_gethex32(*c)
  ;End
  ;****************************************************************
  ;   At this poin result correct!!!
  ; high: 225989dbbc349b6f319ca3eed777a46f55b1dc22e97af11261167d213c1f060d
  ; low : 29520a21508989b06ed1194129efb1517cee385a708abe44718bc509775ad540
  ;
  ;result:
  ;high:225989dbbc349b6f319ca3eed777a46f55b1dc22e97af11261167d213c1f060d
  ;low :29520a21508989b06ed1194129efb1517cee385a708abe44718bc509775ad540
  ;correct!!!
  ;****************************************************************

  ;Store high[6] And high[7] since they will be overwritten
  ;**Reduce from 512 to 320
  
  !mov rbx, rsi
  !add rbx,64
  !mov r8, 0x1000003D1
  
  ;imm_umul
  
  !mov rax,[rsi+32]
  !mul r8; *1000003D1h>t[0]
  !mov [rbx+0], rax;
  !mov rcx,rdx;carry
  
  !mov rax,[rsi+40]
  !mul r8; *1000003D1h  
  !add rax, rcx
  ;cf=0
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+8], rax;t[1]
  !mov rcx,rdx;carry
  
  !mov rax,[rsi+48]
  !mul r8; *1000003D1h
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+16], rax;t[2]
  !mov rcx,rdx;carry  
  
  !mov rax,[rsi+56]
  !mul r8; *1000003D1h
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+24], rax;t[3]
  !mov rcx,rdx;carry
  
  !mov rax,0
  ;add cf
  !add rax,r9
  !adc rax, rcx  
  !mov [rbx+32], rax;t[4]
  ;end imm_umul
  
  
  !mov rax,[rsi+0];r512[0]
  !add rax, [rbx+0];512[0]+t[0]>512[0]
  !mov [rsi+0],rax;r512[0]
  
  !mov rax,[rsi+8];r512[1]
  !adc rax,[rbx+8];512[1]+t[1]>512[1]
  !mov [rsi+8],rax;r512[1]
  
  !mov rax,[rsi+16];r512[2]
  !adc rax,[rbx+16];512[2]+t[2]>512[2]
  !mov [rsi+16],rax;r512[2]
  
  !mov rax,[rsi+24];r512[3]
  !adc rax,[rbx+24];512[3]+t[2]>512[3]
  !mov [rsi+24],rax;r512[3]
  
  
  ;**Reduce from 320 to 256
  ;No overflow possible here t[4]+c<=0x1000003D1ULL  
  !mov rax,[rbx+32];t[4]+carry
  !adc rax,0
  !mul r8; (t[4]+carry)*1000003D1h>rax>u10 rdx>u11
  
  !add rax,[rsi+0];u10+512[0]>512[0]
  !mov [rsi+0],rax;r512[0]
  
  !adc rdx,[rsi+8];u11+512[1]>512[1]
  !mov [rsi+8],rdx;r512[1]
  
  !mov rax,0
  !adc rax,[rsi+16];0+512[2]>512[2]
  !mov [rsi+16],rax;r512[2]
  
  !mov rax,0
  !adc rax,[rsi+24];0+512[3]>512[3]
  !mov [rsi+24],rax;r512[3]
  
  !mov rax,0
  !adc rax,rax
  !mov [p.v_overflow],rax
  
  
    
  borrow = m_subX64(*c,*c,*p)  
  If overflow
    If Not borrow      
       m_subX64(*c,*c,*p) 
    EndIf
  Else
    If borrow      
      m_addX64(*c,*c,*p) 
    EndIf  
  EndIf  
  

  move32b_(p.p_c,p.p_res,0,0)
EndProcedure

Procedure m_mulModX64v1(*res,*a,*b,*p, *high)
  Protected *c, overflow, borrow
  
  ;m_Ecc_ClearMX64(*high)
  *c=*high+32
  
  !mov rdi,[p.p_b]  
  !mov rsi,[p.p_c]
  !mov r9,[p.p_a]  
  !mov r8, [p.p_high]
  
 
  ;a[7] * b (low) 
  !mov r10, [r9];a7
  mul4blo(0)  
  mul4blo(8)
  mul4blo(16)
  mul4blo(24)  
  
  ;a[7] * b (high)  
  
  madhi(0, 8)
  madhicc(8, 16)
  madhicc(16, 24)   
  madhiccHIGHZero(24, 0) ;b0;high7
    
  ;a[6] * b (low)  
  !mov r10, [r9+8];a6
  madlo(0, 8) 
  madlocc(8, 16)
  madlocc(16, 24)    
  madloccHIGH(24, 0)  ;b0;high7
  ;add carry  
  !mov [r8+8], rcx ;HIGH6
  
  ;a[6] * b (high)   
  madhi(0, 16)
  madhicc(8, 24)    
  madhiccHIGH(16, 0) ;b1;high7
  madhiccHIGH(24, 8) ;b0;high6
  
  ;a[5] * b (low)  
  !mov r10, [r9+16];a5
  madlo(0, 16) 
  madlocc(8, 24)   
  madloccHIGH(16, 0)  ;b1;high7;a5
  madloccHIGH(24, 8)  ;b0;high6;a5
  ;add carry  
  !mov [r8+16], rcx ;HIGH5
  
  ;a[5] * b (high) 
  madhi(0, 24)  
  madhiccHIGH(8, 0) ;b2;high7;a5
  madhiccHIGH(16, 8) ;b1;high6;a5
  madhiccHIGH(24, 16) ;b0;high5;a5
  
  ;a[4] * b (low) 
  !mov r10, [r9+24];a4
  madlo(0, 24)     
  madloccHIGH(8, 0)  ;b2;high7;a4
  madloccHIGH(16, 8)  ;b1;high6;a4
  madloccHIGH(24, 16)  ;b0;high5;a4
  ;add carry 
  !mov [r8+24], rcx ;HIGH4
  

 
  !xor ecx,ecx ;clear carry!
  madhiccHIGH(0, 0) ;b7;high7;a0
  madhiccHIGH(8, 8) ;b6;high6;a0
  madhiccHIGH(16, 16) ;b5;high5;a0
  madhiccHIGH(24, 24) ;b4;high4;a0 
  
  ; At this point we have 16 32-bit words representing a 512-bit value
	; high[0 ... 7] And c[0 ... 7]
  ;Debug "high:"+m_gethex32(*high)
  ;Debug "low :"+m_gethex32(*c)
  ;End
  ;****************************************************************
  ;   At this poin result correct!!!
  ; high: 225989dbbc349b6f319ca3eed777a46f55b1dc22e97af11261167d213c1f060d
  ; low : 29520a21508989b06ed1194129efb1517cee385a708abe44718bc509775ad540
  ;
  ;result:
  ;high:225989dbbc349b6f319ca3eed777a46f55b1dc22e97af11261167d213c1f060d
  ;low :29520a21508989b06ed1194129efb1517cee385a708abe44718bc509775ad540
  ;correct!!!
  ;****************************************************************

  ;Store high[6] And high[7] since they will be overwritten
  
  !mov rdi,r8
  
  !mov r8d, [rdi+4] ;high6 
  !mov r9d, [rdi+0] ;high7
  
  ;Take high 256 bits, multiply by 2^32, add to low 256 bits
  ;That is, take high[0 ... 7], shift it left 1 word And add it To c[0 ... 7] 
  
  ;rdi- poiner to HIGH, rsi -pointer to C
  !clc
  !xor rcx,rcx
  
  !mov rax,[rsi+4];c6
  !add rax,[rdi+0];high7
  !mov [rsi+4], rax
  
  !mov rax,[rsi+12];c5
  !adc rax,[rdi+8];high6
  !mov [rsi+12], rax
  
  !mov rax,[rsi+20];c4
  !adc rax,[rdi+16];high5
  !mov [rsi+20], rax
  
  !mov eax,[rsi+28];c0
  !adc eax,[rdi+24];high1
  !mov [rsi+28], eax
    
  !mov eax,0
  !adc eax,[rdi+28];high0
  !mov [rdi+0], eax;high7
  
  !mov eax,0
  !adc eax,0
  !mov [rdi+4], eax;high6
  
  ;****************************************************************
  ;shold be e586a59082262d9f4648bdb07fa18d746669296cd1a13b65adaacb16775ad540
  ; high 225989dbbc349b6f319ca3eed777a46f55b1dc22e97af11200000000225989db
  ;****************************************************************
  ;high :225989dbbc349b6f319ca3eed777a46f55b1dc22e97af11200000000225989db
  ;low :e586a59082262d9f4648bdb07fa18d746669296cd1a13b65adaacb16775ad540
  ;success!!!
  
  ;Debug "high :"+m_gethex32(*high)  
  ;Debug "low :"+m_gethex32(*c)
  ;End
  
  ;Take high 256 bits, multiply by 977, add To low 256 bits
  ;That is, take high[0 ... 5], high6, high7, multiply by 977 And add To c[0 ... 7]
    
  !mov ebx, r9d ;hightemp7
  !mov eax,977
  !mul ebx 
  !add eax,[rsi+0];c7
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+0], eax
  
  !mov ebx, r8d
  !mov eax,977
  !mul ebx 
  ;add carry
  !add eax,ecx
  !add eax,[rsi+4];c6
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+4], eax;c6
  
   ;madlocc
  !mov ebx, [rdi+8] ;high5
  !mov eax,977
  !mul ebx 
  ;add carry
  !add eax,ecx
  !add eax,[rsi+8] 
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+8], eax;c5
  
  !mov ebx, [rdi+12] ;high4
  !mov eax,977
  !mul ebx 
  ;add carry
  !add eax,ecx
  !add eax,[rsi+12] 
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+12], eax;c4
  
  !mov ebx, [rdi+16] ;high3
  !mov eax,977
  !mul ebx 
  ;add carry
  !add eax,ecx
  !add eax,[rsi+16] 
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+16], eax;c3
  
  !mov ebx, [rdi+20] ;high2
  !mov eax,977
  !mul ebx 
  ;add carry
  !add eax,ecx
  !add eax,[rsi+20] 
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+20], eax;c2
  
  !mov ebx, [rdi+24] ;high1
  !mov eax,977
  !mul ebx 
  ;add carry
  !add eax,ecx
  !add eax,[rsi+24] 
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+24], eax;c1
  
  !mov ebx, [rdi+28] ;high0
  !mov eax,977
  !mul ebx 
  ;add carry
  !add eax,ecx
  !add eax,[rsi+28] 
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+28], eax;c0
  
  ;add carry
  !mov eax, ecx 
  !add eax, [rdi];high7
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rdi], eax ;high7
    
  ;add carry
  !mov eax, ecx 
  !add eax, [rdi+4] ;high6
  !mov [rdi+4], eax ;high6
  
 
  ;****************************************************************
  ;shold be fd3dc25bc6eb603e9d165cfecf3c19137232472edfd34118347e5607e9c0ecdd
  ; high 225989dbbc349b6f319ca3eed777a46f55b1dc22e97af11200000000225989db
  ;****************************************************************
  ; high 225989dbbc349b6f319ca3eed777a46f55b1dc22e97af11200000000225989db
  ; fd3dc25bc6eb603e9d165cfecf3c19137232472edfd34118347e5607e9c0ecdd
  ;success!!!
  ;Debug "high :"+m_gethex32(*high)
  
  ;Debug "low :"+m_gethex32(*c)
  ;End
  !mov ebx, r9d ;hightemp7
  !mov eax,977
  !mul ebx 
  !add edx,[rsi+4] ;c6
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+4], edx ;c6  
  
  !mov ebx, r8d
  !mov eax,977
  !mul ebx 
  ;add carry
  !add edx,ecx
  !add edx,[rsi+8] ;c5
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+8], edx ;c5
  
  ;madhighcc
  !mov ebx, [rdi+8]; high5
  !mov eax,977
  !mul ebx;edx:eax 
  ;add carry
  !add edx,ecx
  !add edx,[rsi+12] 
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+12], edx;c4
  
  !mov ebx, [rdi+12]; high4
  !mov eax,977
  !mul ebx;edx:eax 
  ;add carry
  !add edx,ecx
  !add edx,[rsi+16] 
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+16], edx;c3
  
  !mov ebx, [rdi+16]; high3
  !mov eax,977
  !mul ebx;edx:eax 
  ;add carry
  !add edx,ecx
  !add edx,[rsi+20] 
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+20], edx;c2
  
  !mov ebx, [rdi+20]; high2
  !mov eax,977
  !mul ebx;edx:eax 
  ;add carry
  !add edx,ecx
  !add edx,[rsi+24] 
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+24], edx;c1
  
  !mov ebx, [rdi+24]; high1
  !mov eax,977
  !mul ebx;edx:eax 
  ;add carry
  !add edx,ecx
  !add edx,[rsi+28] 
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+28], edx;c0
 
  
  !mov ebx, [rdi+28];high0
  !mov eax,977
  !mul ebx 
  ;add carry
  !add edx,ecx
  !add edx,[rdi+0];high7
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rdi+0], edx;high7
  
  ;add carry
  !mov eax,ecx  
  !add eax, [rdi+4];high6
  !mov [rdi+4], eax;high6
  
  ;****************************************************************
  ;fd3dc529c6eb60fb9d166034cf3c1a5a72324aa9dfd3428a347e56ece9c0ecdd
  ;****************************************************************
  ; success!!!
  ;Debug "low :"+m_gethex32(*c)
  ;End
  ;Repeat the same steps, but this time we only need To handle high[6] And high[7]
  !mov r8d, [rdi+4] ;high6 
  !mov r9d, [rdi+0] ;high7
  
  ;Take the high 64 bits, multiply by 2^32 and add to the low 256 bits
  !mov eax,[rsi+4];c6
  !add eax,[rdi+0];high7
  !mov [rsi+4], eax;c6
  
  !mov eax,[rsi+8];c5
  !adc eax,[rdi+4];high6
  !mov [rsi+8], eax;c5
  
  !mov eax,[rsi+12];c4
  !adc eax,0
  !mov [rsi+12], eax
  
  !mov eax,[rsi+16];c3
  !adc eax,0
  !mov [rsi+16], eax
  
  !mov eax,[rsi+20];c2
  !adc eax,0
  !mov [rsi+20], eax
  
  !mov eax,[rsi+24];c1
  !adc eax,0
  !mov [rsi+24], eax
  
  !mov eax,[rsi+28];c0
  !adc eax,0
  !mov [rsi+28], eax
  
  !mov eax,0
  !adc eax,0
  !mov [rdi+0], eax;high7  
  
  ;****************************************************************
  ;fd3dc529c6eb60fb9d166034cf3c1a5a72324aa9dfd3428a56d7e14ae9c0ecdd
  ;****************************************************************
  ;success!!!
  ;Debug "low :"+m_gethex32(*c)
  ;End
  ;Take the high 64 bits, multiply by 977 and add to the low 256 bits
  !mov ebx, r9d ;hightemp7
  !mov eax,977
  !mul ebx 
  !add eax,[rsi+0];c7
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+0], eax
  
  
  !mov ebx, r8d
  !mov eax,977
  !mul ebx 
  ;add carry
  !add eax,ecx
  !add eax,[rsi+4];c6  
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+4], eax;c6
  
  
  !mov eax,ecx
  !adc eax,[rsi+8];c5
  !mov [rsi+8], eax
  
  !mov eax,[rsi+12];c4
  !adc eax,0
  !mov [rsi+12], eax
  
  !mov eax,[rsi+16];c3
  !adc eax,0
  !mov [rsi+16], eax
  
  !mov eax,[rsi+20];c2
  !adc eax,0
  !mov [rsi+20], eax
  
  !mov eax,[rsi+24];c1
  !adc eax,0
  !mov [rsi+24], eax
  
  !mov eax,[rsi+28];c0
  !adc eax,0
  !mov [rsi+28], eax
  
  !mov eax,[rdi+0];high7
  !adc eax,0
  !mov [rdi+0], eax
  
  ;****************************************************************
  ;fd3dc529c6eb60fb9d166034cf3c1a5a72324aa9dfd3428a56d7e14b0179fd9b
  ;****************************************************************
  ;success!!!
  ;Debug "low :"+m_gethex32(*c)
  ;End
  !mov ebx, r9d ;hightemp7
  !mov eax,977
  !mul ebx 
  !add edx,[rsi+4] ;c6
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+4], edx ;c6

  !mov ebx, r8d
  !mov eax,977
  !mul ebx 
  ;add carry
  !add edx,ecx
  !add edx,[rsi+8] ;c5  
  ;store carry
  !mov ecx,0
  !adc ecx, ecx 
  !mov [rsi+8], edx ;c5
  
  
  
  !mov eax,ecx
  !adc eax,[rsi+12];c4
  !mov [rsi+12], eax
  
  !mov eax,[rsi+16];c3
  !adc eax,0
  !mov [rsi+16], eax
  
  !mov eax,[rsi+20];c2
  !adc eax,0
  !mov [rsi+20], eax
  
  !mov eax,[rsi+24];c1
  !adc eax,0
  !mov [rsi+24], eax
  
  !mov eax,[rsi+28];c0
  !adc eax,0
  !mov [rsi+28], eax
  
  !mov eax,[rdi+0];high7
  !adc eax,0
  !mov [p.v_overflow],eax
  
  
    
  ;low :fd3dc529c6eb60fb9d166034cf3c1a5a72324aa9dfd3428a56d7e1ce0179fd9b
  
  ;Debug "low :"+m_gethex32(*c)
  ;End
  borrow = m_subX64(*c,*c,*p)   
  
  If overflow
    If Not borrow      
       m_subX64(*c,*c,*p) 
    EndIf
  Else
    If borrow      
      m_addX64(*c,*c,*p) 
    EndIf  
  EndIf
  move32b_(p.p_c,p.p_res,0,0)
  ;FreeMemory(*high)
EndProcedure

Procedure m_squareModX64(*res,*a,*p, *r512)
  Protected *tt, overflow, borrow
  
  ;NOTE!!! *r512 buffer 64+40=104bytes!!!
  
  *tt=*r512+64
    
  !mov rbx,[p.p_tt]
  !mov rdi, [p.p_a]  
  !mov rsi,[p.p_r512]
    
  ;**K=0
  !mov rax, [rdi];a0
  !mov r8, rax;store a0
  !mul rax ;a0>>rax>512[0] rdx>t[1]
  !mov [rsi], rax;low>512[0]
  !mov [rbx+8], rdx;high>t[1]
  ;**K=1
  !mov rax, r8;a0
  !mul qword[rdi+8];a1>>rax>t[3] rdx>t[4]  
  !add rax,rax;t[3]+t[3]>t[3]  
  !adc rdx, rdx;t[4]+t[4]>t[4]
  
  ;store carry
  !mov rcx,0
  !adc rcx, rcx ;t1 
  
  !add rax,[rbx+8];t[3]+t[1]>t[3]
  !mov [rbx+24], rax;t[3]
  
  !adc rdx, 0;t[4]+0>t[4]
  !mov [rbx+32], rdx;t[4]
  
  !adc rcx, 0
  !mov r9, rcx;t1  
  !mov [rsi+8], rax;r512[1] = t[3]
  
  
  ;**K=2
  !mov rax, r8;a0
  !mul qword [rdi+16];a2>>rax>t[0] rdx>t[1]
  
  !add rax,rax;t[0]+t[0]>t[0]
  !mov [rbx+0], rax;t[0]
  
  !adc rdx, rdx;t[1]+t[1]>t[1]
  !mov [rbx+8], rdx;t[1]
  
  ;store carry
  !mov rcx,0
  !adc rcx, 0;t2
    
  !mov rax, [rdi+8];a1
  !mul rax ;a1>>rax>u10 rdx>u11
  
  !add rax,[rbx+0];u10+t[0]>t[0] 
  
  !adc rdx, [rbx+8];u11+t[1]>t[1]  
  
  !adc rcx, 0;t2 
  
  
  !add rax,[rbx+32];t[0]+t[4]>t[0]
  !mov [rbx+0], rax;t[0]
  
  !adc rdx, r9;t[1]+t1>t[1]
  !mov [rbx+8], rdx;t[1]
  
  !adc rcx, 0
  !mov r9, rcx;t2  
  !mov [rsi+16], rax;r512[2] = t[0]
  
  ;**K=3
  !mov rax, r8;a0
  !mul qword [rdi+24];a3>>rax>t[3] rdx>t[4]
  !mov [rbx+24], rax;t[3]
  !mov [rbx+32], rdx;t[4]
  
  !mov rax, [rdi+8];a1
  !mov r8,rax;store a1
  !mul qword [rdi+16];a2>>rax>u10 rdx>u11
  
  !add rax,[rbx+24];u10+t[3]>t[3] 
  
  !adc rdx, [rbx+32];u11+t[4]>t[4]
  
  ;store carry
  !mov rcx,0
  !adc rcx, 0;t1
  
  !add rcx,rcx;t1 += t1
  
  !add rax,rax;t[3]+t[3]>t[3]  
  
  !adc rdx,rdx;t[4]+t[4]>t[4]
  
  
  !adc rcx, 0;t1
  
  !add rax,[rbx+8];t[3]+t[1]>t[3]
  !mov [rbx+24], rax;t[3]
  
  !adc rdx,r9;t[4]+t2>t[4]
  !mov [rbx+32], rdx;t[4]
  
  !adc rcx, 0;t1
  !mov r9, rcx;t1  
  !mov [rsi+24], rax;r512[3] = t[3]
  
  ;**K=4
  !mov rax, r8;a1
  !mul qword [rdi+24];a3>>rax>t[0] rdx>t[1]
  !add rax,rax;t[0]+t[0]>t[0] 
  !mov [rbx+0], rax;t[0]
  
  !adc rdx,rdx;t[1]+t[1]>t[1]
  !mov [rbx+8], rdx;t[1]
  
  ;store carry
  !mov rcx,0
  !adc rcx, 0;t2
  
  !mov rax,[rdi+16];a2
  !mov r8,rax;store a2
  !mul rax;a2>>rax>u10 rdx>u11
  !add rax,[rbx+0];u10+t[0]>t[0]  
  
  !adc rdx,[rbx+8];u11+t[1]>t[1]
  
  
  !adc rcx, 0;t2
  
  !add rax,[rbx+32];t[0]+t[4]>t[0] 
  !mov [rbx+0], rax;t[0]
  
  !adc rdx,r9;t[1]+t1>t[1]
  !mov [rbx+8], rdx;t[1]
  
  !adc rcx, 0;t2
  !mov r9, rcx;t2  
  !mov [rsi+32], rax;r512[4] = t[0]
  
  ;**K=5
  !mov rax, r8;a2
  !mul qword [rdi+24];a3>>rax>t[3] rdx>t[4]
  !add rax,rax;t[3]+t[3]>t[3] 
  
  !adc rdx,rdx;t[4]+t[4]>t[4]
  
  ;store carry
  !mov rcx,0
  !adc rcx, 0;t1
  
  !add rax,[rbx+8];t[3]+t[1]>t[3] 
  !mov [rbx+24], rax;t[3]
  
  !adc rdx,r9;t[4]+t2>t[4]
  !mov [rbx+32], rdx;t[4]
  
  !adc rcx, 0;t1
  !mov r9, rcx;t1  
  !mov [rsi+40], rax;r512[5] = t[3]
  
  ;**K=6
  !mov rax,[rdi+24];a3
  !mul rax;a3>>rax>t[0] rdx>t[1]
  
  !add rax,[rbx+32];t[0]+t[4]>t[0] 
  !mov [rbx+0], rax;t[0]
  
  !adc rdx,r9;t[1]+t1>t[1]
  !mov [rbx+8], rdx;t[1]
  
  !mov [rsi+48], rax;r512[6] = t[0]
  
  ;**K=6
  !mov [rsi+56], rdx;r512[7] = t[1]
      
  ;sould be
  ;high 0fee1a06f69e1b8286bf0b2d8858740848984fa8f6a052b12d136d89f930e01e
  ;low  7b1cd51763e164b25e3a46f9d61432f7f003bbabe62013086c3f2ef66fbb2e31
  ;result correct!!!

  
  ;**Reduce from 512 to 320
  !mov r8, 0x1000003D1
  
  ;imm_umul
  
  !mov rax,[rsi+32]
  !mul r8; *1000003D1h>t[0]
  !mov [rbx+0], rax;
  !mov rcx,rdx;carry
  
  !mov rax,[rsi+40]
  !mul r8; *1000003D1h  
  !add rax, rcx
  ;cf=0
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+8], rax;t[1]
  !mov rcx,rdx;carry
  
  !mov rax,[rsi+48]
  !mul r8; *1000003D1h
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+16], rax;t[2]
  !mov rcx,rdx;carry  
  
  !mov rax,[rsi+56]
  !mul r8; *1000003D1h
  ;add cf
  !add rax,r9
  !adc rax, rcx
  ;store cf
  !mov r9,0
  !adc r9,0 
  !mov [rbx+24], rax;t[3]
  !mov rcx,rdx;carry
  
  !mov rax,0
  ;add cf
  !add rax,r9
  !adc rax, rcx  
  !mov [rbx+32], rax;t[4]
  ;end imm_umul
  
  
  !mov rax,[rsi+0];r512[0]
  !add rax, [rbx+0];512[0]+t[0]>512[0]
  !mov [rsi+0],rax;r512[0]
  
  !mov rax,[rsi+8];r512[1]
  !adc rax,[rbx+8];512[1]+t[1]>512[1]
  !mov [rsi+8],rax;r512[1]
  
  !mov rax,[rsi+16];r512[2]
  !adc rax,[rbx+16];512[2]+t[2]>512[2]
  !mov [rsi+16],rax;r512[2]
  
  !mov rax,[rsi+24];r512[3]
  !adc rax,[rbx+24];512[3]+t[2]>512[3]
  !mov [rsi+24],rax;r512[3]
  
  
  ;**Reduce from 320 to 256
  ;No overflow possible here t[4]+c<=0x1000003D1ULL  
  !mov rax,[rbx+32];t[4]+carry
  !adc rax,0
  !mul r8; (t[4]+carry)*1000003D1h>rax>u10 rdx>u11
  
  !add rax,[rsi+0];u10+512[0]>512[0]
  !mov [rsi+0],rax;r512[0]
  
  !adc rdx,[rsi+8];u11+512[1]>512[1]
  !mov [rsi+8],rdx;r512[1]
  
  !mov rax,0
  !adc rax,[rsi+16];0+512[2]>512[2]
  !mov [rsi+16],rax;r512[2]
  
  !mov rax,0
  !adc rax,[rsi+24];0+512[3]>512[3]
  !mov [rsi+24],rax;r512[3]
  
  !mov rax,0
  !adc rax,rax
  !mov [p.v_overflow],rax  
  ;Probability of carry here Or that this>P is very very unlikely
  
  ;Debug "low :"+m_gethex32(*r512)
  ;End
  borrow = m_subX64(*r512,*r512,*p)   
  ;38f37014ce22fc29cf19f28a5ce4da091445536c3e2cff318ba07c2a3048f518
  If overflow
    If Not borrow      
       m_subX64(*r512,*r512,*p) 
    EndIf
  Else
    If borrow      
      m_addX64(*r512,*r512,*p) 
    EndIf  
  EndIf  
  ;shold be 3d6c452d1c076d0425ac63c7783f563df3ec12324d0f16bf7c8335253ef4be33


  move32b_(p.p_r512,p.p_res,0,0)
 
EndProcedure

Procedure m_Ecc_modInv_updateX64(*uv, *mod)
  Protected carry
  If Not m_Ecc_EvenX64(*uv)
    carry=m_addX64(*uv,*uv,*mod)
  EndIf
  m_shrX64(*uv)
  If carry
    PokeI(*uv+24,PeekI(*uv+24)|$8000000000000000)
  EndIf
EndProcedure



Procedure m_Ecc_modInvX64(*res,*inp,*mod)
  Protected *a, *b, *u, *v, cmpResult
  ;Computes result = (1 / input) % mod
  ;From Euclid's GCD to Montgomery Multiplication to the Great Divide
  
  If Not m_check_nonzeroX64(*inp)
    ;==0    
    FillMemory(*res,32,0,#PB_Long)    
  Else
    *a=AllocateMemory(128, #PB_Memory_NoClear)
    *b=*a+32
    *u=*a+64
    *v=*a+96
    
    move32b_(p.p_inp, p.p_a,0,0)
    move32b_(p.p_mod, p.p_b,0,0)  
    m_Ecc_ClearMX64(*u)
    m_Ecc_ClearMX64(*v)
    
    PokeI(*u,1)    
    ;Debug Curve::m_gethex32(*u)
    cmpResult=m_check_less_more_equilX64(*a,*b)    
    While cmpResult  
     
      If m_Ecc_EvenX64(*a)
        m_shrX64(*a)
        m_Ecc_modInv_updateX64(*u, *mod)
      ElseIf   m_Ecc_EvenX64(*b)
          m_shrX64(*b)
          m_Ecc_modInv_updateX64(*v, *mod)
      ElseIf cmpResult=2;more
          m_subX64(*a,*a,*b)
          m_shrX64(*a)
          If m_check_less_more_equilX64(*u,*v)=1; less
            m_addX64(*u,*u,*mod)
          EndIf
          m_subX64(*u,*u,*v)
          m_Ecc_modInv_updateX64(*u, *mod)
      Else
        m_subX64(*b,*b,*a)
        m_shrX64(*b)
        If m_check_less_more_equilX64(*v,*u)=1; less
            m_addX64(*v,*v,*mod)
        EndIf
        m_subX64(*v,*v,*u)
        m_Ecc_modInv_updateX64(*v, *mod)
      EndIf
      cmpResult=m_check_less_more_equilX64(*a,*b)
    Wend
    move32b_(p.p_u, p.p_res,0,0)
    FreeMemory(*a)
  EndIf
EndProcedure

Procedure m_DBLTX64(*cx,*cy,*x,*y,*p)
  Protected *s, *ds, *dx, *tx, *high
  *s=AllocateMemory(192+40,#PB_Memory_NoClear)
  *ds=*s+32
  *dx=*s+64
  *tx=*s+96
  *high=*s+128
  ;get inverse inverse(2*y,p)
  m_addModX64(*s,*y,*y,*p)
  m_Ecc_modInvX64(*s,*s,*p)
  ;3x^2
  m_squareModX64(*dx,*x,*p, *high)
  m_addModX64(*tx,*dx,*dx,*p)
  m_addModX64(*tx,*dx,*tx,*p)
  ;s = 3x^2 * 1/2y
  m_mulModX64(*s,*tx,*s,*p, *high)
  ;s^2
  m_squareModX64(*ds,*s,*p, *high)
  ;Rx = s^2 - 2px
  m_subModX64(*ds,*ds,*x,*p)
  m_subModX64(*ds,*ds,*x,*p);ds=x
  ;Ry = s(px - rx) - py =>slope*(x-xsum)-y 
  m_subModX64(*dx,*x,*ds,*p)
  m_mulModX64(*tx,*s,*dx,*p, *high)
  m_subModX64(*cy,*tx,*y,*p) 
  move32b_(p.p_ds, p.p_cx,0,0)
  FreeMemory(*s)
EndProcedure

Procedure m_ADDPTX64(*cxout,*cyout,*ax,*ay,*bx,*by,*p)
  Protected *s,*cx,*cy, *high
  *s=AllocateMemory(160+40, #PB_Memory_NoClear)
  *cx = *s+32
  *cy = *s+64
  *high = *s+96
  
  If m_check_equilX64(*ax,*bx)    
    m_DBLTX64(*cxout,*cyout,*ax,*ay,*p)
  Else
    ;get inverse inverse(x1-x2,p)
    m_subModX64(*s,*ax,*bx,*p)
    m_Ecc_modInvX64(*s,*s,*p)
    ;slope=(y1-y2)*inverse(x1-x2,p)
    m_subModX64(*cy,*ay,*by,*p)
    m_mulModX64(*s,*cy,*s,*p, *high)
    ;Rx = s^2 - Gx - Qx =>  pow_mod(slope,2,p)-(x1+x2)    
    m_squareModX64(*cy,*s,*p, *high)   
    m_subModX64(*cy,*cy,*ax,*p)
    m_subModX64(*cx,*cy,*bx,*p)
    ;Ry = s(px - rx) - py => slope*(x1-xsum)-y1 
    m_subModX64(*cy,*ax,*cx,*p)
    m_mulModX64(*cy,*s,*cy,*p, *high)
    m_subModX64(*cy,*cy,*ay,*p)
    move32b_(p.p_cx, p.p_cxout,0,0)
    move32b_(p.p_cy, p.p_cyout,0,0)
  EndIf
 
  FreeMemory(*s)
EndProcedure



Procedure m_PTMULX64(*cx, *cy, *ax, *ay, *multipler,*p)
  Protected *locala, *scaleX, *scaleY
  *locala = AllocateMemory(96)
  *scaleX  = *locala+32
  *scaleY = *locala+64
  
  move32b_(p.p_multipler, p.p_locala,0,0)

  
  move32b_(p.p_ax, p.p_scaleX,0,0)
  move32b_(p.p_ay, p.p_scaleY,0,0)
  
  m_Ecc_ClearMX64(*cx):m_Ecc_ClearMX64(*cy)
  
  While m_check_nonzeroX64(*locala)    
    !mov rsi,[p.p_locala]
    !mov eax,[rsi]
    !bt  eax,0
    !jnc llm_ptmul_continue
   
    If m_check_nonzeroX64(*cx)=0 Or m_check_nonzeroX64(*cy)=0      
        move32b_(p.p_scaleX, p.p_cx,0,0)
        move32b_(p.p_scaleY, p.p_cy,0,0)        
      Else            
        m_ADDPTX64(*cx,*cy,*cx,*cy,*scaleX,*scaleY,*p)       
      EndIf  
    
    !llm_ptmul_continue:
    m_DBLTX64(*scaleX,*scaleY,*scaleX,*scaleY,*p)    
    m_shrX64(*locala)    
  Wend
   
  FreeMemory(*locala)
EndProcedure

Procedure m_PTDIVX64(*cx, *cy, *ax, *ay, *multipler,*p, *n)
  Protected *indiv
  Shared *Curveqn
  *indiv = AllocateMemory(32)
  m_Ecc_modInvX64(*indiv,*multipler,*n)
   
  m_PTMULX64(*cx, *cy, *ax, *ay, *indiv,*p)
  FreeMemory(*indiv)
EndProcedure
Procedure DoPowMod(*res, *base, *exp, *modulus, *high)
  Protected *s,*e,*b
  *s=AllocateMemory(96, #PB_Memory_NoClear)
  *e = *s+32   
  *b = *s+64
  
  
  CopyMemory(*exp,*e,32)
  CopyMemory(*base,*b,32)
  
  Curve::m_sethex32(*s, @"0000000000000000000000000000000000000000000000000000000000000001")
  
  ;base = DoMod(base, modulus) ;base % modulus;
 ;Debug "base:"+base
  While Curve::m_check_nonzeroX64(*e);exp > 0 
    
    If Curve::m_Ecc_TestBitX64(*e, 0) ;(exp & 1)
      Curve::m_mulModX64(*s,*s,*b,*modulus, *high);(result * base) % modulus;      
    EndIf
    Curve::m_mulModX64(*b,*b,*b,*modulus, *high);(base * base) % modulus
    Curve::m_shrX64(*e);exp>> 1
  Wend
  CopyMemory(*s,*res,32)
FreeMemory(*s)
EndProcedure

Procedure m_YfromX64(*cyout,*ax,*p)
  Protected *s,*c7,*c1, *high, *p1
  *s=AllocateMemory(160+128, #PB_Memory_NoClear)
  *c7 = *s+32
  *c1 = *s+64
  *p1 = *s+96
  *high = *s+128
  Curve::m_sethex32(*c7, @"0000000000000000000000000000000000000000000000000000000000000007")
  Curve::m_sethex32(*c1, @"0000000000000000000000000000000000000000000000000000000000000001")
  ;a = (pow(x, 3, p) + 7) % p
  Curve::m_mulModX64(*s,*ax,*ax,*p, *high)
  Curve::m_mulModX64(*s,*s,*ax,*p, *high)
  Curve::m_addModX64(*s,*s,*c7,*p)
  
  
  ;y = Pow(a, (p+1)//4, p)
  Curve::m_addX64(*p1,*p,*c1)
  Curve::m_shrX64(*p1)
  Curve::m_shrX64(*p1)  
  DoPowMod(*cyout,*s,*p1,*p,*high)
   
  ;Curve::m_NegModX64(*cyout,*s,*p)
   
  
 
  FreeMemory(*s)
EndProcedure

Procedure m_Ecc_XYcZ_addX64(*X1, *Y1, *X2, *Y2, *inpX1,*inpY1,*inpX2,*inpY2)
  Protected *t5, *high
  Shared *CurveP
  *t5=AllocateMemory(96+40)
  *high = *t5+32
  move32b_(p.p_inpX1, p.p_X1,0,0) 
  move32b_(p.p_inpY1, p.p_Y1,0,0)
  move32b_(p.p_inpX2, p.p_X2,0,0)
  move32b_(p.p_inpY2, p.p_Y2,0,0)
    
  m_subModX64(*t5,*X2,*X1,*CurveP);t5 = x2 - x1
  m_squareModX64(*t5,*t5,*CurveP, *high);t5 = (x2 - x1)^2 = A
  m_mulModX64(*X1,*X1,*t5,*CurveP, *high);t1 = x1*A = B	
  m_mulModX64(*X2,*X2,*t5,*CurveP, *high);t3 = x2*A = C
  m_subModX64(*Y2,*Y2,*Y1,*CurveP);t4 = y2 - y1
  m_squareModX64(*t5,*Y2,*CurveP, *high);t5 = (y2 - y1)^2 = D
  
   
  m_subModX64(*t5,*t5,*X1,*CurveP);t5 = D - B
  m_subModX64(*t5,*t5,*X2,*CurveP);t5 = D - B - C = x3
  m_subModX64(*X2,*X2,*X1,*CurveP);t3 = C - B
  m_mulModX64(*Y1,*Y1,*X2,*CurveP, *high);t2 = y1*(C - B)
  m_subModX64(*X2,*X1,*t5,*CurveP);t3 = B - x3
  m_mulModX64(*Y2,*Y2,*X2,*CurveP, *high);t4 = (y2 - y1)*(B - x3)
  m_subModX64(*Y2,*Y2,*Y1,*CurveP);t4 = y3
  
  move32b_(p.p_t5, p.p_X2,0,0)
  
  FreeMemory(*t5)
EndProcedure

Procedure m_Ecc_XYcZ_addCX64(*X1, *Y1, *X2, *Y2, *inpX1,*inpY1,*inpX2,*inpY2)
  Protected *t5, *t6, *t7, *high
  Shared *CurveP
  *t5=AllocateMemory(160+40)
  *t6=*t5+32
  *t7=*t5+64
  *high = *t5+96
  ;*rx0+(1-nb)*32, *ry0+(1-nb)*32, *rx0+nb*32, *ry0+nb*32
  
  move32b_(p.p_inpX1, p.p_X1,0,0) 
  move32b_(p.p_inpY1, p.p_Y1,0,0)
  move32b_(p.p_inpX2, p.p_X2,0,0)
  move32b_(p.p_inpY2, p.p_Y2,0,0)
  ;Debug "x1 "+m_gethex32(*X1)
  ;Debug "y1 "+m_gethex32(*Y1)
  ;Debug "x2 "+m_gethex32(*X2)
  ;Debug "y2 "+m_gethex32(*Y2)
    
  ;t1 = X1, t2 = Y1, t3 = X2, t4 = Y2
  m_subModX64(*t5,*X2,*X1,*CurveP);t5 = x2 - x1
  m_squareModX64(*t5,*t5,*CurveP, *high);t5 = (x2 - x1)^2 = A
  m_mulModX64(*X1,*X1,*t5,*CurveP, *high);t1 = x1*A = B	
  m_mulModX64(*X2,*X2,*t5,*CurveP, *high);t3 = x2*A = C
  m_addModX64(*t5,*Y2,*Y1,*CurveP);t5 = y2 + y1
  m_subModX64(*Y2,*Y2,*Y1,*CurveP);t4 = y2 - y1
  
  m_subModX64(*t6,*X2,*X1,*CurveP);t6 = C - B
  m_mulModX64(*Y1,*Y1,*t6,*CurveP, *high);t2 = y1 * (C - B) = E
  m_addModX64(*t6,*X1,*X2,*CurveP);t6 = B + C
  m_squareModX64(*X2,*Y2,*CurveP, *high);t3 = (y2 - y1)^2 = D
  m_subModX64(*X2,*X2,*t6,*CurveP);t3 = D - (B + C) = x3
  
  m_subModX64(*t7,*X1,*X2,*CurveP);t7 = B - x3
  m_mulModX64(*Y2,*Y2,*t7,*CurveP, *high);t4 = (y2 - y1)*(B - x3)
  m_subModX64(*Y2,*Y2,*Y1,*CurveP);t4 = (y2 - y1)*(B - x3) - E = y3
  
  m_squareModX64(*t7,*t5,*CurveP, *high);t7 = (y2 + y1)^2 = F
  m_subModX64(*t7,*t7,*t6,*CurveP);t7 = F - (B + C) = x3'
  m_subModX64(*t6,*t7,*X1,*CurveP);t6 = x3' - B
  m_mulModX64(*t6,*t6,*t5,*CurveP, *high);t6 = (y2+y1)*(x3' - B)
  m_subModX64(*Y1,*t6,*Y1,*CurveP);t2 = (y2+y1)*(x3' - B) - E = y3'
  
  move32b_(p.p_t7, p.p_X1,0,0) 
  FreeMemory(*t5)
EndProcedure

Procedure m_Ecc_apply_zX64(*X1, *Y1, *Z1)
  Protected *t1, *high
  Shared *CurveP
  *t1=AllocateMemory(96+40)
  *high=*t1+32
  m_squareModX64(*t1,*Z1,*CurveP, *high)
  m_mulModX64(*X1,*X1,*t1,*CurveP, *high)
  m_mulModX64(*t1,*t1,*Z1,*CurveP, *high)
  m_mulModX64(*Y1,*Y1,*t1,*CurveP, *high)
  FreeMemory(*t1)
EndProcedure

Procedure m_Ecc_double_jacobian_secp256k1X64(*X1, *Y1, *Z1)
  Protected *t5, *t4, *high
  Shared *CurveP
  *t5=AllocateMemory(128+40)  
  *t4=*t5+32;32-63
  *high=*t5+64
  If Not m_check_nonzeroX64(*Z1)
    ;==0
    Debug "requered z>0(double_jacobian_secp256k1)"
    End
  EndIf
  m_squareModX64(*t5,*Y1,*CurveP, *high);t5 = y1^2 
  m_mulModX64(*t4,*X1,*t5,*CurveP, *high);t4 = x1*y1^2 = A 
  m_squareModX64(*X1,*X1,*CurveP, *high);t1 = x1^2 
  m_squareModX64(*t5,*t5,*CurveP, *high);t5 = y1^4 
  m_mulModX64(*Z1,*Y1,*Z1,*CurveP, *high);t3 = y1*z1 = z3 
  
  m_addModX64(*Y1,*X1,*X1,*CurveP);t2 = 2*x1^2
  m_addModX64(*Y1,*Y1,*X1,*CurveP);t2 = 3*x1^2
  
  ;t2 = 3/2*(x1^2) = B
  If PeekB(*Y1) & 1
    m_addX64(*Y1,*Y1,*CurveP)
  EndIf
  m_shrX64(*Y1)
  
  m_squareModX64(*X1,*Y1,*CurveP, *high);t1 = B^2 
  m_subModX64(*X1,*X1,*t4,*CurveP);t1 = B^2 - A 
  m_subModX64(*X1,*X1,*t4,*CurveP);t1 = B^2 - 2A = x3 
  
  m_subModX64(*t4,*t4,*X1,*CurveP);t4 = A - x3 
  m_mulModX64(*Y1,*Y1,*t4,*CurveP, *high);t2 = B * (A - x3) 
  m_subModX64(*Y1,*Y1,*t5,*CurveP);t2 = B * (A - x3) - y1^4 = y3 
  
  FreeMemory(*t5)
EndProcedure

Procedure m_Ecc_XYcZ_initial_doubleX64(*X1, *Y1, *X2, *Y2, *gx, *gy, *z)
  If Not m_check_nonzeroX64(*z)
    ;==0
    FillMemory(*z,32,0,#PB_Long)
    PokeI(*z,1)    
  EndIf
  move32b_(p.p_gx, p.p_X1,0,0) 
  move32b_(p.p_gy, p.p_Y1,0,0)  
  move32b_(p.p_X1, p.p_X2,0,0) 
  move32b_(p.p_Y1, p.p_Y2,0,0) 
  m_Ecc_apply_zX64(*X1, *Y1, *z)
  m_Ecc_double_jacobian_secp256k1X64(*X1, *Y1, *z)
  m_Ecc_apply_zX64(*X2, *Y2, *z)
EndProcedure




Procedure m_Ecc_regularize_kX64(*k0, *key)
  Protected carry, *nt, *ntcut, gt
  Shared  *Curveqn , *Curveqncut
 
  *nt = *Curveqn
  *ntcut = *Curveqncut
  carry = m_subX64(*k0,*key,*nt)  
  Debug carry
  gt = m_check_less_more_equilX64(*k0,*nt)>>1
  Debug gt
  Debug "testbit"+Str(m_Ecc_TestBitX64(*k0,255))
  If(carry)   
    Debug "add"
    m_addX64(*k0,*key,*nt)
  EndIf

EndProcedure

Procedure m_EccPoint_multX64(*resultX, *resultY, *Gx, *Gy, *key)
  Protected *z, *rx0, *ry0, *rx1, *ry1, *scalar, i, *high
  Protected num_bits=257, nb
  
  Shared *CurveP
  *z  =AllocateMemory(256+40, #PB_Memory_NoClear)  
  *rx0=*z+32;31-63
  *rx1=*z+64;64-95
  *ry0=*z+96;96-127
  *ry1=*z+128;128-159
  *scalar = *z+160;160-191
  *high=*z+192
  
  m_Ecc_ClearMX64(*z)
  
  If m_check_equilX64(*key,*z)
    ;key==0
    m_Ecc_ClearMX64(*resultX)
    m_Ecc_ClearMX64(*resulty)
  Else
    PokeI(*z,1)
    If m_check_equilX64(*key,*z)
      ;key==1
      move32b_(p.p_Gx, p.p_resultX,0,0)  
      move32b_(p.p_Gy, p.p_resultY,0,0)
    Else
      m_Ecc_XYcZ_initial_doubleX64(*rx1,*ry1,*rx0,*ry0, *Gx, *Gy, *z)
      
      m_Ecc_regularize_kX64(*scalar, *key) 
     
      i=num_bits-2
      While i>0
        nb= 1 - m_Ecc_TestBitX64(*scalar, i)   
        
       
        m_Ecc_XYcZ_addCX64(*rx0+(1-nb)*32, *ry0+(1-nb)*32, *rx0+nb*32, *ry0+nb*32,    *rx0+(1-nb)*32, *ry0+(1-nb)*32, *rx0+nb*32, *ry0+nb*32)
       
        m_Ecc_XYcZ_addX64(*rx0+nb*32, *ry0+nb*32 ,*rx0+(1-nb)*32, *ry0+(1-nb)*32,     *rx0+nb*32, *ry0+nb*32 ,*rx0+(1-nb)*32, *ry0+(1-nb)*32)
        
        i-1        
      Wend 
      
      nb= 1 - m_Ecc_TestBitX64(*scalar, i) 
      m_Ecc_XYcZ_addCX64(*rx0+(1-nb)*32, *ry0+(1-nb)*32, *rx0+nb*32, *ry0+nb*32,    *rx0+(1-nb)*32, *ry0+(1-nb)*32, *rx0+nb*32, *ry0+nb*32)
      ;Find final 1/Z value.
      
      
      m_subModX64(*z,*rx1,*rx0,*CurveP)
      m_mulModX64(*z,*z,*ry0+(1-nb)*32,*CurveP, *high)
      m_mulModX64(*z,*z,*Gx,*CurveP, *high)
      m_Ecc_modInvX64(*z,*z,*CurveP)
      
      
      m_mulModX64(*z,*z,*Gy,*CurveP, *high)
      m_mulModX64(*z,*z,*rx0+(1-nb)*32,*CurveP, *high)
      
      
      m_Ecc_XYcZ_addX64(*rx0+nb*32, *ry0+nb*32 ,*rx0+(1-nb)*32, *ry0+(1-nb)*32,     *rx0+nb*32, *ry0+nb*32 ,*rx0+(1-nb)*32, *ry0+(1-nb)*32)
     
      m_Ecc_apply_zX64(*rx0, *ry0, *z)
      move32b_(p.p_rx0, p.p_resultX,0,0)  
      move32b_(p.p_ry0, p.p_resultY,0,0)
    EndIf
  EndIf
  
  FreeMemory(*z)
EndProcedure

Procedure beginBatchAdd(*Invout, totalpoints, *apointX, *apointY,  *pointarr)
  Protected *s, *pointer, *temp, i, *high
  Shared *CurveP
  *s=AllocateMemory(128+40)
  *temp=*s+32
  *high=*s+64
  PokeI(*s,1)
  
  *pointer=*pointarr
  ;pointarr 96b line (x32,y32,diff32)
  i=0
  While i<totalpoints
    If m_check_equilX64(*apointX, *pointer)
      ;px==x
      ;addModP(py, py, x)      
      m_addModX64(*temp,*apointY,*apointY,*CurveP)
    Else
      ;subModP(px, x, x)      
      m_subModX64(*temp,*apointX,*pointer,*CurveP)
    EndIf
    
    m_mulModX64(*s,*s,*temp,*CurveP, *high)
    move32b_(p.p_s, p.p_pointer,0,64)
    
    *pointer+96   
    i+1
  Wend
  m_Ecc_modInvX64(*Invout,*s,*CurveP)
   
  FreeMemory(*s)
EndProcedure

Procedure completeBatchAddWithDouble(*newpointarr,lenline, totalpoints, *apointX, *apointY,  *pointarr, *InvTotal)
  Protected *s, *pointer, *temp, i, *curvInv, *NewpointX, *NewpointY, *pointerdiff, *pointerToNew, *high
  Shared *CurveP
  *s=AllocateMemory(224+40)
  *temp=*s+32
  *curvInv=*s+64
  *NewpointX=*s+96
  *NewpointY=*s+128
  *high=*s+160
  move32b_(p.p_InvTotal, p.p_curvInv,0,0)
  
  *pointer=*pointarr+(totalpoints-1)*96
  *pointerToNew = *newpointarr+(totalpoints-1)*lenline
  totalpoints - 1   
  
  While totalpoints>0
    *pointerdiff = *pointer-96
    m_mulModX64(*s,*curvInv,*pointerdiff+64,*CurveP, *high)
       
    If m_check_equilX64(*apointX, *pointer)
      ;px==x
      ;addModP(py, py, x)      
      m_addModX64(*temp,*apointY,*apointY,*CurveP)
    Else
      ;px!=x
      ;subModP(px, x, x)      
      m_subModX64(*temp,*apointX,*pointer,*CurveP)
    EndIf
    
    m_mulModX64(*curvInv,*curvInv,*temp,*CurveP, *high)
    
    If m_check_equilX64(*apointX, *pointer)
      ;x1==x2
      m_DBLTX64(*NewpointX,*NewpointY,*apointX,*apointY,*CurveP)
    Else
      ;//slope=(y1-y2)*inverse(x1-x2,p)
      m_subModX64(*NewpointY,*apointY,*pointer+32,*CurveP)
      m_mulModX64(*s,*NewpointY,*s,*CurveP, *high)
      ;Rx = s^2 - Gx - Qx =>  pow_mod(slope,2,p)-(x1+x2)
      m_squareModX64(*NewpointY,*s,*CurveP, *high)
      m_subModX64(*NewpointY,*NewpointY,*apointX,*CurveP)
      m_subModX64(*NewpointX,*NewpointY,*pointer,*CurveP)
      ;Ry = s(px - rx) - py
      m_subModX64(*NewpointY,*apointX,*NewpointX,*CurveP)
      m_mulModX64(*NewpointY,*NewpointY,*s,*CurveP, *high)
      m_subModX64(*NewpointY,*NewpointY,*apointY,*CurveP)
      
    EndIf
    ;Debug("["+Str(totalpoints)+"] x: "+m_gethex32(*NewpointX))
    ;Debug("["+Str(totalpoints)+"] y: "+m_gethex32(*NewpointY))
    
    move32b_(p.p_NewpointX, p.p_pointerToNew,0,0)
    move32b_(p.p_NewpointY, p.p_pointerToNew,0,32)
    
    *pointer-96
    *pointerToNew-lenline
    totalpoints - 1
  Wend
  If totalpoints=0
     If m_check_equilX64(*apointX, *pointer)
      ;x1==x2
      m_DBLTX64(*NewpointX,*NewpointY,*apointX,*apointY,*CurveP)
    Else
      ;slope=(y1-y2)*inverse(x1-x2,p)
      m_subModX64(*NewpointY,*apointY,*pointer+32,*CurveP)
      m_mulModX64(*curvInv,*NewpointY,*curvInv,*CurveP, *high)
      ;Rx = s^2 - Gx - Qx =>  pow_mod(slope,2,p)-(x1+x2)
      m_squareModX64(*NewpointY,*curvInv,*CurveP, *high)
      m_subModX64(*NewpointY,*NewpointY,*apointX,*CurveP)
      m_subModX64(*NewpointX,*NewpointY,*pointer,*CurveP)
      ;Ry = s(px - rx) - py
      ;Ry = s(px - rx) - py
      m_subModX64(*NewpointY,*apointX,*NewpointX,*CurveP)
      m_mulModX64(*NewpointY,*NewpointY,*curvInv,*CurveP, *high)
      m_subModX64(*NewpointY,*NewpointY,*apointY,*CurveP)
    EndIf
    
    ;Debug("["+Str(totalpoints)+"] x: "+m_gethex32(*NewpointX))
    ;Debug("["+Str(totalpoints)+"] y: "+m_gethex32(*NewpointY))
    
    move32b_(p.p_NewpointX, p.p_pointerToNew,0,0)
    move32b_(p.p_NewpointY, p.p_pointerToNew,0,32)
  EndIf
  
  FreeMemory(*s)
EndProcedure

Procedure fillarrayN(*pointarr, totalpoints, *apointX, *apointY)
  Protected *pointer, i, k, *invret, j
  Shared *CurveP, *CurveGx, *CurveGy
  
  *invret=AllocateMemory(32)
  If *invret=0
    Debug"!"
  EndIf
  *pointer=*pointarr
  
  If totalpoints
    move32b_(p.p_apointX, p.p_pointer,0,0)
    move32b_(p.p_apointY, p.p_pointer,0,32)
    
    i+1
    While i<totalpoints
      k=i
      If (k+i)>=totalpoints
        k=totalpoints-i
      EndIf  
      
      beginBatchAdd(*invret, k, *pointer, *pointer+32, *pointarr)
      completeBatchAddWithDouble(*pointer+96, 96, k, *pointer, *pointer+32,  *pointarr, *invret)      
     
      *pointer+k*96
      i+k
      
    Wend
  EndIf
  
  FreeMemory(*invret)
EndProcedure
EndModule

;-Usage - Example
CompilerIf #PB_Compiler_IsMainFile
  OpenConsole()
  
Define *CurveP, *CurveGx, *CurveGY
  *CurveP = Curve::m_getCurveValues()
  *CurveGx = *CurveP+32
  *CurveGY = *CurveP+64 
Define a$, b$, c$, *a, *ax, *ay, *b, *bx, *by, *c, *cx, *cy, i, carry, borrow, starttime, *high

*a=AllocateMemory(32)
*ax=AllocateMemory(32)
*ay=AllocateMemory(32)
*b=AllocateMemory(32)
*bx=AllocateMemory(32)
*by=AllocateMemory(32)
*c=AllocateMemory(32)
*cx=AllocateMemory(32)
*cy=AllocateMemory(32)
*high=AllocateMemory(64+40)

b$ = "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
Curve::m_sethex32(*b, @b$)
PrintN( "b:"+Curve::m_gethex32(*b))
PrintN("Negative")
PrintN( "mod "+Curve::m_gethex32(*CurveP))
Curve::m_NegModX64(*c,*b,*CurveP)
PrintN( "low:"+Curve::m_gethex32(*c))
;sould be b7c52588d95c3b9a a25b0403f1eef757 02e84bb7597aabe6 63b82f6f04ef2777     

a$ = "0000000000000000000000000000000000000000000000000000000000000003"
b$ = "0000000000000000000000000000000000000000000000000000000000000004"
c$ = "0000000000000000000000000000000000000000000000000000000000000005"
Curve::m_sethex32(*a, @a$)
Curve::m_sethex32(*b, @b$)
Curve::m_sethex32(*c, @c$)
PrintN( "a:"+Curve::m_gethex32(*a))
PrintN( "*")
PrintN( "b:"+Curve::m_gethex32(*b))
PrintN( "mod "+Curve::m_gethex32(*c))
Curve::m_mulModX64(*c,*a,*b,*c, *high)
PrintN( "low:"+Curve::m_gethex32(*c))
;should be fd3dc529c6eb60fb9d166034cf3c1a5a72324aa9dfd3428a56d7e1ce0179fd9b
PrintN("------------------------")


a$ = "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
b$ = "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
Curve::m_sethex32(*ax, @a$)
Curve::m_sethex32(*ay, @b$)
a$ = "c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5"
b$ = "1ae168fea63dc339a3c58419466ceaeef7f632653266d0e1236431a950cfe52a"
Curve::m_sethex32(*bx, @a$)
Curve::m_sethex32(*by, @b$)
PrintN( "ax:"+Curve::m_gethex32(*ax))
PrintN( "ay:"+Curve::m_gethex32(*ay))
PrintN( "+(addpt)")
PrintN( "bx:"+Curve::m_gethex32(*bx))
PrintN( "by:"+Curve::m_gethex32(*by))
Curve::m_ADDPTX64(*cx,*cy,*ax,*ay,*bx,*by,*CurveP)
PrintN( "x:"+Curve::m_gethex32(*cx))
PrintN( "y:"+Curve::m_gethex32(*cy))
PrintN("------------------------")
;shold be
;x: f9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9
;y: 388f7b0f632de8140fe337e62a37f3566500a99934c2231b6cb9fd7584b8e672


a$="0x342119815c0f816f31f431a9fe98a6c76d11425ecaeaecf2d0ef6def197c56b0"
Curve::m_sethex32(*a, @a$)
PrintN( "a:"+Curve::m_gethex32(*a))
PrintN( "^2")
PrintN( "mod "+Curve::m_gethex32(*CurveP))
Curve::m_squareModX64(*c,*a,*CurveP, *high)
PrintN( "="+Curve::m_gethex32(*c))
PrintN("------------------------")
;sould be
; 38f37014ce22fc29cf19f28a5ce4da091445536c3e2cff318ba07c2a3048f518



a$="0x3fdc2a05828a06c18e057a8d9549bdc3ff05ee69a352342ce382aafeaeb98ef9"
Curve::m_sethex32(*a, @a$)
PrintN( "a:"+Curve::m_gethex32(*a))
PrintN( "^2")
PrintN( "mod "+Curve::m_gethex32(*CurveP))
Curve::m_squareModX64(*c,*a,*CurveP, *high)
PrintN( "="+Curve::m_gethex32(*c))
PrintN("------------------------")
;sould be
; 3d6c452d1c076d0425ac63c7783f563df3ec12324d0f16bf7c8335253ef4be33

a$="0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
Curve::m_sethex32(*a, @a$)
PrintN( "a:"+Curve::m_gethex32(*a))
PrintN( "^2")
PrintN( "mod "+Curve::m_gethex32(*CurveP))
Curve::m_squareModX64(*c,*a,*CurveP, *high)
PrintN( "="+Curve::m_gethex32(*c))
PrintN("------------------------")
;sould be
; 4866d6a5ab41ab2c6bcc57ccd3735da5f16f80a548e5e20a44e4e9b8118c26f2


a$ = "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
Curve::m_sethex32(*a, @a$)
PrintN( "a:"+ Curve::m_gethex32(*a))
PrintN( "a is infininty?"+Str(Curve::m_IsInfinityX64(*a)))
PrintN("------------------------")


a$="0x3fdc2a05828a06c18e057a8d9549bdc3ff05ee69a352342ce382aafeaeb98ef9"
Curve::m_sethex32(*a, @a$)

For i=255 To 0 Step -1
Print(Str( Curve::m_Ecc_TestBitX64(*a,i)))
Next i
;0011111111011100001010100000010110000010100010100000011011000001100011100000010101111010100011011001010101001001101111011100001111111111000001011110111001101001101000110101001000110100001011001110001110000010101010101111111010101110101110011000111011111001
;0011111111011100001010100000010110000010100010100000011011000001100011100000010101111010100011011001010101001001101111011100001111111111000001011110111001101001101000110101001000110100001011001110001110000010101010101111111010101110101110011000111011111001
PrintN("")
PrintN("------------------------")


a$="0x3fdc2a05828a06c18e057a8d9549bdc3ff05ee69a352342ce382aafeaeb98ef9"
Curve::m_sethex32(*a, @a$)
b$="0xdfcad171d3196bdb20eaaf272f8f9bcc6b5a47d4fe53d3d874e703cd2566197e"
Curve::m_sethex32(*b, @b$)
Curve::m_Ecc_AndX64(*c, *a, *b)
PrintN("a: "+Curve::m_gethex32(*a))
PrintN("AND")
PrintN("b: "+Curve::m_gethex32(*b))
PrintN("result: "+Curve::m_gethex32(*c))
;sould be 1fc80001820802c100002a05050999c06b004640a2521008608202cc24200878
PrintN("---------------")


a$="0x3fdc2a05828a06c18e057a8d9549bdc3ff05ee69a352342ce382aafeaeb98ef9"
Curve::m_sethex32(*a, @a$)
b$="0xdfcad171d3196bdb20eaaf272f8f9bcc6b5a47d4fe53d3d874e703cd2566197e"
Curve::m_sethex32(*b, @b$)
carry = Curve::m_addX64(*c, *a, *b)
PrintN("a: "+Curve::m_gethex32(*a))
PrintN("+")
PrintN("b: "+Curve::m_gethex32(*b))
PrintN("result: "+Curve::m_gethex32(*c))
PrintN("carry: "+Str(carry))
;sould be 11fa6fb7755a3729caef029b4c4d959906a60363ea1a608055869aecbd41fa877
PrintN("---------------")


a$="0x3fdc2a05828a06c18e057a8d9549bdc3ff05ee69a352342ce382aafeaeb98ef9"
Curve::m_sethex32(*a, @a$)
b$="0xdfcad171d3196bdb20eaaf272f8f9bcc6b5a47d4fe53d3d874e703cd2566197e"
Curve::m_sethex32(*b, @b$)
borrow = Curve::m_subX64(*c, *a, *b)
PrintN("a: "+Curve::m_gethex32(*a))
PrintN("-")
PrintN("b: "+Curve::m_gethex32(*b))
PrintN("result: "+Curve::m_gethex32(*c))
PrintN("borrow: "+Str(borrow))
;sould be 60115893af709ae66d1acb6665ba21f793aba694a4fe60546e9ba7318953757b
PrintN("---------------")

a$="0x3fdc2a05828a06c18e057a8d9549bdc3ff05ee69a352342ce382aafeaeb98ef9"
Curve::m_sethex32(*a, @a$)
b$="0xdfcad171d3196bdb20eaaf272f8f9bcc6b5a47d4fe53d3d874e703cd2566197e"
Curve::m_sethex32(*b, @b$)
PrintN( "a:"+Curve::m_gethex32(*a))
PrintN( "+")
PrintN( "b:"+Curve::m_gethex32(*b))
PrintN( "mod "+Curve::m_gethex32(*CurveP))
Curve::m_addModX64(*c,*a,*b,*CurveP)
PrintN( "="+Curve::m_gethex32(*c))
PrintN("------------------------")
;shold be 1fa6fb7755a3729caef029b4c4d959906a60363ea1a608055869aeccd41fac48
;1fa6fb7755a3729caef029b4c4d959906a60363ea1a608055869aeccd41fac48

a$="0x3fdc2a05828a06c18e057a8d9549bdc3ff05ee69a352342ce382aafeaeb98ef9"
Curve::m_sethex32(*a, @a$)
b$="0xdfcad171d3196bdb20eaaf272f8f9bcc6b5a47d4fe53d3d874e703cd2566197e"
Curve::m_sethex32(*b, @b$)
PrintN( "a:"+Curve::m_gethex32(*a))
PrintN( "-")
PrintN( "b:"+Curve::m_gethex32(*b))
PrintN( "mod "+Curve::m_gethex32(*CurveP))
Curve::m_subModX64(*c,*a,*b,*CurveP)
PrintN( "="+Curve::m_gethex32(*c))
PrintN("------------------------")
;shold be 60115893af709ae66d1acb6665ba21f793aba694a4fe60546e9ba730895371aa




a$ = "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
Curve::m_sethex32(*a, @a$)
PrintN( "a:"+Curve::m_gethex32(*a))
PrintN( "Inverse")
PrintN( "mod "+Curve::m_gethex32(*CurveP))
Curve::m_Ecc_modInvX64(*c,*a,*CurveP)
PrintN( "low:"+Curve::m_gethex32(*c))
;shold be 237afdf1d2938d86870aaeb8ad77626a67b8e794abfb076be61d003687ca9ef6
PrintN("------------------------")

a$ = "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
b$ = "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
Curve::m_sethex32(*a, @a$)
Curve::m_sethex32(*b, @b$)
PrintN( "a:"+Curve::m_gethex32(*a))
PrintN( "b:"+Curve::m_gethex32(*b))
PrintN( "DBPTL")
Curve::m_DBLTX64(*cx,*cy,*a,*b,*CurveP)
PrintN( "x:"+Curve::m_gethex32(*cx))
PrintN( "y:"+Curve::m_gethex32(*cy))
PrintN("------------------------")
;shold be
;x: c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5
;y: 1ae168fea63dc339a3c58419466ceaeef7f632653266d0e1236431a950cfe52a


a$ = "c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5"
b$ = "1ae168fea63dc339a3c58419466ceaeef7f632653266d0e1236431a950cfe52a"
Curve::m_sethex32(*a, @a$)
Curve::m_sethex32(*b, @b$)
PrintN( "a:"+Curve::m_gethex32(*a))
PrintN( "b:"+Curve::m_gethex32(*b))
PrintN( "DBPTL")
Curve::m_DBLTX64(*cx,*cy,*a,*b,*CurveP)
PrintN( "x:"+Curve::m_gethex32(*cx))
PrintN( "y:"+Curve::m_gethex32(*cy))
PrintN("------------------------")
;shold be
;x: e493dbf1c10d80f3581e4904930b1404cc6c13900ee0758474fa94abe8c4cd13
;y: 51ed993ea0d455b75642e2098ea51448d967ae33bfbdfe40cfe97bdc47739922



a$ = "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
b$ = "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
Curve::m_sethex32(*ax, @a$)
Curve::m_sethex32(*ay, @b$)
a$ = "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
b$ = "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
Curve::m_sethex32(*bx, @a$)
Curve::m_sethex32(*by, @b$)
starttime= ElapsedMilliseconds()
For i = 0 To 9999  
Curve::m_ADDPTX64(*bx,*by,*ax,*ay,*bx,*by,*CurveP)
Next i
PrintN( "x:"+Curve::m_gethex32(*bx))
PrintN( "y:"+Curve::m_gethex32(*by))
PrintN( "compute in"+Str(ElapsedMilliseconds()-starttime)+"ms")
;sould be 
;x:db7432110ba814bfe6371ddfd03ba554b558548aa90e81b8e1421321656065a8
;y:8236f24d965a900384b382e8d772d7e92dee2ce6c3cb33883ea627d54a5170c4




a$ = "0x109a76b996c4b957445be784c15af96b6c7ff16363f1ede51925ecacd1ac6263"
Curve::m_sethex32(*a, @a$)
a$ = "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
b$ = "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
Curve::m_sethex32(*ax, @a$)
Curve::m_sethex32(*ay, @b$)

PrintN( "a:"+Curve::m_gethex32(*a))
PrintN( "Ecc_pointMult")
Curve::m_EccPoint_multX64(*cx, *cy, *ax, *ay, *a)
PrintN( "x:"+Curve::m_gethex32(*cx))
PrintN( "y:"+Curve::m_gethex32(*cy))
PrintN("------------------------")


a$ = "0xdfcad171d3196bdb20eaaf272f8f9bcc6b5a47d4fe53d3d874e703cd2566197e"
Curve::m_sethex32(*a, @a$)
a$ = "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
b$ = "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
Curve::m_sethex32(*ax, @a$)
Curve::m_sethex32(*ay, @b$)

PrintN( "a:"+Curve::m_gethex32(*a))
PrintN( "Ecc_pointMult")
Curve::m_EccPoint_multX64(*cx, *cy, *ax, *ay, *a)
PrintN( "x:"+Curve::m_gethex32(*cx))
PrintN( "y:"+Curve::m_gethex32(*cy))
PrintN("------------------------")



a$ = "0x342119815c0f816f31f431a9fe98a6c76d11425ecaeaecf2d0ef6def197c56b0"
Curve::m_sethex32(*a, @a$)
a$ = "c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5"
b$ = "1ae168fea63dc339a3c58419466ceaeef7f632653266d0e1236431a950cfe52a"
Curve::m_sethex32(*ax, @a$)
Curve::m_sethex32(*ay, @b$)

PrintN( "a:"+Curve::m_gethex32(*a))
PrintN( "Ecc_pointMult")
Curve::m_EccPoint_multX64(*cx, *cy, *ax, *ay, *a)
PrintN( "x:"+Curve::m_gethex32(*cx))
PrintN( "y:"+Curve::m_gethex32(*cy))
PrintN("------------------------")

;G base point!!
a$ = "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
b$ = "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
Curve::m_sethex32(*ax, @a$)
Curve::m_sethex32(*ay, @b$)

;multipler
c$ = "000000000000000000000000000000000000000000000000000000000000000A"
c$ = "3fdc2a05828a06c18e057a8d9549bdc3ff05ee69a352342ce382aafeaeb98ef9"
Curve::m_sethex32(*c, @c$)

PrintN( "Gx:"+Curve::m_gethex32(*ax))
PrintN( "Gy:"+Curve::m_gethex32(*ay))
PrintN( "*(ptmul)")
PrintN( "multipler:"+Curve::m_gethex32(*c))

Curve::m_PTMULX64(*cx, *cy, *ax, *ay, *c,*CurveP)
PrintN( "x:"+Curve::m_gethex32(*cx))
PrintN( "y:"+Curve::m_gethex32(*cy))
PrintN("------------------------")
;shold be
;x:510f6efbef396a1985da989104a295063606319beafa4e1fd0ebd29ace19088f
;y:fcf1cb9e1a9c02fea09e983fe5fe8fb7ce74a80ed3b1783706e27bde4b2ede5e


Input()
CloseConsole()
CompilerEndIf
