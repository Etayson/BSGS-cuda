EnableExplicit
IncludeFile "lib\Curve64.pb"
CompilerIf #PB_Compiler_Unicode
  Debug" switch to Ascii mode"
  End
CompilerEndIf
CompilerIf Not #PB_Compiler_Processor = #PB_Processor_x64
  Debug" only x64 processor support"
  End
CompilerEndIf
#MB=1048576
#GB=1073741824
#array_dim=64
#line_dim=64
#alignMemoryGpu=64   
#LOGFILE=1
#WINFILE="win.txt"

#binfile = 777
#hashbyteoffset=0
#appver="1.9.6File-onlygen"+#hashbyteoffset
Structure JobSturucture
  *arr
  *NewPointsArr
  beginrangeX$
  beginrangeY$
  totalpoints.i  
  pointsperbatch.i
  isAlive.i
  isError.i  
  Yoffset.i
  beginNumberPoint.i
EndStructure

Structure sortjobStructure
  *ptarr
  *sortptarray
  totallines.i
  curpos.i
EndStructure

Structure comparsationStructure
  pos.i
  direction.i
EndStructure


Structure CoordPoint
  *x
  *y
EndStructure
#helpsize = 4096
#align_size=128
#HashTablesz=4;4B counter items
#Pointersz=8
#HashTableSizeHash=4
#HashTableSizeItems=8
#maximumgpucount = 32

Structure HashTableResultStructure   
 size.l
 *contentpointer
 fileoffset.i
EndStructure

Define TableMutex
Define cls$=RSet(cls$,80,Chr(8))
TableMutex = CreateMutex()


Define *CurveP, *CurveGX, *CurveGY, *Curveqn
*CurveP = Curve::m_getCurveValues()
*CurveGX = *CurveP+32
*CurveGY = *CurveP+64
*Curveqn = *CurveP+96

Global Dim a$(7)
Global Dim gpu(#maximumgpucount)
a$(0)="MAX_THREADS_PER_BLOCK "
a$(1)="SHARED_SIZE_BYTES "
a$(2)="CONST_SIZE_BYTES "
a$(3)="LOCAL_SIZE_BYTES "
a$(4)="NUM_REGS "
a$(5)="PTX_VERSION "
a$(6)="BINARY_VERSION" 
Define recovery=0
Define recoveryCNT$
Define recoverypos
Define recoveryfilename$
Define recoverypub$
Define recoveryfingerprint$
Define cnttimer=180
Define settingsvalue$, settingsFingerPrint$
Define cls$
Define  threadtotal.i
Define  blocktotal.i
Define pparam.w
Define.s Gx , Gy, p, privkey, privkeyend, pball$, walletall$, mainpub, addpubs
Define waletcounter, usedgpucount, isruning
Define maxnonce, *GiantArr, *GiantArrPacked, *HelperArr, totallaunched, *GpuHT_unalign, *GpuHT
Define NewMap job.JobSturucture()
Define NewMap sortjob.sortjobStructure()
Define keyMutex, quit, *PointerTable_unalign, *PointerTable
Define *PrivBIG, PubkeyBIG.CoordPoint, *MaxNonceBIG, FINDPUBG.CoordPoint, ADDPUBG.CoordPoint, *bufferResult, *addX, *addY, *PRKADDBIG, PUBADDBIG.CoordPoint, REALPUB.CoordPoint, *WINKEY, Two.CoordPoint
Define *WidthRange
Define Defdevice$, HT_POW=25, endrangeflag=0, pubfile$="", NewList publist.s() , globalquit, isreadyjob, listpos, allbabypoint
Define JobMutex
Define *GlobKey
Define GlobPub.CoordPoint
Define *CenterBig, *CenterX, *CenterY
Define *GlobCnt, memtotalfreed, countertotalfreed, checkerror
JobMutex = CreateMutex()
keyMutex = CreateMutex()

;-VARIABLES
;-infile 1024.txt
threadtotal = 256;512
blocktotal = 22*6;68 2080ti 10-10606gb 22-1660 super
pparam=400
waletcounter=Int(Pow(2, 25.2))

;mainpub.s = "ec85814d714e77cbf4b12b7de522bcdc541532fafb77ea9a87852cdbebf80b19d773de78d0e5f7ce4ffcb3143253fd519fc979de564493962d1c4fc8b6cc1bd9"
;mainpub.s = "9b46a5c1c66aa27ac6409414db3f7994c79c7b2aa22a63f79fec0b3a6c2ba706c8913a9abf96cc9dc6ea102e19ffea29a0845f2c6d12f88380aea8de61a368fe"
;mainpub.s = "11569442e870326ceec0de24eb5478c19e146ecd9d15e4666440f2f638875f42524c08d882f868347f8b69d3330dc1913a159d8fb2b27864f197693a0eb39a23"
mainpub.s = "e1e5e6f7b0b8d67604e3940c87bf06b814cedc486112b9956c68e3d78b1bd81297fe4f65fbd6e9f7eb1eea80b144d1487f2a9b0aeae5fcf6f43b41491641884e" ;125357 1E9AD
;mainpub.s = "59A3BFDAD718C9D3FAC7C187F1139F0815AC5D923910D516E186AFDA28B221DC994327554CED887AAE5D211A2407CDD025CFC3779ECB9C9D7F2F1A1DDF3E9FF8"
mainpub.s = "036d05521c67b9cc1c0ef906b42215c7120c7302c34d9316a2726199bedac50936"  ;0016f7027bbf8454a5c
;privkey.s="0x0000000000000000000000000000000000000000000000000000fde000000000"
             
;privkey.s="0x0000000000000000000000000000000000000000000000000020000000000000"
;privkey.s="0x10000000000000000"
;privkeyend.s="0x1ffffffffffffffff"
privkey.s=   "0x1";"0x10000000000000000"
privkeyend.s="0x1ffffffffffffffff"; "0x1ffffffffffffffff";"0x80001F4"
                
Declare getprogparam()
Declare exit(str.s)
Declare Log2(Quad.q)
Declare ReadHTpack(*hash, *arr, *res.HashTableResultStructure)
Declare FilePutContents(filename.s, *mem, size)
Declare RemoveGiantArrTemp() 
Declare compareHTpack(*hash, *rescmp.comparsationStructure, startpos=0)
Declare HashTableInsert(*hash, position)
OpenConsole()

getprogparam()

Define HT_items = Int(Pow(2,HT_POW))
Define HT_mask = HT_items-1
Define HT_total_items = 0
Define HT_max_collisions = 0
Define HT_items_with_collisions = 0
Define HT_total_hashes = 0
Define initHTsize=1
;If BABYS_pow>HT_POW
  ;initHTsize=Int(Pow(2,BABYS_pow-HT_POW))
  ;initHTsize=BABYS_pow-HT_POW
;EndIf

PrintN("initHTsize #"+Str(initHTsize))





maxnonce = threadtotal * blocktotal * pparam






*HelperArr=AllocateMemory(#helpsize * 96 * CountCPUs(#PB_System_ProcessCPUs))
If *HelperArr=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf

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

Procedure toLittleInd32(*a)
  !mov rsi,[p.p_a]  
  !mov eax,[rsi]
  !mov ecx,[rsi+4]
  !bswap eax
  !mov [rsi],eax
  !bswap ecx
  !mov [rsi+4],ecx  
EndProcedure

Procedure toLittleInd32_64(*a)
  !mov rsi,[p.p_a]  
  !mov eax,[rsi]
  !mov ecx,[rsi+4]
  !bswap eax
  !mov [rsi+4],eax
  !bswap ecx
  !mov [rsi],ecx  
EndProcedure

Procedure.s commpressed2uncomressedPub(ha$)
  Protected y_parity, ruc$, x$, a$, *a, *res
  Shared *CurveP
  *a = AllocateMemory(64)  
  *res=*a + 32  
  
  y_parity = Val(Left(ha$,2))-2
  x$ = Right(ha$,Len(ha$)-2)
  
  a$=RSet(x$, 64,"0")
  Curve::m_sethex32(*a, @a$)  
  Curve::m_YfromX64(*res,*a, *CurveP)  
  
  If PeekB(*res)&1<>y_parity
    Curve::m_subModX64(*res,*CurveP,*res,*CurveP)
  EndIf
  
  ruc$ = Curve::m_gethex32(*res)
  
  FreeMemory(*a)
  ProcedureReturn x$+ruc$

EndProcedure

Procedure.s uncomressed2commpressedPub(ha$)
  Protected Str1.s, Str2.s, x$,y$,ru$,rc$
  ha$=LCase(ha$)
  If Left(ha$,2)="04" And Len(ha$)=130
    ha$=Right(ha$,Len(ha$)-2)
  EndIf
  Str1=Left(ha$,64)
  Str2=Right(ha$,64)
  Debug Str1
  Debug Str2
  
  x$=PeekS(@Str1,-1,#PB_Ascii)
  x$=RSet(x$,64,"0")
  y$=PeekS(@Str2,-1,#PB_Ascii)
  y$=RSet(y$,64,"0")
  ru$="04"+x$+y$
  If FindString("13579bdf",Right(y$,1))>0
    rc$="03"+x$
  Else
    rc$="02"+x$
  EndIf
  
  ProcedureReturn rc$

EndProcedure


Procedure Log2(Quad.q)
Protected Result
   While Quad <> 0
      Result + 1
      Quad>>1
   Wend
   ProcedureReturn Result-1
 EndProcedure
 
Procedure ValueL(*a)
  !mov rbx,[p.p_a]   
  !mov eax,[rbx]  
ProcedureReturn
EndProcedure

Procedure INCvalue32(*a)
  !mov rsi,[p.p_a]  
  !mov eax,[rsi]
  !inc eax 
  !mov [rsi],eax  
EndProcedure

Procedure swap8(*a)
  !mov rsi,[p.p_a]  
  !mov eax,[rsi]
  !mov ecx,[rsi+4]  
  !mov [rsi+4],eax
  !mov [rsi],ecx  
EndProcedure

Procedure swap32(*a)
  !mov rsi,[p.p_a]  
  !mov eax,[rsi+24]
  !mov ecx,[rsi+4]
  !mov [rsi+4],eax
  !mov [rsi+24],ecx 
  
  !mov rsi,[p.p_a]  
  !mov eax,[rsi+28]
  !mov ecx,[rsi]
  !mov [rsi],eax
  !mov [rsi+28],ecx 
  
  !mov rsi,[p.p_a]  
  !mov eax,[rsi+20]
  !mov ecx,[rsi+8]
  !mov [rsi+8],eax
  !mov [rsi+20],ecx 
  
  !mov rsi,[p.p_a]  
  !mov eax,[rsi+16]
  !mov ecx,[rsi+12]
  !mov [rsi+12],eax
  !mov [rsi+16],ecx 
EndProcedure

Procedure m_check_less_more_equilX8(*s,*t); 0 - s = t, 1- s < t, 2- s > t
  !mov rsi,[p.p_s]  
  !mov rdi,[p.p_t]
  
    
  !xor cx,cx
  !llm_check_less_continueQ:
  
  !mov rax,[rsi]
  !mov rbx,[rdi]
   
  !cmp rax,rbx
  !jb llm_check_less_exit_lessQ
  !ja llm_check_less_exit_moreQ 
  
  !xor rax,rax
  !jmp llm_check_less_exitQ  
  
  !llm_check_less_exit_moreQ:
  !mov rax,2
  !jmp llm_check_less_exitQ  
  
  !llm_check_less_exit_lessQ:
  !mov rax,1
  !llm_check_less_exitQ:
ProcedureReturn  
EndProcedure

Procedure check_equil(*s,*t,len=8)
  !mov rsi,[p.p_s]  
  !mov rdi,[p.p_t]
  !xor cx,cx
  !ll_check_equil_continue:
  
  !mov eax,[rsi]
  !mov ebx,[rdi]
  !add rsi,4
  !add rdi,4
  !bswap eax
  !bswap ebx
  !cmp eax,ebx
  !jne ll_check_equil_exit_noteqil
  !inc cx 
  !cmp cx,[p.v_len]
  !jb ll_check_equil_continue
  
  !mov eax,1
  !jmp ll_check_equil_exit  
  
  !ll_check_equil_exit_noteqil:
  !mov eax,0
  !ll_check_equil_exit:
ProcedureReturn  
EndProcedure


Procedure div8(*s,n,*q,*r);8 byte / n> *q, *r
  !mov rsi,[p.p_s]   
  !xor rdx,rdx
  !mov rax,[rsi]
  !mov rbx,[p.v_n]
  !div rbx
  !mov rsi,[p.p_r]   
  !mov [rsi],rdx
  !mov rsi,[p.p_q] 
  !mov [rsi],rax
  
ProcedureReturn  
EndProcedure

Procedure sub8(*a,*b,*c);8 byte a-b> c
  !mov rsi,[p.p_a]  
  !mov rax,[rsi]
  !mov rdi,[p.p_b]
  !sub rax,[rdi]
  !mov rsi,[p.p_c] 
  !mov [rsi],rax
  
ProcedureReturn  
EndProcedure

Procedure add8(*a,*b,*c);8 byte a+b> c
  !mov rsi,[p.p_a]  
  !mov rax,[rsi]
  !mov rdi,[p.p_b]
  !add rax,[rdi]
  !mov rsi,[p.p_c] 
  !mov [rsi],rax
  
ProcedureReturn  
EndProcedure

Procedure add8ui(*a,n,*c);8 byte a+b> c
  !mov rsi,[p.p_a]  
  !mov rax,[rsi]
  !add rax,[p.v_n]
  !mov rsi,[p.p_c] 
  !mov [rsi],rax
  
ProcedureReturn  
EndProcedure

Procedure mul8ui(*s,n,*q);8 byte * n
  !mov rsi,[p.p_s]   
  !mov rax,[rsi]  
  !mov rbx,[p.v_n]
  !mul rbx
  !mov rsi,[p.p_q] 
  !mov [rsi],rax
  
ProcedureReturn  
EndProcedure

Procedure deserialize(*a,b,*sptr,counter=32);fron hex
  Protected *ptr
    *ptr=*a+64*b  
  
  !mov rbx,[p.p_ptr] ;ebx > rbx
  !mov rdi,[p.p_sptr] ;edi > rdi  
  
  !xor cx,cx  
  !ll_MyLabelf:
  
  !push cx
  !mov eax,[rdi]
  !mov ecx,eax
  !xor edx,edx
  
   
  !sub al,48  
  !cmp al,15     
  !jb ll_MyLabelf1        
  !sub al,7
  
  !ll_MyLabelf1:
  !and al,15      ;1
  !or dl,al  
  !rol edx,4
  !ror ecx,8
  !mov al,cl
  
  !sub al,48  
  !cmp al,15     
  !jb ll_MyLabelf2        
  !sub al,7
  
  !ll_MyLabelf2:
  !and al,15      ;2
  !or dl,al  
  !rol edx,4
  !ror ecx,8
  !mov al,cl
  
  !sub al,48  
  !cmp al,15     
  !jb ll_MyLabelf3        
  !sub al,7
  
  !ll_MyLabelf3:
  !and al,15      ;3
  !or dl,al  
  !rol edx,4
  !ror ecx,8
  !mov al,cl
  
  !sub al,48  
  !cmp al,15     
  !jb ll_MyLabelf4        
  !sub al,7
  
  !ll_MyLabelf4:
  !and al,15      ;4
  !or dl,al  
  
  !ror dx,8
  !mov [rbx],dx
  !add rdi,4
  !add rbx,2
  
  
  !pop cx   
  !inc cx 
  !cmp cx,[p.v_counter]
  !jb ll_MyLabelf 
  
  

EndProcedure

Procedure serialize(*a,b,*sptr,counter=32);>hex  
 Protected *ptr
  *ptr=*a+#array_dim*b  
  
  !mov rbx,[p.p_ptr] ;ebx > rbx
  !mov rdi,[p.p_sptr] ;edi > rdi
  
  !xor cx,cx
  !ll_MyLabel:
  
  !push cx
  
  !mov ax,[rbx]
  !xor edx,edx
  
  !mov cx,ax
  
  !and ax,0fh
  !cmp al,10     ;1
  !jb ll_MyLabel1        
  !add al,39
  
  !ll_MyLabel1:
  !add al,48   
  !or dx,ax
  !shl edx,8
  
  !ror cx,4
  !mov ax,cx
  
  !and ax,0fh
  !cmp al,10     ;2
  !jb ll_MyLabel2        
  !add al,39
  
  !ll_MyLabel2:
  !add al,48   
  !or dx,ax

  !shl edx,8
  
  !ror cx,4
  !mov ax,cx
  
  !and ax,0fh
  !cmp al,10     ;3
  !jb ll_MyLabel3        
  !add al,39
  
  !ll_MyLabel3:
  !add al,48   
  !or dx,ax
  !shl edx,8
  
  !ror cx,4
  !mov ax,cx
  
  !and ax,0fh
  !cmp al,10     ;4
  !jb ll_MyLabel4        
  !add al,39
  
  !ll_MyLabel4:
  !add al,48   
  !or dx,ax
  !ror edx,16
  !mov [rdi],edx
  !add rdi,4
  !add rbx,2
  
  !pop cx
  !inc cx
  !cmp cx,[p.v_counter]; words
  !jb ll_MyLabel 
EndProcedure

Procedure exit(a$)
  PrintN(a$)
  PrintN("Press Enter to exit")
  Input()
  CloseConsole()
  End
EndProcedure


Procedure.s cutHex(a$)
  a$=Trim(UCase(a$)) 
  If Left(a$,2)="0X" 
    a$=Mid(a$,3,Len(a$)-2)
  EndIf 
  If Len(a$)=1
    a$="0"+a$
  EndIf
ProcedureReturn LCase(a$)
EndProcedure

Procedure getprogparam()
  Protected parametrscount, datares$, i, walid
  Shared Defdevice$,  privkey, privkeyend
  Shared threadtotal, blocktotal, pparam, waletcounter,mainpub, HT_POW, pubfile$, recovery, recoveryfilename$, cnttimer
  parametrscount=CountProgramParameters()
  
  i=0
  While i<parametrscount  
    Select LCase(ProgramParameter(i))
      Case "-h"
        Debug "found -h"
        
           PrintN( " -t      Number of GPU threads, default "+Str(threadtotal))
           PrintN( " -b      Number of GPU blocks, default "+Str(blocktotal))
           PrintN( " -p      Number of pparam, default "+Str(pparam))
           PrintN( " -d      Select GPU IDs, default "+Defdevice$)           
           PrintN( "-w       Set number of baby items 2^ or decimal representation")
           PrintN( "-htsz    Set number of HashTable 2^ , default "+Str(HT_POW))
           PrintN("    Recommendation:")
           PrintN("    with htsz 27 value -w should be less Or equil To 1331331443 Or 2^30.310222637591963")
           PrintN("    with htsz 28 value -w should be less or equil to 1777178603 Or 2^30.726941530690112")
           PrintN("    with htsz 29 value -w should be less or equil to 3069485950 Or 2^31.515349920643907")
           PrintN("    with htsz 30 value -w should be less or equil to 3069485951 Or 2^31.515349920643907")
           PrintN("    with htsz 31 value -w should be less or equil to 3069485951 Or 2^31.515349920643907")           
           Input()
           End
      Case "-wl"
        Debug "found -wl"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-"  
          recoveryfilename$ = datares$    
          recovery=1
          PrintN( "Recovery work file: "+recoveryfilename$)
         EndIf
      Case "-wt"
        Debug "found -wt"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-"  
          cnttimer = Val(datares$)
          If cnttimer<30
            cnttimer=30
          EndIf
          PrintN( "Saving timer every "+Str(cnttimer)+" seconds")
         EndIf   
      Case "-infile"
        Debug "found -infile"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-"  
          pubfile$=datares$         
          PrintN( "Will be used file: "+pubfile$)
        EndIf
      Case "-t"
        Debug "found -t"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-"  
          threadtotal=Val(datares$)          
           PrintN( "Number of GPU threads set to #"+threadtotal)
        EndIf
      Case "-b"
        Debug "found -b"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-"  
          blocktotal=Val(datares$)          
          PrintN( "Number of GPU blocks set to #"+blocktotal)
        EndIf
      Case "-p"
        Debug "found -p"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-"  
          pparam=Val(datares$)          
          PrintN( "Number of pparam set to #"+pparam)
        EndIf  
      Case "-d"
        Debug "found -d"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-"  
          Defdevice$=datares$
          PrintN( "Used GPU devices #"+Defdevice$)
        EndIf
        
      Case "-pb"
        Debug "found -pb"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-"  
          mainpub=LCase(cutHex(datares$))
          PrintN( "Pubkey set to "+mainpub)
          walid + 1
        EndIf
              
       Case "-pk"
        Debug "found -pk"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-"  
          privkey=LCase(cutHex(datares$))
          PrintN( "Range begin: 0x"+privkey)         
        EndIf 
        
       Case "-pke"
        Debug "found -pke"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-"  
          privkeyend=LCase(cutHex(datares$))
          PrintN( "Range end: 0x"+privkeyend)         
        EndIf  
        
       Case "-w"
        Debug "found -w"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-" 
          If ValD(datares$)>32
            waletcounter=Val(datares$)
            PrintN( "Items number set to "+Str(waletcounter)+" = 2^"+StrD(Log(waletcounter)/Log(2)))
          Else            
            waletcounter=Int(Pow(2, ValD(datares$)))
            PrintN( "Items number set to 2^"+datares$+"="+Str(waletcounter))
          EndIf
          
        EndIf 
        
       Case "-htsz"
        Debug "found -htsz"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-"  
          HT_POW=Val(datares$)
          PrintN( "HT size set to 2^"+HT_POW)         
        EndIf 
        
      Default
        exit("Unknown parameter ["+ProgramParameter(i)+"]")
    EndSelect
    
    


    i+1 
  Wend
EndProcedure

Procedure check_less_more_equil(*s,*t,len=8);0 - s = t, 1- s < t, 2- s > t
  !mov rsi,[p.p_s]  
  !mov rdi,[p.p_t]
  !xor cx,cx
  !ll_check_less_continue:
  
  !mov eax,[rsi]
  !mov ebx,[rdi]
  !add rsi,4
  !add rdi,4
  !bswap eax
  !bswap ebx
  !cmp eax,ebx
  !jb ll_check_less_exit_less
  !ja ll_check_less_exit_more  
  !inc cx 
  !cmp cx,[p.v_len]
  !jb ll_check_less_continue
  
  !mov eax,0
  !jmp ll_check_less_exit  
  
  !ll_check_less_exit_more:
  !mov eax,2
  !jmp ll_check_less_exit  
  
  !ll_check_less_exit_less:
  !mov eax,1
  !ll_check_less_exit:
ProcedureReturn  
EndProcedure

Procedure BabycompleteBatchAddWithDouble(*newpointarr,lenline, totalpoints, *apointX, *apointY,  *pointarr, *InvTotal)
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
    Curve::m_mulModX64(*s,*curvInv,*pointerdiff+64,*CurveP, *high)
       
    If Curve::m_check_equilX64(*apointX, *pointer)
      ;px==x
      ;addModP(py, py, x)      
      Curve::m_addModX64(*temp,*apointY,*apointY,*CurveP)
    Else
      ;px!=x
      ;subModP(px, x, x)      
      Curve::m_subModX64(*temp,*apointX,*pointer,*CurveP)
    EndIf
    
    Curve::m_mulModX64(*curvInv,*curvInv,*temp,*CurveP, *high)
    
    If Curve::m_check_equilX64(*apointX, *pointer)
      ;x1==x2
      Curve::m_DBLTX64(*NewpointX,*NewpointY,*apointX,*apointY,*CurveP)
    Else
      ;//slope=(y1-y2)*inverse(x1-x2,p)
      Curve::m_subModX64(*NewpointY,*apointY,*pointer+32,*CurveP)
      Curve::m_mulModX64(*s,*NewpointY,*s,*CurveP, *high)
      ;Rx = s^2 - Gx - Qx =>  pow_mod(slope,2,p)-(x1+x2)
      Curve::m_squareModX64(*NewpointY,*s,*CurveP, *high)
      Curve::m_subModX64(*NewpointY,*NewpointY,*apointX,*CurveP)
      Curve::m_subModX64(*NewpointX,*NewpointY,*pointer,*CurveP)
      ;Ry = s(px - rx) - py
      Curve::m_subModX64(*NewpointY,*apointX,*NewpointX,*CurveP)
      Curve::m_mulModX64(*NewpointY,*NewpointY,*s,*CurveP, *high)
      Curve::m_subModX64(*NewpointY,*NewpointY,*apointY,*CurveP)
      
    EndIf
    ;PrintN("["+Str(totalpoints)+"] x: "+Curve::m_gethex32(*NewpointX))
    ;PrintN("["+Str(totalpoints)+"] y: "+Curve::m_gethex32(*NewpointY))
    
    CopyMemory(*NewpointX+#hashbyteoffset,*pointerToNew,8)
    
    
    *pointer-96
    *pointerToNew-lenline
    totalpoints - 1
  Wend
  If totalpoints=0
     If Curve::m_check_equilX64(*apointX, *pointer)
      ;x1==x2
      Curve::m_DBLTX64(*NewpointX,*NewpointY,*apointX,*apointY,*CurveP)
    Else
      ;slope=(y1-y2)*inverse(x1-x2,p)
      Curve::m_subModX64(*NewpointY,*apointY,*pointer+32,*CurveP)
      Curve::m_mulModX64(*curvInv,*NewpointY,*curvInv,*CurveP, *high)
      ;Rx = s^2 - Gx - Qx =>  pow_mod(slope,2,p)-(x1+x2)
      Curve::m_squareModX64(*NewpointY,*curvInv,*CurveP, *high)
      Curve::m_subModX64(*NewpointY,*NewpointY,*apointX,*CurveP)
      Curve::m_subModX64(*NewpointX,*NewpointY,*pointer,*CurveP)
      ;Ry = s(px - rx) - py
      ;Ry = s(px - rx) - py
      Curve::m_subModX64(*NewpointY,*apointX,*NewpointX,*CurveP)
      Curve::m_mulModX64(*NewpointY,*NewpointY,*curvInv,*CurveP, *high)
      Curve::m_subModX64(*NewpointY,*NewpointY,*apointY,*CurveP)
    EndIf
    
    ;PrintN("["+Str(totalpoints)+"] x: "+Curve::m_gethex32(*NewpointX))
    ;PrintN("["+Str(totalpoints)+"] y: "+Curve::m_gethex32(*NewpointY))
    
    CopyMemory(*NewpointX+#hashbyteoffset,*pointerToNew,8)
  EndIf
  
  FreeMemory(*s)
EndProcedure

Procedure baby(id)
  Protected *my_pubX, *my_pubY, *arr, *newarrr, pointsperbatch, totalpoints, *inv, msg$="#"+Str(id)+"  ", newbatchsz, calculatesz, leadzero, a$, *ptrarr
  Protected *bufferResult, temp$, *addX, *addY , i , localbeginNumberPoint
  Shared job(), totallaunched, TableMutex
  Shared *CurveGX, *CurveGY, *CurveP, allbabypoint

  
  leadzero=32
  *inv = AllocateMemory(192)
  If *inv=0
    PrintN(msg$+"Can`t allocate memory")
    exit("")
  EndIf
  *bufferResult= *inv+32
  *addX= *inv+64
  *addY= *inv+96  
  *my_pubX = *inv+128
  *my_pubY = *inv+160
  
  *arr = job(Str(id))\arr  
  localbeginNumberPoint = job(Str(id))\beginNumberPoint
  totalpoints = job(Str(id))\totalpoints
  pointsperbatch = job(Str(id))\pointsperbatch
  *newarrr = AllocateMemory(pointsperbatch * 8) ;job(Str(id))\NewPointsArr
  
  Curve::m_sethex32(*my_pubX, @job(Str(id))\beginrangeX$)
  Curve::m_sethex32(*my_pubY, @job(Str(id))\beginrangeY$)
  Print(msg$)
  ;Print(msg$+"("+Curve::m_gethex32(*my_pubX)+Curve::m_gethex32(*my_pubY)+")")
  
  a$=RSet(Hex(pointsperbatch), 64,"0")
  Curve::m_sethex32(*bufferResult, @a$)
  
  Curve::m_PTMULX64(*addX, *addY, *CurveGX, *CurveGY, *bufferResult,*CurveP)
    
    
  calculatesz=0
  While calculatesz<totalpoints
    
    ;CopyMemory(*my_pubX+24,*newarrr+calculatesz*8,8)
    CopyMemory(*my_pubX+#hashbyteoffset,*newarrr,8)
    
    If calculatesz<totalpoints
      newbatchsz = pointsperbatch
      If newbatchsz>=(totalpoints-calculatesz)
        newbatchsz = totalpoints-calculatesz
        a$=Hex(newbatchsz)
        Curve::m_sethex32(*bufferResult, @a$)      
        Curve::m_PTMULX64(*addX, *addY, *CurveGX, *CurveGY, *bufferResult,*CurveP)
      EndIf
      
      
      
      
      Curve::beginBatchAdd(*inv, newbatchsz-1, *my_pubX, *my_pubY,  *arr)
      ;BabycompleteBatchAddWithDouble(*newarrr+calculatesz*8+8,8, newbatchsz-1, *my_pubX, *my_pubY,  *arr, *inv)
      BabycompleteBatchAddWithDouble(*newarrr + 8,8, newbatchsz-1, *my_pubX, *my_pubY,  *arr, *inv)
      
      For i = 0 To newbatchsz-1
        HashTableInsert(*newarrr + i*8, localbeginNumberPoint + i)        
      Next i
      
      calculatesz  + newbatchsz   
      localbeginNumberPoint + newbatchsz
      LockMutex(TableMutex)
      allbabypoint +  newbatchsz
      UnlockMutex(TableMutex)
      Curve::m_ADDPTX64(*my_pubX,*my_pubY,*my_pubX, *my_pubY,*addX,*addY,*CurveP)
   EndIf
  Wend
totallaunched-1
  FreeMemory(*newarrr)
  FreeMemory(*inv)
EndProcedure

Procedure GenBabys(*xpoint, *ypoint)
  Protected totalCPUcout, i, jobperthread, restjob, a$, filebinname$, full_size, wrbytes, savedbytes, maxsavebytes, loadedbytes, *pp, totalloadbytes, maxloadbytes
  Shared *HelperArr, waletcounter, *CurveGX, *CurveGY, job(), totallaunched, FINDPUBG, *addX, *addY, *CurveP, *bufferResult, mainpub, waletcounter, allbabypoint
  Protected CurNumberPoint, infostr$, cls$
  
  allbabypoint = 0
  filebinname$=Curve::m_gethex32(*xpoint)+"_"+Str(waletcounter)+"_b.BIN"
  full_size=waletcounter*8
  
  
    PrintN("Generate HT with "+Str(waletcounter)+" items")
 
    Curve::fillarrayN(*HelperArr , #helpsize, *CurveGX, *CurveGY)
    ;prntarrBIG(*HelperArr, 5)
    
    totalCPUcout = CountCPUs(#PB_System_ProcessCPUs)
    If totalCPUcout>1 And waletcounter>#helpsize
      ;copythe same points to other threads
      For i =1 To totalCPUcout-1
        CopyMemory(*HelperArr, *HelperArr + i * #helpsize * 96, #helpsize * 96)
      Next
      
      CurNumberPoint = 0
      jobperthread = waletcounter/totalCPUcout
      ;PrintN("jobperthread: "+Str(jobperthread)+" items")
      job(Str(0))\beginNumberPoint = CurNumberPoint 
      job(Str(0))\arr = *HelperArr      
      job(Str(0))\totalpoints = jobperthread
      job(Str(0))\pointsperbatch = #helpsize
      job(Str(0))\beginrangeX$  = Curve::m_gethex32(*xpoint)
      job(Str(0))\beginrangeY$  = Curve::m_gethex32(*ypoint)
      
      CurNumberPoint = CurNumberPoint + job(Str(0))\totalpoints
      restjob = waletcounter - (jobperthread * totalCPUcout)
      ;PrintN("Rest points: "+Str(restjob))
      
    Else  
      ;PrintN("jobperthread: "+Str(waletcounter)+" items")
      job(Str(0))\beginNumberPoint = CurNumberPoint 
      job(Str(0))\arr = *HelperArr      
      job(Str(0))\totalpoints = waletcounter
      job(Str(0))\pointsperbatch = #helpsize
      job(Str(0))\beginrangeX$  = Curve::m_gethex32(*xpoint)
      job(Str(0))\beginrangeY$  = Curve::m_gethex32(*ypoint)
      
      CurNumberPoint = CurNumberPoint + job(Str(0))\totalpoints      
    EndIf
    
    
    CreateThread(@baby(),0)
    totallaunched+1
    If totalCPUcout>1 And waletcounter>#helpsize
      For i = 1 To totalCPUcout-1
        job(Str(i))\beginNumberPoint = CurNumberPoint
        job(Str(i))\arr = *HelperArr + i * #helpsize * 96        
        job(Str(i))\totalpoints = jobperthread
        job(Str(i))\pointsperbatch = #helpsize
        a$=Hex(jobperthread*i)
        Curve::m_sethex32(*bufferResult, @a$)
        Curve::m_PTMULX64(*addX, *addY, *CurveGX, *CurveGY, *bufferResult,*CurveP)
         Curve::m_ADDPTX64(*addX,*addY,*xpoint, *ypoint,*addX,*addY,*CurveP)
        job(Str(i))\beginrangeX$  = Curve::m_gethex32(*addX)
        job(Str(i))\beginrangeY$  = Curve::m_gethex32(*addY)
        CreateThread(@baby(),i)
        totallaunched+1
        CurNumberPoint = CurNumberPoint + job(Str(i))\totalpoints 
        Delay(500)
      Next i
    EndIf
    Print("    ")
    While totallaunched
      Delay(200)
      infostr$ = " "+StrD(allbabypoint/waletcounter * 100,1)+"%"
      cls$=RSet("",Len(infostr$),Chr(8))
      Print(cls$ + infostr$)
    Wend
    
    If restjob
        a$=Hex(jobperthread*i)
        Curve::m_sethex32(*bufferResult, @a$)
        Curve::m_PTMULX64(*addX, *addY, *CurveGX, *CurveGY, *bufferResult,*CurveP)
        Curve::m_ADDPTX64(*addX,*addY,*xpoint, *ypoint,*addX,*addY,*CurveP)
        job(Str(0))\beginNumberPoint = CurNumberPoint
        job(Str(0))\beginrangeX$  = Curve::m_gethex32(*addX)
        job(Str(0))\beginrangeY$  = Curve::m_gethex32(*addY)
        job(Str(0))\arr = *HelperArr        
        job(Str(0))\totalpoints = restjob
        job(Str(0))\pointsperbatch = #helpsize
        baby(0)
    EndIf     
    PrintN("")
EndProcedure


Procedure GiantcompleteBatchAddWithDouble(*newpointarr,lenline,Yoffset, totalpoints, *apointX, *apointY,  *pointarr, *InvTotal)
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
    Curve::m_mulModX64(*s,*curvInv,*pointerdiff+64,*CurveP, *high)
       
    If Curve::m_check_equilX64(*apointX, *pointer)
      ;px==x
      ;addModP(py, py, x)      
      Curve::m_addModX64(*temp,*apointY,*apointY,*CurveP)
    Else
      ;px!=x
      ;subModP(px, x, x)      
      Curve::m_subModX64(*temp,*apointX,*pointer,*CurveP)
    EndIf
    
    Curve::m_mulModX64(*curvInv,*curvInv,*temp,*CurveP, *high)
    
    If Curve::m_check_equilX64(*apointX, *pointer)
      ;x1==x2
      Curve::m_DBLTX64(*NewpointX,*NewpointY,*apointX,*apointY,*CurveP)
    Else
      ;//slope=(y1-y2)*inverse(x1-x2,p)
      Curve::m_subModX64(*NewpointY,*apointY,*pointer+32,*CurveP)
      Curve::m_mulModX64(*s,*NewpointY,*s,*CurveP, *high)
      ;Rx = s^2 - Gx - Qx =>  pow_mod(slope,2,p)-(x1+x2)
      Curve::m_squareModX64(*NewpointY,*s,*CurveP, *high)
      Curve::m_subModX64(*NewpointY,*NewpointY,*apointX,*CurveP)
      Curve::m_subModX64(*NewpointX,*NewpointY,*pointer,*CurveP)
      ;Ry = s(px - rx) - py
      Curve::m_subModX64(*NewpointY,*apointX,*NewpointX,*CurveP)
      Curve::m_mulModX64(*NewpointY,*NewpointY,*s,*CurveP, *high)
      Curve::m_subModX64(*NewpointY,*NewpointY,*apointY,*CurveP)
      
    EndIf
    ;PrintN("["+Str(totalpoints)+"] x: "+Curve::m_gethex32(*NewpointX))
    ;PrintN("["+Str(totalpoints)+"] y: "+Curve::m_gethex32(*NewpointY))
    
    CopyMemory(*NewpointX,*pointerToNew,32)
    CopyMemory(*NewpointY,*pointerToNew+Yoffset,32)
    
    *pointer-96
    *pointerToNew-lenline
    totalpoints - 1
  Wend
  If totalpoints=0
     If Curve::m_check_equilX64(*apointX, *pointer)
      ;x1==x2
      Curve::m_DBLTX64(*NewpointX,*NewpointY,*apointX,*apointY,*CurveP)
    Else
      ;slope=(y1-y2)*inverse(x1-x2,p)
      Curve::m_subModX64(*NewpointY,*apointY,*pointer+32,*CurveP)
      Curve::m_mulModX64(*curvInv,*NewpointY,*curvInv,*CurveP, *high)
      ;Rx = s^2 - Gx - Qx =>  pow_mod(slope,2,p)-(x1+x2)
      Curve::m_squareModX64(*NewpointY,*curvInv,*CurveP, *high)
      Curve::m_subModX64(*NewpointY,*NewpointY,*apointX,*CurveP)
      Curve::m_subModX64(*NewpointX,*NewpointY,*pointer,*CurveP)
      ;Ry = s(px - rx) - py
      ;Ry = s(px - rx) - py
      Curve::m_subModX64(*NewpointY,*apointX,*NewpointX,*CurveP)
      Curve::m_mulModX64(*NewpointY,*NewpointY,*curvInv,*CurveP, *high)
      Curve::m_subModX64(*NewpointY,*NewpointY,*apointY,*CurveP)
    EndIf
    
    ;PrintN("["+Str(totalpoints)+"] x: "+Curve::m_gethex32(*NewpointX))
    ;PrintN("["+Str(totalpoints)+"] y: "+Curve::m_gethex32(*NewpointY))
    
    CopyMemory(*NewpointX,*pointerToNew,32)
    CopyMemory(*NewpointY,*pointerToNew+Yoffset,32)
  EndIf
  
  FreeMemory(*s)
EndProcedure

Procedure giant(id)
  Protected *my_pubX, *my_pubY, *arr, *newarrr, pointsperbatch, totalpoints, *inv, msg$="Giant #"+Str(id)+"  ", newbatchsz, calculatesz, leadzero, a$, *ptrarr, Yoffset
  Protected *bufferResult, temp$, *addX, *addY , *Rbx, *Rby
  Shared job()
  Shared *CurveGX, *CurveGY, *CurveP

  
  leadzero=32
  *inv = AllocateMemory(256)
  If *inv=0
    PrintN(msg$+"Can`t allocate memory")
    exit("")
  EndIf
  *bufferResult= *inv+32
  *addX= *inv+64
  *addY= *inv+96  
  *my_pubX = *inv+128
  *my_pubY = *inv+160
  *Rbx= *inv+192
  *Rby= *inv+224
  
  *arr = job(Str(id))\arr  
  *newarrr = job(Str(id))\NewPointsArr
  totalpoints = job(Str(id))\totalpoints
  pointsperbatch = job(Str(id))\pointsperbatch
  Yoffset = job(Str(id))\Yoffset
  
  Curve::m_sethex32(*Rbx, @job(Str(id))\beginrangeX$)
  Curve::m_sethex32(*Rby, @job(Str(id))\beginrangeY$)
   Curve::m_sethex32(*my_pubX, @job(Str(id))\beginrangeX$)
   Curve::m_sethex32(*my_pubY, @job(Str(id))\beginrangeY$)
   
  ;PrintN(msg$+"("+Curve::m_gethex32(*my_pubX)+Curve::m_gethex32(*my_pubY)+")")
  
  a$=Hex(pointsperbatch)
  Curve::m_sethex32(*bufferResult, @a$)
  
  Curve::m_PTMULX64(*addX, *addY, *Rbx, *Rby, *bufferResult,*CurveP)
  
  calculatesz=0
  
  
   While calculatesz<totalpoints
    
    CopyMemory(*my_pubX,*newarrr+calculatesz*32,32)
    CopyMemory(*my_pubY,*newarrr+calculatesz*32+Yoffset,32)
    
    If calculatesz<totalpoints
      newbatchsz = pointsperbatch
      If newbatchsz>=(totalpoints-calculatesz)
        newbatchsz = totalpoints-calculatesz
        a$=Hex(newbatchsz)
        Curve::m_sethex32(*bufferResult, @a$)      
        Curve::m_PTMULX64(*addX, *addY, *Rbx, *Rby, *bufferResult,*CurveP)
      EndIf
      
      
      
      
      Curve::beginBatchAdd(*inv, newbatchsz-1, *my_pubX, *my_pubY,  *arr)
      GiantcompleteBatchAddWithDouble(*newarrr+calculatesz*32+32,32,Yoffset, newbatchsz-1, *my_pubX, *my_pubY,  *arr, *inv)
      
      calculatesz  + newbatchsz    
      Curve::m_ADDPTX64(*my_pubX,*my_pubY,*my_pubX, *my_pubY,*addX,*addY,*CurveP)
   EndIf
  Wend
 
  

  FreeMemory(*inv)
EndProcedure

Procedure prntarr(initstr$,*arr, linestotal, lenbytes=32)
  Protected resser.s=Space(lenbytes*4)
  Protected i
  PrintN("**************")
  For i = 0 To linestotal-1
    serialize(*arr+i*lenbytes,0,@resser,lenbytes/2)
    PrintN(initstr$+"["+Str(i)+"]"+PeekS(@resser,lenbytes*2))
  Next i
  PrintN("")
EndProcedure

Procedure.s m_gethex8(*bin)  
  Protected *sertemp=AllocateMemory(16, #PB_Memory_NoClear)
  Protected res$  
  ;************************************************************************
  ;Convert bytes in LITTLE indian format to HEX string in BIG indian format
  ;************************************************************************ 
  Curve::m_serializeX64(*bin,0,*sertemp,2)  
  res$=PeekS(*sertemp,16, #PB_Ascii)
  FreeMemory(*sertemp)
ProcedureReturn res$
EndProcedure

Procedure prntarrBIG(*arr, linestotal, offset=96)
  
  Protected i
  
  For i = 0 To linestotal-1
    
    PrintN("["+Str(i)+"]"+Curve::m_gethex32(*arr+i*offset))
  Next i
  PrintN("")
EndProcedure

Procedure checkGiantArr(*arr, *x,*y, Yoffset, totalpoints, numberofrand=1024)
  Protected res=0, *MyX, *MyY,*bufferResult, a$, randnum
  Shared *CurveP
  *MyX = AllocateMemory(96)
  *MyY = *MyX+32
  *bufferResult= *MyX+64
  If numberofrand<1024
    numberofrand=1024
  EndIf
  CopyMemory(*x,*MyX,32)
  CopyMemory(*y,*MyY,32)
  If Curve::m_check_equilX64(*MyX,*arr)=0 Or  Curve::m_check_equilX64(*MyY,*arr+Yoffset)=0
    PrintN("false")
    PrintN(Curve::m_gethex32(*MyX)  +"-"+Curve::m_gethex32(*arr))
    PrintN(Curve::m_gethex32(*MyY)  +"-"+Curve::m_gethex32(*arr+Yoffset))
    res=1
  EndIf
  numberofrand-1
  
  While numberofrand>0 And res=0
    randnum = Random(totalpoints-1,1)
    a$=Hex(randnum+1)
    Curve::m_sethex32(*bufferResult, @a$)
    Curve::m_PTMULX64(*MyX, *MyY, *x, *y, *bufferResult,*CurveP)
    If Curve::m_check_equilX64(*MyX,*arr+randnum*32)=0 Or  Curve::m_check_equilX64(*MyY,*arr+Yoffset+randnum*32)=0
      PrintN("false")
      PrintN("["+Str(randnum)+"] Est."+Curve::m_gethex32(*MyX)  +"- got"+Curve::m_gethex32(*arr+randnum*32))
      PrintN("["+Str(randnum)+"] Est. "+Curve::m_gethex32(*MyY)  +"- got"+Curve::m_gethex32(*arr+Yoffset+randnum*32))
      res=1
      Break
    EndIf
    numberofrand-1
  Wend
  FreeMemory(*MyX)
  ProcedureReturn res
EndProcedure

Procedure checkBabyArr(*arr, *x,*y, totalpoints, numberofrand=1024)
  Protected res=0, *MyX, *MyY,*bufferResult, a$, randnum
  Shared *CurveP, *CurveGX, *CurveGY
  *MyX = AllocateMemory(96)
  *MyY = *MyX+32
  *bufferResult= *MyX+64
  If numberofrand<1024
    numberofrand=4096
  EndIf
  
  
  While numberofrand>0 And res=0
    randnum = Random(totalpoints-1,1)
    a$=Hex(randnum)
    Curve::m_sethex32(*bufferResult, @a$)
    Curve::m_PTMULX64(*MyX, *MyY, *CurveGX, *CurveGY, *bufferResult,*CurveP)
    ;Curve::m_ADDPTX64(*MyX,*MyY,*x,*y,*MyX,*MyY,*CurveP)
    If check_equil(*MyX+#hashbyteoffset,*arr+randnum*8-8,2)=0
      PrintN("false")
      PrintN("["+Str(randnum)+"] Est."+m_gethex8(*MyX+#hashbyteoffset)  +"- got"+m_gethex8(*arr+randnum*8))     
      res=1
      Break
    EndIf
    numberofrand-1
  Wend
  FreeMemory(*MyX)
  ProcedureReturn res
EndProcedure

Procedure checkSortedArray(*arr,totallines,len=32)
  
  Protected i,rescmp, err
  Protected *min=AllocateMemory(len)
  
  
  ;set zero as min
  FillMemory(*min, len)
  
  For i=0 To totallines-1
    ;0 - s = t, 1- s < t, 2- s > t
    ;PrintN("0x"+getStrfrombin(*arr+i*len,len))
    
    ;get min
    rescmp = m_check_less_more_equilX8(*arr+i*len,*min)
    ;PrintN("rescmp:"+Str(rescmp))
    If rescmp=2;more
      CopyMemory(*arr+i*len, *min, len)    
    Else
      PrintN("Warning!!!")
      PrintN("min set : "+m_gethex8(*min))
      PrintN("  value : "+m_gethex8(*arr+i*len))
      err=i+1
      Break
    EndIf
  Next i
  FreeMemory(*min)
  If err
    PrintN("Value ["+Str(err-1)+"] is Not sorted!!!") 
  EndIf
  ProcedureReturn err   
  
EndProcedure

Procedure findMinMax8(*arr,totallines, *min,*max)
  Protected i,rescmp, err, len=8
  
  CopyMemory(*arr, *min, len)
  FillMemory(*max, len)
  
  For i=1 To totallines-1
    ;0 - s = t, 1- s < t, 2- s > t
    ;PrintN("0x"+getStrfrombin(*arr+i*len))
    
    ;get min
    rescmp = m_check_less_more_equilX8(*arr+i*len,*min)
    ;PrintN("rescmp:"+Str(rescmp))
    If rescmp=1;less
      CopyMemory(*arr+i*len, *min, len)    
    ElseIf rescmp=0
      err=1
      Break
    EndIf
    
    ;get max
    rescmp = m_check_less_more_equilX8(*arr+i*len,*max)
    ;PrintN("rescmp:"+Str(rescmp))
    If rescmp=2;more
      CopyMemory(*arr+i*len, *max, len)
    ElseIf rescmp=0
      err=1
      Break
    EndIf
    
      
  Next i
  If err
    PrintN("Warning!!!") 
  EndIf    
EndProcedure

Procedure foundinarr8(*findvalue, *arr, beginrange, endrange, *res.comparsationStructure)
  Protected temp_beginrange, temp_endrange, rescmp,   exit.b, center
 
  
  temp_beginrange = beginrange
  temp_endrange = endrange

  While (endrange-beginrange)>=0
    If beginrange=endrange
      If endrange<=temp_endrange
        ;0 - s = t, 1- s < t, 2- s > t        
        rescmp = m_check_less_more_equilX8(*findvalue,*arr+beginrange*8)        
        If rescmp=2;more
          *res\pos=-1
          *res\direction=endrange+1
          exit=1
          Break
        ElseIf rescmp=1;less
          If endrange>0
            *res\pos=-1
            *res\direction=endrange
            exit=1
            Break
          Else
            *res\pos=-1
            *res\direction=0
            exit=1
            Break
          EndIf
        Else;equil
         
          *res\pos=beginrange
          *res\direction=0
          exit=1
          Break
        EndIf
      Else
        exit("Unknown exeptions")
      EndIf
    EndIf
    center=(endrange-beginrange)/2+beginrange    
    rescmp = m_check_less_more_equilX8(*findvalue,*arr+center*8)
    If rescmp=2;more
      If (center+1)<=endrange:
        beginrange=center+1
      Else
        beginrange=endrange
      EndIf
    ElseIf rescmp=1;less
      If (center-1)>=beginrange:
        endrange=center-1
      Else
        endrange=beginrange
      EndIf
    Else;equil
      *res\pos=center
      *res\direction=0
      exit=1
      Break
    EndIf
  Wend
  If exit=0
    If beginrange=temp_endrange:
        *res\pos=-1
        *res\direction=1 
    Else
        *res\pos=-1
        *res\direction=-1
    EndIf
  EndIf
EndProcedure




 Procedure isInRange8(*arr,*min,*max)
  Protected rescmp, result
  rescmp = m_check_less_more_equilX8(*arr,*min)   
  If rescmp=2 Or rescmp=0;>=MIN
    rescmp = m_check_less_more_equilX8(*arr,*max)   
    If rescmp=1 Or rescmp=0;<=MAX
      result=1    
    EndIf      
  EndIf
ProcedureReturn result  
EndProcedure

Procedure sortinghashMinMax(id)  
  Protected *arrPointer, *arrPointer_sorted, len=8, msg$="["+Str(id)+"] ", counters,init, persent.d,curprocent.d
  Protected *min=AllocateMemory(len)
  Protected *max=AllocateMemory(len)
  Protected err, i, rescmp, pos
  Protected res.comparsationStructure
  Shared sortjob.sortjobStructure()
  Shared keyMutex, totallaunched
  Shared waletcounter
  
  *arrPointer = sortjob(Str(id))\ptarr
  *arrPointer_sorted = sortjob(Str(id))\sortptarray
  
  CopyMemory(*arrPointer_sorted, *min, len)  
  CopyMemory(*arrPointer_sorted+len, *max, len)
  FillMemory(*arrPointer_sorted, len*2)
  
  counters = sortjob(Str(id))\totallines  
    
  ;PrintN(msg$+"Total points in Source Array: "+Str(totallines))
  ;PrintN(msg$+"MIN: "+getStrfrombin(*min,len))
  ;PrintN(msg$+"MAX: "+getStrfrombin(*max,len))
  
   
  ;PrintN(msg$+"Total items: "+Str(counters))
   
    
  pos=0   
  init=0
  persent=0
  curprocent=0
  ;PrintN(msg$+"Sorting")
  For i=0 To waletcounter-1
    If Not isInRange8(*arrPointer+i*len,*min,*max)
      ;skip values that is out of range
      Continue
    EndIf
    If init=0
      ;initial
      CopyMemory(*arrPointer+i*len, *arrPointer_sorted+pos*len, len)
      ;PrintN(msg$+"II: "+getStrfrombin(*arrPointer_sorted+pos*len))
      init=1
      Continue
    EndIf  
    
    curprocent = pos*100/counters
    If curprocent-persent>0.5
      persent = curprocent
      sortjob(Str(id))\curpos = pos
      ;PrintN(msg$+"Sorting:"+Str(persent)+"%")
    EndIf
    
    ;PrintN(msg$+">>: "+getStrfrombin(*arrPointer+i*len))
    foundinarr8(*arrPointer+i*len, *arrPointer_sorted, 0, pos, @res.comparsationStructure)
    ;PrintN(msg$+"("+res\pos+","+res\direction+")")
    If res\pos=-1
      ;that mean that value is Not found in range
      If res\direction>pos
        pos=res\direction
        CopyMemory(*arrPointer+i*len, *arrPointer_sorted+pos*len, len)
      Else
        ;move block forward
        ;PrintN(msg$+"move block")
        pos+1
        CopyMemory(*arrPointer_sorted+res\direction*len, *arrPointer_sorted+res\direction*len+len, (pos-res\direction)*len)
        CopyMemory(*arrPointer+i*len, *arrPointer_sorted+res\direction*len, len)
      EndIf
    Else
      PrintN(msg$+"Warning!!!>"+m_gethex8(*arrPointer+i*len))
    EndIf
    ;prntarr("",sortjob(Str(id))\sortptarray, counters)
  Next i
  
    
  FreeMemory(*min)
  FreeMemory(*max)
  LockMutex(keyMutex)
    totallaunched-1
    ;PrintN("Sort thread id:"+Str(id)+" ended")
  UnlockMutex(keyMutex)
EndProcedure


Procedure Writeint(*Aptr, idx.i, blockDim.w, threadDim.w,  *targPtr)
  Protected *initAptr, threadIdx, blockIdx.i = 0, threadtotal.i, base.i, threadId.i, index.i, threadtotal64.i, temp.i
  Shared pparam
  ; ЭТО ПРАВИЛЬНЫЙ ВАРИАНТ
  ;PrintN("IDX:"+Str(idx))
  
  *initAptr = *Aptr
  
  blockIdx = idx / (threadDim*pparam)
  threadIdx = (idx -blockIdx * threadDim*pparam)/pparam
  
  idx = idx - (threadIdx * pparam + blockIdx*threadDim*pparam)
  
  ;PrintN( "ThreadID>"+Str(threadIdx))
  ;PrintN( "BlockID>"+Str(blockIdx))
  ;PrintN("idx local for thread>"+Str(idx))
 
 threadtotal.i = threadDim * blockDim
 base.i = idx *  threadtotal * 8
 
 threadId.i = threadIdx
 temp.i = blockIdx * threadDim 
 threadId.i = temp.i + threadId 
 
 index.i = base + threadId
 ;Debug "index: "+Str(index) 
 threadtotal64.i = index * 4
 
 
 *Aptr = *Aptr + threadtotal64
 
 threadtotal64.i = threadtotal * 4
 
 CopyMemory(*targPtr, *Aptr, 4)
 
 ;Debug "[0] offest:"+Str(*Aptr-*initAptr)+" "+Hex(PeekL(*Aptr))
 *Aptr = *Aptr + threadtotal64
 
 
 CopyMemory(*targPtr+4, *Aptr, 4)
 ;Debug "[1] offest:"+Str(*Aptr-*initAptr)+" "+Hex(PeekL(*Aptr))
 *Aptr = *Aptr + threadtotal64
 
 
 CopyMemory(*targPtr+8, *Aptr, 4)
 ;Debug "[2] offest:"+Str(*Aptr-*initAptr)+" "+Hex(PeekL(*Aptr))
 *Aptr = *Aptr + threadtotal64
 
 
 CopyMemory(*targPtr+12, *Aptr, 4)
 ;Debug "[3] offest:"+Str(*Aptr-*initAptr)+" "+Hex(PeekL(*Aptr))
 *Aptr = *Aptr + threadtotal64
 
 
 CopyMemory(*targPtr+16, *Aptr, 4)
 ;Debug "[4] offest:"+Str(*Aptr-*initAptr)+" "+Hex(PeekL(*Aptr))
 *Aptr = *Aptr + threadtotal64
 
 
 CopyMemory(*targPtr+20, *Aptr, 4)
 ;Debug "[5] offest:"+Str(*Aptr-*initAptr)+" "+Hex(PeekL(*Aptr))
 *Aptr = *Aptr + threadtotal64
 
 
 CopyMemory(*targPtr+24, *Aptr, 4)
 ;Debug "[6] offest:"+Str(*Aptr-*initAptr)+" "+Hex(PeekL(*Aptr))
 *Aptr = *Aptr + threadtotal64
 
 
 CopyMemory(*targPtr+28, *Aptr, 4)
 ;Debug "[7] offest:"+Str(*Aptr-*initAptr)+" "+Hex(PeekL(*Aptr))
 ;Debug "-------------"
EndProcedure

Procedure Save_Load_Giants()
  Protected i,j, filebinname$, full_size, len=8, hash.s, *pp, counters, totalpos, jobcomplete.d, prejobcomplete.d, wrbytes, savedbytes, maxsavebytes, loadedbytes
  Protected totalloadbytes, maxloadbytes, *temper, Yoffset, w$
  Shared *HelperArr, *GiantArr, *GiantArrPacked, blocktotal, threadtotal, pparam, ADDPUBG, maxnonce, job(), waletcounter

  
    
  filebinname$=Str(threadtotal)+"_"+Str(blocktotal)+"_"+Str(pparam)+"_"+Str(waletcounter)+"_g2.BIN"
  full_size=maxnonce * 64
  
  If FileSize(filebinname$) = -1
    PrintN("Generate Giants Buffer: "+Str(maxnonce)+" items")
    ; file does not exist    
    *GiantArrPacked=AllocateMemory((maxnonce+1)*64)
    If *GiantArrPacked=0
      PrintN("Can`t allocate memory for giantpacked array")
      exit("")
    EndIf
    
    *GiantArr=AllocateMemory((maxnonce+1)*64)
    If *GiantArr=0
      PrintN("Can`t allocate memory for giant array")
      exit("")
    EndIf

    Curve::fillarrayN(*HelperArr , #helpsize, ADDPUBG\x, ADDPUBG\y)
    ;prntarrBIG(*HelperArr, 16)
    
    job(Str(0))\arr = *HelperArr
    job(Str(0))\NewPointsArr = *GiantArr
    job(Str(0))\totalpoints = maxnonce
    job(Str(0))\pointsperbatch = #helpsize
    job(Str(0))\beginrangeX$  = Curve::m_gethex32(ADDPUBG\x)
    job(Str(0))\beginrangeY$  = Curve::m_gethex32(ADDPUBG\y)
    job(Str(0))\Yoffset = maxnonce * 32
    giant(0)
    
    Print("Verify giant array...")
    If checkGiantArr(*GiantArr, ADDPUBG\x,ADDPUBG\y, maxnonce * 32, maxnonce, maxnonce/65536)=1
      
      exit("")
    Else
      PrintN("ok")
    EndIf

    Print("Prepear Giant Buffer for GPU using...")
    *temper = AllocateMemory(64)
    If *temper=0
      PrintN("Can`t allocate memory")
      exit("")
    EndIf
    Yoffset = maxnonce * 32
    For i =  0 To maxnonce - 1
      w$ = Curve::m_gethex32(*GiantArr+i*32)+Curve::m_gethex32(*GiantArr+i*32+Yoffset)
      
      ;PrintN(w$)
      deserialize(*temper,0,@w$,32)
      
      
      Writeint(*GiantArrPacked, i, blocktotal, threadtotal, *temper)
      
      Writeint(*GiantArrPacked+Yoffset, i, blocktotal, threadtotal, *temper+32)
    Next i
    PrintN("ok")
    Print("Convert BigIntegers to 32b...")
    For i = 0 To (maxnonce*64)/8-1
      toLittleInd32(*GiantArrPacked+i*8) ;смещаем, т.к. первые 32б системные и их конвертить не надо
    Next 
    PrintN("ok")
    
    RemoveGiantArrTemp() 
    ;Saving BIN FILE
    PrintN("Save BIN file:"+filebinname$)
    savedbytes=0
    maxsavebytes=full_size
    If full_size>#GB
      maxsavebytes = #GB
    EndIf
    *pp=*GiantArrPacked
    
    If CreateFile(0, filebinname$,#PB_File_NoBuffering)           ; we create a new text file...
      i=0
      Repeat
      PrintN("["+Str(i)+"] chunk:"+Str(maxsavebytes)+"b")
      wrbytes =WriteData(0, *pp, maxsavebytes) 
      savedbytes + maxsavebytes
      
      If maxsavebytes<>wrbytes
        Print("Error when saving: save:"+Str(maxsavebytes)+"b, got:"+Str(wrbytes)+"b")
        CloseFile(0)
        exit("")
      EndIf
      
      *pp+maxsavebytes
      
      If savedbytes<full_size
        If savedbytes+maxsavebytes>full_size
          maxsavebytes = full_size-savedbytes
          PrintN("Last chunk:"+Str(maxsavebytes)+"b")
        EndIf
        
      EndIf
      i+1
      Until savedbytes>=full_size
      CloseFile(0) 
      PrintN("Saved:"+Str(savedbytes)+" bytes")
    Else
      Debug "May not create the file!"
    EndIf
    
    FreeMemory(*GiantArrPacked)
  Else
    PrintN("Giant file exist")
  EndIf
  If *temper
    FreeMemory(*temper)
  EndIf
EndProcedure

Procedure findsolution(*solution, *arr, linestotal, len=16)
  Protected result=-1, i
  For i =0 To linestotal-1
    If check_equil(*solution,*arr+i*len,len/4)
      result=i
      Break
    EndIf
  Next i
ProcedureReturn result
EndProcedure

Procedure.s getStrfrombin(*pointlocation,lenbytes=32)
  Protected resser.s=Space(lenbytes*4)
  serialize(*pointlocation,0,@resser,lenbytes/2)
  ProcedureReturn PeekS(@resser,lenbytes*2)
EndProcedure


Procedure HashTableInsert(*hash, position)  
  Protected offset, hashcut, val, sz, *contentpointer
  Shared TableMutex, *PointerTable, HT_mask, HT_total_hashes, HT_items_with_collisions, HT_max_collisions, HT_total_items, initHTsize
  #addsz = 1
  ;PrintN("hash insert>"+m_gethex8(*hash))
  
  hashcut = ValueL(*hash) & HT_mask 
  
  
  LockMutex(TableMutex)
  offset = hashcut*#Pointersz
  *contentpointer = PeekI(*PointerTable+offset) 
  If *contentpointer
    sz = MemorySize(*contentpointer)/#HashTableSizeItems
  Else
    sz = 0
  EndIf
  
  If sz = 0
     
    *contentpointer = AllocateMemory(#HashTableSizeItems * initHTsize)    
    If Not *contentpointer     
      exit("Can`t allocate memory")
    EndIf
    ;PrintN("Hash #"+Hex(hashcut)+" "+Str(*contentpointer))   
    ;store new pointer to PointTable
    PokeI(*PointerTable+offset, *contentpointer) 
    ;store part of hash
    CopyMemory(*hash+4, *contentpointer, #HashTableSizeHash)
    PokeL(*contentpointer+#HashTableSizeHash,position)   
    HT_total_hashes + 1
  Else
    ;PrintN("Hash #"+Hex(hashcut)+" has "+Str(sz)+" items")
    ;PrintN("Need realocate")
    *contentpointer = PeekI(*PointerTable+offset)
    If sz=initHTsize      
      *contentpointer = ReAllocateMemory(*contentpointer, (sz+#addsz)*#HashTableSizeItems, #PB_Memory_NoClear)      
        If Not *contentpointer     
          exit("Can`t reallocate memory")
        EndIf      
        ;store new pointer to PointTable
        PokeI(*PointerTable+offset, *contentpointer)      
    EndIf
    
    If sz>initHTsize
      If (sz-initHTsize)%#addsz=0
        *contentpointer = ReAllocateMemory(*contentpointer, (sz+#addsz)*#HashTableSizeItems, #PB_Memory_NoClear)        
        If Not *contentpointer     
          exit("Can`t reallocate memory")
        EndIf      
        ;store new pointer to PointTable
        PokeI(*PointerTable+offset, *contentpointer) 
      EndIf 
    EndIf
    CopyMemory(*hash+4, *contentpointer+ sz*#HashTableSizeItems, #HashTableSizeHash)
    PokeL(*contentpointer+ sz*#HashTableSizeItems+#HashTableSizeHash,position)
    sz+1
    HT_items_with_collisions + 1
    If sz>HT_max_collisions
      HT_max_collisions = sz
    EndIf
  EndIf
  
  
  HT_total_items+1  
  UnlockMutex(TableMutex)
 
EndProcedure

Procedure HashTableRead(*hash, *res.HashTableResultStructure)  
  Protected offset, hashcut, *contentpointer
  Shared  *PointerTable, HT_mask
  
  hashcut = ValueL(*hash) & HT_mask 
  offset = hashcut * #Pointersz 
  *contentpointer = PeekI(*PointerTable+offset) 
  If *contentpointer
    *res\size = MemorySize(*contentpointer)/#HashTableSizeItems
  Else
    *res\size = 0
  EndIf  
   
  *res\contentpointer = *contentpointer
  ;PrintN("Hash:" +Str(hashcut)+" sz:"+Str(*res\size))
EndProcedure

Procedure HashTableSammary() 
  Protected totalbytes
  Shared HT_total_items, HT_total_hashes, HT_max_collisions, HT_items_with_collisions, HT_mask, HT_items, HT_POW
  PrintN("----------HashTable Info----------")
  PrintN("Table size: 2^"+Str(HT_POW))
  PrintN("Table mask: "+Hex(HT_mask))
  PrintN("Table used: "+StrD(HT_total_hashes*100/HT_items,2)+"%")
  PrintN("Total unique hashes: "+Str(HT_total_hashes)+" = "+StrD(HT_total_hashes*100/HT_total_items,1)+"%")
  PrintN("Total hashes: "+Str(HT_total_items)+"="+Str(HT_total_items)+"x"+Str(#HashTableSizeItems)+"="+StrD((HT_total_items * #HashTableSizeItems)/#MB)+" MB")
  totalbytes = HT_total_items*#HashTableSizeItems + HT_items * #Pointersz+ HT_items * 8 
  PrintN("Total "+Str(totalbytes)+" bytes = "+StrD(totalbytes/#MB,1)+"MB")
  PrintN("Total colisions:"+Str(HT_items_with_collisions)+" = "+StrD(HT_items_with_collisions*100/HT_total_items,1)+"%")  
  PrintN("Max. colisions:"+Str(HT_max_collisions))
  PrintN("----------------------------------")
  
EndProcedure


Procedure check_LME32bit(*s,*t)
  !mov rsi,[p.p_s]  
  !mov rdi,[p.p_t]
    
  !mov eax,[rsi]
  !cmp eax,[rdi]
  !jb llm_LME32bit_exit_less
  !ja llm_LME32bit_exit_more  
   
  !xor eax,eax
  !jmp llm_LME32bit_exit  
  
  !llm_LME32bit_exit_more:
  !mov eax,2
  !jmp llm_LME32bit_exit  
  
  !llm_LME32bit_exit_less:
  !mov eax,1
  !llm_LME32bit_exit:
ProcedureReturn  
EndProcedure

Procedure findInHashTable32bit(*findvalue, *arr, beginrange, endrange, *res.comparsationStructure)
  Protected temp_beginrange, temp_endrange, rescmp,   exit.b, center
  
  temp_beginrange = beginrange
  temp_endrange = endrange

  While (endrange-beginrange)>=0
    If beginrange=endrange
      If endrange<=temp_endrange
        ;0 - s = t, 1- s < t, 2- s > t
        rescmp = check_LME32bit(*findvalue,*arr + beginrange * #HashTableSizeItems)
        ;Debug "cmp "+get64bithash(*findvalue)+" - "+get64bithash(*arr + beginrange * #HashTableSizeItems)+" = "+Str(rescmp)
        If rescmp=2;more
          *res\pos=-1
          *res\direction=endrange+1
          exit=1
          Break
        ElseIf rescmp=1;less
          If endrange>0
            *res\pos=-1
            *res\direction=endrange
            exit=1
            Break
          Else
            *res\pos=-1
            *res\direction=0
            exit=1
            Break
          EndIf
        Else;equil
          *res\pos=beginrange
          *res\direction=0
          exit=1
          Break
        EndIf
      Else
        Debug("Unknown exeptions")        
      EndIf
    EndIf
    center=(endrange-beginrange)/2+beginrange    
    rescmp = check_LME32bit(*findvalue,*arr + center * #HashTableSizeItems)
    ;Debug "cmp "+get64bithash(*findvalue)+" - "+get64bithash(*arr + beginrange * #HashTableSizeItems)+" = "+Str(rescmp)
    If rescmp=2;more
      If (center+1)<=endrange:
        beginrange=center+1
      Else
        beginrange=endrange
      EndIf
    ElseIf rescmp=1;less
      If (center-1)>=beginrange:
        endrange=center-1
      Else
        endrange=beginrange
      EndIf
    Else;equil
      *res\pos=center
      *res\direction=0
      exit=1
      Break
    EndIf
  Wend
  If exit=0
    If beginrange=temp_endrange:
        *res\pos=-1
        *res\direction=1 
    Else
        *res\pos=-1
        *res\direction=-1
    EndIf
  EndIf
EndProcedure

Procedure findInHashTable32bitSimple(*findvalue, *arr, beginrange, endrange, *res.comparsationStructure)
  Protected temp_beginrange, temp_endrange, rescmp,   exit.b, center
  
  *res\pos=-1
  While endrange>=beginrange   
    center= beginrange+(endrange-beginrange)/2
    rescmp = check_LME32bit(*findvalue,*arr + center * #HashTableSizeItems)   
    If rescmp=2;more
      beginrange=center+1
    ElseIf rescmp=1;less
      endrange=center-1
    Else ;equil
      *res\pos=center
      Break
    EndIf     
  Wend 
EndProcedure

Procedure sortHashTable32bit(*arr, totalines)
  Protected err, i, rescmp,*temp, *INShash,pos, res.comparsationStructure
  *temp=AllocateMemory(#HashTableSizeItems)
  Shared TableMutex
  
  pos = 0  
  LockMutex(TableMutex)
  While pos<totalines-1 
      *INShash = *arr+(pos+1) * #HashTableSizeItems
      findInHashTable32bit(*INShash, *arr, 0, pos, @res.comparsationStructure)
      ;Debug "pos:"+Str(pos)
      ;Debug get64bithash(*INShash)
      ;Debug "res\pos:"+Str(res\pos)+"res\dir:"+Str(res\direction)
      If res\pos=-1
        ;that mean that value is Not found in range
        If res\direction>pos
          pos=res\direction
          CopyMemory(*INShash, *arr + pos * #HashTableSizeItems, #HashTableSizeItems)       
        Else
          ;move block forward
          ;PrintN("move block")
          pos+1
          CopyMemory(*INShash, *temp, #HashTableSizeItems)
          CopyMemory(*arr + res\direction * #HashTableSizeItems, *arr + res\direction * #HashTableSizeItems + #HashTableSizeItems, (pos-res\direction) * #HashTableSizeItems)
          CopyMemory(*temp, *arr + res\direction * #HashTableSizeItems, #HashTableSizeItems)        
        EndIf
      Else
        err=1
        pos+1
        PrintN("Value exist!!!>"+Hex(Valuel(*INShash))+" ("+Hex(Valuel(*INShash+#HashTableSizeHash))+")")
        PrintN("Value exist!!!>"+Hex(Valuel(*arr + res\pos * #HashTableSizeItems))+" ("+Hex(Valuel(*arr + res\pos * #HashTableSizeItems+#HashTableSizeHash))+")")
        CopyMemory(*INShash, *temp, #HashTableSizeItems)
        CopyMemory(*arr + res\pos * #HashTableSizeItems, *arr + res\pos * #HashTableSizeItems + #HashTableSizeItems, (pos-res\pos) * #HashTableSizeItems)
        CopyMemory(*temp, *arr + res\pos * #HashTableSizeItems, #HashTableSizeItems)
      EndIf
      ;For i =0 To totalines-1
        ;Debug ("["+Str(i)+"] "+get64bithash(*arr + i * #HashTableSizeItems))  
      ;Next i
  Wend
  UnlockMutex(TableMutex)
  If err   
    ;PrintN("Value exist!!!>"+Hex(Valuel(*INShash)))
    ;For i =0 To totalines-1
      ;PrintN ("["+Str(i)+"] "+Hex(Valuel(*arr + i * #HashTableSizeItems))+" ("+Hex(Valuel(*arr + i * #HashTableSizeItems+#HashTableSizeHash))+")")  
    ;Next i
    ;exit("Try increase -htsz")
  EndIf
  FreeMemory(*temp)
  ProcedureReturn err
EndProcedure 

Procedure sortWholeHashTable(threadId)
  Protected i, res.HashTableResultStructure, totalitems
  Shared job(), allbabypoint, totallaunched, JobMutex
  
  totalitems = job(Str(threadId))\beginNumberPoint + job(Str(threadId))\totalpoints
  
  For i =job(Str(threadId))\beginNumberPoint To totalitems-1 
    HashTableRead(@i, @res) 
    If res\size    
      If sortHashTable32bit(res\contentpointer, res\size)
        ; mean can`t sort
        
      EndIf
    EndIf     
  Next i
  LockMutex(JobMutex)
  Print(" #"+Str(threadId))
  totallaunched-1 
  UnlockMutex(JobMutex)
EndProcedure

Procedure sortWholeHashTableThreaded(totalitems)
 Protected totalCPUcout, jobperthread, CurNumberPoint, restjob, i
  Shared job() , totallaunched, JobMutex
  
    totallaunched = 0
    totalCPUcout = CountCPUs(#PB_System_ProcessCPUs)
    If totalCPUcout>1 And totalitems>#MB       
      CurNumberPoint = 0
      jobperthread = totalitems/totalCPUcout
      ;PrintN("jobperthread: "+Str(jobperthread)+" items")
      job(Str(0))\beginNumberPoint = CurNumberPoint           
      job(Str(0))\totalpoints = jobperthread    
      CurNumberPoint = CurNumberPoint + job(Str(0))\totalpoints
      restjob = totalitems - (jobperthread * totalCPUcout)
      ;PrintN("Rest points: "+Str(restjob))
      
    Else  
      ;PrintN("jobperthread: "+Str(waletcounter)+" items")
      job(Str(0))\beginNumberPoint = CurNumberPoint           
      job(Str(0))\totalpoints = totalitems      
      CurNumberPoint = CurNumberPoint + job(Str(0))\totalpoints      
    EndIf
    LockMutex(JobMutex)
    totallaunched+1
    UnlockMutex(JobMutex)
    CreateThread(@sortWholeHashTable(),0)
    
    If totalCPUcout>1 And totalitems>#MB       
      For i = 1 To totalCPUcout-1
        job(Str(i))\beginNumberPoint = CurNumberPoint        
        job(Str(i))\totalpoints = jobperthread
        LockMutex(JobMutex)
        totallaunched+1
        UnlockMutex(JobMutex)
        CreateThread(@sortWholeHashTable(),i)
        
        CurNumberPoint = CurNumberPoint + job(Str(i))\totalpoints 
        Delay(500)
      Next i
    EndIf
    
    While totallaunched
      Delay(200)      
    Wend   
    Print(" rest")
    If restjob        
        job(Str(0))\beginNumberPoint = CurNumberPoint          
        job(Str(0))\totalpoints = restjob        
        sortWholeHashTable(0)
    EndIf     
    
    
EndProcedure


Procedure checkHashTableContent(*arr, contentsz)
  
  Protected i,rescmp, err
  Protected *min=AllocateMemory(#HashTableSizeHash)
  Shared TableMutex
  
  ;set zero as min
  FillMemory(*min, #HashTableSizeHash)
  
  For i=0 To contentsz-1   
   
    rescmp = check_LME32bit(*arr + i * #HashTableSizeItems,*min)    
    If rescmp=2;more
      CopyMemory(*arr + i * #HashTableSizeItems, *min, #HashTableSizeHash)    
    ElseIf rescmp=0 And Valuel(*arr + i * #HashTableSizeItems)<>0
      PrintN("Warning !!! Same value founded:"+ Hex(Valuel(*min)))
    ElseIf i>0
      
        PrintN("Warning!!!")
        PrintN("min set : "+Hex(ValueL(*min)))
        PrintN("value at pos["+Str(i)+"] : "+Hex(ValueL(*arr + i * #HashTableSizeItems)))
        err=i
        Break
      
    EndIf
  Next i
  FreeMemory(*min)
  If err
    PrintN("Values ("+Str(err)+") is Not sorted!!!")
    ProcedureReturn 1
  Else
    ProcedureReturn 0 ; no error
  EndIf 
  
EndProcedure

Procedure checkWholeHashTableContent(threadId)
  Protected  res.HashTableResultStructure, i, err, totalitems
  Shared job() ,totallaunched, checkerror, JobMutex
  
  totalitems = job(Str(threadId))\beginNumberPoint + job(Str(threadId))\totalpoints
  For i =job(Str(threadId))\beginNumberPoint To totalitems-1 
    HashTableRead(@i, @res) 
    If res\size>1
      If checkHashTableContent(res\contentpointer, res\size)  
        ;if error
        err=1
        Break
      EndIf
    EndIf
  Next i
  
  If err
    PrintN("false") 
    checkerror + 1
  EndIf
 LockMutex(JobMutex)
 Print(" #"+Str(threadId))
 totallaunched-1
 UnlockMutex(JobMutex)
EndProcedure

Procedure checkWholeHashTableContenThreaded()
  Protected totalCPUcout, jobperthread, CurNumberPoint, restjob, i, totalitems
  Shared job() , totallaunched, HT_items, checkerror , JobMutex 
  
    checkerror = 0
    totalitems = HT_items
    totallaunched = 0
    totalCPUcout = CountCPUs(#PB_System_ProcessCPUs)
    If totalCPUcout>1 And totalitems>#MB       
      CurNumberPoint = 0
      jobperthread = totalitems/totalCPUcout
      ;PrintN("jobperthread: "+Str(jobperthread)+" items")
      job(Str(0))\beginNumberPoint = CurNumberPoint           
      job(Str(0))\totalpoints = jobperthread    
      CurNumberPoint = CurNumberPoint + job(Str(0))\totalpoints
      restjob = totalitems - (jobperthread * totalCPUcout)
      ;PrintN("Rest points: "+Str(restjob))
      
    Else  
      ;PrintN("jobperthread: "+Str(waletcounter)+" items")
      job(Str(0))\beginNumberPoint = CurNumberPoint           
      job(Str(0))\totalpoints = totalitems      
      CurNumberPoint = CurNumberPoint + job(Str(0))\totalpoints      
    EndIf
    LockMutex(JobMutex)
    totallaunched+1
    UnlockMutex(JobMutex)
    CreateThread(@checkWholeHashTableContent(),0)
    
    If totalCPUcout>1 And totalitems>#MB       
      For i = 1 To totalCPUcout-1
        job(Str(i))\beginNumberPoint = CurNumberPoint        
        job(Str(i))\totalpoints = jobperthread
        LockMutex(JobMutex)
        totallaunched+1
        UnlockMutex(JobMutex)
        CreateThread(@checkWholeHashTableContent(),i)
        
        CurNumberPoint = CurNumberPoint + job(Str(i))\totalpoints 
        Delay(500)
      Next i
    EndIf
    
    While totallaunched
      Delay(200)      
    Wend   
    Print(" rest")
    If restjob        
        job(Str(0))\beginNumberPoint = CurNumberPoint          
        job(Str(0))\totalpoints = restjob        
        checkWholeHashTableContent(0)
    EndIf     
    
    ProcedureReturn checkerror
  EndProcedure
  
Procedure compareWithHashtable(*hash, startpos=0)
  Protected  res.HashTableResultStructure, rescmp.comparsationStructure
  rescmp\pos=-1
  HashTableRead(*hash, @res) 
  ;PrintN( "cmp hash:"+m_gethex8(*hash))
  ;PrintN( "sz:"+Str(res\size))
  If res\size And startpos<=res\size    
    findInHashTable32bitSimple(*hash+4, res\contentpointer, startpos, res\size, @rescmp)
    ;PrintN("Content pos: "+Str(rescmp\pos))    
  EndIf
  ProcedureReturn rescmp\pos
EndProcedure

Procedure ReadHTpack(*hash, *arr, *res.HashTableResultStructure)  
  Protected offset, hash
  Shared HT_mask, HT_items
  
  hash = ValueL(*hash) & HT_mask   
  *res\size = ValueL(*arr + hash * 4 + 4) - ValueL(*arr + hash * 4)  
  *res\contentpointer =*arr + HT_items * 4 + 4 + ValueL(*arr + hash * 4) * #HashTableSizeItems
  ;PrintN("Hash:" +Str(hash)+" sz:"+Str(*res\size))
EndProcedure

Procedure compareHTpack(*hash, *rescmp.comparsationStructure, startpos=0)
  Protected  res.HashTableResultStructure
  Shared *GpuHT
  *rescmp\pos=-1
  ReadHTpack(*hash, *GpuHT, @res) 
  ;PrintN( "cmp hash:"+m_gethex8(*hash))
  ;PrintN( "sz:"+Str(res\size))
  If res\size And startpos<=res\size    
    findInHashTable32bitSimple(*hash+4, res\contentpointer, startpos, res\size, *rescmp)
    ;PrintN("Content pos: "+Str(rescmp\pos)+", "+Str(ValueL(res\contentpointer+rescmp\pos*#HashTableSizeItems+#HashTableSizeHash))) 
    If *rescmp\pos<>-1
      *rescmp\direction = *rescmp\pos
      *rescmp\pos=ValueL(res\contentpointer+*rescmp\pos*#HashTableSizeItems+#HashTableSizeHash)
    EndIf
  EndIf
  
EndProcedure

Procedure packHTFile(filebinname$)   
  Protected i, hash, res.HashTableResultStructure, offset, counter, filecounter, *contentpointer, fileoffset, *ptr
  Shared *PointerTable, HT_items, HT_total_items , HT_mask, waletcounter,  *CurveGX
  ;for CPU we store xpoint position and xpoint  
  *ptr = AllocateMemory(512*#MB)
  fileoffset = HT_items * 4 + 4
  counter=0  
  filecounter=0
  ;save packed htcpu to file  
  If CreateFile(#binfile, filebinname$, #PB_File_NoBuffering)
    FileSeek(#binfile, fileoffset)
    For i =0 To HT_items-1 
      hash = ValueL(@i) & HT_mask 
      offset = hash * #Pointersz
      *contentpointer = PeekI(*PointerTable+offset) 
      If *contentpointer
        res\size = MemorySize(*contentpointer)/#HashTableSizeItems
      Else
        res\size = 0
      EndIf  
      
      If res\size>0         
        res\contentpointer = *contentpointer        
        If (filecounter * #HashTableSizeItems + res\size * #HashTableSizeItems)<512*#MB
          CopyMemory(res\contentpointer, *ptr + filecounter * #HashTableSizeItems,  res\size*#HashTableSizeItems)
        Else
          ;save buffer to file
          Print(" "+StrD(filecounter * #HashTableSizeItems /#MB,2)+"MB")
          If WriteData(#binfile, *ptr, filecounter * #HashTableSizeItems) <> filecounter * #HashTableSizeItems    
            exit("Can`t save "+Str(res\size*#HashTableSizeItems)+"b")
          EndIf 
          filecounter=0
          CopyMemory(res\contentpointer, *ptr + filecounter * #HashTableSizeItems,  res\size*#HashTableSizeItems)
        EndIf
        
        counter+res\size  
        filecounter+res\size  
      EndIf
    Next i    
    If filecounter>0
      ;save rest to file
      Print(" "+StrD(filecounter * #HashTableSizeItems /#MB,2)+"MB")
      If WriteData(#binfile, *ptr, filecounter * #HashTableSizeItems) <> filecounter * #HashTableSizeItems    
        exit("Can`t save "+Str(filecounter * #HashTableSizeItems)+"b")
      EndIf
    EndIf 
    
    Print(" +")
    counter=0
    filecounter=0
    FileSeek(#binfile, 0 )
    For i =0 To HT_items-1 
      hash = ValueL(@i) & HT_mask 
      offset = hash * #Pointersz
      *contentpointer = PeekI(*PointerTable+offset) 
      If *contentpointer
        res\size = MemorySize(*contentpointer)/#HashTableSizeItems
      Else
        res\size = 0
      EndIf 
      
      If (filecounter * 4 + 4)<512*#MB        
        If res\size>0         
          PokeL (*ptr + filecounter * 4, counter)       
          counter+res\size
        Else       
          PokeL (*ptr + filecounter * 4, counter)          
        EndIf 
        filecounter+1
      Else
        Print(" "+StrD(filecounter * 4 /#MB,2)+"MB")
        If WriteData(#binfile, *ptr, filecounter * 4) <> filecounter * 4    
          exit("Can`t save "+Str(filecounter * 4)+"b")
        EndIf 
        filecounter=0
        If res\size>0         
          PokeL (*ptr + filecounter * 4, counter)       
          counter+res\size
        Else       
          PokeL (*ptr + filecounter * 4, counter)          
        EndIf 
        filecounter+1
      EndIf
      
      
    Next i   
    If filecounter>0
      ;save rest to file
      Print(" "+StrD(filecounter * 4 /#MB,2)+"MB")
      If WriteData(#binfile, *ptr, filecounter * 4) <> filecounter * 4    
        exit("Can`t save "+Str(filecounter * 4)+"b")
      EndIf
    EndIf 
    
    If Not WriteLong(#binfile, counter)
      exit("Can`t save 8b")
    EndIf 
    CloseFile(#binfile)
  Else
      exit("Can`t create "+filebinname$)
  EndIf
    
  FreeMemory(*ptr)
EndProcedure

Procedure packHTGPUFile(filebinname$)   
    Protected i, j,  hash, res.HashTableResultStructure, offset, counter, filecounter, *contentpointer, fileoffset, *ptr
  Shared *PointerTable, HT_items, HT_total_items , HT_mask, waletcounter,  *CurveGX
  ;for CPU we store xpoint position and xpoint  
  *ptr = AllocateMemory(512*#MB)
  fileoffset = HT_items * 4 + 4
  counter=0  
  filecounter=0
  ;save packed htcpu to file  
  If CreateFile(#binfile, filebinname$, #PB_File_NoBuffering)
    FileSeek(#binfile, fileoffset)
    For i =0 To HT_items-1 
      hash = ValueL(@i) & HT_mask 
      offset = hash * #Pointersz
      *contentpointer = PeekI(*PointerTable+offset) 
      If *contentpointer
        res\size = MemorySize(*contentpointer)/#HashTableSizeItems
      Else
        res\size = 0
      EndIf  
      
      If res\size>0         
        res\contentpointer = *contentpointer        
        If (filecounter * #HashTableSizeHash + res\size * #HashTableSizeHash)<512*#MB
          For j = 0 To res\size-1
            CopyMemory(res\contentpointer + j*#HashTableSizeItems, *ptr + filecounter * #HashTableSizeHash + j*#HashTableSizeHash,  #HashTableSizeHash)
          Next j
        Else
          ;save buffer to file
          Print(" "+StrD(filecounter * #HashTableSizeHash /#MB,2)+"MB")
          If WriteData(#binfile, *ptr, filecounter * #HashTableSizeHash) <> filecounter * #HashTableSizeHash    
            exit("Can`t save "+Str(filecounter * #HashTableSizeHash)+"b")
          EndIf 
          filecounter=0
          For j = 0 To res\size-1
            CopyMemory(res\contentpointer + j*#HashTableSizeItems, *ptr + filecounter * #HashTableSizeHash + j*#HashTableSizeHash,  #HashTableSizeHash)
          Next j
        EndIf
        
        counter+res\size  
        filecounter+res\size  
      EndIf
    Next i    
    If filecounter>0
      ;save rest to file
      Print(" "+StrD(filecounter * #HashTableSizeHash /#MB,2)+"MB")
      If WriteData(#binfile, *ptr, filecounter * #HashTableSizeHash) <> filecounter * #HashTableSizeHash    
        exit("Can`t save "+Str(filecounter * #HashTableSizeHash)+"b")
      EndIf
    EndIf 
    
    Print(" +")
    counter=0
    filecounter=0
    FileSeek(#binfile, 0 )
    For i =0 To HT_items-1 
      hash = ValueL(@i) & HT_mask 
      offset = hash * #Pointersz
      *contentpointer = PeekI(*PointerTable+offset) 
      If *contentpointer
        res\size = MemorySize(*contentpointer)/#HashTableSizeItems
      Else
        res\size = 0
      EndIf 
      
      If (filecounter * 4 + 4)<512*#MB        
        If res\size>0         
          PokeL (*ptr + filecounter * 4, counter)       
          counter+res\size
        Else       
          PokeL (*ptr + filecounter * 4, counter)          
        EndIf 
        filecounter+1
      Else
        Print(" "+StrD(filecounter * 4 /#MB,2)+"MB")
        If WriteData(#binfile, *ptr, filecounter * 4) <> filecounter * 4    
          exit("Can`t save "+Str(filecounter * 4)+"b")
        EndIf 
        filecounter=0
        If res\size>0         
          PokeL (*ptr + filecounter * 4, counter)       
          counter+res\size
        Else       
          PokeL (*ptr + filecounter * 4, counter)          
        EndIf 
        filecounter+1
      EndIf
      
      
    Next i   
    If filecounter>0
      ;save rest to file
      Print(" "+StrD(filecounter * 4 /#MB,2)+"MB")
      If WriteData(#binfile, *ptr, filecounter * 4) <> filecounter * 4    
        exit("Can`t save "+Str(filecounter * 4)+"b")
      EndIf
    EndIf 
    
    If Not WriteLong(#binfile, counter)
      exit("Can`t save 8b")
    EndIf 
    CloseFile(#binfile)
  Else
      exit("Can`t create "+filebinname$)
  EndIf
    
  FreeMemory(*ptr)
  
EndProcedure



Procedure ReadHTpackFile(*hash, *res.HashTableResultStructure)  
  Protected offset, hash, *arr=AllocateMemory(8)
  Shared HT_mask, HT_items
  
  hash = ValueL(*hash) & HT_mask   
 
  FileSeek(#binfile, hash * 4 )
  If ReadData(#binfile, *arr, 8)
    *res\size = ValueL(*arr + 4) - ValueL(*arr)  
    *res\fileoffset =HT_items * 4 + 4 + ValueL(*arr ) * #HashTableSizeItems
  Else
    exit( "error during loading from file: pos["+Str(hash * 4)+"] 8b")
  EndIf
  
  ;PrintN("Hash:" +Str(hash)+" sz:"+Str(*res\size))
  FreeMemory(*arr)
EndProcedure



Procedure compareHTpackFile(*hash, *rescmp.comparsationStructure, startpos=0)
  Protected  res.HashTableResultStructure  , *arr
  *rescmp\pos=-1
  ReadHTpackFile(*hash,  @res) 
  ;PrintN( "cmp hash:"+m_gethex8(*hash))
  ;PrintN( "sz:"+Str(res\size))
  If res\size And startpos<=res\size 
    FileSeek(#binfile, res\fileoffset )
    *arr=AllocateMemory(res\size * #HashTableSizeItems)
    If ReadData(#binfile, *arr, res\size * #HashTableSizeItems)
      findInHashTable32bitSimple(*hash+4, *arr, startpos, res\size, *rescmp)
      ;Debug("Content pos: "+Str(*rescmp\pos)+", "+Str(ValueL(*arr+*rescmp\pos*#HashTableSizeItems+#HashTableSizeHash))) 
      If *rescmp\pos<>-1
        *rescmp\direction = *rescmp\pos
        *rescmp\pos=ValueL(*arr+*rescmp\pos*#HashTableSizeItems+#HashTableSizeHash)
      EndIf
    Else
      exit( "error during loading from file: pos["+Str(res\fileoffset)+"] "+Str(res\size * #HashTableSizeItems)+"b")
    EndIf
    FreeMemory(*arr)
    
  EndIf
  
EndProcedure

Procedure checkHTpackFile(filebinname$, totalpoints, numberofrand=1024)
  Protected res=0,  randnum, a$, *bx= AllocateMemory(32), *by=AllocateMemory(32), *pos=AllocateMemory(32), rescmp.comparsationStructure
  Shared *CurveGX, *CurveGY,*CurveP
  If numberofrand<1024
    numberofrand=4096
  EndIf
  
  If ReadFile(#binfile, filebinname$)
    While numberofrand>0 And res=0
      randnum = Random(totalpoints-1,0)
      a$=RSet(Hex(randnum+1), 64,"0")
      Curve::m_sethex32(*pos, @a$ )
      ;PrintN("["+Str(randnum)+"] Est."+m_gethex8(*bx+24)) 
      Curve::m_PTMULX64(*bx, *by, *CurveGX, *CurveGY, *pos,*CurveP)
      compareHTpackFile(*bx+#hashbyteoffset, @rescmp)
      If rescmp\pos=-1
        PrintN("false")
        PrintN("["+Str(numberofrand)+"]["+Str(randnum)+"] Est."+m_gethex8(*bx+#hashbyteoffset))     
        res=1
        Break
      EndIf
      numberofrand-1
    Wend 
    
    CloseFile(#binfile)
  Else
    exit("Can`t read "+filebinname$)
  EndIf   
  
  FreeMemory(*bx)
  FreeMemory(*by)
  FreeMemory(*pos)
  ProcedureReturn res
EndProcedure

Procedure checkWholeHashTableContentPackFile(filebinname$)
  Protected  res.HashTableResultStructure, i, err, *arr
  Shared HT_items, HT_total_items
  If ReadFile(#binfile, filebinname$)
    For i =0 To HT_items-1 
      ReadHTpackFile(@i, @res) 
      If res\size>1
        FileSeek(#binfile, res\fileoffset )
        *arr=AllocateMemory(res\size * #HashTableSizeItems)
        If ReadData(#binfile, *arr, res\size * #HashTableSizeItems)
          If checkHashTableContent(*arr, res\size)  
            ;if error
            err=1
            Break
          EndIf
        Else
          exit( "error during loading from file: pos["+Str(res\fileoffset)+"] "+Str(res\size * #HashTableSizeItems)+"b")
        EndIf
        FreeMemory(*arr)
      EndIf
    Next i
    
    If err
      PrintN("false")  
    EndIf
    
    CloseFile(#binfile)
  Else
    exit("Can`t read "+filebinname$)
  EndIf 
  ProcedureReturn err
EndProcedure
 
Procedure RemoveGiantArrTemp()   
  Protected memtotal
  Shared  *GiantArr
  
  memtotal + MemorySize(*GiantArr) 
  
  FreeMemory(*GiantArr) 
  PrintN("Freed memory: "+StrD(memtotal/#MB,3)+" MB")
EndProcedure

 Procedure RemoveTempHashTable(threadId)   
  Protected *ptr, i, hash, res.HashTableResultStructure, offset, *contentpointer, totalitems, localcountertotalfreed, localmemtotalfreed
  Shared job() , totallaunched, *PointerTable , HT_mask , memtotalfreed, countertotalfreed, JobMutex
  
  totalitems = job(Str(threadId))\beginNumberPoint + job(Str(threadId))\totalpoints
  
  For i = job(Str(threadId))\beginNumberPoint To totalitems-1 
    hash = ValueL(@i) & HT_mask 
    offset = hash * #Pointersz 
    *contentpointer = PeekI(*PointerTable+offset) 
    If *contentpointer
      res\size = MemorySize(*contentpointer)/#HashTableSizeItems
    Else
      res\size = 0
    EndIf  
  
    If res\size>0       
      res\contentpointer = *contentpointer
      localmemtotalfreed + MemorySize(res\contentpointer)
      FreeMemory(res\contentpointer)      
      localcountertotalfreed + res\size
    EndIf
  Next i
  LockMutex(JobMutex)
  memtotalfreed + localmemtotalfreed
  countertotalfreed + localcountertotalfreed
  Print(" #"+Str(threadId))
  totallaunched - 1 
  UnlockMutex(JobMutex)
EndProcedure

Procedure RemoveTempHashTableThreaded()
  Protected totalCPUcout, jobperthread, CurNumberPoint, restjob, i, totalitems
  Shared job() , totallaunched, HT_items, memtotalfreed, *PointerTable_unalign, countertotalfreed, JobMutex
  
    Print("Remove Temp HashTable...")
    totalitems = HT_items
    totallaunched = 0
    totalCPUcout = CountCPUs(#PB_System_ProcessCPUs)
    If totalCPUcout>1 And totalitems>#MB       
      CurNumberPoint = 0
      jobperthread = totalitems/totalCPUcout
      ;PrintN("jobperthread: "+Str(jobperthread)+" items")
      job(Str(0))\beginNumberPoint = CurNumberPoint           
      job(Str(0))\totalpoints = jobperthread    
      CurNumberPoint = CurNumberPoint + job(Str(0))\totalpoints
      restjob = totalitems - (jobperthread * totalCPUcout)
      ;PrintN("Rest points: "+Str(restjob))
      
    Else  
      ;PrintN("jobperthread: "+Str(waletcounter)+" items")
      job(Str(0))\beginNumberPoint = CurNumberPoint           
      job(Str(0))\totalpoints = totalitems      
      CurNumberPoint = CurNumberPoint + job(Str(0))\totalpoints      
    EndIf
    LockMutex(JobMutex)
    totallaunched+1
    UnlockMutex(JobMutex)
    CreateThread(@RemoveTempHashTable(),0)
    
    If totalCPUcout>1 And totalitems>#MB       
      For i = 1 To totalCPUcout-1
        job(Str(i))\beginNumberPoint = CurNumberPoint        
        job(Str(i))\totalpoints = jobperthread
        LockMutex(JobMutex)
        totallaunched+1
        UnlockMutex(JobMutex)
        CreateThread(@RemoveTempHashTable(),i)
        
        CurNumberPoint = CurNumberPoint + job(Str(i))\totalpoints 
        Delay(500)
      Next i
    EndIf
    
    While totallaunched
      Delay(200)      
    Wend   
    Print(" rest")
    If restjob        
        job(Str(0))\beginNumberPoint = CurNumberPoint          
        job(Str(0))\totalpoints = restjob        
        RemoveTempHashTable(0)
    EndIf     
    
    memtotalfreed +  MemorySize(*PointerTable_unalign)  
    FreeMemory(*PointerTable_unalign) 
    PrintN("Total removed items: "+Str(countertotalfreed)+", freed memory: "+StrD(memtotalfreed/#MB,3)+" MB")
  EndProcedure
  
Procedure checkHT( totalpoints, numberofrand=1024)
  Protected res=0,  randnum, a$, *b= AllocateMemory(32), *x = AllocateMemory(64)
  Shared *CurveGX, *CurveGY, *CurveP
  If numberofrand<1024
    numberofrand=4096
  EndIf
  
  a$=RSet("0000000000000000", 64,"0")
  Curve::m_sethex32(*b, @a$ )
  
  While numberofrand>0 And res=0
    randnum = Random(totalpoints-1,0)
    a$=RSet(Hex(randnum), 64,"0")
    Curve::m_sethex32(*b, @a$ )
    Curve::m_PTMULX64(*x, *x+32, *CurveGX, *CurveGY, *b,*CurveP)
    
    If compareWithHashtable(*x+#hashbyteoffset)=-1
      PrintN("false")
      PrintN("["+Str(numberofrand)+"]["+Str(randnum)+"] Est."+m_gethex8(*x+#hashbyteoffset))     
      res=1
      Break
    EndIf
    numberofrand-1
  Wend  
  
    FreeMemory(*b)
    FreeMemory(*x)
  ProcedureReturn res
EndProcedure

Procedure FilePutContents(filename.s, *mem, size)
 Protected f,res,r
  f=CreateFile(#PB_Any,filename)

  If f    
    res=WriteData(f,*mem,size)
    CloseFile(f)    
    If res=size
      r=1
    Else
      r=0
    EndIf    
  EndIf
  ProcedureReturn r
EndProcedure

Procedure Save_HTpacked(*xpoint)
  Protected i,j, filebinname$, full_size, len=8, hash.s, *pp, counters, totalpos, jobcomplete.d, prejobcomplete.d, wrbytes, savedbytes, maxsavebytes, loadedbytes, starttime
  Protected totalloadbytes, maxloadbytes, Yoffset, w$, ramneed1, ramneed2, ramneed3, *hash=AllocateMemory(32), a$
  Shared *GpuHT, *GpuHT_unalign, *PointerTable_unalign, *PointerTable,  waletcounter, HT_items,  *CurveGX, *CurveGY, maxnonce
 Shared initHTsize,  HT_POW
  
    
  filebinname$=Curve::m_gethex32(*xpoint)+"_"+Str(waletcounter)+"_"+Str(HT_items)
  
  
  If FileSize(filebinname$+"_htGPUv"+Str(#hashbyteoffset)+".BIN") = -1 Or FileSize(filebinname$+"_htCPUv"+Str(#hashbyteoffset)+".BIN") = -1
     ;unpacked ht array
     ramneed1 = #helpsize * 96 * CountCPUs(#PB_System_ProcessCPUs) ;helper array
     ramneed1 + #HashTableSizeItems * initHTsize * waletcounter ;content ht     
     ramneed1 + CountCPUs(#PB_System_ProcessCPUs) * #helpsize * 8 ;baby array     
     ramneed1 + HT_items * #Pointersz +#align_size  ;pointers for ht
     ramneed1 + HT_items * 8                        ;Purebasic keep pointer(8b) for every allocated memory  
     If (HT_items * 4 + waletcounter * #HashTableSizeItems + 4)>=512*#MB ;filebuffer
       ramneed1 + 512*#MB
     Else
       ramneed1 + HT_items * 4 + waletcounter * #HashTableSizeItems + 4
     EndIf
     PrintN("[R1] "+Str(ramneed1/#MB)+" MB")    
     
     ;giant array + packed giant array
     ramneed2 = #helpsize * 96 * CountCPUs(#PB_System_ProcessCPUs) ;helper array     
     ramneed2 + (maxnonce+1)*64; giant array
     ramneed2 + (maxnonce+1)*64; packed giant array
     PrintN("[R2] "+Str(ramneed2/#MB)+" MB")  
     
     If ramneed2>ramneed1
       ramneed1=ramneed2
     EndIf
     
     PrintN("Free RAM["+Str(MemoryStatus(#PB_System_FreePhysical)/#MB)+" MB], need["+Str(ramneed1/#MB)+" MB]") 
     If MemoryStatus(#PB_System_FreePhysical)<ramneed1*1.5
       PrintN("Warning!!! To compute all arrays you need to have free "+Str(ramneed1/#MB)+" MB of RAM")
       Delay(2000)     
     EndIf
    
     PrintN("[pHTCPU "+StrD( (HT_items * 4  + waletcounter * #HashTableSizeItems + 4)/#MB,2 )+"MB]") 
     PrintN("[pHTGPU "+StrD( (HT_items * 4  + waletcounter * #HashTableSizeHash + 4)/#MB,2 )+"MB]")
     PrintN("[pG2_host "+StrD( ((maxnonce+1)*64)/#MB,2 )+"MB]"+"[pG2_gpu "+StrD( (maxnonce* 96+160)/#MB,2 )+"MB]")
     
    *PointerTable_unalign=AllocateMemory(HT_items*#Pointersz + #align_size)
    If *PointerTable_unalign=0
      PrintN("Can`t allocate memory Pointer array for HT")
      exit("")
    EndIf
    *PointerTable=*PointerTable_unalign+#align_size-(*PointerTable_unalign % #align_size)
    
    
    ;Generate Babys points array AGAIN
      starttime= ElapsedMilliseconds()
      GenBabys(*CurveGX, *CurveGY)
      PrintN("Done in "+FormatDate("%hh:%ii:%ss", (ElapsedMilliseconds()-starttime)/1000)+"s")
      
    HashTableSammary()    
    
    Print("Sorting HT items...")
    sortWholeHashTableThreaded(HT_items)
    PrintN(" ok")
     
    
    Print("Verify HT sorting...")
    If checkWholeHashTableContenThreaded()=1  
      exit("")
    Else
      PrintN(" ok")
    EndIf
    
    Print("Verify HT items...")
    If checkHT(waletcounter, 4096)=1  
      exit("")
    Else
      PrintN(" ok")
    EndIf
    
        
    If FileSize(filebinname$+"_htCPUv"+Str(#hashbyteoffset)+".BIN") = -1
      ;create CPU file  
      Print("Pack HTCPU items to file...")
      packHTFile(filebinname$+"_htCPUv"+Str(#hashbyteoffset)+".BIN")
      PrintN(" ok")
      
      Print("Random verify packed HTCPU items in file...")
      If checkHTpackFile(filebinname$+"_htCPUv"+Str(#hashbyteoffset)+".BIN", waletcounter, 1024)=1
        exit("")
      Else
        PrintN(" ok")
      EndIf
      
      ;Print("Verify packed HTCPU items sorting in file...")
      ;If checkWholeHashTableContentPackFile(filebinname$+"_htCPUv"+Str(#hashbyteoffset)+".BIN")
       ;exit("")
      ;Else
        ;PrintN("ok")
      ;EndIf 
    EndIf
    
    If FileSize(filebinname$+"_htGPUv"+Str(#hashbyteoffset)+".BIN") = -1
      ;create GPU file      
      Print("Pack HTGPU items to file...")
      packHTGPUFile(filebinname$+"_htGPUv"+Str(#hashbyteoffset)+".BIN")
      PrintN(" ok")      
    EndIf    
    
    RemoveTempHashTableThreaded()  
    
  Else
    PrintN("Both HT files exist") 
  EndIf
  
  
EndProcedure


Procedure ErrorHandler()
  Protected ErrorMessage$
  
  ErrorMessage$ = "A program error was detected:" + Chr(13) 
  ErrorMessage$ + Chr(13)
  ErrorMessage$ + "Error Message:   " + ErrorMessage()      + Chr(13)
  ErrorMessage$ + "Error Code:      " + Str(ErrorCode())    + Chr(13)  
  ErrorMessage$ + "Code Address:    " + Str(ErrorAddress()) + Chr(13)
 
  If ErrorCode() = #PB_OnError_InvalidMemory   
    ErrorMessage$ + "Target Address:  " + Str(ErrorTargetAddress()) + Chr(13)
  EndIf
 
  If ErrorLine() = -1
    ErrorMessage$ + "Sourcecode line: Enable OnError lines support to get code line information." + Chr(13)
  Else
    ErrorMessage$ + "Sourcecode line: " + Str(ErrorLine()) + Chr(13)
    ErrorMessage$ + "Sourcecode file: " + ErrorFile() + Chr(13)
  EndIf
 
  ErrorMessage$ + Chr(13)
  ErrorMessage$ + "Register content:" + Chr(13)
 
  CompilerSelect #PB_Compiler_Processor 
    CompilerCase #PB_Processor_x86
      ErrorMessage$ + "EAX = " + Str(ErrorRegister(#PB_OnError_EAX)) + Chr(13)
      ErrorMessage$ + "EBX = " + Str(ErrorRegister(#PB_OnError_EBX)) + Chr(13)
      ErrorMessage$ + "ECX = " + Str(ErrorRegister(#PB_OnError_ECX)) + Chr(13)
      ErrorMessage$ + "EDX = " + Str(ErrorRegister(#PB_OnError_EDX)) + Chr(13)
      ErrorMessage$ + "EBP = " + Str(ErrorRegister(#PB_OnError_EBP)) + Chr(13)
      ErrorMessage$ + "ESI = " + Str(ErrorRegister(#PB_OnError_ESI)) + Chr(13)
      ErrorMessage$ + "EDI = " + Str(ErrorRegister(#PB_OnError_EDI)) + Chr(13)
      ErrorMessage$ + "ESP = " + Str(ErrorRegister(#PB_OnError_ESP)) + Chr(13)
 
    CompilerCase #PB_Processor_x64
      ErrorMessage$ + "RAX = " + Str(ErrorRegister(#PB_OnError_RAX)) + Chr(13)
      ErrorMessage$ + "RBX = " + Str(ErrorRegister(#PB_OnError_RBX)) + Chr(13)
      ErrorMessage$ + "RCX = " + Str(ErrorRegister(#PB_OnError_RCX)) + Chr(13)
      ErrorMessage$ + "RDX = " + Str(ErrorRegister(#PB_OnError_RDX)) + Chr(13)
      ErrorMessage$ + "RBP = " + Str(ErrorRegister(#PB_OnError_RBP)) + Chr(13)
      ErrorMessage$ + "RSI = " + Str(ErrorRegister(#PB_OnError_RSI)) + Chr(13)
      ErrorMessage$ + "RDI = " + Str(ErrorRegister(#PB_OnError_RDI)) + Chr(13)
      ErrorMessage$ + "RSP = " + Str(ErrorRegister(#PB_OnError_RSP)) + Chr(13)
      ErrorMessage$ + "Display of registers R8-R15 skipped."         + Chr(13)
 
    CompilerCase #PB_Processor_PowerPC
      ErrorMessage$ + "r0 = " + Str(ErrorRegister(#PB_OnError_r0)) + Chr(13)
      ErrorMessage$ + "r1 = " + Str(ErrorRegister(#PB_OnError_r1)) + Chr(13)
      ErrorMessage$ + "r2 = " + Str(ErrorRegister(#PB_OnError_r2)) + Chr(13)
      ErrorMessage$ + "r3 = " + Str(ErrorRegister(#PB_OnError_r3)) + Chr(13)
      ErrorMessage$ + "r4 = " + Str(ErrorRegister(#PB_OnError_r4)) + Chr(13)
      ErrorMessage$ + "r5 = " + Str(ErrorRegister(#PB_OnError_r5)) + Chr(13)
      ErrorMessage$ + "r6 = " + Str(ErrorRegister(#PB_OnError_r6)) + Chr(13)
      ErrorMessage$ + "r7 = " + Str(ErrorRegister(#PB_OnError_r7)) + Chr(13)
      ErrorMessage$ + "Display of registers r8-R31 skipped."       + Chr(13)
 
  CompilerEndSelect  
  
  
   If  CreateFile(#LOGFILE, FormatDate("%dd_%mm-%hh_%ii_%ss ", Date())+"_error_log.txt",#PB_File_SharedRead )
     WriteStringN(#LOGFILE,FormatDate("%dd/%mm/%hh:%ii:%ss:", Date())+ErrorMessage$,#PB_UTF8)
     FlushFileBuffers(#LOGFILE)
     CloseFile(#LOGFILE)
   EndIf
  ;If StratServ        
 ;       CloseNetworkServer(#Server)
  ;EndIf
End
EndProcedure
;-START

OnErrorCall(@ErrorHandler())

Define  i, pointcount, ndev, a$, starttime, jobperthread, res, totalCPUcout, restjob, begintime, workingtime, Title$,  result.comparsationStructure, finditems, lastlogtime,totalhash, perf$
Define cnt$, infostr$, hashd.d, wald.d = Log(waletcounter*2)/Log(2) ;due to use x2GS


PrintN("APP VERSION: "+#appver)
PrintN("**********************************************************************************")
PrintN(" This version ["+#appver+"] may content various bugs,                           ")
PrintN(" Don`t use this version for serious task.                                       ")
PrintN(" It is needed to test the possibility of using the -w parameter greater than 30 ")
PrintN(" if you accept this press ENTER to continue or close the program otherwise.     ")
PrintN("**********************************************************************************")
Input()
begintime=Date()

If waletcounter>=3069485951
  exit("-w should be less or equil to 3069485951 Or 2^31.515349920643907")
EndIf

If HT_POW>31
  exit("-htsz should be less than 32")
EndIf

If HT_POW=27 And waletcounter>1331331443
  PrintN("With -htsz 27 value -w should be less or equil to 1331331443 or 2^30.310222637591963") 
  PrintN("Due to the possibility of duplicate values in the hash table")
  PrintN("It is unsafe to use values higher than those specified above") 
  PrintN("To continue in UNSAFE mode type Y and press ENTER")
  If Input()<>"Y"
    exit("")
  EndIf
EndIf

If HT_POW=28 And waletcounter>1777178603
  PrintN("With -htsz 28 value -w should be less or equil to 1777178603 Or 2^30.726941530690112")
  PrintN("Due to the possibility of duplicate values in the hash table")
  PrintN("It is unsafe to use values higher than those specified above") 
  PrintN("To continue in UNSAFE mode type Y and press ENTER")
  If Input()<>"Y"
    exit("")
  EndIf
EndIf

If HT_POW=29 And waletcounter>3069485951
  PrintN("With -htsz 29 value -w should be less or equil to 3069485950 Or 2^31.515349920643907") 
  PrintN("Due to the possibility of duplicate values in the hash table")
  PrintN("It is unsafe to use values higher than those specified above") 
  PrintN("To continue in UNSAFE mode type Y and press ENTER")
  If Input()<>"Y"
    exit("")
  EndIf
EndIf

If HT_POW=30 And waletcounter>3069485951
  PrintN("With -htsz 30 value -w should be less or equil to 3069485951 Or 2^31.515349920643907") 
  PrintN("Due to the possibility of duplicate values in the hash table")
  PrintN("It is unsafe to use values higher than those specified above") 
  PrintN("To continue in UNSAFE mode type Y and press ENTER")
  If Input()<>"Y"
    exit("")
  EndIf
EndIf

If HT_POW=31 And waletcounter>3069485951
  PrintN("With -htsz 31 value -w should be less or equil to 3069485951 Or 2^31.515349920643907") 
  PrintN("Due to the possibility of duplicate values in the hash table")
  PrintN("It is unsafe to use values higher than those specified above") 
  PrintN("To continue in UNSAFE mode type Y and press ENTER")
  If Input()<>"Y"
    exit("")
  EndIf
EndIf

If Int(Log(waletcounter)/Log(2))-HT_POW>3
  PrintN("WARNING! -htsz parametr is to low, should be at least "+Str(Int(Log(waletcounter)/Log(2))-2))
EndIf

*CenterBig=AllocateMemory(32)
If *CenterBig=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*CenterX=AllocateMemory(32)
If *CenterX=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*CenterY=AllocateMemory(32)
If *CenterY=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf

*GlobKey=AllocateMemory(32)
If *GlobKey=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
GlobPub\x=AllocateMemory(32)
If GlobPub\x=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
GlobPub\y=AllocateMemory(32)
If GlobPub\y=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*WINKEY=AllocateMemory(32)
If *WINKEY=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf

*WidthRange=AllocateMemory(32)
If *WidthRange=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf

*PrivBIG=AllocateMemory(32)
If *PrivBIG=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
PubkeyBIG\x=AllocateMemory(32)
PubkeyBIG\y=AllocateMemory(32)
If PubkeyBIG\x=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
If PubkeyBIG\y=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*MaxNonceBIG=AllocateMemory(32)
If *MaxNonceBIG=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
ADDPUBG\x=AllocateMemory(32)
If ADDPUBG\x=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
ADDPUBG\y=AllocateMemory(32)
If ADDPUBG\y=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*bufferResult=AllocateMemory(32)
If *bufferResult=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*addX=AllocateMemory(32)
If *addX=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*addY=AllocateMemory(32)
If *addY=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf

*PRKADDBIG=AllocateMemory(32)
If *PRKADDBIG=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf

PUBADDBIG\x=AllocateMemory(32)
If PUBADDBIG\x=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
PUBADDBIG\y=AllocateMemory(32)
If PUBADDBIG\y=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf


FINDPUBG\x=AllocateMemory(32)
If FINDPUBG\x=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
FINDPUBG\y=AllocateMemory(32)
If FINDPUBG\y=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf

REALPUB\x=AllocateMemory(32)
If REALPUB\x=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
REALPUB\y=AllocateMemory(32)
If REALPUB\y=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf

Two\x=AllocateMemory(32)
If Two\x=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
Two\y=AllocateMemory(32)
If Two\y=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf

Curve::m_ADDPTX64(Two\x, Two\y, *CurveGX, *CurveGY, *CurveGX, *CurveGY, *CurveP)

If pparam &1=1
   exit("vaule -p must be a multiple of 2")
 EndIf
 
 If threadtotal &1=1
   exit("vaule -t must be a multiple of 2")
 EndIf
 
 If blocktotal &1=1
   exit("vaule -b must be a multiple of 2")
 EndIf
 
If pubfile$ And FileSize(pubfile$)=-1
  exit("File["+pubfile$+"] not found!")
EndIf



a$=RSet(Hex(waletcounter*2), 64,"0")
Curve::m_sethex32(*MaxNonceBIG, @a$)




;PrintN("GiantSUBvalue:"+Curve::m_gethex32(*MaxNonceBIG))
Curve::m_PTMULX64(ADDPUBG\x, ADDPUBG\y, *CurveGX, *CurveGY, *MaxNonceBIG,*CurveP)
;make it negative> p-ypoint
Curve::m_subModX64(ADDPUBG\y,*CurveP,ADDPUBG\y,*CurveP)
;PrintN("GiantSUBpubkey: "+uncomressed2commpressedPub(Curve::m_gethex32(ADDPUBG\x)+Curve::m_gethex32(ADDPUBG\y)))


PrintN("*******************************")
PrintN("Total GPU Memory Need: "+ StrD((maxnonce* 96+160 + HT_items * 4 + 4 + #align_size + waletcounter * #HashTableSizeHash)/#MB,3)+"MB")
PrintN("*******************************")



a$=RSet(Hex(pparam * waletcounter), 64,"0")
Curve::m_sethex32(*CenterBig, @a$)
Curve::m_PTMULX64(*CenterX, *CenterY, *CurveGX, *CurveGY, *CenterBig,*CurveP)
;make it negative> p-ypoint
Curve::m_subModX64(*CenterY,*CurveP,*CenterY,*CurveP)


  
;Generate HT table 
Save_HTpacked(*CurveGX)


;Generate GIANTS points

starttime= ElapsedMilliseconds() 

Save_Load_Giants()

PrintN("Done in "+FormatDate("%hh:%ii:%ss", (ElapsedMilliseconds()-starttime)/1000)+"s")
  




    



PrintN("Total time "+FormatDate("%hh:%ii:%ss", Date()-begintime)+"s")   
Delay(2000)
PrintN("finished ok")
exit("")



; IDE Options = PureBasic 5.31 (Windows - x64)
; ExecutableFormat = Console
; CursorPosition = 20
; Folding = 8bNS-2x8---v+
; EnableThread
; EnableXP
; Executable = onlygen_1_9_6File0.exe
; DisableDebugger