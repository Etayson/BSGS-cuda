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

#array_dim=64
#line_dim=64
#alignMemoryGpu=64   
#LOGFILE=1
#WINFILE="win.txt"
#appver="1.7.3"
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

Import "lib\cuda.lib"
  cuInit(Flags.i)
  
  cuMemGetInfo_v2(freebytes.i,totalbytes.i)
  cuEventCreate(phEvent.i,Flags.i)	
  cuEventDestroy 	(hEvent.i)  	
  cuEventQuery 	(hEvent.i) 
  cuEventRecord 	(hEvent.i,Stream.i) 
  cuEventSynchronize 	(hEvent.i)  	
  cuDeviceTotalMem(bytes.i,dev.i)
  cuDeviceTotalMem_v2(bytes.i,dev.i)
  cuDeviceComputeCapability(major.i,minor.i,dev.i) 	
  cuDeviceGetCount(count.i)
  cuDeviceGetName(name.s,len.i,dev.i)  
  cuDeviceGetAttribute(pi.i,attrib.i,dev.i)
  cuDeviceGet(device.i, ordinal.i)
  cuGetErrorName ( err.i, err_string.s )
  cuCtxCreate(pctx.i, flags.i, dev.i)
  cuCtxCreate_v2(pctx.i, flags.i, dev.i)
  cuMemAlloc(dptr.i, bytesize.i)
  cuMemAlloc_v2(dptr.i, bytesize.i)
  cuModuleGetGlobal (dptr.i, bytesize.i,hmodule.i,name.i)	
  cuModuleLoadData(hmodule.i, image.i)
  cuModuleLoad(hmodule.i, fname.i)
  cuModuleGetFunction(hfunc.i, hmod.i, name.s)
  cuParamSetSize(hfunc.i, numbytes.i)
  cuParamSetv(hfunc.i, offset.i, ptr.i, numbytes.i)
  cuParamSeti(hfunc.i, offset.i, value.i)
  cuFuncSetBlockShape(hfunc.i, x.i, y.i, z.i)
  cuLaunchGridAsync( hfunc.i, x.i, y.i, z.i,hstream.i)		
  cuLaunchGrid(f.i, grid_width.i, grid_height.i)
  cuFuncSetSharedSize(f.i,numbytes.i) 	
  cuFuncSetCacheConfig 	( f.i,config.i) 
  cuLaunch(f.i)
  
  cuFuncGetAttribute 	(pi.i,attrib.i,f.i) 	
  cuStreamCreate (hStream.i, Flags.i)
  cuStreamCreate_v2 (hStream.i, Flags.i)
  cuStreamDestroy (hStream.i)
  cuStreamSynchronize (hStream)
  cuStreamQuery 	(hStream.i)  	
  cuCtxSynchronize()
  cuMemcpyDtoH(dstHost.i, srcDevice.i, ByteCount.i)
  cuMemcpyDtoH_v2(dstHost.i, srcDevice.i, ByteCount.i)
  cuMemcpyHtoD(dstDevice.i, srcHost.i, ByteCount.i)
  cuMemcpyHtoD_v2(dstDevice.i, srcHost.i, ByteCount.i)
  cuMemFree(dptr.i)
  cuMemFree_v2(dptr.i)
  cuCtxDestroy(ctx.i)
  cuCtxDestroy_v2(ctx.i)
EndImport

Structure CoordPoint
  *x
  *y
EndStructure

#align_size=128
#HashTablesz=8;4B counter items and 4B offset in PointersTable 
#Pointersz=8
#HashTableSizeHash=4
#HashTableSizeItems=8
#maximumgpucount = 32

Structure HashTableResultStructure   
 size.l
 *contentpointer
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
Define maxnonce, *BabyArr, *BabyArr_unalign, *BabyArrSorted, *BabyArrSorted_unalign, *GiantArr, *GiantArrPacked, *HelperArr, totallaunched, *Table_unalign, *Table, *GpuHT_unalign, *GpuHT
Define NewMap job.JobSturucture()
Define NewMap sortjob.sortjobStructure()
Define keyMutex, quit, *PointerTable_unalign, *PointerTable
Define *PrivBIG, PubkeyBIG.CoordPoint, *MaxNonceBIG, FINDPUBG.CoordPoint, ADDPUBG.CoordPoint, *bufferResult, *addX, *addY, *PRKADDBIG, PUBADDBIG.CoordPoint, REALPUB.CoordPoint, *WINKEY, Two.CoordPoint
Define *WidthRange
Define Defdevice$, HT_POW=26, endrangeflag=0, pubfile$="", NewList publist.s() , globalquit, isreadyjob, listpos
Define JobMutex
Define *GlobKey
Define GlobPub.CoordPoint
Define *CenterBig, *CenterX, *CenterY
Define *GlobCnt
JobMutex = CreateMutex()
keyMutex = CreateMutex()

;-VARIABLES

threadtotal = 512;512
blocktotal = 68;68
pparam=256
waletcounter=Int(Pow(2, 27))

;mainpub.s = "0222479403f4eb300b997bc76feab5e9e0631be2d2b006e3d87ec586bd48a94720"
;mainpub.s = "9b46a5c1c66aa27ac6409414db3f7994c79c7b2aa22a63f79fec0b3a6c2ba706c8913a9abf96cc9dc6ea102e19ffea29a0845f2c6d12f88380aea8de61a368fe"
;mainpub.s = "11569442e870326ceec0de24eb5478c19e146ecd9d15e4666440f2f638875f42524c08d882f868347f8b69d3330dc1913a159d8fb2b27864f197693a0eb39a23"
;mainpub.s = "e1e5e6f7b0b8d67604e3940c87bf06b814cedc486112b9956c68e3d78b1bd81297fe4f65fbd6e9f7eb1eea80b144d1487f2a9b0aeae5fcf6f43b41491641884e" ;125357 1E9AD
;mainpub.s = "59A3BFDAD718C9D3FAC7C187F1139F0815AC5D923910D516E186AFDA28B221DC994327554CED887AAE5D211A2407CDD025CFC3779ECB9C9D7F2F1A1DDF3E9FF8"
;mainpub.s = "0232c9a17a7c9f6d9d99f19657e3615d76213d4bf1a5f7473321c776ac73d4b607"
;privkey.s="0x0000000000000000000000000000000000000000000000000000fde000000000"
             
;privkey.s="0x0000000000000000000000000000000000000000000000000020000000000000"
;privkey.s="0x10000000000000000"
;privkeyend.s="0x1ffffffffffffffff"
privkey.s="0x1"
privkeyend.s="";"0x80001F4"

Declare getprogparam()
Declare exit(str.s)
Declare Log2(Quad.q)
Declare ReadHTpack(*hash, *arr, *res.HashTableResultStructure)
Declare FilePutContents(filename.s, *mem, size)
Declare RemoveGiantArrTemp() 
Declare compareHTpack(*hash)

OpenConsole()

getprogparam()

Define BABYS_pow = log2(waletcounter)
Define HT_items = Int(Pow(2,HT_POW))
Define HT_mask = HT_items-1
Define HT_total_items = 0
Define HT_max_collisions = 0
Define HT_items_with_collisions = 0
Define HT_total_hashes = 0
Define initHTsize=1
If BABYS_pow>HT_POW
  ;initHTsize=Int(Pow(2,BABYS_pow-HT_POW))
  initHTsize=BABYS_pow-HT_POW
EndIf







maxnonce = threadtotal * blocktotal * pparam






*HelperArr=AllocateMemory(1024*96*CountCPUs(#PB_System_ProcessCPUs))
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
  If Left(ha$,2)="04"
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

Procedure Tune(memsize, sm)
  Protected  treadN, blockN, pparamN, wN, htszN, k, httotsize, frmem, gsize, ramneed1, ramneed2
  ;Debug "freemem:"+Str(memsize)
  blockN=sm
  treadN=256
  pparamN=256
  gsize= treadN * blockN * pparamN *96 + 160 + #align_size
  ;Debug gsize
  If memsize - gsize>0
      k = Log(memsize - gsize)/Log(2)
      ;Debug k
      If k>30
        k=30
      EndIf
      k+1
      Repeat 
        k-1
        Select k
          ;Case 31
            ;htszN=30
          Case 30
            htszN=28
          Case 29
            htszN=28
          Case 28
            htszN=27
          Case 27
            htszN=25
        EndSelect
        
        httotsize = Pow(2,k)*4 + Pow(2,htszN)*8
        ;Debug "k:"+Str(k)+" htszN:"+Str(htszN)+" = "+Str(httotsize)
      Until (gsize+httotsize)<memsize Or k<25
      Repeat
        frmem = memsize - httotsize
        ;Debug frmem
        frmem = (frmem - 160 - #align_size)/96
        ;Debug frmem
        frmem = frmem / blockN
        ;Debug frmem
        frmem = frmem / treadN
        ;Debug frmem
        If frmem>512
          If treadN<512
            treadN+32
            ;Debug ">>thread set to "+Str(treadN)
          Else
            blockN + sm
            ;Debug ">>block set to "+Str(blockN)
          EndIf
        EndIf
        ;Debug "******"
        
      Until frmem<512 Or blockN/sm>=4
        frmem = memsize - httotsize    
        frmem = (frmem - 160 - #align_size-96)/96
        frmem = frmem / blockN / treadN
        If frmem&1
          frmem+1
        EndIf
        pparamN = frmem
        gsize= treadN * blockN * pparamN *96 + 160 + #align_size       
        If gsize+httotsize>memsize
          pparamN-2
           gsize= treadN * blockN * pparamN *96 + 160 + #align_size
         EndIf
         
     ramneed1 = (Pow(2,k)+1)*8 + #align_size ;baby array
     ramneed1 + Pow(2,htszN)*8 +#align_size + Pow(2,k)*8 +#align_size + Pow(2,k)*8 +#align_size;HT unpacked
     ;Debug "ramneed:"+StrD((ramneed1)/1024/1024,3)
     ramneed2 = Pow(2,htszN)*8 +#align_size + Pow(2,k)*8 +#align_size + Pow(2,k)*8 +#align_size;HT unpacked
     ramneed2 + Pow(2,htszN)*8 +#align_size + Pow(2,k)*8 +#align_size ;HTCPU packed
     ;Debug "ramneed:"+StrD((ramneed2)/1024/1024,3)
     If ramneed2>ramneed1
       ramneed1=ramneed2
     EndIf
     PrintN("Try -t "+Str(treadN)+" -b "+Str(blockN)+" -p "+Str(pparamN)+" -w "+Str(k)+" -htsz "+Str(htszN)+" ["+StrD((gsize+httotsize)/1024/1024,3)+" MB] Gen RAM["+Str(ramneed1/1024/1024)+" MB]")
     
     ;Debug "gsize: "+Str(gsize)
     ;Debug "htsize: "+Str(httotsize)
     ;Debug "total MB: "+StrD((gsize+httotsize)/1024/1024,3)+"/"+StrD(memsize/1024/1024,3)
   EndIf
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

Procedure retGPUcount()
Protected namedev.s=Space(128)
Protected sizebytes.i
Protected piattrib.i
Protected major.i
Protected minor.i
Protected count.i
Protected mp.i
Protected cores.i
Protected pi.i    
Protected i.i
Protected result.i
Protected CudaDevice.i
Protected freebytes.i
Protected totalbytes.i
Protected CudaContext.i
cuInit(0)

cuDeviceGetCount(@count)
PrintN("Found "+count+" Cuda device.")

For i = 0 To count-1
      result = cuDeviceGet(@CudaDevice, i)               
      If result
        exit("cuDeviceGet - "+Str(result)+#CRLF$+"Try change -d param")
      EndIf
      result = cuDeviceGetName(namedev,128,CudaDevice)
      If result
        exit("cuDeviceGetName - "+Str(result))
      EndIf
      result = cuDeviceTotalMem_v2(@sizebytes,CudaDevice)
      If result
        exit("cuDeviceTotalMem - "+Str(result))
      EndIf
      result =  cuCtxCreate_v2(@CudaContext, 4, CudaDevice)    ; CU_CTX_BLOCKING_SYNC = 4 -- cuCtxSynchronize()
      If result
        exit("cuCtxCreate - "+Str(result))
      EndIf
      result=cuMemGetInfo_v2 	(@freebytes,@totalbytes) 	
      If result
       exit("error cuMemGetInfo_v2-"+Str(result))
      EndIf
      cuCtxDestroy_v2(CudaContext)
      
      
      PrintN("Cuda device:"+namedev+" ("+StrD(freebytes/1048576,3)+"/"+Str(sizebytes/1048576)+"MB)")
      
      cuDeviceComputeCapability(@major,@minor,CudaDevice) 
      cuDeviceGetAttribute(@piattrib,16,CudaDevice)
      mp=piattrib
      Select major
          
          Case 2 ;Fermi
            Debug "Fermi"
            If minor=1
              cores = mp * 48
            Else 
              cores = mp * 32
            EndIf
          Case 3; Kepler 
            Debug "Kepler"
            cores = mp * 192
            
          Case 5; Maxwell 
            Debug "Maxwell"
            cores = mp * 128
            
          Case 6; Pascal 
            Debug "Pascal"
            cores = mp * 64
            
          Case 7; Pascal 
            Debug "Pascal RTX"
            cores = mp * 64
            
          Case 8; Ampere 
            Debug "Ampere RTX"
            cores = mp * 128
          Default
            Debug "Unknown device type"
        EndSelect
      
      PrintN("Device have: MP:"+mp+" Cores+"+cores)
      
      
      ;cuDeviceGetAttribute(@pi,8,CudaDevice)      
      ;PrintN("Shared memory total:"+Str(pi))
      
      ;cuDeviceGetAttribute(@pi,9,CudaDevice)     
      ;PrintN("Constant memory total:"+Str(pi))
      If mp
        Tune(freebytes, mp)
      EndIf
      PrintN("---------------")
Next i
ProcedureReturn count
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
           PrintN( "-pb      Set single uncompressed/compressed pubkey for searching")
           PrintN( "-pk      Range start from , default "+privkey)
           PrintN( "-pke     End range ")
           PrintN( "-w       Set number of baby items 2^")
           PrintN( "-htsz    Set number of HashTable 2^ , default "+Str(HT_POW))
           PrintN( "-infile  Set file with pubkey for searching in uncompressed/compressed  format (search sequential)")
           PrintN( "-wl      Set recovery file from which the state will be loaded")
           PrintN( "-wt      Set timer for autosaving current state, default every "+Str(cnttimer)+"seconds")
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
          waletcounter=Int(Pow(2, Val(datares$)))
          PrintN( "Items number set to 2^"+Str(Val(datares$)))
        EndIf 
        
       Case "-htsz"
        Debug "found -htsz"
        i+1   
        datares$ = ProgramParameter(i) 
        If datares$<>"" And Left(datares$,1)<>"-"  
          HT_POW=Val(datares$)
          PrintN( "HT size number set to 2^"+HT_POW)         
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
    
    CopyMemory(*NewpointX+24,*pointerToNew,8)
    
    
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
    
    CopyMemory(*NewpointX+24,*pointerToNew,8)
  EndIf
  
  FreeMemory(*s)
EndProcedure

Procedure baby(id)
  Protected *my_pubX, *my_pubY, *arr, *newarrr, pointsperbatch, totalpoints, *inv, msg$="Baby #"+Str(id)+"  ", newbatchsz, calculatesz, leadzero, a$, *ptrarr
  Protected *bufferResult, temp$, *addX, *addY  
  Shared job(), totallaunched
  Shared *CurveGX, *CurveGY, *CurveP

  
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
  *newarrr = job(Str(id))\NewPointsArr
  totalpoints = job(Str(id))\totalpoints
  pointsperbatch = job(Str(id))\pointsperbatch
  
  Curve::m_sethex32(*my_pubX, @job(Str(id))\beginrangeX$)
  Curve::m_sethex32(*my_pubY, @job(Str(id))\beginrangeY$)
  
  PrintN(msg$+"("+Curve::m_gethex32(*my_pubX)+Curve::m_gethex32(*my_pubY)+")")
  
  a$=RSet(Hex(pointsperbatch), 64,"0")
  Curve::m_sethex32(*bufferResult, @a$)
  
  Curve::m_PTMULX64(*addX, *addY, *CurveGX, *CurveGY, *bufferResult,*CurveP)
    
    
  calculatesz=0
  While calculatesz<totalpoints
    
    CopyMemory(*my_pubX+24,*newarrr+calculatesz*8,8)
    
    If calculatesz<totalpoints
      newbatchsz = pointsperbatch
      If newbatchsz>=(totalpoints-calculatesz)
        newbatchsz = totalpoints-calculatesz
        a$=Hex(newbatchsz)
        Curve::m_sethex32(*bufferResult, @a$)      
        Curve::m_PTMULX64(*addX, *addY, *CurveGX, *CurveGY, *bufferResult,*CurveP)
      EndIf
      
      
      
      
      Curve::beginBatchAdd(*inv, newbatchsz-1, *my_pubX, *my_pubY,  *arr)
      BabycompleteBatchAddWithDouble(*newarrr+calculatesz*8+8,8, newbatchsz-1, *my_pubX, *my_pubY,  *arr, *inv)
      
      calculatesz  + newbatchsz    
      Curve::m_ADDPTX64(*my_pubX,*my_pubY,*my_pubX, *my_pubY,*addX,*addY,*CurveP)
   EndIf
  Wend
totallaunched-1

  FreeMemory(*inv)
EndProcedure

Procedure GenBabys(*xpoint, *ypoint)
  Protected totalCPUcout, i, jobperthread, restjob, a$, filebinname$, full_size, wrbytes, savedbytes, maxsavebytes, loadedbytes, *pp, totalloadbytes, maxloadbytes
  Shared *HelperArr, *BabyArr, *BabyArr_unalign, waletcounter, *CurveGX, *CurveGY, job(), totallaunched, FINDPUBG, *addX, *addY, *CurveP, *bufferResult, mainpub, waletcounter
  
  *BabyArr_unalign=AllocateMemory((waletcounter+1)*8 + #align_size)
  If *BabyArr_unalign=0
    PrintN("Can`t allocate memory for baby array")
    exit("")
  EndIf
  *BabyArr=*BabyArr_unalign+#align_size-(*BabyArr_unalign % #align_size)

  filebinname$=Curve::m_gethex32(*xpoint)+"_"+Str(waletcounter)+"_b.BIN"
  full_size=waletcounter*8
  
  If FileSize(filebinname$) = -1
    PrintN("Generate Babys Buffer: "+Str(waletcounter)+" items")
 
    Curve::fillarrayN(*HelperArr , 1024, *CurveGX, *CurveGY)
    ;prntarrBIG(*HelperArr, 5)
    
    totalCPUcout = CountCPUs(#PB_System_ProcessCPUs)
    If totalCPUcout>1 And waletcounter>1024
      ;copythe same points to other threads
      For i =1 To totalCPUcout-1
        CopyMemory(*HelperArr, *HelperArr + i * 1024*96, 1024*96)
      Next
      
      jobperthread = waletcounter/totalCPUcout
      PrintN("jobperthread: "+Str(jobperthread)+" items")
      job(Str(0))\arr = *HelperArr
      job(Str(0))\NewPointsArr = *BabyArr
      job(Str(0))\totalpoints = jobperthread
      job(Str(0))\pointsperbatch = 1024
      job(Str(0))\beginrangeX$  = Curve::m_gethex32(*xpoint)
      job(Str(0))\beginrangeY$  = Curve::m_gethex32(*ypoint)
      
      restjob = waletcounter - (jobperthread * totalCPUcout)
      PrintN("Rest points: "+Str(restjob))
      
    Else  
      PrintN("jobperthread: "+Str(waletcounter)+" items")
      job(Str(0))\arr = *HelperArr
      job(Str(0))\NewPointsArr = *BabyArr
      job(Str(0))\totalpoints = waletcounter
      job(Str(0))\pointsperbatch = 1024
      job(Str(0))\beginrangeX$  = Curve::m_gethex32(*xpoint)
      job(Str(0))\beginrangeY$  = Curve::m_gethex32(*ypoint)
    EndIf
    
    
    CreateThread(@baby(),0)
    totallaunched+1
    If totalCPUcout>1 And waletcounter>1024
      For i = 1 To totalCPUcout-1
        job(Str(i))\arr = *HelperArr+i*1024*96
        job(Str(i))\NewPointsArr = *BabyArr+i*jobperthread*8
        job(Str(i))\totalpoints = jobperthread
        job(Str(i))\pointsperbatch = 1024
        a$=Hex(jobperthread*i)
        Curve::m_sethex32(*bufferResult, @a$)
        Curve::m_PTMULX64(*addX, *addY, *CurveGX, *CurveGY, *bufferResult,*CurveP)
         Curve::m_ADDPTX64(*addX,*addY,*xpoint, *ypoint,*addX,*addY,*CurveP)
        job(Str(i))\beginrangeX$  = Curve::m_gethex32(*addX)
        job(Str(i))\beginrangeY$  = Curve::m_gethex32(*addY)
        CreateThread(@baby(),i)
        totallaunched+1
      Next i
    EndIf
    While totallaunched
      Delay(10)
    Wend
    
    If restjob
        a$=Hex(jobperthread*i)
        Curve::m_sethex32(*bufferResult, @a$)
        Curve::m_PTMULX64(*addX, *addY, *CurveGX, *CurveGY, *bufferResult,*CurveP)
         Curve::m_ADDPTX64(*addX,*addY,*xpoint, *ypoint,*addX,*addY,*CurveP)
        job(Str(0))\beginrangeX$  = Curve::m_gethex32(*addX)
        job(Str(0))\beginrangeY$  = Curve::m_gethex32(*addY)
        job(Str(0))\arr = *HelperArr
        job(Str(0))\NewPointsArr = *BabyArr+i*jobperthread*8
        job(Str(0))\totalpoints = restjob
        job(Str(0))\pointsperbatch = 1024
        baby(0)
    EndIf
    
    ;********************************
    For i = 0 To waletcounter-1
      toLittleInd32_64(*BabyArr+i*8) 
    Next i
    ;********************************
    ;Saving BIN FILE
    PrintN("Total: "+Str(full_size)+" bytes")
    ; -- SAVE DAG FILE
    PrintN("Save BIN file:"+filebinname$)
    savedbytes=0
    maxsavebytes=full_size
    If full_size>1024*1024*1024
      maxsavebytes = 1024*1024*1024
    EndIf
    *pp=*BabyArr
    
    If CreateFile(0, filebinname$,#PB_File_NoBuffering)           ; we create a new text file...
      i=0
      Repeat
      PrintN("["+Str(i)+"] chunk:"+Str(maxsavebytes)+"b")
      wrbytes =WriteData(0, *pp, maxsavebytes) 
      savedbytes + maxsavebytes
      
      If maxsavebytes<>wrbytes
        Print("Error when saving chunk: save:"+Str(maxsavebytes)+"b, got:"+Str(wrbytes)+"b")
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
  Else
    If OpenFile(0,filebinname$,#PB_File_NoBuffering)   
      ;Load BIN if exist
      PrintN("Load BIN file:"+filebinname$)  
      totalloadbytes=0
      maxloadbytes=full_size
      If full_size>1024*1024*1024
        maxloadbytes = 1024*1024*1024
      EndIf
      *pp=*BabyArr
      i=0
      Repeat
        PrintN("["+Str(i)+"] chunk:"+Str(maxloadbytes)+"b")
        loadedbytes=ReadData(0, *pp, maxloadbytes)
        totalloadbytes + maxloadbytes
        
        If maxloadbytes<>loadedbytes
          Print("Error when loading: need:"+Str(maxloadbytes)+"b, got:"+Str(loadedbytes)+"b")
          CloseFile(0)
          exit("")
        EndIf
        
        *pp+maxloadbytes
        
        If totalloadbytes<full_size
          If totalloadbytes+maxloadbytes>full_size
            maxloadbytes = full_size-totalloadbytes
            PrintN("Last chunk:"+Str(maxloadbytes)+"b")
          EndIf          
        EndIf
        i+1
      Until totalloadbytes>=full_size
    
      
      CloseFile(0)
    Else
      exit("Can`t open file:"+filebinname$)
    EndIf 
  EndIf
  ;********************************
    For i = 0 To waletcounter-1
      toLittleInd32_64(*BabyArr+i*8) 
    Next i
    ;********************************
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
   
  PrintN(msg$+"("+Curve::m_gethex32(*my_pubX)+Curve::m_gethex32(*my_pubY)+")")
  
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
    If check_equil(*MyX+24,*arr+randnum*8-8,2)=0
      PrintN("false")
      PrintN("["+Str(randnum)+"] Est."+m_gethex8(*MyX+24)  +"- got"+m_gethex8(*arr+randnum*8))     
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


Procedure sortinghash(id)  
  Protected *arrPointer, *arrPointer_sorted, len=8
  Protected *min=AllocateMemory(len), *max=AllocateMemory(len)
  Protected err, i, rescmp, pos
  Protected res.comparsationStructure
  Shared sortjob.sortjobStructure()
  Shared keyMutex, totallaunched
  
  *arrPointer = sortjob(Str(id))\ptarr
  *arrPointer_sorted = sortjob(Str(id))\sortptarray
  CopyMemory(*arrPointer, *min, len)
  
  ;prntarr("",sortjob(Str(id))\ptarr, sortjob(Str(id))\totallines)
  
  ;PrintN("Total points in Array:"+Str(sortjob(Str(id))\totallines))
  ;first find min value
   
  findMinMax8(*arrPointer,sortjob(Str(id))\totallines, *min,*max)   
  pos=0
  CopyMemory(*min, *arrPointer_sorted+pos*len, len)
  PrintN("MIN:0x"+m_gethex8(*arrPointer_sorted))
  Print("Sorting")
  For i=0 To sortjob(Str(id))\totallines-1
    If Not i%1000
      Print(".")
    EndIf
    foundinarr8(*arrPointer+i*len, sortjob(Str(id))\sortptarray, 0, pos, @res.comparsationStructure)
    ;PrintN("("+res\pos+","+res\direction+")")
    If res\pos=-1
      ;that mean that value is Not found in range
      If res\direction>pos
        pos=res\direction
        CopyMemory(*arrPointer+i*len, *arrPointer_sorted+pos*len, len)
      Else
        ;move block forward
        ;PrintN("move block")
        pos+1
        CopyMemory(*arrPointer_sorted+res\direction*len, *arrPointer_sorted+res\direction*len+len, (pos-res\direction)*len)
        CopyMemory(*arrPointer+i*len, *arrPointer_sorted+res\direction*len, len)
      EndIf
    Else
      ;PrintN("Warning!!!>"+getStrfrombin(*arrPointer+i*len,len))
    EndIf
    ;prntarr("",sortjob(Str(id))\sortptarray, sortjob(Str(id))\totallines)
  Next i
  
  
  FreeMemory(*min)
  LockMutex(keyMutex)
    totallaunched-1
    PrintN("Sort thread id:"+Str(id)+" ended")
  UnlockMutex(keyMutex)
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

Procedure SortingArrays(totalthread, *xpoint)
  Protected sortbatchperthr, sortrest, i,j, multimode, filebinname$, full_size, len=8, hash.s, *pp, counters, totalpos, jobcomplete.d, prejobcomplete.d, wrbytes, savedbytes, maxsavebytes, loadedbytes
  Protected totalloadbytes, maxloadbytes, A$
  Shared sortjob.sortjobStructure()  
  Shared totallaunched
  Shared mainpub
  Shared *BabyArr, *BabyArrSorted, *BabyArrSorted_unalign
  Shared waletcounter
  
  Protected *min, *max, *TotalRange, *A, *B, *Q8,*R8, *rb8, *re8
  
  *BabyArrSorted_unalign=AllocateMemory((waletcounter+1)*8 + #align_size)
  If *BabyArrSorted_unalign=0
    PrintN("Can`t allocate memory for sorted baby array")
    exit("")
  EndIf
  *BabyArrSorted=*BabyArrSorted_unalign+#align_size-(*BabyArrSorted_unalign % #align_size)


  *min=AllocateMemory(176)
  If *min=0
     PrintN("Can`t allocate memory")
    exit("")
  EndIf
  *max = *min + 8
  *TotalRange = *max+8
  *A= *TotalRange + 32
  *B= *A + 32
  *Q8= *B + 32
  *R8= *Q8 + 8
  *rb8=*R8+8
  *re8=*rb8+8
  
  filebinname$=Curve::m_gethex32(*xpoint)+"_"+Str(waletcounter)+"_s.BIN"
  full_size=waletcounter*len
  
  If FileSize(filebinname$) = -1
    ; file does not exist    
    ;sort
    If waletcounter>totalthread*2 And totalthread>1
      multimode=1
    EndIf
    
    ;multimode=0; just for test
    
    
    
    
    If multimode
      PrintN("Sorting Babys Array ["+Str(waletcounter)+"] with ["+Str(totalthread)+"] threads")
      
      ;prntarr("",*Arraypt, maxnonce)
    
      findMinMax8(*BabyArr,waletcounter, *min,*max)      
      
      
      PrintN("MIN: "+m_gethex8(*min) )
      PrintN("MAX: "+m_gethex8(*max) )
      
      sub8(*max,*min,*TotalRange)
      
      
      PrintN("TOTAL RANGE: "+m_gethex8(*TotalRange))
            
      div8(*TotalRange,8,*Q8,*R8)      
      PrintN("RANGE PER THREAD: "+m_gethex8(*Q8))
      
      CopyMemory(*max, *R8,8);temporary save
      
      CopyMemory(*min, *rb8,8)
      add8(*rb8,*Q8, *re8 )
      
      *pp=*BabyArrSorted 
      totallaunched=0
      For i=0 To totalthread-2
        PrintN("RANGE ["+Str(i)+"]")
        PrintN("FROM "+m_gethex8(*rb8))
        PrintN("  TO "+m_gethex8(*re8))
        
        ;copy rangeB to temporary array
        CopyMemory(*rb8, *min,8)
        ;copy rangeE to temporary array
        CopyMemory(*re8, *max,8)
        
        ;first need count items for our thread
        counters=0
        For j=0 To waletcounter-1    
          If isInRange8(*BabyArr+j*len,*min,*max)
            counters+1
          EndIf
        Next j
        
        sortjob(Str(i))\ptarr = *BabyArr
        sortjob(Str(i))\sortptarray = *pp
        sortjob(Str(i))\totallines = counters
        
        ;copy rangeB to destination array
        CopyMemory(*min, *pp, len)
        ;copy rangeE to destination array
        CopyMemory(*max, *pp+len, len)
        
        If CreateThread(@sortinghashMinMax(),i)
          totallaunched+1
        EndIf
        add8ui(*re8,1, *rb8 )
        add8(*re8,*Q8, *re8 )
        
        
        *pp+counters*len
        
        ;prntarr("",sortjob(Str(i))\sortptarray, sortjob(Str(i))\totallines)
      Next i
      ;last thread
      CopyMemory(*R8, *re8,8)
      
      ;copy rangeB to temporary array
        CopyMemory(*rb8, *min,8)
        ;copy rangeE to temporary array
        CopyMemory(*re8, *max,8)
      
      ;first need count items for our thread
      counters=0
      For j=0 To waletcounter-1    
        If isInRange8(*BabyArr+j*len,*min,*max)
          counters+1
        EndIf
      Next j  
      
      sortjob(Str(i))\ptarr = *BabyArr
      sortjob(Str(i))\sortptarray = *pp
      sortjob(Str(i))\totallines = counters
       
      ;copy rangeB to destination array
      CopyMemory(*min, *pp, len)
      ;copy rangeE to destination array
      CopyMemory(*max, *pp+len, len)
      
      PrintN("RANGE ["+Str(i)+"]")
      PrintN("FROM "+m_gethex8(*rb8))
      PrintN("  TO "+m_gethex8(*re8))
        
      If CreateThread(@sortinghashMinMax(),i)
        totallaunched+1
      EndIf
      
      Print("Sorting Babys Array>00%")
      While totallaunched
        ;while waitin print persantage
        totalpos=0
        For i=0 To totalthread-1
          totalpos +sortjob(Str(i))\curpos
        Next i
        
        jobcomplete = totalpos*100/waletcounter
        If jobcomplete-prejobcomplete>=1          
          Print(Chr(8)+Chr(8)+Chr(8)+RSet(StrD(jobcomplete,0), 2)+"%")
          prejobcomplete=jobcomplete
        EndIf
        Delay(50)
      Wend
    Else
      PrintN("Sorting Babys Array ["+Str(waletcounter)+"] with [1] thread")
      sortjob(Str(0))\ptarr = *BabyArr
      sortjob(Str(0))\sortptarray = *BabyArrSorted
      sortjob(Str(0))\totallines = waletcounter
      sortinghash(0)
    EndIf
    
    ;********************************
    For i = 0 To waletcounter-1
      toLittleInd32_64(*BabyArrSorted+i*8) 
    Next i
    ;********************************
     ;Saving BIN FILE
    PrintN("Save BIN file:"+filebinname$)
    savedbytes=0
    maxsavebytes=full_size
    If full_size>1024*1024*1024
      maxsavebytes = 1024*1024*1024
    EndIf
    *pp=*BabyArrSorted
    
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
  Else
    If OpenFile(0,filebinname$,#PB_File_NoBuffering)   
      ;Load BIN if exist
      PrintN("Load BIN file:"+filebinname$)  
      totalloadbytes=0
      maxloadbytes=full_size
      If full_size>1024*1024*1024
        maxloadbytes = 1024*1024*1024
      EndIf
      *pp=*BabyArrSorted
      i=0
      Repeat
        PrintN("["+Str(i)+"] chunk:"+Str(maxloadbytes)+"b")
        loadedbytes=ReadData(0, *pp, maxloadbytes)
        totalloadbytes + maxloadbytes
        
        If maxloadbytes<>loadedbytes
          Print("Error when loading: need:"+Str(maxloadbytes)+"b, got:"+Str(loadedbytes)+"b")
          CloseFile(0)
          exit("")
        EndIf
        
        *pp+maxloadbytes
        
        If totalloadbytes<full_size
          If totalloadbytes+maxloadbytes>full_size
            maxloadbytes = full_size-totalloadbytes
            PrintN("Last chunk:"+Str(maxloadbytes)+"b")
          EndIf
        EndIf
        
        
        i+1
      Until totalloadbytes>=full_size
    
      
      CloseFile(0)
      
    Else
      exit("Can`t open file:"+filebinname$)
    EndIf 
  EndIf
  
  ;********************************
    For i = 0 To waletcounter-1
      toLittleInd32_64(*BabyArrSorted+i*8) 
    Next i
    ;********************************
  FreeMemory(*min)

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

  *GiantArrPacked=AllocateMemory((maxnonce+1)*64)
  If *GiantArrPacked=0
    PrintN("Can`t allocate memory for giantpacked array")
    exit("")
  EndIf
    
  filebinname$=Str(threadtotal)+"_"+Str(blocktotal)+"_"+Str(pparam)+"_"+Str(waletcounter)+"_g2.BIN"
  full_size=maxnonce * 64
  
  If FileSize(filebinname$) = -1
    ; file does not exist    
    
    *GiantArr=AllocateMemory((maxnonce+1)*64)
    If *GiantArr=0
      PrintN("Can`t allocate memory for giant array")
      exit("")
    EndIf

    Curve::fillarrayN(*HelperArr , 1024, ADDPUBG\x, ADDPUBG\y)
    ;prntarrBIG(*HelperArr, 16)
    
    job(Str(0))\arr = *HelperArr
    job(Str(0))\NewPointsArr = *GiantArr
    job(Str(0))\totalpoints = maxnonce
    job(Str(0))\pointsperbatch = 1024
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
    If full_size>1024*1024*1024
      maxsavebytes = 1024*1024*1024
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
  Else
    
    If OpenFile(0,filebinname$,#PB_File_NoBuffering)   
      ;Load BIN if exist
      PrintN("Load BIN file:"+filebinname$)  
      totalloadbytes=0
      maxloadbytes=full_size
      If full_size>1024*1024*1024
        maxloadbytes = 1024*1024*1024
      EndIf
      *pp=*GiantArrPacked
      i=0
      Repeat
        PrintN("["+Str(i)+"] chunk:"+Str(maxloadbytes)+"b")
        loadedbytes=ReadData(0, *pp, maxloadbytes)
        totalloadbytes + maxloadbytes
        
        If maxloadbytes<>loadedbytes
          Print("Error when loading: need:"+Str(maxloadbytes)+"b, got:"+Str(loadedbytes)+"b")
          CloseFile(0)
          exit("")
        EndIf
        
        *pp+maxloadbytes
        
        If totalloadbytes<full_size
          If totalloadbytes+maxloadbytes>full_size
            maxloadbytes = full_size-totalloadbytes
            PrintN("Last chunk:"+Str(maxloadbytes)+"b")
          EndIf
        EndIf
        
        
        i+1
      Until totalloadbytes>=full_size
    
      
      CloseFile(0)
      
    Else
      exit("Can`t open file:"+filebinname$)
    EndIf 
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

Procedure GetJob(*key, *pubx,*puby)
  Shared JobMutex
  Shared *GlobKey
  Shared GlobPub
  Shared PUBADDBIG
  Shared *PRKADDBIG
  Shared *CurveP
  Shared *Curveqn
  LockMutex(JobMutex)
  CopyMemory(*GlobKey,*key,32)
  CopyMemory(GlobPub\x,*pubx,32)
  CopyMemory(GlobPub\y,*puby,32)
  Curve::m_ADDPTX64(GlobPub\x,GlobPub\y, GlobPub\x,GlobPub\y,PUBADDBIG\x,PUBADDBIG\y, *CurveP)
  Curve::m_addModX64(*GlobKey,*GlobKey,*PRKADDBIG,*Curveqn)
  UnlockMutex(JobMutex)
EndProcedure


Procedure cuda(gpuid.i)
Protected CudaDevice.i
Protected CudaContext.i
Protected CudaModule.i
Protected CudaFunction.i
Protected err
Protected fname$
Protected DeviceConstantPointer.i
Protected ReturnNumber.i
Protected bytesize.i
Protected myhashrate
Protected DeviceReturnNumber.i
Protected DeviceReturnNumberUnAlign.i
Protected *b
Protected *a
Protected *c
Protected *r
Protected *batch
Protected resser.s
Protected Time1.i
Protected Time1end.i
Protected totalsizeneeded.i
Protected w$, Jobprk$,JobMg$
Protected const_name$
Protected casharrsize.i
Protected desiredarrsize.i
Protected totalpubsizeneeded.i
Protected i.i
Protected pi.i 
Protected winset.l
Protected winpubid.l
Protected wintid.l
Protected isequil.i
Protected Yoffset
Protected *temper
Protected diference
Protected starttime
Protected res.comparsationStructure
Protected winflag, pos_param
Protected puboffset,paramsize
Protected batchsize, a$, rest.HashTableResultStructure, freebytes, totalbytes
Shared  maxnonce, waletcounter, privkey, isruning
Shared sortjob()
Shared *GiantArrPacked, *GpuHT, HT_items,HT_mask, quit, globalquit, *WidthRange, endrangeflag,isreadyjob
Shared *GlobCnt

Shared threadtotal
Shared blocktotal
Shared pparam, keyMutex, listpos
Shared *CurveGX, *CurveGy, *CurveP, *Curveqn, FINDPUBG, ADDPUBG, pubkeyBIG, *PRKADDBIG, PUBADDBIG, REALPUB, *WINKEY
Protected *counterBig, *counterBigTemp, *tempor, *MylocalPrk, MylocalPUB.CoordPoint, TestPUB.CoordPoint, *PRKADD, *px, *py
Protected hashd.d, gpupos
Shared *CenterBig, *CenterX, *CenterY

Shared *PrivBIG

LockMutex(keyMutex)
isruning+1
UnlockMutex(keyMutex)
Delay(5)

gpupos = gpu(gpuid)
*MylocalPrk= *GlobCnt + gpupos * 40

*temper=AllocateMemory(32)
If *temper=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf

*batch=AllocateMemory(32)
If *batch=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf

*r=AllocateMemory(32)
If *r=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*counterBig=AllocateMemory(32)
If *counterBig=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*counterBigTemp=AllocateMemory(32)
If *counterBigTemp=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*tempor=AllocateMemory(32)
If *tempor=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf


MylocalPUB\x=AllocateMemory(32)
If MylocalPUB\x=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
MylocalPUB\y=AllocateMemory(32)
If MylocalPUB\y=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
TestPUB\x=AllocateMemory(32)
If TestPUB\x=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
TestPUB\y=AllocateMemory(32)
If TestPUB\y=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*PRKADD=AllocateMemory(32)
If *PRKADD=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
err = cuDeviceGet(@CudaDevice, gpuid)                 
If err
  exit("cuDeviceGet - "+Str(err))
EndIf
err =  cuCtxCreate_v2(@CudaContext, 4, CudaDevice)    ; CU_CTX_BLOCKING_SYNC = 4 -- cuCtxSynchronize()
If err
  exit("cuCtxCreate - "+Str(err))
EndIf


;fname$="BSGS4_cuda_quad_htchangeble_v2_1_6NEGx2.ptx"

;err=cuModuleLoad(@CudaModule, @fname$)
err=cuModuleLoadData(@CudaModule, ?BSGS4_cuda_quad_htchangeble_v2 ) 
If err
  exit("error cuModuleLoad1-"+Str(err))
EndIf
err=cuModuleGetFunction(@CudaFunction, CudaModule, "_test1")      ;получает адрес функции
If err
  exit("error cuModuleGetFunction1-"+Str(err))
EndIf

err=cuFuncSetCacheConfig 	(CudaFunction,2) 
If err
 exit("error cuFuncSetCacheConfig1-"+Str(err))
EndIf
                      

casharrsize = maxnonce 
desiredarrsize = waletcounter 
paramsize=128; do not change
totalsizeneeded = casharrsize * 96+#alignMemoryGpu
puboffset = totalsizeneeded+#alignMemoryGpu-(totalsizeneeded % #alignMemoryGpu)
puboffset = puboffset + paramsize
totalsizeneeded = totalsizeneeded + paramsize
totalpubsizeneeded = HT_items * #HashTablesz  + waletcounter * #HashTableSizeHash

batchsize = blocktotal * threadtotal * pparam


;PrintN("---------------")
;PrintN("Nonces per Thread: " + Str(pparam) )
;PrintN("Threads in block: " + Str(threadtotal) )
;PrintN("Blocks in grid: " + Str(blocktotal) )
;PrintN("Total nonces: " + Str(batchsize) )
;PrintN("Baby steps: " + Str(desiredarrsize) )
;PrintN("---------------")


*a=AllocateMemory(256)
If *a=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*b=AllocateMemory(paramsize) 
If *b=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf
*c=AllocateMemory(256) 
resser.s=Space(4096) 

;PrintN("GPU #"+Str(gpuid)+" GiantsBuff: "+Str(casharrsize)+" points(96b) = "+  StrD(totalsizeneeded/1024/1024,3)+"Mb")
;PrintN("GPU #"+Str(gpuid)+"     HTbuff: "+Str(desiredarrsize)+" points(8b) = "+  StrD(totalpubsizeneeded/1024/1024,3)+"Mb")
err=cuMemGetInfo_v2 	(@freebytes,@totalbytes) 	
If err
 exit("error cuMemGetInfo_v2-"+Str(err))
EndIf
PrintN("GPU #"+Str(gpuid)+" Free memory: "+Str(freebytes/1048576)+"Mb")
PrintN("GPU #"+Str(gpuid)+" Total memory: "+Str(totalbytes/1048576)+"Mb")
PrintN("GPU #"+Str(gpuid)+" TotalBuff: "+ StrD((totalsizeneeded+totalpubsizeneeded)/1024/1024,3)+"Mb")
;PrintN("---------------")



err=cuMemAlloc_v2(@DeviceReturnNumberUnAlign, (totalsizeneeded+totalpubsizeneeded+paramsize) + #alignMemoryGpu)  ;аллоцирует 200 байт на gpu
If err
  exit("error cuMemAlloc-"+Str(err))
EndIf
DeviceReturnNumber=DeviceReturnNumberUnAlign+#alignMemoryGpu-(DeviceReturnNumberUnAlign % #alignMemoryGpu)




err=cuParamSetSize(CudaFunction, 8)           
If err
  exit("error cuParamSetSize-"+Str(err))
EndIf
err=cuParamSeti(CudaFunction, 0, PeekL(@DeviceReturnNumber))  
If err
  exit("error cuParamSeti-"+Str(err))
EndIf
err=cuParamSeti(CudaFunction,4, PeekL(@DeviceReturnNumber+4))  
If err
  exit("error cuParamSeti-"+Str(err))
EndIf
err=cuFuncSetBlockShape(CudaFunction, threadtotal,1,1)
If err
  exit("error cuFuncSetBlockShape-"+Str(err))
EndIf



;For i=0 To 6
  ;err = cuFuncGetAttribute 	(@pi,i,CudaFunction) 
  ;If err
    ;exit("cuFuncGetAttribute - "+Str(err))
  ;EndIf
  ;PrintN (a$(i)+Str(pi))
;Next i
;PrintN("---------------")


;структура глобальной памяти
;0-31 системные 0:3 результат гпу, 4:7 пубкаунтер 8:11 pparam 12:15 -maxnonce  16[при солюшене] - пубкейайди, нить
;32-95 magicpoint (pubkey for operation)
;96-104 puboffset (offset for baby points array)
;104-111 HT size
;112-119 HT mask
;128-N массив точек X,Y координаты по 64 б
;за ним идет массив бебистеп


PokeL(*b+4,waletcounter)
;поставим pparam
PokeL(*b+8,pparam)

PokeL(*b+12,casharrsize)

PokeI(*b+96,puboffset)

PokeI(*b+104,HT_items * #HashTablesz)

PokeL(*b+112,HT_mask)



;copy params to GPU
cuMemcpyHtoD_v2(DeviceReturnNumber,*b, paramsize)  
If err
  exit("error cuMemcpyHtoD-"+Str(err))
EndIf
;copy giant points to GPU
cuMemcpyHtoD_v2(DeviceReturnNumber+paramsize,*GiantArrPacked, maxnonce*64)  
If err
  exit("error cuMemcpyHtoD-"+Str(err))
EndIf

;copy packed HT(baby points) to GPU
cuMemcpyHtoD_v2(DeviceReturnNumber + puboffset,*GpuHT, totalpubsizeneeded)  
If err
  exit("error cuMemcpyHtoD-"+Str(err))
EndIf

;-iteration BEGIN

LockMutex(keyMutex)
isruning-1
UnlockMutex(keyMutex)


Repeat 
PokeQ(*MylocalPrk+32,0)  
While isreadyjob=0
  Delay(1)
Wend
 
LockMutex(keyMutex)
isruning+1
UnlockMutex(keyMutex)
Delay(10)

a$=RSet(Hex(batchsize*2), 64,"0");due to use simmetry in addition points and x2GS
Curve::m_sethex32(*batch, @a$ )


a$=RSet(Hex(0), 64,"0")
Curve::m_sethex32(*counterBig, @a$ )
Curve::m_sethex32(*counterBigTemp, @a$ )

Time1 = ElapsedMilliseconds()
starttime =Date()

GetJob(*MylocalPrk, MylocalPUB\x, MylocalPUB\y)
;******

;******

;PrintN("CENTER-"+Curve::m_gethex32(*CenterBig))
;PrintN("CENTERpub-"+Curve::m_gethex32(*CenterX)+","+Curve::m_gethex32(*CenterY))
;Input()
;Curve::m_subModX64(*MylocalPrk,*MylocalPrk,*CenterBig,*Curveqn)

;PrintN("GPU#"+Str(gpuid)+" Cnt:"+Curve::m_gethex32(*MylocalPrk))









CopyMemory(MylocalPUB\x,*b+32,32)
CopyMemory(MylocalPUB\y,*b+64,32)
isequil=0

;PrintN(": "+Curve::m_gethex32(MylocalPUB\x) + " "+ Curve::m_gethex32(MylocalPUB\y))
;foundinarr8(*b+32+24, *BabyArrSorted, 0, waletcounter, @res.comparsationStructure)
;res\pos = compareHTpack(*b+32+24)
  ;If res\pos<>-1
    ;PrintN("FOUND on CPU!!!")  
    ;isequil = 1
  ;Else
    ;isequil = 0
  ;EndIf

*px=MylocalPUB\x
*py=MylocalPUB\y
 
Repeat
  

  ;PrintN("WorkPub: "+Curve::m_gethex32(*px)+","+Curve::m_gethex32(*py))
  res\pos = compareHTpack(*px+24)
  If res\pos<>-1
    ;PrintN("FOUND on CPU!!!")  
    isequil = 1
  Else
    isequil = 0
  EndIf
  If ElapsedMilliseconds()-Time1>2000
    
    Time1end = ElapsedMilliseconds()-Time1
    Time1 = ElapsedMilliseconds()
    Curve::m_subX64(*tempor,*counterBig,*counterBigTemp)
    
    diference =PeekQ(*tempor)
    mul8ui(*tempor,1000,*tempor)
    
   
    
    
    
    If Time1end
      div8(*tempor,Time1end,*tempor,*r)
      
      myhashrate =PeekQ(*tempor)
    Else
      myhashrate = 0
    EndIf
    CopyMemory(*counterBig, *counterBigTemp,32)
    ;save hashrate
    PokeQ(*MylocalPrk+32,myhashrate)
    
    hashd= Log(myhashrate)/Log(2)
    ;PrintN("GPU#"+Str(gpuid)+" Cnt:"+Curve::m_gethex32(*MylocalPrk)+" "+Str(myhashrate/1024/1024)+"MKey/s x"+Str(waletcounter) +" 2^"+ StrD(hashd,2)+" x2^"+StrD(wald,0)+"=2^"+StrD(hashd+wald,2))
    
  EndIf


  If isequil=0
    
    ;CopyMemory(MylocalPUB\x,*b+32,32)
    ;CopyMemory(MylocalPUB\y,*b+64,32)
    move32b_(p.p_px, p.p_b,0,32)
    move32b_(p.p_py, p.p_b,0,64)
    
    swap32(*b+32)
    swap32(*b+64)
    
    ;копируем magic point в gpu ГЛОБАЛЬНУЮ память
    cuMemcpyHtoD_v2(DeviceReturnNumber+32,*b+32, 64)  
    If err
      exit("error cuMemcpyHtoD-"+Str(err))
    EndIf
    
    
   
    
    err=cuLaunchGrid(CudaFunction, blocktotal, 1)
    If err
      exit("error cuLaunchGrid-"+Str(err))
    EndIf
    
    err=cuCtxSynchronize()
    
    If err
      exit("error cuCtxSynchronize-"+Str(err))  
    EndIf
    
    
    
    err=cuMemcpyDtoH_v2(*a, DeviceReturnNumber, 4)  ;копирует 4 байта из gpu
    If err
        exit("error cuMemcpyDtoH-"+Str(err))
    EndIf
      
    ;check if win set
    winset = PeekL(*a)
    
    
    If winset>0
      
      err = cuMemcpyDtoH_v2(*a, DeviceReturnNumber+16, winset*8)
      If err
        exit("cuMemcpyDtoH - "+Str(err))
      EndIf
      
      
      winflag=0
      For i = 0 To winset-1
            winpubid = PeekL(*a+8*i)        
            wintid = PeekL(*a+8*i+4)
            
            
            
            
            ;PrintN("***********GPU#"+Str(gpuid)+"************")
            ;PrintN("Total solutions: "+Str(winset))            
            ;PrintN("Winnonce: "+Str(wintid))            
            ;PrintN("ArrayID: "+Str(winpubid))   ;pos in sorted array   
            ;PrintN("At: "+Curve::m_gethex32(*MylocalPrk))
            ;PrintN("Pub: "+Curve::m_gethex32(MylocalPUB\x)+" , "+ Curve::m_gethex32(MylocalPUB\y))
            ;PrintN("Fnd: "+Curve::m_gethex32(FINDPUBG\x))
            
            pos_param = wintid - Int(wintid / pparam) * pparam
            ;PrintN("pos_param:"+Str(pos_param))
            
            If winpubid=4
              a$=RSet(Hex((wintid+1) * (waletcounter*2)), 64,"0")
              Curve::m_sethex32(*tempor, @a$ )
              Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
              ;PrintN(Curve::m_gethex32(TestPUB\x)+","+Curve::m_gethex32(TestPUB\y))
              If Curve::m_check_equilX64(*px,TestPUB\x)=1 
                If Curve::m_check_equilX64(*py,TestPUB\y)=0                 
                  Curve::m_subModX64(*tempor,*Curveqn,*tempor,*Curveqn)
                EndIf
                ;PrintN(Curve::m_gethex32(*tempor))
                
                
                
                Curve::m_addModX64(*temper,*tempor,*CenterBig,*Curveqn)
                Curve::m_addModX64(*temper,*MylocalPrk,*temper,*Curveqn)
                Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *temper,*CurveP)
                
                ;PrintN(Curve::m_gethex32(TestPUB\x)+","+Curve::m_gethex32(TestPUB\y))
                If Curve::m_check_equilX64(FINDPUBG\x,TestPUB\x)=0                
                  Curve::m_subModX64(*temper,*tempor,*CenterBig,*Curveqn)
                  Curve::m_addModX64(*temper,*MylocalPrk,*temper,*Curveqn)
                  Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *temper,*CurveP)
                EndIf
                ;PrintN(Curve::m_gethex32(TestPUB\x)+","+Curve::m_gethex32(TestPUB\y))
                
                Curve::m_addModX64(*temper,*PrivBIG,*temper,*Curveqn) 
                Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *temper,*CurveP)
                If Curve::m_check_equilX64(REALPUB\x,TestPUB\x)=1
                  If Curve::m_check_equilX64(REALPUB\y,TestPUB\y)=0                 
                    Curve::m_subModX64(*temper,*Curveqn,*temper,*Curveqn)
                  EndIf
                  Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *temper,*CurveP)
                  CopyMemory(*temper, *tempor, 32)
                  ;PrintN("***********GPU#"+Str(gpuid)+"************")
                  ;PrintN("KEY["+Str(listpos)+"]: 0x"+Curve::m_gethex32(*tempor)) 
                  ;PrintN(RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(TestPUB\x)+ Curve::m_gethex32(TestPUB\y)))
                  ;PrintN("****************************")   
                  ;PrintN("Found in "+Str(Date()-starttime)+" seconds")
                  winflag=1
                  Break 
                Else
                   ;PrintN("False")
                EndIf
              Else
                ;PrintN("False")
              EndIf
              
              
              
            Else
              ;----
              a$=RSet(Hex(wintid+1), 64,"0")
            Curve::m_sethex32(*tempor, @a$ )
            Curve::m_PTMULX64(TestPUB\x, TestPUB\y, ADDPUBG\x, ADDPUBG\y, *tempor,*CurveP)

            If winpubid=2            
              Curve::m_subModX64(TestPUB\y,*CurveP,TestPUB\y,*CurveP)              
            EndIf
            pos_param  = winpubid
            Curve::m_ADDPTX64(TestPUB\x,TestPUB\y,MylocalPUB\x,MylocalPUB\y,TestPUB\x,TestPUB\y,*CurveP)
            
            
            
              winpubid =  compareHTpack(TestPUB\x+24)
              If winpubid<>-1
                ;PrintN("SolutionID: "+Str(winpubid))
              Else
                PrintN("Can`t found solution ((")
              EndIf
              
              
              a$=RSet(Hex((wintid+1)*waletcounter*2+winpubid+1), 64,"0")
              Curve::m_sethex32(*tempor, @a$ )
              
              If pos_param=1
                ;***
                Curve::m_addModX64(*tempor,*tempor,*CenterBig,*Curveqn)
                ;***               
                Curve::m_addModX64(*tempor,*MylocalPrk,*tempor,*Curveqn)
               
              Else
                Curve::m_subModX64(*tempor,*tempor,*CenterBig,*Curveqn)
                Curve::m_subModX64(*tempor,*MylocalPrk,*tempor,*Curveqn)
                
              EndIf
                ;2c675b852189a20
                
                ;2c675b852189a21
                ;PrintN("At: "+Curve::m_gethex32(*tempor))
                
              Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
              ;PrintN(">>: "+Curve::m_gethex32(TestPUB\x))            
              If Curve::m_check_equilX64(FINDPUBG\x,TestPUB\x)=1
                ;PrintN("Yay!!>"+Curve::m_gethex32(*tempor))
                Curve::m_addModX64(*tempor,*PrivBIG,*tempor,*Curveqn) 
                
                Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                If Curve::m_check_equilX64(REALPUB\x,TestPUB\x)=1
                  
                  If Curve::m_check_equilX64(REALPUB\y,TestPUB\y)=0
                    Print("<*>")
                    Curve::m_subModX64(*tempor,*Curveqn,*tempor,*Curveqn)
                    Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                  EndIf
                  ;PrintN("***********GPU#"+Str(gpuid)+"************")
                  ;PrintN("KEY["+Str(listpos)+"]: 0x"+Curve::m_gethex32(*tempor)) 
                  ;PrintN(RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(TestPUB\x)+ Curve::m_gethex32(TestPUB\y)))
                  ;PrintN("****************************")   
                  ;PrintN("Found in "+Str(Date()-starttime)+" seconds")
                  winflag=1
                  Break               
                 
                Else
                  
                  ;PrintN("False collision")
                  ;PrintN(">"+Curve::m_gethex32(*tempor)) 
                  ;PrintN(Curve::m_gethex32(FINDPUBG\x))
                  ;PrintN(Curve::m_gethex32(TestPUB\x))
                EndIf
                
              Else
                a$=RSet(Hex((wintid)*waletcounter*2+(waletcounter*2-winpubid-1)), 64,"0")
                Curve::m_sethex32(*tempor, @a$ )
                If pos_param=1
                  ;***
                  Curve::m_addModX64(*tempor,*tempor,*CenterBig,*Curveqn)
                  ;***               
                  Curve::m_addModX64(*tempor,*MylocalPrk,*tempor,*Curveqn)
                 
                Else
                  Curve::m_subModX64(*tempor,*tempor,*CenterBig,*Curveqn)
                  Curve::m_subModX64(*tempor,*MylocalPrk,*tempor,*Curveqn)
                  
                EndIf            
                Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                ;PrintN("At2: "+Curve::m_gethex32(*tempor))
                 ;PrintN(">>2: "+Curve::m_gethex32(TestPUB\x)) 
                If Curve::m_check_equilX64(FINDPUBG\x,TestPUB\x)=1
                  ;key was under zero
                  ;PrintN("Yay!!>"+Curve::m_gethex32(*tempor))
                  Curve::m_addModX64(*tempor,*PrivBIG,*tempor,*Curveqn) 
                
                  Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                  If Curve::m_check_equilX64(REALPUB\x,TestPUB\x)=1
                    
                    If Curve::m_check_equilX64(REALPUB\y,TestPUB\y)=0
                      Print("<*>")
                      Curve::m_subModX64(*tempor,*Curveqn,*tempor,*Curveqn)
                      Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                    EndIf
                    ;PrintN("***********GPU#"+Str(gpuid)+"************")
                    ;PrintN("KEY["+Str(listpos)+"]: 0x"+Curve::m_gethex32(*tempor)) 
                    ;PrintN(RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(TestPUB\x)+ Curve::m_gethex32(TestPUB\y)))
                    ;PrintN("****************************")   
                    ;PrintN("Found in "+Str(Date()-starttime)+" seconds")
                    winflag=1
                    Break               
                   
                  Else
                    
                    ;PrintN("False collision")
                    ;PrintN(">"+Curve::m_gethex32(*tempor)) 
                    ;PrintN(Curve::m_gethex32(FINDPUBG\x))
                    ;PrintN("** "+Curve::m_gethex32(TestPUB\x))
                  EndIf
                Else
                  a$=RSet(Hex((wintid)*waletcounter*2+(waletcounter*2-winpubid+1)), 64,"0")
                  Curve::m_sethex32(*tempor, @a$ )
                  If pos_param=1
                    ;***
                    Curve::m_addModX64(*tempor,*tempor,*CenterBig,*Curveqn)
                    ;***               
                    Curve::m_addModX64(*tempor,*MylocalPrk,*tempor,*Curveqn)
                   
                  Else
                    Curve::m_subModX64(*tempor,*tempor,*CenterBig,*Curveqn)
                    Curve::m_subModX64(*tempor,*MylocalPrk,*tempor,*Curveqn)
                    
                  EndIf   
                  Curve::m_subModX64(*tempor,*Curveqn,*tempor,*Curveqn)
                  Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                  ;PrintN("At3: "+Curve::m_gethex32(*tempor))
                  ;PrintN(">>3: "+Curve::m_gethex32(TestPUB\x)) 
                
                  If Curve::m_check_equilX64(FINDPUBG\x,TestPUB\x)=1
                    ;key was under zero
                    ;PrintN("Yay!!>"+Curve::m_gethex32(*tempor))
                    Curve::m_addModX64(*tempor,*PrivBIG,*tempor,*Curveqn) 
                  
                    Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                    If Curve::m_check_equilX64(REALPUB\x,TestPUB\x)=1
                      
                      If Curve::m_check_equilX64(REALPUB\y,TestPUB\y)=0
                        Print("<*>")
                        Curve::m_subModX64(*tempor,*Curveqn,*tempor,*Curveqn)
                        Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                      EndIf
                      ;PrintN("***********GPU#"+Str(gpuid)+"************")
                      ;PrintN("KEY["+Str(listpos)+"]: 0x"+Curve::m_gethex32(*tempor)) 
                      ;PrintN(RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(TestPUB\x)+ Curve::m_gethex32(TestPUB\y)))
                      ;PrintN("****************************")   
                      ;PrintN("Found in "+Str(Date()-starttime)+" seconds")
                      winflag=1
                      Break               
                     
                    Else
                      
                      ;PrintN("False collision")
                      ;PrintN(">"+Curve::m_gethex32(*tempor)) 
                      ;PrintN(Curve::m_gethex32(FINDPUBG\x))
                      ;PrintN("** "+Curve::m_gethex32(TestPUB\x))
                    EndIf
                  EndIf
                EndIf
                
             
                
              EndIf
            EndIf
            
            
           
            
      Next i
      ;clear collision
      PokeL(*b,0)
      cuMemcpyHtoD_v2(DeviceReturnNumber,*b, 4)  
      If err
        exit("error cuMemcpyHtoD-"+Str(err))
      EndIf
      
      If winflag=0
        winset=0
      EndIf
    EndIf
  Else
    ;PrintN("*******CPU/GPU#"+Str(gpuid)+"**************")
    
    ;PrintN("At: "+Curve::m_gethex32(*MylocalPrk))
    ;PrintN("Pub: "+Curve::m_gethex32(MylocalPUB\x)+" , "+ Curve::m_gethex32(MylocalPUB\y))
    
    ;winpubid =  findsolution(*BabyArrSorted+res\pos*8, *BabyArr, waletcounter,8)
    winpubid=res\pos
            If winpubid<>-1
              ;PrintN("SolutionID: "+Str(winpubid))
            Else
              PrintN("Can`t found solution ((")
            EndIf
            
            
            a$=RSet(Hex(winpubid+1), 64,"0")
            Curve::m_sethex32(*tempor, @a$ )   
            Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
            If Curve::m_check_equilX64(*px,TestPUB\x)=1
              If Curve::m_check_equilX64(*py,TestPUB\y)=0
                Curve::m_subModX64(*tempor,*Curveqn,*tempor,*Curveqn)
              EndIf
              ;***
              Curve::m_addModX64(*tempor,*tempor,*CenterBig,*Curveqn)
              ;***
              Curve::m_addModX64(*tempor,*MylocalPrk,*tempor,*Curveqn)
              
              Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)              
              If Curve::m_check_equilX64(FINDPUBG\x,TestPUB\x)=1
                ;ok X points is equil
                If Curve::m_check_equilX64(FINDPUBG\y,TestPUB\y)=0
                  ;Print("<*>")
                  Curve::m_subModX64(*tempor,*Curveqn,*tempor,*Curveqn)
                  
                EndIf
                ;PrintN("Yay!!>"+Curve::m_gethex32(*tempor))
                CopyMemory(*tempor, *temper,32)
                Curve::m_addModX64(*tempor,*PrivBIG,*tempor,*Curveqn) 
                
                
                Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                    If Curve::m_check_equilX64(REALPUB\x,TestPUB\x)=1
                      
                      If Curve::m_check_equilX64(REALPUB\y,TestPUB\y)=0
                        ;Print("<*>")
                        Curve::m_subModX64(*tempor,*Curveqn,*tempor,*Curveqn)
                        Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                      EndIf
                      ;PrintN("KEY["+Str(listpos)+"]: 0x"+Curve::m_gethex32(*tempor)) 
                      ;PrintN(RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(TestPUB\x)+ Curve::m_gethex32(TestPUB\y)))
                      ;PrintN("****************************")   
                      ;PrintN("Found in "+Str(Date()-starttime)+" seconds")
                      winset=1
                      isequil=0
                      Break               
                     
                    Else
                      
                      ;Print("<*>")
                      CopyMemory(*temper, *tempor,32)
                      Curve::m_subModX64(*tempor,*tempor,*PrivBIG,*Curveqn)
                      Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                      If Curve::m_check_equilX64(REALPUB\x,TestPUB\x)=1
                      
                        If Curve::m_check_equilX64(REALPUB\y,TestPUB\y)=0
                          ;Print("<*>")
                          Curve::m_subModX64(*tempor,*Curveqn,*tempor,*Curveqn)
                          Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                        EndIf
                        ;PrintN("KEY["+Str(listpos)+"]: 0x"+Curve::m_gethex32(*tempor)) 
                        ;PrintN(RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(TestPUB\x)+ Curve::m_gethex32(TestPUB\y)))
                        ;PrintN("****************************")   
                        ;PrintN("Found in "+Str(Date()-starttime)+" seconds")
                        winset=1
                        isequil=0
                        Break               
                       
                      Else
                       
                        
                       ; PrintN("False collision")
                        ;PrintN(">"+Curve::m_gethex32(*tempor)) 
                        ;PrintN(Curve::m_gethex32(FINDPUBG\x))
                        ;PrintN(Curve::m_gethex32(TestPUB\x))
                        winset=0
                        isequil=0
                      EndIf
                      
                    EndIf
                
                
              Else
                ;maybe under zero?
                a$=RSet(Hex(winpubid+1), 64,"0")
                Curve::m_sethex32(*tempor, @a$ )    
                ;***
                Curve::m_addModX64(*tempor,*tempor,*CenterBig,*Curveqn)
                ;***
                Curve::m_subModX64(*tempor,*tempor,*MylocalPrk,*Curveqn)
                Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                If Curve::m_check_equilX64(FINDPUBG\x,TestPUB\x)=1
                  
                  If Curve::m_check_equilX64(FINDPUBG\y,TestPUB\y)=0
                    ;Print("<*>")
                    Curve::m_subModX64(*tempor,*Curveqn,*tempor,*Curveqn)
                  EndIf 
                  Curve::m_addModX64(*tempor,*PrivBIG,*tempor,*Curveqn) 
                  Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                  If Curve::m_check_equilX64(REALPUB\x,TestPUB\x)=1
                      
                      If Curve::m_check_equilX64(REALPUB\y,TestPUB\y)=0
                        ;Print("<*>")
                        Curve::m_subModX64(*tempor,*Curveqn,*tempor,*Curveqn)
                        Curve::m_PTMULX64(TestPUB\x, TestPUB\y, *CurveGX, *CurveGY, *tempor,*CurveP)
                      EndIf
                      ;PrintN("KEY["+Str(listpos)+"]: 0x"+Curve::m_gethex32(*tempor)) 
                      ;PrintN(RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(TestPUB\x)+ Curve::m_gethex32(TestPUB\y)))
                      ;PrintN("****************************")   
                      ;PrintN("Found in "+Str(Date()-starttime)+" seconds")
                      winset=1
                      isequil=0
                      Break               
                     
                    Else
                      
                      ;Print("<*>")
                      Curve::m_subModX64(*tempor,*Curveqn,*tempor,*Curveqn)
                      
                      ;PrintN("False collision")
                      ;PrintN(">"+Curve::m_gethex32(*tempor)) 
                      ;PrintN(Curve::m_gethex32(FINDPUBG\x))
                      ;PrintN(Curve::m_gethex32(TestPUB\x))
                      winset=0
                      isequil=0
                    EndIf
                Else
                  ;PrintN("False collision")
                  ;PrintN(Curve::m_gethex32(FINDPUBG\x))
                  ;PrintN(Curve::m_gethex32(TestPUB\x))
                  winset=0
                  isequil=0
                EndIf
                
                
              EndIf
            Else
              Exit("Something wrong with calculation")
              
            EndIf
            
    
  EndIf
  
    If endrangeflag
      If Curve::m_check_less_more_equilX64(*MylocalPrk, *WidthRange )=2
        ;PrintN("GPU#"+Str(gpuid)+" Reached end of space")
        PokeQ(*MylocalPrk+32,0)
        Break
      EndIf
    EndIf 
    GetJob(*MylocalPrk, MylocalPUB\x, MylocalPUB\y)
    Curve::m_addX64(*counterBig, *counterBig,*batch)
    
     
Until winset Or quit

If winset
  CopyMemory(*tempor,*WINKEY,32)
EndIf

LockMutex(keyMutex)
isruning-1
UnlockMutex(keyMutex)
If winset
  quit=1
EndIf

While isreadyjob
  Delay(1)
Wend
PrintN("GPU#"+Str(gpuid)+" job finished")

Until globalquit

cuMemFree_v2(DeviceReturnNumberUnAlign)
cuCtxDestroy_v2(CudaContext)
FreeMemory(*batch)
FreeMemory(*r)
FreeMemory(*counterBig)
FreeMemory(*counterBigTemp)
FreeMemory(*tempor)
FreeMemory(MylocalPUB\x)
FreeMemory(MylocalPUB\y)
FreeMemory(TestPUB\x)
FreeMemory(TestPUB\y)
FreeMemory(*PRKADD)
FreeMemory(*a)
FreeMemory(*b)
FreeMemory(*c)
FreeMemory(*temper)


PrintN("GPU#"+Str(gpuid)+" thread finished")
EndProcedure

Procedure HashTableInsert(*hash, position)  
  Protected *a=AllocateMemory(4), offset, hashcut, val, *pointer, sz, *contentpointer
  Shared TableMutex, *Table, *PointerTable, HT_mask, HT_total_hashes, HT_items_with_collisions, HT_max_collisions, HT_total_items, initHTsize
  
  ;PrintN("hash insert>"+m_gethex8(*hash))
  
  hashcut = ValueL(*hash) & HT_mask 
  *pointer = *Table + hashcut*#HashTablesz
  
  LockMutex(TableMutex)
  
  sz = ValueL(*pointer)
  offset = hashcut*#Pointersz
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
    ;increase counter
    INCvalue32(*pointer)
    HT_total_hashes + 1
  Else
    ;PrintN("Hash #"+Hex(hashcut)+" has "+Str(sz)+" items")
    ;PrintN("Need realocate")
    *contentpointer = PeekI(*PointerTable+offset)
    If sz>=initHTsize
      *contentpointer = ReAllocateMemory(*contentpointer, (sz+1)*#HashTableSizeItems)
      If Not *contentpointer     
        exit("Can`t reallocate memory")
      EndIf      
      ;store new pointer to PointTable
      PokeI(*PointerTable+offset, *contentpointer) 
       
    EndIf
    CopyMemory(*hash+4, *contentpointer+ sz*#HashTableSizeItems, #HashTableSizeHash)
    PokeL(*contentpointer+ sz*#HashTableSizeItems+#HashTableSizeHash,position)
    ;increase counter
    INCvalue32(*pointer)
    
    HT_items_with_collisions + 1
    If ValueL(*pointer)>HT_max_collisions
      HT_max_collisions = ValueL(*pointer)
    EndIf
  EndIf
  
  
  HT_total_items+1  
  UnlockMutex(TableMutex) 
  FreeMemory(*a)
EndProcedure

Procedure HashTableRead(*hash, *res.HashTableResultStructure)  
  Protected offset, hash
  Shared *Table, *PointerTable, HT_mask
  
  hash = ValueL(*hash) & HT_mask   
  *res\size = ValueL(*Table + hash * #HashTablesz)
  offset = hash*#Pointersz  
  *res\contentpointer = PeekI(*PointerTable + offset)
  ;PrintN("Hash:" +Str(hash)+" sz:"+Str(*res\size))
EndProcedure

Procedure HashTableSammary() 
  Protected totalbytes
  Shared HT_total_items, HT_total_hashes, HT_max_collisions, HT_items_with_collisions, HT_mask, HT_items, HT_POW
  PrintN("----------HashTable Info----------")
  PrintN("Table size: 2^"+Str(HT_POW)+"x"+Str(#HashTablesz)+"="+Str(HT_items * #HashTablesz)+" bytes")
  PrintN("Table mask: "+Hex(HT_mask))
  PrintN("Table used: "+StrD(HT_total_hashes*100/HT_items,2)+"%")
  PrintN("Total unique hashes: "+Str(HT_total_hashes)+" = "+StrD(HT_total_hashes*100/HT_total_items,1)+"%")
  PrintN("Total hashes: "+Str(HT_total_items)+"="+Str(HT_total_items)+"x"+Str(#HashTableSizeItems)+"="+Str(HT_total_items * #HashTableSizeItems)+" bytes")
  totalbytes = HT_total_items*#HashTableSizeItems + HT_items * #HashTablesz
  PrintN("Total "+Str(totalbytes)+" bytes = "+StrD(totalbytes/1024/1024,1)+"Mb")
  PrintN("Total colisions:"+Str(HT_items_with_collisions)+" = "+StrD(HT_items_with_collisions*100/HT_total_items,1)+"%")  
  PrintN("Max. colisions:"+Str(HT_max_collisions))
  PrintN("----------------------------------")
  
  ProcedureReturn HT_total_items*#HashTableSizeItems+HT_total_hashes * #HashTablesz
EndProcedure

Procedure GenHashTable()
  Protected i,*ponter, a, persent, nxt
  Shared *BabyArr,waletcounter
  *ponter = *BabyArr
  Print("Add baby points to HashTable..00%")
  
  a=waletcounter/100
  nxt=a
  For i= 0 To waletcounter-1
    HashTableInsert(*ponter,i)
    *ponter+8
    If i>=nxt
      persent+1
      Print(Chr(8)+Chr(8)+Chr(8)+RSet(Str(persent), 2, "0")+"%")
      nxt+a
    EndIf
  Next i
  PrintN(Chr(8)+Chr(8)+Chr(8)+"100%")
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
  While pos<totalines-1 And err=0
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
      EndIf
      ;For i =0 To totalines-1
        ;Debug ("["+Str(i)+"] "+get64bithash(*arr + i * #HashTableSizeItems))  
      ;Next i
  Wend
  UnlockMutex(TableMutex)
  If err   
    PrintN("Value exist!!!>"+Hex(Valuel(*INShash)))
    For i =0 To totalines-1
      PrintN ("["+Str(i)+"] "+Hex(Valuel(*arr + i * #HashTableSizeItems))+" ("+Hex(Valuel(*arr + i * #HashTableSizeItems+#HashTableSizeHash))+")")  
    Next i
    exit("Try increase -htsz")
  EndIf
  FreeMemory(*temp)
EndProcedure 

Procedure sortWholeHashTable(*arr, totalitems)
Protected i, res.HashTableResultStructure
  For i =0 To totalitems-1 
    HashTableRead(@i, @res) 
    If res\size    
      sortHashTable32bit(res\contentpointer, res\size)
    EndIf    
  Next i
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
    Else
      If i<>0
        PrintN("Warning!!!")
        PrintN("min set : "+Hex(ValueL(*min)))
        PrintN("  value : "+Hex(ValueL(*arr + i * #HashTableSizeItems)))
        err=i
        Break
      EndIf
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

Procedure checkWholeHashTableContent()
  Protected  res.HashTableResultStructure, i, err
  Shared HT_items, HT_total_items
  
  For i =0 To HT_items-1 
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
   EndIf
   ProcedureReturn err
EndProcedure

Procedure compareWithHashtable(*hash)
  Protected  res.HashTableResultStructure, rescmp.comparsationStructure
  rescmp\pos=-1
  HashTableRead(*hash, @res) 
  ;PrintN( "cmp hash:"+m_gethex8(*hash))
  ;PrintN( "sz:"+Str(res\size))
  If res\size    
    findInHashTable32bitSimple(*hash+4, res\contentpointer, 0, res\size, @rescmp)
    ;PrintN("Content pos: "+Str(rescmp\pos))    
  EndIf
  ProcedureReturn rescmp\pos
EndProcedure

Procedure ReadHTpack(*hash, *arr, *res.HashTableResultStructure)  
  Protected offset, hash
  Shared HT_mask, HT_items
  
  hash = ValueL(*hash) & HT_mask   
  *res\size = ValueL(*arr + hash * #HashTablesz)  
  *res\contentpointer =*arr + HT_items * #HashTablesz + ValueL(*arr + hash * #HashTablesz+4) * #HashTableSizeItems
  ;PrintN("Hash:" +Str(hash)+" sz:"+Str(*res\size))
EndProcedure

Procedure compareHTpack(*hash)
  Protected  res.HashTableResultStructure, rescmp.comparsationStructure
  Shared *GpuHT
  rescmp\pos=-1
  ReadHTpack(*hash, *GpuHT, @res) 
  ;PrintN( "cmp hash:"+m_gethex8(*hash))
  ;PrintN( "sz:"+Str(res\size))
  If res\size
    findInHashTable32bitSimple(*hash+4, res\contentpointer, 0, res\size, @rescmp)
    ;PrintN("Content pos: "+Str(rescmp\pos)+", "+Str(ValueL(res\contentpointer+rescmp\pos*#HashTableSizeItems+#HashTableSizeHash))) 
    If rescmp\pos<>-1
      rescmp\pos=ValueL(res\contentpointer+rescmp\pos*#HashTableSizeItems+#HashTableSizeHash)
    EndIf
  EndIf
  ProcedureReturn rescmp\pos
EndProcedure

Procedure packHT()   
  Protected *ptr, i, hash, res.HashTableResultStructure, offset, counter
  Shared *Table, *GpuHT, *PointerTable, HT_items, HT_total_items , HT_mask 
  ;for CPU we store xpoint position and xpoint
  CopyMemory(*Table, *GpuHT, HT_items*#HashTablesz)
  *ptr=*GpuHT+HT_items*#HashTablesz
  counter=0
  For i =0 To HT_items-1 
    hash = ValueL(@i) & HT_mask   
    res\size = ValueL(*Table + hash * #HashTablesz)
    
    If res\size>0
      offset = hash*#Pointersz  
      res\contentpointer = PeekI(*PointerTable + offset)          
      CopyMemory(res\contentpointer, *ptr + counter * #HashTableSizeItems,  res\size*#HashTableSizeItems)
      PokeL (*GpuHT + hash * #HashTablesz+4, counter) 
      ;PrintN("Hash:" +Str(hash)+" sz:"+Str(res\size)+ "offset: "+Str(*ptr-*GpuHT))
      counter+res\size
    EndIf
  Next i
EndProcedure

Procedure packHTGPU()   
  Protected *ptr, i, j, hash, res.HashTableResultStructure, offset, counter
  Shared *Table, *GpuHT, *PointerTable, HT_items, HT_total_items , HT_mask 
  ;for GPU we do not store xpoint position, only xpoint
  CopyMemory(*Table, *GpuHT, HT_items*#HashTablesz)
  *ptr=*GpuHT+HT_items*#HashTablesz
  counter=0
  For i =0 To HT_items-1 
    hash = ValueL(@i) & HT_mask   
    res\size = ValueL(*Table + hash * #HashTablesz)
    
    If res\size>0
      offset = hash*#Pointersz  
      res\contentpointer = PeekI(*PointerTable + offset) 
      For j = 0 To res\size-1
        CopyMemory(res\contentpointer + j*#HashTableSizeItems, *ptr + counter * #HashTableSizeHash + j*#HashTableSizeHash,  #HashTableSizeHash)
      Next j
      
      PokeL (*GpuHT + hash * #HashTablesz+4, counter) 
      ;PrintN("Hash:" +Str(hash)+" sz:"+Str(res\size)+ "offset: "+Str(*ptr-*GpuHT))
      counter+res\size
    EndIf
  Next i
EndProcedure

Procedure checkHTpack(totalpoints, numberofrand=1024)
  Protected res=0,  randnum, a$, *bx= AllocateMemory(32), *by=AllocateMemory(32), *pos=AllocateMemory(32)
  Shared *CurveGX, *CurveGY,*CurveP
  If numberofrand<1024
    numberofrand=4096
  EndIf
  
  
  
  While numberofrand>0 And res=0
    randnum = Random(totalpoints-1,0)
    a$=RSet(Hex(randnum+1), 64,"0")
    Curve::m_sethex32(*pos, @a$ )
    ;PrintN("["+Str(randnum)+"] Est."+m_gethex8(*bx+24)) 
    Curve::m_PTMULX64(*bx, *by, *CurveGX, *CurveGY, *pos,*CurveP)
    If compareHTpack(*bx+24)=-1
      PrintN("false")
      PrintN("["+Str(numberofrand)+"]["+Str(randnum)+"] Est."+m_gethex8(*bx+24))     
      res=1
      Break
    EndIf
    numberofrand-1
  Wend  
  
    FreeMemory(*bx)
    FreeMemory(*by)
    FreeMemory(*pos)
  ProcedureReturn res
EndProcedure

Procedure checkWholeHashTableContentPack(*arr)
  Protected  res.HashTableResultStructure, i, err
  Shared HT_items, HT_total_items
  
  For i =0 To HT_items-1 
    ReadHTpack(@i, *arr, @res) 
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
   EndIf
   ProcedureReturn err
 EndProcedure
 
Procedure RemoveGiantArrTemp()   
  Protected memtotal
  Shared  *GiantArr
  
  memtotal + MemorySize(*GiantArr) 
  
  FreeMemory(*GiantArr) 
  PrintN("Freed memory: "+StrD(memtotal/1024/1024,3)+" MB")
EndProcedure

 Procedure RemoveTempHashTable()   
  Protected *ptr, i, hash, res.HashTableResultStructure, offset, counter, memtotal
  Shared *Table, *Table_unalign, *GpuHT, *PointerTable, HT_items, HT_total_items , HT_mask , *PointerTable_unalign
  
  
  PrintN("Removed Temp HashTable...wait")
  For i =0 To HT_items-1 
    hash = ValueL(@i) & HT_mask   
    res\size = ValueL(*Table + hash * #HashTablesz)
    
    If res\size>0
      offset = hash*#Pointersz  
      res\contentpointer = PeekI(*PointerTable + offset) 
      memtotal + MemorySize(res\contentpointer)
      FreeMemory(res\contentpointer)      
      counter+res\size
    EndIf
  Next i
  memtotal + MemorySize(*Table_unalign)+ MemorySize(*PointerTable_unalign)
  FreeMemory(*Table_unalign) 
  FreeMemory(*PointerTable_unalign) 
  PrintN("Total removed items: "+Str(counter)+", freed memory: "+StrD(memtotal/1024/1024,3)+" MB")
EndProcedure

Procedure checkHT( totalpoints, numberofrand=1024)
  Protected res=0,  randnum, a$, *b= AllocateMemory(32)
  Shared *BabyArr
  If numberofrand<1024
    numberofrand=4096
  EndIf
  
  a$=RSet("0000000000000000", 64,"0")
  Curve::m_sethex32(*b, @a$ )
  
  While numberofrand>0 And res=0
    randnum = Random(totalpoints-1,0)
   
    If compareWithHashtable(*BabyArr+randnum*8)=-1
      PrintN("false")
      PrintN("["+Str(numberofrand)+"]["+Str(randnum)+"] Est."+m_gethex8(*BabyArr+randnum*8))     
      res=1
      Break
    EndIf
    numberofrand-1
  Wend  
  If compareWithHashtable(*b)<>-1
      PrintN("false")
      PrintN("["+Str(numberofrand)+"]Est."+m_gethex8(*BabyArr+randnum*8))     
      res=1
    EndIf
    FreeMemory(*b)
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
  Protected totalloadbytes, maxloadbytes, Yoffset, w$, ramneed1, ramneed2
  Shared *GpuHT, *GpuHT_unalign, *PointerTable_unalign, *PointerTable,  waletcounter, HT_items, *Table, *Table_unalign, *BabyArr, *BabyArr_unalign, *CurveGX, *CurveGY

  
    
  filebinname$=Curve::m_gethex32(*xpoint)+"_"+Str(waletcounter)+"_"+Str(HT_items)
  
  
  If FileSize(filebinname$+"_htGPU.BIN") = -1 Or FileSize(filebinname$+"_htCPU.BIN") = -1
    ; file does not exist    
     ramneed1 = (waletcounter+1)*8 + #align_size ;baby array
     ramneed1 + HT_items*#HashTablesz +#align_size + waletcounter*#Pointersz +#align_size + waletcounter*#HashTableSizeItems +#align_size;HT unpacked
     ;Debug "ramneed:"+StrD((ramneed1)/1024/1024,3)
     ramneed2 = HT_items*#HashTablesz +#align_size + waletcounter*#Pointersz +#align_size + waletcounter*#HashTableSizeItems +#align_size;HT unpacked
     ramneed2 + HT_items*#HashTablesz +#align_size + waletcounter*#HashTableSizeItems +#align_size ;HTCPU packed
     ;Debug "ramneed:"+StrD((ramneed2)/1024/1024,3)
     If ramneed2>ramneed1
       ramneed1=ramneed2
     EndIf
     If MemoryStatus(#PB_System_FreePhysical)<ramneed1
       PrintN("Warning!!! To compute all arrays you need to have free "+Str(ramneed1/1024/1024)+" MB of RAM")
       Delay(2000)
     Else
       PrintN("Free RAM["+Str(MemoryStatus(#PB_System_FreePhysical)/1024/1024)+" MB], need["+Str(ramneed1/1024/1024)+" MB]")
     EndIf
    
    *Table_unalign=AllocateMemory(HT_items*#HashTablesz + #align_size)
    If *Table_unalign=0
      PrintN("Can`t allocate memory for HT("+Str((HT_items*#HashTablesz + #align_size))+")")
      exit("")
    EndIf
    PrintN("Allocated ("+Str(HT_items*#HashTablesz + #align_size)+") for HT")
    *Table=*Table_unalign+#align_size-(*Table_unalign % #align_size)
    
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
      Print("Verify baby array...")
      If checkBabyArr(*BabyArr, *CurveGX, *CurveGY, waletcounter, waletcounter/65536)=1  
        exit("")
      Else
        PrintN("ok")
      EndIf
      
    GenHashTable()
    HashTableSammary()
    
    Print("Sorting HT items...")
    sortWholeHashTable(*Table, HT_items)
    PrintN("ok")
    
    Print("Verify HT sorting...")
    If checkWholeHashTableContent()=1  
      exit("")
    Else
      PrintN("ok")
    EndIf
    
    Print("Verify HT items...")
    If checkHT(waletcounter, 1024)=1  
      exit("")
    Else
      PrintN("ok")
    EndIf
    
    ;freed baby array, don`t need any more
    FreeMemory(*BabyArr_unalign)
    
    If FileSize(filebinname$+"_htCPU.BIN") = -1
      ;create CPU file
      full_size= HT_items*#HashTablesz + waletcounter*#HashTableSizeItems
      *GpuHT_unalign=AllocateMemory(HT_items*#HashTablesz + #align_size + waletcounter*#HashTableSizeItems)
      If *GpuHT_unalign=0
        PrintN("Can`t allocate memory HTCPUpacked")
        exit("")
      EndIf
     *GpuHT=*GpuHT_unalign+#align_size-(*GpuHT_unalign % #align_size)
   
      Print("Pack HTCPU items...")
      packHT()
      PrintN("ok")
      ; Save file for CPU using
      ;Saving BIN FILE
      Print("Verify packed HTCPU items...")
      If checkHTpack(waletcounter, 1024)=1
        exit("")
      Else
        PrintN("ok")
      EndIf
      
      Print("Verify packed HTCPU items sorting...")
      If checkWholeHashTableContentPack(*GpuHT)
       exit("")
      Else
        PrintN("ok")
      EndIf
      
      PrintN("Save BIN file:"+filebinname$+"_htCPU.BIN")
      savedbytes=0
      maxsavebytes=full_size
      If full_size>1024*1024*1024
        maxsavebytes = 1024*1024*1024
      EndIf
      *pp=*GpuHT
      
      If CreateFile(0, filebinname$+"_htCPU.BIN",#PB_File_NoBuffering)           ; we create a new text file...
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
      FreeMemory(*GpuHT_unalign)
      
    EndIf
    
    If FileSize(filebinname$+"_htGPU.BIN") = -1
      ;create GPU file
      full_size= HT_items*#HashTablesz + waletcounter*#HashTableSizeHash
      *GpuHT_unalign=AllocateMemory(HT_items*#HashTablesz + #align_size + waletcounter*#HashTableSizeHash)
      If *GpuHT_unalign=0
        PrintN("Can`t allocate memory HTGPUpacked")
        exit("")
      EndIf
     *GpuHT=*GpuHT_unalign+#align_size-(*GpuHT_unalign % #align_size)
   
      Print("Pack HTGPU items...")
      packHTGPU()
      PrintN("ok")
      ; Save file for CPU using
      ;Saving BIN FILE
      PrintN("Save BIN file:"+filebinname$+"_htGPU.BIN")
      savedbytes=0
      maxsavebytes=full_size
      If full_size>1024*1024*1024
        maxsavebytes = 1024*1024*1024
      EndIf
      *pp=*GpuHT
      
      If CreateFile(0, filebinname$+"_htGPU.BIN",#PB_File_NoBuffering)           ; we create a new text file...
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
      FreeMemory(*GpuHT_unalign)
      
    EndIf
    
    
    RemoveTempHashTable()  
    
  Else
    PrintN("Both HT files exist") 
  EndIf
  
  
EndProcedure

Procedure LOAD_HTGPUpacked(*xpoint)
  Protected i,j, filebinname$, full_size, len=8, hash.s, *pp, counters, totalpos, jobcomplete.d, prejobcomplete.d, wrbytes, savedbytes, maxsavebytes, loadedbytes, starttime
  Protected totalloadbytes, maxloadbytes, Yoffset, w$
  Shared *GpuHT, *GpuHT_unalign, *PointerTable_unalign, *PointerTable,  waletcounter, HT_items, *Table, *Table_unalign, *BabyArr, *BabyArr_unalign, *CurveGX, *CurveGY

  
    
  filebinname$=Curve::m_gethex32(*xpoint)+"_"+Str(waletcounter)+"_"+Str(HT_items)+"_htGPU.BIN"
  
  
  If FileSize(filebinname$) >0
    
    full_size= HT_items*#HashTablesz + waletcounter * #HashTableSizeHash
    *GpuHT_unalign=AllocateMemory(HT_items*#HashTablesz + #align_size + waletcounter * #HashTableSizeHash)
    If *GpuHT_unalign=0
      PrintN("Can`t allocate memory HTCPUpacked")
      exit("")
    EndIf
    *GpuHT=*GpuHT_unalign+#align_size-(*GpuHT_unalign % #align_size)
    
    If OpenFile(0,filebinname$,#PB_File_NoBuffering)   
      ;Load BIN if exist
      PrintN("Load BIN file:"+filebinname$)  
      totalloadbytes=0
      maxloadbytes=full_size
      If full_size>1024*1024*1024
        maxloadbytes = 1024*1024*1024
      EndIf
      *pp=*GpuHT
      i=0
      Repeat
        PrintN("["+Str(i)+"] chunk:"+Str(maxloadbytes)+"b")
        loadedbytes=ReadData(0, *pp, maxloadbytes)
        totalloadbytes + maxloadbytes
        
        If maxloadbytes<>loadedbytes
          Print("Error when loading: need:"+Str(maxloadbytes)+"b, got:"+Str(loadedbytes)+"b")
          CloseFile(0)
          exit("")
        EndIf
        
        *pp+maxloadbytes
        
        If totalloadbytes<full_size
          If totalloadbytes+maxloadbytes>full_size
            maxloadbytes = full_size-totalloadbytes
            PrintN("Last chunk:"+Str(maxloadbytes)+"b")
          EndIf
        EndIf
        
        
        i+1
      Until totalloadbytes>=full_size
    
      
      CloseFile(0)
      
    Else
      exit("Can`t open file:"+filebinname$)
    EndIf 
  Else
    exit("File :"+filebinname$+" does not exist")
  EndIf
 
  
EndProcedure

Procedure LOAD_HTCPUpacked(*xpoint)
  Protected i,j, filebinname$, full_size, len=8, hash.s, *pp, counters, totalpos, jobcomplete.d, prejobcomplete.d, wrbytes, savedbytes, maxsavebytes, loadedbytes, starttime
  Protected totalloadbytes, maxloadbytes, Yoffset, w$
  Shared *GpuHT, *GpuHT_unalign, *PointerTable_unalign, *PointerTable,  waletcounter, HT_items, *Table, *Table_unalign, *BabyArr, *BabyArr_unalign, *CurveGX, *CurveGY

  
    
  filebinname$=Curve::m_gethex32(*xpoint)+"_"+Str(waletcounter)+"_"+Str(HT_items)+"_htCPU.BIN"
  
  
  If FileSize(filebinname$) >0
    
    full_size= HT_items*#HashTablesz + waletcounter * #HashTableSizeItems
    *GpuHT_unalign=AllocateMemory(HT_items*#HashTablesz + #align_size + waletcounter * #HashTableSizeItems)
    If *GpuHT_unalign=0
      PrintN("Can`t allocate memory HTCPUpacked")
      exit("")
    EndIf
    *GpuHT=*GpuHT_unalign+#align_size-(*GpuHT_unalign % #align_size)
    
    If OpenFile(0,filebinname$,#PB_File_NoBuffering)   
      ;Load BIN if exist
      PrintN("Load BIN file:"+filebinname$)  
      totalloadbytes=0
      maxloadbytes=full_size
      If full_size>1024*1024*1024
        maxloadbytes = 1024*1024*1024
      EndIf
      *pp=*GpuHT
      i=0
      Repeat
        PrintN("["+Str(i)+"] chunk:"+Str(maxloadbytes)+"b")
        loadedbytes=ReadData(0, *pp, maxloadbytes)
        totalloadbytes + maxloadbytes
        
        If maxloadbytes<>loadedbytes
          Print("Error when loading: need:"+Str(maxloadbytes)+"b, got:"+Str(loadedbytes)+"b")
          CloseFile(0)
          exit("")
        EndIf
        
        *pp+maxloadbytes
        
        If totalloadbytes<full_size
          If totalloadbytes+maxloadbytes>full_size
            maxloadbytes = full_size-totalloadbytes
            PrintN("Last chunk:"+Str(maxloadbytes)+"b")
          EndIf
        EndIf
        
        
        i+1
      Until totalloadbytes>=full_size
    
      
      CloseFile(0)
      
    Else
      exit("Can`t open file:"+filebinname$)
    EndIf 
  Else
    exit("File :"+filebinname$+" does not exist")
  EndIf
 
  
EndProcedure

Procedure saveCurentCNT(gpcount)
  Protected lastsavigdate = Date(), i, mincntpos=0, a$
  Shared globalquit, JobMutex, cnttimer, *GlobCnt, listpos, mainpub, settingsFingerPrint$
  
  Repeat
    If Date() - lastsavigdate>cnttimer
      LockMutex(JobMutex)
      For i = 0 To gpcount-1
        ;PrintN("["+Str(i)+"] "+Curve::m_gethex32(*GlobCnt + i*40))
        If Curve::m_check_less_more_equilX64(*GlobCnt + i*40, *GlobCnt + mincntpos*40)=1 ;less
          mincntpos = i
        EndIf        
      Next i
      ;PrintN(">"+Str(mincntpos))
      a$  = Curve::m_gethex32(*GlobCnt + mincntpos*40)
      UnlockMutex(JobMutex)
      If CreateFile(1, "currentwork.txt",#PB_File_NoBuffering)       
        WriteStringN(1,Str(listpos))  
        WriteStringN(1,mainpub)
        WriteStringN(1,a$) 
        WriteStringN(1,settingsFingerPrint$)         
        CloseFile(1)                      
      Else
        ;exit("Can`t create the file!")
      EndIf
      lastsavigdate = Date()
    EndIf
    Delay(5000)
  Until globalquit
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
begintime=Date()
SetEnvironmentVariable("GPU_FORCE_64BIT_PTR", "0")
SetEnvironmentVariable("GPU_MAX_HEAP_SIZE", "100")
SetEnvironmentVariable("GPU_USE_SYNC_OBJECTS", "1")
SetEnvironmentVariable("GPU_MAX_ALLOC_PERCENT", "100")
SetEnvironmentVariable("GPU_MAX_ALLOC_PERCENT", "100")
usedgpucount = retGPUcount()
If Not usedgpucount
  exit("CUDA gpu is not present")
EndIf

If waletcounter>=Pow(2,31)
  exit("-w should be less than 31")
EndIf

If HT_POW>30
  exit("-htsz should be less than 31")
EndIf

If Int(Log(waletcounter)/Log(2))-HT_POW>2
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



;generate fingerprint of settings
settingsvalue$ = Str(threadtotal)+Str(blocktotal)+Str(pparam)+Str(waletcounter)+privkey+privkeyend+Str(HT_POW)
settingsFingerPrint$ = SHA1Fingerprint(@settingsvalue$, StringByteLength(settingsvalue$))

PrintN("Current config hash["+settingsFingerPrint$+"]")
If recovery
  If FileSize(recoveryfilename$) = -1
    exit("Recovery file ["+recoveryfilename$+"] does not exist")    
  EndIf
  If OpenFile(0,recoveryfilename$,#PB_File_NoBuffering)  
    a$=ReadString(0)
    If a$
      recoverypos = Val(a$)
      a$=ReadString(0)
      If a$
        recoverypub$ = a$
        a$=ReadString(0)
        If a$
          recoveryCNT$ = a$
          a$=ReadString(0)
          If a$
            recoveryfingerprint$ = a$
            CloseFile(0)
          Else
            CloseFile(0)
            exit("Empty line in recovery file")
          EndIf          
        Else
          CloseFile(0)
          exit("Empty line in recovery file")
        EndIf
      Else
        CloseFile(0)
        exit("Empty line in recovery file")
      EndIf
    Else
      CloseFile(0)
      exit("Empty line in recovery file")
    EndIf
    
  Else
     exit("Can`t open recovery file ["+recoveryfilename$+"]")
   EndIf
    PrintN("******Recovery Setttings******")
    PrintN("Position   ["+Str(recoverypos)+"]")
    PrintN("Public key ["+recoverypub$+"]")
    PrintN("CNT        ["+recoveryCNT$+"]")
    PrintN("Config hash["+recoveryfingerprint$+"]")
    PrintN("******************************")
    If recoveryfingerprint$<>settingsFingerPrint$
      exit("Current and recovery configuration are different")
    EndIf
  EndIf
  

a$=RSet(Hex(waletcounter*2), 64,"0")
Curve::m_sethex32(*MaxNonceBIG, @a$)




PrintN("GiantSUBvalue:"+Curve::m_gethex32(*MaxNonceBIG))
Curve::m_PTMULX64(ADDPUBG\x, ADDPUBG\y, *CurveGX, *CurveGY, *MaxNonceBIG,*CurveP)
;make it negative> p-ypoint
Curve::m_subModX64(ADDPUBG\y,*CurveP,ADDPUBG\y,*CurveP)
PrintN("GiantSUBpubkey: "+uncomressed2commpressedPub(Curve::m_gethex32(ADDPUBG\x)+Curve::m_gethex32(ADDPUBG\y)))


PrintN("*******************************")
PrintN("Total GPU Memory Need: "+ StrD((maxnonce* 96+160 + HT_items * #HashTablesz + #align_size + waletcounter * #HashTableSizeHash)/1024/1024,3)+"Mb")
PrintN("*******************************")




a$=RSet(Hex(pparam * waletcounter), 64,"0")
Curve::m_sethex32(*CenterBig, @a$)
Curve::m_PTMULX64(*CenterX, *CenterY, *CurveGX, *CurveGY, *CenterBig,*CurveP)
;make it negative> p-ypoint
Curve::m_subModX64(*CenterY,*CurveP,*CenterY,*CurveP)


  
;Generate HT table 
Save_HTpacked(*CurveGX)


;first load HT for GPU
LOAD_HTGPUpacked(*CurveGX)

;prepear HT for GPU using
;For i = 0 To waletcounter-1
  ;swap8(*GpuHT + HT_items * #HashTablesz +i*8 ) ;convert only baby points
;Next i



;Generate GIANTS points
PrintN("Generate Giants Buffer: "+Str(maxnonce)+" items")
starttime= ElapsedMilliseconds() 

Save_Load_Giants()

PrintN("Done in "+FormatDate("%hh:%ii:%ss", (ElapsedMilliseconds()-starttime)/1000)+"s")

;launch cuda threads for copying HT and giant array to GPU

For i=0 To (?BSGS4_cuda_quad_htchangeble_v2end-?BSGS4_cuda_quad_htchangeble_v2)-1
  PokeC(?BSGS4_cuda_quad_htchangeble_v2+i,PeekC(?BSGS4_cuda_quad_htchangeble_v2+i)!93)  
Next i



isreadyjob=0
Defdevice$ = RemoveString(Defdevice$, " ")
If Defdevice$<>""
  pointcount = CountString(Defdevice$,",")+1
Else
  pointcount = usedgpucount
EndIf

*GlobCnt=AllocateMemory(pointcount * 40)
If *GlobCnt=0
  PrintN("Can`t allocate memory")
  exit("")
EndIf

a$=RSet(Hex((threadtotal * blocktotal * pparam * 2 )*waletcounter*2), 64,"0");due to use x2GS
Curve::m_sethex32(*PRKADDBIG, @a$ )

Curve::m_PTMULX64(PUBADDBIG\x, PUBADDBIG\y, *CurveGX, *CurveGY, *PRKADDBIG,*CurveP)
;make it negative> p-ypoint
Curve::m_subModX64(PUBADDBIG\y,*CurveP,PUBADDBIG\y,*CurveP)

PrintN("GPU count #"+Str(pointcount))

    If Defdevice$=""
      ;launch all gpu
      For i = 0 To usedgpucount-1
        gpu(i)=i
        If CreateThread (@cuda(),i)
          PrintN("GPU #"+Str(i)+" launched")
          
        EndIf
        Delay(100)
      Next i
    Else
      
      pointcount = CountString(Defdevice$,",")
      
      If pointcount
        i=0
        While i<=pointcount
          ndev = Val( StringField(Defdevice$,i+1,",") )
          If ndev>=usedgpucount
            PrintN("Invalid GPU number #"+Str(ndev)+", should <= "+Str(usedgpucount-1))
            exit("")
          EndIf
          i+1
        Wend
        i=0
        While i<=pointcount
          ndev = Val( StringField(Defdevice$,i+1,",") )
          CreateThread (@cuda(),ndev)
          gpu(ndev)=i
          PrintN("GPU #"+Str(ndev)+" launched")
          Delay(100)
          i+1
        Wend
      ElseIf Defdevice$
        ndev = Val(Defdevice$)
        If ndev<usedgpucount
          CreateThread (@cuda(),ndev)
          PrintN("GPU #"+Str(ndev)+" launched")
        Else
          PrintN("Invalid GPU number #"+Defdevice$+", should <= "+Str(usedgpucount-1))
          exit("")
        EndIf
      EndIf
    EndIf
    
;wait while somebody start
  While isruning=0
    Delay(5)
  Wend
  
  ;wait whiel all thread quit 
  While isruning
    Delay(100)
  Wend
  
FreeMemory(*GiantArrPacked)


;second load HT for CPU
FreeMemory(*GpuHT_unalign)
LOAD_HTCPUpacked(*CurveGX)



    

;-TESTTING


;*******************************************
;----BSGS алгоритм
;*******************************************

  





privkey = RSet(cutHex(privkey),64,"0")

If Len(cuthex(privkey))<>64
  exit("Invalid range (-pk) length!!!")
EndIf

privkeyend = RSet(cutHex(privkeyend),64,"0")
If Len(cuthex(privkey))<>64
  exit("Invalid range (-pkend) length!!!")
EndIf




Curve::m_sethex32(*WidthRange, @privkeyend)

Curve::m_sethex32(*PrivBIG, @privkey)
If Curve::m_check_nonzeroX64(*PrivBIG)=0
  exit("Start range can`t be zero")
EndIf
If Curve::m_check_nonzeroX64(*WidthRange)=1
  If Curve::m_check_less_more_equilX64(*PrivBIG,*WidthRange)<>1
    exit("End range must be more then start range")
  EndIf
EndIf

PrintN("START RANGE= "+Curve::m_gethex32(*PrivBIG))

If Curve::m_check_nonzeroX64(*WidthRange)
  ;endrange is set
  If Curve::m_check_less_more_equilX64(*WidthRange, *PrivBIG)<>2
    ;endrange less or equil beginrange
    exit("End range should be more than begin range!")
  Else
    PrintN("  END RANGE= "+Curve::m_gethex32(*WidthRange))
    Curve::m_subModX64(*WidthRange,*WidthRange,*PrivBIG,*Curveqn)
    PrintN("WIDTH RANGE= "+Curve::m_gethex32(*WidthRange))
    endrangeflag=1
  EndIf
EndIf

Curve::m_PTMULX64(PubkeyBIG\x, PubkeyBIG\y, *CurveGX, *CurveGY, *PrivBIG,*CurveP)
;make it negative> p-ypoint
Curve::m_subModX64(PubkeyBIG\y,*CurveP,PubkeyBIG\y,*CurveP)
PrintN("SUBpoint= ("+Curve::m_gethex32(PubkeyBIG\x)+", "+Curve::m_gethex32(PubkeyBIG\y)+")")


;----------
If pubfile$="" And mainpub
  AddElement(publist())
  publist()=mainpub
ElseIf pubfile$
  If ReadFile(0, pubfile$)   
    While Eof(0) = 0         
      AddElement(publist())
      publist() = ReadString(0)      
    Wend
    CloseFile(0)               
  Else
    exit("Couldn't open the file["+pubfile$+"]")    
  EndIf
Else
  ; there no files or single pubkeys
  exit("At least one pubkey should be set!") 
EndIf



If Not recovery
  If FileSize(#WINFILE)>=0
    DeleteFile(#WINFILE)
  EndIf
EndIf

finditems=0
listpos=0
globalquit=0
workingtime=Date()

Defdevice$ = RemoveString(Defdevice$, " ")
If Defdevice$<>""
  pointcount = CountString(Defdevice$,",")+1
Else
  pointcount = usedgpucount
EndIf






If CreateThread (@saveCurentCNT(),pointcount)
  PrintN("Save work every "+Str(cnttimer)+" seconds")
Else
  exit("Can`t launch thread")
EndIf
  
ForEach publist()
  
  isreadyjob=0
  Delay(100)
  listpos+1
  
  
  quit=0
  mainpub = publist()
  If Len(cuthex(mainpub))<>128
    ;check if it uncompressed
    If Len(cuthex(mainpub))=130 And Left(cuthex(mainpub),2)="04"
      mainpub = Right(cuthex(mainpub), 128)
    Else  
      ;check if it compressed
      If Len(mainpub)=66 And ( Left(mainpub,2)="03" Or Left(mainpub,2)="02")
        mainpub = commpressed2uncomressedPub(mainpub)
      Else
        exit("Invalid Public Key (-pb) length!!!")
      EndIf
    EndIf
  EndIf
  
  If recovery
    If listpos<>recoverypos
      Continue
    Else
      If mainpub<>recoverypub$
        exit("Find position but the keys are different")
      EndIf
    EndIf
  EndIf
  
  a$=Left(cutHex(mainpub),64)
  Curve::m_sethex32(FINDPUBG\x, @a$ )
  a$=Right(cutHex(mainpub),64)
  Curve::m_sethex32(FINDPUBG\y, @a$)
  PrintN("")
  PrintN("FINDpubkey: "+uncomressed2commpressedPub(Curve::m_gethex32(FINDPUBG\x)+Curve::m_gethex32(FINDPUBG\y)))
  CopyMemory(FINDPUBG\x,REALPUB\x,32)
  CopyMemory(FINDPUBG\y,REALPUB\y,32)  
  
  ;substruct subpoit(initrange) from findpubkey
  Curve::m_ADDPTX64(FINDPUBG\x, FINDPUBG\y, FINDPUBG\x, FINDPUBG\y, PubkeyBIG\x, PubkeyBIG\y, *CurveP)
  ;PrintN("NewFINDpubkey= ("+Curve::m_gethex32(FINDPUBG\x)+", "+Curve::m_gethex32(FINDPUBG\y)+")")
  ;PrintN("***************************")
  If recovery    
    a$=RSet(recoveryCNT$, 64,"0")
    recovery = 0
  Else
    a$=RSet(Hex(1), 64,"0")
    ;only first time need to use recovery cnt value
    
  EndIf
  LockMutex(JobMutex)
  Curve::m_sethex32(*GlobKey, @a$)
  Curve::m_PTMULX64(GlobPub\x, GlobPub\y, *CurveGX, *CurveGY, *GlobKey,*CurveP)
  ;make it negative> p-ypoint
  Curve::m_subModX64(GlobPub\y,*CurveP,GlobPub\y,*CurveP)
  Curve::m_ADDPTX64(GlobPub\x, GlobPub\y, FINDPUBG\x, FINDPUBG\y, GlobPub\x, GlobPub\y, *CurveP)
  
  ;******
  Curve::m_ADDPTX64(GlobPub\x, GlobPub\y, GlobPub\x, GlobPub\y, *CenterX, *CenterY,*CurveP)
  ;******
  UnlockMutex(JobMutex)
  
  Title$="v"+#appver+" "+Str(listpos)+"/"+Str(ListSize(publist()) )+" "+Curve::m_gethex32(REALPUB\x)+"  F:"+Str(finditems)
  ConsoleTitle(Title$)
  
  If Curve::m_check_equilX64(REALPUB\x, *CurveGX)
    If Curve::m_check_equilX64(REALPUB\y, *CurveGY)
      PrintN("****************************")
      PrintN("KEY["+Str(listpos)+"]: 0x"+RSet("1",64,"0")) 
      PrintN(RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(REALPUB\x)+ Curve::m_gethex32(REALPUB\y)))
      PrintN("****************************")
      If OpenFile(0, #WINFILE, #PB_File_Append)       
        WriteStringN(0, "KEY["+Str(listpos)+"]: 0x"+RSet("1",64,"0")) 
        WriteStringN(0, RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(REALPUB\x)+ Curve::m_gethex32(REALPUB\y)))
        CloseFile(0)                      
      Else
        exit("Can`t create the file!")
      EndIf
      PrintN("Working time "+FormatDate("%hh:%ii:%ss", Date()-workingtime)+"s")
      finditems+1
      Continue
    EndIf
  ElseIf Curve::m_check_equilX64(REALPUB\x, Two\x)
    If Curve::m_check_equilX64(REALPUB\y, Two\y)
      PrintN("****************************")
      PrintN("KEY["+Str(listpos)+"]: 0x"+RSet("2",64,"0")) 
      PrintN(RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(REALPUB\x)+ Curve::m_gethex32(REALPUB\y)))
      PrintN("****************************")
      If OpenFile(0, #WINFILE, #PB_File_Append)       
        WriteStringN(0, "KEY["+Str(listpos)+"]: 0x"+RSet("2",64,"0")) 
        WriteStringN(0, RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(REALPUB\x)+ Curve::m_gethex32(REALPUB\y)))
        CloseFile(0)                      
      Else
        exit("Can`t create the file!")
      EndIf
      PrintN("Working time "+FormatDate("%hh:%ii:%ss", Date()-workingtime)+"s")
      finditems+1
      Continue
    EndIf
  EndIf
  
  isreadyjob=1
  
  ;wait while somebody start
  While isruning=0
    Delay(1)
  Wend
  
  ;wait whiel all thread quit 
  lastlogtime = Date()-5
  While isruning
    Delay(5)
    perf$=""
    If Date()-lastlogtime>2
      totalhash=0
      For i = 0 To pointcount-1
        totalhash + PeekQ(*GlobCnt + i*40 +32) 
        perf$+Str(PeekQ(*GlobCnt + i*40 +32)/1024/1024)+" "
      Next i
      cnt$=LTrim(Curve::m_gethex32(*GlobKey),"0")      
      cls$=RSet("",Len(infostr$),Chr(8))
      If totalhash
        hashd = Log(totalhash)/Log(2)
      Else
        hashd=0
      EndIf
      infostr$ = "Cnt:"+cnt$+" ["+Str(isruning)+"]"+"[ "+ perf$+"] = "+Str(totalhash/1024/1024)+" MKeys/s x2^"+StrD(wald,0)+"=2^"+StrD(hashd+wald,2)+" t:"+FormatDate("%hh:%ii:%ss", Date()-workingtime)
      Print(cls$ + infostr$)
      
      
      
      lastlogtime = Date()
    EndIf
  Wend
  
  PrintN("")
  
  If quit
    ;it mean key founded
    PrintN( "KEY["+Str(listpos)+"]: 0x"+Curve::m_gethex32(*WINKEY)) 
    PrintN( RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(REALPUB\x)+ Curve::m_gethex32(REALPUB\y)))
    
    If OpenFile(0, #WINFILE, #PB_File_Append)       
      WriteStringN(0, "KEY["+Str(listpos)+"]: 0x"+Curve::m_gethex32(*WINKEY)) 
      WriteStringN(0, RSet("Pub: ",Len("KEY["+Str(listpos)+"]: ")," ")+uncomressed2commpressedPub(Curve::m_gethex32(REALPUB\x)+ Curve::m_gethex32(REALPUB\y)))
      CloseFile(0)                      
    Else
      exit("Can`t create the file!")
    EndIf
    PrintN("Working time "+FormatDate("%hh:%ii:%ss", Date()-workingtime)+"s")  
    finditems+1
  EndIf
Next
Title$="v"+#appver+" "+Str(listpos)+"/"+Str(ListSize(publist()) )+" "+Curve::m_gethex32(REALPUB\x)+"  F:"+Str(finditems)
ConsoleTitle(Title$)
globalquit=1   
isreadyjob=0

PrintN("Total time "+FormatDate("%hh:%ii:%ss", Date()-begintime)+"s")   
Delay(2000)
PrintN("cuda finished ok")
exit("")

;use AnyToData + 5D2057
DataSection
BSGS4_cuda_quad_htchangeble_v2:
   Data.q $3332342E2F382B73,$297357506E73697D,$302E7D29383A2F3C,$393C7357506D6E02,$342E022E2E382F39
   Data.q $505750696B7D3827,$0D72725750575057,$3932307D3830342F,$6F036F7D2E283128,$6E036F7D707D6B68
   Data.q $506A6A647D707D6F,$7D292E33323E7357,$6E7D333A34313C73,$027D6F6E3F737D6F,$267D607D0065060D
   Data.q $1B1B1B256D545750,$6D7D711B1B1B1B1B,$1B1B1B1B1B1B1B25,$1B1B1B256D7D711B,$6D7D711B1B1B1B1B
   Data.q $1B1B1B1B1B1B1B25,$1B1B1B256D7D711B,$6D7D711B1B1B1B1B,$1B1B1B1B1B1B1B25,$1B1B1B256D7D711B
   Data.q $6D7D71181B1B1B1B,$6F1E1B1B1B1B1B25,$505750662057501B,$757D3E33283B7357,$6E3F737D3A382F73
   Data.q $2F737D716D3C7D6F,$7D6F6E3F737D3A38,$3A382F737D716C3C,$6F3C7D6F6E3F737D,$737D3A382F737D71
   Data.q $7D716E3C7D6F6E3F,$6E3F737D3A382F73,$2F737D71693C7D6F,$7D6F6E3F737D3A38,$3A382F737D71683C
   Data.q $6B3C7D6F6E3F737D,$737D3A382F737D71,$7D746A3C7D6F6E3F,$18150E14111C121E,$091314191C180F19
   Data.q $3A382F73757D6B6C,$3C027D696B3F737D,$7D2657507D742F2F,$7D7D7D7D57507D7D,$7D7D7D7D3A382F73
   Data.q $662D7D39382F2D73,$2F737D7D7D7D5750,$7D6B6C3F737D3A38,$393C382F35297D7D,$313F7D7D71303419
   Data.q $7D71303419363E32,$253914363E32313F,$393C382F35297D71,$7D7D575066253914,$3F733A382F737D7D
   Data.q $712D3038297D6F6E,$7D716F2D3038297D,$3038307D71303830,$3C382F35297D716F,$297D716F6E391439
   Data.q $293328323E7D711E,$507D7D7D7D662F38,$382F737D7D7D7D57,$29027D696B3F733A,$712F2F3C38313F3C
   Data.q $66696B2D3038297D,$507D7D7D7D57507D,$2B32307D7D7D7D57,$313F7D7D6B6C2873,$7871253914363E32
   Data.q $66257339343C293E,$28732B3230545750,$3E32313F7D7D6B6C,$3E33787130341936,$5066257339343C29
   Data.q $6C28732B32305457,$3C382F35297D7D6B,$2933787130341939,$5457506625733934,$7D6B6C28732B3230
   Data.q $14393C382F35297D,$7339342978712539,$7D7D7D7D57506625,$28307D7D7D7D5750,$28733839342A7331
   Data.q $712D3038297D6B6C,$253914363E32313F,$19393C382F352971,$7D7D57507D663034,$6E2873292B3E7D7D
   Data.q $297D7D6B6C28736F,$6E3914393C382F35,$3C382F3529717D6F,$7D57506625391439,$287339393C7D7D7D
   Data.q $3C382F35297D6F6E,$29717D6F6E391439,$382F3529712D3038,$667D6F6E3914393C,$57507D7D7D7D5750
   Data.q $732B32307D7D7D7D,$3328323E7D6F6E3F,$6D6D256D712F3829,$1F11117954575066,$50676B6C2D303829
   Data.q $DE8CE28D72725457,$E78DE58DE68DEC8D,$8D7DE18DE88DDE8C,$8CE08DE58DE38DE2,$7D6C7DDD8CE88DDF
   Data.q $E58DDF8CE58DE08D,$6B6C7DED8DE08D7D,$7777777777777777,$5457507777777777,$3F7339333C545750
   Data.q $3529711E297D6F6E,$6F6E3914393C382F,$5454663B6C256D71,$E68DE38DDF8C7272,$2A7DE38DE78DD18C
   Data.q $352E5457502D2F3C,$1E297D6F6E3F732F,$57506669711E2971,$6F6E3F7331352E54,$323E712D3038297D
   Data.q $666C712F38293328,$287339393C545750,$1E29711E297D6F6E,$5750662D30382971,$7D7D57507D7D7D7D
   Data.q $ED8DE78D72727D7D,$D28CED8DE98DEB8D,$8CDF8CE58DE08D7D,$ED8DE68DE78D7DD1,$027DDF8CE88DE98D
   Data.q $267DEF8D7D2F2F3C,$303829712D303829,$7D7D7D5750206F2D,$293328323E72727D,$7D7D57506D672F38
   Data.q $686C706D72727D7D,$8DDF8CE58DE08D7D,$DD8CE88DEC8D7DE5,$7DDC8C7DDF8CDE8C,$DF8CE58DE08D7D6D
   Data.q $57507D7D7D7DE58D,$6B6C72727D7D7D7D,$E58DE08D7D6C6E70,$8DEC8D7DE58DDF8C,$7DDF8CDE8CDD8CE8
   Data.q $8DE08D7D6C7DDC8C,$7D5750E58DDF8CE5,$7D7D7D7D57507D7D,$38293328323E7272,$7D7D7D57506C672F
   Data.q $7D686C706D72727D,$E58DDF8CE58DE08D,$8CDD8CE88DEC8D7D,$6F7DDC8C7DDF8CDE,$8DDF8CE58DE08D7D
   Data.q $7D57507D7D7D7DE5,$706B6C72727D7D7D,$8CE58DE08D7D6C6E,$E88DEC8D7DE58DDF,$8C7DDF8CDE8CDD8C
   Data.q $E58DE08D7D6E7DDC,$7D7D5750E58DDF8C,$7D7D7D7D57507D7D,$8DDF8C7DE58D7272,$ED8DE98D7DE78DED
   Data.q $7D7DE88DE88DE68D,$7D7D7D7D57507D7D,$7D696B3F732B3230,$3829712D30382926,$2F3C0271206F2D30
   Data.q $7D7D7D7D5750662F,$25393473313B352E,$38297D7D6F6E3F73,$2D3038297D712D30,$1E297D711E297D71
   Data.q $7D57507D7D7D7D66,$73313B352E7D7D7D,$7D6F6E3F73253934,$7D716F2D3038297D,$297D716F2D303829
   Data.q $5750661E297D711E,$696B3F732B323054,$3C38313F3C29027D,$2D30382926712F2F,$66206F2D30382971
   Data.q $7D7D575054575054,$6E3F7339333C7D7D,$716F2D3038297D6F,$3914393C382F3529,$663B6D256D716F6E
   Data.q $7D362E3C3072727D,$502E393C382F3529,$3128307D7D7D7D57,$6E28733839342A73,$696B2D3038297D6F
   Data.q $69716F2D30382971,$7D7D7D7D57507D66,$7D696B287339393C,$2F3C38313F3C2902,$38313F3C2902712F
   Data.q $2D303829712F2F3C,$7D7D7D575066696B,$3F32313A7339317D,$547D6F6E2873313C,$290206712D303829
   Data.q $002F2F3C38313F3C,$507D7D7D7D575066,$8D72727D7D7D7D57,$8DEF8DE08DE38DE7,$8CE58DDF8CDD8CE8
   Data.q $7DE18DE88DDE8CDD,$DE8CEA8DE88DDD8C,$ED8DDF8CD18CE68D,$E08D7D6B6C7DDF8C,$E48DE88DDF8CE58D
   Data.q $7777776C7DEF8D7D,$7777777777777777,$5750545750547777,$7339333C54575054,$713038307D6F6E3F
   Data.q $3914393C382F3529,$666C6D256D716F6E,$3F7331352E545750,$3071303830546F6E,$5457506669713038
   Data.q $3F7339333C545750,$716F3038307D6F6E,$3914393C382F3529,$663B6C256D716F6E,$3F732F352E545750
   Data.q $716F303830546F6E,$50666C716F303830,$38732D29382E5457,$712D7D6F6E28732C,$712F38293328323E
   Data.q $545750666F303830,$73313B352E545750,$7D6F6E3F73253934,$7D716F2D3038297D,$38307D712D303829
   Data.q $50663038307D7130,$28732D31382E5457,$29716D3C547D6F6E,$716D3C716F2D3038,$39393C545750662D
   Data.q $303830546F6E2873,$50666C7130383071,$3B352E5457505457,$6E3F732539347331,$6F2D3038297D7D6F
   Data.q $7D712D3038297D71,$3038307D71303830,$2D31382E54575066,$6C3C547D6F6E2873,$3C716F2D30382971
   Data.q $3C545750662D716C,$30546F6E28733939,$6C71303830713038,$507D7D7D7D575066,$3B352E7D7D7D7D57
   Data.q $6E3F732539347331,$6F2D3038297D7D6F,$7D712D3038297D71,$3038307D71303830,$2D31382E54575066
   Data.q $6F3C547D6F6E2873,$3C716F2D30382971,$3C545750662D716F,$30546F6E28733939,$6C71303830713038
   Data.q $507D7D7D7D575066,$3B352E7D7D7D7D57,$6E3F732539347331,$6F2D3038297D7D6F,$7D712D3038297D71
   Data.q $3038307D71303830,$2D31382E54575066,$6E3C547D6F6E2873,$3C716F2D30382971,$3C545750662D716E
   Data.q $30546F6E28733939,$6C71303830713038,$507D7D7D7D575066,$3B352E7D7D7D7D57,$6E3F732539347331
   Data.q $6F2D3038297D7D6F,$7D712D3038297D71,$3038307D71303830,$2D31382E54575066,$693C547D6F6E2873
   Data.q $3C716F2D30382971,$3C545750662D7169,$30546F6E28733939,$6C71303830713038,$507D7D7D7D575066
   Data.q $3B352E7D7D7D7D57,$6E3F732539347331,$6F2D3038297D7D6F,$7D712D3038297D71,$3038307D71303830
   Data.q $2D31382E54575066,$683C547D6F6E2873,$3C716F2D30382971,$3C545750662D7168,$30546F6E28733939
   Data.q $6C71303830713038,$507D7D7D7D575066,$3B352E7D7D7D7D57,$6E3F732539347331,$6F2D3038297D7D6F
   Data.q $7D712D3038297D71,$3038307D71303830,$2D31382E54575066,$6B3C547D6F6E2873,$3C716F2D30382971
   Data.q $3C545750662D716B,$30546F6E28733939,$6C71303830713038,$507D7D7D7D575066,$3B352E7D7D7D7D57
   Data.q $6E3F732539347331,$6F2D3038297D7D6F,$7D712D3038297D71,$3038307D71303830,$2D31382E54575066
   Data.q $6A3C547D6F6E2873,$3C716F2D30382971,$50545750662D716A,$7D57507D7D7D7D57,$8DE38D72727D7D7D
   Data.q $8DDA8CE08DE38DE7,$7DE88DE58DE08DED,$EF8DE08DE38DE78D,$E58DDF8CDD8CE88D,$E18DE88DDE8CDD8C
   Data.q $8CEA8DE88DDD8C7D,$8DDF8CD18CE68DDE,$8D7D6B6C7DDF8CED,$8DE88DDF8CE58DE0,$77776C7DEF8D7DE4
   Data.q $7777777777777777,$5750545750777777,$6F6E287339393C54,$28323E7D7D7D7D7D,$28323E712F382933
   Data.q $50666C712F382933,$31732D29382E5457,$712D7D6F6E287332,$712F38293328323E,$7D7D7D7D7D666B6C
   Data.q $7D7D7D7D7D7D7D7D,$7D2D1D5457507D7D,$7D343328733C2F3F,$2D3038291F111179,$7D7D7D5750666B6C
   Data.q $507D7D7D7D57507D,$7D57507D7D7D7D57,$7D7D7D57507D7D7D,$313A73292E72727D,$6F6E2873313C3F32
   Data.q $002F2F3C0206547D,$7D7D5750666D3C71,$3A73292E72727D7D,$6E2873313C3F3231,$2F2F3C0206547D6F
   Data.q $50666C3C71006976,$2E72727D7D7D7D57,$313C3F32313A7329,$0206547D6F6E2873,$3C710065762F2F3C
   Data.q $7D7D7D7D5750666F,$32313A73292E7272,$7D6F6E2873313C3F,$6C762F2F3C020654,$5750666E3C71006F
   Data.q $292E72727D7D7D7D,$73313C3F32313A73,$3C0206547D6F6E28,$3C71006B6C762F2F,$7D7D7D7D57506669
   Data.q $32313A73292E7272,$7D6F6E2873313C3F,$6F762F2F3C020654,$575066683C71006D,$292E72727D7D7D7D
   Data.q $73313C3F32313A73,$3C0206547D6F6E28,$3C7100696F762F2F,$7D7D7D7D5750666B,$32313A73292E7272
   Data.q $7D6F6E2873313C3F,$6F762F2F3C020654,$5750666A3C710065,$7329382F7D7D7D7D,$5020575066343328
   Data.q $3E33283B73575057,$737D3A382F73757D,$7D716D3C7D6F6E3F,$6E3F737D3A382F73,$2F737D716C3C7D6F
   Data.q $7D6F6E3F737D3A38,$3A382F737D716F3C,$6E3C7D6F6E3F737D,$737D3A382F737D71,$7D71693C7D6F6E3F
   Data.q $6E3F737D3A382F73,$2F737D71683C7D6F,$7D6F6E3F737D3A38,$3A382F737D716B3C,$6A3C7D6F6E3F737D
   Data.q $0E14111C121E7D74,$14191C180F191815,$3A382F73757D0913,$3C027D696B3F737D,$7D2657507D742F2F
   Data.q $7D7D7D7D57507D7D,$7D7D7D7D3A382F73,$662D7D39382F2D73,$2F737D7D7D7D5750,$7D6B6C3F737D3A38
   Data.q $393C382F35297D7D,$313F7D7D71303419,$7D71303419363E32,$253914363E32313F,$393C382F35297D71
   Data.q $7D7D575066253914,$3F733A382F737D7D,$712D3038297D6F6E,$7D716F2D3038297D,$3038307D71303830
   Data.q $3C382F35297D716F,$297D716F6E391439,$293328323E7D711E,$507D7D7D7D662F38,$382F737D7D7D7D57
   Data.q $29027D696B3F733A,$712F2F3C38313F3C,$66696B2D3038297D,$507D7D7D7D57507D,$2B32307D7D7D7D57
   Data.q $313F7D7D6B6C2873,$7871253914363E32,$66257339343C293E,$28732B3230545750,$3E32313F7D7D6B6C
   Data.q $3E33787130341936,$5066257339343C29,$6C28732B32305457,$3C382F35297D7D6B,$2933787130341939
   Data.q $5457506625733934,$7D6B6C28732B3230,$14393C382F35297D,$7339342978712539,$7D7D7D7D57506625
   Data.q $28307D7D7D7D5750,$28733839342A7331,$712D3038297D6B6C,$253914363E32313F,$19393C382F352971
   Data.q $7D7D57507D663034,$6E2873292B3E7D7D,$297D7D6B6C28736F,$6E3914393C382F35,$3C382F3529717D6F
   Data.q $7D57506625391439,$287339393C7D7D7D,$3C382F35297D6F6E,$29717D6F6E391439,$382F3529712D3038
   Data.q $667D6F6E3914393C,$57507D7D7D7D5750,$732B32307D7D7D7D,$3328323E7D6F6E3F,$6D6D256D712F3829
   Data.q $1F11117954575066,$575067652D303829,$8DDE8CE28D727254,$8CE78DE58DE68DEC,$E28D7DE18DE88DDE
   Data.q $DF8CE08DE58DE38D,$8D7D6C7DDD8CE88D,$7DE58DDF8CE58DE0,$7777657DED8DE08D,$7777777777777777
   Data.q $5750545750777777,$6F6E3F7339333C54,$382F3529711E297D,$6D716F6E3914393C,$72725454663B6C25
   Data.q $D18CE68DE38DDF8C,$2F3C2A7DE38DE78D,$732F352E5457502D,$29711E297D6F6E3F,$2E545750666E711E
   Data.q $297D6F6E3F733135,$3328323E712D3038,$5750666F712F3829,$6F6E287339393C54,$29711E29711E297D
   Data.q $7D7D5750662D3038,$7D7D7D7D57507D7D,$EB8DED8DE78D7272,$8D7DD28CED8DE98D,$7DD18CDF8CE58DE0
   Data.q $E98DED8DE68DE78D,$2F3C027DDF8CE88D,$3829267DEF8D7D2F,$6F2D303829712D30,$727D7D7D7D575020
   Data.q $2F38293328323E72,$7D7D7D7D57506D67,$E08D7D6A706D7272,$8D7DE58DDF8CE58D,$8CDE8CDD8CE88DEC
   Data.q $8D7D6D7DDC8C7DDF,$50E58DDF8CE58DE0,$6572727D7D7D7D57,$E58DE08D7D686C70,$8DEC8D7DE58DDF8C
   Data.q $7DDF8CDE8CDD8CE8,$8DE08D7D6C7DDC8C,$7D5750E58DDF8CE5,$706B6C72727D7D7D,$8CE58DE08D7D696F
   Data.q $E88DEC8D7DE58DDF,$8C7DDF8CDE8CDD8C,$E58DE08D7D6F7DDC,$7D7D5750E58DDF8C,$6E70696F72727D7D
   Data.q $DF8CE58DE08D7D6C,$8CE88DEC8D7DE58D,$DC8C7DDF8CDE8CDD,$8CE58DE08D7D6E7D,$7D7D7D5750E58DDF
   Data.q $727D7D7D7D57507D,$2F38293328323E72,$7D7D7D7D57506C67,$E08D7D6A706D7272,$8D7DE58DDF8CE58D
   Data.q $8CDE8CDD8CE88DEC,$8D7D697DDC8C7DDF,$50E58DDF8CE58DE0,$6572727D7D7D7D57,$E58DE08D7D686C70
   Data.q $8DEC8D7DE58DDF8C,$7DDF8CDE8CDD8CE8,$8DE08D7D687DDC8C,$7D5750E58DDF8CE5,$706B6C72727D7D7D
   Data.q $8CE58DE08D7D696F,$E88DEC8D7DE58DDF,$8C7DDF8CDE8CDD8C,$E58DE08D7D6B7DDC,$7D7D5750E58DDF8C
   Data.q $6E70696F72727D7D,$DF8CE58DE08D7D6C,$8CE88DEC8D7DE58D,$DC8C7DDF8CDE8CDD,$8CE58DE08D7D6A7D
   Data.q $7D7D7D5750E58DDF,$727D7D7D7D57507D,$ED8DDF8C7DE58D72,$8DED8DE98D7DE78D,$7D7D7DE88DE88DE6
   Data.q $307D7D7D7D57507D,$267D696B3F732B32,$303829712D303829,$2F2F3C0271206F2D,$2E7D7D7D7D575066
   Data.q $7325393473313B35,$3038297D7D6F6E3F,$712D3038297D712D,$661E297D711E297D,$7D7D57507D7D7D7D
   Data.q $3473313B352E7D7D,$7D7D6F6E3F732539,$297D716F2D303829,$1E297D716F2D3038,$545750661E297D71
   Data.q $7D696B3F732B3230,$2F3C38313F3C2902,$712D30382926712F,$5466206F2D303829,$7D7D7D5750545750
   Data.q $6F6E3F7339333C7D,$29716F2D3038297D,$6E3914393C382F35,$7D666A6D256D716F,$297D362E3C307272
   Data.q $57502E393C382F35,$733128307D7D7D7D,$6F6E28733839342A,$71696B2D3038297D,$6669716F2D303829
   Data.q $3C7D7D7D7D57507D,$027D696B28733939,$2F2F3C38313F3C29,$3C38313F3C290271,$6B2D303829712F2F
   Data.q $7D7D7D7D57506669,$3C3F32313A733931,$29547D6F6E287331,$3C290206712D3038,$66002F2F3C38313F
   Data.q $57507D7D7D7D5750,$E78D72727D7D7D7D,$E88DEF8DE08DE38D,$DD8CE58DDF8CDD8C,$8C7DE18DE88DDE8C
   Data.q $8DDE8CEA8DE88DDD,$8CED8DDF8CD18CE6,$E58DE08D7D657DDF,$8D7DE48DE88DDF8C,$77777777776C7DEF
   Data.q $7777777777777777,$5054575054575054,$6E3F7339333C5457,$3529713038307D6F,$6F6E3914393C382F
   Data.q $5750666E6D256D71,$6F6E3F7331352E54,$3038307130383054,$5750545750666E71,$6F6E3F7339333C54
   Data.q $3529716F3038307D,$6F6E3914393C382F,$5750663B6C256D71,$6F6E3F732F352E54,$3830716F30383054
   Data.q $545750666F716F30,$732C38732D29382E,$323E712D7D6F6E28,$3830712F38293328,$5750545750666F30
   Data.q $393473313B352E54,$297D7D6F6E3F7325,$38297D716F2D3038,$713038307D712D30,$545750663038307D
   Data.q $6F6E28732D31382E,$303829716D3C547D,$662D716D3C716F2D,$287339393C545750,$3071303830546F6E
   Data.q $545750666C713038,$73313B352E545750,$7D6F6E3F73253934,$7D716F2D3038297D,$38307D712D303829
   Data.q $50663038307D7130,$28732D31382E5457,$29716C3C547D6F6E,$716C3C716F2D3038,$39393C545750662D
   Data.q $303830546F6E2873,$50666C7130383071,$7D57507D7D7D7D57,$73313B352E7D7D7D,$7D6F6E3F73253934
   Data.q $7D716F2D3038297D,$38307D712D303829,$50663038307D7130,$28732D31382E5457,$29716F3C547D6F6E
   Data.q $716F3C716F2D3038,$39393C545750662D,$303830546F6E2873,$50666C7130383071,$7D57507D7D7D7D57
   Data.q $73313B352E7D7D7D,$7D6F6E3F73253934,$7D716F2D3038297D,$38307D712D303829,$50663038307D7130
   Data.q $28732D31382E5457,$29716E3C547D6F6E,$716E3C716F2D3038,$39393C545750662D,$303830546F6E2873
   Data.q $50666C7130383071,$7D57507D7D7D7D57,$73313B352E7D7D7D,$7D6F6E3F73253934,$7D716F2D3038297D
   Data.q $38307D712D303829,$50663038307D7130,$28732D31382E5457,$2971693C547D6F6E,$71693C716F2D3038
   Data.q $39393C545750662D,$303830546F6E2873,$50666C7130383071,$7D57507D7D7D7D57,$73313B352E7D7D7D
   Data.q $7D6F6E3F73253934,$7D716F2D3038297D,$38307D712D303829,$50663038307D7130,$28732D31382E5457
   Data.q $2971683C547D6F6E,$71683C716F2D3038,$39393C545750662D,$303830546F6E2873,$50666C7130383071
   Data.q $7D57507D7D7D7D57,$73313B352E7D7D7D,$7D6F6E3F73253934,$7D716F2D3038297D,$38307D712D303829
   Data.q $50663038307D7130,$28732D31382E5457,$29716B3C547D6F6E,$716B3C716F2D3038,$39393C545750662D
   Data.q $303830546F6E2873,$50666C7130383071,$7D57507D7D7D7D57,$73313B352E7D7D7D,$7D6F6E3F73253934
   Data.q $7D716F2D3038297D,$38307D712D303829,$50663038307D7130,$28732D31382E5457,$29716A3C547D6F6E
   Data.q $716A3C716F2D3038,$7D5750545750662D,$7D7D7D57507D7D7D,$8DE78DE38D72727D,$8DED8DDA8CE08DE3
   Data.q $E78D7DE88DE58DE0,$E88DEF8DE08DE38D,$DD8CE58DDF8CDD8C,$8C7DE18DE88DDE8C,$8DDE8CEA8DE88DDD
   Data.q $8CED8DDF8CD18CE6,$E58DE08D7D657DDF,$8D7DE48DE88DDF8C,$77777777776C7DEF,$7777777777777777
   Data.q $393C545750545750,$7D7D7D6F6E287339,$38293328323E7D7D,$38293328323E712F,$2E545750666C712F
   Data.q $28733231732D2938,$28323E712D7D6F6E,$7D6665712F382933,$7D7D7D7D7D7D7D7D,$57507D7D7D7D7D7D
   Data.q $733C2F3F7D2D1D54,$1F1111797D343328,$575066652D303829,$7D7D57507D7D7D7D,$7D7D7D7D57507D7D
   Data.q $57507D7D7D7D5750,$292E72727D7D7D7D,$73313C3F32313A73,$3C0206547D6F6E28,$50666D3C71002F2F
   Data.q $2E72727D7D7D7D57,$313C3F32313A7329,$0206547D6F6E2873,$3C710069762F2F3C,$7D7D7D7D5750666C
   Data.q $32313A73292E7272,$7D6F6E2873313C3F,$65762F2F3C020654,$7D5750666F3C7100,$73292E72727D7D7D
   Data.q $2873313C3F32313A,$2F3C0206547D6F6E,$6E3C71006F6C762F,$727D7D7D7D575066,$3F32313A73292E72
   Data.q $547D6F6E2873313C,$6B6C762F2F3C0206,$7D575066693C7100,$73292E72727D7D7D,$2873313C3F32313A
   Data.q $2F3C0206547D6F6E,$683C71006D6F762F,$727D7D7D7D575066,$3F32313A73292E72,$547D6F6E2873313C
   Data.q $696F762F2F3C0206,$7D5750666B3C7100,$73292E72727D7D7D,$2873313C3F32313A,$2F3C0206547D6F6E
   Data.q $6A3C7100656F762F,$2F7D7D7D7D575066,$5066343328732938,$5750575057502057,$73757D3E33283B73
   Data.q $6F6E3F737D3A382F,$382F737D716D3C7D,$3C7D6F6E3F737D3A,$7D3A382F737D716C,$716F3C7D6F6E3F73
   Data.q $3F737D3A382F737D,$737D716E3C7D6F6E,$6F6E3F737D3A382F,$382F737D71693C7D,$3C7D6F6E3F737D3A
   Data.q $7D3A382F737D7168,$716B3C7D6F6E3F73,$3F737D3A382F737D,$1E7D746A3C7D6F6E,$1918150E14111C12
   Data.q $69091314191C180F,$737D3A382F73757D,$2F2F3C027D696B3F,$7D7D7D2657507D74,$2F737D7D7D7D5750
   Data.q $2D737D7D7D7D3A38,$5750662D7D39382F,$3A382F737D7D7D7D,$7D7D7D6B6C3F737D,$3419393C382F3529
   Data.q $3E32313F7D7D7130,$313F7D7130341936,$7D71253914363E32,$3914393C382F3529,$7D7D7D7D57506625
   Data.q $6F6E3F733A382F73,$297D712D3038297D,$38307D716F2D3038,$716F3038307D7130,$14393C382F35297D
   Data.q $711E297D716F6E39,$2F38293328323E7D,$7D57507D7D7D7D66,$733A382F737D7D7D,$3F3C29027D696B3F
   Data.q $297D712F2F3C3831,$507D66696B2D3038,$7D57507D7D7D7D57,$28732B32307D7D7D,$3E32313F7D7D6B6C
   Data.q $293E787125391436,$575066257339343C,$6B6C28732B323054,$19363E32313F7D7D,$3C293E3378713034
   Data.q $5457506625733934,$7D6B6C28732B3230,$19393C382F35297D,$3934293378713034,$3230545750662573
   Data.q $297D7D6B6C28732B,$253914393C382F35,$6625733934297871,$57507D7D7D7D5750,$733128307D7D7D7D
   Data.q $6B6C28733839342A,$313F712D3038297D,$2971253914363E32,$303419393C382F35,$7D7D7D7D57507D66
   Data.q $736F6E2873292B3E,$2F35297D7D6B6C28,$7D6F6E3914393C38,$14393C382F352971,$7D7D7D5750662539
   Data.q $6F6E287339393C7D,$14393C382F35297D,$303829717D6F6E39,$393C382F3529712D,$5750667D6F6E3914
   Data.q $7D7D57507D7D7D7D,$6E3F732B32307D7D,$38293328323E7D6F,$50666D6D256D712F,$38291F1111795457
   Data.q $7254575067692D30,$8DEC8DDE8CE28D72,$8DDE8CE78DE58DE6,$E38DE28D7DE18DE8,$E88DDF8CE08DE58D
   Data.q $8DE08D7D6C7DDD8C,$E08D7DE58DDF8CE5,$77777777697DED8D,$7777777777777777,$3C54575054575077
   Data.q $297D6F6E3F733933,$393C382F3529711E,$6C256D716F6E3914,$DF8C72725454663B,$E78DD18CE68DE38D
   Data.q $502D2F3C2A7DE38D,$6E3F732F352E5457,$711E29711E297D6F,$31352E545750666F,$3038297D6F6E3F73
   Data.q $38293328323E712D,$3C545750666E712F,$297D6F6E28733939,$303829711E29711E,$7D7D7D7D5750662D
   Data.q $72727D7D7D7D5750,$E98DEB8DED8DE78D,$8DE08D7DD28CED8D,$E78D7DD18CDF8CE5,$E88DE98DED8DE68D
   Data.q $7D2F2F3C027DDF8C,$2D303829267DEF8D,$50206F2D30382971,$3E72727D7D7D7D57,$6D672F3829332832
   Data.q $72727D7D7D7D5750,$E58DE08D7D6E706D,$8DEC8D7DE58DDF8C,$7DDF8CDE8CDD8CE8,$8DE08D7D6D7DDC8C
   Data.q $7D5750E58DDF8CE5,$6A706972727D7D7D,$8DDF8CE58DE08D7D,$DD8CE88DEC8D7DE5,$7DDC8C7DDF8CDE8C
   Data.q $DF8CE58DE08D7D6C,$7D7D7D7D5750E58D,$8D7D6C6C70657272,$7DE58DDF8CE58DE0,$DE8CDD8CE88DEC8D
   Data.q $7D6F7DDC8C7DDF8C,$E58DDF8CE58DE08D,$72727D7D7D7D5750,$E08D7D686C706F6C,$8D7DE58DDF8CE58D
   Data.q $8CDE8CDD8CE88DEC,$8D7D6E7DDC8C7DDF,$50E58DDF8CE58DE0,$6C72727D7D7D7D57,$8DE08D7D646C706B
   Data.q $EC8D7DE58DDF8CE5,$DF8CDE8CDD8CE88D,$E08D7D697DDC8C7D,$5750E58DDF8CE58D,$6D6F72727D7D7D7D
   Data.q $E58DE08D7D6E6F70,$8DEC8D7DE58DDF8C,$7DDF8CDE8CDD8CE8,$8DE08D7D687DDC8C,$7D5750E58DDF8CE5
   Data.q $70696F72727D7D7D,$8CE58DE08D7D6A6F,$E88DEC8D7DE58DDF,$8C7DDF8CDE8CDD8C,$E58DE08D7D6B7DDC
   Data.q $7D7D5750E58DDF8C,$6E70656F72727D7D,$DF8CE58DE08D7D6C,$8CE88DEC8D7DE58D,$DC8C7DDF8CDE8CDD
   Data.q $8CE58DE08D7D6A7D,$7D7D7D5750E58DDF,$727D7D7D7D57507D,$2F38293328323E72,$7D7D7D7D57506C67
   Data.q $E08D7D6E706D7272,$8D7DE58DDF8CE58D,$8CDE8CDD8CE88DEC,$8D7D657DDC8C7DDF,$50E58DDF8CE58DE0
   Data.q $6972727D7D7D7D57,$8CE58DE08D7D6A70,$E88DEC8D7DE58DDF,$8C7DDF8CDE8CDD8C,$E58DE08D7D647DDC
   Data.q $7D7D5750E58DDF8C,$6C6C706572727D7D,$8DDF8CE58DE08D7D,$DD8CE88DEC8D7DE5,$7DDC8C7DDF8CDE8C
   Data.q $8CE58DE08D7D6D6C,$7D7D7D5750E58DDF,$686C706F6C72727D,$8DDF8CE58DE08D7D,$DD8CE88DEC8D7DE5
   Data.q $7DDC8C7DDF8CDE8C,$8CE58DE08D7D6C6C,$7D7D7D5750E58DDF,$646C706B6C72727D,$8DDF8CE58DE08D7D
   Data.q $DD8CE88DEC8D7DE5,$7DDC8C7DDF8CDE8C,$8CE58DE08D7D6F6C,$7D7D7D5750E58DDF,$6E6F706D6F72727D
   Data.q $8DDF8CE58DE08D7D,$DD8CE88DEC8D7DE5,$7DDC8C7DDF8CDE8C,$8CE58DE08D7D6E6C,$7D7D7D5750E58DDF
   Data.q $6A6F70696F72727D,$8DDF8CE58DE08D7D,$DD8CE88DEC8D7DE5,$7DDC8C7DDF8CDE8C,$8CE58DE08D7D696C
   Data.q $7D7D7D5750E58DDF,$6C6E70656F72727D,$8DDF8CE58DE08D7D,$DD8CE88DEC8D7DE5,$7DDC8C7DDF8CDE8C
   Data.q $8CE58DE08D7D686C,$7D7D7D5750E58DDF,$727D7D7D7D57507D,$ED8DDF8C7DE58D72,$8DED8DE98D7DE78D
   Data.q $7D7D7DE88DE88DE6,$307D7D7D7D57507D,$267D696B3F732B32,$303829712D303829,$2F2F3C0271206F2D
   Data.q $2E7D7D7D7D575066,$7325393473313B35,$3038297D7D6F6E3F,$712D3038297D712D,$661E297D711E297D
   Data.q $7D7D57507D7D7D7D,$3473313B352E7D7D,$7D7D6F6E3F732539,$297D716F2D303829,$1E297D716F2D3038
   Data.q $545750661E297D71,$7D696B3F732B3230,$2F3C38313F3C2902,$712D30382926712F,$5466206F2D303829
   Data.q $7D7D7D5750545750,$6F6E3F7339333C7D,$29716F2D3038297D,$6E3914393C382F35,$7D666E6D256D716F
   Data.q $297D362E3C307272,$57502E393C382F35,$733128307D7D7D7D,$6F6E28733839342A,$71696B2D3038297D
   Data.q $6665716F2D303829,$3C7D7D7D7D57507D,$027D696B28733939,$2F2F3C38313F3C29,$3C38313F3C290271
   Data.q $6B2D303829712F2F,$7D7D7D7D57506669,$3C3F32313A733931,$6F6E28736F2B7331,$7D712D303829267D
   Data.q $0671206F2D303829,$2F3C38313F3C2902,$7D7D7D575066002F,$727D7D7D7D57507D,$8DE08DE38DE78D72
   Data.q $8DDF8CDD8CE88DEF,$8DE88DDE8CDD8CE5,$EA8DE88DDD8C7DE1,$DF8CD18CE68DDE8C,$8D7D697DDF8CED8D
   Data.q $8DE88DDF8CE58DE0,$77776C7DEF8D7DE4,$7777777777777777,$5054575054777777,$39333C5457505457
   Data.q $3038307D6F6E3F73,$14393C382F352971,$6A6D256D716F6E39,$7331352E54575066,$71303830546F6E3F
   Data.q $5750666F71303830,$7339333C54575054,$6F3038307D6F6E3F,$14393C382F352971,$3B6C256D716F6E39
   Data.q $732F352E54575066,$6F303830546F6E3F,$666E716F30383071,$732D29382E545750,$2D7D6F6E28732C38
   Data.q $2F38293328323E71,$5750666F30383071,$313B352E54575054,$6F6E3F7325393473,$38297D711E297D7D
   Data.q $713038307D712D30,$545750663038307D,$6F6E28732D31382E,$711E29716D3C547D,$7D5750662D716D3C
   Data.q $73313B352E7D7D7D,$7D6F6E3F73253934,$3038297D711E297D,$713038307D716F2D,$545750663038307D
   Data.q $6F6E28732D31382E,$711E29716C3C547D,$545750662D716C3C,$546F6E287339393C,$7130383071303830
   Data.q $545750545750666C,$25393473313B352E,$1E297D7D6F6E3F73,$7D712D3038297D71,$3038307D71303830
   Data.q $2D31382E54575066,$6F3C547D6F6E2873,$2D716F3C711E2971,$2E7D7D7D7D575066,$7325393473313B35
   Data.q $711E297D7D6F6E3F,$7D716F2D3038297D,$3038307D71303830,$2D31382E54575066,$6E3C547D6F6E2873
   Data.q $2D716E3C711E2971,$7339393C54575066,$71303830546F6E28,$5750666C71303830,$2E7D7D7D7D575054
   Data.q $7325393473313B35,$711E297D7D6F6E3F,$307D712D3038297D,$663038307D713038,$732D31382E545750
   Data.q $71693C547D6F6E28,$662D71693C711E29,$352E7D7D7D7D5750,$3F7325393473313B,$7D711E297D7D6F6E
   Data.q $307D716F2D303829,$663038307D713038,$732D31382E545750,$71683C547D6F6E28,$662D71683C711E29
   Data.q $287339393C545750,$3071303830546F6E,$7D5750666C713038,$7D7D7D57507D7D7D,$393473313B352E7D
   Data.q $297D7D6F6E3F7325,$712D3038297D711E,$38307D713038307D,$31382E5457506630,$3C547D6F6E28732D
   Data.q $716B3C711E29716B,$7D7D7D7D5750662D,$25393473313B352E,$1E297D7D6F6E3F73,$716F2D3038297D71
   Data.q $38307D713038307D,$31382E5457506630,$3C547D6F6E28732D,$716A3C711E29716A,$7D5750545750662D
   Data.q $7D7D7D57507D7D7D,$8DE78DE38D72727D,$8DED8DDA8CE08DE3,$E78D7DE88DE58DE0,$E88DEF8DE08DE38D
   Data.q $DD8CE58DDF8CDD8C,$8C7DE18DE88DDE8C,$8DDE8CEA8DE88DDD,$8CED8DDF8CD18CE6,$E58DE08D7D697DDF
   Data.q $8D7DE48DE88DDF8C,$77777777776C7DEF,$7777777777777777,$393C545750545750,$7D7D7D6F6E287339
   Data.q $38293328323E7D7D,$38293328323E712F,$2E545750666C712F,$28733231732D2938,$28323E712D7D6F6E
   Data.q $7D6669712F382933,$7D7D7D7D7D7D7D7D,$57507D7D7D7D7D7D,$733C2F3F7D2D1D54,$1F1111797D343328
   Data.q $7D7D66692D303829,$7D7D7D7D57507D7D,$382F7D7D7D7D5750,$5750663433287329,$283B735750575020
   Data.q $3A382F73757D3E33,$6D3C7D6F6E3F737D,$737D3A382F737D71,$7D716C3C7D6F6E3F,$6E3F737D3A382F73
   Data.q $2F737D716F3C7D6F,$7D6F6E3F737D3A38,$3A382F737D716E3C,$693C7D6F6E3F737D,$737D3A382F737D71
   Data.q $7D71683C7D6F6E3F,$6E3F737D3A382F73,$2F737D716B3C7D6F,$7D6F6E3F737D3A38,$111C121E7D746A3C
   Data.q $1C180F1918150E14,$73757D6F09131419,$696B3F737D3A382F,$507D742F2F3C027D,$7D57507D7D7D2657
   Data.q $7D3A382F737D7D7D,$39382F2D737D7D7D,$7D7D7D5750662D7D,$3F737D3A382F737D,$2F35297D7D7D6B6C
   Data.q $7D71303419393C38,$3419363E32313F7D,$363E32313F7D7130,$2F35297D71253914,$5066253914393C38
   Data.q $382F737D7D7D7D57,$38297D6F6E3F733A,$2D3038297D712D30,$6E2D3038297D716F,$71692D3038297D71
   Data.q $38307D713038307D,$382F35297D716F30,$7D716F6E3914393C,$3328323E7D711E29,$7D7D7D7D662F3829
   Data.q $2F737D7D7D7D5750,$027D696B3F733A38,$2F2F3C38313F3C29,$696B2D3038297D71,$7D7D7D7D57507D66
   Data.q $32307D7D7D7D5750,$3F7D7D6B6C28732B,$71253914363E3231,$257339343C293E78,$732B323054575066
   Data.q $32313F7D7D6B6C28,$337871303419363E,$66257339343C293E,$28732B3230545750,$382F35297D7D6B6C
   Data.q $337871303419393C,$5750662573393429,$6B6C28732B323054,$393C382F35297D7D,$3934297871253914
   Data.q $7D7D7D5750662573,$307D7D7D7D57507D,$733839342A733128,$2D3038297D6B6C28,$3914363E32313F71
   Data.q $393C382F35297125,$7D57507D66303419,$2873292B3E7D7D7D,$7D7D6B6C28736F6E,$3914393C382F3529
   Data.q $382F3529717D6F6E,$575066253914393C,$7339393C7D7D7D7D,$382F35297D6F6E28,$717D6F6E3914393C
   Data.q $2F3529712D303829,$7D6F6E3914393C38,$507D7D7D7D575066,$2F32257D7D7D7D57,$28323E7D6F6E3F73
   Data.q $28323E712F382933,$323E7D712F382933,$5750662F38293328,$732F3C3F7D7D7D7D,$50666D7D3E33242E
   Data.q $38291F1111795457,$72545750676F2D30,$8DEC8DDE8CE28D72,$8DDE8CE78DE58DE6,$E38DE28D7DE18DE8
   Data.q $E88DDF8CE08DE58D,$8DE08D7D6C7DDD8C,$E08D7DE58DDF8CE5,$777777776F7DED8D,$7777777777777777
   Data.q $3C54575054575077,$297D6F6E3F733933,$393C382F3529711E,$6C256D716F6E3914,$DF8C72725454663B
   Data.q $E78DD18CE68DE38D,$502D2F3C2A7DE38D,$6E3F732F352E5457,$711E29711E297D6F,$31352E545750666C
   Data.q $3038297D6F6E3F73,$38293328323E712D,$3C5457506669712F,$297D6F6E28733939,$303829711E29711E
   Data.q $7D7D7D7D5750662D,$72727D7D7D7D5750,$E98DEB8DED8DE78D,$8DE08D7DD28CED8D,$E78D7DD18CDF8CE5
   Data.q $E88DE98DED8DE68D,$7D2F2F3C027DDF8C,$2D303829267DEF8D,$50206F2D30382971,$3E72727D7D7D7D57
   Data.q $6D672F3829332832,$72727D7D7D7D5750,$E58DE08D7D6C706D,$8DEC8D7DE58DDF8C,$7DDF8CDE8CDD8CE8
   Data.q $8DE08D7D6D7DDC8C,$7D5750E58DDF8CE5,$6E706F72727D7D7D,$8DDF8CE58DE08D7D,$DD8CE88DEC8D7DE5
   Data.q $7DDC8C7DDF8CDE8C,$DF8CE58DE08D7D6C,$7D7D7D7D5750E58D,$E08D7D6870697272,$8D7DE58DDF8CE58D
   Data.q $8CDE8CDD8CE88DEC,$8D7D6F7DDC8C7DDF,$50E58DDF8CE58DE0,$6B72727D7D7D7D57,$8CE58DE08D7D6A70
   Data.q $E88DEC8D7DE58DDF,$8C7DDF8CDE8CDD8C,$E58DE08D7D6E7DDC,$7D7D5750E58DDF8C,$7D64706572727D7D
   Data.q $E58DDF8CE58DE08D,$8CDD8CE88DEC8D7D,$697DDC8C7DDF8CDE,$8DDF8CE58DE08D7D,$727D7D7D7D5750E5
   Data.q $8D7D6C6C706D6C72,$7DE58DDF8CE58DE0,$DE8CDD8CE88DEC8D,$7D687DDC8C7DDF8C,$E58DDF8CE58DE08D
   Data.q $72727D7D7D7D5750,$E08D7D6E6C706F6C,$8D7DE58DDF8CE58D,$8CDE8CDD8CE88DEC,$8D7D6B7DDC8C7DDF
   Data.q $50E58DDF8CE58DE0,$6C72727D7D7D7D57,$8DE08D7D686C7069,$EC8D7DE58DDF8CE5,$DF8CDE8CDD8CE88D
   Data.q $E08D7D6A7DDC8C7D,$5750E58DDF8CE58D,$6B6C72727D7D7D7D,$E58DE08D7D6A6C70,$8DEC8D7DE58DDF8C
   Data.q $7DDF8CDE8CDD8CE8,$8DE08D7D657DDC8C,$7D5750E58DDF8CE5,$70656C72727D7D7D,$8CE58DE08D7D646C
   Data.q $E88DEC8D7DE58DDF,$8C7DDF8CDE8CDD8C,$E58DE08D7D647DDC,$7D7D5750E58DDF8C,$6F706D6F72727D7D
   Data.q $DF8CE58DE08D7D6C,$8CE88DEC8D7DE58D,$DC8C7DDF8CDE8CDD,$E58DE08D7D6D6C7D,$7D7D5750E58DDF8C
   Data.q $6F706F6F72727D7D,$DF8CE58DE08D7D6E,$8CE88DEC8D7DE58D,$DC8C7DDF8CDE8CDD,$E58DE08D7D6C6C7D
   Data.q $7D7D5750E58DDF8C,$6F70696F72727D7D,$DF8CE58DE08D7D68,$8CE88DEC8D7DE58D,$DC8C7DDF8CDE8CDD
   Data.q $E58DE08D7D6F6C7D,$7D7D5750E58DDF8C,$6F706B6F72727D7D,$DF8CE58DE08D7D6A,$8CE88DEC8D7DE58D
   Data.q $DC8C7DDF8CDE8CDD,$E58DE08D7D6E6C7D,$7D7D5750E58DDF8C,$6F70656F72727D7D,$DF8CE58DE08D7D64
   Data.q $8CE88DEC8D7DE58D,$DC8C7DDF8CDE8CDD,$E58DE08D7D696C7D,$7D7D5750E58DDF8C,$6E706D6E72727D7D
   Data.q $DF8CE58DE08D7D6C,$8CE88DEC8D7DE58D,$DC8C7DDF8CDE8CDD,$E58DE08D7D686C7D,$7D7D5750E58DDF8C
   Data.q $7D7D7D7D57507D7D,$38293328323E7272,$7D7D7D57506C672F,$8D7D6C706D72727D,$7DE58DDF8CE58DE0
   Data.q $DE8CDD8CE88DEC8D,$6B6C7DDC8C7DDF8C,$8DDF8CE58DE08D7D,$727D7D7D7D5750E5,$8DE08D7D6E706F72
   Data.q $EC8D7DE58DDF8CE5,$DF8CDE8CDD8CE88D,$8D7D6A6C7DDC8C7D,$50E58DDF8CE58DE0,$6972727D7D7D7D57
   Data.q $8CE58DE08D7D6870,$E88DEC8D7DE58DDF,$8C7DDF8CDE8CDD8C,$8DE08D7D656C7DDC,$7D5750E58DDF8CE5
   Data.q $6A706B72727D7D7D,$8DDF8CE58DE08D7D,$DD8CE88DEC8D7DE5,$7DDC8C7DDF8CDE8C,$8CE58DE08D7D646C
   Data.q $7D7D7D5750E58DDF,$8D7D64706572727D,$7DE58DDF8CE58DE0,$DE8CDD8CE88DEC8D,$6D6F7DDC8C7DDF8C
   Data.q $8DDF8CE58DE08D7D,$727D7D7D7D5750E5,$8D7D6C6C706D6C72,$7DE58DDF8CE58DE0,$DE8CDD8CE88DEC8D
   Data.q $6C6F7DDC8C7DDF8C,$8DDF8CE58DE08D7D,$727D7D7D7D5750E5,$8D7D6E6C706F6C72,$7DE58DDF8CE58DE0
   Data.q $DE8CDD8CE88DEC8D,$6F6F7DDC8C7DDF8C,$8DDF8CE58DE08D7D,$727D7D7D7D5750E5,$8D7D686C70696C72
   Data.q $7DE58DDF8CE58DE0,$DE8CDD8CE88DEC8D,$6E6F7DDC8C7DDF8C,$8DDF8CE58DE08D7D,$727D7D7D7D5750E5
   Data.q $8D7D6A6C706B6C72,$7DE58DDF8CE58DE0,$DE8CDD8CE88DEC8D,$696F7DDC8C7DDF8C,$8DDF8CE58DE08D7D
   Data.q $727D7D7D7D5750E5,$8D7D646C70656C72,$7DE58DDF8CE58DE0,$DE8CDD8CE88DEC8D,$686F7DDC8C7DDF8C
   Data.q $8DDF8CE58DE08D7D,$727D7D7D7D5750E5,$8D7D6C6F706D6F72,$7DE58DDF8CE58DE0,$DE8CDD8CE88DEC8D
   Data.q $6B6F7DDC8C7DDF8C,$8DDF8CE58DE08D7D,$727D7D7D7D5750E5,$8D7D6E6F706F6F72,$7DE58DDF8CE58DE0
   Data.q $DE8CDD8CE88DEC8D,$6A6F7DDC8C7DDF8C,$8DDF8CE58DE08D7D,$727D7D7D7D5750E5,$8D7D686F70696F72
   Data.q $7DE58DDF8CE58DE0,$DE8CDD8CE88DEC8D,$656F7DDC8C7DDF8C,$8DDF8CE58DE08D7D,$727D7D7D7D5750E5
   Data.q $8D7D6A6F706B6F72,$7DE58DDF8CE58DE0,$DE8CDD8CE88DEC8D,$646F7DDC8C7DDF8C,$8DDF8CE58DE08D7D
   Data.q $727D7D7D7D5750E5,$8D7D646F70656F72,$7DE58DDF8CE58DE0,$DE8CDD8CE88DEC8D,$6D6E7DDC8C7DDF8C
   Data.q $8DDF8CE58DE08D7D,$727D7D7D7D5750E5,$8D7D6C6E706D6E72,$7DE58DDF8CE58DE0,$DE8CDD8CE88DEC8D
   Data.q $6C6E7DDC8C7DDF8C,$8DDF8CE58DE08D7D,$507D7D7D7D5750E5,$8D72727D7D7D7D57,$E78DED8DDF8C7DE5
   Data.q $8DE68DED8DE98D7D,$507D7D7D7DE88DE8,$2B32307D7D7D7D57,$3829267D696B3F73,$6F2D303829712D30
   Data.q $50662F2F3C027120,$3B352E7D7D7D7D57,$6E3F732539347331,$712D3038297D7D6F,$297D712D3038297D
   Data.q $7D7D661E297D711E,$7D7D7D7D57507D7D,$25393473313B352E,$38297D7D6F6E3F73,$3038297D716F2D30
   Data.q $7D711E297D716F2D,$3230545750661E29,$29027D696B3F732B,$712F2F3C38313F3C,$3829712D30382926
   Data.q $57505466206F2D30,$3C7D7D7D7D575054,$297D6F6E3F733933,$2F3529716F2D3038,$716F6E3914393C38
   Data.q $72727D666C6D256D,$2F35297D362E3C30,$7D7D57502E393C38,$342A733128307D7D,$297D6F6E28733839
   Data.q $382971696B2D3038,$7D666B6C716F2D30,$393C7D7D7D7D5750,$29027D696B287339,$712F2F3C38313F3C
   Data.q $2F3C38313F3C2902,$696B2D303829712F,$507D7D7D7D575066,$7339317D7D7D7D57,$383134293C31322B
   Data.q $73313C3F32313A73,$267D6F6E2873692B,$38297D712D303829,$3038297D716F2D30,$2D3038297D716E2D
   Data.q $3F3C290206712069,$5066002F2F3C3831,$7D57507D7D7D7D57,$8DE78D72727D7D7D,$8CE88DEF8DE08DE3
   Data.q $8CDD8CE58DDF8CDD,$DD8C7DE18DE88DDE,$E68DDE8CEA8DE88D,$DF8CED8DDF8CD18C,$8CE58DE08D7D6F7D
   Data.q $EF8D7DE48DE88DDF,$7777777777776C7D,$5477777777777777,$5750545750545750,$6F6E3F7339333C54
   Data.q $2F3529713038307D,$716F6E3914393C38,$545750663B6D256D,$546F6E3F7331352E,$7130383071303830
   Data.q $545750545750666C,$7D6F6E3F7339333C,$2F3529716F303830,$716F6E3914393C38,$545750663B6C256D
   Data.q $546F6E3F732F352E,$303830716F303830,$2E5457506669716F,$28732C38732D2938,$28323E712D7D6F6E
   Data.q $303830712F382933,$545750545750666F,$25393473313B352E,$1E297D7D6F6E3F73,$7D712D3038297D71
   Data.q $3038307D71303830,$2D31382E54575066,$6D3C547D6F6E2873,$2D716D3C711E2971,$2E7D7D7D7D575066
   Data.q $7325393473313B35,$711E297D7D6F6E3F,$7D716F2D3038297D,$3038307D71303830,$2D31382E54575066
   Data.q $6C3C547D6F6E2873,$2D716C3C711E2971,$2E7D7D7D7D575066,$7325393473313B35,$711E297D7D6F6E3F
   Data.q $7D716E2D3038297D,$3038307D71303830,$2D31382E54575066,$6F3C547D6F6E2873,$2D716F3C711E2971
   Data.q $2E7D7D7D7D575066,$7325393473313B35,$711E297D7D6F6E3F,$7D71692D3038297D,$3038307D71303830
   Data.q $2D31382E54575066,$6E3C547D6F6E2873,$2D716E3C711E2971,$7339393C54575066,$71303830546F6E28
   Data.q $5750666C71303830,$313B352E54575054,$6F6E3F7325393473,$38297D711E297D7D,$713038307D712D30
   Data.q $545750663038307D,$6F6E28732D31382E,$711E2971693C547D,$7D5750662D71693C,$73313B352E7D7D7D
   Data.q $7D6F6E3F73253934,$3038297D711E297D,$713038307D716F2D,$545750663038307D,$6F6E28732D31382E
   Data.q $711E2971683C547D,$7D5750662D71683C,$73313B352E7D7D7D,$7D6F6E3F73253934,$3038297D711E297D
   Data.q $713038307D716E2D,$545750663038307D,$6F6E28732D31382E,$711E29716B3C547D,$7D5750662D716B3C
   Data.q $73313B352E7D7D7D,$7D6F6E3F73253934,$3038297D711E297D,$713038307D71692D,$545750663038307D
   Data.q $6F6E28732D31382E,$711E29716A3C547D,$545750662D716A3C,$57507D7D7D7D5750,$E38D72727D7D7D7D
   Data.q $DA8CE08DE38DE78D,$E88DE58DE08DED8D,$8DE08DE38DE78D7D,$8DDF8CDD8CE88DEF,$8DE88DDE8CDD8CE5
   Data.q $EA8DE88DDD8C7DE1,$DF8CD18CE68DDE8C,$8D7D6F7DDF8CED8D,$8DE88DDF8CE58DE0,$77776C7DEF8D7DE4
   Data.q $7777777777777777,$5750545750777777,$6F6E287339393C54,$28323E7D7D7D7D7D,$28323E712F382933
   Data.q $50666C712F382933,$31732D29382E5457,$712D7D6F6E287332,$712F38293328323E,$7D7D7D7D7D7D666F
   Data.q $7D7D7D7D7D7D7D7D,$3F7D2D1D5457507D,$797D343328733C2F,$6F2D3038291F1111,$7D57507D7D7D7D66
   Data.q $7D7D7D57507D7D7D,$3433287329382F7D,$5750575020575066,$73757D3E33283B73,$6F6E3F737D3A382F
   Data.q $382F737D716D3E7D,$3E7D6F6E3F737D3A,$7D3A382F737D716C,$716F3E7D6F6E3F73,$3F737D3A382F737D
   Data.q $737D716E3E7D6F6E,$6F6E3F737D3A382F,$382F737D71693E7D,$3E7D6F6E3F737D3A,$7D3A382F737D7168
   Data.q $716B3E7D6F6E3F73,$3F737D3A382F737D,$077D746A3E7D6F6E,$575074757D120F18,$7D7D7D7D5750267D
   Data.q $7D6F6E3F732F3225,$7D716D3E7D716D3E,$7D7D7D5750666D3E,$6F6E3F732F32257D,$716C3E7D716C3E7D
   Data.q $7D7D5750666C3E7D,$6E3F732F32257D7D,$6F3E7D716F3E7D6F,$7D5750666F3E7D71,$3F732F32257D7D7D
   Data.q $3E7D716E3E7D6F6E,$5750666E3E7D716E,$732F32257D7D7D7D,$7D71693E7D6F6E3F,$5066693E7D71693E
   Data.q $2F32257D7D7D7D57,$71683E7D6F6E3F73,$66683E7D71683E7D,$32257D7D7D7D5750,$6B3E7D6F6E3F732F
   Data.q $6B3E7D716B3E7D71,$257D7D7D7D575066,$3E7D6F6E3F732F32,$3E7D716A3E7D716A,$7D7D7D7D5750666A
   Data.q $663433287329382F,$507D5750207D5750,$33283B7357507D57,$7D3A382F73757D3E,$716D3F7D6F6E3F73
   Data.q $3F737D3A382F737D,$737D716C3F7D6F6E,$6F6E3F737D3A382F,$382F737D716F3F7D,$3F7D6F6E3F737D3A
   Data.q $7D3A382F737D716E,$71693F7D6F6E3F73,$3F737D3A382F737D,$737D71683F7D6F6E,$6F6E3F737D3A382F
   Data.q $382F737D716B3F7D,$3F7D6F6E3F737D3A,$1F040D121E7D746A,$73757D0913141A14,$6F6E3F737D3A382F
   Data.q $382F737D716D3C7D,$3C7D6F6E3F737D3A,$7D3A382F737D716C,$716F3C7D6F6E3F73,$3F737D3A382F737D
   Data.q $737D716E3C7D6F6E,$6F6E3F737D3A382F,$382F737D71693C7D,$3C7D6F6E3F737D3A,$7D3A382F737D7168
   Data.q $716B3C7D6F6E3F73,$3F737D3A382F737D,$507D746A3C7D6F6E,$7D7D7D5750267D57,$6F6E28732B32307D
   Data.q $666D3C716D3F7D7D,$32307D7D7D7D5750,$3F7D7D6F6E28732B,$7D5750666C3C716C,$28732B32307D7D7D
   Data.q $3C716F3F7D7D6F6E,$7D7D7D7D5750666F,$7D6F6E28732B3230,$50666E3C716E3F7D,$2B32307D7D7D7D57
   Data.q $693F7D7D6F6E2873,$7D7D575066693C71,$6E28732B32307D7D,$683C71683F7D7D6F,$307D7D7D7D575066
   Data.q $7D7D6F6E28732B32,$5750666B3C716B3F,$732B32307D7D7D7D,$716A3F7D7D6F6E28,$7D7D7D5750666A3C
   Data.q $3433287329382F7D,$7D5750207D575066,$3E33283B737D5750,$737D3A382F73757D,$7D742C387D6F6E3F
   Data.q $73757D1114080C18,$6F6E3F737D3A382F,$382F737D716D3C7D,$3C7D6F6E3F737D3A,$7D3A382F737D716C
   Data.q $716F3C7D6F6E3F73,$3F737D3A382F737D,$737D716E3C7D6F6E,$6F6E3F737D3A382F,$382F737D71693C7D
   Data.q $3C7D6F6E3F737D3A,$7D3A382F737D7168,$716B3C7D6F6E3F73,$3F737D3A382F737D,$7D7D716A3C7D6F6E
   Data.q $3F737D3A382F737D,$737D716D3F7D6F6E,$6F6E3F737D3A382F,$382F737D716C3F7D,$3F7D6F6E3F737D3A
   Data.q $7D3A382F737D716F,$716E3F7D6F6E3F73,$3F737D3A382F737D,$737D71693F7D6F6E,$6F6E3F737D3A382F
   Data.q $382F737D71683F7D,$3F7D6F6E3F737D3A,$7D3A382F737D716B,$746A3F7D6F6E3F73,$7D5750267D57507D
   Data.q $7D3A382F737D7D7D,$39382F2D737D7D7D,$7D7D7D5750662D7D,$2C38732D29382E7D,$3C712D7D6F6E2873
   Data.q $7D5750666D3F716D,$732D29382E7D7D7D,$28732C387339333C,$716C3C712D7D6F6E,$7D5750662D716C3F
   Data.q $732D29382E7D7D7D,$28732C387339333C,$716F3C712D7D6F6E,$7D5750662D716F3F,$732D29382E7D7D7D
   Data.q $28732C387339333C,$716E3C712D7D6F6E,$7D5750662D716E3F,$732D29382E7D7D7D,$28732C387339333C
   Data.q $71693C712D7D6F6E,$7D5750662D71693F,$732D29382E7D7D7D,$28732C387339333C,$71683C712D7D6F6E
   Data.q $7D5750662D71683F,$732D29382E7D7D7D,$28732C387339333C,$716B3C712D7D6F6E,$7D5750662D716B3F
   Data.q $732D29382E7D7D7D,$28732C387339333C,$716A3C712D7D6F6E,$7D5750662D716A3F,$732D31382E7D7D7D
   Data.q $712C38547D6F6E28,$5750662D716D716C,$7329382F7D7D7D7D,$207D575066343328,$737D7D57507D5750
   Data.q $2F73757D3E33283B,$7D6F6E3F737D3A38,$14080C187D742C38,$73757D1B111C1511,$6F6E3F737D3A382F
   Data.q $382F737D716D3C7D,$3C7D6F6E3F737D3A,$7D3A382F737D716C,$716F3C7D6F6E3F73,$3F737D3A382F737D
   Data.q $7D7D716E3C7D6F6E,$3F737D3A382F737D,$737D716D3F7D6F6E,$6F6E3F737D3A382F,$382F737D716C3F7D
   Data.q $3F7D6F6E3F737D3A,$7D3A382F737D716F,$746E3F7D6F6E3F73,$7D5750267D57507D,$7D3A382F737D7D7D
   Data.q $39382F2D737D7D7D,$7D7D7D5750662D7D,$2C38732D29382E7D,$3C712D7D6F6E2873,$7D5750666D3F716D
   Data.q $732D29382E7D7D7D,$28732C387339333C,$716C3C712D7D6F6E,$7D5750662D716C3F,$732D29382E7D7D7D
   Data.q $28732C387339333C,$716F3C712D7D6F6E,$7D5750662D716F3F,$732D29382E7D7D7D,$28732C387339333C
   Data.q $716E3C712D7D6F6E,$7D7D7D662D716E3F,$2E7D7D7D7D57507D,$7D6F6E28732D3138,$716D716C712C3854
   Data.q $7D7D7D7D5750662D,$663433287329382F,$507D5750207D5750,$7D3E33283B737D57,$3F737D3A382F7375
   Data.q $7D743B2E347D6F6E,$1413141B13140E14,$3A382F73757D0409,$6D3C7D6F6E3F737D,$737D3A382F737D71
   Data.q $7D716C3C7D6F6E3F,$6E3F737D3A382F73,$2F737D716F3C7D6F,$7D6F6E3F737D3A38,$3A382F737D716E3C
   Data.q $693C7D6F6E3F737D,$737D3A382F737D71,$7D71683C7D6F6E3F,$6E3F737D3A382F73,$2F737D716B3C7D6F
   Data.q $7D6F6E3F737D3A38,$267D57507D746A3C,$2F737D7D7D7D5750,$2D737D7D7D7D3A38,$5750662D7D39382F
   Data.q $3A382F737D7D7D7D,$6F6E3F737D7D7D7D,$3334716C3B33347D,$7D7D7D5750666F3B,$307D7D7D7D57507D
   Data.q $7D7D6F6E28732B32,$50666C716C3B3334,$2B32307D7D7D7D57,$33347D7D6F6E2873,$7D5750666D716F3B
   Data.q $7D7D7D57507D7D7D,$3833732D29382E7D,$3C712D7D6F6E2873,$3B3B3B3B256D716D,$7D5750663B3B3B3B
   Data.q $732D31382E7D7D7D,$3B3334547D6F6E28,$34716F3B3334716C,$5750662D716C3B33,$7D7D57507D7D7D7D
   Data.q $33732D29382E7D7D,$712D7D6F6E287338,$3B3B3B256D716C3C,$5750663B3B3B3B3B,$2D31382E7D7D7D7D
   Data.q $3334547D6F6E2873,$716F3B3334716C3B,$7D662D716C3B3334,$57507D7D7D7D5750,$2D29382E7D7D7D7D
   Data.q $7D6F6E2873383373,$3B256D716F3C712D,$663B3B3B3B3B3B3B,$382E7D7D7D7D5750,$547D6F6E28732D31
   Data.q $3B3334716C3B3334,$2D716C3B3334716F,$7D7D7D7D57507D66,$382E7D7D7D7D5750,$6E28733833732D29
   Data.q $6D716E3C712D7D6F,$3B3B3B3B3B3B3B25,$7D7D7D7D5750663B,$6F6E28732D31382E,$34716C3B3334547D
   Data.q $6C3B3334716F3B33,$7D7D57507D662D71,$7D7D7D7D57507D7D,$733833732D29382E,$693C712D7D6F6E28
   Data.q $3B3B3B3B3B256D71,$7D7D5750663B3B3B,$28732D31382E7D7D,$6C3B3334547D6F6E,$3334716F3B333471
   Data.q $57507D662D716C3B,$7D7D57507D7D7D7D,$33732D29382E7D7D,$712D7D6F6E287338,$3B3B3B256D71683C
   Data.q $5750663B3B3B3B3B,$2D31382E7D7D7D7D,$3334547D6F6E2873,$716F3B3334716C3B,$50662D716C3B3334
   Data.q $7D57507D7D7D7D57,$732D29382E7D7D7D,$2D7D6F6E28733833,$3B3B256D716B3C71,$50663B3B3B3B3B3B
   Data.q $31382E7D7D7D7D57,$34547D6F6E28732D,$6F3B3334716C3B33,$662D716C3B333471,$57507D7D7D7D5750
   Data.q $2D29382E7D7D7D7D,$7D6F6E2873383373,$3B256D716A3C712D,$663B3B3B3B3B3B3B,$382E7D7D7D7D5750
   Data.q $547D6F6E28732D31,$3B3334716C3B3334,$2D716C3B3334716F,$507D7D7D7D575066,$2B32307D7D7D7D57
   Data.q $2E347D7D6F6E2873,$50666C3B3334713B,$29382F7D7D7D7D57,$7D57506634332873,$3B7357507D575020
   Data.q $382F73757D3E3328,$3E7D6F6E3F737D3A,$7D3A382F737D716D,$716C3E7D6F6E3F73,$3F737D3A382F737D
   Data.q $737D716F3E7D6F6E,$6F6E3F737D3A382F,$382F737D716E3E7D,$3E7D6F6E3F737D3A,$7D3A382F737D7169
   Data.q $71683E7D6F6E3F73,$3F737D3A382F737D,$737D716B3E7D6F6E,$6F6E3F737D3A382F,$382F737D716A3E7D
   Data.q $3E7D6F6E3F737D3A,$191C7D74242F2F3C,$7D3A382F73757D19,$716D3C7D6F6E3F73,$3F737D3A382F737D
   Data.q $737D716C3C7D6F6E,$6F6E3F737D3A382F,$382F737D716F3C7D,$3C7D6F6E3F737D3A,$7D3A382F737D716E
   Data.q $71693C7D6F6E3F73,$3F737D3A382F737D,$737D71683C7D6F6E,$6F6E3F737D3A382F,$382F737D716B3C7D
   Data.q $3C7D6F6E3F737D3A,$382F737D7D7D716A,$3F7D6F6E3F737D3A,$7D3A382F737D716D,$716C3F7D6F6E3F73
   Data.q $3F737D3A382F737D,$737D716F3F7D6F6E,$6F6E3F737D3A382F,$382F737D716E3F7D,$3F7D6F6E3F737D3A
   Data.q $7D3A382F737D7169,$71683F7D6F6E3F73,$3F737D3A382F737D,$737D716B3F7D6F6E,$6F6E3F737D3A382F
   Data.q $7D57507D746A3F7D,$57507D7D7D575026,$7339393C7D7D7D7D,$3E7D6F6E28733E3E,$3F7D716A3C7D716A
   Data.q $7D7D7D7D5750666A,$733E3E733E39393C,$7D716B3E7D6F6E28,$50666B3F7D716B3C,$39393C7D7D7D7D57
   Data.q $6F6E28733E3E733E,$71683C7D71683E7D,$7D7D575066683F7D,$3E733E39393C7D7D,$693E7D6F6E28733E
   Data.q $693F7D71693C7D71,$3C7D7D7D7D575066,$28733E3E733E3939,$3C7D716E3E7D6F6E,$5750666E3F7D716E
   Data.q $3E39393C7D7D7D7D,$7D6F6E28733E3E73,$7D716F3C7D716F3E,$7D7D7D5750666F3F,$3E3E733E39393C7D
   Data.q $716C3E7D6F6E2873,$666C3F7D716C3C7D,$393C7D7D7D7D5750,$6E28733E3E733E39,$6D3C7D716D3E7D6F
   Data.q $7D5750666D3F7D71,$7D7D7D57507D7D7D,$6F6E3F732F32257D,$7D71242F2F3C3E7D,$3E7D71242F2F3C3E
   Data.q $7D575066242F2F3C,$733E39393C7D7D7D,$7D7D7D7D7D6F6E28,$716D71242F2F3C3E,$507D7D7D5750666D
   Data.q $29382F7D7D7D7D57,$2057506634332873,$33283B7357505750,$7D3A382F73757D3E,$716D3E7D6F6E3F73
   Data.q $3F737D3A382F737D,$737D716C3E7D6F6E,$6F6E3F737D3A382F,$382F737D716F3E7D,$3E7D6F6E3F737D3A
   Data.q $7D3A382F737D716E,$71693E7D6F6E3F73,$3F737D3A382F737D,$737D71683E7D6F6E,$6F6E3F737D3A382F
   Data.q $382F737D716B3E7D,$3E7D6F6E3F737D3A,$7D3A382F737D716A,$2F323F7D6F6E3F73,$1F080E7D742A322F
   Data.q $737D3A382F73757D,$7D716D3C7D6F6E3F,$6E3F737D3A382F73,$2F737D716C3C7D6F,$7D6F6E3F737D3A38
   Data.q $3A382F737D716F3C,$6E3C7D6F6E3F737D,$737D3A382F737D71,$7D71693C7D6F6E3F,$6E3F737D3A382F73
   Data.q $2F737D71683C7D6F,$7D6F6E3F737D3A38,$3A382F737D716B3C,$6A3C7D6F6E3F737D,$3A382F737D7D7D71
   Data.q $6D3F7D6F6E3F737D,$737D3A382F737D71,$7D716C3F7D6F6E3F,$6E3F737D3A382F73,$2F737D716F3F7D6F
   Data.q $7D6F6E3F737D3A38,$3A382F737D716E3F,$693F7D6F6E3F737D,$737D3A382F737D71,$7D71683F7D6F6E3F
   Data.q $6E3F737D3A382F73,$2F737D716B3F7D6F,$7D6F6E3F737D3A38,$267D57507D746A3F,$57507D7D7D7D5750
   Data.q $733F282E7D7D7D7D,$3E7D6F6E28733E3E,$3F7D716A3C7D716A,$7D7D7D7D5750666A,$733E3E733E3F282E
   Data.q $7D716B3E7D6F6E28,$50666B3F7D716B3C,$3F282E7D7D7D7D57,$6F6E28733E3E733E,$71683C7D71683E7D
   Data.q $7D7D575066683F7D,$3E733E3F282E7D7D,$693E7D6F6E28733E,$693F7D71693C7D71,$2E7D7D7D7D575066
   Data.q $28733E3E733E3F28,$3C7D716E3E7D6F6E,$5750666E3F7D716E,$3E3F282E7D7D7D7D,$7D6F6E28733E3E73
   Data.q $7D716F3C7D716F3E,$7D7D7D5750666F3F,$3E3E733E3F282E7D,$716C3E7D6F6E2873,$666C3F7D716C3C7D
   Data.q $282E7D7D7D7D5750,$6E28733E3E733E3F,$6D3C7D716D3E7D6F,$7D5750666D3F7D71,$7D7D7D57507D7D7D
   Data.q $6F6E3F732F32257D,$712A322F2F323F7D,$712A322F2F323F7D,$662A322F2F323F7D,$282E7D7D7D7D5750
   Data.q $7D7D6F6E28733E3F,$322F2F323F7D7D7D,$5750666D716D712A,$7339333C7D7D7D7D,$2F2F323F7D6F6E3F
   Data.q $2F2F323F7D712A32,$6C6D256D7D712A32,$2F7D7D7D7D575066,$5066343328732938,$3B73575057502057
   Data.q $382F73757D3E3328,$3E7D6F6E3F737D3A,$7D3A382F737D716D,$716C3E7D6F6E3F73,$3F737D3A382F737D
   Data.q $737D716F3E7D6F6E,$6F6E3F737D3A382F,$382F737D716E3E7D,$3E7D6F6E3F737D3A,$7D3A382F737D7169
   Data.q $71683E7D6F6E3F73,$3F737D3A382F737D,$737D716B3E7D6F6E,$6F6E3F737D3A382F,$1F080E7D746A3E7D
   Data.q $2F73757D0D191210,$7D6F6E3F737D3A38,$3A382F737D716D3C,$6C3C7D6F6E3F737D,$737D3A382F737D71
   Data.q $7D716F3C7D6F6E3F,$6E3F737D3A382F73,$2F737D716E3C7D6F,$7D6F6E3F737D3A38,$3A382F737D71693C
   Data.q $683C7D6F6E3F737D,$737D3A382F737D71,$7D716B3C7D6F6E3F,$6E3F737D3A382F73,$7D7D7D716A3C7D6F
   Data.q $6E3F737D3A382F73,$2F737D716D3F7D6F,$7D6F6E3F737D3A38,$3A382F737D716C3F,$6F3F7D6F6E3F737D
   Data.q $737D3A382F737D71,$7D716E3F7D6F6E3F,$6E3F737D3A382F73,$2F737D71693F7D6F,$7D6F6E3F737D3A38
   Data.q $3A382F737D71683F,$6B3F7D6F6E3F737D,$737D3A382F737D71,$7D746A3F7D6F6E3F,$7D7D5750267D5750
   Data.q $737D3A382F737D7D,$2F2F323F7D6F6E3F,$7D7D7D5750662A32,$7D7D7D3A382F737D,$2D7D39382F2D737D
   Data.q $7D57507D7D7D7D66,$7D3A382F737D7D7D,$612D787D6F6E3F73,$507D7D7D7D666365,$7D57507D7D7D7D57
   Data.q $323E7339317D7D7D,$2873692B73292E33,$716D2D78267D6F6E,$2D787D716C2D787D,$71206E2D787D716F
   Data.q $5066006D760D0206,$7339317D7D7D7D57,$692B73292E33323E,$2D78267D6F6E2873,$7D71682D787D7169
   Data.q $6A2D787D716B2D78,$6B6C760D02067120,$7D7D7D7D57506600,$57507D7D7D7D5750,$733F282E7D7D7D7D
   Data.q $3E7D6F6E28733E3E,$3F7D716A3C7D716A,$7D7D7D7D5750666A,$733E3E733E3F282E,$7D716B3E7D6F6E28
   Data.q $50666B3F7D716B3C,$3F282E7D7D7D7D57,$6F6E28733E3E733E,$71683C7D71683E7D,$7D7D575066683F7D
   Data.q $3E733E3F282E7D7D,$693E7D6F6E28733E,$693F7D71693C7D71,$2E7D7D7D7D575066,$28733E3E733E3F28
   Data.q $3C7D716E3E7D6F6E,$5750666E3F7D716E,$3E3F282E7D7D7D7D,$7D6F6E28733E3E73,$7D716F3C7D716F3E
   Data.q $7D7D7D5750666F3F,$3E3E733E3F282E7D,$716C3E7D6F6E2873,$666C3F7D716C3C7D,$282E7D7D7D7D5750
   Data.q $6E28733E3E733E3F,$6D3C7D716D3E7D6F,$7D5750666D3F7D71,$7D7D7D57507D7D7D,$6F6E3F732F32257D
   Data.q $712A322F2F323F7D,$712A322F2F323F7D,$662A322F2F323F7D,$282E7D7D7D7D5750,$7D7D6F6E28733E3F
   Data.q $322F2F323F7D7D7D,$5750666D716D712A,$7D7D7D57507D7D7D,$7D7D7D7D7D7D7D7D,$72727D7D7D7D5750
   Data.q $2F2F323F757D3B34,$7D7D7D5750742A32,$3435732D29382E7D,$3F712D7D6F6E2873,$666D712A322F2F32
   Data.q $7D7D7D7D7D7D5750,$727D7D7D7D57507D,$34062D7D29382E72,$323F757D3B347D00,$2F327D742A322F2F
   Data.q $2F383529327D6D7D,$7D7D5750382E342A,$28732D31382E7D7D,$78716D3C547D6F6E,$50662D716D716D2D
   Data.q $31382E7D7D7D7D57,$3C547D6F6E28732D,$716D716C2D78716C,$7D7D7D7D5750662D,$6F6E28732D31382E
   Data.q $6F2D78716F3C547D,$7D5750662D716D71,$732D31382E7D7D7D,$716E3C547D6F6E28,$662D716D716E2D78
   Data.q $382E7D7D7D7D5750,$547D6F6E28732D31,$6D71692D7871693C,$7D7D7D5750662D71,$6E28732D31382E7D
   Data.q $2D7871683C547D6F,$5750662D716D7168,$2D31382E7D7D7D7D,$6B3C547D6F6E2873,$2D716D716B2D7871
   Data.q $2E7D7D7D7D575066,$7D6F6E28732D3138,$716A2D78716A3C54,$7D7D5750662D716D,$7D7D7D7D57507D7D
   Data.q $28733E3E7339393C,$3E7D716A3E7D6F6E,$5750666A3C7D716A,$3E39393C7D7D7D7D,$7D6F6E28733E3E73
   Data.q $7D716B3E7D716B3E,$7D7D7D5750666B3C,$3E3E733E39393C7D,$71683E7D6F6E2873,$66683C7D71683E7D
   Data.q $393C7D7D7D7D5750,$6E28733E3E733E39,$693E7D71693E7D6F,$7D575066693C7D71,$733E39393C7D7D7D
   Data.q $3E7D6F6E28733E3E,$3C7D716E3E7D716E,$7D7D7D7D5750666E,$733E3E733E39393C,$7D716F3E7D6F6E28
   Data.q $50666F3C7D716F3E,$39393C7D7D7D7D57,$6F6E28733E3E733E,$716C3E7D716C3E7D,$7D7D5750666C3C7D
   Data.q $3E733E39393C7D7D,$6D3E7D6F6E28733E,$6D3C7D716D3E7D71,$2F7D7D7D7D575066,$5066343328732938
   Data.q $3B73575057502057,$382F73757D3E3328,$3E7D6F6E3F737D3A,$7D3A382F737D716D,$716C3E7D6F6E3F73
   Data.q $3F737D3A382F737D,$737D716F3E7D6F6E,$6F6E3F737D3A382F,$382F737D716E3E7D,$3E7D6F6E3F737D3A
   Data.q $7D3A382F737D7169,$71683E7D6F6E3F73,$3F737D3A382F737D,$737D716B3E7D6F6E,$6F6E3F737D3A382F
   Data.q $19191C7D746A3E7D,$2F73757D0D191210,$7D6F6E3F737D3A38,$3A382F737D716D3C,$6C3C7D6F6E3F737D
   Data.q $737D3A382F737D71,$7D716F3C7D6F6E3F,$6E3F737D3A382F73,$2F737D716E3C7D6F,$7D6F6E3F737D3A38
   Data.q $3A382F737D71693C,$683C7D6F6E3F737D,$737D3A382F737D71,$7D716B3C7D6F6E3F,$6E3F737D3A382F73
   Data.q $7D7D7D716A3C7D6F,$6E3F737D3A382F73,$2F737D716D3F7D6F,$7D6F6E3F737D3A38,$3A382F737D716C3F
   Data.q $6F3F7D6F6E3F737D,$737D3A382F737D71,$7D716E3F7D6F6E3F,$6E3F737D3A382F73,$2F737D71693F7D6F
   Data.q $7D6F6E3F737D3A38,$3A382F737D71683F,$6B3F7D6F6E3F737D,$737D3A382F737D71,$7D746A3F7D6F6E3F
   Data.q $7D7D5750267D5750,$737D3A382F737D7D,$2F2F3C3E7D6F6E3F,$7D7D7D7D57506624,$7D7D7D7D3A382F73
   Data.q $293A7D39382F2D73,$7D7D5750662D7D71,$737D3A382F737D7D,$2D30323E7D6B6C28,$6C2D30323E7D716D
   Data.q $666F2D30323E7D71,$2F737D7D7D7D5750,$7D6F6E3F737D3A38,$5750666365612D78,$7D7D57507D7D7D7D
   Data.q $33323E7339317D7D,$6E2873692B73292E,$7D716D2D78267D6F,$6F2D787D716C2D78,$0671206E2D787D71
   Data.q $575066006D760D02,$3E7339317D7D7D7D,$73692B73292E3332,$692D78267D6F6E28,$787D71682D787D71
   Data.q $206A2D787D716B2D,$006B6C760D020671,$507D7D7D7D575066,$7D57507D7D7D7D57,$3E7339393C7D7D7D
   Data.q $6A3E7D6F6E28733E,$6A3F7D716A3C7D71,$3C7D7D7D7D575066,$28733E3E733E3939,$3C7D716B3E7D6F6E
   Data.q $5750666B3F7D716B,$3E39393C7D7D7D7D,$7D6F6E28733E3E73,$7D71683C7D71683E,$7D7D7D575066683F
   Data.q $3E3E733E39393C7D,$71693E7D6F6E2873,$66693F7D71693C7D,$393C7D7D7D7D5750,$6E28733E3E733E39
   Data.q $6E3C7D716E3E7D6F,$7D5750666E3F7D71,$733E39393C7D7D7D,$3E7D6F6E28733E3E,$3F7D716F3C7D716F
   Data.q $7D7D7D7D5750666F,$733E3E733E39393C,$7D716C3E7D6F6E28,$50666C3F7D716C3C,$39393C7D7D7D7D57
   Data.q $6F6E28733E3E733E,$716D3C7D716D3E7D,$7D7D5750666D3F7D,$7D7D7D7D57507D7D,$7D6F6E3F732F3225
   Data.q $3E7D71242F2F3C3E,$3C3E7D71242F2F3C,$7D7D575066242F2F,$28733E39393C7D7D,$3E7D7D7D7D7D6F6E
   Data.q $6D716D71242F2F3C,$57507D7D7D575066,$732B32307D7D7D7D,$3A7D7D7D39382F2D,$7D5750666D7D7129
   Data.q $7D7D7D7D57507D7D,$7D6B6C28732B3230,$6D2D30323E547D7D,$5454666D6D256D71,$DD8C7D6D72725454
   Data.q $D68CE08DEF8DED8D,$393C7D7D7D7D5750,$7D7D7D6B6C287339,$716C2D30323E7D7D,$7D716D2D30323E7D
   Data.q $7D7D7D7D7D7D666C,$E18D7D6C72727D7D,$D58CD18CE08DE88D,$7D7D7D7D5750E88D,$7D6B6C287339393C
   Data.q $2D30323E7D7D7D7D,$6C2D30323E7D716F,$7D7D7D7D666C7D71,$7D6F72727D7D7D7D,$D18CE68DE38DEC8D
   Data.q $50545750E88DD58C,$3472727D7D7D7D57,$637D0034063E753B,$50740034060D027D,$7D57507D7D7D7D57
   Data.q $732D29382E7D7D7D,$2D7D6F6E28733435,$666D2D78716D3E71,$732D31382E545750,$30323E547D6B6C28
   Data.q $6F2D30323E716C2D,$2D716C2D30323E71,$2E7D7D7D7D575066,$28732C38732D2938,$716D3E712D7D6F6E
   Data.q $7D7D5750666D2D78,$28732D31382E7D7D,$2D30323E547D6B6C,$716D2D30323E716D,$662D716C2D30323E
   Data.q $57507D7D7D7D5750,$2D29382E7D7D7D7D,$7D6F6E2873343573,$6C2D78716C3E712D,$2D31382E54575066
   Data.q $323E547D6B6C2873,$2D30323E716C2D30,$716C2D30323E716F,$7D7D7D7D5750662D,$732C38732D29382E
   Data.q $6C3E712D7D6F6E28,$7D5750666C2D7871,$732D31382E7D7D7D,$30323E547D6B6C28,$6D2D30323E716D2D
   Data.q $2D716C2D30323E71,$507D7D7D7D575066,$29382E7D7D7D7D57,$6F6E28733435732D,$2D78716F3E712D7D
   Data.q $31382E545750666F,$3E547D6B6C28732D,$30323E716C2D3032,$6C2D30323E716F2D,$7D7D7D5750662D71
   Data.q $2C38732D29382E7D,$3E712D7D6F6E2873,$5750666F2D78716F,$2D31382E7D7D7D7D,$323E547D6B6C2873
   Data.q $2D30323E716D2D30,$716C2D30323E716D,$7D7D7D7D5750662D,$382E7D7D7D7D5750,$6E28733435732D29
   Data.q $78716E3E712D7D6F,$382E545750666E2D,$547D6B6C28732D31,$323E716C2D30323E,$2D30323E716F2D30
   Data.q $7D7D5750662D716C,$38732D29382E7D7D,$712D7D6F6E28732C,$50666E2D78716E3E,$31382E7D7D7D7D57
   Data.q $3E547D6B6C28732D,$30323E716D2D3032,$6C2D30323E716D2D,$7D7D7D5750662D71,$2E7D7D7D7D57507D
   Data.q $28733435732D2938,$71693E712D7D6F6E,$2E54575066692D78,$7D6B6C28732D3138,$3E716C2D30323E54
   Data.q $30323E716F2D3032,$7D5750662D716C2D,$732D29382E7D7D7D,$2D7D6F6E28732C38,$66692D7871693E71
   Data.q $382E7D7D7D7D5750,$547D6B6C28732D31,$323E716D2D30323E,$2D30323E716D2D30,$7D7D5750662D716C
   Data.q $7D7D7D7D57507D7D,$733435732D29382E,$683E712D7D6F6E28,$54575066682D7871,$6B6C28732D31382E
   Data.q $716C2D30323E547D,$323E716F2D30323E,$5750662D716C2D30,$2D29382E7D7D7D7D,$7D6F6E28732C3873
   Data.q $682D7871683E712D,$2E7D7D7D7D575066,$7D6B6C28732D3138,$3E716D2D30323E54,$30323E716D2D3032
   Data.q $7D5750662D716C2D,$7D7D7D57507D7D7D,$3435732D29382E7D,$3E712D7D6F6E2873,$5750666B2D78716B
   Data.q $6C28732D31382E54,$6C2D30323E547D6B,$3E716F2D30323E71,$50662D716C2D3032,$29382E7D7D7D7D57
   Data.q $6F6E28732C38732D,$2D78716B3E712D7D,$7D7D7D7D5750666B,$6B6C28732D31382E,$716D2D30323E547D
   Data.q $323E716D2D30323E,$5750662D716C2D30,$7D7D57507D7D7D7D,$35732D29382E7D7D,$712D7D6F6E287334
   Data.q $50666A2D78716A3E,$28732D31382E5457,$2D30323E547D6B6C,$716F2D30323E716C,$662D716C2D30323E
   Data.q $382E7D7D7D7D5750,$6E28732C38732D29,$78716A3E712D7D6F,$7D7D7D5750666A2D,$6C28732D31382E7D
   Data.q $6D2D30323E547D6B,$3E716D2D30323E71,$50662D716C2D3032,$7D57507D7D7D7D57,$3F732F352E7D7D7D
   Data.q $6D2D30323E7D6B6C,$6C716D2D30323E71,$507D7D7D7D575066,$3472727D7D7D7D57,$7D242F2F3C3E753B
   Data.q $575074293A7D2121,$2D29382E7D7D7D7D,$7D6B6C2873343573,$6D2D30323E71293A,$7D7D7D5750666D71
   Data.q $3435732D29382E7D,$7D6F6E28732F3273,$71242F2F3C3E712D,$575066293A7D716D,$7D7D57507D7D7D7D
   Data.q $7D29382E72727D7D,$7D3B347D0034062D,$217D242F2F3C3E75,$2F327D74293A7D21,$2F383529327D6D7D
   Data.q $7D7D5750382E342A,$28732D31382E7D7D,$78716D3C547D6F6E,$50662D716D716D2D,$31382E7D7D7D7D57
   Data.q $3C547D6F6E28732D,$716D716C2D78716C,$7D7D7D7D5750662D,$6F6E28732D31382E,$6F2D78716F3C547D
   Data.q $7D5750662D716D71,$732D31382E7D7D7D,$716E3C547D6F6E28,$662D716D716E2D78,$382E7D7D7D7D5750
   Data.q $547D6F6E28732D31,$6D71692D7871693C,$7D7D7D5750662D71,$6E28732D31382E7D,$2D7871683C547D6F
   Data.q $5750662D716D7168,$2D31382E7D7D7D7D,$6B3C547D6F6E2873,$2D716D716B2D7871,$2E7D7D7D7D575066
   Data.q $7D6F6E28732D3138,$716A2D78716A3C54,$7D7D5750662D716D,$7D7D7D7D57507D7D,$28733E3E733F282E
   Data.q $3E7D716A3E7D6F6E,$5750666A3C7D716A,$3E3F282E7D7D7D7D,$7D6F6E28733E3E73,$7D716B3E7D716B3E
   Data.q $7D7D7D5750666B3C,$3E3E733E3F282E7D,$71683E7D6F6E2873,$66683C7D71683E7D,$282E7D7D7D7D5750
   Data.q $6E28733E3E733E3F,$693E7D71693E7D6F,$7D575066693C7D71,$733E3F282E7D7D7D,$3E7D6F6E28733E3E
   Data.q $3C7D716E3E7D716E,$7D7D7D7D5750666E,$733E3E733E3F282E,$7D716F3E7D6F6E28,$50666F3C7D716F3E
   Data.q $3F282E7D7D7D7D57,$6F6E28733E3E733E,$716C3E7D716C3E7D,$7D7D5750666C3C7D,$3E733E3F282E7D7D
   Data.q $6D3E7D6F6E28733E,$6D3C7D716D3E7D71,$2F7D7D7D7D575066,$5066343328732938,$3B73575057502057
   Data.q $382F73757D3E3328,$3E7D6F6E3F737D3A,$7D3A382F737D716D,$716C3E7D6F6E3F73,$3F737D3A382F737D
   Data.q $737D716F3E7D6F6E,$6F6E3F737D3A382F,$382F737D716E3E7D,$3E7D6F6E3F737D3A,$7D3A382F737D7169
   Data.q $71683E7D6F6E3F73,$3F737D3A382F737D,$737D716B3E7D6F6E,$6F6E3F737D3A382F,$1108107D746A3E7D
   Data.q $2F73757D0D191210,$7D6F6E3F737D3A38,$3A382F737D716D3C,$6C3C7D6F6E3F737D,$737D3A382F737D71
   Data.q $7D716F3C7D6F6E3F,$6E3F737D3A382F73,$2F737D716E3C7D6F,$7D6F6E3F737D3A38,$3A382F737D71693C
   Data.q $683C7D6F6E3F737D,$737D3A382F737D71,$7D716B3C7D6F6E3F,$6E3F737D3A382F73,$7D7D7D716A3C7D6F
   Data.q $6E3F737D3A382F73,$2F737D716D3F7D6F,$7D6F6E3F737D3A38,$3A382F737D716C3F,$6F3F7D6F6E3F737D
   Data.q $737D3A382F737D71,$7D716E3F7D6F6E3F,$6E3F737D3A382F73,$2F737D71693F7D6F,$7D6F6E3F737D3A38
   Data.q $3A382F737D71683F,$6B3F7D6F6E3F737D,$737D3A382F737D71,$7D746A3F7D6F6E3F,$7D7D5750267D5750
   Data.q $737D3A382F737D7D,$2F2F3C3E7D6F6E3F,$7D7D7D7D57506624,$7D7D7D7D3A382F73,$293A7D39382F2D73
   Data.q $382B327D712D7D71,$5750662A32313B2F,$3A382F737D7D7D7D,$323E7D6B6C28737D,$30323E7D716D2D30
   Data.q $2D30323E7D716C2D,$7D7D7D7D5750666F,$6E28737D3A382F73,$716365612D787D6F,$636561353A343578
   Data.q $7D57506629787D71,$7D3A382F737D7D7D,$3A34357D6F6E2873,$7D716A2D30382935,$2D303829353A3435
   Data.q $7D5750662E7D716B,$7D3A382F737D7D7D,$323F7D7D6F6E2873,$7D5750662A322F2F,$7D7D7D57507D7D7D
   Data.q $78757D31313C3E7D,$787D716D353A3435,$787D716C353A3435,$787D716F353A3435,$787D716E353A3435
   Data.q $787D7169353A3435,$787D7168353A3435,$787D716B353A3435,$7D71746A353A3435,$66747571120F1807
   Data.q $7D7D7D7D57507D7D,$242E732F3C3F7272,$7D5750666D7D3E33,$323E7339317D7D7D,$2873692B73292E33
   Data.q $716D2D78267D6F6E,$2D787D716C2D787D,$71206E2D787D716F,$5066006D760D0206,$7339317D7D7D7D57
   Data.q $692B73292E33323E,$2D78267D6F6E2873,$7D71682D787D7169,$6A2D787D716B2D78,$6B6C760D02067120
   Data.q $57507D7D7D7D6600,$7D7D57507D7D7D7D,$6E28732B32307D7D,$7D7129787D7D7D6F,$7D7D7D5750666A3C
   Data.q $727D7D7D7D57507D,$777D006A063C7D72,$742A3231757D3F7D,$28307D7D7D7D5750,$6F6E287332317331
   Data.q $3F712978716A3E7D,$7D7D7D7D5750666A,$2873323173312830,$2978716B3E7D6F6E,$7D7D5750666B3F71
   Data.q $3231733128307D7D,$71683E7D6F6E2873,$575066683F712978,$733128307D7D7D7D,$3E7D6F6E28733231
   Data.q $66693F7129787169,$28307D7D7D7D5750,$6F6E287332317331,$3F712978716E3E7D,$7D7D7D7D5750666E
   Data.q $2873323173312830,$2978716F3E7D6F6E,$7D7D5750666F3F71,$3231733128307D7D,$716C3E7D6F6E2873
   Data.q $5750666C3F712978,$733128307D7D7D7D,$3E7D6F6E28733231,$666D3F712978716D,$57507D7D7D7D5750
   Data.q $3C7D72727D7D7D7D,$7D3F7D777D006A06,$575074353A343575,$73393C307D7D7D7D,$6E28733E3E733435
   Data.q $712978716B3E7D6F,$5750666B3E716A3F,$3E393C307D7D7D7D,$28733E3E73343573,$297871683E7D6F6E
   Data.q $5066683E716B3F71,$393C307D7D7D7D57,$733E3E733435733E,$7871693E7D6F6E28,$66693E71683F7129
   Data.q $3C307D7D7D7D5750,$3E3E733435733E39,$716E3E7D6F6E2873,$6E3E71693F712978,$307D7D7D7D575066
   Data.q $3E733435733E393C,$6F3E7D6F6E28733E,$3E716E3F71297871,$7D7D7D7D5750666F,$733435733E393C30
   Data.q $3E7D6F6E28733E3E,$716F3F712978716C,$7D7D7D5750666C3E,$3435733E393C307D,$7D6F6E28733E3E73
   Data.q $6C3F712978716D3E,$7D7D5750666D3E71,$35733E393C307D7D,$35787D6F6E287334,$712978716A353A34
   Data.q $353A343578716D3F,$7D7D7D7D5750666A,$72727D7D7D7D5750,$7D777D006B063C7D,$50742A3231757D3F
   Data.q $2B32307D7D7D7D57,$787D7D7D6F6E2873,$5750666B3C7D7129,$73393C307D7D7D7D,$6E28733E3E733231
   Data.q $712978716B3E7D6F,$5750666B3E716A3F,$3E393C307D7D7D7D,$28733E3E73323173,$297871683E7D6F6E
   Data.q $5066683E716B3F71,$393C307D7D7D7D57,$733E3E733231733E,$7871693E7D6F6E28,$66693E71683F7129
   Data.q $3C307D7D7D7D5750,$3E3E733231733E39,$716E3E7D6F6E2873,$6E3E71693F712978,$307D7D7D7D575066
   Data.q $3E733231733E393C,$6F3E7D6F6E28733E,$3E716E3F71297871,$7D7D7D7D5750666F,$733231733E393C30
   Data.q $3E7D6F6E28733E3E,$716F3F712978716C,$7D7D7D5750666C3E,$3231733E393C307D,$7D6F6E28733E3E73
   Data.q $6C3F712978716D3E,$7D7D5750666D3E71,$31733E393C307D7D,$6F6E28733E3E7332,$716A353A3435787D
   Data.q $3578716D3F712978,$7D7D7D666A353A34,$3C7D7D7D7D57507D,$7D6F6E28733E3939,$78716B353A343578
   Data.q $666D716B353A3435,$57507D7D7D7D5750,$3C7D72727D7D7D7D,$7D3F7D777D006B06,$575074353A343575
   Data.q $73393C307D7D7D7D,$6E28733E3E733435,$71297871683E7D6F,$575066683E716A3F,$3E393C307D7D7D7D
   Data.q $28733E3E73343573,$297871693E7D6F6E,$5066693E716B3F71,$393C307D7D7D7D57,$733E3E733435733E
   Data.q $78716E3E7D6F6E28,$666E3E71683F7129,$3C307D7D7D7D5750,$3E3E733435733E39,$716F3E7D6F6E2873
   Data.q $6F3E71693F712978,$307D7D7D7D575066,$3E733435733E393C,$6C3E7D6F6E28733E,$3E716E3F71297871
   Data.q $7D7D7D7D5750666C,$733435733E393C30,$3E7D6F6E28733E3E,$716F3F712978716D,$7D7D7D5750666D3E
   Data.q $3435733E393C307D,$7D6F6E28733E3E73,$78716A353A343578,$343578716C3F7129,$7D7D5750666A353A
   Data.q $35733E393C307D7D,$35787D6F6E287334,$712978716B353A34,$353A343578716D3F,$7D7D7D7D5750666B
   Data.q $72727D7D7D7D5750,$7D777D0068063C7D,$50742A3231757D3F,$2B32307D7D7D7D57,$787D7D7D6F6E2873
   Data.q $575066683C7D7129,$73393C307D7D7D7D,$6E28733E3E733231,$71297871683E7D6F,$575066683E716A3F
   Data.q $3E393C307D7D7D7D,$28733E3E73323173,$297871693E7D6F6E,$5066693E716B3F71,$393C307D7D7D7D57
   Data.q $733E3E733231733E,$78716E3E7D6F6E28,$666E3E71683F7129,$3C307D7D7D7D5750,$3E3E733231733E39
   Data.q $716F3E7D6F6E2873,$6F3E71693F712978,$307D7D7D7D575066,$3E733231733E393C,$6C3E7D6F6E28733E
   Data.q $3E716E3F71297871,$7D7D7D7D5750666C,$733231733E393C30,$3E7D6F6E28733E3E,$716F3F712978716D
   Data.q $7D7D7D5750666D3E,$3231733E393C307D,$7D6F6E28733E3E73,$78716A353A343578,$343578716C3F7129
   Data.q $7D7D5750666A353A,$31733E393C307D7D,$6F6E28733E3E7332,$716B353A3435787D,$3578716D3F712978
   Data.q $7D5750666B353A34,$733E39393C7D7D7D,$3A3435787D6F6E28,$353A343578716835,$7D7D5750666D7168
   Data.q $7D7D7D7D57507D7D,$7D0068063C7D7272,$3A3435757D3F7D77,$7D7D7D7D57507435,$3E73343573393C30
   Data.q $693E7D6F6E28733E,$3E716A3F71297871,$7D7D7D7D57506669,$733435733E393C30,$3E7D6F6E28733E3E
   Data.q $716B3F712978716E,$7D7D7D5750666E3E,$3435733E393C307D,$7D6F6E28733E3E73,$683F712978716F3E
   Data.q $7D7D5750666F3E71,$35733E393C307D7D,$6F6E28733E3E7334,$3F712978716C3E7D,$7D5750666C3E7169
   Data.q $733E393C307D7D7D,$6E28733E3E733435,$712978716D3E7D6F,$5750666D3E716E3F,$3E393C307D7D7D7D
   Data.q $28733E3E73343573,$353A3435787D6F6E,$716F3F712978716A,$50666A353A343578,$393C307D7D7D7D57
   Data.q $733E3E733435733E,$3A3435787D6F6E28,$6C3F712978716B35,$666B353A34357871,$3C307D7D7D7D5750
   Data.q $6E28733435733E39,$68353A3435787D6F,$78716D3F71297871,$57506668353A3435,$7D7D57507D7D7D7D
   Data.q $69063C7D72727D7D,$31757D3F7D777D00,$7D7D7D5750742A32,$6F6E28732B32307D,$3C7D7129787D7D7D
   Data.q $7D7D7D7D57506669,$3E73323173393C30,$693E7D6F6E28733E,$3E716A3F71297871,$7D7D7D7D57506669
   Data.q $733231733E393C30,$3E7D6F6E28733E3E,$716B3F712978716E,$7D7D7D5750666E3E,$3231733E393C307D
   Data.q $7D6F6E28733E3E73,$683F712978716F3E,$7D7D5750666F3E71,$31733E393C307D7D,$6F6E28733E3E7332
   Data.q $3F712978716C3E7D,$7D5750666C3E7169,$733E393C307D7D7D,$6E28733E3E733231,$712978716D3E7D6F
   Data.q $5750666D3E716E3F,$3E393C307D7D7D7D,$28733E3E73323173,$353A3435787D6F6E,$716F3F712978716A
   Data.q $50666A353A343578,$393C307D7D7D7D57,$733E3E733231733E,$3A3435787D6F6E28,$6C3F712978716B35
   Data.q $666B353A34357871,$3C307D7D7D7D5750,$3E3E733231733E39,$3435787D6F6E2873,$3F7129787168353A
   Data.q $68353A343578716D,$3C7D7D7D7D575066,$7D6F6E28733E3939,$787169353A343578,$666D7169353A3435
   Data.q $57507D7D7D7D5750,$3C7D72727D7D7D7D,$7D3F7D777D006906,$575074353A343575,$73393C307D7D7D7D
   Data.q $6E28733E3E733435,$712978716E3E7D6F,$5750666E3E716A3F,$3E393C307D7D7D7D,$28733E3E73343573
   Data.q $2978716F3E7D6F6E,$50666F3E716B3F71,$393C307D7D7D7D57,$733E3E733435733E,$78716C3E7D6F6E28
   Data.q $666C3E71683F7129,$3C307D7D7D7D5750,$3E3E733435733E39,$716D3E7D6F6E2873,$6D3E71693F712978
   Data.q $307D7D7D7D575066,$3E733435733E393C,$35787D6F6E28733E,$712978716A353A34,$353A343578716E3F
   Data.q $7D7D7D7D5750666A,$733435733E393C30,$787D6F6E28733E3E,$2978716B353A3435,$3A343578716F3F71
   Data.q $7D7D7D5750666B35,$3435733E393C307D,$7D6F6E28733E3E73,$787168353A343578,$343578716C3F7129
   Data.q $7D7D57506668353A,$35733E393C307D7D,$35787D6F6E287334,$7129787169353A34,$353A343578716D3F
   Data.q $7D7D7D7D57506669,$72727D7D7D7D5750,$7D777D006E063C7D,$50742A3231757D3F,$2B32307D7D7D7D57
   Data.q $787D7D7D6F6E2873,$5750666E3C7D7129,$73393C307D7D7D7D,$6E28733E3E733231,$712978716E3E7D6F
   Data.q $5750666E3E716A3F,$3E393C307D7D7D7D,$28733E3E73323173,$2978716F3E7D6F6E,$50666F3E716B3F71
   Data.q $393C307D7D7D7D57,$733E3E733231733E,$78716C3E7D6F6E28,$666C3E71683F7129,$3C307D7D7D7D5750
   Data.q $3E3E733231733E39,$716D3E7D6F6E2873,$6D3E71693F712978,$307D7D7D7D575066,$3E733231733E393C
   Data.q $35787D6F6E28733E,$712978716A353A34,$353A343578716E3F,$7D7D7D7D5750666A,$733231733E393C30
   Data.q $787D6F6E28733E3E,$2978716B353A3435,$3A343578716F3F71,$7D7D7D5750666B35,$3231733E393C307D
   Data.q $7D6F6E28733E3E73,$787168353A343578,$343578716C3F7129,$7D7D57506668353A,$31733E393C307D7D
   Data.q $6F6E28733E3E7332,$7169353A3435787D,$3578716D3F712978,$7D57506669353A34,$733E39393C7D7D7D
   Data.q $3A3435787D6F6E28,$353A343578716E35,$7D7D5750666D716E,$727D7D7D7D57507D,$777D006E063C7D72
   Data.q $353A3435757D3F7D,$307D7D7D7D575074,$3E3E73343573393C,$716F3E7D6F6E2873,$6F3E716A3F712978
   Data.q $307D7D7D7D575066,$3E733435733E393C,$6C3E7D6F6E28733E,$3E716B3F71297871,$7D7D7D7D5750666C
   Data.q $733435733E393C30,$3E7D6F6E28733E3E,$71683F712978716D,$7D7D7D5750666D3E,$3435733E393C307D
   Data.q $7D6F6E28733E3E73,$78716A353A343578,$34357871693F7129,$7D7D5750666A353A,$35733E393C307D7D
   Data.q $6F6E28733E3E7334,$716B353A3435787D,$3578716E3F712978,$7D5750666B353A34,$733E393C307D7D7D
   Data.q $6E28733E3E733435,$68353A3435787D6F,$78716F3F71297871,$57506668353A3435,$3E393C307D7D7D7D
   Data.q $28733E3E73343573,$353A3435787D6F6E,$716C3F7129787169,$506669353A343578,$393C307D7D7D7D57
   Data.q $6F6E28733435733E,$716E353A3435787D,$3578716D3F712978,$7D5750666E353A34,$7D7D7D57507D7D7D
   Data.q $006F063C7D72727D,$3231757D3F7D777D,$7D7D7D7D5750742A,$7D6F6E28732B3230,$6F3C7D7129787D7D
   Data.q $307D7D7D7D575066,$3E3E73323173393C,$716F3E7D6F6E2873,$6F3E716A3F712978,$307D7D7D7D575066
   Data.q $3E733231733E393C,$6C3E7D6F6E28733E,$3E716B3F71297871,$7D7D7D7D5750666C,$733231733E393C30
   Data.q $3E7D6F6E28733E3E,$71683F712978716D,$7D7D7D5750666D3E,$3231733E393C307D,$7D6F6E28733E3E73
   Data.q $78716A353A343578,$34357871693F7129,$7D7D5750666A353A,$31733E393C307D7D,$6F6E28733E3E7332
   Data.q $716B353A3435787D,$3578716E3F712978,$7D5750666B353A34,$733E393C307D7D7D,$6E28733E3E733231
   Data.q $68353A3435787D6F,$78716F3F71297871,$57506668353A3435,$3E393C307D7D7D7D,$28733E3E73323173
   Data.q $353A3435787D6F6E,$716C3F7129787169,$506669353A343578,$393C307D7D7D7D57,$733E3E733231733E
   Data.q $3A3435787D6F6E28,$6D3F712978716E35,$666E353A34357871,$393C7D7D7D7D5750,$787D6F6E28733E39
   Data.q $3578716F353A3435,$50666D716F353A34,$7D57507D7D7D7D57,$063C7D72727D7D7D,$757D3F7D777D006F
   Data.q $7D575074353A3435,$3573393C307D7D7D,$6F6E28733E3E7334,$3F712978716C3E7D,$7D5750666C3E716A
   Data.q $733E393C307D7D7D,$6E28733E3E733435,$712978716D3E7D6F,$5750666D3E716B3F,$3E393C307D7D7D7D
   Data.q $28733E3E73343573,$353A3435787D6F6E,$71683F712978716A,$50666A353A343578,$393C307D7D7D7D57
   Data.q $733E3E733435733E,$3A3435787D6F6E28,$693F712978716B35,$666B353A34357871,$3C307D7D7D7D5750
   Data.q $3E3E733435733E39,$3435787D6F6E2873,$3F7129787168353A,$68353A343578716E,$307D7D7D7D575066
   Data.q $3E733435733E393C,$35787D6F6E28733E,$7129787169353A34,$353A343578716F3F,$7D7D7D7D57506669
   Data.q $733435733E393C30,$787D6F6E28733E3E,$2978716E353A3435,$3A343578716C3F71,$7D7D7D5750666E35
   Data.q $3435733E393C307D,$3435787D6F6E2873,$3F712978716F353A,$6F353A343578716D,$507D7D7D7D575066
   Data.q $7D72727D7D7D7D57,$3F7D777D006C063C,$5750742A3231757D,$732B32307D7D7D7D,$29787D7D7D6F6E28
   Data.q $7D5750666C3C7D71,$3173393C307D7D7D,$6F6E28733E3E7332,$3F712978716C3E7D,$7D5750666C3E716A
   Data.q $733E393C307D7D7D,$6E28733E3E733231,$712978716D3E7D6F,$5750666D3E716B3F,$3E393C307D7D7D7D
   Data.q $28733E3E73323173,$353A3435787D6F6E,$71683F712978716A,$50666A353A343578,$393C307D7D7D7D57
   Data.q $733E3E733231733E,$3A3435787D6F6E28,$693F712978716B35,$666B353A34357871,$3C307D7D7D7D5750
   Data.q $3E3E733231733E39,$3435787D6F6E2873,$3F7129787168353A,$68353A343578716E,$307D7D7D7D575066
   Data.q $3E733231733E393C,$35787D6F6E28733E,$7129787169353A34,$353A343578716F3F,$7D7D7D7D57506669
   Data.q $733231733E393C30,$787D6F6E28733E3E,$2978716E353A3435,$3A343578716C3F71,$7D7D7D5750666E35
   Data.q $3231733E393C307D,$7D6F6E28733E3E73,$78716F353A343578,$343578716D3F7129,$7D7D5750666F353A
   Data.q $28733E39393C7D7D,$353A3435787D6F6E,$6C353A343578716C,$7D7D7D5750666D71,$727D7D7D7D57507D
   Data.q $777D006C063C7D72,$353A3435757D3F7D,$307D7D7D7D575074,$3E3E73343573393C,$716D3E7D6F6E2873
   Data.q $6D3E716A3F712978,$307D7D7D7D575066,$3E733435733E393C,$35787D6F6E28733E,$712978716A353A34
   Data.q $353A343578716B3F,$7D7D7D7D5750666A,$733435733E393C30,$787D6F6E28733E3E,$2978716B353A3435
   Data.q $3A34357871683F71,$7D7D7D5750666B35,$3435733E393C307D,$7D6F6E28733E3E73,$787168353A343578
   Data.q $34357871693F7129,$7D7D57506668353A,$35733E393C307D7D,$6F6E28733E3E7334,$7169353A3435787D
   Data.q $3578716E3F712978,$7D57506669353A34,$733E393C307D7D7D,$6E28733E3E733435,$6E353A3435787D6F
   Data.q $78716F3F71297871,$5750666E353A3435,$3E393C307D7D7D7D,$28733E3E73343573,$353A3435787D6F6E
   Data.q $716C3F712978716F,$50666F353A343578,$393C307D7D7D7D57,$6F6E28733435733E,$716C353A3435787D
   Data.q $3578716D3F712978,$7D5750666C353A34,$7D7D7D57507D7D7D,$006D063C7D72727D,$3231757D3F7D777D
   Data.q $7D7D7D7D5750742A,$7D6F6E28732B3230,$6D3C7D7129787D7D,$307D7D7D7D575066,$3E3E73323173393C
   Data.q $716D3E7D6F6E2873,$6D3E716A3F712978,$307D7D7D7D575066,$3E733231733E393C,$35787D6F6E28733E
   Data.q $712978716A353A34,$353A343578716B3F,$7D7D7D7D5750666A,$733231733E393C30,$787D6F6E28733E3E
   Data.q $2978716B353A3435,$3A34357871683F71,$7D7D7D5750666B35,$3231733E393C307D,$7D6F6E28733E3E73
   Data.q $787168353A343578,$34357871693F7129,$7D7D57506668353A,$31733E393C307D7D,$6F6E28733E3E7332
   Data.q $7169353A3435787D,$3578716E3F712978,$7D57506669353A34,$733E393C307D7D7D,$6E28733E3E733231
   Data.q $6E353A3435787D6F,$78716F3F71297871,$5750666E353A3435,$3E393C307D7D7D7D,$28733E3E73323173
   Data.q $353A3435787D6F6E,$716C3F712978716F,$50666F353A343578,$393C307D7D7D7D57,$733E3E733231733E
   Data.q $3A3435787D6F6E28,$6D3F712978716C35,$666C353A34357871,$393C7D7D7D7D5750,$787D6F6E28733E39
   Data.q $3578716D353A3435,$50666D716D353A34,$7D57507D7D7D7D57,$063C7D72727D7D7D,$757D3F7D777D006D
   Data.q $7D575074353A3435,$3573393C307D7D7D,$6F6E28733E3E7334,$716A353A3435787D,$3578716A3F712978
   Data.q $7D5750666A353A34,$733E393C307D7D7D,$6E28733E3E733435,$6B353A3435787D6F,$78716B3F71297871
   Data.q $5750666B353A3435,$3E393C307D7D7D7D,$28733E3E73343573,$353A3435787D6F6E,$71683F7129787168
   Data.q $506668353A343578,$393C307D7D7D7D57,$733E3E733435733E,$3A3435787D6F6E28,$693F712978716935
   Data.q $6669353A34357871,$3C307D7D7D7D5750,$3E3E733435733E39,$3435787D6F6E2873,$3F712978716E353A
   Data.q $6E353A343578716E,$307D7D7D7D575066,$3E733435733E393C,$35787D6F6E28733E,$712978716F353A34
   Data.q $353A343578716F3F,$7D7D7D7D5750666F,$733435733E393C30,$787D6F6E28733E3E,$2978716C353A3435
   Data.q $3A343578716C3F71,$7D7D7D5750666C35,$3435733E393C307D,$3435787D6F6E2873,$3F712978716D353A
   Data.q $6D353A343578716D,$507D7D7D7D575066,$7D72727D7D7D7D57,$7D2E3435297D291C,$382A7D293334322D
   Data.q $6B6C7D382B3C357D,$7D29343F706F6E7D,$382F7D2E392F322A,$342933382E382F2D,$6F6C687D3C7D3A33
   Data.q $313C2B7D29343F70,$7D72725457503828,$737D6D06353A3435,$333C7D006A7D7373,$73737D6D063E7D39
   Data.q $7D7D5750006A7D73,$7D7D7D7D57507D7D,$7D6F6E28732B3230,$6A6A647D712E7D7D,$507D7D7D7D575066
   Data.q $7D72727D7D7D7D57,$34357D382F32290E,$333C7D006B06353A,$6A06353A34357D39,$7D383E33342E7D00
   Data.q $31342A7D24383529,$382B327D383F7D31,$33382929342F2A2F,$32307D7D7D7D5750,$7D7D7D6F6E28732B
   Data.q $2D303829353A3435,$353A3435787D716A,$7D7D7D7D5750666A,$7D6F6E28732B3230,$3829353A34357D7D
   Data.q $3435787D716B2D30,$7D7D5750666B353A,$727D7D7D7D57507D,$357D38363C097D72,$7D6B686F7D353A34
   Data.q $28307D712E29343F,$3F7D24312D342931,$7D716F6E036F7D24,$317D32297D39393C,$3F7D6B686F7D2A32
   Data.q $7D7D7D57502E2934,$293C35097D72727D,$363C297D712E347D,$6D06353A34357D38,$71006A7D7373737D
   Data.q $347D293B34352E7D,$6C7D293B38317D29,$333C7D392F322A7D,$29347D39393C7D39,$7D6D063E7D32297D
   Data.q $7D7D006A7D737373,$7D7D7D7D57507D7D,$28733E3E7339393C,$78716B3E7D7D6F6E,$6B3E716A353A3435
   Data.q $3C7D7D7D7D575066,$28733E3E733E3939,$357871683E7D6F6E,$66683E716B353A34,$393C7D7D7D7D5750
   Data.q $6E28733E3E733E39,$34357871693E7D6F,$5066693E7168353A,$39393C7D7D7D7D57,$6F6E28733E3E733E
   Data.q $3A343578716E3E7D,$5750666E3E716935,$3E39393C7D7D7D7D,$7D6F6E28733E3E73,$353A343578716F3E
   Data.q $7D5750666F3E716E,$733E39393C7D7D7D,$3E7D6F6E28733E3E,$6F353A343578716C,$7D7D5750666C3E71
   Data.q $3E733E39393C7D7D,$6D3E7D6F6E28733E,$716C353A34357871,$7D7D7D5750666D3E,$3E3E733E39393C7D
   Data.q $3435787D6F6E2873,$3A343578716A353A,$7D5750666D716D35,$733E39393C7D7D7D,$3A3435787D6F6E28
   Data.q $50666D716D716B35,$7D57507D7D7D7D57,$3C097D72727D7D7D,$7D353A34357D3836,$2E29343F7D6B686F
   Data.q $2D34293128307D71,$6A647D243F7D2431,$297D39393C7D716A,$686F7D2A32317D32,$57502E29343F7D6B
   Data.q $293C35097D727254,$363C297D712E347D,$6D06353A34357D38,$7100687D7373737D,$7D716B353A34357D
   Data.q $307D716A353A3435,$7D24312D34293128,$3C7D6A6A647D243F,$297D39393C7D3933,$73737D6D063E7D32
   Data.q $7D7D5750006A7D73,$323173393C307D7D,$7D6F6E28733E3E73,$29353A3435716A3E,$3E712E716A2D3038
   Data.q $7D7D7D7D5750666A,$733231733E393C30,$3E7D6F6E28733E3E,$3829353A3435716B,$6B3E712E716B2D30
   Data.q $307D7D7D7D575066,$3E733231733E393C,$683E7D6F6E28733E,$7168353A34357871,$7D575066683E712E
   Data.q $733E393C307D7D7D,$6E28733E3E733231,$34357871693E7D6F,$693E712E7169353A,$307D7D7D7D575066
   Data.q $3E733231733E393C,$6E3E7D6F6E28733E,$716E353A34357871,$7D5750666E3E712E,$733E393C307D7D7D
   Data.q $6E28733E3E733231,$343578716F3E7D6F,$6F3E712E716F353A,$307D7D7D7D575066,$3E733231733E393C
   Data.q $6C3E7D6F6E28733E,$716C353A34357871,$7D5750666C3E712E,$733E393C307D7D7D,$6E28733E3E733231
   Data.q $343578716D3E7D6F,$6D3E712E716D353A,$3C7D7D7D7D575066,$28733E3E733E3939,$353A3435787D6F6E
   Data.q $6A353A343578716A,$7D7D7D5750666D71,$6E28733E39393C7D,$6B353A3435787D6F,$716B353A34357871
   Data.q $7D7D7D7D5750666D,$3C307D7D7D7D5750,$733E3E7334357339,$35716B3E7D6F6E28,$6A2D303829353A34
   Data.q $5750666B3E712E71,$3E393C307D7D7D7D,$28733E3E73343573,$343571683E7D6F6E,$716B2D303829353A
   Data.q $7D575066683E712E,$733E393C307D7D7D,$6E28733E3E733435,$34357871693E7D6F,$693E712E7168353A
   Data.q $307D7D7D7D575066,$3E733435733E393C,$6E3E7D6F6E28733E,$7169353A34357871,$7D5750666E3E712E
   Data.q $733E393C307D7D7D,$6E28733E3E733435,$343578716F3E7D6F,$6F3E712E716E353A,$307D7D7D7D575066
   Data.q $3E733435733E393C,$6C3E7D6F6E28733E,$716F353A34357871,$7D5750666C3E712E,$733E393C307D7D7D
   Data.q $6E28733E3E733435,$343578716D3E7D6F,$6D3E712E716C353A,$307D7D7D7D575066,$3E733435733E393C
   Data.q $35787D6F6E28733E,$343578716A353A34,$3578712E716D353A,$7D5750666A353A34,$733E39393C7D7D7D
   Data.q $3A3435787D6F6E28,$353A343578716B35,$7D7D5750666D716B,$7D7D7D7D57507D7D,$3C382D380F7D7272
   Data.q $3C2E7D3835297D29,$2E2D38292E7D3830,$35297D29283F7D71,$7D383034297D2E34,$7D243133327D382A
   Data.q $7D32297D39383833,$357D383139333C35,$3C7D006B06353A34,$06353A34357D3933,$7D7D7D7D5750006A
   Data.q $7D6F6E28732B3230,$3829353A34357D7D,$3435787D716A2D30,$7D7D5750666A353A,$6E28732B32307D7D
   Data.q $353A34357D7D7D6F,$787D716B2D303829,$5750666B353A3435,$7D7D57507D7D7D7D,$363C097D72727D7D
   Data.q $34357D3835297D38,$343F7D696B7D353A,$293128307D712E29,$7D243F7D24312D34,$39333C7D6F6E036F
   Data.q $7D32297D39393C7D,$7D2A32317D383529,$2E29343F7D6B686F,$393C7D7D7D7D5750,$6F6E28733E3E7339
   Data.q $343578716B3E7D7D,$50666B3E716A353A,$39393C7D7D7D7D57,$6F6E28733E3E733E,$3A34357871683E7D
   Data.q $575066683E716B35,$3E39393C7D7D7D7D,$7D6F6E28733E3E73,$666D71693E71693E,$393C7D7D7D7D5750
   Data.q $6E28733E3E733E39,$716E3E716E3E7D6F,$7D7D7D7D5750666D,$733E3E733E39393C,$3E716F3E7D6F6E28
   Data.q $7D7D5750666D716F,$3E733E39393C7D7D,$6C3E7D6F6E28733E,$5750666D716C3E71,$3E39393C7D7D7D7D
   Data.q $7D6F6E28733E3E73,$666D716D3E716D3E,$393C7D7D7D7D5750,$787D6F6E28733E39,$716D716A353A3435
   Data.q $7D7D7D7D5750666D,$72727D7D7D7D5750,$35297D38363C097D,$6B7D353A34357D38,$7D712E29343F7D69
   Data.q $24312D3429312830,$7D6A6A647D243F7D,$7D39393C7D39333C,$317D3835297D3229,$3F7D6B686F7D2A32
   Data.q $7D7D7D57502E2934,$73323173393C307D,$3E7D6F6E28733E3E,$3829353A3435716A,$6A3E712E716A2D30
   Data.q $307D7D7D7D575066,$3E733231733E393C,$6B3E7D6F6E28733E,$303829353A343571,$666B3E712E716B2D
   Data.q $393C7D7D7D7D5750,$6E28733E3E733E39,$71683E71683E7D6F,$7D7D7D7D5750666D,$733E3E733E39393C
   Data.q $3E71693E7D6F6E28,$7D7D5750666D7169,$3E733E39393C7D7D,$6E3E7D6F6E28733E,$5750666D716E3E71
   Data.q $3E39393C7D7D7D7D,$7D6F6E28733E3E73,$666D716F3E716F3E,$393C7D7D7D7D5750,$6E28733E3E733E39
   Data.q $716C3E716C3E7D6F,$7D7D7D7D5750666D,$733E3E733E39393C,$3E716D3E7D6F6E28,$7D7D5750666D716D
   Data.q $28733E39393C7D7D,$353A3435787D6F6E,$6A353A343578716A,$7D7D7D5750666D71,$307D7D7D7D57507D
   Data.q $3E3E73343573393C,$716B3E7D6F6E2873,$2D303829353A3435,$50666B3E712E716A,$393C307D7D7D7D57
   Data.q $733E3E733435733E,$3571683E7D6F6E28,$6B2D303829353A34,$575066683E712E71,$3E39393C7D7D7D7D
   Data.q $7D6F6E28733E3E73,$666D71693E71693E,$393C7D7D7D7D5750,$6E28733E3E733E39,$716E3E716E3E7D6F
   Data.q $7D7D7D7D5750666D,$733E3E733E39393C,$3E716F3E7D6F6E28,$7D7D5750666D716F,$3E733E39393C7D7D
   Data.q $6C3E7D6F6E28733E,$5750666D716C3E71,$3E39393C7D7D7D7D,$7D6F6E28733E3E73,$666D716D3E716D3E
   Data.q $393C7D7D7D7D5750,$787D6F6E28733E39,$3578716A353A3435,$50666D716A353A34,$7D57507D7D7D7D57
   Data.q $732D29382E7D7D7D,$327D6F6E28733833,$712A32313B2F382B,$6D716A353A343578,$507D7D7D7D575066
   Data.q $313C3E7D7D7D7D57,$3E757D3433287331,$3E7D716C3E7D716D,$3E7D716E3E7D716F,$3E7D71683E7D7169
   Data.q $3F7D716A3E7D716B,$7D71742A322F2F32,$6D3E757D711F080E,$6F3E7D716C3E7D71,$693E7D716E3E7D71
   Data.q $6B3E7D71683E7D71,$787D7D716A3E7D71,$716C2D787D716D2D,$2D787D716F2D787D,$7D71692D787D716E
   Data.q $6B2D787D71682D78,$5066746A2D787D71,$327C1D7D7D7D7D57,$7D2A32313B2F382B,$1108107D3C2F3F7D
   Data.q $666C11020D191210,$7D7D7D7D7D57507D,$33732D29382E7D7D,$712D7D6F6E287338,$6D712A322F2F323F
   Data.q $7D7D5750666C6D25,$2D7C1D7D7D7D7D7D,$1108107D3C2F3F7D,$666F11020D191210,$7D7D7D7D7D57507D
   Data.q $313C3E7D2D1D7D7D,$3E7D716D3E757D31,$3E7D716F3E7D716C,$3E7D71693E7D716E,$3E7D716B3E7D7168
   Data.q $322F2F323F7D716A,$711F080E7D71742A,$6C3E7D716D3E757D,$6E3E7D716F3E7D71,$683E7D71693E7D71
   Data.q $6A3E7D716B3E7D71,$7D716D2D787D7D71,$6F2D787D716C2D78,$787D716E2D787D71,$71682D787D71692D
   Data.q $2D787D716B2D787D,$7D7D7D575066746A,$0D1912101108107D,$7D7D5750676C1102,$29382E7D7D7D7D7D
   Data.q $6F6E28732C38732D,$322F2F323F712D7D,$50666C6D256D712A,$7D7D7D7D7D7D7D57,$7D31313C3E7D2D1D
   Data.q $716C3E7D716D3E75,$716E3E7D716F3E7D,$71683E7D71693E7D,$716A3E7D716B3E7D,$742A322F2F323F7D
   Data.q $757D7119191C7D71,$7D716C3E7D716D3E,$7D716E3E7D716F3E,$7D71683E7D71693E,$7D716A3E7D716B3E
   Data.q $2D787D716D2D787D,$7D716F2D787D716C,$692D787D716E2D78,$787D71682D787D71,$746A2D787D716B2D
   Data.q $107D7D7D7D575066,$11020D1912101108,$7D7D7D7D5750676F,$382F7D7D7D7D5750,$5750663433287329
   Data.q $7357507D5750207D,$2F73757D3E33283B,$7D6F6E3F737D3A38,$3A382F737D716D3C,$6C3C7D6F6E3F737D
   Data.q $737D3A382F737D71,$7D716F3C7D6F6E3F,$6E3F737D3A382F73,$2F737D716E3C7D6F,$7D6F6E3F737D3A38
   Data.q $3A382F737D71693C,$683C7D6F6E3F737D,$737D3A382F737D71,$7D716B3C7D6F6E3F,$6E3F737D3A382F73
   Data.q $382F7D746A3C7D6F,$73757D293314393C,$696B3F737D3A382F,$290D25383933347D,$7D3A382F737D712F
   Data.q $2539347D6F6E3F73,$5750267D57507D74,$3A382F737D7D7D7D,$7D7D7D6B6C3F737D,$3419393C382F3529
   Data.q $3E32313F7D7D7130,$313F7D7130341936,$7D71253914363E32,$3914393C382F3529,$7D7D7D7D57506625
   Data.q $6F6E3F733A382F73,$297D712D3038297D,$7D3914393C382F35,$393C382F35297D71,$7D7D66313C293229
   Data.q $7D7D7D7D57507D7D,$696B3F733A382F73,$29393C382F35297D,$7D71696B313C2932,$7D66696B2D303829
   Data.q $57507D7D7D7D5750,$732B32307D7D7D7D,$32313F7D7D6B6C28,$3E7871253914363E,$5066257339343C29
   Data.q $6C28732B32305457,$363E32313F7D7D6B,$293E337871303419,$575066257339343C,$6B6C28732B323054
   Data.q $393C382F35297D7D,$3429337871303419,$3054575066257339,$7D7D6B6C28732B32,$3914393C382F3529
   Data.q $2573393429787125,$507D7D7D7D575066,$7D57507D7D7D7D57,$7D7D7D57507D7D7D,$39342A733128307D
   Data.q $35297D6B6C287338,$3C293229393C382F,$19363E32313F7131,$3C382F3529713034,$7D7D7D6630341939
   Data.q $7D7D7D7D7D57507D,$7D7D7D57507D7D7D,$39342A733128307D,$35297D6F6E287338,$3C293229393C382F
   Data.q $7125393471696B31,$3229393C382F3529,$7D7D575066313C29,$6B3F7331352E7D7D,$393C382F35297D69
   Data.q $71696B313C293229,$3229393C382F3529,$666E71696B313C29,$575065777D72727D,$7D7D7D7D7D7D7D7D
   Data.q $2B3E7D7D7D7D5750,$6C28736F6E287329,$3C382F35297D7D6B,$2F3529717D391439,$5466253914393C38
   Data.q $2A73312830545750,$7D6B6C2873383934,$32313F712D303829,$352971253914363E,$66303419393C382F
   Data.q $393C5457507D7D7D,$35297D6F6E287339,$717D3914393C382F,$2F3529712D303829,$50667D3914393C38
   Data.q $7D57507D7D7D7D57,$2873292B3E7D7D7D,$7D7D6F6E2873696B,$717D696B2D303829,$3914393C382F3529
   Data.q $507D7D7D7D575066,$39393C7D7D7D7D57,$2F35297D696B2873,$313C293229393C38,$382F3529717D696B
   Data.q $6B313C293229393C,$696B2D3038297169,$7D7D7D7D5750667D,$7D696B3F7331352E,$3229393C382F3529
   Data.q $352971696B313C29,$3C293229393C382F,$7D7D666F71696B31,$7D7D69777D72727D,$57507D7D7D7D5750
   Data.q $7339393C7D7D7D7D,$383933347D696B28,$33347D712F290D25,$7D712F290D253839,$3229393C382F3529
   Data.q $575066696B313C29,$7D7D57507D7D7D7D,$342A733128307D7D,$297D6F6E28733839,$293229393C382F35
   Data.q $2F352971696B313C,$313C293229393C38,$7D7D7D5750666971,$507D7D7D7D57507D,$7339317D7D7D7D57
   Data.q $2873313C3F32313A,$3406716D3C7D6F6E,$002F290D25383933,$3C7D7D7D7D575066,$347D696B28733939
   Data.q $712F290D25383933,$290D25383933347D,$3C382F35297D712F,$696B313C29322939,$507D7D7D7D575066
   Data.q $7339317D7D7D7D57,$2873313C3F32313A,$3406716C3C7D6F6E,$002F290D25383933,$3C7D7D7D7D575066
   Data.q $347D696B28733939,$712F290D25383933,$290D25383933347D,$3C382F35297D712F,$696B313C29322939
   Data.q $507D7D7D7D575066,$7339317D7D7D7D57,$2873313C3F32313A,$3406716F3C7D6F6E,$002F290D25383933
   Data.q $3C7D7D7D7D575066,$347D696B28733939,$712F290D25383933,$290D25383933347D,$3C382F35297D712F
   Data.q $696B313C29322939,$507D7D7D7D575066,$7339317D7D7D7D57,$2873313C3F32313A,$3406716E3C7D6F6E
   Data.q $002F290D25383933,$3C7D7D7D7D575066,$347D696B28733939,$712F290D25383933,$290D25383933347D
   Data.q $3C382F35297D712F,$696B313C29322939,$507D7D7D7D575066,$7339317D7D7D7D57,$2873313C3F32313A
   Data.q $340671693C7D6F6E,$002F290D25383933,$3C7D7D7D7D575066,$347D696B28733939,$712F290D25383933
   Data.q $290D25383933347D,$3C382F35297D712F,$696B313C29322939,$507D7D7D7D575066,$7339317D7D7D7D57
   Data.q $2873313C3F32313A,$340671683C7D6F6E,$002F290D25383933,$3C7D7D7D7D575066,$347D696B28733939
   Data.q $712F290D25383933,$290D25383933347D,$3C382F35297D712F,$696B313C29322939,$507D7D7D7D575066
   Data.q $7339317D7D7D7D57,$2873313C3F32313A,$3406716B3C7D6F6E,$002F290D25383933,$3C7D7D7D7D575066
   Data.q $347D696B28733939,$712F290D25383933,$290D25383933347D,$3C382F35297D712F,$696B313C29322939
   Data.q $507D7D7D7D575066,$7339317D7D7D7D57,$2873313C3F32313A,$3406716A3C7D6F6E,$002F290D25383933
   Data.q $507D7D7D7D575066,$7D57507D7D7D7D57,$287329382F7D7D7D,$50207D5750663433,$283B737D57507D57
   Data.q $3829342F2A7D3E33,$382F73757D293314,$347D696B3F737D3A,$712F290D25383933,$3F737D3A382F737D
   Data.q $7D712539347D6F6E,$6E3F737D3A382F73,$2F737D716D3C7D6F,$7D6F6E3F737D3A38,$3A382F737D716C3C
   Data.q $6F3C7D6F6E3F737D,$737D3A382F737D71,$7D716E3C7D6F6E3F,$6E3F737D3A382F73,$2F737D71693C7D6F
   Data.q $7D6F6E3F737D3A38,$3A382F737D71683C,$6B3C7D6F6E3F737D,$737D3A382F737D71,$7D746A3C7D6F6E3F
   Data.q $7D7D5750267D5750,$737D3A382F737D7D,$35297D7D7D6B6C3F,$71303419393C382F,$19363E32313F7D7D
   Data.q $3E32313F7D713034,$35297D7125391436,$66253914393C382F,$2F737D7D7D7D5750,$297D6F6E3F733A38
   Data.q $2F35297D712D3038,$7D717D3914393C38,$3229393C382F3529,$7D7D7D7D66313C29,$2F737D7D7D7D5750
   Data.q $297D696B3F733A38,$293229393C382F35,$38297D71696B313C,$57507D66696B2D30,$7D7D57507D7D7D7D
   Data.q $6C28732B32307D7D,$363E32313F7D7D6B,$3C293E7871253914,$5457506625733934,$7D6B6C28732B3230
   Data.q $3419363E32313F7D,$343C293E33787130,$3054575066257339,$7D7D6B6C28732B32,$3419393C382F3529
   Data.q $7339342933787130,$2B32305457506625,$35297D7D6B6C2873,$71253914393C382F,$5066257339342978
   Data.q $7D57507D7D7D7D57,$7D7D7D57507D7D7D,$307D7D7D7D57507D,$733839342A733128,$382F35297D6B6C28
   Data.q $71313C293229393C,$303419363E32313F,$19393C382F352971,$507D7D7D7D663034,$7D7D7D7D7D7D7D57
   Data.q $307D7D7D7D57507D,$733839342A733128,$382F35297D6F6E28,$6B313C293229393C,$3529712539347169
   Data.q $3C293229393C382F,$7D7D7D7D57506631,$7D696B3F7331352E,$3229393C382F3529,$352971696B313C29
   Data.q $3C293229393C382F,$727D666E71696B31,$7D7D575065777D72,$57507D7D7D7D7D7D,$73292B3E7D7D7D7D
   Data.q $7D6B6C28736F6E28,$14393C382F35297D,$3C382F3529717D39,$5750546625391439,$39342A7331283054
   Data.q $38297D6B6C287338,$363E32313F712D30,$382F352971253914,$7D7D66303419393C,$7339393C5457507D
   Data.q $382F35297D6F6E28,$3829717D3914393C,$3C382F3529712D30,$7D5750667D391439,$7D7D7D57507D7D7D
   Data.q $696B2873292B3E7D,$38297D7D6F6E2873,$3529717D696B2D30,$50663914393C382F,$7D57507D7D7D7D57
   Data.q $287339393C7D7D7D,$3C382F35297D696B,$696B313C29322939,$393C382F3529717D,$71696B313C293229
   Data.q $667D696B2D303829,$352E7D7D7D7D5750,$35297D696B3F7331,$3C293229393C382F,$382F352971696B31
   Data.q $6B313C293229393C,$727D7D7D666F7169,$7D7D7D7D69777D72,$507D7D7D7D57507D,$39393C7D7D7D7D57
   Data.q $3933347D696B2873,$347D712F290D2538,$712F290D25383933,$29393C382F35297D,$5066696B313C2932
   Data.q $7D57507D7D7D7D57,$2A733128307D7D7D,$7D6F6E2873383934,$3229393C382F3529,$352971696B313C29
   Data.q $3C293229393C382F,$7D7D575066697131,$7D7D7D7D57507D7D,$3C3F32313A73292E,$34067D6F6E287331
   Data.q $002F290D25383933,$7D5750666D3C7D71,$287339393C7D7D7D,$25383933347D696B,$3933347D712F290D
   Data.q $297D712F290D2538,$293229393C382F35,$7D575066696B313C,$313A73292E7D7D7D,$6F6E2873313C3F32
   Data.q $0D2538393334067D,$666C3C7D71002F29,$393C7D7D7D7D5750,$33347D696B287339,$7D712F290D253839
   Data.q $2F290D2538393334,$393C382F35297D71,$66696B313C293229,$292E7D7D7D7D5750,$73313C3F32313A73
   Data.q $393334067D6F6E28,$7D71002F290D2538,$7D7D7D5750666F3C,$696B287339393C7D,$290D25383933347D
   Data.q $25383933347D712F,$2F35297D712F290D,$313C293229393C38,$7D7D7D575066696B,$3F32313A73292E7D
   Data.q $067D6F6E2873313C,$2F290D2538393334,$5750666E3C7D7100,$7339393C7D7D7D7D,$383933347D696B28
   Data.q $33347D712F290D25,$7D712F290D253839,$3229393C382F3529,$575066696B313C29,$3A73292E7D7D7D7D
   Data.q $6E2873313C3F3231,$2538393334067D6F,$693C7D71002F290D,$3C7D7D7D7D575066,$347D696B28733939
   Data.q $712F290D25383933,$290D25383933347D,$3C382F35297D712F,$696B313C29322939,$2E7D7D7D7D575066
   Data.q $313C3F32313A7329,$3334067D6F6E2873,$71002F290D253839,$7D7D575066683C7D,$6B287339393C7D7D
   Data.q $0D25383933347D69,$383933347D712F29,$35297D712F290D25,$3C293229393C382F,$7D7D575066696B31
   Data.q $32313A73292E7D7D,$7D6F6E2873313C3F,$290D253839333406,$50666B3C7D71002F,$39393C7D7D7D7D57
   Data.q $3933347D696B2873,$347D712F290D2538,$712F290D25383933,$29393C382F35297D,$5066696B313C2932
   Data.q $73292E7D7D7D7D57,$2873313C3F32313A,$38393334067D6F6E,$3C7D71002F290D25,$7D7D7D7D5750666A
   Data.q $57507D7D7D7D5750,$7329382F7D7D7D7D,$207D575066343328,$57507D57507D5750,$73757D3E33283B73
   Data.q $6F6E3F737D3A382F,$382F737D716D3F7D,$3F7D6F6E3F737D3A,$7D3A382F737D716C,$716F3F7D6F6E3F73
   Data.q $3F737D3A382F737D,$737D716E3F7D6F6E,$6F6E3F737D3A382F,$382F737D71693F7D,$3F7D6F6E3F737D3A
   Data.q $7D3A382F737D7168,$716B3F7D6F6E3F73,$3F737D3A382F737D,$0E7D746A3F7D6F6E,$191210180F1C080C
   Data.q $7D3A382F73757D0D,$716D3C7D6F6E3F73,$3F737D3A382F737D,$737D716C3C7D6F6E,$6F6E3F737D3A382F
   Data.q $382F737D716F3C7D,$3C7D6F6E3F737D3A,$7D3A382F737D716E,$71693C7D6F6E3F73,$3F737D3A382F737D
   Data.q $737D71683C7D6F6E,$6F6E3F737D3A382F,$382F737D716B3C7D,$3C7D6F6E3F737D3A,$50267D57507D746A
   Data.q $7D57507D7D7D7D57,$7331313C3E7D7D7D,$716D3F757D343328,$716F3F7D716C3F7D,$71693F7D716E3F7D
   Data.q $716B3F7D71683F7D,$08107D71746A3F7D,$757D710D19121011,$7D716C3C7D716D3C,$7D716E3C7D716F3C
   Data.q $7D71683C7D71693C,$7D716A3C7D716B3C,$716C3C7D716D3C7D,$716E3C7D716F3C7D,$71683C7D71693C7D
   Data.q $746A3C7D716B3C7D,$7D57507D7D7D7D66,$287329382F7D7D7D,$50207D5750663433,$33283B7357507D57
   Data.q $7D3A382F73757D3E,$716D3F7D6F6E3F73,$3F737D3A382F737D,$737D716C3F7D6F6E,$6F6E3F737D3A382F
   Data.q $382F737D716F3F7D,$3F7D6F6E3F737D3A,$7D3A382F737D716E,$71693F7D6F6E3F73,$3F737D3A382F737D
   Data.q $737D71683F7D6F6E,$6F6E3F737D3A382F,$382F737D716B3F7D,$3F7D6F6E3F737D3A,$12100B13147D746A
   Data.q $3A382F73757D0D19,$6D3C7D6F6E3F737D,$737D3A382F737D71,$7D716C3C7D6F6E3F,$6E3F737D3A382F73
   Data.q $2F737D716F3C7D6F,$7D6F6E3F737D3A38,$3A382F737D716E3C,$693C7D6F6E3F737D,$737D3A382F737D71
   Data.q $7D71683C7D6F6E3F,$6E3F737D3A382F73,$2F737D716B3C7D6F,$7D6F6E3F737D3A38,$267D57507D746A3C
   Data.q $7D7D7D7D57505750,$6E28737D3A382F73,$7163656125787D6F,$7D6663656124787D,$2F737D7D7D7D5750
   Data.q $2D737D7D7D7D3A38,$50662D7D7D39382F,$382F737D7D7D7D57,$347D6F6E28737D3A,$507D7D7D7D575066
   Data.q $313C3E7D7D7D7D57,$78757D3433287331,$716C25787D716D25,$25787D716F25787D,$7D716925787D716E
   Data.q $6B25787D71682578,$7D71746A25787D71,$141A141F040D121E,$716D3C757D710913,$716F3C7D716C3C7D
   Data.q $71693C7D716E3C7D,$716B3C7D71683C7D,$7D575066746A3C7D,$3F732F32257D7D7D,$716D24787D7D6F6E
   Data.q $666D2478716D2478,$32257D7D7D7D5750,$787D7D6F6E3F732F,$78716C2478716C24,$7D7D7D5750666C24
   Data.q $6F6E3F732F32257D,$2478716F24787D7D,$5750666F2478716F,$732F32257D7D7D7D,$6E24787D7D6F6E3F
   Data.q $6E2478716E247871,$257D7D7D7D575066,$7D7D6F6E3F732F32,$7169247871692478,$7D7D575066692478
   Data.q $6E3F732F32257D7D,$78716824787D7D6F,$5066682478716824,$2F32257D7D7D7D57,$24787D7D6F6E3F73
   Data.q $2478716B2478716B,$7D7D7D7D5750666B,$7D6F6E28732B3230,$6D256D716A24787D,$7D7D7D7D5750666C
   Data.q $7D7D575057505750,$39256D7D72727D7D,$506C6D6C6C7D707D,$313C3E7D7D7D7D57,$78757D3433287331
   Data.q $716C24787D716D24,$24787D716F24787D,$7D716924787D716E,$6B24787D71682478,$7D71746A24787D71
   Data.q $710D191210110810,$787D716D2578757D,$716F25787D716C25,$25787D716E25787D,$7D716825787D7169
   Data.q $6A25787D716B2578,$7D716D24787D7D71,$6F24787D716C2478,$787D716E24787D71,$716824787D716924
   Data.q $24787D716B24787D,$7D7D7D575066746A,$3C3E7D7D7D7D5750,$757D343328733131,$6C25787D716D2578
   Data.q $787D716F25787D71,$716925787D716E25,$25787D716825787D,$71746A25787D716B,$0D1912101108107D
   Data.q $7D716D2578757D71,$6F25787D716C2578,$787D716E25787D71,$716825787D716925,$25787D716B25787D
   Data.q $716D25787D7D716A,$25787D716C25787D,$7D716E25787D716F,$6825787D71692578,$787D716B25787D71
   Data.q $5750575066746A25,$283072727D7D7D7D,$7125750D39321031,$7D7D57506674247D,$287331313C3E7D7D
   Data.q $716D2578757D3433,$25787D716C25787D,$7D716E25787D716F,$6825787D71692578,$787D716B25787D71
   Data.q $1108107D71746A25,$78757D710D191210,$716C25787D716D25,$25787D716F25787D,$7D716925787D716E
   Data.q $6B25787D71682578,$7D7D716A25787D71,$6C25787D716D2578,$787D716F25787D71,$716925787D716E25
   Data.q $25787D716825787D,$66746A25787D716B,$7D7D7D7D57505750,$3433287331313C3E,$787D716D2478757D
   Data.q $716F24787D716C24,$24787D716E24787D,$7D716824787D7169,$6A24787D716B2478,$12101108107D7174
   Data.q $6D2578757D710D19,$787D716C25787D71,$716E25787D716F25,$25787D716925787D,$7D716B25787D7168
   Data.q $24787D7D716A2578,$7D716C24787D716D,$6E24787D716F2478,$787D716924787D71,$716B24787D716824
   Data.q $575066746A24787D,$31313C3E7D7D7D7D,$2578757D34332873,$7D716C25787D716D,$6E25787D716F2578
   Data.q $787D716925787D71,$716B25787D716825,$107D71746A25787D,$7D710D1912101108,$25787D716D257875
   Data.q $7D716F25787D716C,$6925787D716E2578,$787D716825787D71,$716A25787D716B25,$787D716D25787D7D
   Data.q $716F25787D716C25,$25787D716E25787D,$7D716825787D7169,$6A25787D716B2578,$7D7D7D7D57506674
   Data.q $3433287331313C3E,$787D716D2478757D,$716F24787D716C24,$24787D716E24787D,$7D716824787D7169
   Data.q $6A24787D716B2478,$12101108107D7174,$6D2578757D710D19,$787D716C25787D71,$716E25787D716F25
   Data.q $25787D716925787D,$7D716B25787D7168,$24787D7D716A2578,$7D716C24787D716D,$6E24787D716F2478
   Data.q $787D716924787D71,$716B24787D716824,$575066746A24787D,$31313C3E7D7D7D7D,$2578757D34332873
   Data.q $7D716C25787D716D,$6E25787D716F2578,$787D716925787D71,$716B25787D716825,$107D71746A25787D
   Data.q $7D710D1912101108,$25787D716D257875,$7D716F25787D716C,$6925787D716E2578,$787D716825787D71
   Data.q $716A25787D716B25,$787D716D25787D7D,$716F25787D716C25,$25787D716E25787D,$7D716825787D7169
   Data.q $6A25787D716B2578,$7D7D7D7D57506674,$7D7D575057505750,$6F256D7D72727D7D,$506D6C6D6D7D707D
   Data.q $1031283072725457,$247D7125750D3932,$7D7D7D7D57506674,$3433287331313C3E,$787D716D2578757D
   Data.q $716F25787D716C25,$25787D716E25787D,$7D716825787D7169,$6A25787D716B2578,$12101108107D7174
   Data.q $6D2578757D710D19,$787D716C25787D71,$716E25787D716F25,$25787D716925787D,$7D716B25787D7168
   Data.q $25787D7D716A2578,$7D716C25787D716D,$6E25787D716F2578,$787D716925787D71,$716B25787D716825
   Data.q $575066746A25787D,$31313C3E7D7D7D7D,$2478757D34332873,$7D716C24787D716D,$6E24787D716F2478
   Data.q $787D716924787D71,$716B24787D716824,$107D71746A24787D,$7D710D1912101108,$25787D716D257875
   Data.q $7D716F25787D716C,$6925787D716E2578,$787D716825787D71,$716A25787D716B25,$787D716D24787D7D
   Data.q $716F24787D716C24,$24787D716E24787D,$7D716824787D7169,$6A24787D716B2478,$7D7D7D7D57506674
   Data.q $3433287331313C3E,$787D716D2578757D,$716F25787D716C25,$25787D716E25787D,$7D716825787D7169
   Data.q $6A25787D716B2578,$12101108107D7174,$6D2578757D710D19,$787D716C25787D71,$716E25787D716F25
   Data.q $25787D716925787D,$7D716B25787D7168,$25787D7D716A2578,$7D716C25787D716D,$6E25787D716F2578
   Data.q $787D716925787D71,$716B25787D716825,$575066746A25787D,$283072727D7D7D7D,$7125750D39321031
   Data.q $7D7D57506674247D,$287331313C3E7D7D,$716D2578757D3433,$25787D716C25787D,$7D716E25787D716F
   Data.q $6825787D71692578,$787D716B25787D71,$1108107D71746A25,$78757D710D191210,$716C25787D716D25
   Data.q $25787D716F25787D,$7D716925787D716E,$6B25787D71682578,$7D7D716A25787D71,$6C25787D716D2578
   Data.q $787D716F25787D71,$716925787D716E25,$25787D716825787D,$66746A25787D716B,$72727D7D7D7D5750
   Data.q $750D393210312830,$57506674247D7125,$31313C3E7D7D7D7D,$2578757D34332873,$7D716C25787D716D
   Data.q $6E25787D716F2578,$787D716925787D71,$716B25787D716825,$107D71746A25787D,$7D710D1912101108
   Data.q $25787D716D257875,$7D716F25787D716C,$6925787D716E2578,$787D716825787D71,$716A25787D716B25
   Data.q $787D716D25787D7D,$716F25787D716C25,$25787D716E25787D,$7D716825787D7169,$6A25787D716B2578
   Data.q $7D7D7D7D57506674,$72727D7D7D7D5750,$6D7D607D3E256D7D,$5457506D6D6C6C25,$3932103128307272
   Data.q $6674247D7125750D,$3C3E7D7D7D7D5750,$757D343328733131,$6C25787D716D2578,$787D716F25787D71
   Data.q $716925787D716E25,$25787D716825787D,$71746A25787D716B,$0D1912101108107D,$7D716D2578757D71
   Data.q $6F25787D716C2578,$787D716E25787D71,$716825787D716925,$25787D716B25787D,$716D25787D7D716A
   Data.q $25787D716C25787D,$7D716E25787D716F,$6825787D71692578,$787D716B25787D71,$7D7D575066746A25
   Data.q $1031283072727D7D,$247D7125750D3932,$7D7D7D7D57506674,$3433287331313C3E,$787D716D2578757D
   Data.q $716F25787D716C25,$25787D716E25787D,$7D716825787D7169,$6A25787D716B2578,$12101108107D7174
   Data.q $6D2578757D710D19,$787D716C25787D71,$716E25787D716F25,$25787D716925787D,$7D716B25787D7168
   Data.q $25787D7D716A2578,$7D716C25787D716D,$6E25787D716F2578,$787D716925787D71,$716B25787D716825
   Data.q $575066746A25787D,$31313C3E7D7D7D7D,$2478757D34332873,$7D716C24787D716D,$6E24787D716F2478
   Data.q $787D716924787D71,$716B24787D716824,$107D71746A24787D,$7D710D1912101108,$25787D716D257875
   Data.q $7D716F25787D716C,$6925787D716E2578,$787D716825787D71,$716A25787D716B25,$787D716D24787D7D
   Data.q $716F24787D716C24,$24787D716E24787D,$7D716824787D7169,$6A24787D716B2478,$7D7D7D7D57506674
   Data.q $3433287331313C3E,$787D716D2578757D,$716F25787D716C25,$25787D716E25787D,$7D716825787D7169
   Data.q $6A25787D716B2578,$12101108107D7174,$6D2578757D710D19,$787D716C25787D71,$716E25787D716F25
   Data.q $25787D716925787D,$7D716B25787D7168,$25787D7D716A2578,$7D716C25787D716D,$6E25787D716F2578
   Data.q $787D716925787D71,$716B25787D716825,$575066746A25787D,$31313C3E7D7D7D7D,$2478757D34332873
   Data.q $7D716C24787D716D,$6E24787D716F2478,$787D716924787D71,$716B24787D716824,$107D71746A24787D
   Data.q $7D710D1912101108,$25787D716D257875,$7D716F25787D716C,$6925787D716E2578,$787D716825787D71
   Data.q $716A25787D716B25,$787D716D24787D7D,$716F24787D716C24,$24787D716E24787D,$7D716824787D7169
   Data.q $6A24787D716B2478,$7D7D7D7D57506674,$3433287331313C3E,$787D716D2578757D,$716F25787D716C25
   Data.q $25787D716E25787D,$7D716825787D7169,$6A25787D716B2578,$12101108107D7174,$6D2578757D710D19
   Data.q $787D716C25787D71,$716E25787D716F25,$25787D716925787D,$7D716B25787D7168,$25787D7D716A2578
   Data.q $7D716C25787D716D,$6E25787D716F2578,$787D716925787D71,$716B25787D716825,$575066746A25787D
   Data.q $575057507D7D7D7D,$6D7D72727D7D7D7D,$57503B3B3B3B3B25,$732F32257D7D7D7D,$3471347D7D6F6E3F
   Data.q $7D7D7D5750663471,$12100B131457507D,$5750676C11020D19,$31313C3E7D7D7D7D,$2478757D34332873
   Data.q $7D716C24787D716D,$6E24787D716F2478,$787D716924787D71,$716B24787D716824,$107D71746A24787D
   Data.q $7D710D1912101108,$25787D716D257875,$7D716F25787D716C,$6925787D716E2578,$787D716825787D71
   Data.q $716A25787D716B25,$787D716D24787D7D,$716F24787D716C24,$24787D716E24787D,$7D716824787D7169
   Data.q $6A24787D716B2478,$7D7D7D7D57506674,$3433287331313C3E,$787D716D2578757D,$716F25787D716C25
   Data.q $25787D716E25787D,$7D716825787D7169,$6A25787D716B2578,$12101108107D7174,$6D2578757D710D19
   Data.q $787D716C25787D71,$716E25787D716F25,$25787D716925787D,$7D716B25787D7168,$25787D7D716A2578
   Data.q $7D716C25787D716D,$6E25787D716F2578,$787D716925787D71,$716B25787D716825,$575066746A25787D
   Data.q $7D7D57507D7D7D7D,$6E287339393C7D7D,$71347D7D7D7D7D6F,$2E545750666C7134,$28733231732D2938
   Data.q $6F7134712D7D6F6E,$7D7D7D7D7D7D666D,$7D7D7D7D7D7D7D7D,$3F7D2D1D5457507D,$147D343328733C2F
   Data.q $11020D1912100B13,$7D7D7D7D5750666C,$72727D7D7D7D5750,$6C7D707D38256D7D,$72725457506D6C6C
   Data.q $750D393210312830,$57506674247D7125,$31313C3E7D7D7D7D,$2578757D34332873,$7D716C25787D716D
   Data.q $6E25787D716F2578,$787D716925787D71,$716B25787D716825,$107D71746A25787D,$7D710D1912101108
   Data.q $25787D716D257875,$7D716F25787D716C,$6925787D716E2578,$787D716825787D71,$716A25787D716B25
   Data.q $787D716D25787D7D,$716F25787D716C25,$25787D716E25787D,$7D716825787D7169,$6A25787D716B2578
   Data.q $7D7D7D7D57506674,$3433287331313C3E,$787D716D2478757D,$716F24787D716C24,$24787D716E24787D
   Data.q $7D716824787D7169,$6A24787D716B2478,$12101108107D7174,$6D2578757D710D19,$787D716C25787D71
   Data.q $716E25787D716F25,$25787D716925787D,$7D716B25787D7168,$24787D7D716A2578,$7D716C24787D716D
   Data.q $6E24787D716F2478,$787D716924787D71,$716B24787D716824,$575066746A24787D,$31313C3E7D7D7D7D
   Data.q $2578757D34332873,$7D716C25787D716D,$6E25787D716F2578,$787D716925787D71,$716B25787D716825
   Data.q $107D71746A25787D,$7D710D1912101108,$25787D716D257875,$7D716F25787D716C,$6925787D716E2578
   Data.q $787D716825787D71,$716A25787D716B25,$787D716D25787D7D,$716F25787D716C25,$25787D716E25787D
   Data.q $7D716825787D7169,$6A25787D716B2578,$7D7D7D7D57506674,$3433287331313C3E,$787D716D2478757D
   Data.q $716F24787D716C24,$24787D716E24787D,$7D716824787D7169,$6A24787D716B2478,$12101108107D7174
   Data.q $6D2578757D710D19,$787D716C25787D71,$716E25787D716F25,$25787D716925787D,$7D716B25787D7168
   Data.q $24787D7D716A2578,$7D716C24787D716D,$6E24787D716F2478,$787D716924787D71,$716B24787D716824
   Data.q $575066746A24787D,$31313C3E7D7D7D7D,$2578757D34332873,$7D716C25787D716D,$6E25787D716F2578
   Data.q $787D716925787D71,$716B25787D716825,$107D71746A25787D,$7D710D1912101108,$25787D716D257875
   Data.q $7D716F25787D716C,$6925787D716E2578,$787D716825787D71,$716A25787D716B25,$787D716D25787D7D
   Data.q $716F25787D716C25,$25787D716E25787D,$7D716825787D7169,$6A25787D716B2578,$7D7D7D7D57506674
   Data.q $3433287331313C3E,$787D716D2478757D,$716F24787D716C24,$24787D716E24787D,$7D716824787D7169
   Data.q $6A24787D716B2478,$12101108107D7174,$6D2578757D710D19,$787D716C25787D71,$716E25787D716F25
   Data.q $25787D716925787D,$7D716B25787D7168,$24787D7D716A2578,$7D716C24787D716D,$6E24787D716F2478
   Data.q $787D716924787D71,$716B24787D716824,$575066746A24787D,$31313C3E7D7D7D7D,$2578757D34332873
   Data.q $7D716C25787D716D,$6E25787D716F2578,$787D716925787D71,$716B25787D716825,$107D71746A25787D
   Data.q $7D710D1912101108,$25787D716D257875,$7D716F25787D716C,$6925787D716E2578,$787D716825787D71
   Data.q $716A25787D716B25,$787D716D25787D7D,$716F25787D716C25,$25787D716E25787D,$7D716825787D7169
   Data.q $6A25787D716B2578,$7D7D7D7D57506674,$7D7D7D7D57505750,$3B3B3B256D7D7272,$3B3B3B3B3B3B3B3B
   Data.q $3B3B3B3B3B3B3B3B,$3B3B3B3B3B3B3B3B,$3B3B3B3B3B3B3B3B,$3B3B3B3B3B3B3B3B,$3B3B3B3B3B3B3B3B
   Data.q $7D7D57503B3B3B3B,$6E3F732F32257D7D,$34713471347D7D6F,$507D7D7D7D575066,$0D1912100B131457
   Data.q $57507D7D676F1102,$31313C3E7D7D7D7D,$2478757D34332873,$7D716C24787D716D,$6E24787D716F2478
   Data.q $787D716924787D71,$716B24787D716824,$107D71746A24787D,$7D710D1912101108,$25787D716D257875
   Data.q $7D716F25787D716C,$6925787D716E2578,$787D716825787D71,$716A25787D716B25,$787D716D24787D7D
   Data.q $716F24787D716C24,$24787D716E24787D,$7D716824787D7169,$6A24787D716B2478,$7D7D7D7D57506674
   Data.q $3433287331313C3E,$787D716D2578757D,$716F25787D716C25,$25787D716E25787D,$7D716825787D7169
   Data.q $6A25787D716B2578,$12101108107D7174,$6D2578757D710D19,$787D716C25787D71,$716E25787D716F25
   Data.q $25787D716925787D,$7D716B25787D7168,$25787D7D716A2578,$7D716C25787D716D,$6E25787D716F2578
   Data.q $787D716925787D71,$716B25787D716825,$575066746A25787D,$7D7D57507D7D7D7D,$6E287339393C7D7D
   Data.q $71347D7D7D7D7D6F,$2E545750666C7134,$28733231732D2938,$6F7134712D7D6F6E,$7D7D7D7D7D66646C
   Data.q $7D7D7D7D7D7D7D7D,$7D2D1D5457507D7D,$7D343328733C2F3F,$020D1912100B1314,$5057505750666F11
   Data.q $313C3E7D7D7D7D57,$78757D3433287331,$716C24787D716D24,$24787D716F24787D,$7D716924787D716E
   Data.q $6B24787D71682478,$7D71746A24787D71,$710D191210110810,$787D716D2578757D,$716F25787D716C25
   Data.q $25787D716E25787D,$7D716825787D7169,$6A25787D716B2578,$7D716D24787D7D71,$6F24787D716C2478
   Data.q $787D716E24787D71,$716824787D716924,$24787D716B24787D,$7D7D7D575066746A,$33287331313C3E7D
   Data.q $3F7D716D3F757D34,$3F7D716F3F7D716C,$3F7D71693F7D716E,$3F7D716B3F7D7168,$040D121E7D71746A
   Data.q $7D710913141A141F,$24787D716D247875,$7D716F24787D716C,$6924787D716E2478,$787D716824787D71
   Data.q $746A24787D716B24,$507D7D7D7D575066,$29382F7D7D7D7D57,$7D57506634332873,$737D57507D575020
   Data.q $2F73757D3E33283B,$7D6F6E3F737D3A38,$3A382F737D716D3F,$6C3F7D6F6E3F737D,$737D3A382F737D71
   Data.q $7D716F3F7D6F6E3F,$6E3F737D3A382F73,$2F737D716E3F7D6F,$7D6F6E3F737D3A38,$3A382F737D71693F
   Data.q $683F7D6F6E3F737D,$737D3A382F737D71,$7D716B3F7D6F6E3F,$6E3F737D3A382F73,$18137D746A3F7D6F
   Data.q $73757D0D1912101A,$6F6E3F737D3A382F,$382F737D716D3C7D,$3C7D6F6E3F737D3A,$7D3A382F737D716C
   Data.q $716F3C7D6F6E3F73,$3F737D3A382F737D,$737D716E3C7D6F6E,$6F6E3F737D3A382F,$382F737D71693C7D
   Data.q $3C7D6F6E3F737D3A,$7D3A382F737D7168,$716B3C7D6F6E3F73,$3F737D3A382F737D,$507D746A3C7D6F6E
   Data.q $7D7D7D5750267D57,$7D7D7D7D7D7D7D7D,$737D7D7D7D57507D,$6F6E3F737D3A382F,$7D666365612D787D
   Data.q $7D7D7D57507D7D7D,$317D7D7D7D57507D,$73292E33323E7339,$267D6F6E2873692B,$6C2D787D716D2D78
   Data.q $787D716F2D787D71,$760D020671206E2D,$7D7D7D575066006D,$2E33323E7339317D,$6F6E2873692B7329
   Data.q $787D71692D78267D,$716B2D787D71682D,$020671206A2D787D,$575066006B6C760D,$7D7D57507D7D7D7D
   Data.q $7D7D7D7D57507D7D,$28733E3E733F282E,$787D716D3F7D6F6E,$50666D3C7D716D2D,$3F282E7D7D7D7D57
   Data.q $6F6E28733E3E733E,$6C2D787D716C3F7D,$7D5750666C3C7D71,$733E3F282E7D7D7D,$3F7D6F6E28733E3E
   Data.q $7D716F2D787D716F,$7D7D7D5750666F3C,$3E3E733E3F282E7D,$716E3F7D6F6E2873,$6E3C7D716E2D787D
   Data.q $2E7D7D7D7D575066,$28733E3E733E3F28,$787D71693F7D6F6E,$5066693C7D71692D,$3F282E7D7D7D7D57
   Data.q $6F6E28733E3E733E,$682D787D71683F7D,$7D575066683C7D71,$733E3F282E7D7D7D,$3F7D6F6E28733E3E
   Data.q $7D716B2D787D716B,$7D7D7D5750666B3C,$3E3E733E3F282E7D,$716A3F7D6F6E2873,$6A3C7D716A2D787D
   Data.q $2F7D7D7D7D575066,$5066343328732938,$57507D5750207D57,$757D3E33283B737D,$6E3F737D3A382F73
   Data.q $716D052A38337D6F,$3F737D3A382F737D,$6C052A38337D6F6E,$737D3A382F737D71,$052A38337D6F6E3F
   Data.q $7D3A382F737D716F,$2A38337D6F6E3F73,$3A382F737D716E05,$38337D6F6E3F737D,$382F737D7169052A
   Data.q $337D6F6E3F737D3A,$2F737D7168052A38,$7D6F6E3F737D3A38,$737D716B052A3833,$6F6E3F737D3A382F
   Data.q $7D716A052A38337D,$3F737D3A382F737D,$6D042A38337D6F6E,$737D3A382F737D71,$042A38337D6F6E3F
   Data.q $7D3A382F737D716C,$2A38337D6F6E3F73,$3A382F737D716F04,$38337D6F6E3F737D,$382F737D716E042A
   Data.q $337D6F6E3F737D3A,$2F737D7169042A38,$7D6F6E3F737D3A38,$737D7168042A3833,$6F6E3F737D3A382F
   Data.q $7D716B042A38337D,$6E3F737D3A382F73,$746A042A38337D6F,$757D090D111F197D,$6E3F737D3A382F73
   Data.q $2F737D716D257D6F,$7D6F6E3F737D3A38,$3A382F737D716C25,$6F257D6F6E3F737D,$737D3A382F737D71
   Data.q $7D716E257D6F6E3F,$6E3F737D3A382F73,$2F737D7169257D6F,$7D6F6E3F737D3A38,$3A382F737D716825
   Data.q $6B257D6F6E3F737D,$737D3A382F737D71,$7D716A257D6F6E3F,$3F737D3A382F737D,$737D716D247D6F6E
   Data.q $6F6E3F737D3A382F,$382F737D716C247D,$247D6F6E3F737D3A,$7D3A382F737D716F,$716E247D6F6E3F73
   Data.q $3F737D3A382F737D,$737D7169247D6F6E,$6F6E3F737D3A382F,$382F737D7168247D,$247D6F6E3F737D3A
   Data.q $7D3A382F737D716B,$746A247D6F6E3F73,$7D5750267D57507D,$7D3A382F737D7D7D,$2519787D6F6E3F73
   Data.q $2509787D71636561,$612E787D71636561,$612E19787D716365,$7D7D57507D666365,$7D7D7D7D57507D7D
   Data.q $33347D29383A7272,$33347D382E2F382B,$776F75382E2F382B,$7D7D5750742D7124,$287331313C3E7D7D
   Data.q $716D2E78757D3433,$2E787D716C2E787D,$7D716E2E787D716F,$682E787D71692E78,$787D716B2E787D71
   Data.q $19191C7D71746A2E,$24757D710D191210,$247D716C247D716D,$247D716E247D716F,$247D7168247D7169
   Data.q $7D7D716A247D716B,$7D716C247D716D24,$7D716E247D716F24,$7D7168247D716924,$66746A247D716B24
   Data.q $3C3E7D7D7D7D5750,$757D343328733131,$6C2E787D716D2E78,$787D716F2E787D71,$71692E787D716E2E
   Data.q $2E787D71682E787D,$71746A2E787D716B,$0D1912100B13147D,$7D716D2E78757D71,$6F2E787D716C2E78
   Data.q $787D716E2E787D71,$71682E787D71692E,$2E787D716B2E787D,$7D7D7D575066746A,$727D7D7D7D57507D
   Data.q $57506F03256E7D72,$31313C3E7D7D7D7D,$1978757D34332873,$6C2519787D716D25,$7D716F2519787D71
   Data.q $19787D716E251978,$682519787D716925,$7D716B2519787D71,$107D71746A251978,$7D710D1912101108
   Data.q $716C257D716D2575,$716E257D716F257D,$7168257D7169257D,$716A257D716B257D,$6C257D716D257D7D
   Data.q $6E257D716F257D71,$68257D7169257D71,$6A257D716B257D71,$7D7D7D7D57506674,$3433287331313C3E
   Data.q $7D716D250978757D,$09787D716C250978,$6E2509787D716F25,$7D71692509787D71,$09787D7168250978
   Data.q $6A2509787D716B25,$121019191C7D7174,$251978757D710D19,$716C2519787D716D,$787D716F2519787D
   Data.q $2519787D716E2519,$71682519787D7169,$787D716B2519787D,$19787D7D716A2519,$6C2519787D716D25
   Data.q $7D716F2519787D71,$19787D716E251978,$682519787D716925,$7D716B2519787D71,$575066746A251978
   Data.q $31313C3E7D7D7D7D,$0978757D34332873,$6C2509787D716D25,$7D716F2509787D71,$09787D716E250978
   Data.q $682509787D716925,$7D716B2509787D71,$1C7D71746A250978,$7D710D1912101919,$787D716D25197875
   Data.q $2519787D716C2519,$716E2519787D716F,$787D71692519787D,$2519787D71682519,$716A2519787D716B
   Data.q $7D716D2509787D7D,$09787D716C250978,$6E2509787D716F25,$7D71692509787D71,$09787D7168250978
   Data.q $6A2509787D716B25,$7D7D7D7D57506674,$72727D7D7D7D5750,$03256E7D607D2E7D,$246F726C7D777D6F
   Data.q $3C3E7D7D7D7D5750,$757D343328733131,$6C2E787D716D2E78,$787D716F2E787D71,$71692E787D716E2E
   Data.q $2E787D71682E787D,$71746A2E787D716B,$0D1912101108107D,$716D250978757D71,$787D716C2509787D
   Data.q $2509787D716F2509,$71692509787D716E,$787D71682509787D,$2509787D716B2509,$716D2E787D7D716A
   Data.q $2E787D716C2E787D,$7D716E2E787D716F,$682E787D71692E78,$787D716B2E787D71,$7D7D575066746A2E
   Data.q $7D7D7D7D57507D7D,$57506F032E7D7272,$31313C3E7D7D7D7D,$1978757D34332873,$6C2E19787D716D2E
   Data.q $7D716F2E19787D71,$19787D716E2E1978,$682E19787D71692E,$7D716B2E19787D71,$107D71746A2E1978
   Data.q $7D710D1912101108,$2E787D716D2E7875,$7D716F2E787D716C,$692E787D716E2E78,$787D71682E787D71
   Data.q $716A2E787D716B2E,$787D716D2E787D7D,$716F2E787D716C2E,$2E787D716E2E787D,$7D71682E787D7169
   Data.q $6A2E787D716B2E78,$7D7D7D7D57506674,$72727D7D7D7D5750,$032E7D607D250F7D,$50252D6F7D707D6F
   Data.q $313C3E7D7D7D7D57,$33757D3433287331,$38337D716D052A38,$2A38337D716C052A,$052A38337D716F05
   Data.q $69052A38337D716E,$7168052A38337D71,$7D716B052A38337D,$7D71746A052A3833,$710D1912101F080E
   Data.q $7D716D2E1978757D,$19787D716C2E1978,$6E2E19787D716F2E,$7D71692E19787D71,$19787D71682E1978
   Data.q $6A2E19787D716B2E,$257D716D257D7D71,$257D716F257D716C,$257D7169257D716E,$257D716B257D7168
   Data.q $7D7D7D575066746A,$33287331313C3E7D,$6D052A3833757D34,$716C052A38337D71,$7D716F052A38337D
   Data.q $337D716E052A3833,$38337D7169052A38,$2A38337D7168052A,$052A38337D716B05,$101F080E7D71746A
   Data.q $3833757D710D1912,$2A38337D716D052A,$052A38337D716C05,$6E052A38337D716F,$7169052A38337D71
   Data.q $7D7168052A38337D,$337D716B052A3833,$257D7D716A052A38,$257D716C257D716D,$257D716E257D716F
   Data.q $257D7168257D7169,$5066746A257D716B,$7D57507D7D7D7D57,$240F7D72727D7D7D,$7D252D752E7D607D
   Data.q $7D707D74252F7D70,$32312E63607D242D,$2E2570257577382D,$7D7D7D2470743028,$3C3E7D7D7D7D5750
   Data.q $757D343328733131,$19787D716D251978,$6F2519787D716C25,$7D716E2519787D71,$19787D7169251978
   Data.q $6B2519787D716825,$71746A2519787D71,$0D1912101F080E7D,$257D716D25757D71,$257D716F257D716C
   Data.q $257D7169257D716E,$257D716B257D7168,$052A38337D7D716A,$6C052A38337D716D,$716F052A38337D71
   Data.q $7D716E052A38337D,$337D7169052A3833,$38337D7168052A38,$2A38337D716B052A,$7D7D575066746A05
   Data.q $287331313C3E7D7D,$042A3833757D3433,$6C042A38337D716D,$716F042A38337D71,$7D716E042A38337D
   Data.q $337D7169042A3833,$38337D7168042A38,$2A38337D716B042A,$1108107D71746A04,$78757D710D191210
   Data.q $716C2E787D716D2E,$2E787D716F2E787D,$7D71692E787D716E,$6B2E787D71682E78,$7D7D716A2E787D71
   Data.q $19787D716D251978,$6F2519787D716C25,$7D716E2519787D71,$19787D7169251978,$6B2519787D716825
   Data.q $66746A2519787D71,$3C3E7D7D7D7D5750,$757D343328733131,$337D716D042A3833,$38337D716C042A38
   Data.q $2A38337D716F042A,$042A38337D716E04,$68042A38337D7169,$716B042A38337D71,$71746A042A38337D
   Data.q $0D1912101F080E7D,$6D042A3833757D71,$716C042A38337D71,$7D716F042A38337D,$337D716E042A3833
   Data.q $38337D7169042A38,$2A38337D7168042A,$042A38337D716B04,$7D716D247D7D716A,$7D716F247D716C24
   Data.q $7D7169247D716E24,$7D716B247D716824,$7D7D575066746A24,$33287329382F7D7D,$5750207D57506634
   Data.q $33283B737D57507D,$7D3A382F73757D3E,$2A38337D6F6E3F73,$3A382F737D716D05,$38337D6F6E3F737D
   Data.q $382F737D716C052A,$337D6F6E3F737D3A,$2F737D716F052A38,$7D6F6E3F737D3A38,$737D716E052A3833
   Data.q $6F6E3F737D3A382F,$7D7169052A38337D,$6E3F737D3A382F73,$7168052A38337D6F,$3F737D3A382F737D
   Data.q $6B052A38337D6F6E,$737D3A382F737D71,$052A38337D6F6E3F,$3A382F737D7D716A,$38337D6F6E3F737D
   Data.q $382F737D716D042A,$337D6F6E3F737D3A,$2F737D716C042A38,$7D6F6E3F737D3A38,$737D716F042A3833
   Data.q $6F6E3F737D3A382F,$7D716E042A38337D,$6E3F737D3A382F73,$7169042A38337D6F,$3F737D3A382F737D
   Data.q $68042A38337D6F6E,$737D3A382F737D71,$042A38337D6F6E3F,$7D3A382F737D716B,$2A38337D6F6E3F73
   Data.q $0D19191C7D746A04,$7D3A382F73757D09,$0D1C027D696B3F73,$3A382F737D712F29,$05027D696B3F737D
   Data.q $382F737D712F290D,$027D696B3F737D3A,$382F73712F290D04,$027D6F6E3F737D3A,$267D575074253934
   Data.q $2F737D7D7D7D5750,$7D6F6E3F737D3A38,$7D716365611C2578,$7D716365611C2478,$7D716365611F2578
   Data.q $7D716365611F2478,$636561382E342F78,$666365612E787D71,$737D7D7D7D57507D,$737D7D7D7D3A382F
   Data.q $662D7D7D39382F2D,$2F737D7D7D7D5750,$7D6F6E3F737D3A38,$7D7D7D5750662C38,$7D7D57507D7D7D7D
   Data.q $57507D7D7D7D7D7D,$323172727D7D7D7D,$257D711F257D393C,$2E293334322D7D1F,$1F027D30322F3B7D
   Data.q $7D7D7D57502F290D,$33287331313C3E7D,$716D1F2578757D34,$787D716C1F25787D,$1F25787D716F1F25
   Data.q $71691F25787D716E,$787D71681F25787D,$1F25787D716B1F25,$393C382F7D71746A,$0502757D71293314
   Data.q $393402717D2F290D,$7D7D7D5750667425,$33287331313C3E7D,$716D1F2478757D34,$787D716C1F24787D
   Data.q $1F24787D716F1F24,$71691F24787D716E,$787D71681F24787D,$1F24787D716B1F24,$393C382F7D71746A
   Data.q $0402757D71293314,$393402717D2F290D,$7D7D7D5750667425,$507D7D7D7D57507D,$3172727D7D7D7D57
   Data.q $7D711C257D393C32,$293334322D7D1C25,$027D30322F3B7D2E,$7D7D57502F290D1C,$32313A7339317D7D
   Data.q $2873692B73313C3F,$6D1C2578267D6F6E,$7D716C1C25787D71,$25787D716F1C2578,$0D1C020671206E1C
   Data.q $575066006D762F29,$3A7339317D7D7D7D,$692B73313C3F3231,$2578267D6F6E2873,$681C25787D71691C
   Data.q $7D716B1C25787D71,$020671206A1C2578,$006B6C762F290D1C,$7D7D7D7D57507D66,$3C3F32313A733931
   Data.q $6F6E2873692B7331,$7D716D1C2478267D,$24787D716C1C2478,$6E1C24787D716F1C,$2F290D1C02067120
   Data.q $7D575066006F6E76,$313A7339317D7D7D,$73692B73313C3F32,$1C2478267D6F6E28,$71681C24787D7169
   Data.q $787D716B1C24787D,$1C020671206A1C24,$66006569762F290D,$57507D7D7D7D5750,$31313C3E7D7D7D7D
   Data.q $2C38757D34332873,$1114080C187D7174,$716D1C2578757D71,$787D716C1C25787D,$1C25787D716F1C25
   Data.q $71691C25787D716E,$787D71681C25787D,$1C25787D716B1C25,$6D1F25787D7D716A,$7D716C1F25787D71
   Data.q $25787D716F1F2578,$691F25787D716E1F,$7D71681F25787D71,$25787D716B1F2578,$7D7D7D7D66746A1F
   Data.q $382E7D7D7D7D5750,$6E28732C38732D29,$6C712C38712D7D6F,$1D7D7D7D7D575066,$7D3C2F3F7D7D2D7C
   Data.q $6C1102090D19191C,$7D7D7D7D57507D66,$3B3472727D7D7D7D,$676F2560606C257D,$7D7D7D7D7D7D5750
   Data.q $757D31313C3E7D7D,$337D716D052A3833,$38337D716C052A38,$2A38337D716F052A,$052A38337D716E05
   Data.q $68052A38337D7169,$716B052A38337D71,$7D716A052A38337D,$7D716D042A38337D,$337D716C042A3833
   Data.q $38337D716F042A38,$2A38337D716E042A,$042A38337D716904,$6B042A38337D7168,$746A042A38337D71
   Data.q $71090D111F197D71,$7D716D1C2578757D,$25787D716C1C2578,$6E1C25787D716F1C,$7D71691C25787D71
   Data.q $25787D71681C2578,$6A1C25787D716B1C,$716D1C24787D7D71,$787D716C1C24787D,$1C24787D716F1C24
   Data.q $71691C24787D716E,$787D71681C24787D,$1C24787D716B1C24,$7D7D7D575066746A,$3C2F3F7D7D7D7D7D
   Data.q $1102090D19191C7D,$1C57506609140518,$676C1102090D1919,$72727D7D7D7D5750,$382B33347D29383A
   Data.q $382B33347D382E2F,$25706C2575382E2F,$7D7D5750742D716F,$287331313C3E7D7D,$716D2E78757D3433
   Data.q $2E787D716C2E787D,$7D716E2E787D716F,$682E787D71692E78,$787D716B2E787D71,$1F080E7D71746A2E
   Data.q $78757D710D191210,$1C25787D716D1C25,$716F1C25787D716C,$787D716E1C25787D,$1C25787D71691C25
   Data.q $716B1C25787D7168,$7D7D716A1C25787D,$25787D716D1F2578,$6F1F25787D716C1F,$7D716E1F25787D71
   Data.q $25787D71691F2578,$6B1F25787D71681F,$66746A1F25787D71,$3C3E7D7D7D7D5750,$757D343328733131
   Data.q $6C2E787D716D2E78,$787D716F2E787D71,$71692E787D716E2E,$2E787D71682E787D,$71746A2E787D716B
   Data.q $0D1912100B13147D,$7D716D2E78757D71,$6F2E787D716C2E78,$787D716E2E787D71,$71682E787D71692E
   Data.q $2E787D716B2E787D,$7D7D7D575066746A,$727D7D7D7D57507D,$7560382D32312E72,$3477746F24706C24
   Data.q $2575382E2F382B33,$50742D716F25706C,$313C3E7D7D7D7D57,$78757D3433287331,$787D716D382E342F
   Data.q $787D716C382E342F,$787D716F382E342F,$787D716E382E342F,$787D7169382E342F,$787D7168382E342F
   Data.q $787D716B382E342F,$7D71746A382E342F,$710D1912101F080E,$7D716D1C2478757D,$24787D716C1C2478
   Data.q $6E1C24787D716F1C,$7D71691C24787D71,$24787D71681C2478,$6A1C24787D716B1C,$716D1F24787D7D71
   Data.q $787D716C1F24787D,$1F24787D716F1F24,$71691F24787D716E,$787D71681F24787D,$1F24787D716B1F24
   Data.q $7D7D7D575066746A,$33287331313C3E7D,$7D716D2E78757D34,$6F2E787D716C2E78,$787D716E2E787D71
   Data.q $71682E787D71692E,$2E787D716B2E787D,$101108107D71746A,$2F78757D710D1912,$2F787D716D382E34
   Data.q $2F787D716C382E34,$2F787D716F382E34,$2F787D716E382E34,$2F787D7169382E34,$2F787D7168382E34
   Data.q $2F787D716B382E34,$787D7D716A382E34,$716C2E787D716D2E,$2E787D716F2E787D,$7D71692E787D716E
   Data.q $6B2E787D71682E78,$5066746A2E787D71,$727D7D7D7D575057,$607D250F7D727272,$1A7D707D6F032E7D
   Data.q $607D250C7D707D25,$30022A322D7D7D63,$382D32312E753932,$257570742D716F71,$7D5750746F25766C
   Data.q $7331313C3E7D7D7D,$342F78757D343328,$342F787D716D382E,$342F787D716C382E,$342F787D716F382E
   Data.q $342F787D716E382E,$342F787D7169382E,$342F787D7168382E,$342F787D716B382E,$08107D71746A382E
   Data.q $757D710D19121011,$6C2E787D716D2E78,$787D716F2E787D71,$71692E787D716E2E,$2E787D71682E787D
   Data.q $7D716A2E787D716B,$2E787D716D2E787D,$7D716F2E787D716C,$692E787D716E2E78,$787D71682E787D71
   Data.q $746A2E787D716B2E,$3E7D7D7D7D575066,$7D3433287331313C,$716D382E342F7875,$716C382E342F787D
   Data.q $716F382E342F787D,$716E382E342F787D,$7169382E342F787D,$7168382E342F787D,$716B382E342F787D
   Data.q $746A382E342F787D,$1912101F080E7D71,$2E342F78757D710D,$2E342F787D716D38,$2E342F787D716C38
   Data.q $2E342F787D716F38,$2E342F787D716E38,$2E342F787D716938,$2E342F787D716838,$2E342F787D716B38
   Data.q $1C25787D7D716A38,$716C1C25787D716D,$787D716F1C25787D,$1C25787D716E1C25,$71681C25787D7169
   Data.q $787D716B1C25787D,$7D575066746A1C25,$7331313C3E7D7D7D,$2A3833757D343328,$052A38337D716D05
   Data.q $6F052A38337D716C,$716E052A38337D71,$7D7169052A38337D,$337D7168052A3833,$38337D716B052A38
   Data.q $080E7D71746A052A,$757D710D1912101F,$7D716D382E342F78,$7D716C382E342F78,$7D716F382E342F78
   Data.q $7D716E382E342F78,$7D7169382E342F78,$7D7168382E342F78,$7D716B382E342F78,$7D716A382E342F78
   Data.q $787D716D1F25787D,$1F25787D716C1F25,$716E1F25787D716F,$787D71691F25787D,$1F25787D71681F25
   Data.q $746A1F25787D716B,$507D7D7D7D575066,$7D72727D7D7D7D57,$2D752E7D607D240F,$7D74252F7D707D25
   Data.q $7D63607D242D7D70,$257577382D32312E,$707430282E25706C,$57507D7D7D7D6C24,$31313C3E7D7D7D7D
   Data.q $2F78757D34332873,$2F787D716D382E34,$2F787D716C382E34,$2F787D716F382E34,$2F787D716E382E34
   Data.q $2F787D7169382E34,$2F787D7168382E34,$2F787D716B382E34,$0E7D71746A382E34,$7D710D1912101F08
   Data.q $787D716D1C257875,$1C25787D716C1C25,$716E1C25787D716F,$787D71691C25787D,$1C25787D71681C25
   Data.q $716A1C25787D716B,$716D052A38337D7D,$7D716C052A38337D,$337D716F052A3833,$38337D716E052A38
   Data.q $2A38337D7169052A,$052A38337D716805,$6A052A38337D716B,$7D7D7D7D57506674,$3433287331313C3E
   Data.q $6D382E342F78757D,$6C382E342F787D71,$6F382E342F787D71,$6E382E342F787D71,$69382E342F787D71
   Data.q $68382E342F787D71,$6B382E342F787D71,$6A382E342F787D71,$12101108107D7174,$6D2E78757D710D19
   Data.q $787D716C2E787D71,$716E2E787D716F2E,$2E787D71692E787D,$7D716B2E787D7168,$2F787D7D716A2E78
   Data.q $2F787D716D382E34,$2F787D716C382E34,$2F787D716F382E34,$2F787D716E382E34,$2F787D7169382E34
   Data.q $2F787D7168382E34,$2F787D716B382E34,$575066746A382E34,$31313C3E7D7D7D7D,$3833757D34332873
   Data.q $2A38337D716D042A,$042A38337D716C04,$6E042A38337D716F,$7169042A38337D71,$7D7168042A38337D
   Data.q $337D716B042A3833,$0E7D71746A042A38,$7D710D1912101F08,$716D382E342F7875,$716C382E342F787D
   Data.q $716F382E342F787D,$716E382E342F787D,$7169382E342F787D,$7168382E342F787D,$716B382E342F787D
   Data.q $716A382E342F787D,$7D716D1C24787D7D,$24787D716C1C2478,$6E1C24787D716F1C,$7D71691C24787D71
   Data.q $24787D71681C2478,$6A1C24787D716B1C,$7D7D7D7D57506674,$02090D19191C5750,$507D670914051811
   Data.q $29382F7D7D7D7D57,$7D57506634332873,$7D57505750575020,$73757D3E33283B73,$6F6E3F737D3A382F
   Data.q $7D716D052A38337D,$6E3F737D3A382F73,$716C052A38337D6F,$3F737D3A382F737D,$6F052A38337D6F6E
   Data.q $737D3A382F737D71,$052A38337D6F6E3F,$7D3A382F737D716E,$2A38337D6F6E3F73,$3A382F737D716905
   Data.q $38337D6F6E3F737D,$382F737D7168052A,$337D6F6E3F737D3A,$2F737D716B052A38,$7D6F6E3F737D3A38
   Data.q $7D7D716A052A3833,$6E3F737D3A382F73,$716D042A38337D6F,$3F737D3A382F737D,$6C042A38337D6F6E
   Data.q $737D3A382F737D71,$042A38337D6F6E3F,$7D3A382F737D716F,$2A38337D6F6E3F73,$3A382F737D716E04
   Data.q $38337D6F6E3F737D,$382F737D7169042A,$337D6F6E3F737D3A,$2F737D7168042A38,$7D6F6E3F737D3A38
   Data.q $737D716B042A3833,$6F6E3F737D3A382F,$7D746A042A38337D,$2E3829090D19191C,$7D3A382F73757D29
   Data.q $0D1C027D696B3F73,$3A382F737D712F29,$05027D696B3F737D,$382F737D712F290D,$027D696B3F737D3A
   Data.q $2F737D712F290D04,$7D696B3F737D3A38,$2F290D3B3B341902,$737D3A382F737D71,$253934027D6F6E3F
   Data.q $7D5750267D575074,$7D3A382F737D7D7D,$1C25787D6F6E3F73,$1C24787D71636561,$1F25787D71636561
   Data.q $1F24787D71636561,$342F787D71636561,$787D71636561382E,$57507D666365612E,$3A382F737D7D7D7D
   Data.q $382F2D737D7D7D7D,$7D5750662D7D7D39,$7D3A382F737D7D7D,$662C387D6F6E3F73,$7D7D7D7D7D7D5750
   Data.q $7D7D7D7D7D57507D,$7D7D7D57507D7D7D,$7D393C323172727D,$2D7D1F257D711F25,$2F3B7D2E29333432
   Data.q $2F290D1F027D3032,$3C3E7D7D7D7D5750,$757D343328733131,$337D716D052A3833,$38337D716C052A38
   Data.q $2A38337D716F052A,$052A38337D716E05,$68052A38337D7169,$716B052A38337D71,$71746A052A38337D
   Data.q $293314393C382F7D,$2F290D0502757D71,$667425393402717D,$3C3E7D7D7D7D5750,$757D343328733131
   Data.q $337D716D042A3833,$38337D716C042A38,$2A38337D716F042A,$042A38337D716E04,$68042A38337D7169
   Data.q $716B042A38337D71,$71746A042A38337D,$293314393C382F7D,$2F290D0402757D71,$667425393402717D
   Data.q $57507D7D7D7D5750,$7D7D57507D7D7D7D,$7D7D7D7D57507D7D,$663433287329382F,$507D5750207D5750
   Data.q $7D3E33283B737D57,$3F737D3A382F7375,$716D2B33347D6F6E,$3F737D3A382F737D,$716C2B33347D6F6E
   Data.q $3F737D3A382F737D,$716F2B33347D6F6E,$3F737D3A382F737D,$716E2B33347D6F6E,$3F737D3A382F737D
   Data.q $71692B33347D6F6E,$3F737D3A382F737D,$71682B33347D6F6E,$3F737D3A382F737D,$716B2B33347D6F6E
   Data.q $3F737D3A382F737D,$746A2B33347D6F6E,$3C1F33343A383F7D,$757D39391C353E29,$6B3F737D3A382F73
   Data.q $712F290D1C027D69,$3F737D3A382F737D,$2F290D05027D696B,$737D3A382F737D71,$290D04027D696B3F
   Data.q $7D3A382F737D712F,$3419027D696B3F73,$737D712F290D3B3B,$6F6E3F737D3A382F,$28322F253C10027D
   Data.q $267D5750747D3933,$2F737D7D7D7D5750,$7D6F6E3F737D3A38,$7D716365611C2578,$7D716365611C2478
   Data.q $7D716365611F2578,$507D666365612E78,$382F737D7D7D7D57,$2F2D737D7D7D7D3A,$5750662D7D7D3938
   Data.q $3A382F737D7D7D7D,$34027D6F6E3F737D,$2C382E347D712539,$7D7D7D7D7D575066,$7D7D7D7D57507D7D
   Data.q $727D7D7D7D57507D,$1C257D393C323172,$141C107D1C257D71,$2E293334322D7D13,$1C027D30322F3B7D
   Data.q $7D7D7D57502F290D,$3F32313A7339317D,$6E2873692B73313C,$716D1C2578267D6F,$787D716C1C25787D
   Data.q $1C25787D716F1C25,$290D1C020671206E,$7D575066006D762F,$313A7339317D7D7D,$73692B73313C3F32
   Data.q $1C2578267D6F6E28,$71681C25787D7169,$787D716B1C25787D,$1C020671206A1C25,$66006B6C762F290D
   Data.q $317D7D7D7D57507D,$313C3F32313A7339,$7D6F6E2873692B73,$787D716D1C247826,$1C24787D716C1C24
   Data.q $206E1C24787D716F,$762F290D1C020671,$7D7D575066006F6E,$32313A7339317D7D,$2873692B73313C3F
   Data.q $691C2478267D6F6E,$7D71681C24787D71,$24787D716B1C2478,$0D1C020671206A1C,$5066006569762F29
   Data.q $7D7D7D7D7D7D7D57,$507D7D7D7D57507D,$7D57507D7D7D7D57,$34333472727D7D7D,$2E2F382B33347D29
   Data.q $257D7D7D7D575038,$787D6F6E3F732F32,$716D2E787D716D2E,$7D5750666D2E787D,$3F732F32257D7D7D
   Data.q $7D716C2E787D6F6E,$6C2E787D716C2E78,$257D7D7D7D575066,$787D6F6E3F732F32,$716F2E787D716F2E
   Data.q $7D5750666F2E787D,$3F732F32257D7D7D,$7D716E2E787D6F6E,$6E2E787D716E2E78,$257D7D7D7D575066
   Data.q $787D6F6E3F732F32,$71692E787D71692E,$7D575066692E787D,$3F732F32257D7D7D,$7D71682E787D6F6E
   Data.q $682E787D71682E78,$257D7D7D7D575066,$787D6F6E3F732F32,$716B2E787D716B2E,$7D5750666B2E787D
   Data.q $7D7D7D7D57507D7D,$7D6F6E28732B3230,$6D256D7D716A2E78,$507D7D7D5750666C,$2F32257D7D7D7D57
   Data.q $3934027D6F6E3F73,$71253934027D7125,$575066253934027D,$797D57507D7D7D7D,$293C1F33343A383F
   Data.q $676C1139391C353E,$7D7D7D57507D7D7D,$727D7D7D7D57507D,$1F257D393C323172,$34322D7D1F257D71
   Data.q $30322F3B7D2E2933,$57502F290D1F027D,$31313C3E7D7D7D7D,$2578757D34332873,$6C1F25787D716D1F
   Data.q $7D716F1F25787D71,$25787D716E1F2578,$681F25787D71691F,$7D716B1F25787D71,$2F7D71746A1F2578
   Data.q $7D71293314393C38,$717D2F290D050275,$5750667425393402,$7D7D7D7D7D7D7D7D,$3C3E7D7D7D7D5750
   Data.q $2C382E34757D3131,$1114080C187D7174,$716D1C2578757D71,$787D716C1C25787D,$1C25787D716F1C25
   Data.q $71691C25787D716E,$787D71681C25787D,$1C25787D716B1C25,$6D1F25787D7D716A,$7D716C1F25787D71
   Data.q $25787D716F1F2578,$691F25787D716E1F,$7D71681F25787D71,$25787D716B1F2578,$7D7D575066746A1F
   Data.q $38732D29382E7D7D,$712D7D6F6E28732C,$50666C712C382E34,$2D7C1D7D7D7D7D57,$383F797D3C2F3F7D
   Data.q $353E293C1F33343A,$5750666F1139391C,$7D7D7D7D7D7D7D7D,$60252D7D3B347272,$7D7D7D7D57502560
   Data.q $393C72727D7D7D7D,$242D750D39321039,$74297D71242D7D71,$7D7D7D7D7D7D5750,$287331313C3E7D7D
   Data.q $6D1F2578757D3433,$7D716C1F25787D71,$25787D716F1F2578,$691F25787D716E1F,$7D71681F25787D71
   Data.q $25787D716B1F2578,$19191C7D71746A1F,$78757D710D191210,$1C24787D716D1C24,$716F1C24787D716C
   Data.q $787D716E1C24787D,$1C24787D71691C24,$716B1C24787D7168,$7D7D716A1C24787D,$24787D716D1C2478
   Data.q $6F1C24787D716C1C,$7D716E1C24787D71,$24787D71691C2478,$6B1C24787D71681C,$66746A1C24787D71
   Data.q $7D7D7D7D7D7D5750,$3F797D3C2F3F7D7D,$3E293C1F33343A38,$50666E1139391C35,$7D7D7D7D7D7D7D57
   Data.q $343A383F7957507D,$391C353E293C1F33,$7D57507D676F1139,$3F282E72727D7D7D,$71252D750D393210
   Data.q $506674297D71257D,$313C3E7D7D7D7D57,$78757D3433287331,$1F25787D716D1F25,$716F1F25787D716C
   Data.q $787D716E1F25787D,$1F25787D71691F25,$716B1F25787D7168,$7D71746A1F25787D,$710D1912101F080E
   Data.q $7D716D1C2578757D,$25787D716C1C2578,$6E1C25787D716F1C,$7D71691C25787D71,$25787D71681C2578
   Data.q $6A1C25787D716B1C,$716D1F25787D7D71,$787D716C1F25787D,$1F25787D716F1F25,$71691F25787D716E
   Data.q $787D71681F25787D,$1F25787D716B1F25,$7D7D7D575066746A,$343A383F7957507D,$391C353E293C1F33
   Data.q $7D7D5750676E1139,$72727D7D7D7D5750,$750D393210312830,$2F382B33347D7129,$7D7D57506674382E
   Data.q $287331313C3E7D7D,$716D2E78757D3433,$2E787D716C2E787D,$7D716E2E787D716F,$682E787D71692E78
   Data.q $787D716B2E787D71,$1108107D71746A2E,$78757D710D191210,$716C2E787D716D2E,$2E787D716F2E787D
   Data.q $7D71692E787D716E,$6B2E787D71682E78,$7D7D716A2E787D71,$25787D716D1F2578,$6F1F25787D716C1F
   Data.q $7D716E1F25787D71,$25787D71691F2578,$6B1F25787D71681F,$66746A1F25787D71,$57507D7D7D7D5750
   Data.q $31313C3E7D7D7D7D,$2F2A7D7D34332873,$7D71293314382934,$290D3B3B34190275,$71253934027D712F
   Data.q $2E787D716D2E787D,$7D716F2E787D716C,$692E787D716E2E78,$787D71682E787D71,$746A2E787D716B2E
   Data.q $507D7D7D7D575066,$39393C7D7D7D7D57,$7D7D7D7D6F6E2873,$340271253934027D,$7D5750666C712539
   Data.q $732D29382E7D7D7D,$2D7D6F6E28733231,$1002712539340271,$66393328322F253C,$7D7D7D7D7D7D7D7D
   Data.q $3F7D2D1D5457507D,$797D343328733C2F,$293C1F33343A383F,$666C1139391C353E,$57507D7D7D7D5750
   Data.q $31313C3E7D7D7D7D,$3334757D34332873,$6C2B33347D716D2B,$7D716F2B33347D71,$33347D716E2B3334
   Data.q $682B33347D71692B,$7D716B2B33347D71,$147D71746A2B3334,$7D710D1912100B13,$2E787D716D2E7875
   Data.q $7D716F2E787D716C,$692E787D716E2E78,$787D71682E787D71,$746A2E787D716B2E,$727D7D7D7D575066
   Data.q $33287331313C3E72,$716D2B3334757D34,$347D716C2B33347D,$2B33347D716F2B33,$71692B33347D716E
   Data.q $347D71682B33347D,$2B33347D716B2B33,$040D121E7D71746A,$7D710913141A141F,$2E787D716D2E7875
   Data.q $7D716F2E787D716C,$692E787D716E2E78,$787D71682E787D71,$746A2E787D716B2E,$2F7D7D7D7D575066
   Data.q $5066343328732938,$57507D5750207D57,$757D3E33283B737D,$6E3F737D3A382F73,$18117D742C387D6F
   Data.q $101114080C180E0E,$382F73757D180F12,$3C7D6F6E3F737D3A,$7D3A382F737D716D,$716C3C7D6F6E3F73
   Data.q $3F737D3A382F737D,$3334320D027D696B,$7D74243C2F2F3C29,$7D7D7D5750265750,$7D7D7D3A382F737D
   Data.q $2D7D39382F2D737D,$737D7D7D7D575066,$6F6E3F737D3A382F,$50666369613F787D,$7D57507D7D7D7D57
   Data.q $313A7339317D7D7D,$736F2B73313C3F32,$3F7826547D6F6E28,$71206C3F787D716D,$3C293334320D0206
   Data.q $57506600243C2F2F,$2D29382E7D7D7D7D,$7D6F6E28732C3873,$6D3F78716D3C712D,$1D7D7D7D7D575066
   Data.q $7D3C2F3F7D7D2D7C,$080C180E0E181179,$026D180F12101114,$7D7D5750662C3833,$72727D7D7D7D7D7D
   Data.q $63637D6D3F606D3C,$382833342933323E,$3334363E38353E7D,$7D7D7D7D7D57503A,$732D29382E7D7D7D
   Data.q $2D7D6F6E28732C38,$666C3F78716C3C71,$7D7D7D7D7D7D5750,$3F7D7D2D7C1D7D7D,$0E0E1811797D3C2F
   Data.q $0F12101114080C18,$50662C3833026C18,$7D7D7D7D7D7D7D57,$3C72727D7D7D7D7D,$3863637D6C3F606C
   Data.q $7D7D7D5750293425,$7D7D7D7D7D7D7D7D,$6F6E3F732F32257D,$712C387D712C387D,$6D72727D662C387D
   Data.q $57503134282C3870,$7D7D7D7D7D7D7D7D,$7D3C2F3F7D7D7D7D,$080C180E0E181179,$2538180F12101114
   Data.q $7D7D7D5750662934,$7D7D7D7D7D7D7D7D,$7D7D7D7D7D7D7D7D,$7D7D7D7D7D57507D,$7D7D7D7D7D7D7D7D
   Data.q $7D7D7D7D7D7D7D7D,$7D7D7D7D7D7D7D7D,$7D7D7D7D7D7D7D7D,$7D7D7D7D7D7D7D7D,$507D7D7D7D7D7D7D
   Data.q $7D7D7D7D7D7D7D57,$7D7D7D7D7D7D7D7D,$7D7D7D7D7D7D7D7D,$0E0E18117957507D,$0F12101114080C18
   Data.q $50672C3833026D18,$7D7D7D7D7D7D7D57,$3F63616D3C72727D,$7D7D7D7D7D57506D,$732D29382E7D7D7D
   Data.q $2D7D6F6E28733435,$666D3F78716D3C71,$7D7D7D7D7D7D5750,$28732D31382E7D7D,$6F712C38547D6F6E
   Data.q $7D5750662D716C71,$3F7D7D7D7D7D7D7D,$0E0E1811797D3C2F,$0F12101114080C18,$5750662934253818
   Data.q $180E0E1811795750,$180F12101114080C,$5750672C3833026C,$7D7D7D7D7D7D7D7D,$6C3F63616C3C7272
   Data.q $7D7D7D7D7D7D5750,$35732D29382E7D7D,$712D7D6F6E287334,$50666C3F78716C3C,$7D7D7D7D7D7D7D57
   Data.q $6E28732D31382E7D,$716F712C38547D6F,$7D7D5750662D716C,$57507D7D7D7D7D7D,$507D7D7D7D7D7D7D
   Data.q $7D7D7D7D7D7D7D57,$7D7D7D7D7D7D7D7D,$0E0E18117957507D,$0F12101114080C18,$5750672934253818
   Data.q $6629382F7D7D7D7D,$57507D7D7D7D5750,$5057505750575020,$3E33283B73575057,$737D3A382F73757D
   Data.q $7D742C387D6F6E3F,$0E18111114080C18,$111C15180F12100E,$7D3A382F73757D1B,$716D3C7D6F6E3F73
   Data.q $3F737D3A382F737D,$3334320D027D696B,$7D74243C2F2F3C29,$7D5750267D57507D,$7D3A382F737D7D7D
   Data.q $39382F2D737D7D7D,$7D7D7D5750662D7D,$3F737D3A382F737D,$3B7D716D3B7D6F6E,$5750666F3B7D716C
   Data.q $3A382F737D7D7D7D,$3F787D6F6E3F737D,$7D7D575066636F61,$7D7D7D7D57507D7D,$7D6F6E28732B3230
   Data.q $6D716D3B547D7D7D,$54545454666D6D25,$EF8DED8DDD8C7272,$30545750D68CE08D,$7D7D6F6E28732B32
   Data.q $256D716C3B547D7D,$7254545454666C6D,$8CE08DE88DE18D72,$545750E88DD58CD1,$7D6F6E28732B3230
   Data.q $6D716F3B547D7D7D,$54545454666F6D25,$E68DE38DEC8D7272,$5750E88DD58CD18C,$7D7D57507D7D7D7D
   Data.q $32313A7339317D7D,$7D6F6E2873313C3F,$0D0206716D3F7854,$3C2F2F3C29333432,$7D7D7D5750660024
   Data.q $2E7D7D7D7D57507D,$28733435732D2938,$716D3C712D7D6F6E,$2E545750666D3F78,$7D6F6E28732D3138
   Data.q $3B716F3B716C3B54,$2E545750662D716C,$28732C38732D2938,$716D3C712D7D6F6E,$2E545750666D3F78
   Data.q $7D6F6E28732D3138,$3B716D3B712C3854,$50545750662D716C,$7D57507D7D7D7D57,$287329382F7D7D7D
   Data.q $5750205750663433,$3B73575057505750,$382F73757D3E3328,$387D6F6E3F737D3A,$7D3A382F737D712C
   Data.q $2E322D7D6F6E3F73,$14191308121B7D74,$0A1813090F120E13,$3F737D3A382F7375,$737D716D3C7D6F6E
   Data.q $6F6E3F737D3A382F,$382F737D716C3C7D,$027D696B3F737D3A,$382F737D712F2F3C,$3F7D6F6E3F737D3A
   Data.q $3A333C2F33343A38,$7D3A382F737D7138,$3933387D6F6E3F73,$737D71383A333C2F,$6F6E3F737D3A382F
   Data.q $71362E3C3029357D,$3F737D3A382F737D,$38293429357D696B,$502657507D742E30,$382F737D7D7D7D57
   Data.q $2F2D737D7D7D7D3A,$7D5750662D7D3938,$7D3A382F737D7D7D,$3038297D6F6E3F73,$333C2F393338022D
   Data.q $2D3038297D71383A,$7D712C382E347D71,$50662F382933383E,$382F737D7D7D7D57,$027D696B3F737D3A
   Data.q $2F2F3C293334320D,$2A3833027D71243C,$7D7D5750662F2F1C,$737D3A382F737D7D,$2E3B3B327D6F6E3F
   Data.q $66272E7D717D2938,$57507D7D7D7D5750,$7D7D57507D7D7D7D,$507D7D7D57507D7D,$7D57507D7D7D7D57
   Data.q $3F7339333C7D7D7D,$2E547D7D7D7D6F6E,$357D716C3C7D7127,$575066362E3C3029,$73292B3E7D7D7D7D
   Data.q $7D6F6E2873696B28,$3C293334320D027D,$272E7D71243C2F2F,$307D7D7D7D575066,$6B2873323173393C
   Data.q $293334320D027D69,$0D0271243C2F2F3C,$3C2F2F3C29333432,$2F2F3C0271657124,$317D7D7D7D575066
   Data.q $313C3F32313A7339,$7D6F6E28736F2B73,$3B3B3271272E2654,$020671207D29382E,$2F2F3C293334320D
   Data.q $3172727D6600243C,$3827342E7D293B38,$7D29353A342F7D71,$575029382E3B3B32,$7D7D57507D7D7D7D
   Data.q $6E28732B32307D7D,$3C2F3933387D7D6F,$7D66272E71383A33,$7D7D7D7D7D7D7D7D,$32307D7D7D7D5750
   Data.q $297D7D6F6E28732B,$2F393338022D3038,$39333871383A333C,$575066383A333C2F,$7D57507D7D7D7D7D
   Data.q $2873292B3E7D7D7D,$7D7D6F6E2873696B,$2F3C293334320D02,$2E3B3B3271243C2F,$57507D7D7D662938
   Data.q $7331352E7D7D7D7D,$34320D027D696B3F,$71243C2F2F3C2933,$2F3C293334320D02,$7D7D666F71243C2F
   Data.q $7D7D7D7D57507D7D,$7D696B287339393C,$2F3C293334320D02,$34320D0271243C2F,$71243C2F2F3C2933
   Data.q $662E303829342935,$393C7D7D7D7D5750,$33027D696B287339,$0D02712F2F1C2A38,$3C2F2F3C29333432
   Data.q $50662F2F3C027124,$7D57507D7D7D7D57,$507D7D7D7D7D7D7D,$08121B2A38337957,$090F120E13141913
   Data.q $7D7D7D6733343C30,$2E7D7D7D7D57507D,$28733231732D2938,$3A383F712D7D6F6E,$71383A333C2F3334
   Data.q $383A333C2F393338,$1D7D7D7D7D575066,$7D3C2F3F7D7D2D7C,$121B11112A383379,$0F120E1314191308
   Data.q $382F2D2934253809,$7D7D7D7D7D575066,$343C3072727D7D7D,$57502D3232317D33,$7D7D7D7D7D7D7D7D
   Data.q $7D6F6E287339393C,$38712F382933383E,$71383A333C2F3933,$333C2F33343A383F,$7D7D7D575066383A
   Data.q $2F352E7D7D7D7D7D,$383E7D7D6F6E3F73,$33383E712F382933,$5750666C712F3829,$7D7D7D7D7D7D7D7D
   Data.q $3839342A73312830,$320D027D6F6E2873,$243C2F2F3C293334,$712F382933383E71,$7D7D7D57507D6669
   Data.q $39393C7D7D7D7D7D,$320D027D696B2873,$243C2F2F3C293334,$3C293334320D0271,$38330271243C2F2F
   Data.q $7D5750662F2F1C2A,$727D7D7D7D7D7D7D,$34757D31313C3E72,$18117D71742C382E,$101114080C180E0E
   Data.q $6D3C757D71180F12,$0D027D716C3C7D71,$3C2F2F3C29333432,$7D7D7D5750667424,$313C3E7D7D7D7D7D
   Data.q $742C382E34757D31,$111114080C187D71,$15180F12100E0E18,$6D3C757D711B111C,$293334320D027D71
   Data.q $506674243C2F2F3C,$7D7D7D7D7D7D7D57,$2C38732D29382E7D,$34712D7D6F6E2873,$727D666F712C382E
   Data.q $7D5750382F323072,$1D7D7D7D7D7D7D7D,$33797D3C2F3F7D2D,$14191308121B2A38,$323002090F120E13
   Data.q $7D7D7D575066382F,$7D7D7D7D7D7D7D7D,$2C38732D29382E7D,$34712D7D6F6E2873,$727D666C712C382E
   Data.q $7D57502E2E383172,$7D7D7D7D7D7D7D7D,$2F3F7D2D1D7D7D7D,$121B2A3833797D3C,$0F120E1314191308
   Data.q $50662E2E38310209,$7D7D7D7D7D7D7D57,$7D7D7D7D7D7D7D7D,$3134282C3872727D,$7D7D7D7D7D7D5750
   Data.q $7D7D7D7D7D7D7D7D,$6E28732B32307D7D,$666C712C387D7D6F,$7D7D7D7D7D7D5750,$7D7D7D7D7D7D7D7D
   Data.q $6E28732B32307D7D,$3E712E322D7D7D6F,$7D7D662F38293338,$7D7D7D7D7D57507D,$7D7D7D7D7D7D7D7D
   Data.q $797D3C2F3F7D7D7D,$191308121B2A3833,$2538090F120E1314,$7D7D7D5750662934,$7D7D7D7D7D7D7D7D
   Data.q $7957507D7D7D7D7D,$191308121B2A3833,$3002090F120E1314,$7D7D7D7D67382F32,$393C7D7D7D7D5750
   Data.q $38297D6F6E287339,$382933383E712D30,$7D7D5750666C712F,$31732D29382E7D7D,$712D7D6F6E28732E
   Data.q $393338712D303829,$727D66383A333C2F,$2F382933383E7572,$3933386061746C76,$3E7D62383A333C2F
   Data.q $676C762F38293338,$383A333C2F393338,$382E7D7D7D7D5750,$547D6F6E28732D31,$333C2F33343A383F
   Data.q $712D30382971383A,$383A333C2F393338,$507D7D7D7D662D71,$3C2F3F7D7D7D7D57,$08121B2A3833797D
   Data.q $090F120E13141913,$7D57506633343C30,$38337957507D7D7D,$1314191308121B2A,$2E383102090F120E
   Data.q $7D7D7D7D5750672E,$7D6F6E28732B3230,$3A333C2F3933387D,$2F382933383E7138,$3F7D7D7D7D575066
   Data.q $1B2A3833797D3C2F,$120E131419130812,$506633343C30090F,$7957507D7D7D7D57,$08121B11112A3833
   Data.q $090F120E13141913,$67382F2D29342538,$72727D7D7D7D5750,$322F3B7D29342538,$57502D3232317D30
   Data.q $732B32307D7D7D7D,$712C387D7D6F6E28,$7D7D7D7D5750666D,$7D6F6E28732B3230,$50666D712E322D7D
   Data.q $7957507D7D7D7D57,$191308121B2A3833,$2538090F120E1314,$507D7D7D7D672934,$29382F7D7D7D7D57
   Data.q $5750575020575066,$33283B7357505750,$312D30323E7D7D3E,$353E293C1F382938,$193529340A39391C
   Data.q $73757D38313F2832,$696B3F737D3A382F,$7D712F290D1C027D,$6B3F737D3A382F73,$712F290D05027D69
   Data.q $3F737D3A382F737D,$2F290D04027D696B,$737D3A382F737D71,$3B3419027D696B3F,$2F737D712F290D3B
   Data.q $7D6F6E3F737D3A38,$3328322F253C1002,$7D3A382F737D7139,$2B33347D6F6E3F73,$7D3A382F737D716D
   Data.q $2B33347D6F6E3F73,$7D3A382F737D716C,$2B33347D6F6E3F73,$7D3A382F737D716F,$2B33347D6F6E3F73
   Data.q $7D3A382F737D716E,$2B33347D6F6E3F73,$7D3A382F737D7169,$2B33347D6F6E3F73,$7D3A382F737D7168
   Data.q $2B33347D6F6E3F73,$7D3A382F737D716B,$2B33347D6F6E3F73,$50267D5750747D6A,$382F737D7D7D7D57
   Data.q $787D6F6E3F737D3A,$787D716365611C25,$787D716365611C24,$787D716365611F25,$787D716365611F24
   Data.q $39787D716365612E,$7D716365613B3B34,$612B33342F283E78,$2E342F787D716365,$3E787D7163656138
   Data.q $57507D6663656125,$3A382F737D7D7D7D,$382F2D737D7D7D7D,$7D5750662D7D7D39,$7D3A382F737D7D7D
   Data.q $253C107D6F6E3F73,$382E347D713E3314,$3C382F35297D712C,$5750666F6E391439,$3A382F737D7D7D7D
   Data.q $280D7D696B3F737D,$0D027D712F290D3F,$392F787D712F292D,$712F292D107D716D,$293409157D71297D
   Data.q $507D7D7D662E3038,$382F737D7D7D7D57,$2D7D6F6E3F737D3A,$38293328323E3F28,$507D7D7D7D66092F
   Data.q $382F737D7D7D7D57,$7D7D6B6C3F737D3A,$14393C382F35297D,$382F35297D712539,$3F7D71303419393C
   Data.q $66253914363E3231,$7D7D7D7D57507D7D,$6E3F737D3A382F73,$716D052A38337D6F,$716C052A38337D7D
   Data.q $716F052A38337D7D,$716E052A38337D7D,$7169052A38337D7D,$7168052A38337D7D,$716B052A38337D7D
   Data.q $716A052A38337D7D,$66362E3C3009157D,$57507D7D7D7D5750,$732B32307D7D7D7D,$32313F7D7D6B6C28
   Data.q $3E7871253914363E,$5466257339343C29,$28732B3230545750,$382F35297D7D6B6C,$337871303419393C
   Data.q $5750662573393429,$6B6C28732B323054,$393C382F35297D7D,$3934297871253914,$7D7D7D5750662573
   Data.q $507D7D7D7D57507D,$292B3E7D7D7D7D57,$6B6C28736F6E2873,$393C382F35297D7D,$2F3529716F6E3914
   Data.q $5466253914393C38,$2A73312830545750,$7D6B6C2873383934,$3F713E3314253C10,$71253914363E3231
   Data.q $3419393C382F3529,$5457507D7D7D6630,$7D6F6E287339393C,$3914393C382F3529,$3314253C10716F6E
   Data.q $393C382F3529713E,$7D5750666F6E3914,$7D7D7D57507D7D7D,$507D7D7D7D57507D,$3172727D7D7D7D57
   Data.q $3E3F282D7D393C32,$57502F3829332832,$7D7D57507D7D7D7D,$6B3F732B32307D7D,$7D716D392F787D69
   Data.q $5750662F290D0502,$733F282E7D7D7D7D,$6D392F787D696B28,$7D716D392F787D71,$7D57507D66656F6C
   Data.q $7D7D7D57507D7D7D,$317D7D7D7D57507D,$313C3F32313A7339,$253C107D6F6E2873,$392F7806713E3314
   Data.q $727D66006F6C766D,$322D313C29322972,$27342E7D2E293334,$7D7D7D7D57507D38,$39317D7D7D7D5750
   Data.q $73313C3F32313A73,$292D0D027D696B28,$766D392F7806712F,$282D727266006B64,$7D29382E3B3B323F
   Data.q $7D7D7D57507D7D7D,$696B287339393C7D,$2D0D027D7D7D7D7D,$716D392F78712F29,$5750662F292D0D02
   Data.q $7D7D57507D7D7D7D,$32313A7339317D7D,$7D6F6E2873313C3F,$20362E3C30091526,$6C766D392F780671
   Data.q $7D7D575066006F6C,$32313A7339317D7D,$7D696B2873313C3F,$2E30382934091526,$766D392F78067120
   Data.q $7D7D7D6600696D6C,$317D7D7D7D57507D,$313C3F32313A7339,$3F282D7D6F6E2873,$092F38293328323E
   Data.q $69766D392F780671,$7D7D7D7D57506600,$57507D7D7D7D5750,$323172727D7D7D7D,$257D711C257D393C
   Data.q $2D7D13141C107D1C,$2F3B7D2E29333432,$2F290D1C027D3032,$39317D7D7D7D5750,$73313C3F32313A73
   Data.q $267D6F6E2873692B,$25787D716D1C2578,$6F1C25787D716C1C,$71206E1C25787D71,$6D762F290D1C0206
   Data.q $7D7D7D7D57506600,$3C3F32313A733931,$6F6E2873692B7331,$7D71691C2578267D,$25787D71681C2578
   Data.q $6A1C25787D716B1C,$2F290D1C02067120,$57507D66006B6C76,$3A7339317D7D7D7D,$692B73313C3F3231
   Data.q $2478267D6F6E2873,$6C1C24787D716D1C,$7D716F1C24787D71,$020671206E1C2478,$006F6E762F290D1C
   Data.q $317D7D7D7D575066,$313C3F32313A7339,$7D6F6E2873692B73,$787D71691C247826,$1C24787D71681C24
   Data.q $206A1C24787D716B,$762F290D1C020671,$7D7D575066006569,$7D7D7D7D57507D7D,$57507D7D7D7D5750
   Data.q $7D7D7D7D57507D7D,$57507D7D7D7D5750,$7D7D57507D7D7D7D,$287331313C3E7D7D,$2F283E78757D3433
   Data.q $3E787D716D2B3334,$7D716C2B33342F28,$6F2B33342F283E78,$33342F283E787D71,$2F283E787D716E2B
   Data.q $3E787D71692B3334,$7D71682B33342F28,$6B2B33342F283E78,$33342F283E787D71,$0D121E7D71746A2B
   Data.q $710913141A141F04,$7D716D2B3334757D,$33347D716C2B3334,$6E2B33347D716F2B,$7D71692B33347D71
   Data.q $33347D71682B3334,$6A2B33347D716B2B,$57507D7D7D7D6674,$7D7D57507D7D7D7D,$3231733128307D7D
   Data.q $2F35297D6F6E2873,$716F6E3914393C38,$14393C382F35297D,$3C10027D716F6E39,$5066393328322F25
   Data.q $3F282E7D7D7D7D57,$3C10027D6F6E2873,$7D71393328322F25,$3328322F253C1002,$7D7D7D666C7D7139
   Data.q $507D7D7D7D57507D,$3F282E7D7D7D7D57,$253C107D6F6E2873,$3C10027D713E3314,$7D71393328322F25
   Data.q $7D7D7D7D5750666C,$57507D7D7D7D5750,$7D57507D7D7D7D7D,$7D7D7D57507D7D7D,$727D7D7D7D57507D
   Data.q $1F257D393C323172,$34322D7D1F257D71,$30322F3B7D2E2933,$57502F290D05027D,$31313C3E7D7D7D7D
   Data.q $2578757D34332873,$6C1F25787D716D1F,$7D716F1F25787D71,$25787D716E1F2578,$681F25787D71691F
   Data.q $7D716B1F25787D71,$2F7D71746A1F2578,$7D71293314393C38,$717D2F290D050275,$3328322F253C1002
   Data.q $7D7D7D5750667439,$7D393C323172727D,$2D7D1F247D711F24,$2F3B7D2E29333432,$2F290D04027D3032
   Data.q $3C3E7D7D7D7D5750,$757D343328733131,$24787D716D1F2478,$6F1F24787D716C1F,$7D716E1F24787D71
   Data.q $24787D71691F2478,$6B1F24787D71681F,$71746A1F24787D71,$293314393C382F7D,$2F290D0402757D71
   Data.q $322F253C1002717D,$7D57506674393328,$7D7D7D57507D7D7D,$7D393C323172727D,$293C3F7D3B3B3439
   Data.q $57507D6C707D353E,$31313C3E7D7D7D7D,$3978757D34332873,$39787D716D3B3B34,$39787D716C3B3B34
   Data.q $39787D716F3B3B34,$39787D716E3B3B34,$39787D71693B3B34,$39787D71683B3B34,$39787D716B3B3B34
   Data.q $2F7D71746A3B3B34,$7D71293314393C38,$290D3B3B34190275,$3314253C10717D2F,$507D7D7D7D66743E
   Data.q $313C3E7D7D7D7D57,$78757D3433287331,$716C2E787D716D2E,$2E787D716F2E787D,$7D71692E787D716E
   Data.q $6B2E787D71682E78,$7D71746A2E787D71,$710D191210110810,$33342F283E78757D,$2F283E787D716D2B
   Data.q $3E787D716C2B3334,$7D716F2B33342F28,$6E2B33342F283E78,$33342F283E787D71,$2F283E787D71692B
   Data.q $3E787D71682B3334,$7D716B2B33342F28,$6A2B33342F283E78,$3B3B3439787D7D71,$3B3B3439787D716D
   Data.q $3B3B3439787D716C,$3B3B3439787D716F,$3B3B3439787D716E,$3B3B3439787D7169,$3B3B3439787D7168
   Data.q $3B3B3439787D716B,$7D7D7D575066746A,$727D7D7D7D57507D,$307D38293C392872,$382B33347D33343C
   Data.q $7D7D7D5750382E2F,$33287331313C3E7D,$742C382E34757D34,$711114080C187D71,$7D716D1C2578757D
   Data.q $25787D716C1C2578,$6E1C25787D716F1C,$7D71691C25787D71,$25787D71681C2578,$6A1C25787D716B1C
   Data.q $716D1F25787D7D71,$787D716C1F25787D,$1F25787D716F1F25,$71691F25787D716E,$787D71681F25787D
   Data.q $1F25787D716B1F25,$7D7D7D575066746A,$727D7D7D7D57507D,$6060252D7D3B3472,$3E7D7D7D7D575025
   Data.q $7D3433287331313C,$6D3B3B3439787D75,$6C3B3B3439787D71,$6F3B3B3439787D71,$6E3B3B3439787D71
   Data.q $693B3B3439787D71,$683B3B3439787D71,$6B3B3B3439787D71,$6A3B3B3439787D71,$121019191C7D7174
   Data.q $1C2478757D710D19,$716C1C24787D716D,$787D716F1C24787D,$1C24787D716E1C24,$71681C24787D7169
   Data.q $787D716B1C24787D,$24787D7D716A1C24,$6C1C24787D716D1C,$7D716F1C24787D71,$24787D716E1C2478
   Data.q $681C24787D71691C,$7D716B1C24787D71,$575066746A1C2478,$3B3472727D7D7D7D,$575025607C252D7D
   Data.q $31313C3E7D7D7D7D,$787D757D34332873,$787D716D382E342F,$787D716C382E342F,$787D716F382E342F
   Data.q $787D716E382E342F,$787D7169382E342F,$787D7168382E342F,$787D716B382E342F,$7D71746A382E342F
   Data.q $710D1912101F080E,$7D716D1C2578757D,$25787D716C1C2578,$6E1C25787D716F1C,$7D71691C25787D71
   Data.q $25787D71681C2578,$6A1C25787D716B1C,$716D1F25787D7D71,$787D716C1F25787D,$1F25787D716F1F25
   Data.q $71691F25787D716E,$787D71681F25787D,$1F25787D716B1F25,$7D7D7D575066746A,$2C38732D29382E7D
   Data.q $34712D7D6F6E2873,$5750666C712C382E,$2D31382E7D7D7D7D,$3978547D6F6E2873,$343978716D3B3B34
   Data.q $2E342F78716D3B3B,$7D5750662D716D38,$732D31382E7D7D7D,$343978547D6F6E28,$3B343978716C3B3B
   Data.q $382E342F78716C3B,$7D7D5750662D716C,$28732D31382E7D7D,$3B343978547D6F6E,$3B3B343978716F3B
   Data.q $6F382E342F78716F,$7D7D7D5750662D71,$6E28732D31382E7D,$3B3B343978547D6F,$6E3B3B343978716E
   Data.q $716E382E342F7871,$7D7D7D7D5750662D,$6F6E28732D31382E,$693B3B343978547D,$71693B3B34397871
   Data.q $2D7169382E342F78,$2E7D7D7D7D575066,$7D6F6E28732D3138,$71683B3B34397854,$7871683B3B343978
   Data.q $662D7168382E342F,$382E7D7D7D7D5750,$547D6F6E28732D31,$78716B3B3B343978,$2F78716B3B3B3439
   Data.q $50662D716B382E34,$31382E7D7D7D7D57,$78547D6F6E28732D,$3978716A3B3B3439,$342F78716A3B3B34
   Data.q $5750662D716A382E,$507D7D7D7D7D7D7D,$313C3E7D7D7D7D57,$78757D3433287331,$716D2B33342F283E
   Data.q $2B33342F283E787D,$342F283E787D716C,$283E787D716F2B33,$787D716E2B33342F,$71692B33342F283E
   Data.q $2B33342F283E787D,$342F283E787D7168,$283E787D716B2B33,$7D71746A2B33342F,$710D191210110810
   Data.q $33342F283E78757D,$2F283E787D716D2B,$3E787D716C2B3334,$7D716F2B33342F28,$6E2B33342F283E78
   Data.q $33342F283E787D71,$2F283E787D71692B,$3E787D71682B3334,$7D716B2B33342F28,$6A2B33342F283E78
   Data.q $3B3B3439787D7D71,$3B3B3439787D716D,$3B3B3439787D716C,$3B3B3439787D716F,$3B3B3439787D716E
   Data.q $3B3B3439787D7169,$3B3B3439787D7168,$3B3B3439787D716B,$7D7D7D575066746A,$727D7D7D7D57507D
   Data.q $7070707070707072,$7070707070707070,$7070707070707070,$5070707070707070,$3E72727D7D7D7D57
   Data.q $38293C31283E313C,$2B3429342E322D7D,$2E3828313C2B7D38,$7D7D575077777777,$7D7D7D7D57507D7D
   Data.q $3433287331313C3E,$71742C382E34757D,$7D711114080C187D,$787D716D1C257875,$1C25787D716C1C25
   Data.q $716E1C25787D716F,$787D71691C25787D,$1C25787D71681C25,$716A1C25787D716B,$7D716D1F25787D7D
   Data.q $25787D716C1F2578,$6E1F25787D716F1F,$7D71691F25787D71,$25787D71681F2578,$6A1F25787D716B1F
   Data.q $57507D7D7D7D6674,$2D29382E7D7D7D7D,$7D6F6E28732C3873,$6C712C382E34712D,$1D7D7D7D7D575066
   Data.q $7D3C2F3F7D7D2D7C,$2938312D30323E79,$391C353E293C1F38,$2832193529340A39,$090E0F141B38313F
   Data.q $7D7D57507D666911,$72727D7D7D7D7D7D,$2560606C257D3B34,$7D7D7D7D5750676F,$3C3E72727D7D7D7D
   Data.q $052A3833757D3131,$6C052A38337D716D,$716F052A38337D71,$7D716E052A38337D,$337D7169052A3833
   Data.q $38337D7168052A38,$2A38337D716B052A,$342F787D7D716A05,$342F787D716D382E,$342F787D716C382E
   Data.q $342F787D716F382E,$342F787D716E382E,$342F787D7169382E,$342F787D7168382E,$342F787D716B382E
   Data.q $1F197D71746A382E,$2578757D71090D11,$6C1C25787D716D1C,$7D716F1C25787D71,$25787D716E1C2578
   Data.q $681C25787D71691C,$7D716B1C25787D71,$787D7D716A1C2578,$1C24787D716D1C24,$716F1C24787D716C
   Data.q $787D716E1C24787D,$1C24787D71691C24,$716B1C24787D7168,$5066746A1C24787D,$7D7D7D7D7D7D7D57
   Data.q $297D38281972727D,$7D3A33342E287D32,$3A313C7D2E3A2E3F,$6C257D39333C7D32,$3E7D382A7D6F2560
   Data.q $297D243C2E7D333C,$3C7D382A7D293C35,$367D24393C382F31,$57506C257D2A3233,$7D7D7D7D7D7D7D7D
   Data.q $7D6F6E3F732B3230,$50666C712C382E34,$7D7D7D7D7D7D7D57,$8CDC8CDE8C72727D,$8DED8DE08DED8DDF
   Data.q $8DEF8DE58DE68DEF,$D98C7DE18DE88DED,$5750EE8DED8DE68D,$7D7D7D7D7D7D7D7D,$733032293C7D2D1D
   Data.q $3C73313C3F32313A,$7D7D6F6E28733939,$06716D382E342F78,$2E3471006D392F78,$7D7D7D5750662C38
   Data.q $7D57507D7D7D7D7D,$3C7D7D7D7D7D7D7D,$7D7D6F6E3F733933,$382E342F78547D7D,$6D382E342F78716D
   Data.q $1B1B1B1B256D7D71,$7D7D7D7D7D575066,$2873292B3E7D7D7D,$7D7D6F6E2873696B,$6D382E342F787129
   Data.q $7D7D7D7D7D575066,$7D7D7D57507D7D7D,$7D2D1D7D7D7D7D7D,$2873323173393C30,$712F292D107D696B
   Data.q $6D392F7871657129,$8C72727D7D7D7D66,$8CE88DE28DE88DDF,$ED8DE78D7DD18CDD,$E48DD68CE98DEB8D
   Data.q $8DE38DE68DEC8D7D,$DF8CE58DE08D06E7,$8DDE8CE28D71D18C,$8DE98DE48DED8DEC,$7DED8DE08D7D00E5
   Data.q $E88DE38DEF8DDC8C,$8CE88DE18D7DE18D,$7D7D7DE88DDF8CDC,$57507D7D7D7D7D7D,$7D7D7D7D7D7D7D7D
   Data.q $7D7D7D57507D7D7D,$39393C7D7D7D7D7D,$342F787D6F6E2873,$2F35297D716C382E,$716F6E3914393C38
   Data.q $28322F253C10027D,$7D7D7D5750663933,$7D57507D7D7D7D7D,$1D7D7D7D7D7D7D7D,$32313A73292E7D2D
   Data.q $28736F2B73313C3F,$2F292D10067D6F6E,$71692671006B6C76,$66206C382E342F78,$7D7D7D7D7D7D5750
   Data.q $7D7D7D7D57507D7D,$7D3C2F3F7D7D7D7D,$2938312D30323E79,$391C353E293C1F38,$2832193529340A39
   Data.q $090E0F141B38313F,$7957506609140518,$382938312D30323E,$39391C353E293C1F,$3F2832193529340A
   Data.q $11090E0F141B3831,$7D7D7D7D57506769,$7D7D57507D7D7D7D,$2D32312E72727D7D,$6F24706C24756038
   Data.q $2E2F382B33347774,$716F25706C257538,$7D7D7D7D5750742D,$3433287331313C3E,$6D382E342F78757D
   Data.q $6C382E342F787D71,$6F382E342F787D71,$6E382E342F787D71,$69382E342F787D71,$68382E342F787D71
   Data.q $6B382E342F787D71,$6A382E342F787D71,$12101F080E7D7174,$1C2478757D710D19,$716C1C24787D716D
   Data.q $787D716F1C24787D,$1C24787D716E1C24,$71681C24787D7169,$787D716B1C24787D,$24787D7D716A1C24
   Data.q $6C1F24787D716D1F,$7D716F1F24787D71,$24787D716E1F2478,$681F24787D71691F,$7D716B1F24787D71
   Data.q $575066746A1F2478,$31313C3E7D7D7D7D,$3E78757D34332873,$6C253E787D716D25,$7D716F253E787D71
   Data.q $3E787D716E253E78,$68253E787D716925,$7D716B253E787D71,$107D71746A253E78,$7D710D1912101108
   Data.q $716D382E342F7875,$716C382E342F787D,$716F382E342F787D,$716E382E342F787D,$7169382E342F787D
   Data.q $7168382E342F787D,$716B382E342F787D,$716A382E342F787D,$787D716D2E787D7D,$716F2E787D716C2E
   Data.q $2E787D716E2E787D,$7D71682E787D7169,$6A2E787D716B2E78,$7D7D575057506674,$0F7D727272727D7D
   Data.q $7D6F032E7D607D25,$0C7D707D251A7D70,$322D7D7D63607D25,$312E75393230022A,$742D716F71382D32
   Data.q $746F25766C257570,$3C3E7D7D7D7D5750,$757D343328733131,$7D716D382E342F78,$7D716C382E342F78
   Data.q $7D716F382E342F78,$7D716E382E342F78,$7D7169382E342F78,$7D7168382E342F78,$7D716B382E342F78
   Data.q $71746A382E342F78,$0D1912101108107D,$716D253E78757D71,$787D716C253E787D,$253E787D716F253E
   Data.q $7169253E787D716E,$787D7168253E787D,$253E787D716B253E,$6D253E787D7D716A,$7D716C253E787D71
   Data.q $3E787D716F253E78,$69253E787D716E25,$7D7168253E787D71,$3E787D716B253E78,$7D7D575066746A25
   Data.q $287331313C3E7D7D,$2E342F78757D3433,$2E342F787D716D38,$2E342F787D716C38,$2E342F787D716F38
   Data.q $2E342F787D716E38,$2E342F787D716938,$2E342F787D716838,$2E342F787D716B38,$1F080E7D71746A38
   Data.q $78757D710D191210,$787D716D382E342F,$787D716C382E342F,$787D716F382E342F,$787D716E382E342F
   Data.q $787D7169382E342F,$787D7168382E342F,$787D716B382E342F,$7D7D716A382E342F,$25787D716D1C2578
   Data.q $6F1C25787D716C1C,$7D716E1C25787D71,$25787D71691C2578,$6B1C25787D71681C,$66746A1C25787D71
   Data.q $3C3E7D7D7D7D5750,$757D343328733131,$337D716D052A3833,$38337D716C052A38,$2A38337D716F052A
   Data.q $052A38337D716E05,$68052A38337D7169,$716B052A38337D71,$71746A052A38337D,$0D1912101F080E7D
   Data.q $382E342F78757D71,$382E342F787D716D,$382E342F787D716C,$382E342F787D716F,$382E342F787D716E
   Data.q $382E342F787D7169,$382E342F787D7168,$382E342F787D716B,$6D1F25787D7D716A,$7D716C1F25787D71
   Data.q $25787D716F1F2578,$691F25787D716E1F,$7D71681F25787D71,$25787D716B1F2578,$7D7D575066746A1F
   Data.q $7D7D7D7D57507D7D,$3E7957507D7D7D7D,$1F382938312D3032,$0A39391C353E293C,$313F283219352934
   Data.q $0518090E0F141B38,$7D7D57507D670914,$7D7D7D7D57507D7D,$57507D7D7D7D5750,$141B72727D7D7D7D
   Data.q $1A1C090E7D111C13,$0F1C0D10121E7D18,$0E091314120D7D18,$7D7D7D7D7D7D7D7D,$3C3E7D7D7D7D5750
   Data.q $757D343328733131,$2F787D712C382E34,$1B7D71746F382E34,$120E131419130812,$757D710A1813090F
   Data.q $337D716D052A3833,$0D027D716C052A38,$7D716D7D712F292D,$293328323E3F282D,$3009157D71092F38
   Data.q $3409157D71362E3C,$575066742E303829,$575057507D7D7D7D,$7D7D57507D7D7D7D,$35732D29382E7D7D
   Data.q $712D7D6F6E287334,$50666D712C382E34,$DC8CDE8C72725457,$ED8DE08DED8DDF8C,$EF8DE58DE68DEF8D
   Data.q $8C7DE18DE88DED8D,$50EE8DED8DE68DD9,$32293C7D2D1D5457,$313C3F32313A7330,$6F6E287339393C73
   Data.q $6D382E342F787D7D,$71006D392F780671,$7D5750662C382E34,$7D7D7D57507D7D7D,$6F6E3F7339333C7D
   Data.q $342F78547D7D7D7D,$2E342F78716D382E,$1B1B256D7D716D38,$7D7D7D5750661B1B,$696B2873292B3E7D
   Data.q $71297D7D6F6E2873,$50666D382E342F78,$5457507D7D7D7D57,$3173393C307D2D1D,$2D107D696B287332
   Data.q $7871657129712F29,$7D7D7D7D666D392F,$E28DE88DDF8C7272,$8D7DD18CDD8CE88D,$8CE98DEB8DED8DE7
   Data.q $E68DEC8D7DE48DD6,$8DE08D06E78DE38D,$E28D71D18CDF8CE5,$E48DED8DEC8DDE8C,$E08D7D00E58DE98D
   Data.q $8DEF8DDC8C7DED8D,$E18D7DE18DE88DE3,$E88DDF8CDC8CE88D,$7D7D7D7D7D7D7D7D,$7D7D7D7D7D57507D
   Data.q $7D7D7D7D57507D7D,$7D6F6E287339393C,$7D716C382E342F78,$3914393C382F3529,$253C10027D716F6E
   Data.q $575066393328322F,$7D7D57507D7D7D7D,$73292E7D2D1D7D7D,$2B73313C3F32313A,$10067D6F6E28736F
   Data.q $71006B6C762F292D,$382E342F78716C26,$7D7D7D575066206C,$2E7D7D7D7D57507D,$027D6F6E28733F28
   Data.q $393328322F253C10,$322F253C10027D71,$50666C7D71393328,$7957507D7D7D7D57,$382938312D30323E
   Data.q $39391C353E293C1F,$3F2832193529340A,$7D5750676C113831,$7D7D7D57507D7D7D,$7D393C323172727D
   Data.q $2D7D1F257D711F25,$2F3B7D2E29333432,$2F290D05027D3032,$3C3E7D7D7D7D5750,$757D343328733131
   Data.q $25787D716D1F2578,$6F1F25787D716C1F,$7D716E1F25787D71,$25787D71691F2578,$6B1F25787D71681F
   Data.q $71746A1F25787D71,$293314393C382F7D,$2F290D0502757D71,$322F253C1002717D,$7D57506674393328
   Data.q $3C323172727D7D7D,$1F247D711F247D39,$7D2E293334322D7D,$0D04027D30322F3B,$7D7D7D7D57502F29
   Data.q $3433287331313C3E,$7D716D1F2478757D,$24787D716C1F2478,$6E1F24787D716F1F,$7D71691F24787D71
   Data.q $24787D71681F2478,$6A1F24787D716B1F,$14393C382F7D7174,$0D0402757D712933,$253C1002717D2F29
   Data.q $506674393328322F,$7D57507D7D7D7D57,$7D7D7D57507D7D7D,$3435732D29382E7D,$02712D7D6F6E2873
   Data.q $393328322F253C10,$507D7D7D7D666D71,$2F3F7D2D7C1D5457,$3E797D343328733C,$1F382938312D3032
   Data.q $0A39391C353E293C,$313F283219352934,$575066322F380738,$7D7D57507D7D7D7D,$6E28733F282E7D7D
   Data.q $3E3314253C107D6F,$322F253C10027D71,$7D666C7D71393328,$57507D7D7D7D5750,$7D7D7D7D7D7D7D7D
   Data.q $7D7D57507D7D7D7D,$57507D7D7D7D7D7D,$323172727D7D7D7D,$7D3B3B34397D393C,$6C707D353E293C3F
   Data.q $3E7D7D7D7D57507D,$7D3433287331313C,$716D3B3B34397875,$716C3B3B3439787D,$716F3B3B3439787D
   Data.q $716E3B3B3439787D,$71693B3B3439787D,$71683B3B3439787D,$716B3B3B3439787D,$746A3B3B3439787D
   Data.q $3314393C382F7D71,$3B341902757D7129,$3C10717D2F290D3B,$7D7D66743E331425,$7D7D7D7D57507D7D
   Data.q $3433287331313C3E,$787D716D2E78757D,$716F2E787D716C2E,$2E787D716E2E787D,$7D71682E787D7169
   Data.q $6A2E787D716B2E78,$12101108107D7174,$283E78757D710D19,$787D716D2B33342F,$716C2B33342F283E
   Data.q $2B33342F283E787D,$342F283E787D716F,$283E787D716E2B33,$787D71692B33342F,$71682B33342F283E
   Data.q $2B33342F283E787D,$342F283E787D716B,$39787D7D716A2B33,$39787D716D3B3B34,$39787D716C3B3B34
   Data.q $39787D716F3B3B34,$39787D716E3B3B34,$39787D71693B3B34,$39787D71683B3B34,$39787D716B3B3B34
   Data.q $575066746A3B3B34,$7D7D57507D7D7D7D,$7D7D7D7D57507D7D,$57507D7D7D7D5750,$392872727D7D7D7D
   Data.q $33343C307D38293C,$382E2F382B33347D,$3C3E7D7D7D7D5750,$757D343328733131,$187D71742C382E34
   Data.q $78757D711114080C,$1C25787D716D1C25,$716F1C25787D716C,$787D716E1C25787D,$1C25787D71691C25
   Data.q $716B1C25787D7168,$7D7D716A1C25787D,$25787D716D1F2578,$6F1F25787D716C1F,$7D716E1F25787D71
   Data.q $25787D71691F2578,$6B1F25787D71681F,$66746A1F25787D71,$57507D7D7D7D5750,$3B3472727D7D7D7D
   Data.q $5750256060252D7D,$31313C3E7D7D7D7D,$787D757D34332873,$787D716D3B3B3439,$787D716C3B3B3439
   Data.q $787D716F3B3B3439,$787D716E3B3B3439,$787D71693B3B3439,$787D71683B3B3439,$787D716B3B3B3439
   Data.q $7D71746A3B3B3439,$710D19121019191C,$7D716D1C2478757D,$24787D716C1C2478,$6E1C24787D716F1C
   Data.q $7D71691C24787D71,$24787D71681C2478,$6A1C24787D716B1C,$716D1C24787D7D71,$787D716C1C24787D
   Data.q $1C24787D716F1C24,$71691C24787D716E,$787D71681C24787D,$1C24787D716B1C24,$7D7D7D575066746A
   Data.q $252D7D3B3472727D,$7D7D7D575025607C,$33287331313C3E7D,$2E342F787D757D34,$2E342F787D716D38
   Data.q $2E342F787D716C38,$2E342F787D716F38,$2E342F787D716E38,$2E342F787D716938,$2E342F787D716838
   Data.q $2E342F787D716B38,$1F080E7D71746A38,$78757D710D191210,$1C25787D716D1C25,$716F1C25787D716C
   Data.q $787D716E1C25787D,$1C25787D71691C25,$716B1C25787D7168,$7D7D716A1C25787D,$25787D716D1F2578
   Data.q $6F1F25787D716C1F,$7D716E1F25787D71,$25787D71691F2578,$6B1F25787D71681F,$66746A1F25787D71
   Data.q $382E7D7D7D7D5750,$6E28732C38732D29,$2C382E34712D7D6F,$7D7D7D5750666C71,$6E28732D31382E7D
   Data.q $3B3B343978547D6F,$6D3B3B343978716D,$716D382E342F7871,$7D7D7D7D5750662D,$6F6E28732D31382E
   Data.q $6C3B3B343978547D,$716C3B3B34397871,$2D716C382E342F78,$2E7D7D7D7D575066,$7D6F6E28732D3138
   Data.q $716F3B3B34397854,$78716F3B3B343978,$662D716F382E342F,$382E7D7D7D7D5750,$547D6F6E28732D31
   Data.q $78716E3B3B343978,$2F78716E3B3B3439,$50662D716E382E34,$31382E7D7D7D7D57,$78547D6F6E28732D
   Data.q $397871693B3B3439,$342F7871693B3B34,$5750662D7169382E,$2D31382E7D7D7D7D,$3978547D6F6E2873
   Data.q $34397871683B3B34,$2E342F7871683B3B,$7D5750662D716838,$732D31382E7D7D7D,$343978547D6F6E28
   Data.q $3B343978716B3B3B,$382E342F78716B3B,$7D7D5750662D716B,$28732D31382E7D7D,$3B343978547D6F6E
   Data.q $3B3B343978716A3B,$6A382E342F78716A,$7D7D7D5750662D71,$7D7D57507D7D7D7D,$287331313C3E7D7D
   Data.q $2F283E78757D3433,$3E787D716D2B3334,$7D716C2B33342F28,$6F2B33342F283E78,$33342F283E787D71
   Data.q $2F283E787D716E2B,$3E787D71692B3334,$7D71682B33342F28,$6B2B33342F283E78,$33342F283E787D71
   Data.q $1108107D71746A2B,$78757D710D191210,$716D2B33342F283E,$2B33342F283E787D,$342F283E787D716C
   Data.q $283E787D716F2B33,$787D716E2B33342F,$71692B33342F283E,$2B33342F283E787D,$342F283E787D7168
   Data.q $283E787D716B2B33,$7D7D716A2B33342F,$7D716D3B3B343978,$7D716C3B3B343978,$7D716F3B3B343978
   Data.q $7D716E3B3B343978,$7D71693B3B343978,$7D71683B3B343978,$7D716B3B3B343978,$66746A3B3B343978
   Data.q $57507D7D7D7D5750,$707072727D7D7D7D,$7070707070707070,$7070707070707070,$7070707070707070
   Data.q $7D7D575070707070,$3E313C3E72727D7D,$38337D38293C3128,$2B7D382B34293C3A,$7777772E3828313C
   Data.q $727D7D7D7D575077,$33347D043A383372,$5750382E342F787D,$31313C3E7D7D7D7D,$2F78757D34332873
   Data.q $2F787D716D382E34,$2F787D716C382E34,$2F787D716F382E34,$2F787D716E382E34,$2F787D7169382E34
   Data.q $2F787D7168382E34,$2F787D716B382E34,$137D71746A382E34,$7D710D1912101A18,$787D716D1F247875
   Data.q $1F24787D716C1F24,$716E1F24787D716F,$787D71691F24787D,$1F24787D71681F24,$746A1F24787D716B
   Data.q $3E7D7D7D7D575066,$7D3433287331313C,$716D382E342F7875,$716C382E342F787D,$716F382E342F787D
   Data.q $716E382E342F787D,$7169382E342F787D,$7168382E342F787D,$716B382E342F787D,$746A382E342F787D
   Data.q $1912101F080E7D71,$6D1C2478757D710D,$7D716C1C24787D71,$24787D716F1C2478,$691C24787D716E1C
   Data.q $7D71681C24787D71,$24787D716B1C2478,$2E342F787D716A1C,$2E342F787D716D38,$2E342F787D716C38
   Data.q $2E342F787D716F38,$2E342F787D716E38,$2E342F787D716938,$2E342F787D716838,$2E342F787D716B38
   Data.q $7D7D575066746A38,$287331313C3E7D7D,$6D253E78757D3433,$7D716C253E787D71,$3E787D716F253E78
   Data.q $69253E787D716E25,$7D7168253E787D71,$3E787D716B253E78,$1108107D71746A25,$78757D710D191210
   Data.q $787D716D382E342F,$787D716C382E342F,$787D716F382E342F,$787D716E382E342F,$787D7169382E342F
   Data.q $787D7168382E342F,$787D716B382E342F,$7D7D716A382E342F,$6C2E787D716D2E78,$787D716F2E787D71
   Data.q $71692E787D716E2E,$2E787D71682E787D,$66746A2E787D716B,$7D7D7D7D57505750,$3C3E7D7D7D7D5750
   Data.q $757D343328733131,$7D716D382E342F78,$7D716C382E342F78,$7D716F382E342F78,$7D716E382E342F78
   Data.q $7D7169382E342F78,$7D7168382E342F78,$7D716B382E342F78,$71746A382E342F78,$0D1912101108107D
   Data.q $716D253E78757D71,$787D716C253E787D,$253E787D716F253E,$7169253E787D716E,$787D7168253E787D
   Data.q $253E787D716B253E,$6D253E787D7D716A,$7D716C253E787D71,$3E787D716F253E78,$69253E787D716E25
   Data.q $7D7168253E787D71,$3E787D716B253E78,$7D7D575066746A25,$287331313C3E7D7D,$2E342F78757D3433
   Data.q $2E342F787D716D38,$2E342F787D716C38,$2E342F787D716F38,$2E342F787D716E38,$2E342F787D716938
   Data.q $2E342F787D716838,$2E342F787D716B38,$1F080E7D71746A38,$78757D710D191210,$787D716D382E342F
   Data.q $787D716C382E342F,$787D716F382E342F,$787D716E382E342F,$787D7169382E342F,$787D7168382E342F
   Data.q $787D716B382E342F,$7D7D716A382E342F,$25787D716D1C2578,$6F1C25787D716C1C,$7D716E1C25787D71
   Data.q $25787D71691C2578,$6B1C25787D71681C,$66746A1C25787D71,$3C3E7D7D7D7D5750,$757D343328733131
   Data.q $337D716D052A3833,$38337D716C052A38,$2A38337D716F052A,$052A38337D716E05,$68052A38337D7169
   Data.q $716B052A38337D71,$71746A052A38337D,$0D1912101F080E7D,$382E342F78757D71,$382E342F787D716D
   Data.q $382E342F787D716C,$382E342F787D716F,$382E342F787D716E,$382E342F787D7169,$382E342F787D7168
   Data.q $382E342F787D716B,$6D1F25787D7D716A,$7D716C1F25787D71,$25787D716F1F2578,$691F25787D716E1F
   Data.q $7D71681F25787D71,$25787D716B1F2578,$7D7D575066746A1F,$7D7D7D7D57507D7D,$7D111C13141B7272
   Data.q $121E7D181A1C090E,$120D7D180F1C0D10,$7D7D7D7D0E091314,$7D7D57507D7D7D7D,$287331313C3E7D7D
   Data.q $2C382E34757D3433,$6F382E342F787D71,$191308121B7D7174,$1813090F120E1314,$052A3833757D710A
   Data.q $6C052A38337D716D,$712F292D0D027D71,$3E3F282D7D716D7D,$71092F3829332832,$71362E3C3009157D
   Data.q $2E3038293409157D,$7D7D7D7D57506674,$382E7D7D7D7D5750,$6E28733435732D29,$2C382E34712D7D6F
   Data.q $7272545750666D71,$ED8DDF8CDC8CDE8C,$E68DEF8DED8DE08D,$E88DED8DEF8DE58D,$8DE68DD98C7DE18D
   Data.q $2D1D545750EE8DED,$313A733032293C7D,$39393C73313C3F32,$2F787D7D6F6E2873,$2F7806716D382E34
   Data.q $2C382E3471006D39,$507D7D7D7D575066,$39333C7D7D7D7D57,$7D7D7D7D6F6E3F73,$716D382E342F7854
   Data.q $7D716D382E342F78,$50661B1B1B1B256D,$292B3E7D7D7D7D57,$6F6E2873696B2873,$2E342F7871297D7D
   Data.q $7D7D7D5750666D38,$307D2D1D5457507D,$6B2873323173393C,$29712F292D107D69,$666D392F78716571
   Data.q $DF8C72727D7D7D7D,$DD8CE88DE28DE88D,$8DED8DE78D7DD18C,$7DE48DD68CE98DEB,$E78DE38DE68DEC8D
   Data.q $8CDF8CE58DE08D06,$EC8DDE8CE28D71D1,$E58DE98DE48DED8D,$8C7DED8DE08D7D00,$8DE88DE38DEF8DDC
   Data.q $DC8CE88DE18D7DE1,$7D7D7D7DE88DDF8C,$7D57507D7D7D7D7D,$57507D7D7D7D7D7D,$7339393C7D7D7D7D
   Data.q $2E342F787D6F6E28,$382F35297D716C38,$7D716F6E3914393C,$3328322F253C1002,$7D7D7D7D57506639
   Data.q $2D1D7D7D7D7D5750,$3F32313A73292E7D,$6E28736F2B73313C,$762F292D10067D6F,$78716F2671006B6C
   Data.q $5066206C382E342F,$7D57507D7D7D7D57,$7D7D7D57507D7D7D,$283E313C3E72727D,$2E322D7D38293C31
   Data.q $3C2B7D382B342934,$777777772E382831,$57507D7D7D7D5750,$31313C3E7D7D7D7D,$2E34757D34332873
   Data.q $080C187D71742C38,$1C2578757D711114,$716C1C25787D716D,$787D716F1C25787D,$1C25787D716E1C25
   Data.q $71681C25787D7169,$787D716B1C25787D,$25787D7D716A1C25,$6C1F25787D716D1F,$7D716F1F25787D71
   Data.q $25787D716E1F2578,$681F25787D71691F,$7D716B1F25787D71,$7D7D66746A1F2578,$7D7D7D7D57507D7D
   Data.q $732C38732D29382E,$2E34712D7D6F6E28,$7D5750666C712C38,$7D7D2D7C1D7D7D7D,$30323E797D3C2F3F
   Data.q $293C1F382938312D,$29340A39391C353E,$1138313F28321935,$7D7D7D57507D6669,$3472727D7D7D7D7D
   Data.q $6F2560606C257D3B,$7D7D7D7D7D575067,$313C3E72727D7D7D,$6D052A3833757D31,$716C052A38337D71
   Data.q $7D716F052A38337D,$337D716E052A3833,$38337D7169052A38,$2A38337D7168052A,$052A38337D716B05
   Data.q $2E342F787D7D716A,$2E342F787D716D38,$2E342F787D716C38,$2E342F787D716F38,$2E342F787D716E38
   Data.q $2E342F787D716938,$2E342F787D716838,$2E342F787D716B38,$111F197D71746A38,$1C2578757D71090D
   Data.q $716C1C25787D716D,$787D716F1C25787D,$1C25787D716E1C25,$71681C25787D7169,$787D716B1C25787D
   Data.q $24787D7D716A1C25,$6C1C24787D716D1C,$7D716F1C24787D71,$24787D716E1C2478,$681C24787D71691C
   Data.q $7D716B1C24787D71,$575066746A1C2478,$7D7D7D7D7D7D7D7D,$297D38281972727D,$7D3A33342E287D32
   Data.q $3A313C7D2E3A2E3F,$6C257D39333C7D32,$3E7D382A7D6F2560,$297D243C2E7D333C,$3C7D382A7D293C35
   Data.q $367D24393C382F31,$57506C257D2A3233,$7D7D7D7D7D7D7D7D,$7D6F6E3F732B3230,$50666C712C382E34
   Data.q $7D7D7D7D7D7D7D57,$8CDC8CDE8C72727D,$8DED8DE08DED8DDF,$8DEF8DE58DE68DEF,$D98C7DE18DE88DED
   Data.q $5750EE8DED8DE68D,$7D7D7D7D7D7D7D7D,$733032293C7D2D1D,$3C73313C3F32313A,$7D7D6F6E28733939
   Data.q $06716D382E342F78,$2E3471006D392F78,$7D7D7D5750662C38,$7D57507D7D7D7D7D,$3C7D7D7D7D7D7D7D
   Data.q $7D7D6F6E3F733933,$382E342F78547D7D,$6D382E342F78716D,$1B1B1B1B256D7D71,$7D7D7D7D7D575066
   Data.q $2873292B3E7D7D7D,$7D7D6F6E2873696B,$6D382E342F787129,$7D7D7D7D7D575066,$7D7D7D57507D7D7D
   Data.q $7D2D1D7D7D7D7D7D,$2873323173393C30,$712F292D107D696B,$6D392F7871657129,$8C72727D7D7D7D66
   Data.q $8CE88DE28DE88DDF,$ED8DE78D7DD18CDD,$E48DD68CE98DEB8D,$8DE38DE68DEC8D7D,$DF8CE58DE08D06E7
   Data.q $8DDE8CE28D71D18C,$8DE98DE48DED8DEC,$7DED8DE08D7D00E5,$E88DE38DEF8DDC8C,$8CE88DE18D7DE18D
   Data.q $7D7D7DE88DDF8CDC,$57507D7D7D7D7D7D,$7D7D7D7D7D7D7D7D,$7D7D7D57507D7D7D,$39393C7D7D7D7D7D
   Data.q $342F787D6F6E2873,$2F35297D716C382E,$716F6E3914393C38,$28322F253C10027D,$7D7D7D5750663933
   Data.q $7D57507D7D7D7D7D,$1D7D7D7D7D7D7D7D,$32313A73292E7D2D,$28736F2B73313C3F,$2F292D10067D6F6E
   Data.q $71692671006B6C76,$66206C382E342F78,$7D7D7D7D7D7D5750,$3E797D3C2F3F7D7D,$1F382938312D3032
   Data.q $0A39391C353E293C,$313F283219352934,$5750660914051838,$7D7D7D7D7D7D7D7D,$312D30323E795750
   Data.q $353E293C1F382938,$193529340A39391C,$67691138313F2832,$7D7D7D7D7D7D5750,$7D7D7D7D57507D7D
   Data.q $60382D32312E7272,$77746F24706C2475,$75382E2F382B3334,$742D716F25706C25,$3C3E7D7D7D7D5750
   Data.q $757D343328733131,$7D716D382E342F78,$7D716C382E342F78,$7D716F382E342F78,$7D716E382E342F78
   Data.q $7D7169382E342F78,$7D7168382E342F78,$7D716B382E342F78,$71746A382E342F78,$0D1912101F080E7D
   Data.q $716D1C2478757D71,$787D716C1C24787D,$1C24787D716F1C24,$71691C24787D716E,$787D71681C24787D
   Data.q $1C24787D716B1C24,$6D1F24787D7D716A,$7D716C1F24787D71,$24787D716F1F2478,$691F24787D716E1F
   Data.q $7D71681F24787D71,$24787D716B1F2478,$7D7D575066746A1F,$287331313C3E7D7D,$6D253E78757D3433
   Data.q $7D716C253E787D71,$3E787D716F253E78,$69253E787D716E25,$7D7168253E787D71,$3E787D716B253E78
   Data.q $1108107D71746A25,$78757D710D191210,$787D716D382E342F,$787D716C382E342F,$787D716F382E342F
   Data.q $787D716E382E342F,$787D7169382E342F,$787D7168382E342F,$787D716B382E342F,$7D7D716A382E342F
   Data.q $6C2E787D716D2E78,$787D716F2E787D71,$71692E787D716E2E,$2E787D71682E787D,$66746A2E787D716B
   Data.q $7D7D7D7D57505750,$7D250F7D72727272,$7D707D6F032E7D60,$7D250C7D707D251A,$022A322D7D7D6360
   Data.q $2D32312E75393230,$7570742D716F7138,$5750746F25766C25,$31313C3E7D7D7D7D,$2F78757D34332873
   Data.q $2F787D716D382E34,$2F787D716C382E34,$2F787D716F382E34,$2F787D716E382E34,$2F787D7169382E34
   Data.q $2F787D7168382E34,$2F787D716B382E34,$107D71746A382E34,$7D710D1912101108,$787D716D253E7875
   Data.q $253E787D716C253E,$716E253E787D716F,$787D7169253E787D,$253E787D7168253E,$716A253E787D716B
   Data.q $7D716D253E787D7D,$3E787D716C253E78,$6E253E787D716F25,$7D7169253E787D71,$3E787D7168253E78
   Data.q $6A253E787D716B25,$7D7D7D7D57506674,$3433287331313C3E,$6D382E342F78757D,$6C382E342F787D71
   Data.q $6F382E342F787D71,$6E382E342F787D71,$69382E342F787D71,$68382E342F787D71,$6B382E342F787D71
   Data.q $6A382E342F787D71,$12101F080E7D7174,$342F78757D710D19,$342F787D716D382E,$342F787D716C382E
   Data.q $342F787D716F382E,$342F787D716E382E,$342F787D7169382E,$342F787D7168382E,$342F787D716B382E
   Data.q $25787D7D716A382E,$6C1C25787D716D1C,$7D716F1C25787D71,$25787D716E1C2578,$681C25787D71691C
   Data.q $7D716B1C25787D71,$575066746A1C2578,$31313C3E7D7D7D7D,$3833757D34332873,$2A38337D716D052A
   Data.q $052A38337D716C05,$6E052A38337D716F,$7169052A38337D71,$7D7168052A38337D,$337D716B052A3833
   Data.q $0E7D71746A052A38,$7D710D1912101F08,$716D382E342F7875,$716C382E342F787D,$716F382E342F787D
   Data.q $716E382E342F787D,$7169382E342F787D,$7168382E342F787D,$716B382E342F787D,$716A382E342F787D
   Data.q $7D716D1F25787D7D,$25787D716C1F2578,$6E1F25787D716F1F,$7D71691F25787D71,$25787D71681F2578
   Data.q $6A1F25787D716B1F,$7D7D7D7D57506674,$7D7D7D7D7D7D5750,$30323E7957507D7D,$293C1F382938312D
   Data.q $29340A39391C353E,$1838313F28321935,$7D57507D67091405,$7D7D7D57507D7D7D,$507D7D7D7D57507D
   Data.q $1B72727D7D7D7D57,$1C090E7D111C1314,$1C0D10121E7D181A,$091314120D7D180F,$7D7D7D7D7D7D7D0E
   Data.q $3E7D7D7D7D57507D,$7D3433287331313C,$787D712C382E3475,$7D71746F382E342F,$0E1314191308121B
   Data.q $7D710A1813090F12,$7D716D052A383375,$027D716C052A3833,$716D7D712F292D0D,$3328323E3F282D7D
   Data.q $09157D71092F3829,$09157D71362E3C30,$5066742E30382934,$5057507D7D7D7D57,$7D57507D7D7D7D57
   Data.q $732D29382E7D7D7D,$2D7D6F6E28733435,$666D712C382E3471,$8CDE8C7272545750,$8DE08DED8DDF8CDC
   Data.q $8DE58DE68DEF8DED,$7DE18DE88DED8DEF,$EE8DED8DE68DD98C,$293C7D2D1D545750,$3C3F32313A733032
   Data.q $6E287339393C7331,$382E342F787D7D6F,$006D392F7806716D,$5750662C382E3471,$7D7D57507D7D7D7D
   Data.q $6E3F7339333C7D7D,$2F78547D7D7D7D6F,$342F78716D382E34,$1B256D7D716D382E,$7D7D5750661B1B1B
   Data.q $6B2873292B3E7D7D,$297D7D6F6E287369,$666D382E342F7871,$57507D7D7D7D5750,$73393C307D2D1D54
   Data.q $107D696B28733231,$71657129712F292D,$7D7D7D666D392F78,$8DE88DDF8C72727D,$7DD18CDD8CE88DE2
   Data.q $E98DEB8DED8DE78D,$8DEC8D7DE48DD68C,$E08D06E78DE38DE6,$8D71D18CDF8CE58D,$8DED8DEC8DDE8CE2
   Data.q $8D7D00E58DE98DE4,$EF8DDC8C7DED8DE0,$8D7DE18DE88DE38D,$8DDF8CDC8CE88DE1,$7D7D7D7D7D7D7DE8
   Data.q $7D7D7D7D57507D7D,$7D7D7D57507D7D7D,$6F6E287339393C7D,$716C382E342F787D,$14393C382F35297D
   Data.q $3C10027D716F6E39,$5066393328322F25,$7D57507D7D7D7D57,$292E7D2D1D7D7D7D,$73313C3F32313A73
   Data.q $067D6F6E28736F2B,$006B6C762F292D10,$2E342F78716C2671,$7D7D575066206C38,$7D7D7D7D57507D7D
   Data.q $7D7D7D57507D7D7D,$2E7D7D7D7D57507D,$027D6F6E28733F28,$393328322F253C10,$322F253C10027D71
   Data.q $50666C7D71393328,$3C2F3F7D7D7D7D57,$323E797D34332873,$3C1F382938312D30,$340A39391C353E29
   Data.q $38313F2832193529,$7D7D7D5750666C11,$507D7D7D7D57507D,$38312D30323E7957,$1C353E293C1F3829
   Data.q $32193529340A3939,$322F380738313F28,$727D7D7D7D575067,$3E293C3F7D3B3472,$7D7D57506D606035
   Data.q $287331313C3E7D7D,$716D2E78757D3433,$2E787D716C2E787D,$7D716E2E787D716F,$682E787D71692E78
   Data.q $787D716B2E787D71,$0D121E7D71746A2E,$710913141A141F04,$33342F283E78757D,$2F283E787D716D2B
   Data.q $3E787D716C2B3334,$7D716F2B33342F28,$6E2B33342F283E78,$33342F283E787D71,$2F283E787D71692B
   Data.q $3E787D71682B3334,$7D716B2B33342F28,$6A2B33342F283E78,$7D7D7D7D57506674,$72727D7D7D7D5750
   Data.q $293C31283E313C3E,$34293C3A38337D38,$3828313C2B7D382B,$7D5750777777772E,$3A383372727D7D7D
   Data.q $342F787D33347D04,$7D7D7D7D5750382E,$3433287331313C3E,$6D382E342F78757D,$6C382E342F787D71
   Data.q $6F382E342F787D71,$6E382E342F787D71,$69382E342F787D71,$68382E342F787D71,$6B382E342F787D71
   Data.q $6A382E342F787D71,$12101A18137D7174,$1F2478757D710D19,$716C1F24787D716D,$787D716F1F24787D
   Data.q $1F24787D716E1F24,$71681F24787D7169,$787D716B1F24787D,$7D575066746A1F24,$7331313C3E7D7D7D
   Data.q $342F78757D343328,$342F787D716D382E,$342F787D716C382E,$342F787D716F382E,$342F787D716E382E
   Data.q $342F787D7169382E,$342F787D7168382E,$342F787D716B382E,$080E7D71746A382E,$757D710D1912101F
   Data.q $24787D716D1C2478,$6F1C24787D716C1C,$7D716E1C24787D71,$24787D71691C2478,$6B1C24787D71681C
   Data.q $7D716A1C24787D71,$7D716D382E342F78,$7D716C382E342F78,$7D716F382E342F78,$7D716E382E342F78
   Data.q $7D7169382E342F78,$7D7168382E342F78,$7D716B382E342F78,$66746A382E342F78,$3C3E7D7D7D7D5750
   Data.q $757D343328733131,$3E787D716D253E78,$6F253E787D716C25,$7D716E253E787D71,$3E787D7169253E78
   Data.q $6B253E787D716825,$71746A253E787D71,$0D1912101108107D,$382E342F78757D71,$382E342F787D716D
   Data.q $382E342F787D716C,$382E342F787D716F,$382E342F787D716E,$382E342F787D7169,$382E342F787D7168
   Data.q $382E342F787D716B,$716D2E787D7D716A,$2E787D716C2E787D,$7D716E2E787D716F,$682E787D71692E78
   Data.q $787D716B2E787D71,$5750575066746A2E,$7D7D57507D7D7D7D,$287331313C3E7D7D,$2E342F78757D3433
   Data.q $2E342F787D716D38,$2E342F787D716C38,$2E342F787D716F38,$2E342F787D716E38,$2E342F787D716938
   Data.q $2E342F787D716838,$2E342F787D716B38,$1108107D71746A38,$78757D710D191210,$253E787D716D253E
   Data.q $716F253E787D716C,$787D716E253E787D,$253E787D7169253E,$716B253E787D7168,$7D7D716A253E787D
   Data.q $3E787D716D253E78,$6F253E787D716C25,$7D716E253E787D71,$3E787D7169253E78,$6B253E787D716825
   Data.q $66746A253E787D71,$3C3E7D7D7D7D5750,$757D343328733131,$7D716D382E342F78,$7D716C382E342F78
   Data.q $7D716F382E342F78,$7D716E382E342F78,$7D7169382E342F78,$7D7168382E342F78,$7D716B382E342F78
   Data.q $71746A382E342F78,$0D1912101F080E7D,$382E342F78757D71,$382E342F787D716D,$382E342F787D716C
   Data.q $382E342F787D716F,$382E342F787D716E,$382E342F787D7169,$382E342F787D7168,$382E342F787D716B
   Data.q $6D1C25787D7D716A,$7D716C1C25787D71,$25787D716F1C2578,$691C25787D716E1C,$7D71681C25787D71
   Data.q $25787D716B1C2578,$7D7D575066746A1C,$287331313C3E7D7D,$052A3833757D3433,$6C052A38337D716D
   Data.q $716F052A38337D71,$7D716E052A38337D,$337D7169052A3833,$38337D7168052A38,$2A38337D716B052A
   Data.q $1F080E7D71746A05,$78757D710D191210,$787D716D382E342F,$787D716C382E342F,$787D716F382E342F
   Data.q $787D716E382E342F,$787D7169382E342F,$787D7168382E342F,$787D716B382E342F,$7D7D716A382E342F
   Data.q $25787D716D1F2578,$6F1F25787D716C1F,$7D716E1F25787D71,$25787D71691F2578,$6B1F25787D71681F
   Data.q $66746A1F25787D71,$57507D7D7D7D5750,$141B72727D7D7D7D,$1A1C090E7D111C13,$0F1C0D10121E7D18
   Data.q $0E091314120D7D18,$7D7D7D7D7D7D7D7D,$3C3E7D7D7D7D5750,$757D343328733131,$2F787D712C382E34
   Data.q $1B7D71746F382E34,$120E131419130812,$757D710A1813090F,$337D716D052A3833,$0D027D716C052A38
   Data.q $7D716D7D712F292D,$293328323E3F282D,$3009157D71092F38,$3409157D71362E3C,$575066742E303829
   Data.q $7D7D57507D7D7D7D,$35732D29382E7D7D,$712D7D6F6E287334,$50666D712C382E34,$DC8CDE8C72725457
   Data.q $ED8DE08DED8DDF8C,$EF8DE58DE68DEF8D,$8C7DE18DE88DED8D,$50EE8DED8DE68DD9,$32293C7D2D1D5457
   Data.q $313C3F32313A7330,$6F6E287339393C73,$6D382E342F787D7D,$71006D392F780671,$7D5750662C382E34
   Data.q $7D7D7D57507D7D7D,$6F6E3F7339333C7D,$342F78547D7D7D7D,$2E342F78716D382E,$1B1B256D7D716D38
   Data.q $7D7D7D5750661B1B,$696B2873292B3E7D,$71297D7D6F6E2873,$50666D382E342F78,$5457507D7D7D7D57
   Data.q $3173393C307D2D1D,$2D107D696B287332,$7871657129712F29,$7D7D7D7D666D392F,$E28DE88DDF8C7272
   Data.q $8D7DD18CDD8CE88D,$8CE98DEB8DED8DE7,$E68DEC8D7DE48DD6,$8DE08D06E78DE38D,$E28D71D18CDF8CE5
   Data.q $E48DED8DEC8DDE8C,$E08D7D00E58DE98D,$8DEF8DDC8C7DED8D,$E18D7DE18DE88DE3,$E88DDF8CDC8CE88D
   Data.q $7D7D7D7D7D7D7D7D,$7D7D7D7D7D57507D,$7D7D7D7D57507D7D,$7D6F6E287339393C,$7D716C382E342F78
   Data.q $3914393C382F3529,$253C10027D716F6E,$575066393328322F,$7D7D57507D7D7D7D,$73292E7D2D1D7D7D
   Data.q $2B73313C3F32313A,$10067D6F6E28736F,$71006B6C762F292D,$382E342F78716F26,$7D7D7D575066206C
   Data.q $507D7D7D7D57507D,$3E72727D7D7D7D57,$38293C31283E313C,$2B3429342E322D7D,$2E3828313C2B7D38
   Data.q $7D7D575077777777,$7D7D7D7D57507D7D,$3433287331313C3E,$71742C382E34757D,$7D711114080C187D
   Data.q $787D716D1C257875,$1C25787D716C1C25,$716E1C25787D716F,$787D71691C25787D,$1C25787D71681C25
   Data.q $716A1C25787D716B,$7D716D1F25787D7D,$25787D716C1F2578,$6E1F25787D716F1F,$7D71691F25787D71
   Data.q $25787D71681F2578,$6A1F25787D716B1F,$7D7D7D57507D6674,$2C38732D29382E7D,$34712D7D6F6E2873
   Data.q $5750666C712C382E,$7D2D7C1D7D7D7D7D,$323E797D3C2F3F7D,$3C1F382938312D30,$340A39391C353E29
   Data.q $38313F2832193529,$7D7D57507D666811,$72727D7D7D7D7D7D,$2560606C257D3B34,$7D7D7D7D5750676F
   Data.q $3C3E72727D7D7D7D,$052A3833757D3131,$6C052A38337D716D,$716F052A38337D71,$7D716E052A38337D
   Data.q $337D7169052A3833,$38337D7168052A38,$2A38337D716B052A,$342F787D7D716A05,$342F787D716D382E
   Data.q $342F787D716C382E,$342F787D716F382E,$342F787D716E382E,$342F787D7169382E,$342F787D7168382E
   Data.q $342F787D716B382E,$1F197D71746A382E,$2578757D71090D11,$6C1C25787D716D1C,$7D716F1C25787D71
   Data.q $25787D716E1C2578,$681C25787D71691C,$7D716B1C25787D71,$787D7D716A1C2578,$1C24787D716D1C24
   Data.q $716F1C24787D716C,$787D716E1C24787D,$1C24787D71691C24,$716B1C24787D7168,$5066746A1C24787D
   Data.q $7D7D7D7D7D7D7D57,$7D38281972727D7D,$3A33342E287D3229,$313C7D2E3A2E3F7D,$257D39333C7D323A
   Data.q $7D382A7D6F25606C,$7D243C2E7D333C3E,$7D382A7D293C3529,$7D24393C382F313C,$506C257D2A323336
   Data.q $7D7D7D7D7D7D7D57,$6F6E3F732B32307D,$666C712C382E347D,$7D7D7D7D7D7D5750,$DC8CDE8C72727D7D
   Data.q $ED8DE08DED8DDF8C,$EF8DE58DE68DEF8D,$8C7DE18DE88DED8D,$50EE8DED8DE68DD9,$7D7D7D7D7D7D7D57
   Data.q $3032293C7D2D1D7D,$73313C3F32313A73,$7D6F6E287339393C,$716D382E342F787D,$3471006D392F7806
   Data.q $7D7D5750662C382E,$57507D7D7D7D7D7D,$7D7D7D7D7D7D7D7D,$7D6F6E3F7339333C,$2E342F78547D7D7D
   Data.q $382E342F78716D38,$1B1B1B256D7D716D,$7D7D7D7D5750661B,$73292B3E7D7D7D7D,$7D6F6E2873696B28
   Data.q $382E342F7871297D,$7D7D7D7D5750666D,$7D7D57507D7D7D7D,$2D1D7D7D7D7D7D7D,$73323173393C307D
   Data.q $2F292D107D696B28,$392F787165712971,$72727D7D7D7D666D,$E88DE28DE88DDF8C,$8DE78D7DD18CDD8C
   Data.q $8DD68CE98DEB8DED,$E38DE68DEC8D7DE4,$8CE58DE08D06E78D,$DE8CE28D71D18CDF,$E98DE48DED8DEC8D
   Data.q $ED8DE08D7D00E58D,$8DE38DEF8DDC8C7D,$E88DE18D7DE18DE8,$7D7DE88DDF8CDC8C,$507D7D7D7D7D7D7D
   Data.q $7D7D7D7D7D7D7D57,$7D7D57507D7D7D7D,$393C7D7D7D7D7D7D,$2F787D6F6E287339,$35297D716C382E34
   Data.q $6F6E3914393C382F,$322F253C10027D71,$7D7D575066393328,$57507D7D7D7D7D7D,$7D7D7D7D7D7D7D7D
   Data.q $313A73292E7D2D1D,$736F2B73313C3F32,$292D10067D6F6E28,$692671006B6C762F,$206C382E342F7871
   Data.q $7D7D7D7D7D575066,$797D3C2F3F7D7D7D,$382938312D30323E,$39391C353E293C1F,$3F2832193529340A
   Data.q $666F091405183831,$312D30323E795750,$353E293C1F382938,$193529340A39391C,$67681138313F2832
   Data.q $7D7D7D7D57505750,$72727D7D7D7D5750,$247560382D32312E,$333477746F24706C,$6C2575382E2F382B
   Data.q $5750742D716F2570,$31313C3E7D7D7D7D,$2F78757D34332873,$2F787D716D382E34,$2F787D716C382E34
   Data.q $2F787D716F382E34,$2F787D716E382E34,$2F787D7169382E34,$2F787D7168382E34,$2F787D716B382E34
   Data.q $0E7D71746A382E34,$7D710D1912101F08,$787D716D1C247875,$1C24787D716C1C24,$716E1C24787D716F
   Data.q $787D71691C24787D,$1C24787D71681C24,$716A1C24787D716B,$7D716D1F24787D7D,$24787D716C1F2478
   Data.q $6E1F24787D716F1F,$7D71691F24787D71,$24787D71681F2478,$6A1F24787D716B1F,$7D7D7D7D57506674
   Data.q $3433287331313C3E,$7D716D253E78757D,$3E787D716C253E78,$6E253E787D716F25,$7D7169253E787D71
   Data.q $3E787D7168253E78,$6A253E787D716B25,$12101108107D7174,$342F78757D710D19,$342F787D716D382E
   Data.q $342F787D716C382E,$342F787D716F382E,$342F787D716E382E,$342F787D7169382E,$342F787D7168382E
   Data.q $342F787D716B382E,$2E787D7D716A382E,$7D716C2E787D716D,$6E2E787D716F2E78,$787D71692E787D71
   Data.q $716B2E787D71682E,$575066746A2E787D,$72727D7D7D7D5750,$7D607D250F7D7272,$251A7D707D6F032E
   Data.q $63607D250C7D707D,$3230022A322D7D7D,$71382D32312E7539,$6C257570742D716F,$7D7D5750746F2576
   Data.q $287331313C3E7D7D,$2E342F78757D3433,$2E342F787D716D38,$2E342F787D716C38,$2E342F787D716F38
   Data.q $2E342F787D716E38,$2E342F787D716938,$2E342F787D716838,$2E342F787D716B38,$1108107D71746A38
   Data.q $78757D710D191210,$253E787D716D253E,$716F253E787D716C,$787D716E253E787D,$253E787D7169253E
   Data.q $716B253E787D7168,$7D7D716A253E787D,$3E787D716D253E78,$6F253E787D716C25,$7D716E253E787D71
   Data.q $3E787D7169253E78,$6B253E787D716825,$66746A253E787D71,$3C3E7D7D7D7D5750,$757D343328733131
   Data.q $7D716D382E342F78,$7D716C382E342F78,$7D716F382E342F78,$7D716E382E342F78,$7D7169382E342F78
   Data.q $7D7168382E342F78,$7D716B382E342F78,$71746A382E342F78,$0D1912101F080E7D,$382E342F78757D71
   Data.q $382E342F787D716D,$382E342F787D716C,$382E342F787D716F,$382E342F787D716E,$382E342F787D7169
   Data.q $382E342F787D7168,$382E342F787D716B,$6D1C25787D7D716A,$7D716C1C25787D71,$25787D716F1C2578
   Data.q $691C25787D716E1C,$7D71681C25787D71,$25787D716B1C2578,$7D7D575066746A1C,$287331313C3E7D7D
   Data.q $052A3833757D3433,$6C052A38337D716D,$716F052A38337D71,$7D716E052A38337D,$337D7169052A3833
   Data.q $38337D7168052A38,$2A38337D716B052A,$1F080E7D71746A05,$78757D710D191210,$787D716D382E342F
   Data.q $787D716C382E342F,$787D716F382E342F,$787D716E382E342F,$787D7169382E342F,$787D7168382E342F
   Data.q $787D716B382E342F,$7D7D716A382E342F,$25787D716D1F2578,$6F1F25787D716C1F,$7D716E1F25787D71
   Data.q $25787D71691F2578,$6B1F25787D71681F,$66746A1F25787D71,$57507D7D7D7D5750,$2938312D30323E79
   Data.q $391C353E293C1F38,$2832193529340A39,$6F0914051838313F,$7D57507D7D7D7D67,$13141B72727D7D7D
   Data.q $181A1C090E7D111C,$180F1C0D10121E7D,$500E091314120D7D,$7D7D57507D7D7D57,$287331313C3E7D7D
   Data.q $2C382E34757D3433,$6F382E342F787D71,$191308121B7D7174,$1813090F120E1314,$052A3833757D710A
   Data.q $6C052A38337D716D,$712F292D0D027D71,$3E3F282D7D716D7D,$71092F3829332832,$71362E3C3009157D
   Data.q $2E3038293409157D,$7D7D7D7D57506674,$57507D7D7D7D5750,$2D29382E7D7D7D7D,$7D6F6E2873343573
   Data.q $6D712C382E34712D,$DE8C727254575066,$E08DED8DDF8CDC8C,$E58DE68DEF8DED8D,$E18DE88DED8DEF8D
   Data.q $8DED8DE68DD98C7D,$3C7D2D1D545750EE,$3F32313A73303229,$287339393C73313C,$2E342F787D7D6F6E
   Data.q $6D392F7806716D38,$50662C382E347100,$7D57507D7D7D7D57,$3F7339333C7D7D7D,$78547D7D7D7D6F6E
   Data.q $2F78716D382E342F,$256D7D716D382E34,$7D5750661B1B1B1B,$2873292B3E7D7D7D,$7D7D6F6E2873696B
   Data.q $6D382E342F787129,$507D7D7D7D575066,$393C307D2D1D5457,$7D696B2873323173,$657129712F292D10
   Data.q $7D7D666D392F7871,$E88DDF8C72727D7D,$D18CDD8CE88DE28D,$8DEB8DED8DE78D7D,$EC8D7DE48DD68CE9
   Data.q $8D06E78DE38DE68D,$71D18CDF8CE58DE0,$ED8DEC8DDE8CE28D,$7D00E58DE98DE48D,$8DDC8C7DED8DE08D
   Data.q $7DE18DE88DE38DEF,$DF8CDC8CE88DE18D,$7D7D7D7D5750E88D,$393C7D7D7D7D5750,$2F787D6F6E287339
   Data.q $35297D716C382E34,$6F6E3914393C382F,$322F253C10027D71,$7D7D575066393328,$7D7D7D7D57507D7D
   Data.q $313A73292E7D2D1D,$736F2B73313C3F32,$292D10067D6F6E28,$6C2671006B6C762F,$206C382E342F7871
   Data.q $507D7D7D7D575066,$7D7D7D7D7D7D7D57,$30323E797D57507D,$293C1F382938312D,$29340A39391C353E
   Data.q $0C38313F28321935,$507D7D7D67293428,$7272727D7D7D7D57,$7272727272727272,$7272727272727272
   Data.q $7272727272727272,$507D7D7D7D575072,$3E72727D7D7D7D57,$2873696B2873292B,$352971297D7D6F6E
   Data.q $6F6E3914393C382F,$507D7D7D7D575066,$73393C3072725457,$107D696B28733231,$71657129712F292D
   Data.q $7D5750666D392F78,$73292E72727D7D7D,$2B73313C3F32313A,$10067D6F6E28736F,$71006B6C762F292D
   Data.q $7D716D052A383326,$7D66206C052A3833,$57507D7D7D7D5750,$333C72727D7D7D7D,$7D7D7D6F6E3F7339
   Data.q $716C052A3833547D,$7D716C052A38337D,$1B1B1B1B1B6C256D,$7D7D7D7D5750661B,$6B2873292B3E7272
   Data.q $297D7D6F6E287369,$666C052A38337D71,$72727D7D7D7D5750,$2873323173393C30,$65712971297D696B
   Data.q $50662F292D0D0271,$3172727D7D7D7D57,$313C3F32313A7339,$7D6F6E28736F2B73,$716D052A38332654
   Data.q $71207D6C052A3833,$3172727D66002906,$3827342E7D293B38,$7D29353A342F7D71,$575029382E3B3B32
   Data.q $352E72727D7D7D7D,$38337D6F6E3F7331,$052A3833716C052A,$7672727D666E716C,$6869686E69656B6F
   Data.q $507D7D7D7D57506B,$3E72727D7D7D7D57,$2873696B2873292B,$337D71297D7D6F6E,$7D5750666C052A38
   Data.q $39393C72727D7D7D,$2971297D696B2873,$69686E69656B6F71,$7D7D7D5750666B68,$287339393C72727D
   Data.q $02712971297D696B,$7D5750662F292D0D,$7D7D7D57507D7D7D,$313A73393172727D,$736F2B73313C3F32
   Data.q $383326547D6F6E28,$052A3833716D052A,$6600290671207D6C,$72727D7D7D7D5750,$3C3F32313A73292E
   Data.q $6F6E28736F2B7331,$6F762F292D10067D,$052A383326710069,$6C052A38337D716D,$7D7D7D57507D6620
   Data.q $507D7D7D7D57507D,$7D57507D7D7D7D57,$7D7D7D57507D7D7D,$2F7D7D7D7D57507D,$5066343328732938
   Data.q $57507D5750207D57,$38313F342E342B73,$7D242F293338737D,$50756C292E382902,$303C2F3C2D735457
   Data.q $29027D696B28737D,$57506D026C292E38,$7354575026575074,$696B3F737D3A382F,$7D716D392F78547D
   Data.q $712F292D107D7129,$7D712F292D0D257D,$397D712F292D0D24,$712F292D0D3B3B34,$712F290D3F280D7D
   Data.q $29393C382F35297D,$3B320471313C2932,$34197D7129382E3B,$29382E3B3B323B3B,$737D7D7D7D575066
   Data.q $382F2D737D3A382F,$7D575054662D7D39,$7D3A382F737D7D7D,$3F78547D6F6E3F73,$613E787D71636561
   Data.q $65613C787D716365,$7D7D7D7D57506663,$6E3F737D3A382F73,$2D3038297D547D6F,$320F253C307D7D71
   Data.q $35297D7D71393328,$6F6E3914393C382F,$382F735457505466,$7D7D6B6C3F737D3A,$19393C382F35297D
   Data.q $32313F7D7D713034,$3F7D71303419363E,$71253914363E3231,$14393C382F35297D,$7D7D7D5750662539
   Data.q $507D7D7D7D57507D,$2B32307D7D7D7D57,$313F7D7D6B6C2873,$7871253914363E32,$66257339343C293E
   Data.q $28732B3230545750,$3E32313F7D7D6B6C,$3E33787130341936,$5066257339343C29,$6C28732B32305457
   Data.q $3C382F35297D7D6B,$2933787130341939,$5457506625733934,$7D6B6C28732B3230,$14393C382F35297D
   Data.q $7339342978712539,$7D7D7D7D57506625,$7D7D7D7D7D7D5750,$727D7D7D7D57507D,$32337D393C323172
   Data.q $3328323E7D383E33,$7D2F382D7D2F3829,$5750393C382F3529,$2D7339317D7D7D7D,$696B2873303C2F3C
   Data.q $7D716D392F78547D,$026C292E38290206,$507D7D7D7D66006D,$7339317D7D7D7D57,$2873313C3F32313A
   Data.q $320F253C307D6F6E,$392F780671393328,$7D5750660065766D,$7D7D7D57507D7D7D,$727D7D7D7D57507D
   Data.q $3C31283E313C3E72,$3B3B327D047D3829,$7D7D7D575029382E,$39342A733128307D,$38297D6B6C287338
   Data.q $363E32313F712D30,$382F352971303419,$575066303419393C,$733128307D7D7D7D,$297D6F6E28733231
   Data.q $2D303829712D3038,$727D7D7D666F6E71,$2E392F322A7D6572,$353E3C387D3F697D,$28307D7D7D7D5750
   Data.q $28733839342A7331,$2E3B3B32047D6F6E,$712D303829712938,$393328320F253C30,$507D7D7D7D575066
   Data.q $3E72727D7D7D7D57,$38293C31283E313C,$3B327D3B3B34397D,$7D57507D29382E3B,$287339393C7D7D7D
   Data.q $3B3B34197D7D696B,$7D7129382E3B3B32,$7129382E3B3B3204,$6629382E3B3B3204,$72727D7D7D7D5750
   Data.q $2873323173393C30,$323B3B34197D6F6E,$38297129382E3B3B,$320F253C30712D30,$3B3B320471393328
   Data.q $7D7D57506629382E,$7D57505457507D7D,$7D7D7D57507D7D7D,$7D38363C3072727D,$7D2F38293334322D
   Data.q $2F2F3C057D2F323B,$7D7D7D7D5750243C,$7D696B28732B3230,$78712F292D0D257D,$7D7D5750666D392F
   Data.q $6B287339393C7D7D,$2F292D0D257D7D69,$712F292D0D257D71,$8C727266656F6C7D,$8DD48CE88DE18DDC
   Data.q $6F6E7DE18DE88DED,$DF8CDC8CE58DDC8C,$D68CE08DE18DE88D,$8D696B7D767DD88C,$8DEB8DE98DD08CE1
   Data.q $6B647D607DE78DE5,$57507D7D7D7D5750,$3C3072727D7D7D7D,$293334322D7D3836,$047D2F323B7D2F38
   Data.q $7D5750243C2F2F3C,$28732B32307D7D7D,$292D0D247D7D696B,$662F292D0D25712F,$72727D7D7D7D5750
   Data.q $73696B2873292B3E,$0471297D7D6F6E28,$506629382E3B3B32,$39393C7D7D7D7D57,$0D247D7D696B2873
   Data.q $2D0D247D712F292D,$3B3B32047D712F29,$7D7D57506629382E,$72727D7D7D7D5750,$34322D7D38363C30
   Data.q $2F323B7D2F382933,$2F2F3C3B3B34197D,$7D7D7D7D5750243C,$7D696B28732B3230,$292D0D3B3B34397D
   Data.q $662F292D0D25712F,$72727D7D7D7D5750,$73696B2873292B3E,$1971297D7D6F6E28,$382E3B3B323B3B34
   Data.q $7D7D7D7D57506629,$7D696B287339393C,$292D0D3B3B34397D,$0D3B3B34397D712F,$3B34197D712F292D
   Data.q $6629382E3B3B323B,$57507D7D7D7D5750,$3C3072727D7D7D7D,$293334322D7D3836,$1F7D2F323B7D2F38
   Data.q $3E343A3C10382E3C,$5750293334322D7D,$732B32307D7D7D7D,$292D107D7D696B28,$7D666D392F78712F
   Data.q $7D7D7D7D57507D7D,$7D696B287339393C,$107D712F292D107D,$666F6E7D712F292D,$29382E3B3B327272
   Data.q $3A3C307D2F323B7D,$293334322D7D3E34,$57507D7D7D7D5750,$507D7D7D7D7D7D7D,$7D57507D7D7D7D57
   Data.q $7D7D7D57507D7D7D,$181A1C090E72727D,$29383A7D637D6C7D,$347D313C2932297D,$507C382E2F382B33
   Data.q $313C3E7D7D7D7D57,$78757D3433287331,$716C3E787D716D3E,$3E787D716F3E787D,$7D71693E787D716E
   Data.q $6B3E787D71683E78,$7D71746A3E787D71,$293C1F33343A383F,$757D7139391C353E,$0D257D712F292D10
   Data.q $2D0D247D712F292D,$3B3B34397D712F29,$3C307D712F292D0D,$6674393328320F25,$57507D7D7D7D5750
   Data.q $31313C3E7D7D7D7D,$323E7D7D34332873,$3C1F382938312D30,$340A39391C353E29,$38313F2832193529
   Data.q $712F292D10757D71,$7D712F292D0D257D,$397D712F292D0D24,$712F292D0D3B3B34,$3328320F253C307D
   Data.q $7D716D3E787D7139,$6F3E787D716C3E78,$787D716E3E787D71,$71683E787D71693E,$3E787D716B3E787D
   Data.q $7D7D7D575066746A,$507D7D7D7D57507D,$7D57507D7D7D7D57,$382F5457507D7D7D,$5D2057506629
BSGS4_cuda_quad_htchangeble_v2end:
EndDataSection


; IDE Options = PureBasic 5.31 (Windows - x64)
; ExecutableFormat = Console
; CursorPosition = 169
; FirstLine = 149
; Folding = TXAg2DB1+---e-
; EnableThread
; EnableXP
; Executable = bsgscudaHT_1_7_3.exe
; DisableDebugger