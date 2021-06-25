*          DATA SET ACCLB40B   AT LEVEL 208 AS OF 08/17/00                      
*PHASE T62140B                                                                  
CLB40    TITLE '- BILL PROGRAM ROUTINES'                                        
* DPEA 205 WHEN REVERSING W/O'S ENSRUE ALL AMOUNTS REVERSED CORRECTLY           
CLB40    CSECT                                                                  
         PRINT NOGEN                                                            
ROUT     NMOD1 0,**CB40**,RR=R8                                                 
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
*                                                                               
         SRL   RF,24               RF = A(ROUTINE,SIZE OF LOCAL W/S)            
         SLL   RF,3                                                             
         LA    RF,AROUT(RF)                                                     
         A     R8,0(RF)            R8 = A(ROUTINE)                              
*                                                                               
         ICM   R3,15,4(RF)         TEST FOR W/S REQUIRED                        
         BZ    ROUT02                                                           
         BCTR  R3,0                                                             
         SRL   R3,3                ENSURE DOUBLE WORD LENGTH                    
         LA    R3,1(R3)                                                         
         SLL   R3,3                                                             
         L     RF,4(RD)            EXTEND W/S                                   
         AR    RD,R3                                                            
         ST    RD,8(RF)                                                         
         ST    RF,4(RD)                                                         
         LR    R2,RC               CLEAR W/S                                    
         XR    RF,RF                                                            
         MVCL  R2,RE                                                            
*                                                                               
ROUT02   DS    0H                  BRANCH TO ROUTINE                            
         BR    R8                                                               
*                                                                               
AROUT    DS    0A                  ROUTINES / LOCAL W/S SIZE                    
         DC    A(FMTBLK,FWORKL)                                                 
         DC    A(RECACT,0)                                                      
         DC    A(OVRSCR,0)                                                      
         DC    A(BLDPFK,BPWORKL)                                                
         DC    A(SETCLM,SCWORKL)                                                
         DC    A(NTRSES,NSWORKL)                                                
         DC    A(XITSES,XSWORKL)                                                
         DC    A(TSTSEC,0)                                                      
         DC    A(SETMSK,0)                                                      
         DC    A(GETOPT,GJWORKL)                                                
         DC    A(ALLTRN,ATWORKL)                                                
         DC    A(GETPTA,GPWORKL)                                                
         DC    A(PUTPTA,PPWORKL)                                                
         DC    A(WOPPTA,WPWORKL)                                                
         DC    A(GETGRB,0)                                                      
         DC    A(SUBTOT,STWORKL)                                                
         DC    A(FMTHED,FHWORKL)                                                
         DC    A(BLDTRN,0)                                                      
         DC    A(CMPPRF,0)                                                      
         DC    A(ADDOBH,BHWORKL)                                                
         DC    A(TSTPFK,0)                                                      
         DC    A(FNDCLM,FCWORKL)                                                
         DC    A(GETWCD,GWWORKL)                                                
*                                                                               
ROUTL    MVI   BCDUB,0             SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   BCDUB,2             SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   BCDUB,1             SET CC EQUAL                                 
ROUTCC   CLI   BCDUB,1                                                          
*                                                                               
ROUTX    XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE ACCOUNT                                         *         
*                                                                     *         
* NTRY: R1=A(UNIT/LEDGER/ACCOUNT CODE)                                *         
***********************************************************************         
         SPACE 1                                                                
VALCOD   NTR1  ,                                                                
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,0(R1)                                                    
         GOTO1 AGETACT,0                                                        
         BNE   ROUTL                                                            
         TM    ACBSTAT,ACBSCLSE+ACBSLOCK                                        
         BZ    ROUTE                                                            
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         TM    ACBSTAT,ACBSCLSE                                                 
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     ROUTL                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO GET EXISTING STAFF/DEPT CODES FROM PTA RECORD            *         
*                                                                     *         
* NTRY: P1=A(PTA RECORD)                                              *         
*       P2=A(AREA FOR DEPARTMENT CODE)                                *         
*       P3=A(AREA FOR STAFF CODE)                                     *         
*       P4=A(AREA FOR ANALYSIS CODE) (US ONLY)                        *         
***********************************************************************         
         SPACE 1                                                                
GETCODS  NTR1  ,                                                                
         LM    R1,R4,0(R1)                                                      
         MVC   0(L'ACTKACT,R2),BCSPACES                                         
         MVC   0(L'ACTKACT,R3),BCSPACES                                         
         LA    R1,PTARFST-PTARECD(R1)                                           
*&&US*&& MVC   0(L'TRNOFFC,R4),TRNOFFC-TRNELD(R1)                               
*                                                                               
         XR    RF,RF                                                            
GCODS02  CLI   0(R1),0                                                          
         BE    GCODS10                                                          
         CLI   0(R1),SPAELQ                                                     
         BNE   GCODS06                                                          
         USING SPAELD,R1                                                        
         CLI   SPATYPE,SPATW2PA    EXTRACT STAFF CODE                           
         BNE   GCODS04                                                          
         MVC   0(L'ACTKACT,R3),SPAAACT                                          
         B     GCODS08                                                          
GCODS04  CLI   SPATYPE,SPATW2DA    EXTRACT DEPARTMENT CODE                      
         BNE   GCODS08                                                          
         MVC   0(L'ACTKACT,R2),SPAAACT                                          
         B     GCODS08                                                          
GCODS06  DS    0H                                                               
*&&US                                                                           
         CLI   0(R1),ANOELQ        EXTRACT ANALYSIS OFFICE                      
         BNE   GCODS08                                                          
         USING ANOELD,R1                                                        
         CLI   ANOTYPE,ANOTPER                                                  
         BNE   GCODS08                                                          
         MVC   0(L'ANOOFFC,R4),ANOOFFC                                          
*&&                                                                             
         DROP  R1                                                               
GCODS08  IC    RF,1(R1)                                                         
         BXH   R1,RF,GCODS02                                                    
*                                                                               
GCODS10  DS    0H                                                               
*&&US                                                                           
         CLC   0(L'SPAAACT,R2),BCSPACES                                         
         BE    GETCODSX                                                         
         LA    RF,L'SPAAACT                                                     
         LA    RE,L'SPAAACT-1(R2)                                               
GCODS12  CLI   0(RE),C' '                                                       
         BH    GCODS14                                                          
         BCTR  RE,0                                                             
         BCT   RF,GCODS12                                                       
         DC    H'0'                                                             
GCODS14  MVC   0(L'SPAAACT-1,R3),1(R3)                                          
         MVI   L'SPAAACT-1(R3),C' '                                             
         BCT   RF,GCODS14                                                       
*&&                                                                             
         MVC   0(L'ACTKACT-1,R2),1(R2)                                          
         MVI   L'SPAAACT-1(R2),C' '                                             
         TM    BCCPYST4,CPYSOFF2   TEST NEW OFFICE SYSTEM                       
         BZ    GETCODSX                                                         
         MVC   0(L'ACTKACT-1,R2),1(R2)                                          
         MVI   L'SPAAACT-1(R2),C' '                                             
GETCODSX B     ROUTX                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST SYSTEM/COUNTRY FILTERS                              *         
*                                                                     *         
* NTRY: R1=A(SYSTEM/COUNTRY)                                          *         
***********************************************************************         
         SPACE 1                                                                
TESTSC   SR    RF,RF                                                            
         ICM   RF,3,0(R1)          TEST ALL SYSTEMS/COUTRIES                    
         BZR   RE                  YES - CC=EQUAL                               
*                                                                               
         CLI   0(R1),0             TEST ALL SYSTEMS                             
         BE    TSC02                                                            
*&&UK*&& CLI   0(R1),SYSUK         MATCH ON UK SYSTEM                           
*&&US*&& CLI   0(R1),SYSUS         MATCH ON US SYSTEM                           
         BNER  RE                  RETURN WITH CC=NOT EQUAL                     
*                                                                               
TSC02    CLI   1(R1),0             TEST ALL COUNTRIES                           
         BER   RE                                                               
         CLC   CUCTRY,1(R1)        MATCH ON CONNECTED COUNTRY                   
         BER   RE                                                               
         TM    1(R1),CTRYNOT       TEST ALL BUT A COUNTRY                       
         BZ    TESTSCN                                                          
         MVC   BCBYTE1,1(R1)                                                    
         XI    BCBYTE1,CTRYNOT                                                  
         CLC   BCBYTE1,CUCTRY                                                   
         BNE   TESTSCY                                                          
*                                                                               
TESTSCN  LTR   RE,RE               CC=NOT EQUAL                                 
         BR    RE                                                               
TESTSCY  CR    RE,RE               CC=EQUAL                                     
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* SPACE FOR LITERALS                                                  *         
***********************************************************************         
         SPACE 1                                                                
LTORG    DS    0H                                                               
         ORG   CLB40+X'400'                                                     
LTORGX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* FORMAT OPTION BLOCK ROUTINES                                        *         
*                                                                     *         
* NTRY: P1 BYTE 0 = 'FBGET' TO CONVERT RECORD INTO BLOCK              *         
*                 = 'FBPUT' TO CONVERT BLOCK INTO RECORD              *         
*                 = 'FBDIS' TO DISPLAY OPTION (IN FVIFLD)             *         
*                 = 'FBVAL' TO VALIDATE OPTION (IN FVIFLD)            *         
*             1-3 = A(FORMAT OPTION BLOCK)                            *         
*       P2        = A(FORMAT CONTROL RECORD)  (FBGET/FBPUT)           *         
*  OR   P2 BYTE 3 = FORMAT OPTION NUMBER      (FBDIS/FBVAL)           *         
***********************************************************************         
         SPACE 1                                                                
         USING FWORKD,RC           RC=A(LOCAL W/S)                              
FMTBLK   DS    0H                                                               
         USING *,R8                                                             
         MVC   FWINPUT,0(R1)                                                    
         L     R6,0(R1)                                                         
         USING FBLKD,R6            R6=A(FORMAT BLOCK)                           
         USING BOFELD,FBBOFEL                                                   
*                                                                               
         XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     FMTGET                                                           
         B     FMTPUT                                                           
         B     FMTVAL                                                           
         B     FMTDIS                                                           
         SPACE 1                                                                
***********************************************************************         
* - SET UP FBLK FROM RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
FMTGET   LA    RF,L'FBLK           CLEAR BLOCK                                  
         LA    RE,FBLK                                                          
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R2,FWAREC                                                        
         USING PBCRECD,R2                                                       
         MVC   FBNUMB,PBCKFMT                                                   
         MVC   FBLANG,PBCKLANG                                                  
         MVC   FBGRPB,PBCRGRPB                                                  
         MVC   FBNAME,BCSPACES                                                  
*                                                                               
         L     R3,FWAREC                                                        
         LA    R3,PBCRFST                                                       
         XR    RF,RF                                                            
FGET02   CLI   0(R3),0                                                          
         BE    FGET10                                                           
         CLI   0(R3),NAMELQ        TEST NAME ELEMENT                            
         BNE   FGET04                                                           
         IC    RE,1(R3)                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   FBNAME(0),NAMEREC-NAMELD(R3)                                     
         B     FGET08                                                           
FGET04   CLI   0(R3),BOFELQ                                                     
         BNE   FGET08                                                           
         MVC   BOFELD(BOFLNQ),0(R3)                                             
FGET08   IC    RF,1(R3)                                                         
         BXH   R3,RF,FGET02                                                     
*                                                                               
FGET10   L     R4,AFOPTAB                                                       
         USING FOPTABD,R4                                                       
         LA    R0,FOPTABN                                                       
*                                                                               
FGET12   CLI   FOPNUM,0            TEST OPTION ENTRY                            
         BE    FGET28                                                           
         CLI   FOPTYPE,FOPTFREE    TEST FREE FORM TEXT                          
         BNE   FGET20                                                           
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,3,FOPFBLK                                                     
         LA    R3,FBLKD(R3)        R3=A(OUTPUT FOR HEADING)                     
         IC    RF,FOPFLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R3),BCSPACES                                                 
         LA    R1,PBCRFST                                                       
         USING FFTELD,R1           SEARCH FOR ELEMENT IN RECORD                 
         XR    RF,RF                                                            
FGET14   CLI   FFTEL,0                                                          
         BE    FGET18                                                           
         IC    RF,FFTLN                                                         
         CLI   FFTEL,FFTELQ                                                     
         BNE   *+14                                                             
         CLC   FFTTYPE,FOPFTYPE                                                 
         BE    *+8                                                              
         BXH   R1,RF,FGET14                                                     
         SH    RF,=Y(FFTDATA+1-FFTELD)                                          
         EX    RF,*+4                                                           
         MVC   0(0,R3),FFTDATA                                                  
         B     FGET28                                                           
         DROP  R1                                                               
FGET18   XR    RE,RE               COPY DEFAULT VALUE                           
         ICM   RE,3,FOPFDEF                                                     
         LA    RE,TWAD(RE)                                                      
         ICM   RF,1,FOPFLEN                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R3),BCSPACES                                                 
         ICM   RF,1,FOPFDLEN                                                    
         BNZ   *+8                                                              
         LA    RF,FBDEFLQ                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R3),0(RE)                                                    
         B     FGET28                                                           
*                                                                               
FGET20   CLI   BOFEL,BOFELQ        TEST DEFAULT BOFELD VALUES REQUIRED          
         BE    FGET28                                                           
         XR    RE,RE                                                            
         ICM   RE,1,FOPDISP                                                     
         BZ    FGET28                                                           
         LA    RE,BOFELD(RE)                                                    
         OC    0(1,RE),FOPDEF                                                   
*                                                                               
FGET28   LA    R4,FOPTABL(R4)                                                   
         BCT   R0,FGET12                                                        
*                                                                               
FGET30   MVI   BOFEL,BOFELQ                                                     
         MVI   BOFLN,BOFLNQ                                                     
         B     ROUTX                                                            
         DROP  R4,R2                                                            
         SPACE 1                                                                
***********************************************************************         
* - UPDATE RECORD FROM FBLK                                           *         
***********************************************************************         
         SPACE 1                                                                
FMTPUT   L     R2,FWAREC                                                        
         USING PBCRECD,R2                                                       
         MVC   PBCRGRPB,FBGRPB                                                  
         LA    R1,PBCRFST                                                       
         XR    RF,RF                                                            
FPUT02   CLI   0(R1),0                                                          
         BE    FPUT10                                                           
         CLI   0(R1),NAMELQ                                                     
         BE    FPUT04                                                           
         CLI   0(R1),BOFELQ                                                     
         BE    FPUT04                                                           
         CLI   0(R1),FFTELQ                                                     
         BNE   FPUT08                                                           
FPUT04   MVI   0(R1),FF                                                         
*                                                                               
FPUT08   IC    RF,1(R1)                                                         
         BXH   R1,RF,FPUT02                                                     
*                                                                               
FPUT10   GOTO1 VHELLO,FWPARM,(C'D',ACCMST),('FF',PBCRECD),0                     
*                                                                               
         LA    R3,BOELEM                                                        
         USING NAMELD,R3                                                        
         MVI   NAMEL,NAMELQ                                                     
         MVC   NAMEREC,FBNAME                                                   
         LA    RF,NAMEREC+L'NAMEREC-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R3                                                            
         LA    RF,1(RF)                                                         
         STC   RF,NAMLN                                                         
         GOTO1 VHELLO,FWPARM,(C'P',ACCMST),PBCRECD,NAMELD                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
         GOTO1 (RF),(R1),,,BOFELD                                               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AFOPTAB                                                       
         USING FOPTABD,R4                                                       
         LA    R0,FOPTABN                                                       
*                                                                               
FPUT12   CLI   FOPNUM,0            TEST OPTION ENTRY                            
         BE    FPUT18                                                           
         CLI   FOPTYPE,FOPTFREE    TEST FREE FORM TEXT                          
         BNE   FPUT18                                                           
         XR    RF,RF                                                            
         ICM   RF,3,FOPFBLK                                                     
         LA    RF,FBLKD(RF)        RF=A(OUTPUT FOR HEADING)                     
         XR    RE,RE                                                            
         ICM   RE,3,FOPFDEF                                                     
         LA    RE,TWAD(RE)                                                      
         MVC   WORK,BCSPACES                                                    
         ICM   R1,1,FOPFDLEN                                                    
         BNZ   *+8                                                              
         LA    R1,FBDEFLQ                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK(0),0(RE)                                                    
         IC    R1,FOPFLEN          TEST IS DEFAULT VALUE                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BE    FPUT18                                                           
         CLC   0(0,RF),WORK                                                     
         LA    R3,BOELEM                                                        
         USING FFTELD,R3                                                        
         EX    R1,*+4                                                           
         MVC   FFTDATA(0),0(RF)                                                 
         MVI   FFTEL,FFTELQ                                                     
         MVC   FFTDLEN,FOPFLEN                                                  
         LA    R1,FFTLN1Q+L'FFTDLEN+1(R1)                                       
         STC   R1,FFTLN                                                         
         MVC   FFTTYPE,FOPFTYPE                                                 
         GOTO1 VHELLO,FWPARM,(C'P',ACCMST),PBCRECD,FFTELD                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
FPUT18   LA    R4,FOPTABL(R4)                                                   
         BCT   R0,FPUT12                                                        
*                                                                               
FPUT20   GOTO1 APUTRAC,FWPARM,('RACTCHA',PBCRECD)  UPDATE ACTIVITY EL.          
         B     ROUTX                                                            
         DROP  R2,R4                                                            
         SPACE 1                                                                
***********************************************************************         
* - VALIDATE INPUT FOR FORMAT OPTION                                  *         
***********************************************************************         
         SPACE 1                                                                
FMTVAL   CLI   FVILEN,0                                                         
         BE    FMTBLKY                                                          
         OI    LSINDS1,LSIUPREC                                                 
         XR    R4,R4                                                            
         IC    R4,FWNUM                                                         
         MH    R4,=Y(FOPTABL)                                                   
         A     R4,AFOPTAB                                                       
         USING FOPTABD,R4                                                       
         CLI   FOPTYPE,FOPTNUM                                                  
         BE    VALNUM                                                           
         CLI   FOPTYPE,FOPTKWRD                                                 
         BE    VALKWRD                                                          
         CLI   FOPTYPE,FOPTFREE                                                 
         BE    VALFREE                                                          
         DC    H'0'                                                             
*                                                                               
VALNUM   XR    R3,R3                                                            
         IC    R3,FOPDISP                                                       
         LA    R3,BOFELD(R3)                                                    
         GOTO1 AVALAMT,BOPARM,FVIHDR,(1,(R3))                                   
         BNE   FMTBLKN                                                          
         CLC   FOPNMIN,0(R3)                                                    
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$FLVTS)                                           
         B     FMTBLKN                                                          
         CLC   FOPNMAX,0(R3)                                                    
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$FLVTL)                                           
         B     FMTBLKN                                                          
         B     FMTBLKY                                                          
*                                                                               
VALKWRD  XR    R3,R3                                                            
         IC    R3,FOPDISP                                                       
         LA    R3,BOFELD(R3)                                                    
         LA    R1,FOPKLIST                                                      
         USING FOPKLIST,R1                                                      
VKWRD02  CLI   FOPKLIST,EOT                                                     
         BE    VKWRD10                                                          
         MVC   BOBYTE1,FOPKVAL                                                  
         XI    BOBYTE1,X'FF'                                                    
         NC    0(1,R3),BOBYTE1                                                  
         LA    R1,L'FOPKLIST(R1)                                                
         B     VKWRD02                                                          
         DROP  R1                                                               
*                                                                               
VKWRD10  LA    R1,FOPKLIST                                                      
         USING FOPKLIST,R1                                                      
VKWRD12  CLI   FOPKLIST,EOT                                                     
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     FMTBLKN                                                          
         XR    RE,RE                                                            
         ICM   RE,3,FOPKWRD                                                     
         LA    RE,TWAD(RE)                                                      
         IC    RF,FVXLEN                                                        
         CLC   FVIFLD(0),0(RE)                                                  
         EX    RF,*-6                                                           
         BE    VKWRD20                                                          
         LA    R1,L'FOPKLIST(R1)                                                
         B     VKWRD12                                                          
*                                                                               
VKWRD20  CLI   FOPKTYPE,FOPKOFF                                                 
         BE    *+10                                                             
         OC    0(1,R3),FOPKVAL                                                  
         B     FMTBLKY                                                          
         DROP  R1                                                               
*                                                                               
VALFREE  XR    RE,RE                                                            
         ICM   RE,3,FOPFBLK                                                     
         LA    RE,FBLKD(RE)                                                     
         IC    RF,FOPFLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),FVIFLD                                                   
         B     FMTBLKY                                                          
         SPACE 1                                                                
         DROP  R4                                                               
         SPACE 1                                                                
***********************************************************************         
* - DISPLAY OUTPUT FOR FORMAT OPTION                                  *         
***********************************************************************         
         SPACE 1                                                                
FMTDIS   XR    R4,R4                                                            
         IC    R4,FWNUM                                                         
         MH    R4,=Y(FOPTABL)                                                   
         A     R4,AFOPTAB                                                       
         USING FOPTABD,R4                                                       
         CLI   FOPTYPE,FOPTNUM                                                  
         BE    DISNUM                                                           
         CLI   FOPTYPE,FOPTKWRD                                                 
         BE    DISKWRD                                                          
         CLI   FOPTYPE,FOPTFREE                                                 
         BE    DISFREE                                                          
         DC    H'0'                                                             
*                                                                               
DISNUM   XR    RE,RE                                                            
         IC    RE,FOPDISP                                                       
         LA    RE,BOFELD(RE)                                                    
         XC    BOHALF1,BOHALF1                                                  
         MVC   BOHALF1+1(1),0(RE)                                               
         EDIT  (B2,BOHALF1),(3,FVIFLD),ALIGN=LEFT,ZERO=NOBLANK                  
         B     ROUTX                                                            
*                                                                               
DISKWRD  XR    RE,RE                                                            
         IC    RE,FOPDISP                                                       
         LA    RE,BOFELD(RE)                                                    
         MVC   BOBYTE1,0(RE)                                                    
         LA    R1,FOPKLIST                                                      
         USING FOPKLIST,R1                                                      
DKWRD02  CLI   FOPKLIST,EOT                                                     
         BE    ROUTX                                                            
         MVC   BOBYTE2,FOPKVAL                                                  
         CLI   FOPKTYPE,FOPKON                                                  
         BNE   DKWRD04                                                          
         NC    BOBYTE2,BOBYTE1                                                  
         BNZ   DKWRD10                                                          
         B     DKWRD08                                                          
*                                                                               
DKWRD04  CLI   FOPKTYPE,FOPKOFF                                                 
         BNE   DKWRD06                                                          
         NC    BOBYTE2,BOBYTE1                                                  
         BZ    DKWRD10                                                          
         B     DKWRD08                                                          
*                                                                               
DKWRD06  CLI   FOPKTYPE,FOPKEQU                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BOBYTE1,BOBYTE2                                                  
         BE    DKWRD10                                                          
*                                                                               
DKWRD08  LA    R1,L'FOPKLIST(R1)                                                
         B     DKWRD02                                                          
*                                                                               
DKWRD10  XR    RE,RE                                                            
         ICM   RE,3,FOPKWRD                                                     
         LA    RE,TWAD(RE)                                                      
         IC    RF,FOPKLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),0(RE)                                                  
         B     ROUTX                                                            
         DROP  R1                                                               
*                                                                               
DISFREE  XR    RE,RE                                                            
         ICM   RE,3,FOPFBLK                                                     
         LA    RE,FBLKD(RE)                                                     
         IC    RF,FOPFLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),0(RE)                                                  
         B     ROUTX                                                            
         SPACE 1                                                                
         DROP  R4                                                               
         SPACE 1                                                                
FMTBLKN  B     ROUTL                                                            
FMTBLKY  B     ROUTE                                                            
         SPACE 1                                                                
         DROP  R6,RC                                                            
         SPACE 1                                                                
***********************************************************************         
* FMTBLK LOCAL W/S                                                    *         
***********************************************************************         
         SPACE 1                                                                
FWORKD   DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    XL64                                                             
FWAR1    DS    A                                                                
FWINPUT  DS    0XL8                * INPUT PARAMETERS *                         
FWACT    DS    0XL1                ACTION EQUATE                                
FWAFMT   DS    A                   A(FMTBLK)                                    
FWAREC   DS    A                   A(RECORD)                                    
         ORG   *-1                                                              
FWNUM    DS    XL1                 FORMAT OPTION NUMBER                         
*                                                                               
FWPARM   DS    6A                                                               
FWORKL   EQU   *-FWORKD                                                         
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SET NEW RECORD/ACTION WORDS                                         *         
*                                                                     *         
* NTRY - R1=AL1(RECORD NUMBER,ACTION NUMBER)                          *         
***********************************************************************         
         SPACE 1                                                                
RECACT   DS    0H                                                               
         USING *,R8                                                             
         GOTO1 ATSTMIX             TEST VALID RECORD/ACTION COMBO               
         BNE   ROUTH                                                            
*                                                                               
         L     R3,AMIXNTRY                                                      
         USING MIXTABD,R3                                                       
         MVC   CSSCRN,MIXSCRN      EXTRACT MIXTAB VALUES                        
         MVC   CSOVER,MIXOVER                                                   
         MVC   CSMIX1,MIXINDS1                                                  
         MVC   CSMIX2,MIXINDS2                                                  
         MVC   CSQRTN,MIXQRTN                                                   
         MVC   LSMIXLST,MIXLST                                                  
*                                                                               
*&&UK*&& MVC   BASOPTX(L'MIXOPTH#),MIXOPTH#                                     
*                                                                               
         CLC   CSREC,MIXRECB       TEST CHANGE OF RECORD TYPE                   
         MVC   CSREC,MIXRECB       SET RECORD NUMBER                            
         BE    *+8                                                              
         OI    BCINDS1,BCINREC+BCINACT                                          
         CLC   CSACT,MIXACTB       TEST CHANGE OF ACTION                        
         MVC   CSACT,MIXACTB       SET ACTION NUMBER                            
         BE    *+8                                                              
         OI    BCINDS1,BCINACT                                                  
         DROP  R3                                                               
*                                                                               
*        CLI   CSACT,ACTHELP       DON'T OUTPUT WORD IF HELP                    
*        BE    RECACTX                                                          
         L     R2,AACTTAB                                                       
         USING ACTTABD,R2                                                       
RECACT4  CLI   ACTTABD,EOT         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ACTNUMB,CSACT                                                    
         BE    *+12                                                             
         LA    R2,ACTTABL(R2)                                                   
         B     RECACT4                                                          
         XR    RF,RF                                                            
         ICM   RF,3,ACTNAMEL                                                    
         LA    RF,TWAD(RF)                                                      
         MVC   BASACT,0(RF)        OUTPUT ACTION WORD AND TRANSMIT              
         OI    BASACTH+FHOID,FHOITR                                             
         OI    BASACTH+FHIID,FHIIVA                                             
         MVC   CSACTNAM,BASACT                                                  
         DROP  R2                                                               
*                                                                               
         TM    BCINDS1,BCINACT     TEST NEW ACTION                              
         BZ    RECACTX                                                          
         TM    CSMIX1,MIXILST      YES - TEST LIST TYPE ACTION                  
         BO    RECACTX                                                          
         XC    BASOPT,BASOPT       NO - CLEAR OPTION FIELD                      
         OI    BASOPTH+FHOID,FHOITR                                             
*                                                                               
RECACTX  B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OVERLAY A SCREEN INTO TWA                                *         
*                                                                     *         
* NTRY - P1=AL1(SCREEN OVERLAY NUMBER),AL3(LOAD POINT)                *         
***********************************************************************         
         SPACE 1                                                                
OVRSCR   DS    0H                                                               
         USING *,R8                                                             
         ICM   R1,15,0(R1)         SET A(LOAD POINT)                            
         STCM  R1,8,BCBYTE1                                                     
         XC    BCPARM(16),BCPARM                                                
         STCM  R1,7,BCPARM+1       SET LOAD POINT                               
         MVI   BCPARM+4,C'R'                                                    
         MVC   BCPARM+5(2),=X'0621'                                             
         MVC   BCPARM+7(1),BCBYTE1 SET SCREEN OVERLAY NUMBER                    
         GOTO1 VCOLY,BCPARM                                                     
         CLI   4(R1),FF            TEST PHASE LOADED OK                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         B     ROUTH                                                            
         LA    R1,BASMSGH                                                       
         USING FHD,R1                                                           
         LA    RF,OSVALS-1                                                      
         XR    RE,RE                                                            
         OI    FHOI,FHOITR                                                      
         ICM   RE,1,FHLN                                                        
         BZ    *+10                                                             
         BXLE  R1,RE,*-12                                                       
         DC    H'0'                SCREEN TOO LARGE                             
         DROP  R1                                                               
         CLC   TWASCRN,BCBYTE1     TEST SCREEN SAME AS LAST                     
         BNE   *+10                                                             
         XC    1(2,R1),1(R1)       YES - DON'T SET BEFORE/AFTER                 
         MVC   TWASCRN,BCBYTE1                                                  
         MVI   TWASCRF,0                                                        
         CR    RE,RE               SET CONDITION CODE TO EQUAL                  
OVRSCRX  B     ROUTX                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PFKEY LINE                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING BPWORKD,RC                                                       
BLDPFK   DS    0H                                                               
         USING *,R8                                                             
         XR    RF,RF               LOCATE AND CLEAR PFKEY LINE                  
         LA    R2,BASOLAYH                                                      
         TM    CSMIX1,MIXILST                                                   
         BZ    BPFK02                                                           
         LH    R2,CS1STFTL                                                      
         LA    R2,TWAD(R2)                                                      
         USING FHD,R2                                                           
BPFK02   ICM   RF,1,FHLN                                                        
         BZ    BLDPFKX                                                          
         TM    FHAT,FHATXH                                                      
         BZ    BPFK08                                                           
         LA    R1,FHD(RF)                                                       
         SH    R1,=Y(FHDAD)                                                     
         CLI   0(R1),PFKFLDNO                                                   
         BE    BPFK10                                                           
BPFK08   BXH   R2,RF,BPFK02                                                     
BPFK10   ST    R2,BPALINE                                                       
         OI    FHOI,FHOITR                                                      
         OI    FHAT,FHATHI                                                      
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         EX    RF,*+4                                                           
         XC    FHDA(0),FHDA                                                     
         BCTR  RF,0                                                             
         STH   RF,BPLINEL          SAVE LENGTH OF LINE                          
         DROP  R2                                                               
*                                                                               
         MVC   BPRECACT,CSRECACT   LOCATE PFKEY TABLE HEADER ENTRY              
         CLI   CSACT,0                                                          
         BNE   *+10                                                             
         MVC   BPRECACT,BCEFFS                                                  
         L     R3,APFKTAB                                                       
         USING PFKTABD,R3                                                       
         XR    RF,RF                                                            
BPFK12   CLI   PFKTABD,EOT                                                      
         BE    BLDPFKX                                                          
         CLC   PFKKEY,BPRECACT                                                  
         BE    BPFK20                                                           
         ICM   RF,3,PFKLEN                                                      
         BXH   R3,RF,BPFK12                                                     
*                                                                               
BPFK20   LA    R3,PFKHEADL(R3)                                                  
         MVC   BPALEFT,AIO1        INITIALIZE ADDRESS OF PF LINES               
         L     RE,BPALEFT                                                       
         BCTR  RE,0                                                             
         MVI   0(RE),0                                                          
         MVC   BPALEFTX,BPALEFT                                                 
         MVC   BPARGHT,AIO2                                                     
         MVC   BPARGHTX,BPARGHT                                                 
*                                                                               
BPFK22   CLI   PFKTABD,EOT         ADD PFKEY DESCRIPTION TO LEFT/RIGHT          
         BE    BPFK30                                                           
         CLI   PFKNUMB,BCPFXITS                                                 
         BE    BPFK28                                                           
         GOTO1 ATSTPFK,PFKTABD                                                  
         BNE   BPFK28                                                           
         LA    R1,BPALEFTX                                                      
*&&UK                                                                           
         TM    PFKINDS1,PFKIQUIT+PFKINEXT                                       
         BZ    *+8                                                              
         LA    R1,BPARGHTX                                                      
         TM    PFKINDS3,PFKIRFSH                                                
         BZ    *+8                                                              
         LA    R1,BPARGHTX                                                      
*&&                                                                             
         GOTO1 BLDDSC,(R1)                                                      
BPFK28   LA    R3,PFKDATBL(R3)                                                  
         B     BPFK22                                                           
*                                                                               
BPFK30   L     RF,BPALEFTX         TEST ALL PFKEYS FIT ON ONE LINE              
         S     RF,BPALEFT                                                       
         L     RE,BPARGHTX                                                      
         S     RE,BPARGHT                                                       
         AR    RE,RF               TEST SOME PFKEYS                             
         BZ    BLDPFKX                                                          
         BCTR  RE,0                                                             
         CH    RE,BPLINEL                                                       
         BH    BPFK40                                                           
         MVI   CSPF#LIN,1                                                       
         MVI   CSPFLIN#,0                                                       
         L     R1,BPALEFT          R1=A(START OF LEFT)                          
         BCTR  RF,0                RF=L(LEFT)                                   
         B     BPFK50                                                           
*                                                                               
BPFK40   LA    R3,PFKMORE          ADD ' 13=MORE' TO RIGHT HAND SIDE            
         L     R1,BPARGHTX                                                      
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         ST    R1,BPARGHTX                                                      
         GOTO1 BLDDSC,BPARGHTX                                                  
         L     R1,BPARGHTX                                                      
         S     R1,BPARGHT                                                       
         BCTR  R1,0                                                             
         LH    RF,BPLINEL                                                       
         SR    RF,R1                                                            
         BCTR  RF,0                RF=MAX.LENGTH FOR EACH LHS                   
         L     R2,BPALEFT                                                       
         LA    R1,BPALIST          R1=A(LIST OF START ADDRESSES)                
         XR    R3,R3               R3=LINE COUNT                                
*                                                                               
BPFK42   ST    R2,0(R1)            SAVE A(CURRENT LINE)                         
         LA    R1,L'BPALIST(R1)                                                 
         LA    R3,1(R3)                                                         
BPFK44   LA    RE,0(R2,RF)         RE=A(MAX. END OF LNE)                        
         C     RE,BPALEFTX                                                      
         BNL   BPFK48                                                           
         CLI   0(RE),0                                                          
         BE    *+8                                                              
         BCT   RE,*-8                                                           
         LA    RE,1(RE)            RE=A(START OF NEXT LINE)                     
         CR    RE,R2               TEST HAVE BUMPED TO NEW LINE                 
         BNH   BPFK45                                                           
         LR    R2,RE                                                            
         B     BPFK42                                                           
*                                                                               
BPFK45   LA    RE,0(R2,RF)         'WORD' IN LHS TOO LONG TO FIT                
         AR    RE,R2               SO PUT NEW DELIMETER IN                      
         SRL   RE,1                AT SPACE NEAREST THE MIDDLE                  
         LR    R4,RE                                                            
BPFK46   CLI   0(RE),C' '                                                       
         BNE   *+12                                                             
         MVI   0(RE),0                                                          
         B     BPFK44                                                           
         BCTR  RE,0                                                             
         CR    RE,R2                                                            
         BH    *+6                                                              
         DC    H'0'                NO SPACE FOUND                               
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNE   BPFK46                                                           
         MVI   0(R4),0                                                          
         B     BPFK44                                                           
*                                                                               
BPFK48   MVC   0(L'BPALIST,R1),BPALEFTX                                         
         STC   R3,CSPF#LIN         SAVE NO. OF LINES                            
         CLC   CSPFLIN#,CSPF#LIN                                                
         BL    *+8                                                              
         MVI   CSPFLIN#,0                                                       
         XR    RE,RE                                                            
         IC    RE,CSPFLIN#                                                      
         SLL   RE,2                                                             
         L     R1,BPALIST(RE)      R1=A(START OF LINE)                          
         L     RF,BPALIST+L'BPALIST(RE)                                         
         SR    RF,R1                                                            
         BCTR  RF,0                RF=L(LEFT HAND SIDE)                         
*                                                                               
BPFK50   L     R2,BPALINE          OUTPUT 'PF'                                  
         LH    RE,=Y(UC@PFKEY-TWAD)                                             
         LA    RE,TWAD(RE)                                                      
         MVC   FHDAD(L'UC@PFKEY,R2),0(RE)                                       
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BM    BPFK52                                                           
         EX    RF,*+4              OUTPUT LEFT HAND SIDE                        
         MVC   FHDAD+L'UC@PFKEY(0,R2),0(R1)                                     
BPFK52   LA    R2,FHDAD+L'UC@PFKEY+2(R2,RF)                                     
         L     R1,BPARGHT          OUTPUT RIGHT HAND SIDE                       
         L     RF,BPARGHTX                                                      
         SR    RF,R1                                                            
         BZ    BLDPFKX                                                          
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R2),0(R1)                                                    
*                                                                               
BLDPFKX  LA    RE,BSVALS           ENSURE SAVED W/S UPDATED                     
         MVC   CSPF#LIN-BCVALS(L'CSPF#LIN,RE),CSPF#LIN                          
         MVC   CSPFLIN#-BCVALS(L'CSPFLIN#,RE),CSPFLIN#                          
         B     ROUTX                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO BUILD PFKEY DESCRIPTION                                  *         
*                                                                     *         
* NTRY: R1=A(ADDRESS OF END OF LINE)                                  *         
* EXIT: END OF LINE IS UPDATED                                        *         
***********************************************************************         
         SPACE 1                                                                
BLDDSC   NTR1  ,                                                                
         L     R2,0(R1)                                                         
*                                                                               
         TM    PFKINDS1,PFKISCRL   JOIN SUCCESSIVE SCROLL PFKEYS                
         BZ    BDSC02                                                           
         TM    BPINDS1,PFKISCRL                                                 
         BZ    BDSC02                                                           
         BCTR  R2,0                                                             
         MVI   0(R2),C' '                                                       
         LA    R2,1(R2)                                                         
*                                                                               
BDSC02   XR    RF,RF               OUTPUT PFKEY NUMBER                          
         IC    RF,PFKNUMB                                                       
         CVD   RF,BPDUB                                                         
         UNPK  0(2,R2),BPDUB                                                    
         OI    1(R2),C'0'                                                       
         CLI   0(R2),C'0'                                                       
         BNE   *+12                                                             
         MVC   0(1,R2),1(R2)                                                    
         BCTR  R2,0                                                             
         MVC   2(1,R2),BCEQUAL     OUTPUT '='                                   
         LA    R2,3(R2)                                                         
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,PFKNAMEL       TEST PFKEY NAME IN TABLE ENTRY               
         BZ    *+12                                                             
         LA    RE,TWAD(RE)                                                      
         B     BDSC10                                                           
*                                                                               
BDSC03   L     RF,AACTTAB          LOOKUP ACTION TABLE FOR NAME                 
         USING ACTTABD,RF                                                       
BDSC04   CLI   ACTTABD,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ACTNUMB,PFKACTN                                                  
         BE    *+12                                                             
         LA    RF,ACTTABL(RF)                                                   
         B     BDSC04                                                           
         XR    RE,RE                                                            
         ICM   RE,3,ACTNAMEL                                                    
         LA    RE,TWAD(RE)                                                      
         B     BDSC10                                                           
*                                                                               
BDSC10   MVC   0(PFKSCRLQ,R2),0(RE)                                             
         LA    R2,PFKSCRLQ-1(R2)                                                
         TM    PFKINDS2,PFKILONG   TEST LONG PFKEY DESCRIPTION                  
         BZ    *+14                                                             
         MVC   1(PFKSCRLQ,R2),PFKSCRLQ(RE)                                      
         LA    R2,PFKSCRLQ(R2)                                                  
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
         MVI   0(R2),0                                                          
         LA    R2,1(R2)                                                         
*                                                                               
BLDDSCX  ST    R2,0(R1)                                                         
         MVC   BPINDS1,PFKINDS1    SAVE LAST PFKEY INDICATORS                   
         B     ROUTX                                                            
         DROP  R3                                                               
         SPACE 1                                                                
PFKMORE  DC    AL1(PFKMOREQ)       DUMMY PFKEY ENTRY FOR 'MORE'                 
         DC    AL1(PFKIALTP,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0),AL2(UC@MORE-TWAD,LC@MORE-TWAD)                      
         DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* BLDPFK LOCAL W/S                                                    *         
***********************************************************************         
         SPACE 1                                                                
BPWORKD  DSECT                                                                  
BPDUB    DS    D                                                                
BPALINE  DS    A                   A(PFKEY LINE)                                
BPALEFT  DS    A                   A(START OF LEFT HAND SIDE)                   
BPALEFTX DS    A                   A(END OF LEFT HAND SIDE)                     
BPARGHT  DS    A                   A(START OF RIGHT HAND SIDE)                  
BPARGHTX DS    A                   A(END OF RIGHT HAND SIDE)                    
BPALIST  DS    6A                  A(LIST OF LINES)                             
BPLINEL  DS    H                   LENGTH ON PFKEY LINE FOR DESCRIPTION         
BPRECACT DS    XL2                                                              
BPINDS1  DS    XL1                 SAVED LAST PFKINDS1                          
BPWORKL  EQU   *-BPWORKD                                                        
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET ACLMHEAD                                             *         
* -----------------------                                             *         
* NTRY: P1 BYTE 0 = COLUMN HEADER NUMBER                              *         
*             1-3 = 0                                                 *         
* EXIT:  ACLMHEAD = A(COLUMN HEADER ENTRY)                            *         
*                                                                     *         
* ROUTINE TO SET ACLMDATA                                             *         
* -----------------------                                             *         
* NTRY: P1 BYTE 0 = 0                                                 *         
*             1-3 = A(1 OR 2 COLUMN CHARACTER CODE)                   *         
* EXIT: P1 BYTE 0 = X'80' IF TWO CHARCTER CODE SEARCHED FOR           *         
*        ACLMDATA = A(COLUMN DATA ENTRY)                              *         
*              CC = LOW IF COLUMN CODE NOT VALID                      *         
*              CC = HIGH IF COLUMN VALID FOR READ ONLY                *         
*              CC = EQUAL IF COLUMN VALID FOR READ AND WRITE          *         
*        LSCLMCOD = 2 CHARACTER COLUMN CODE                           *         
*                                                                     *         
* ROUTINE TO GET NEXT COLUMN TABLE ENTRY                              *         
* --------------------------------------                              *         
* NTRY:  ACLMDATA = CURRENT COLUMN DATA ENTRY OR 0 TO GET 1ST         *         
*              R1 = 0                                                 *         
*              R1 = FF TO BYPASS SECURITY TESTS                       *         
* EXIT:  ACLMDATA = NEXT VALID COLUMN DATA ENTRY                      *         
*              CC = LOW IF END OF TABLE REACHED                       *         
*              CC = HIGH IF COLUMN VALID FOR READ ONLY                *         
*              CC = EQUAL IF COLUMN VALID FOR READ AND WRITE          *         
*        LSCLMCOD = 2 CHARACTER COLUMN CODE                           *         
***********************************************************************         
         SPACE 1                                                                
         USING SCWORKD,RC                                                       
SETCLM   DS    0H                                                               
         USING *,R8                                                             
         LTR   R1,R1               TEST GET NEXT ENTRY                          
         BZ    SCLM30                                                           
         LA    RE,FF                                                            
         CR    R1,RE               TEST SPECIAL CALL                            
         BNE   *+12                                                             
         MVI   SCALL,SCALLQ                                                     
         B     SCLM30                                                           
*                                                                               
         CLI   0(R1),0             TEST ACLMHEAD REQUIRED                       
         BE    SCLM10                                                           
*                                                                               
         XC    ACLMDATA,ACLMDATA                                                
         ICM   R2,15,ACLMHEAD      TEST ACLMHEAD ALREADY SET                    
         USING CLMTABD,R2                                                       
         BZ    *+14                                                             
         CLC   CLMNUMB,0(R1)                                                    
         BE    ROUTE                                                            
         L     R2,ACLMTAB                                                       
         XR    RF,RF                                                            
SCLM02   CLI   CLMTABD,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   CLMNUMB,0(R1)       MATCH ON COLUMN HEADER                       
         BE    SCLM04                                                           
         ICM   RF,3,CLMLEN                                                      
         BXH   R2,RF,SCLM02                                                     
SCLM04   ST    R2,ACLMHEAD                                                      
         MVC   LSGENM,CLMGENM                                                   
         B     ROUTE                                                            
*                                                                               
SCLM10   OI    SCINDS,SCIRET12+SCI2CHAR                                         
         ST    R1,SCAR1                                                         
         L     RF,0(R1)            SEARCH FOR CHARACTER CODE                    
         MVC   SCCODE,0(RF)        ASSUME 2 CHARACTER CODE                      
         LA    RE,PRE2CODE         SEARCH FOR PREFIX                            
         LA    R0,L'PRE2CODE                                                    
         CLC   0(1,RF),0(RE)                                                    
         BE    SCLM12                                                           
         LA    RE,1(RE)                                                         
         BCT   R0,*-14                                                          
         MVI   SCCODE,C' '         NOT FOUND - 1 CHARACTER CODE                 
         MVC   SCCODE+1(1),0(RF)                                                
         NI    SCINDS,FF-SCI2CHAR                                               
SCLM12   OI    SCCODE+1,X'40'                                                   
         XC    ACLMDATA,ACLMDATA                                                
         MVC   LSCLMCOD,SCCODE                                                  
*                                                                               
SCLM20   XR    R2,R2                                                            
         USING CLMTABD,R2                                                       
SCLM22   GOTO1 NXTCLM,SCPARM,CLMTABD                                            
         BNE   SETCLMN                                                          
         L     R2,0(R1)                                                         
         CLC   SCCODE,CLMCODE      MATCH ON CODE                                
         BNE   SCLM22                                                           
         GOTO1 TSTCLM,SCPARM,CLMTABD                                            
         BL    SCLM22                                                           
         BE    SETCLMY                                                          
         BH    SETCLMR                                                          
         DROP  R2                                                               
*                                                                               
SCLM30   ICM   R2,15,ACLMDATA                                                   
         BZ    SCLM32                                                           
         USING CLMTABD,R2                                                       
*                                                                               
SCLM32   GOTO1 NXTCLM,SCPARM,CLMTABD                                            
         BL    SETCLMN                                                          
         L     R2,0(R1)                                                         
         MVC   LSCLMCOD,CLMCODE                                                 
         GOTO1 TSTCLM,SCPARM,CLMTABD                                            
         BL    SCLM32                                                           
         BE    SETCLMY                                                          
         BH    SETCLMR                                                          
*                                                                               
SETCLMY  ST    R2,ACLMDATA         COLUMN VALID EXIT                            
         MVI   BCDUB,1             BCDUB = 1 TO RETURN CC=EQUAL                 
         B     SETCLMX                                                          
*                                                                               
SETCLMR  ST    R2,ACLMDATA         COLUMN VALID FOR READ-ONLY EXIT              
         MVI   BCDUB,2             BCDUB = 2 TO RETURN CC=HIGH                  
         B     SETCLMX                                                          
*                                                                               
SETCLMN  XC    ACLMDATA,ACLMDATA   END-OF-TABLE                                 
         MVI   BCDUB,0             BCDUB = 0 TO RETURN CC=LOW                   
*                                                                               
SETCLMX  TM    SCINDS,SCIRET12     TEST RETURN 1/2 CHARACTER CODE               
         BZ    ROUTCC                                                           
         L     R1,SCAR1                                                         
         MVI   0(R1),0                                                          
         TM    SCINDS,SCI2CHAR     TEST 2 CHARACTER CODE                        
         BZ    ROUTCC                                                           
         MVI   0(R1),X'80'         YES - SET X'80' BIT                          
         B     ROUTCC                                                           
         SPACE 1                                                                
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO GET NEXT COLUMN TABLE ENTRY                              *         
*                                                                     *         
* NTRY: P1 = A(CURRENT ENTRY)                                         *         
* EXIT: P1 = A(NEXT ENTRY)                                            *         
*       CC = LOW IF END-OF-TABLE REACHED                              *         
***********************************************************************         
         SPACE 1                                                                
NXTCLM   NTR1  ,                                                                
         ICM   R2,15,0(R1)         TEST GETTING FIRST ENTRY                     
         USING CLMTABD,R2                                                       
         BZ    *+12                                                             
         LA    R2,CLMDATAL(R2)     NO - BUMP TO NEXT ENTRY                      
         B     NCLM02                                                           
         ICM   R2,15,ACLMHEAD      START AT FIRST COLUMN                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
NCLM01   LA    R2,CLMHEADL(R2)                                                  
*                                                                               
NCLM02   CLI   CLMTABD,EOT                                                      
         BNE   NXTCLMY                                                          
*                                                                               
         XR    RF,RF                                                            
         C     R2,ACLMGEN          TEST DONE GENERAL COLUMNS                    
         BH    *+12                                                             
         L     R2,ACLMGEN          NO - START THEM                              
         B     *+8                                                              
         LA    R2,1(R2)            YES - BUMP TO NEXT HEADER                    
NCLM04   CLI   CLMTABD,EOT                                                      
         BE    SETCLMN                                                          
         ICM   RF,3,CLMGENM        TEST ALL LISTS                               
         BZ    NCLM06                                                           
         MVC   SCMASK,CLMGENM      TEST MASK                                    
         NC    SCMASK,LSGENM                                                    
         BNZ   NCLM06                                                           
         ICM   RF,3,CLMLEN                                                      
         BXH   R2,RF,NCLM04                                                     
NCLM06   B     NCLM01                                                           
*                                                                               
NXTCLMY  ST    R2,0(R1)                                                         
         B     ROUTE                                                            
NXTCLMN  B     ROUTL                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO TEST SECURITY FOR COLUMN TABLE ENTRY                     *         
*                                                                     *         
* NTRY: P1 = A(COLUMN TABLE ENTRY)                                    *         
* EXIT: CC = EQUAL  - OPEN COLUMN IS VALID                            *         
*       CC = HIGH   - COLUMN IS VALID FOR READ ONLY                   *         
*       CC = LOW    - COLUMN IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
TSTCLM   NTR1  ,                                                                
         L     R2,0(R1)                                                         
         USING CLMTABD,R2                                                       
*                                                                               
         TM    CLMINDS1,CLMIDDS    TEST DDS ONLY                                
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS                                                    
         BZ    TSTCLMN                                                          
*                                                                               
         TM    CLMINDS2,CLMIAGYC   TEST COLUMN IS AGENCY CURR. ONLY             
         BZ    *+12                                                             
         TM    BCCPYST6,CPYSFBIL   TEST FOREIGN BILLING ALLOWED                 
         BZ    TSTCLMN                                                          
*                                                                               
         CLI   SCALL,SCALLQ                                                     
         BE    TCLM10                                                           
*                                                                               
         MVC   SCMASK,CSMASK       TEST INVALID MASK VALUES                     
         NC    SCMASK,CLMMASK                                                   
         BNZ   TSTCLMN                                                          
*                                                                               
         CLI   CLMCTRY,0           TEST ALL COUNTRIES                           
         BE    TCLM02                                                           
         CLC   CLMCTRY,CUCTRY      MATCH ON CONNECTED COUNTRY                   
         BE    TCLM02                                                           
         TM    CLMCTRY,CTRYNOT     TEST ALL BUT A COUNTRY                       
         BZ    TSTCLMN                                                          
         MVC   BCBYTE1,CLMCTRY                                                  
         XI    BCBYTE1,CTRYNOT                                                  
         CLC   BCBYTE1,CUCTRY                                                   
         BE    TSTCLMN                                                          
*                                                                               
TCLM02   IC    RE,LSRECCOL         TEST COLUMN FITS ON SCREEN                   
         IC    RF,CLMFWDTH                                                      
         AR    RE,RF                                                            
         CLM   RE,1,=AL1(NUMCOLSQ+1)                                            
         BH    TSTCLMN                                                          
*                                                                               
         TM    CLMINDS2,CLMIAGYC   TEST COLUMN IS AGENCY CURR. ONLY             
         BZ    TCLM04                                                           
         CLC   CSCPYCUR,CSBILCUR   TEST ARE FOREIGN BILLING                     
         BNE   TCLM04                                                           
*&&UK*&& TM    BCCPYST7,CPYSSCNV   OR A EURO ZONE AGENCY                        
*&&UK*&& BNO   TSTCLMN                                                          
*&&US*&& B     TSTCLMN                                                          
*                                                                               
TCLM04   CLI   CLMSEC,0            TEST FIELD SECURITY                          
         BE    TCLM10                                                           
         GOTO1 AFLDSEC,CLMSEC                                                   
         BL    TCLM10                                                           
         BH    TSTCLMR             COLUMN IS READ ONLY                          
*                                                                               
TCLM10   TM    CLMINDS1,CLMIPRO    TEST COLUMN ALWAYS PROTECTED                 
         BO    TSTCLMR             COLUMN IS READ ONLY                          
*                                                                               
TSTCLMY  B     ROUTE               CC = YES  - OPEN COLUMN IS VALID             
TSTCLMR  B     ROUTH               CC = HIGH - COLUMN IS READ ONLY              
TSTCLMN  B     ROUTL               CC = LOW  - COLUMN IS INVALID                
         SPACE 1                                                                
SCWORKD  DSECT                     * SETCLM W/S *                               
SCAR1    DS    A                   A(R1)                                        
SCPARM   DS    6A                                                               
SCCODE   DS    CL2                 MATCH ON COLUMN CODE                         
SCALL    DS    XL1                 PROCESS ALL COLUMNS (NO SEC. TEST)           
SCALLQ   EQU   X'FF'                                                            
SCINDS   DS    XL1                 INDICATOR BYTE                               
SCIRET12 EQU   X'80'               RETURN LOOKING FOR 1/2 CHAR CODE             
SCI2CHAR EQU   X'40'               LOOKING FOR 2 CHAR CODE                      
SCMASK   DS    XL2                 MASK VALUE                                   
SCWORKL  EQU   *-SCWORKD                                                        
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SAVE CURRENT SESSION TO NEXT SESSION SAVE AREA                      *         
*                                                                     *         
* NTRY - TWASESNL=CURRENT LEVEL OF NESTING                            *         
*      - R1=A(SESSION PARAMETERS)                                     *         
* EXIT - TWASESNL=NEXT LEVEL                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING NSWORKD,RC                                                       
NTRSES   DS    0H                                                               
         USING *,R8                                                             
         LR    R3,R1               SAVE SESSION PARAMETERS                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,TWASESNL                                                      
         LA    RE,1(RE)            INCREMENT NEXT LEVEL                         
         CLM   RE,1,=AL1(TWASESMX)                                              
         BNH   *+6                                                              
         DC    H'0'                MAXIMUM NEST LEVEL EXCEEDED                  
         STC   RE,TWASESNL                                                      
         LA    RE,1(RE)                                                         
         SRDL  RE,1                                                             
         STC   RE,BCBYTE1          SAVE ABSOLUTE TEMPSTR PAGE NUMBER            
         IC    RE,TWASESNL                                                      
         SLL   RE,1                                                             
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         MVC   0(L'TWASESRA,RE),CSRECACT                                        
         OC    CSINITRA,CSINITRA                                                
         BZ    *+10                                                             
         MVC   0(L'TWASESRA,RE),CSINITRA                                        
*                                                                               
         XC    BCHALF,BCHALF                                                    
         LTR   RF,RF                                                            
         BNZ   NTRSES02                                                         
         L     R0,ATIA             CLEAR TEMPSTR PAGE                           
         LH    R1,=Y(TWAMAXRL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     NTRSES04                                                         
*                                                                               
NTRSES02 MVC   BCHALF,=Y(TWAMAXRL/2)                                            
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(TWAMAXRL)                                                
         GOTO1 VDMGR,BCPARM,DMREAD,TEMPSTR,(BCBYTE1,0),ATIA,,(RF)               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NTRSES04 L     RE,AGOPBLK          CLEAR SAVED GETOPT VALUES                    
         XC    0(GOADM-GOBLOCK,RE),0(RE)                                        
         L     R2,ATIA             SAVE START ADDRESS INTO TIA                  
         AH    R2,BCHALF                                                        
         USING SESD,R2             R2=A(SESSION SAVE AREA)                      
         USING SELTPARM,R3         R3=A(NTRSES PARAMETERS)                      
         MVI   SESROUT,0                                                        
         LTR   R3,R3               TEST SESSION PARAMETERS PASSED               
         BZ    *+10                                                             
         MVC   SESROUT,SELTRTN     SET EXIT ROUTINE NUMBER                      
*                                                                               
         MVC   SESSCRN,TWASCRN     SAVE GLOBAL VALUES                           
         MVC   SESSCRF,TWASCRF                                                  
*                                                                               
         LA    R0,SESOSSV          SAVE CURRENT OVERLAY SAVE VALUES             
         LA    R1,SESOSSVL                                                      
         LA    RE,OSVALS                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,SESO2SV          SAVE CURRENT OVERLAY SAVE VALUES 2           
         LA    R1,SESO2SVL                                                      
         L     RE,BOSVALS2                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,SESLSSV          SAVE CURRENT LIST SAVE VALUES                
         LA    R1,SESLSSVL                                                      
         L     RE,ALSVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,SESCSSV          SAVE CURRENT SESSION VALUES                  
         LA    R1,SESCSSVL                                                      
         LA    RE,CSVALS                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,SESCPJ           SAVE CLIENT/PRODUCT/JOB VALUES               
         LA    R1,SESCPJL                                                       
         LA    RE,BCCLI                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,SESTWSV          SAVE ENTIRE SCREEN                           
         LA    R1,SESTWSVL                                                      
         LA    RE,BASOPTH                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    CSINITRA,CSINITRA   CLEAR INITIATOR RECORD/ACTION                
         MVI   CSLTINDS,0          CLEAR LIST INDICATORS                        
         XC    CSMASK,CSMASK       RESET MASK                                   
         MVI   CSPFLIN#,0                                                       
*                                                                               
         GOTO1 VDMGR,BCPARM,DMWRITE,TEMPSTR,(BCBYTE1,0),ATIA                    
*                                                                               
         LA    R0,OSVALS           CLEAR OSVALS                                 
         LA    R1,OSVALSL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,BOSVALS2         CLEAR OSVALS2                                
         LA    R1,OSVALS2L                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   NSMIX1,CSMIX1                                                    
         LTR   R3,R3               TEST SESSION PARAMETERS PASSED               
         BZ    NTRSES06                                                         
         MVC   CSOIND1,SELTNSI1    SET NEXT SESSION INDICATORS                  
         MVC   CSOIND2,SELTNSI2                                                 
         SR    RE,RE                                                            
         ICM   RE,3,SELTRECA       TEST RECORD/ACTION PASSED                    
         BZ    NTRSES06                                                         
*                                                                               
         L     R0,AOVERWRK         CLEAR OVERWRK                                
         LH    R1,=Y(OVERWRKL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 ARECACT,SELTREC     SET ENTRY RECORD/ACTION NAMES                
         L     RF,AMIXNTRY         TEST KEEP LSVALS ON ENTRY                    
         TM    MIXINDS1-MIXTABD(RF),MIXINTRY                                    
         BO    NTRSES10                                                         
*                                                                               
NTRSES06 TM    NSMIX1,MIXIEXIT     TEST KEEP LSVALS ON EXIT                     
         BO    NTRSES10                                                         
*                                                                               
NTRSES08 LTR   R3,R3                                                            
         BZ    NTRSES10                                                         
         LA    R0,LSLIN            CLEAR LSVALS (SOME OF IT) ??                 
         LA    R1,LSMIXLST-LSLIN                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    LSCLM,LSCLM                                                      
*                                                                               
NTRSES10 DS    0H                  KEEP OPTIONS FIELD                           
*NTRSES10 OC    BASOPT,BASOPT       TEST ANYTHING IN OPTIONS FIELD              
**&&UK*&& BZ    *+14                                                            
**&&US*&& B     *+14                PASS OPTIONS FIELD IN US                    
*         XC    BASOPT,BASOPT                                                   
*         OI    BASOPTH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         MVC   CSPSRECN,CSHIRECN   SET NEXT SESSION LOW TSAR RECORD#            
         OI    BCINDS2,BCINTRS     SET JUST NTRSES'D                            
*                                                                               
         LTR   R3,R3               TEST SESSION PARAMETERS PASSED               
         BZ    NTRSESX                                                          
         SR    RE,RE                                                            
         ICM   RE,3,SELTRECA       TEST RECORD/ACTION PASSED                    
         BZ    NTRSESX                                                          
*                                                                               
         CLI   SELTNXPF,0          TEST AUTO RETURN PFKEY SET                   
         BE    *+10                                                             
         MVC   CSNEXTPF,SELTNXPF   YES - SET VALUE                              
         OI    TWAINDS1,TWAINTRS   SET NTRSES ISSUED                            
         XC    CSINDSL,CSINDSL                                                  
         MVI   CSINDSL1,CSIUSELC                                                
         L     RD,BCSVRD                                                        
         L     RD,8(RD)                                                         
*                                                                               
NTRSESX  B     ROUTE                                                            
         DROP  R2,R3,RC                                                         
         SPACE 1                                                                
NSWORKD  DSECT                     ** NTRSES S/R LOCAL W/S **                   
NSMIX1   DS    XL2                 SAVED CSMIX1                                 
NSWORKL  EQU   *-NSWORKD                                                        
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* RESTORE PREVIOUS SESSION                                            *         
*                                                                     *         
* NTRY - TWASESNL=CURRENT NESTING LEVEL                               *         
* EXIT - TWASESNL=PREVIOUS NESTING LEVEL                              *         
***********************************************************************         
         SPACE 1                                                                
         USING XSWORKD,RC                                                       
XITSES   DS    0H                                                               
         USING *,R8                                                             
         MVC   XSNXRECN,CSHIRECN                                                
         MVC   XSHIRECN,CSPSRECN                                                
         MVC   XSREPID,CSREPID                                                  
         MVC   XSDSTID,CSDSTID                                                  
         MVC   XSINDSG1,CSINDSG1                                                
         MVC   XSACT,CSACT         SAVE CURRENT ACTION                          
         MVC   XSBASACT,BASACTH                                                 
         MVC   XSBROUT,CSBROUT     SAVE ROUTINE ADDRESSES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,TWASESNL                                                      
         BCTR  RE,0                DECREMENT NEXT LEVEL                         
         LTR   RE,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                MINIMUM NEST LEVEL EXCEEDED                  
         STC   RE,TWASESNL                                                      
         LA    RE,2(RE)                                                         
         SRDL  RE,1                                                             
         STC   RE,BCBYTE1          SAVE ABSOLUTE TEMPSTR PAGE NUMBER            
         L     R2,ATIA                                                          
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         AH    R2,=Y(TWAMAXRL/2)                                                
         USING SESD,R2             R2=A(SESSION SAVE AREA)                      
*                                  READ SAVED TEMPSTR PAGE                      
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(TWAMAXRL)                                                
         GOTO1 VDMGR,BCPARM,DMREAD,TEMPSTR,(BCBYTE1,0),ATIA,,(RF)               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,OSVALS           RESTORE OVERLAY SAVE VALUES                  
         LA    R1,SESOSSVL                                                      
         LA    RE,SESOSSV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,BOSVALS2         RESTORE OVERLAY SAVE AREA 2                  
         LA    R1,SESO2SVL                                                      
         LA    RE,SESO2SV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,ALSVALS          RESTORE LIST SAVE AREA                       
         LA    R1,SESLSSVL                                                      
         LA    RE,SESLSSV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,CSVALS           RESTORE SESSION VALUES                       
         LA    R1,SESCSSVL                                                      
         LA    RE,SESCSSV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   CSNXRECN,XSNXRECN   SET NEXT SESSION HIGH RECORD                 
         MVC   CSHIRECN,XSHIRECN   SET THIS SESSION HIGH RECORD                 
*                                                                               
         LA    R0,BCCLI            RESTORE CLIENT/PRODUCT/JOB VALUES            
         LA    R1,SESCPJL                                                       
         LA    RE,SESCPJ                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TWASCRN,SESSCRN     RESTORE GLOBAL VALUES                        
         MVC   TWASCRF,SESSCRF                                                  
         MVC   CSREPID,XSREPID                                                  
         MVC   CSDSTID,XSDSTID                                                  
         NI    XSINDSG1,CSINDUNW   KEEP UNWIND BIT ON IF WAS ON BEFORE          
         OC    CSINDSG1,XSINDSG1                                                
*                                                                               
         LA    R0,BASOPTH          RESTORE TWA                                  
         LA    R1,SESTWSVL                                                      
         LA    RE,SESTWSV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   CSBROUT(CSBROUTS),XSBROUT RESTORE ROUTINE ADDRESSES              
*                                                                               
         LA    R1,BASACTH          SET CURSOR TO ACTION FIELD                   
         ST    R1,FVADDR                                                        
         LA    R0,BASOLAYH         R0=A(OVERLAY SCREEN START)                   
         LA    R1,BASMSGH          TRANSMIT SCREEN & TURN OFF CURSORS           
         USING FHD,R1                                                           
         XR    RF,RF                                                            
XITSES04 ICM   RF,1,FHLN                                                        
         BZ    XITSES08                                                         
         OI    FHOI,FHOITR                                                      
         NI    FHOI,FF-FHOICU                                                   
         CLI   TWAHELPT,0                                                       
         BNE   XITSES06                                                         
         CR    R1,R0               TEST INTO THE OVERLAY SCEFEN AREA            
         BL    XITSES06                                                         
         C     R0,FVADDR           TEST A(OVERLAY SCREEN FIELD) SET             
         BNH   XITSES06                                                         
         TM    FHAT,FHATPR                                                      
         BNZ   XITSES06                                                         
         ST    R1,FVADDR           A(FIRST UNPROT OVERLAY SCREEN FIELD)         
XITSES06 BXH   R1,RF,XITSES04                                                   
         DROP  R1                                                               
*                                                                               
XITSES08 MVI   1(R1),1             SET CLEAR BEFORE AND AFTER                   
         MVI   2(R1),1                                                          
*                                                                               
         L     RE,AGOPBLK          CLEAR SAVED GETOPT VALUES                    
         XC    0(GOADM-GOBLOCK,RE),0(RE)                                        
*                                                                               
         CLI   XSACT,ACTHLP        TEST ACTION WAS HELP                         
         BNE   XITSES10                                                         
         TM    XSBASACT+FHIID,FHIIVA  AND ACTION FIELD CHANGED                  
         BO    XITSES10                                                         
         MVC   BASACTH(L'XSBASACT),XSBASACT  RESTORE SELECTED ACTION            
         OI    BASACTH+FHOID,FHOITR                                             
         NI    BASACTH+FHIID,FF-FHIIVA                                          
         B     XITSES12                                                         
*                                                                               
XITSES10 MVI   BCBYTE1,0                                                        
         TM    BCINDS1,BCIXERR     TEST XITSES ERROR MESSAGE SET                
         BO    XITSESX               BY CALLER                                  
         CLI   SESROUT,0           TEST RETURN POINT GIVEN                      
         BE    XITSES14                                                         
         GOTO1 ARECACT,CSREC                                                    
*                                                                               
XITSES12 OI    TWAINDS1,TWAIXITS   SET XITSES ISSUED                            
         MVC   BCBYTE1,SESROUT     RETURN ROUTINE NUMBER                        
         OI    BCINDS2,BCIXITS     SET JUST XITSES'D                            
         L     RD,BCSVRD                                                        
         L     RD,8(RD)                                                         
         B     XITSESX                                                          
*                                                                               
XITSES14 MVC   FVMSGNO,=AL2(AI$PSRES)                                           
         MVI   FVOMTYP,GTMINF                                                   
*                                                                               
XITSESX  B     ROUTE                                                            
         DROP  R2                                                               
         SPACE 1                                                                
XSWORKD  DSECT                     ** XITSES S/R LOCAL W/S **                   
XSNXRECN DS    XL2                 SAVED CSHIRECN TO SET CSNXRECN               
XSHIRECN DS    XL2                 SAVED CSPSRECN TO SET CSHIRECN               
XSREPID  DS    XL(L'CSREPID)                                                    
XSDSTID  DS    XL(L'CSDSTID)                                                    
XSINDSG1 DS    XL(L'CSINDSG1)                                                   
XSACT    DS    XL1                                                              
XSBASACT DS    XL(L'BASACTH+L'BASACT)                                           
XSBROUT  DS    XL(CSBROUTS)                                                     
XSWORKL  EQU   *-XSWORKD                                                        
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* TEST ACCOUNT SECURITY                                               *         
***********************************************************************         
         SPACE 1                                                                
TSTSEC   DS    0H                                                               
         USING *,R8                                                             
         ICM   R2,15,ACALDG                                                     
         USING LDGTABD,R2          R2=A(LEDGER TABLE ENTRY)                     
*&&US*&& TM    CSBSECL,CPYBSSEC                                                 
*&&US*&& BZ    TSTSEC02                                                         
         CLC   LDGTSEC,TWAAUTH+1   TEST SECURITY LEVEL                          
         BH    TSTSECSL                                                         
         CLC   ACSECY,TWAAUTH+1                                                 
         BH    TSTSECSL                                                         
*                                                                               
TSTSEC02 MVC   ACOFFC,BCSPACES                                                  
*&&UK*&& CLI   LDGTOFFP,LDGOTRAN   TEST OFFICE IN TRANSACTIONS                  
*&&UK*&& BE    ROUTE                                                            
         CLI   LDGTOFFP,LDGOFLT1   TEST OFFICE IN FILTERS                       
         BNL   TSTSEC08                                                         
         ICM   RF,15,ACAPPR        SET OFFICE FROM PROFILE IF RESOLVED          
         BZ    *+10                                                             
         MVC   ACOFFC,PPRGAOFF-PPRELD(RF)                                       
*&&UK*&& CLC   BCCPYPRD,ACCODE+(ACTKUNT-ACTRECD)                                
*&&UK*&& BE    ROUTE                                                            
         CLI   LDGTOFFP,LDGOPROF   TEST OFFICE IN PRODUCTION PROFILE            
         BE    ROUTE                                                            
*                                                                               
TSTSEC04 CLI   LDGTOFFP,LDGONONE                                                
         BE    TSTSEC10                                                         
         MVC   BCWORK(1),LDGTOFFP                                               
         NI    BCWORK,FF-LDGOKEY2                                               
         CLI   BCWORK,LDGOKEY                                                   
         BH    TSTSEC10                                                         
         SR    R1,R1                                                            
         IC    R1,BCWORK                                                        
         LA    R1,IOKEY+(ACTKACT-ACTRECD-1)(R1)                                 
         MVC   ACOFFC+0(1),0(R1)                                                
         TM    LDGTOFFP,LDGOKEY2   TEST 2 CHARACTER OFFICE IN KEY               
         BZ    *+10                                                             
         MVC   ACOFFC+1(1),1(R1)                                                
         B     TSTSEC10                                                         
*                                                                               
TSTSEC08 PACK  BCDUB,LDGTOFFP      OFFICE IN FILTERS                            
         CVB   R1,BCDUB            VALUE IS OF FORM X'F1'-X'F4'                 
         LA    R1,ACFLTS-1(R1)                                                  
         MVC   ACOFFC(1),0(R1)                                                  
*                                                                               
TSTSEC10 CLC   TWAACCS,BCSPACES    TEST ANY LIMIT ACCESS                        
         BNH   ROUTE                                                            
*&&US*&& TM    CSBSECL,CPYBSOFF    TEST OFFICE OVERRIDE FOR BATCH TYPE          
*&&US*&& BZ    ROUTE                                                            
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAREC,AIO1                                                     
         MVC   OFFAOPOS,LDGTOFFP                                                
         MVC   OFFAOFFC,ACOFFC                                                  
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BE    ROUTE                                                            
         SPACE 1                                                                
TSTSECSL MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(ACTKEND-L'ACTKCPY),IOKEY+(ACTKUNT-ACTRECD)                
         B     ROUTH                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* SET MASK                                                            *         
***********************************************************************         
         SPACE 1                                                                
SETMSK   DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CALL GETOPT                                                         *         
*                                                                     *         
* NTRY: P1 BYTE 0 = INDICATOR BYTE                                    *         
*                   X'80' ON TO NOT USE TIA AS GETOPT BUFFER          *         
*             1-3 = A(JOB/WORK CODE KEY)                              *         
***********************************************************************         
         SPACE 1                                                                
         USING GJWORKD,RC                                                       
GETOPT   DS    0H                                                               
         USING *,R8                                                             
         MVC   GJINDS,0(R1)        SAVE INPUT PARAMETERS                        
         XR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         ST    RF,GJAKEY                                                        
*                                                                               
         MVC   GJIOKEY,IOKEY       SAVE IOKEY                                   
         LA    RE,IOKEY                                                         
         C     RE,GJAKEY                                                        
         BNE   GETOPT00                                                         
         MVC   GJKEY,IOKEY                                                      
         LA    RE,GJKEY                                                         
         ST    RE,GJAKEY                                                        
*                                                                               
GETOPT00 L     R2,AGOPBLK                                                       
         USING GOBLOCKD,R2         R2=A(GETOPT BLOCK)                           
         MVC   GOCTRY,CUCTRY                                                    
         MVC   GOADM,VDMGR                                                      
         MVC   GOAEXT,AGOXBLK      SET A(BLOCK EXTENSION)                       
         MVC   GOABUFF,ATIA                                                     
         MVC   GOLBUFF,=AL4(TWAMAXRL)                                           
         TM    GJINDS,GJINOBUF                                                  
         BZ    GOPT00                                                           
         XC    GOABUFF,GOABUFF                                                  
         XC    GOLBUFF,GOLBUFF                                                  
GOPT00   DS    0H                                                               
*&&UK                                                                           
         MVC   GOACOMP,AIO6                                                     
         MVC   GOALEDG,AIO7                                                     
         MVC   GOACLI,AIO8                                                      
         MVC   GOAPRO,AIO9                                                      
         MVC   GOAJOB,AIOA                                                      
*&&                                                                             
*                                                                               
         L     R1,GJAKEY                                                        
         MVC   IOKEY(L'ACTKEY),BCSPACES                                         
         MVC   IOKEY(ACTKEND),0(R1)                                             
         MVC   GOSELCUL,0(R1)                                                   
         LA    R1,ACTKACT-ACTRECD(R1)                                           
*                                                                               
         MVC   GOSELCLI,BCSPACES                                                
         SR    RF,RF                                                            
         IC    RF,BCCLILEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   GOSELCLI(0),0(R1)                                                
         CLC   GOSELCLI,BCSPACES                                                
         BH    *+10                                                             
         XC    GOSELCLI,GOSELCLI                                                
         LA    R1,1(RF,R1)                                                      
*                                                                               
         MVC   GOSELPRO,BCSPACES                                                
         LA    RE,1(RF)                                                         
         IC    RF,BCPROLEN                                                      
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   GOSELPRO(0),0(R1)                                                
         CLC   GOSELPRO,BCSPACES                                                
         BH    *+10                                                             
         XC    GOSELPRO,GOSELPRO                                                
         LA    R1,1(RF,R1)                                                      
*                                                                               
         MVC   GOSELJOB,BCSPACES                                                
         IC    RE,BCJOBLEN                                                      
         IC    RF,BCPROLEN                                                      
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   GOSELJOB(0),0(R1)                                                
         CLC   GOSELJOB,BCSPACES                                                
         BH    *+10                                                             
         XC    GOSELJOB,GOSELJOB                                                
*                                                                               
*&&US                                                                           
         PUSH  USING                                                            
         USING CPYRECD,IOKEY                                                    
         MVC   CPYKEY,BCSPACES                                                  
         MVC   CPYKCPY,CUABIN                                                   
         L     R3,AIO6                                                          
         CLC   0(L'CPYKEY,R3),IOKEY                                             
         BE    GOPT01                                                           
         ST    R3,IOADDR                                                        
         GOTO1 AIO,IOREAD+IOACCFIL                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R3,GOACOMP                                                       
*                                                                               
         USING LDGRECD,IOKEY                                                    
GOPT01   MVC   LDGKUNT(L'BCCPYPRD),BCCPYPRD                                     
         L     R3,AIO7                                                          
         ST    R3,GOALEDG                                                       
         CLC   0(L'LDGKEY,R3),IOKEY                                             
         BE    GOPT02                                                           
         ST    R3,IOADDR                                                        
         GOTO1 AIO,IOREAD+IOACCFIL                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACTRECD,IOKEY                                                    
GOPT02   CLC   GOSELCLI,BCSPACES                                                
         BNH   GOPT05                                                           
         MVC   ACTKACT(L'GOSELCLI),GOSELCLI                                     
         L     R3,AIO8                                                          
         ST    R3,GOACLI                                                        
         CLC   0(L'LDGKEY,R3),IOKEY                                             
         BE    GOPT03                                                           
         ST    R3,IOADDR                                                        
         GOTO1 AIO,IOREAD+IOACCFIL                                              
         BE    *+6                                                              
         DC    H'0'                                                             
GOPT03   CLC   GOSELPRO,BCSPACES                                                
         BNH   GOPT05                                                           
         SR    RF,RF                                                            
         IC    RF,BCCLILEN                                                      
         LA    RF,ACTKACT(RF)                                                   
         MVC   0(L'GOSELPRO,RF),GOSELPRO                                        
         L     R3,AIO9                                                          
         ST    R3,GOAPRO                                                        
         CLC   0(L'LDGKEY,R3),IOKEY                                             
         BE    GOPT04                                                           
         ST    R3,IOADDR                                                        
         GOTO1 AIO,IOREAD+IOACCFIL                                              
         BE    *+6                                                              
         DC    H'0'                                                             
GOPT04   CLC   GOSELJOB,BCSPACES                                                
         BNH   GOPT05                                                           
         SR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         LA    RF,ACTKACT(RF)                                                   
         MVC   0(L'GOSELJOB,RF),GOSELJOB                                        
         L     R3,AIOA                                                          
         ST    R3,GOAJOB                                                        
         CLC   0(L'LDGKEY,R3),IOKEY                                             
         BE    GOPT05                                                           
         ST    R3,IOADDR                                                        
         GOTO1 AIO,IOREAD+IOACCFIL                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*&&                                                                             
*                                                                               
GOPT05   MVC   GOSELWC,BCSPACES                                                 
         L     R1,GJAKEY           RESET A(KEY)                                 
         CLC   TRNKOFF-TRNRECD(L'TRNKOFF,R1),BCSPACES                           
         BNH   *+10                                                             
         MVC   GOSELWC,TRNKOFF-TRNRECD(R1)                                      
         CLC   =C'**',GOSELWC      TEST IF ORDER                                
         BNE   GOPT10                                                           
         SR    R0,R0                                                            
         LA    R1,TRNRFST-TRNRECD(R1)                                           
GOPT08   IC    R0,OAMLN-OAMELD(R1)                                              
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    GOPT10                                                           
         CLI   0(R1),OAMELQ                                                     
         BNE   GOPT08                                                           
         MVC   GOSELWC,(OAMWORK-OAMELD)(R1)                                     
*                                                                               
GOPT10   GOTO1 VGETOPT,BCPARM,AGOPBLK                                           
*                                                                               
         OC    GOSELCLI,GOSELCLI                                                
         BZ    GETOPTX                                                          
*                                                                               
         L     RF,AIOA             TEST JOB RECORD READ                         
         CLC   IOKEY(L'ACTKEY),0(RF)                                            
         BE    GOPT12                                                           
         LH    R1,=AL2(IOA)                                                     
         GOTO1 AIO,IOREAD+IOACCFIL(R1)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GOPT12   DS    0H                                                               
*&&US                                                                           
         PUSH  USING                                                            
         L     RF,GJAKEY           TEST CONTRA FOR PERSONNEL LEDGER             
         USING TRNRECD,RF                                                       
         CLC   =C'1R',TRNKULC                                                   
         BNE   GOPT40                                                           
         OC    GOABEXT,GOABEXT                                                  
         BZ    GOPT40                                                           
         MVC   GJCULA,TRNKCULC                                                  
         DROP  RF                                                               
*                                                                               
         USING ACTRECD,IOKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,GJCULA                                                  
         GOTO1 AGETLDG                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R3,15,ACALDG        R3=A(LENGTH OF HIGHEST LEVEL A/C)            
         LA    R3,LDGTLVA-LDGTABD(R3)                                           
*                                                                               
GOPT14   MVC   ACTKEY,BCSPACES     GET ACCOUNT FOR CURRENT LEVEL                
         IC    RE,0(R3)                                                         
         LA    RE,LDGKEND-1(RE)                                                 
         EX    RE,*+4                                                           
         MVC   ACTKEY(0),GJCULA                                                 
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BNE   GOPT40                                                           
         MVC   IODAOVER,ACTKDA                                                  
         LA    RF,GJIO                                                          
         ST    RF,IOADDR                                                        
         GOTO1 AIO,IOGET+IOACCMST                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   RE,15,GOABEXT                                                    
         USING GOBBLOCK,RE                                                      
         LA    R1,GJIO+(ACTRFST-ACTRECD)                                        
         USING SPAELD,R1                                                        
         XR    RF,RF                                                            
GOPT16   CLI   SPAEL,0                                                          
         BE    GOPT38                                                           
         CLI   SPAEL,SPAELQ                                                     
         BNE   GOPT28                                                           
         CLI   SPATYPE,SPATINCO    OVERRIDE INCOME A/C                          
         BNE   GOPT18                                                           
         MVC   GOTBC(L'CUABIN),CUABIN                                           
         MVC   GOTBC+L'CUABIN(L'SPAAULA),SPAAULA                                
         B     GOPT28                                                           
GOPT18   CLI   SPATYPE,SPATWOFF    OVERRIDE WRITE-OFF A/C                       
         BNE   GOPT28                                                           
         MVC   GOEXPTAC(L'CUABIN),CUABIN                                        
         MVC   GOEXPTAC+L'CUABIN(L'SPAAULA),SPAAULA                             
*        MVC   GOEXPOAC,GOEXPTAC                                                
GOPT28   IC    RF,SPALN                                                         
         BXH   R1,RF,GOPT16                                                     
         DROP  RE                                                               
*                                                                               
GOPT38   CLI   0(R3),L'ACTKACT     TEST LOWEST LEVEL                            
         BNL   GOPT40                                                           
         LA    R3,1(R3)                                                         
         B     GOPT14                                                           
*                                                                               
GOPT40   CLC   GOSELWC,BCSPACES    TEST HAVE W/C                                
         BNH   GOPT50                                                           
         CLI   GOWRKTY,C' '        TEST HAVE W/C TYPE                           
         BH    GOPT50                                                           
         USING WCORECD,IOKEY       NO - GET IT OFF W/C RECORD                   
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   WCOKWRK,GOSELWC                                                  
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BNE   GOPT50                                                           
         MVC   IODAOVER,WCOKDA                                                  
         LA    RF,GJIO                                                          
         ST    RF,IOADDR                                                        
         GOTO1 AIO,IOGET+IOACCMST                                               
         BNE   GOPT50                                                           
         LA    R3,GJIO+(WCORFST-WCORECD)                                        
         USING WCOELD,R3                                                        
         XR    RF,RF                                                            
GOPT42   CLI   WCOEL,0                                                          
         BE    GOPT50                                                           
         CLI   WCOEL,WCOELQ                                                     
         BE    *+12                                                             
         IC    RF,WCOLN                                                         
         BXH   R3,RF,GOPT42                                                     
         MVC   GOWRKTY,WCOTYPE                                                  
         POP   USING                                                            
*&&                                                                             
GOPT50   LA    R1,GOPTSUB          R1=A(TABLE OF ACCOUNTS)                      
*                                                                               
GOPT52   SR    R3,R3                                                            
         ICM   R3,3,0(R1)                                                       
         BZ    GOPT56                                                           
         ICM   RF,15,GOABEXT                                                    
         BZ    GOPT56              NO EXTENSION BLOCK                           
         AR    R3,RF               R3=A(ACCOUNT)                                
         CLI   0(R3),C' '                                                       
         BNH   GOPT54              NO ACCOUNT DEFINED                           
         MVC   IOKEY(L'ACTKEY),BCSPACES                                         
         MVC   IOKEY(L'ACTKACT),0(R3)                                           
         GOTO1 AGETLDG                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,ACALDG                                                     
         CLI   LDGTOFFP-LDGTABD(RF),LDGONONE                                    
         BE    GOPT54              NO OFFICE POSTITION                          
         CLI   LDGTOFFP-LDGTABD(RF),LDGOKEY                                     
         BH    GOPT54              OFFICE NOT IN KEY                            
         SR    RE,RE                                                            
         IC    RE,LDGTOFFP-LDGTABD(RF)                                          
         BCTR  RE,0                                                             
         LA    RE,(ACTKACT-ACTKEY)(RE,R3)                                       
         MVC   0(1,RE),CSOFFICE                                                 
GOPT54   LA    R1,2(R1)                                                         
         B     GOPT52                                                           
GOPT56   L     R1,GJAKEY                                                        
         MVC   IOKEY(L'ACTKEY),BCSPACES                                         
         MVC   IOKEY(ACTKEND),0(R1)                                             
*                                                                               
         CLC   GOSELWC,BCSPACES    CHECK ANY OVERRIDE COMMN RATE                
         BE    GETOPTX                                                          
         LA    RE,LSCOM                                                         
         LA    R0,5                                                             
GOPT58   CLI   0(RE),0             IF 00 THEN NOT USED FOR OVERRIDE             
         BE    GETOPTX                                                          
         CLC   GOSELWC,0(RE)       MATCH SELECTED WC ON OVERRIDE TABLE          
         BE    GOPT60                                                           
         LA    RE,L'GOSELWC+L'GOAGYCOM(RE)                                      
         BCT   R0,GOPT58                                                        
         B     GETOPTX                                                          
GOPT60   ZAP   GOAGYCOM,L'GOSELWC(L'GOAGYCOM,RE)                                
         B     GETOPTX                                                          
*                                                                               
GETOPTX  MVC   IOKEY,GJIOKEY       RESTORE CALLERS IOKEY                        
         B     ROUTE                                                            
*                                                                               
GOPTSUB  DS    0H                  LIST OF A/CS FOR OFFICE SUBST                
         DC    AL2(GOINCAC-GOBBLOCK)                                            
         DC    AL2(GOEXPTAC-GOBBLOCK)                                           
         DC    AL2(GOEXPOAC-GOBBLOCK)                                           
         DC    AL2(GOWOFTAC-GOBBLOCK)                                           
         DC    AL2(GOWOFOAC-GOBBLOCK)                                           
         DC    AL2(GOSRGAC-GOBBLOCK)                                            
         DC    AL2(GODSCAC-GOBBLOCK)                                            
         DC    AL2(0)                                                           
         DROP  R2                                                               
         SPACE 1                                                                
GJWORKD  DSECT                     ** GETJOB LOCAL W/S **                       
GJAKEY   DS    A                   A(JOB(/WORK CODE)) KEY                       
GJINDS   DS    XL1                                                              
GJINOBUF EQU   X'80'               DON'T USE TIA AS BUFFER                      
GJKEY    DS    XL42                                                             
GJCULA   DS    CL15                                                             
GJIOKEY  DS    XL(L'IOKEY)                                                      
GJIO     DS    XL2048              IO AREA                                      
GJWORKL  EQU   *-GJWORKD                                                        
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ALLOCATE CASH AMOUNTS TO TRANSACTION RECORD              *         
*                                                                     *         
* NTRY: P1 - BYTE 0 = PTA ELEMENT TYPE                                *         
*               1-3 = A(TRANSACTION RECORD)                           *         
*       P2 - BYTE 0 = OC VALUE FOR PTASTAT1                           *         
*               1-3 = A(PRORATA BLOCK)                                *         
*       P3 - BYTE 0 = ZERO IF ADDING AMOUNT, NON-ZERO IF SETTING IT   *         
*               1-3 = A(PL8 OF NET AMOUNT) IF 'PTASTAT1,PTASCASH'     *         
*               1-3 = A(PL8 OF NO. HOURS) IF 'PTASTAT1,PTASHOUR'      *         
*   IF PTATYPE,PTATRAL                                                *         
*       P4 - BYTE 0 = ZERO IF ADDING AMOUNT, NON-ZERO IF SETTING IT   *         
*               1-3 = A(PL8 OF ALLOCATED COMMISSION AMOUNT)           *         
*                                                                     *         
* EXIT: P1          = A(PTA ELEMENT)                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING ATWORKD,RC                                                       
ALLTRN   DS    0H                                                               
         USING *,R8                                                             
         MVC   ATPARMS,0(R1)                                                    
         ST    R1,ATAR1                                                         
*&&UK                                                                           
         MVC   ATWORK,IOKEY        SAVE CURRENT DIRECTORY KEY                   
         XC    0(4,R1),0(R1)                                                    
         LA    R2,IOKEY            READ FOR INCOME ACCOUNT                      
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCKD(RF)                                          
         MVC   ACTKCULA,GOINCAC-GOBBLOCK(RF)                                    
         GOTO1 AIO,IOACCDIR+IOREAD                                              
         BE    ATRN02                                                           
         MVC   FVMSGNO,=AL2(AE$RCNOF)                                           
         MVC   FVXTRA(L'ACTKULA),IOKEYSAV+(ACTKULA-ACTKEY)                      
         B     ROUTL                                                            
         DROP  R2                                                               
ATRN02   L     RE,ATATRN                                                        
         CLC   =C'**',(TRNKWORK-TRNRECD)(RE)                                    
         BNE   ATRN08                                                           
         USING ORDRECD,R2                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUABIN                                                   
         MVC   ORDKORD,(TRNKREF-TRNRECD)(RE)                                    
         GOTO1 AIO,IOACCDIR+IOREAD                                              
         BE    ATRN04                                                           
         MVC   FVMSGNO,=AL2(AE$RCNOF)                                           
         MVC   FVXTRA(L'ACTKULA),IOKEYSAV+(ORDKORD-ORDKEY)                      
         B     ROUTL                                                            
ATRN04   MVC   IODAOVER,ORDKDA                                                  
         LA    RE,ATIOAREA                                                      
         ST    RE,IOADDR                                                        
         GOTO1 AIO,IOACCMST+IOGET                                               
         BE    ATRN06                                                           
         MVC   FVMSGNO,=AL2(AE$RCNOF)                                           
         MVC   FVXTRA(L'ACTKULA),IOKEYSAV+(ORDKORD-ORDKEY)                      
         B     ROUTL                                                            
ATRN06   LA    R2,ATIOAREA                                                      
         LA    R0,ORDELQ           GET ORDER ELEMENT                            
         GOTO1 VHELLO,BCPARM,(C'G',ACCMST),((R0),ORDRECD),0                     
         CLI   BCPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,BCPARM+12                                                     
         TM    ORDSTAT-ORDELD(RE),ORDSMNUP                                      
         BNO   ATRN08                                                           
         MVC   FVMSGNO,=AL2(AE$OFMCH)                                           
         MVC   FVXTRA(L'ACTKULA),IOKEYSAV+(ORDKORD-ORDKEY)                      
         B     ROUTL                                                            
*                                                                               
ATRN08   MVC   IOKEY,ATWORK        RESTORE CALLERS KEY                          
         L     R1,ATAR1                                                         
*&&                                                                             
         L     R2,ATATRN                                                        
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD)                     
         TM    TRNRSTAT,TRNSREVS                                                
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RVURI)                                           
         B     ROUTL                                                            
         TM    TRNRSTAT,TRNSDELT                                                
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RECID)                                           
         B     ROUTL                                                            
         USING TRNELD,TRNRFST                                                   
         L     R4,ATAPROR                                                       
         USING PRORATAD,R4         R4=A(PRORATA BLOCK)                          
*                                                                               
         LA    R0,TRXELQ           FIND TRANSACTION EXTRA STATUS ELEM           
         GOTO1 VHELLO,BCPARM,(C'G',ACCMST),((R0),TRNRECD),0                     
         CLI   BCPARM+12,0                                                      
         BE    ATRN10                                                           
         LA    RE,ATWORK           ADD NEW ELEMENT                              
         USING TRXELD,RE                                                        
         XC    TRXEL(TRXLN1Q),TRXEL                                             
         MVI   TRXEL,TRXELQ                                                     
         MVI   TRXLN,TRXLN1Q                                                    
         DROP  RE                                                               
         GOTO1 VHELLO,BCPARM,(C'P',ACCMST),TRNRECD,ATWORK                       
         CLI   BCPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ATRN10   MVC   ATEXCVAL,CSEXCVAL   COPY EXCHANGE RATE AND TRANSPOSE             
         XI    ATEXCVAL+(CSEXCIND-CSEXCVAL),X'01'                               
         ICM   RE,8,ATEXCVAL+(CSEXCSHF-CSEXCVAL)                                
         SRA   RE,32-8                                                          
         LCR   RE,RE                                                            
         STC   RE,ATEXCVAL+(CSEXCSHF-CSEXCVAL)                                  
*                                                                               
         TM    TLXSTAT,TLXSRVAL    TEST REVALUATION REQUIRED                    
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IMREF)                                           
         B     ROUTL                                                            
*                                                                               
         XC    BOELEM,BOELEM                                                    
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3           FIND PENDING PTA ELEMENT                     
         XR    RF,RF                                                            
ATRN12   CLI   PTAEL,0             TEST END-OF-RECORD                           
         BE    ATRN20                                                           
         IC    RF,PTALN            RF=LENGTH OF ELEMENT                         
         CLI   PTAEL,PTAELQ                                                     
         BNE   ATRN18                                                           
         OC    PTADATE,PTADATE     TEST VIRGIN ELEMENT                          
         BNZ   ATRN14                                                           
         ST    R3,ATAVRGN                                                       
         B     ATRN18                                                           
ATRN14   TM    PTASTAT1,PTASPEND   TEST PENDING                                 
         BZ    ATRN18                                                           
         CLC   PTATYPE,ATTYPE      MATCH ON TYPE                                
         BNE   ATRN18                                                           
ATRN16   BCTR  RF,0                COPY INTO BOELEM                             
         EX    RF,*+4                                                           
         MVC   BOELEM(0),PTAELD                                                 
         ST    R3,ATAPTA                                                        
         B     ATRN22                                                           
ATRN18   BXH   R3,RF,ATRN12                                                     
ATRN20   OI    ATINDS,ATINEW       SET NEW ELEMENT                              
         MVC   ATAPTA,ATAVRGN      USE VIRGIN ELEMENT IF FOUND                  
*                                                                               
ATRN22   LA    R3,BOELEM           R3=A(REPLACEMENT PTA ELAMENT)                
         TM    ATINDS,ATINEW                                                    
         BZ    ATRN24                                                           
         MVI   PTAEL,PTAELQ        INITIALIZE IF NEW ELEMENT                    
         MVC   PTATYPE,ATTYPE                                                   
         OI    PTASTAT1,PTASPEND                                                
         ZAP   PTANET,BCPZERO                                                   
         ZAP   PTANETF,BCPZERO                                                  
         ZAP   PTACDSC,BCPZERO                                                  
ATRN24   MVC   PTACUR,CSBILCUR                                                  
         OC    ATANET,ATANET                                                    
         BZ    *+12                                                             
         NI    PTASTAT1,FF-(PTASCASH+PTASHOUR)                                  
         NI    PTASTAT2,FF-(PTASXISA)                                           
         OC    PTASTAT1,ATSTAT1                                                 
*                                                                               
         ZAP   ATAGYVBL,PM$ANVBL   SET AGENCY CURRENCY NET AVAILABLE            
         AP    ATAGYVBL,PTANET                                                  
         ZAP   ATBILVBL,ATAGYVBL   SET BILLING CURRENCY NET AVAILABLE           
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    ATRN26                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    ATRN26                                                           
         ZAP   ATBILVBL,PM$FNVBL                                                
         AP    ATBILVBL,PTANETF                                                 
ATRN26   XR    RE,RE               SET HOURS AVAILABLE                          
         ICM   RE,3,PTAHOURS                                                    
         CVD   RE,ATHRSVBL                                                      
         AP    ATHRSVBL,PM$HRVBL                                                
*                                                                               
         CLI   PTATYPE,PTATRAL                                                  
         BE    ALLREG                                                           
         CLI   PTATYPE,PTATWOF                                                  
         BE    ALLWOF                                                           
         CLI   PTATYPE,PTATTRFT                                                 
         BE    ALLXFR                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
*  - REGULAR BILLING                                                  *         
***********************************************************************         
         SPACE 1                                                                
ALLREG   TM    BCJOBSTA,BCJOBSCB   CHECK BILLING TYPE FOR ALLOCATION            
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INBTY)                                           
         B     ROUTL                                                            
         CLI   CSACT,ACTREV        TEST ACTION IS REVERSE                       
         BE    ALLREG01            OK                                           
         CLI   TRNRSTYP,99         TEST ADVANCE BILLING                         
         BNE   ALLREG01                                                         
         CP    PA$NETBL,BCPZERO    ENSURE ONLY ONE BILL PER ADVANCE             
         BNE   *+14                                                             
         CP    PA$COMBL,BCPZERO                                                 
         BE    ALLREG01                                                         
         MVC   FVMSGNO,=AL2(AE$BILOC)                                           
         B     ROUTL                                                            
ALLREG01 TM    ATINDS,ATINEW       TEST ADDING REGULAR ELEMENT                  
         BZ    ALLREG02                                                         
         ZAP   PTARCOM,BCPZERO                                                  
         ZAP   PTARFCOM,BCPZERO                                                 
ALLREG02 L     RF,AGOPBLK          SET COMMISSION RATE                          
         ZAP   PTARCORT,GOAGYCOM-GOBLOCK(L'GOAGYCOM,RF)                         
*&&UK                                                                           
         CLI   P#UNAUTH,C'Y'       TEST PREVENT ALLOC. OF UNAUTH ITEMS          
         BNE   ALLREG03                                                         
         TM    TRNSTAT,TRNSAUTH                                                 
         BO    ALLREG03                                                         
         MVC   FVMSGNO,=AL2(AE$CMKUI)                                           
         B     ROUTL                                                            
*&&                                                                             
*                                                                               
ALLREG03 ICM   RF,15,ATACOM                                                     
         BZ    *+10                                                             
         ZAP   ATCOM,0(8,RF)                                                    
*&&UK                                                                           
         CLC   CSCPYCUR,CSBILCUR   TEST BILLING IN AGENCY CURRENCY              
         BE    *+14                                                             
         CLC   BCCPYSEC,CSBILCUR   TEST BILLING IN SECOND CURRENCY              
         BNE   ALLREG04                                                         
*&&                                                                             
         MVI   PTALN,PTARLN1Q                                                   
         OC    ATACOM,ATACOM       TEST FOR COMMISSION                          
         BZ    ALLREGX                                                          
         AP    PTARCOM,ATCOM       ADD/SET COMMISSION AMOUNT                    
         CLI   ATSETCOM,0                                                       
         BE    *+10                                                             
         ZAP   PTARCOM,ATCOM                                                    
         B     ALLREGX                                                          
*                                                                               
*&&UK                                                                           
ALLREG04 CLI   PTALN,PTARLN2Q                                                   
         BNL   *+14                                                             
         MVI   PTALN,PTARLN2Q      SET LENGTH FOR FOREIGN CURRENCY              
         ZAP   PTARFCOM,BCPZERO                                                 
         OC    ATACOM,ATACOM       TEST FOR COMMISSION                          
         BZ    ALLREGX                                                          
         AP    PTARFCOM,ATCOM      ADD/SET FOREIGN CURRENCY COMMISSION          
         CLI   ATSETCOM,0                                                       
         BE    *+10                                                             
         ZAP   PTARFCOM,ATCOM                                                   
         ZAP   ATDUB2,PTARFCOM     SET AGENCY COMMISSION AMOUNT                 
         LA    RF,ATWORK                                                        
         USING EURKBLKD,RF                                                      
         XC    0(EURKBLKL,RF),0(RF)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,CSBILCUR                                                
         MVC   EURKRULE,CSEXCVAL                                                
         DROP  RF                                                               
         GOTO1 VEUREKA,BODMCB,('APPLYQ+INVERTQ',ATWORK),ATDUB2,ATDUB1           
         CLI   ATSETCOM,0          TEST ADDING ...                              
         BNE   ALLREG06                                                         
         CP    ATCOM,BCPZERO       ... A NON ZERO AMOUNT                        
         BE    ALLREG06                                                         
         CP    ATDUB1,PTARCOM      ... WITH NO CHANGE IN AGENCY AMOUNT          
         BNE   ALLREG06                                                         
         MVC   FVMSGNO,=AL2(AE$CUZER)                                           
         B     ROUTL                                                            
ALLREG06 ZAP   PTARCOM,ATDUB1                                                   
         CP    PTARCOM,BCPZERO     TEST ZERO IN AGENCY CURRENCY                 
         BNE   ALLREGX                                                          
         CP    PTARFCOM,BCPZERO    BUT NON-ZERO FOREIGN AMOUNT                  
         BE    ALLREGX                                                          
         MVC   FVMSGNO,=AL2(AE$CUZER)                                           
         B     ROUTL                                                            
*&&                                                                             
*                                                                               
ALLREGX  B     ALLVAL                                                           
         SPACE 1                                                                
***********************************************************************         
*  - WRITE-OFFS                                                       *         
***********************************************************************         
         SPACE 1                                                                
ALLWOF   OC    ATANET,ATANET       TEST ALLOCATING THIS TIME                    
         BZ    ALLWOF01                                                         
         CLI   TRNRSTYP,47         DISALLOW WRITE-OFF OF 47/48                  
         BE    *+12                                                             
         CLI   TRNRSTYP,48                                                      
         BNE   ALLWOF01                                                         
         MVC   FVMSGNO,=AL2(AE$NAWTT)                                           
         B     ROUTL                                                            
ALLWOF01 TM    ATINDS,ATINEW                                                    
         BZ    ALLWOFX                                                          
         MVI   PTALN,PTAWLN1Q                                                   
         MVC   PTAWREF,TRNREF                                                   
         MVC   PTAWDAT,TRNDATE                                                  
*&&US*&& B     ALLWOF02                                                         
*        TEST A PROFILE FOR USING ORIGINAL REFERENCE                            
*        BZ    ALLWOF02                                                         
*&&UK*&& MVC   PTAWDAT,BCTODAYP                                                 
         LA    R1,IOKEY            INITIALIZE PTA RECORD KEY                    
         USING PTARECD,R1                                                       
         XC    PTAKEY,PTAKEY                                                    
         MVI   PTAKTYP,PTAKTYPQ                                                 
         MVC   PTAKCPY,CUABIN                                                   
         MVC   PTAKJOB,BCJOBCOD                                                 
         DROP  R1                                                               
         GOTO1 AIO,IOHI+IOACCDIR   FIND NEXT SEQUENCE NO.                       
         XR    RF,RF                                                            
         CLC   IOKEY(PTAKSEQN-PTAKEY),IOKEYSAV                                  
         BNE   *+8                                                              
         ICM   RF,3,IOKEY+(PTAKSEQN-PTAKEY)                                     
         BCTR  RF,0                                                             
         ICM   RF,12,BCEFFS                                                     
         LCR   RF,RF                                                            
         CVD   RF,ATDUB1                                                        
         OI    ATDUB1+7,X'0F'                                                   
         MVC   PTAWREF(L'WTDEFREF),WTDEFREF                                     
         UNPK  PTAWREF+L'WTDEFREF(L'PTAWREF-L'WTDEFREF),ATDUB1                  
*                                                                               
ALLWOF02 EQU   *                                                                
*&&US                                                                           
         CLC   =C'SK',TRNKULC                                                   
         BE    *+14                IF POSTED TO SK/SI THEN WRITE-OFF            
         CLC   =C'SI',TRNKULC      ACCOUNT MUST BE SAME ACCCOUNT IN SI          
         BNE   ALLWOF04                                                         
         MVC   PTAWEUNT(PTAWEACT-PTAWEUNT),=C'SI'                               
         MVC   PTAWEACT,TRNKCACT                                                
         B     ALLWOFX                                                          
*                                                                               
ALLWOF04 CLI   TRNTYPE,62                                                       
         BE    ALLWOFX             NO DEFAULT ACCOUNT FOR PRICE-LISTS           
*        CLI   TRNTYPE,8                                                        
*        BNE   ALLWOF06                                                         
         CLC   =C'SK',TRNKULC                                                   
         BE    ALLWOFX             NO DEFAULT IF SK/SI FLIP COMING              
         CLC   =C'SI',TRNKULC                                                   
         BE    ALLWOFX             OR IF POSTED STRAIGHT TO INCOME              
*&&                                                                             
ALLWOF06 L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         LA    RF,GOEXPTAC-GOBBLOCK(RF)                                         
*&&UK*&& TM    TLXSTAT,TLXSHOUR                                                 
*&&UK*&& BO    *+8                                                              
*&&US*&& CLC   =C'1R',TRNKULC                                                   
*&&US*&& BE    *+8                                                              
         LA    RF,GOEXPOAC-GOEXPTAC(RF)                                         
         USING GOEXPTAC,RF                                                      
         MVC   PTAWEXPA,GOEXPTAC+L'ACTKCPY                                      
         CLI   GOWOFTAC,C' '       TEST WRITE-OFF ACCOUNT DEFINED               
         BNH   *+14                                                             
         MVC   PTAWWOFA,GOWOFTAC+L'ACTKCPY                                      
         MVI   PTALN,PTAWLN2Q                                                   
         DROP  RF                                                               
*                                                                               
ALLWOFX  B     ALLVAL                                                           
         SPACE 1                                                                
***********************************************************************         
*  - TRANSFERS                                                        *         
***********************************************************************         
         SPACE 1                                                                
ALLXFR   CLI   TRNRSTYP,47                                                      
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NAWTT)                                           
         B     ROUTL                                                            
         TM    ATINDS,ATINEW                                                    
         BZ    ALLXFRX                                                          
         MVI   PTALN,PTATLN1Q                                                   
         MVC   PTATJOB,TRNKACT                                                  
         MVC   PTATWRK,TRNKWORK                                                 
         TM    TRNSTAT,TRNSNOCM    TEST COMMISSIONABLE                          
         BO    *+8                                                              
         OI    PTASTAT2,PTASXCOM                                                
ALLXFRX  B     ALLVAL                                                           
         SPACE 1                                                                
***********************************************************************         
* - VALIDATE NET/HOURS AMOUNT                                         *         
* - SET ATNET TO NET AND ATHRS TO HOURS                               *         
***********************************************************************         
         SPACE 1                                                                
ALLVAL   ICM   RF,15,ATANET                                                     
         BZ    ALLUPD                                                           
*                                                                               
         TM    ATSTAT1,PTASHOUR    TEST AMOUNT IN HOURS                         
         BZ    AVAL06                                                           
         ZAP   ATHRS,0(8,RF)                                                    
         CLI   ATSETNET,0          TEST ADDING TO CURRENT VALUE                 
         BNE   AVAL02                                                           
         XR    RE,RE                                                            
         ICM   RE,3,PTAHOURS                                                    
         CVD   RE,ATDUB1                                                        
         AP    ATHRS,ATDUB1                                                     
AVAL02   CP    PA$HOURS,BCPZERO    TEST HOURS ARE VALID                         
         BNE   AVAL04                                                           
         CP    ATHRS,BCPZERO                                                    
         BE    AVAL04                                                           
         MVC   FVMSGNO,=AL2(AE$OALLC)                                           
         B     ROUTL                                                            
AVAL04   DS    0H                                                               
*&&US                                                                           
         GOTO1 QUARTEST,ATHRS      QUARTER HOURS ONLY VALID                     
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$QRTHR)                                           
         B     ROUTL                                                            
*&&                                                                             
         BAS   RE,HRSTONET         CALCULATE NET AMOUNT                         
         B     AVAL09                                                           
*                                                                               
AVAL06   ZAP   ATNET,0(8,RF)       AMOUNT IS NET                                
         CLI   ATSETNET,0          TEST ADDING TO CURRENT                       
         BNE   AVAL08                                                           
         CLC   CSCPYCUR,CSBILCUR   TEST BILLING IN AGENCY CURRENCY              
         BE    AVAL07                                                           
         CLC   BCCPYSEC,CSBILCUR   TEST BILLING IN SECOND CURRENCY              
         BE    AVAL07                                                           
         AP    ATNET,PTANETF                                                    
         B     AVAL08                                                           
AVAL07   AP    ATNET,PTANET                                                     
*                                                                               
AVAL08   BAS   RE,NETTOHRS         CALCULATE HOURS                              
*                                                                               
AVAL09   DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* - TEST FOR PARTIAL ALLOCATION AND IF IT'S ALLOWED                   *         
***********************************************************************         
         SPACE 1                                                                
         TM    BCJOBSTA,BCJOBSCB   TEST CLIENT BILLING JOB                      
         BO    AVAL12                                                           
         CLI   PTATYPE,PTATWOF     NO - TEST WRITE-OFF                          
         BE    AVAL14              YES - DO NOT ALLOW PARTIAL                   
         CLI   PTATYPE,PTATTRFT    TEST TRANSFER                                
         BE    AVAL14              YES - DO NOT ALLOW PARTIAL                   
AVAL12   CLI   TRNRSTYP,62         TEST TYPE 62                                 
         BNE   AVAL19                                                           
AVAL14   CP    ATNET,ATBILVBL      THEN DO NOT ALLOW PARTIAL                    
         BE    AVAL19                                                           
         CP    ATNET,BCPZERO       ZERO IS ALRIGHT                              
         BE    AVAL19                                                           
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     ROUTL                                                            
*                                                                               
AVAL19   DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* - OVER ALLOCATED HOURS ALLOWED FOR GERMANY                          *         
***********************************************************************         
         SPACE 1                                                                
         TM    ATSTAT1,PTASHOUR    TEST AMOUNT IN HOURS                         
         BZ    AVAL29                                                           
*&&UK                                                                           
         CLI   CUCTRY,CTRYGER      OVERALLOCATED HOURS OK FOR GERMANY           
         BNE   AVAL29                                                           
         CLI   PTATYPE,PTATRAL     ONLY APPLIES TO ALLOCATION                   
         BNE   AVAL29                                                           
         CP    PTARCORT,BCPZERO    BUT COMMISSION RATE MUST BE ZERO             
         BNE   AVAL29                                                           
         ZAP   PTARCOM,BCPZERO     SET COMMISSION AMOUNT TO ZERO                
         ZAP   ATDUB1,ATNET                                                     
         CLC   CSCPYCUR,CSBILCUR   TEST FOREIGN CURRENCY                        
         BE    AVAL22                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    AVAL22                                                           
         ZAP   PTARFCOM,BCPZERO                                                 
         ZAP   ATDUB2,ATNET        CONVERT TO AGENCY CURRENCY                   
         LA    RF,ATWORK                                                        
         USING EURKBLKD,RF                                                      
         XC    0(EURKBLKL,RF),0(RF)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,CSBILCUR                                                
         MVC   EURKRULE,CSEXCVAL                                                
         DROP  RF                                                               
         GOTO1 VEUREKA,BODMCB,('APPLYQ+INVERTQ',ATWORK),ATDUB2,ATDUB1           
AVAL22   CP    ATDUB1,ATAGYVBL     TEST NET EXCEEDS AVAILABLE                   
         BNH   AVAL29              NO - FINE                                    
         SP    ATDUB1,ATAGYVBL                                                  
         NI    PTASTAT2,FF-PTASCISN                                             
         ZAP   PTARCOM,ATDUB1      ADD SURPLUS TO COMMISSION                    
         BZ    *+8                 NO SURPLUS                                   
         OI    PTASTAT2,PTASCISN   SET COMM IS NET FOR PRINTING ONLY            
         ZAP   ATNET,ATAGYVBL      SET NET ALLOCATION TO MAX                    
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    AVAL29                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    AVAL29                                                           
         ZAP   ATDUB2,PTARCOM                                                   
         LA    RF,ATWORK                                                        
         USING EURKBLKD,RF                                                      
         XC    0(EURKBLKL,RF),0(RF)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,CSBILCUR                                                
         MVC   EURKRULE,CSEXCVAL                                                
         DROP  RF                                                               
         GOTO1 VEUREKA,BODMCB,('APPLYQ',ATWORK),ATDUB2,ATDUB1                   
         ZAP   PTARFCOM,ATDUB1                                                  
         ZAP   ATDUB2,ATNET                                                     
         LA    RF,ATWORK                                                        
         USING EURKBLKD,RF                                                      
         XC    0(EURKBLKL,RF),0(RF)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,CSBILCUR                                                
         MVC   EURKRULE,CSEXCVAL                                                
         DROP  RF                                                               
         GOTO1 VEUREKA,BODMCB,('APPLYQ',ATWORK),ATDUB2,ATDUB1                   
         ZAP   ATNET,ATDUB1                                                     
*&&                                                                             
AVAL29   DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* - SET AMOUNTS IN PTAELD (AND CONVERT FOREIGN CURRENY VALUE)         *         
***********************************************************************         
         SPACE 1                                                                
         CVB   RE,ATHRS            SET HOURS                                    
         STCM  RE,3,PTAHOURS                                                    
*                                                                               
*&&UK                                                                           
         CLC   CSCPYCUR,CSBILCUR   TEST FOREIGN CURRENCY                        
         BE    *+14                                                             
         CLC   BCCPYSEC,CSBILCUR                                                
         BNE   AVAL32                                                           
*&&                                                                             
         ZAP   PTANET,ATNET                                                     
         B     AVAL39                                                           
*&&UK                                                                           
AVAL32   ZAP   PTANETF,ATNET       ADD/SET FOREIGN CURRENCY AMOUNT              
         CLI   CSACT,ACTREV        TEST ACTION IS REVERSE                       
         BNE   AVAL33                                                           
         ICM   RF,15,ATARVAGY      TEST IF AGENCY NET AMOUNT PASSED             
         BZ    AVAL33                                                           
         ZAP   PTANET,0(8,RF)      SET NET FROM ORIGINAL PTANET                 
         B     AVAL38                                                           
*                                                                               
AVAL33   CP    PTANETF,ATBILVBL    TEST IT'S THE FULL AMT                       
         BNE   AVAL36                                                           
         ZAP   PTANET,ATAGYVBL     THE SET THE FULL AGENCY AMOUNT               
         B     AVAL38                                                           
*                                                                               
*                                                                               
AVAL36   ZAP   ATDUB2,PTANETF      CONVERT TO AGENCY CURRENCY                   
         LA    RF,ATWORK                                                        
         USING EURKBLKD,RF                                                      
         XC    0(EURKBLKL,RF),0(RF)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,CSBILCUR                                                
         MVC   EURKRULE,CSEXCVAL                                                
         DROP  RF                                                               
         GOTO1 VEUREKA,BODMCB,('APPLYQ+INVERTQ',ATWORK),ATDUB2,ATDUB1           
         ZAP   PTANET,ATDUB1                                                    
*                                                                               
AVAL38   CP    PTANET,BCPZERO      TEST ZERO IN AGENCY CURRENCY                 
         BNE   AVAL39                                                           
         CP    PTANETF,BCPZERO     BUT NON-ZERO FOREIGN AMOUNT                  
         BE    AVAL39                                                           
         MVC   FVMSGNO,=AL2(AE$CUZER)                                           
         B     ROUTL                                                            
*&&                                                                             
AVAL39   DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* - TEST FOR OVER ALLOCATION                                          *         
***********************************************************************         
         SPACE 1                                                                
         CLI   PTATYPE,PTATRAL                                                  
         BNE   *+12                                                             
         CLI   TRNTYPE,99                                                       
         BE    AVAL49              OK TO OVERALLOCATE EXTRA BILLING             
         CLI   CSACT,ACTREV        TEST ACTION IS REVERSE                       
         BE    AVAL49              AMOUNTS ARE ALWAYS ALRIGHT                   
*                                                                               
         CP    PTANET,BCPZERO      TEST POSITIVE ALLOCATION                     
         BE    AVAL49              ZERO IS ALWAYS ALRIGHT                       
         BL    AVAL42                                                           
         CP    ATAGYVBL,BCPZERO    YES - AVAILABLE AMOUNT MUST BE TOO           
         BL    *+14                                                             
         CP    PTANET,ATAGYVBL     TEST TOO MUCH CASH ALLOCATED                 
         BNH   AVAL49                                                           
         MVC   FVMSGNO,=AL2(AE$OALLC)                                           
         B     ROUTL                                                            
*                                                                               
AVAL42   CP    ATAGYVBL,BCPZERO    NEG ALLOC - AMT AVAIL MUST BE NEG            
         BNL   *+14                                                             
         CP    PTANET,ATAGYVBL     TEST TOO MUCH CASH ALLOCATED                 
         BNL   AVAL49                                                           
         MVC   FVMSGNO,=AL2(AE$OALLC)                                           
         B     ROUTL                                                            
*                                                                               
AVAL49   DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* - SET CASH DISCOUNT                                                 *         
***********************************************************************         
         SPACE 1                                                                
         CP    PA$DSC,BCPZERO      TEST ANY CASH DISCOUNT                       
         BE    AVAL59                                                           
*                                                                               
         ZAP   ATBIGDUB,PA$DSC     DISOUNT = TOTAL DISCOUNT                     
         MP    ATBIGDUB,PTANET               * PTANET                           
         SRP   ATBIGDUB,2,0                                                     
         DP    ATBIGDUB,PA$NET               / TOTAL NET                        
         SRP   ATBIGDUB(8),64-2,5                                               
         ZAP   PTACDSC,ATBIGDUB(8)                                              
*                                                                               
AVAL59   DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* - UPDATE TRANSACTION RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
ALLUPD   CP    PTANET,BCPZERO      TEST FOR NO ALLOCATION                       
         BNE   AUPD04                                                           
         NI    PTASTAT1,FF-(PTASHOUR+PTASCASH)                                  
         CLI   PTATYPE,PTATRAL                                                  
         BNE   *+14                                                             
         CP    PTARCOM,BCPZERO                                                  
         BNE   AUPD04                                                           
         MVC   PTACUR,BCSPACES     CLEAR CURRENCY CODE                          
*                                                                               
AUPD04   ICM   RF,15,ATAPTA        TEST ELEMENT WAS ON RECORD                   
         BZ    AUPD06                                                           
         IC    RE,PTALN            TEST ELEMENT HAS CHANGED                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   PTAELD(0),0(RF)                                                  
         BE    ALLTRNX                                                          
         GOTO1 VTOBACCO,BCPARM,('TOBAADEL',0),TRNRECD,ACOM,0,(RF)               
AUPD06   OI    LSINDS1,LSIUPREC    UPDATE ACTIVITY INFO                         
         SR    R0,R0                                                            
         CLC   BCCPYSEC,BCSPACES   TEST AGENCY HAS A SECOND CURRENCY            
         BNH   AUPD08                                                           
*        CLC   BCCPYSEC,CSBILCUR   TEST WORKING IN SECOND CURRENCY              
*        BNE   AUPD08                                                           
         CP    PTANET,ATAGYVBL     TEST FULL AMOUNT UTILISED                    
         BNE   AUPD08                                                           
         LA    R0,BODUB1           PASS OCAEL LIST TO TOBACCO                   
         XC    BODUB1(2*L'BODUB1),BODUB1                                        
         MVI   BODUB1,QPTANET                                                   
         ZAP   BODUB1+2(6),PM$FNVBL                                             
         LA    RF,PP$FALLO                                                      
         CLI   PTATYPE,PTATRAL                                                  
         BE    AUPD07                                                           
         LA    RF,PP$FWOFF                                                      
         CLI   PTATYPE,PTATWOF                                                  
         BE    AUPD07                                                           
         LA    RF,PP$FXFER                                                      
         CLI   PTATYPE,PTATTRFT                                                 
         BNE   AUPD08                                                           
AUPD07   AP    BODUB1+2(6),0(L'PP$FALLO,RF)                                     
AUPD08   MVC   PTADATE,BCTODAYC                                                 
         MVC   PTAPERS,CUPASS                                                   
         MVC   BCWORK(L'CSBILCUR),CSBILCUR                                      
         MVC   BCWORK+L'CSBILCUR(L'CSCPYCUR),CSCPYCUR                           
         TM    LSINDS2,LSTOBACC                                                 
         BO    AUPD09                                                           
         MVC   BCWORK(L'CSCPYCUR),CSCPYCUR                                      
         MVC   BCWORK+L'CSCPYCUR(L'BCCPYSEC),BCCPYSEC                           
AUPD09   GOTO1 VTOBACCO,BCPARM,('TOBAAADD',BCWORK),TRNRECD,ACOM,(R0),  *        
               PTAELD                                                           
         MVC   ATAPTA,16(R1)                                                    
         L     R3,16(R1)           R3=A(UPDATED ELEMENT)                        
         CLI   PTATYPE,PTATRAL     TEST REGULAR ALLOCATION                      
         BNE   AUPD10              LEAVE IT ALONE                               
         CP    PTANET,BCPZERO      TEST ELEMENT CAN BE RE-USED                  
         BNE   AUPD10                                                           
         CP    PTARCOM,BCPZERO                                                  
         BNE   AUPD10                                                           
         MVI   PTATYPE,0           CLEAR ELEMENT TYPE                           
         XC    PTADATE,PTADATE     CLEAR ACTIVITY DATE                          
         TM    PTASTAT2,PTASWRUP   TEST ANY WRITE-UP                            
         BZ    AUPD10                                                           
         NI    PTASTAT2,FF-PTASWRUP                                             
         ZAP   PTAWUAMT,BCPZERO    CLEAR WRITE-UP AMOUNT                        
*                                                                               
AUPD10   XR    R0,R0               CALL PRORATA WITH UPDATED RECORD             
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    AUPD12                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    AUPD12                                                           
         LA    R0,CSEXCVAL                                                      
AUPD12   GOTO1 APRORATA,BCPARM,TRNRECD,AGOPBLK,ACOM,(R0),PRORATAD,0             
*                                                                               
         LA    R0,TRXELQ           GET TRANSACTION EXTRA STATUS ELEM            
         GOTO1 VHELLO,BCPARM,(C'G',ACCMST),((R0),TRNRECD),0                     
         CLI   BCPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,BCPARM+12                                                     
         USING TRXELD,RE                                                        
         MVI   ATRSTA2,0           SET AMOUNT PENDING BIT                       
         CP    PP$AALLO,BCPZERO    TEST ALLOCATION PENDING                      
         BE    *+8                                                              
         OI    ATRSTA2,TRNSBILP                                                 
         CP    PP$ACOMM,BCPZERO    TEST COMMISSION PENDING                      
         BE    *+8                                                              
         OI    ATRSTA2,TRNSBILP                                                 
         CP    PP$AWOFF,BCPZERO    TEST WRITE-OFFS PENDING                      
         BE    *+8                                                              
         OI    ATRSTA2,TRNSWOFP                                                 
         CP    PP$AWOFR,BCPZERO    TEST RECOVERIES PENDING                      
         BE    *+8                                                              
         OI    ATRSTA2,TRNSWOFP                                                 
         CP    PP$AXFER,BCPZERO    TEST TRANSFERS PENDING                       
         BE    *+8                                                              
         OI    ATRSTA2,TRNSXFRP                                                 
         NI    TRNRSTA2,FF-(TRNSBILP+TRNSWOFP+TRNSXFRP)                         
         OC    TRNRSTA2,ATRSTA2                                                 
         NI    TRXSTA2,FF-(TRNSBILP+TRNSWOFP+TRNSXFRP)                          
         OC    TRXSTA2,ATRSTA2     ACLD SETS TRNRSTA2 FROM TRXSTA2              
         DROP  RE                                                               
*                                                                               
ALLTRNX  L     R1,ATAR1            P1=A(PTA ELEMENT)                            
         MVC   0(L'ATAPTA,R1),ATAPTA                                            
         B     ROUTE                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO SET ATHRS FROM ATNET                                     *         
***********************************************************************         
         SPACE 1                                                                
NETTOHRS NTR1  ,                                                                
         ZAP   ATHRS,BCPZERO                                                    
         CP    PA$HOURS,BCPZERO    TEST ANY HOURS ON ITEM                       
         BE    NHRSX                                                            
         CP    ATNET,BCPZERO       TEST NET IS ZERO                             
         BE    NHRSX                                                            
         CLI   CSACT,ACTREV        TEST ACTION REVERSE                          
         BE    NHRS10              YES - USE FULL AMOUNTS                       
         CP    ATNET,ATBILVBL      TEST NET = NET AVAILABLE                     
         BNE   *+14                                                             
         ZAP   ATHRS,ATHRSVBL      YES - HOURS = HOURS AVAILABLE                
         B     NHRSX                                                            
*                                                                               
         ZAP   ATMULT,ATHRSVBL                                                  
         ZAP   ATDIV,ATBILVBL                                                   
         CP    ATBILVBL,BCPZERO    TEST NOTHING AVAILABLE                       
         BNE   NHRS20             (YES - USER OVER-ALLOCATING)                  
NHRS10   ZAP   ATMULT,PA$HOURS     USE FULL ITEM AMOUNTS                        
         ZAP   ATDIV,PA$NET                                                     
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    NHRS20                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    NHRS20                                                           
         ZAP   ATDIV,PB$NET                                                     
*                                                                               
NHRS20   ZAP   ATBIGDUB,ATNET      HOURS = NET                                  
         MP    ATBIGDUB,ATMULT             * HOURS AVAILABLE                    
         SRP   ATBIGDUB,2,0                                                     
         DP    ATBIGDUB,ATDIV              / NET AVAILABLE                      
         SRP   ATBIGDUB(8),64-2,5                                               
         ZAP   ATHRS,ATBIGDUB(8)                                                
*&&US                                                                           
         GOTO1 QUARTEST,ATHRS      ENSURE DIVISIBLE BY QUARTER HOUR             
         BAS   RE,HRSTONET         SET NET FROM HOURS                           
*&&                                                                             
NHRSX    B     ROUTX                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO SET ATNET FROM ATHRS                                     *         
***********************************************************************         
         SPACE 1                                                                
HRSTONET NTR1  ,                                                                
         CLI   CSACT,ACTREV        TEST ACTION REVERSE                          
         BE    HNET10              YES - USE FULL AMOUNTS                       
         CP    ATHRS,ATHRSVBL      TEST HOURS = HOURS AVAILABLE                 
         BNE   *+14                                                             
         ZAP   ATNET,ATBILVBL      YES - NET = NET AVAILABLE                    
         B     HNETX                                                            
         CP    ATHRS,BCPZERO       TEST HOURS = ZERO                            
         BNE   *+14                                                             
         ZAP   ATNET,BCPZERO       YES - NET = ZERO                             
         B     HNETX                                                            
*                                                                               
         ZAP   ATMULT,ATBILVBL                                                  
         ZAP   ATDIV,ATHRSVBL                                                   
         CP    ATHRSVBL,BCPZERO    TEST HOURS AVAILABLE IS ZERO                 
         BNE   HNET20              (YES - USER OVER-ALLOCATING)                 
*                                                                               
HNET10   ZAP   ATMULT,PA$NET       USE FULL ITEM AMOUNTS                        
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    HNET12                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    HNET12                                                           
         ZAP   ATMULT,PB$NET                                                    
HNET12   ZAP   ATDIV,PA$HOURS                                                   
*                                                                               
HNET20   ZAP   ATBIGDUB,ATHRS      NET = HOURS                                  
         MP    ATBIGDUB,ATMULT           * NET AVAILABLE                        
         SRP   ATBIGDUB,2,0                                                     
         DP    ATBIGDUB,ATDIV            / HOURS AVAILABLE                      
         SRP   ATBIGDUB(8),64-2,5                                               
         ZAP   ATNET,ATBIGDUB(8)                                                
*                                                                               
HNETX    B     ROUTX                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO TEST AMOUNT IS DIVISIBLE BY QUARTER HOURS                *         
*                                                                     *         
* NTRY: R1 = A(PL8 AMOUNT)                                            *         
* EXIT: CC = EQUAL IF AMOUNT DIVISIBLE BY QUARTER HOUR                *         
***********************************************************************         
         SPACE 1                                                                
QUARTEST ZAP   ATQUART,0(8,R1)                                                  
         DP    ATQUART,=P'25'                                                   
         MP    ATQUART(6),=P'25'                                                
         CP    0(8,R1),ATQUART(6)                                               
         BER   RE                                                               
         ZAP   0(8,R1),ATQUART(6)                                               
         LTR   RE,RE               SET CC NOT EQUAL                             
         BR    RE                                                               
         SPACE 1                                                                
         DROP  R2,R3,R4,RC                                                      
         SPACE 1                                                                
WTDEFREF DC    C'WO'                                                            
         DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* ALLTRN S/R LOCAL W/S                                                *         
***********************************************************************         
         SPACE 1                                                                
ATWORKD  DSECT                                                                  
ATPARMS  DS    0XL20                                                            
ATTYPE   DS    0XL1                PTA ELEMENT TYPE                             
ATATRN   DS    A                   A(TRANSACTION RECORD)                        
ATSTAT1  DS    0XL1                PTASTAT1 OC VALUE                            
ATAPROR  DS    A                   A(PRORATA BLOCK)                             
ATSETNET DS    0XL1                NON-ZERO TO SET NET AMOUNT                   
ATANET   DS    A                   A(ALLOCATED NET AMOUNT)                      
ATSETCOM DS    0XL1                NON-ZERO TO SET COMM AMOUNT                  
ATACOM   DS    A                   A(ALLOCATED COMMISION AMOUNT)                
ATARVAGY DS    A                   A(AGYCURR NET AMOUNT FOR REVERSAL)           
         ORG   ATPARMS+L'ATPARMS                                                
         DS    0D                                                               
ATNET    DS    PL8                 ALLOCATED NET AMOUNT                         
ATHRS    DS    PL8                 ALLOCATED TIME                               
ATCOM    DS    PL8                 ALLOCATED COMM AMOUNT                        
ATAGYVBL DS    PL8                 NET AMOUNT AVAILABLE (AGENCY)                
ATBILVBL DS    PL8                 NET AMOUNT AVAILABLE (BILLING)               
ATHRSVBL DS    PL8                 HOURS AVAILABLE                              
ATDUB1   DS    PL8                                                              
ATDUB2   DS    PL8                                                              
ATBIGDUB DS    PL16                                                             
ATQUART  DS    PL8                                                              
ATMULT   DS    PL8                                                              
ATDIV    DS    PL8                                                              
*                                                                               
ATAR1    DS    A                   A(CALLERS R1)                                
ATAVRGN  DS    A                   A(VIRGIN ELEMENT)                            
ATAPTA   DS    A                   A(PTA ELEMENT)                               
ATAUWOF  DS    A                   (UPDATED WRITE-OFF ELEMENT)                  
ATEXCVAL DS    XL(L'CSEXCVAL)                                                   
ATINDS   DS    XL1                                                              
ATINEW   EQU   X'80'               ADDING THE PTA ELEMENT                       
ATRSTA2  DS    XL1                 PENDING AMOUNT BYTE                          
ATWORK   DS    XL64                                                             
ATIOAREA DS    XL(IOAREA2-IOAREA1) MY OWN IO AREA                               
ATWORKL  EQU   *-ATWORKD                                                        
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ/CREATE PTA RECORD                                   *         
*                                                                     *         
* NTRY: P1 - BYTE 0 = ZERO IF PTA RECORD TO BE UPDATED                *         
*               1-3 = A(TRANSACTION RECORD)                           *         
*       P2          = A(AREA FOR PTA RECORD)                          *         
*       P3          = A(PRORATA BLOCK)                                *         
*       P4 - BYTE 0 = PTATWOFR FOR WRITE-OFF RECOVERY ELEMENT         *         
*               1-3 = 0 TO FIND CURRENT PTATWOFR ELEMENT              *         
*                   = A(W/O REF#) TO ADD PTATWOFR ELEMENT             *         
*                                                                     *         
* EXIT: P1          = A(PTA ELEMENT IN TRANSACTION RECORD)            *         
*       P2          = A(PTA ELEMENT FOR UPDATED WRITE OFF)            *         
*       BOWORK1(12) = EXISTING DEPARTMENT CODE ON PTA RECORD          *         
*    BOWORK1+12(12) = EXISTING STAFF CODE ON PTA RECORD               *         
*    BOWORK1+24(02) = ANALYSIS OFFICE CODE (US ONLY)                  *         
*                CC = HIGH IF PTA RECORD NOT CURRENTLY ON FILE        *         
*                CC = LOW IF ERROR                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING GPWORKD,RC                                                       
GETPTA   DS    0H                                                               
         USING *,R8                                                             
         MVC   GPPARMS,0(R1)                                                    
         ST    R1,GPAR1                                                         
         MVC   BOWORK1,BCSPACES                                                 
*                                                                               
         CLI   GPWOFR,PTATWOFR     TEST RECOVERY ELEMENT REQUIRED               
         BNE   GPTA50                                                           
         XR    RF,RF                                                            
         ICM   RF,7,GPASEQN        TEST ADDING NEW ELEMENT                      
         BZ    GPTA40                                                           
         L     R2,GPATRN                                                        
         USING TRNRECD,R2                                                       
         MVC   GPSEQN,0(RF)                                                     
         LA    R3,TRNRFST          FIND CURRENT PENDING RECOVERY                
         USING PTAELD,R3                                                        
         XR    RF,RF                                                            
GPTA02   CLI   PTAEL,0                                                          
         BE    GPTA10                                                           
         CLI   PTAEL,PTAELQ                                                     
         BNE   GPTA08                                                           
         CLI   PTATYPE,PTATWOFR                                                 
         BNE   GPTA04                                                           
         CLC   PTASEQN,GPSEQN                                                   
         BNE   GPTA08                                                           
         MVC   FVMSGNO,=AL2(AE$WORUP) RECOVERY ALREADY UPDATED                  
         B     ROUTL                                                            
GPTA04   CLI   PTATYPE,PTATWOF                                                  
         BNE   GPTA08                                                           
         CLC   PTASEQN,GPSEQN                                                   
         BNE   GPTA08                                                           
         TM    PTASTAT1,PTASPEND                                                
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WOPEN) WRITE OFF IS STILL PENDING                
         B     ROUTL                                                            
         ST    R3,GPAUWOF                                                       
GPTA08   IC    RF,PTALN                                                         
         BXH   R3,RF,GPTA02                                                     
*                                                                               
GPTA10   ICM   R3,15,GPAUWOF                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WONOF) WRITE OFF NOT ON FILE                     
         B     ROUTL                                                            
         MVC   BOELEM,PTAELD                                                    
         LA    R3,BOELEM                                                        
         MVC   PTADATE,BCTODAYC    SET ACTIVITY INFO                            
         MVC   PTAPERS,CUPASS                                                   
         MVI   PTATYPE,PTATWOFR    SET ELEMENT TYPE                             
         OI    PTASTAT1,PTASPEND                                                
         ZAP   GPPL8,PTANET        NEGATE NUMBERS                               
         MP    GPPL8,PMINUS1                                                    
         ZAP   PTANET,GPPL8                                                     
         ZAP   GPPL8,PTANETF                                                    
         MP    GPPL8,PMINUS1                                                    
         ZAP   PTANETF,GPPL8                                                    
         ZAP   GPPL8,PTACDSC                                                    
         MP    GPPL8,PMINUS1                                                    
         ZAP   PTACDSC,GPPL8                                                    
         XR    RE,RE                                                            
         ICM   RE,3,PTAHOURS                                                    
         LCR   RE,RE                                                            
         STCM  RE,3,PTAHOURS                                                    
         GOTO1 ATOBCHA,BCPARM,GPATRN,0,PTAELD                                   
         DROP  R3                                                               
*                                                                               
*&&UK                                                                           
         TM    BCCPYST7,CPYSSCNV   HAS SECONDARY CURRENCY?                      
         BNO   GPTA30                                                           
         LA    R4,TRNRFST                                                       
         USING OCAELD,R4           FIND OCAELD                                  
         XR    RF,RF                                                            
GPTA12   CLI   OCAEL,OCAELQ                                                     
         BE    GPTA14                                                           
         CLI   OCAEL,0                                                          
         BE    GPTA30                                                           
         IC    RF,OCALN                                                         
         BXH   R4,RF,GPTA12                                                     
GPTA14   MVC   GPOCAIND,OCAINDS    SAVE ORIGINAL OCAINDS VALUE                  
         DROP  R4                                                               
*                                                                               
         LA    R0,TOBAACVB         CONVERT RECORD OTHER WAY                     
         TM    GPOCAIND,OCAIDSEC                                                
         BO    *+8                                                              
         LA    R0,TOBAACVS                                                      
         GOTO1 VTOBACCO,BCPARM,((R0),TOBCUR),TRNRECD,ACOM,0,0                   
*                                                                               
         LA    R3,TRNRFST                                                       
O        USING PTAELD,R3                                                        
         XR    RF,RF                                                            
GPTA16   CLI   O.PTAEL,PTAELQ      SEARCH FOR ORIGINAL PTAELD                   
         BNE   GPTA18                                                           
         CLI   O.PTATYPE,PTATWOF                                                
         BNE   GPTA18                                                           
         TM    O.PTASTAT1,PTASPEND                                              
         BO    GPTA18                                                           
         CLC   O.PTASEQN,GPSEQN                                                 
         BE    GPTA20                                                           
GPTA18   IC    RF,O.PTALN                                                       
         BXH   R3,RF,GPTA16                                                     
*                                                                               
GPTA20   LA    R4,TRNRFST                                                       
R        USING PTAELD,R4           SEARCH FOR WRITE-OFF RECOVERY                
GPTA22   CLI   R.PTAEL,PTAELQ                                                   
         BNE   GPTA24                                                           
         CLI   R.PTATYPE,PTATWOFR                                               
         BNE   GPTA24                                                           
         TM    R.PTASTAT1,PTASPEND                                              
         BZ    GPTA24                                                           
         CLC   R.PTASEQN,GPSEQN                                                 
         BE    GPTA26                                                           
GPTA24   IC    RF,R.PTALN                                                       
         BXH   R4,RF,GPTA22                                                     
*                                                                               
GPTA26   DS    0H                                                               
         ZAP   GPPL8,O.PTANET      SET RECOVERY TO EXACTLY -ORIGINAL            
         MP    GPPL8,PMINUS1                                                    
         ZAP   R.PTANET,GPPL8                                                   
         ZAP   GPPL8,O.PTANETF                                                  
         MP    GPPL8,PMINUS1                                                    
         ZAP   R.PTANETF,GPPL8                                                  
         ZAP   GPPL8,O.PTACDSC                                                  
         MP    GPPL8,PMINUS1                                                    
         ZAP   R.PTACDSC,GPPL8                                                  
         DROP  O,R                                                              
*                                                                               
         LA    R0,TOBAACVS         CONVERT RECORD BACK TO AS IT WAS             
         TM    GPOCAIND,OCAIDSEC                                                
         BO    *+8                                                              
         LA    R0,TOBAACVB                                                      
         GOTO1 VTOBACCO,BCPARM,((R0),TOBCUR),TRNRECD,ACOM,0,0                   
*&&                                                                             
GPTA30   DS    0H                                                               
         XR    R0,R0               CALL PRORATA WITH UPDATED RECORD             
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    GPTA32                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    GPTA32                                                           
         LA    R0,CSEXCVAL                                                      
GPTA32   GOTO1 APRORATA,BCPARM,TRNRECD,AGOPBLK,ACOM,(R0),GPAPRB,0               
*                                                                               
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
         XR    RF,RF                                                            
GPTA34   CLI   PTAEL,PTAELQ        FIND RECOVERY ELEMENT AGAIN                  
         BNE   GPTA36                                                           
         CLI   PTATYPE,PTATWOFR                                                 
         BNE   GPTA36                                                           
         TM    PTASTAT1,PTASPEND                                                
         BZ    GPTA36                                                           
         CLC   PTASEQN,GPSEQN                                                   
         BE    *+12                                                             
GPTA36   IC    RF,PTALN                                                         
         BXH   R3,RF,GPTA34                                                     
         B     GPTA70                                                           
         DROP  R2,R3                                                            
*                                                                               
GPTA40   L     R3,GPATRN           FIND CURRENT PENDING RECOVERY                
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         USING PTAELD,R3                                                        
         XR    RF,RF                                                            
GPTA42   CLI   PTAEL,0                                                          
         BE    ROUTL               CC=LOW IF NOT THEREE                         
         CLI   PTAEL,PTAELQ                                                     
         BNE   GPTA48                                                           
         CLI   PTATYPE,PTATWOFR                                                 
         BNE   GPTA48                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    GPTA70                                                           
GPTA48   IC    RF,PTALN                                                         
         BXH   R3,RF,GPTA42                                                     
*                                                                               
GPTA50   L     R3,GPATRN                                                        
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         USING PTAELD,R3                                                        
         XR    RF,RF                                                            
GPTA52   CLI   PTAEL,0             LOOK FOR EXISTING PTA ELEMENT                
         BE    GPTA60                                                           
         CLI   PTAEL,PTAELQ                                                     
         BNE   GPTA58                                                           
         CLI   PTATYPE,PTATWOF     TEST WRITE-OFF TYPE                          
         BNE   GPTA58                                                           
         TM    PTASTAT1,PTASPEND   TEST PENDING                                 
         BO    GPTA70                                                           
GPTA58   IC    RF,PTALN                                                         
         BXH   R3,RF,GPTA52                                                     
*                                                                               
GPTA60   GOTO1 AALLTRN,BCPARM,('PTATWOF',GPATRN),GPAPRB,0,0                     
         BNE   ROUTL                                                            
*        BE    *+6                                                              
*        DC    H'0'                                                             
         L     R3,0(R1)            R3=A(NEW PTA ELEMENT)                        
*                                                                               
GPTA70   LA    R2,IOKEY            INITIALIZE PTA RECORD KEY                    
         USING PTARECD,R2                                                       
         XC    PTAKEY,PTAKEY                                                    
         MVI   PTAKTYP,PTAKTYPQ                                                 
         MVC   PTAKCPY,CUABIN                                                   
         MVC   PTAKJOB,BCJOBCOD                                                 
         OC    PTAKSEQN,PTASEQN    TEST RECORD ON FILE                          
         BZ    GPTA72                                                           
         MVC   IOADDR,GPAPTA       READ RECORD FOR UPDATE                       
         LA    R1,IOACCMST+IOREAD                                               
         CLI   GPUPD,GPUPDQ                                                     
         BNE   *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETCODS,BCPARM,GPAPTA,BOWORK1,BOWORK1+12,BOWORK1+24              
         B     GPTA80                                                           
*                                                                               
GPTA72   OI    GPINDS,GPIADD                                                    
*                                                                               
         MVC   GPOFFICE,CSOFFICE   SET OFFICE CODE                              
         L     RE,GPATRN                                                        
         LA    RE,TRNRFST-TRNRECD(RE)                                           
         USING ANOELD,RE                                                        
         XR    RF,RF                                                            
GPTA73   CLI   ANOEL,0                                                          
         BE    GPTA74                                                           
         CLI   ANOEL,ANOELQ                                                     
         BNE   *+12                                                             
         CLI   ANOTYPE,ANOTCLI                                                  
         BE    *+12                                                             
         IC    RF,ANOLN                                                         
         BXH   RE,RF,GPTA73                                                     
         MVC   GPOFFICE,ANOOFFC                                                 
         DROP  RE                                                               
*                                                                               
GPTA74   L     R2,GPAPTA           CREATE PTA RECORD                            
         XC    PTARECD(256),PTARECD                                             
         MVC   PTAKEY,IOKEY                                                     
         LA    RF,PTARFST                                                       
         USING TRNELD,RF           ADD TRANSACTION ELEMENT TO RECORD            
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q                                                    
*&&UK*&& MVI   TRNTYPE,POSTWOFF                                                 
*&&US                                                                           
         MVI   TRNTYPE,POSTWOFC                                                 
         TM    TLXSTAT,TLXSHOUR                                                 
         BZ    *+8                                                              
         MVI   TRNTYPE,POSTWOFT                                                 
*&&                                                                             
GPTA76   MVC   TRNMOS,BCTMON                                                    
         MVC   TRNOFFC,GPOFFICE                                                 
         LA    R0,PTARFST-PTARECD+1+TRNLN1Q                                     
         STCM  R0,3,PTARLEN                                                     
         DROP  RF                                                               
*                                                                               
GPTA80   L     R1,GPAR1            SAVE A(PTA ELEMENT)                          
         ST    R3,0(R1)                                                         
         CLI   GPWOFR,0                                                         
         BNE   *+8                                                              
         OI    LSINDS1,LSIWOPTA    SET HAVE PTA RECORD                          
         TM    GPINDS,GPIADD                                                    
         BO    ROUTH                                                            
         B     ROUTE                                                            
         DROP  R2,R3,RC                                                         
         SPACE 1                                                                
GPWORKD  DSECT                     ** GETPTA S/R LOCAL W/S **                   
GPPARMS  DS    0XL16                                                            
GPUPD    DS    0XL1                READ FOR UPDATE                              
GPUPDQ   EQU   00                                                               
GPATRN   DS    A                   A(TRANSACTION RECORD)                        
GPAPTA   DS    A                   A(PTA RECORD)                                
GPAPRB   DS    A                   A(PRO-RATA BLOCK)                            
GPWOFR   DS    XL1                 =PTATWOFR FOR WRITE-OFF RECOVERY             
GPASEQN  DS    AL3                 A(W/O SEQUENCE #) TO ADD PTATWOFR EL         
*                                                                               
GPAR1    DS    A                   A(R1)                                        
GPAUWOF  DS    A                   A(UPDATED WRITE-OFF ELEMENT)                 
GPINDS   DS    XL1                 INDICATORS                                   
GPIADD   EQU   X'80'               PTA RECORD NEEDS TO BE ADDED                 
GPSEQN   DS    XL2                                                              
GPOFFICE DS    CL2                 OFFICE CODE                                  
GPPL8    DS    PL8                                                              
GPOCAIND DS    XL1                 SAVED OCAINDS VALUE                          
GPWORKL  EQU   *-GPWORKD                                                        
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE THE PTA RECORD ON FILE                                       *         
*                                                                     *         
* NTRY: P1 BYTE 0 = C'T' TO TEST RECORD CAN BE UPDATED (DON'T UPDATE) *         
*                  X'00' TO UPDATE PTA RECORD                         *         
*             1-3 = A(TRANSACTION RECORD)                             *         
*       P2        = A(PTA RECORD)                                     *         
* EXIT:        CC = HIGH IF PTA RECORD ADDED, DELEDTED OR RESTORED    *         
*              CC = EQUAL IF PTA RECORD WRITTEN                       *         
*              CC = LOW IF ERROR                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING PPWORKD,RC                                                       
PUTPTA   DS    0H                                                               
         USING *,R8                                                             
         MVC   PPPARMS,0(R1)                                                    
         MVC   PPTEST,PPPARMS                                                   
         MVI   PPPARMS,0                                                        
*                                                                               
         L     RE,AIO1             SAVE CALLERS IO1                             
         XR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(RE)                                         
         AH    RF,=Y(L'IODA+L'IOWORK)                                           
         SH    RE,=Y(L'IODA+L'IOWORK)                                           
         LA    R0,PPSAVIO1                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         LA    RE,PPSAVIO1+L'IODA+L'IOWORK                                      
         CLC   PPATRN,AIO1                                                      
         BNE   *+8                                                              
         ST    RE,PPATRN                                                        
         CLC   PPAPTA,AIO1                                                      
         BNE   *+8                                                              
         ST    RE,PPAPTA                                                        
*                                                                               
         L     R4,PPAPTA                                                        
         USING PTARECD,R4                                                       
         CLI   PTAKTYP,PTAKTYPQ                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,PPATRN                                                        
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         USING PTAELD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
PPTA02   CLI   PTAEL,0                                                          
         BE    PUTPTAL                                                          
         CLI   PTAEL,PTAELQ                                                     
         BNE   PPTA04                                                           
         CLI   PTATYPE,PTATWOF                                                  
         BNE   PPTA04                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    PPTA10                                                           
PPTA04   IC    RF,PTALN                                                         
         BXH   R3,RF,PPTA02                                                     
*                                                                               
PPTA10   CP    PTANET,BCPZERO      TEST ZERO WRITE-OFF AMOUNT                   
         BNE   PPTA20                                                           
         CLI   PPTEST,PPTESTQ      ONLY TESTING?                                
         BE    PPTA12                                                           
         GOTO1 ATOBCHA,BCPARM,PPATRN,PTAELD,0                                   
*        MVI   PTAEL,FF                                                         
*        GOTO1 VHELLO,BCPARM,(C'D',ACCMST),('FF',PPATRN),0                      
         OC    PTAKSEQN,PTAKSEQN   TEST PTA RECORD EXISTS                       
         BZ    PUTPTAE                                                          
*                                                                               
         MVC   IOKEY,PTAKEY        DELETE PTA RECORD                            
         GOTO1 AIO,IOREAD+IOLOCK+IOACCDIR                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    IOKEY+(PTAKSTAT-PTARECD),PTASDELT                                
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,IOKEY+(PTAKDA-PTARECD)                                  
         GOTO1 AIO,IOGET+IOLOCK+IOACCMST+IO1                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1                                                          
         OI    PTARSTAT-PTARECD(RF),PTASDELT                                    
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
PPTA12   B     PUTPTAH                                                          
*                                                                               
PPTA20   LA    RF,PTARFST                                                       
         USING TRNELD,RF                                                        
         MVC   TRNREF,PTAWREF                                                   
         MVC   TRNDATE,PTAWDAT                                                  
         MVC   TRNAMNT,PTANET                                                   
         DROP  RF                                                               
*                                                                               
         OC    PTAKSEQN,PTAKSEQN   TEST ADDING RECORD                           
         BNZ   PPTA40                                                           
*                                                                               
         LA    R1,PTARFST                                                       
         USING SPAELD,R1                                                        
         XR    RF,RF                                                            
PPTA22   CLI   SPAEL,0                                                          
         BE    PPTA24                                                           
         CLI   SPAEL,SPAELQ                                                     
         BE    PPTA26                                                           
         IC    RF,SPALN                                                         
         BXH   R1,RF,PPTA22                                                     
         DROP  R1                                                               
PPTA24   GOTO1 AWOPPTA,BCPARM,PPATRN,PPAPTA,0,0                                 
         BNE   PUTPTAL                                                          
*                                                                               
PPTA26   CLI   PPTEST,PPTESTQ      ONLY TESTING                                 
         BE    PPTA38                                                           
         MVC   IOKEY,PTAKEY                                                     
         GOTO1 AIO,IOHI+IOACCDIR   FIND NEXT SEQUENCE NO.                       
         XR    RF,RF                                                            
         CLC   PTAKEY(PTAKSEQN-PTAKEY),IOKEY                                    
         BNE   *+8                                                              
         ICM   RF,3,IOKEY+(PTAKSEQN-PTAKEY)                                     
         BCTR  RF,0                                                             
         STCM  RF,3,PTASEQN        SET SEQUENCE NO. IN PTA ELEMENT              
         OC    PTAKSEQN,PTASEQN    AND PTA RECORD                               
         BNZ   *+6                                                              
         DC    H'0'                SEQUENCE NO. MUST NOT BE 0                   
*                                                                               
         MVC   IOKEY,PTAKEY                                                     
         GOTO1 AIO,IORDUPD+IOACCDIR                                             
         CLI   IOERR,IOERNF        TEST RECORD NOT ON FILE                      
         BE    PPTA30                                                           
         CLI   IOERR,IOEDEL        TEST RECORD DELETED                          
         BE    *+6                 YES - RESTORE IT                             
         DC    H'0'                                                             
         NI    IOKEY+(PTAKSTAT-PTARECD),FF-PTASDELT                             
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,IOKEY+(PTAKDA-PTARECD)                                  
         GOTO1 AIO,IOGET+IOLOCK+IOACCMST+IO1                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R4,IOADDR                                                        
         GOTO1 AIO,IOPUT+IOACCMST                                               
         BE    PPTA38                                                           
         DC    H'0'                                                             
*                                                                               
PPTA30   ST    R4,IOADDR                                                        
         GOTO1 AIO,IOADD+IOACCMST                                               
         BE    *+6                                                              
         DC    H'0'                                                             
PPTA38   B     PUTPTAH                                                          
*                                                                               
PPTA40   CLI   PPTEST,PPTESTQ      ONLY TESTING?                                
         BE    PPTA42                                                           
         MVC   IOKEY,PTAKEY                                                     
         GOTO1 AIO,IORDUP+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R4,IOADDR                                                        
         GOTO1 AIO,IOWRITE+IOACCMST                                             
         BE    *+6                                                              
         DC    H'0'                                                             
PPTA42   B     PUTPTAE                                                          
         DROP  R3,R4                                                            
*                                                                               
PUTPTAL  MVI   BCDUB,0             SET CC LOW                                   
         B     PUTPTAX                                                          
PUTPTAH  MVI   BCDUB,2             SET CC HIGH                                  
         B     PUTPTAX                                                          
PUTPTAE  MVI   BCDUB,1             SET CC EQUAL                                 
*                                                                               
PUTPTAX  LA    RE,PPSAVIO1         RESTORE CALLERS IO1                          
         XR    RF,RF                                                            
         ICM   RF,3,L'IODA+L'IOWORK+(TRNRLEN-TRNRECD)(RE)                       
         AH    RF,=Y(L'IODA+L'IOWORK)                                           
         L     R0,AIO1                                                          
         SH    R0,=Y(L'IODA+L'IOWORK)                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     ROUTCC                                                           
         SPACE 1                                                                
PPWORKD  DSECT                     ** PUTPTA S/R LOCAL W/S **                   
PPPARMS  DS    0XL8                                                             
PPATRN   DS    A                                                                
PPAPTA   DS    A                                                                
PPTEST   DS    XL1                                                              
PPTESTQ  EQU   C'T'                                                             
PPSAVIO1 DS    XL(IOAREA2-IOAREA1) AREA TO SAVE USER'S IO1                      
PPWORKL  EQU   *-PPWORKD                                                        
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CREATE WRITE-OFF POSTINGS ON THE PTA RECORD                         *         
*                                                                     *         
* NTRY: P1=A(TRANSACTION RECORD)                                      *         
*       P2=A(PTA RECORD)                                              *         
*       P3=A(NEW DEPARTMENT CODE) OR 0                                *         
*       P4=A(NEW STAFF CODE) OR 0                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING WPWORKD,RC                                                       
         USING CATD,WPCATBLK                                                    
WOPPTA   DS    0H                                                               
         USING *,R8                                                             
         MVC   WPPARMS,0(R1)                                                    
         ST    R1,WPAR1                                                         
*                                                                               
         MVI   WPCODES,C' '        INITIIALIZE ACCOUNT CODES                    
         MVC   WPCODES+1(WPCODESL-1),WPCODES                                    
         MVC   WP9L2(2),NINES                                                   
         MVC   WP9L3(3),NINES                                                   
         MVC   WP9L4(4),NINES                                                   
         MVC   WP9L12,NINES                                                     
         MVI   WPSEP,C'-'                                                       
*                                                                               
         GOTO1 GETCODS,BCPARM,WPAPTA,WPDPT,WPSTF,WPANLOFF                       
         ICM   RF,15,WPADEPT       TEST DEPT GIVEN                              
         BZ    *+10                                                             
         MVC   WPDPT,0(RF)                                                      
         ICM   RF,15,WPASTAFF      TEST STAFF GIVEN                             
         BZ    *+10                                                             
         MVC   WPSTF,0(RF)                                                      
*                                                                               
         L     RF,WPAPTA                                                        
         MVC   WPOFF(L'TRNOFFC),(PTARFST-PTARECD)+(TRNOFFC-TRNELD)(RF)          
*&&US                                                                           
         CLI   WPANLOFF,C' '       OFFICE=ANALYSIS OFFICE IF DEFINED            
         BNH   *+10                                                             
         MVC   WPOFF(L'WPANLOFF),WPANLOFF                                       
*&&                                                                             
         TM    BCCPYST1,CPYSOROE   TEST OFFICE REQUIRED                         
         BZ    WPTA01                                                           
         CLI   WPOFF,C' '          TEST OFFICE EXISTS                           
         BH    WPTA01                                                           
         MVC   FVMSGNO,=AL2(AE$IOFCQ)                                           
         B     ROUTL                                                            
*                                                                               
WPTA01   L     R3,WPATRN                                                        
         LA    R3,PTARFST-PTARECD(R3)                                           
         USING PTAELD,R3                                                        
         XR    RF,RF                                                            
WPTA02   CLI   PTAEL,0                                                          
         BE    ROUTL                                                            
         CLI   PTAEL,PTAELQ                                                     
         BNE   WPTA04                                                           
         CLI   PTATYPE,PTATWOF                                                  
         BNE   WPTA04                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    WPTA06                                                           
WPTA04   IC    RF,PTALN                                                         
         BXH   R3,RF,WPTA02                                                     
WPTA06   GOTO1 VALCOD,PTAWEXPA                                                  
         BNE   ROUTL                                                            
         MVC   WPEXP,PTAWEACT                                                   
*&&UK                                                                           
         CLC   INCSUSUL,PTAWEXPA   NO UNIT 1 POSTINGS IF TO SK                  
         BNE   *+10                                                             
         OC    WPSTAT,=AL2(WOPSNOU1)                                            
*&&                                                                             
         DROP  R3                                                               
*                                                                               
         CLC   INCUL,ACCODE+(ACTKUNT-ACTKCPY) TEST INCOME LEDGER                
         BNE   *+10                                                             
         OC    WPSTAT,=AL2(WOPSINCM)                                            
         CLI   WPDPT,C' '          TEST DEPARTMENT DEFINED                      
         BNH   *+10                                                             
         OC    WPSTAT,=AL2(WOPSDEPD)                                            
         TM    ACBSTAT,ACBSDEPT                                                 
         BZ    WPTA12                                                           
         OC    WPSTAT,=AL2(WOPSDEPT) DEPARTMENT REQUIRED                        
         CLI   WPDPT,C' '                                                       
         BH    WPTA12                                                           
         MVC   FVMSGNO,=AL2(AE$IDPTQ)                                           
         B     ROUTL                                                            
WPTA12   TM    ACBSTAT,ACBSPERS                                                 
         BZ    WPTA14                                                           
         OC    WPSTAT,=AL2(WOPSSTAF) STAFF REQUIRED                             
         CLI   WPSTF,C' '                                                       
         BH    WPTA14                                                           
         MVC   FVMSGNO,=AL2(AE$ISTFQ)                                           
         B     ROUTL                                                            
WPTA14   MVC   WPDPT1P(2),WPDPT    1P DEPT CODE IS FIRST TWO CHARS              
*&&UK                                                                           
         MVC   WPANC(L'ACCOST),ACCOST                                           
*        TM    BCCPYST5,CPYSNCST   TEST NEW COST SYSTEM IN USE                  
*        BZ    *+10                                                             
*        OC    WPSTAT,=AL2(WOPSNCST)                                            
*&&                                                                             
*&&US                                                                           
         TM    BCCPYST5,CPYSNCST   TEST NEW COST SYSTEM IN USE                  
         BZ    WPTA15                                                           
         CLC   EXPUL,ACCODE+(ACTKUNT-ACTKCPY)  TEST EXPENSE LEDGER              
         BNE   WPTA15                                                           
         OC    WPSTAT,=AL2(WOPSNCST)                                            
         B     WPTA20                                                           
WPTA15   MVC   WPANC(L'ACCOST),ACCOST                                           
         CLI   WPANC,C' '                                                       
         BH    WPTA24                                                           
         ICM   R1,15,ACASPA                                                     
         BZ    WPTA24                                                           
         USING SPAELD,R1                                                        
         XR    RF,RF                                                            
WPTA16   CLI   SPAEL,SPAELQ                                                     
         BNE   WPTA24                                                           
         CLI   SPATYPE,SPATANAL                                                 
         BE    *+12                                                             
         IC    RF,SPALN                                                         
         BXH   R1,RF,WPTA16                                                     
         MVC   WPANC,SPAAANAL                                                   
         B     WPTA24                                                           
         DROP  R1                                                               
WPTA20   MVC   CATDMGR,VDMGR       CALL CAT TO CAN ANALYSIS CODE                
         MVC   CATSEAC,ACCODE                                                   
         MVC   CATOFF,WPANLOFF                                                  
         MVC   CATDPT,WPDPT                                                     
         GOTO1 VCATCALL,CATD                                                    
         CLI   CATERR,0            TEST 13 LEDGER ACCOUNT INVALID               
         BE    WPTA22                                                           
         MVC   FVXTRA(L'CATACC3-CPYKEND),CATACC3+CPYKEND                        
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     ROUTL                                                            
WPTA22   MVI   WPANC,C' '                                                       
         CLI   CATPST,C'N'                                                      
         BE    *+10                                                             
         MVC   WPANC(L'CATACC3-LDGKEND),CATACC3+LDGKEND                         
*&&                                                                             
WPTA24   CLI   WPANC,C' '          TEST COSTING GROUP DEFINED                   
         BNH   *+10                                                             
         OC    WPSTAT,=AL2(WOPSCOST)                                            
*                                                                               
WPTA26   MVC   WPCLI,BCCMPPRF+(PPRCOSTA-PPRELD)                                 
         L     R3,WPATRN                                                        
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         XR    RF,RF                                                            
         USING SPAELD,R3                                                        
WPTA28   CLI   SPAEL,0                                                          
         BE    WPTA30                                                           
         CLI   SPAEL,SPAELQ                                                     
         BNE   *+12                                                             
         CLI   SPATYPE,SPATCCST                                                 
         BE    *+12                                                             
         IC    RF,SPALN                                                         
         BXH   R3,RF,WPTA28                                                     
         MVC   WPCLI,SPAAACT                                                    
         DROP  R3                                                               
WPTA30   DS    0H                                                               
*                                                                               
*&&US                                                                           
*        TM    TLXSTAT,TLXSHOUR                                                 
*        BZ    WPTA40              TEST FOR TIME                                
*        MVC   WPHALF,=AL2(WOPSINCM)                                            
*        NC    WPHALF,WPSTAT       TEST FOR INCOME                              
*        BZ    WPTA40                                                           
         MVC   BCWORK,BCSPACES                                                  
         L     R3,WPATRN                                                        
         CLC   INCSUSUL,TRNKULC-TRNKEY(R3)                                      
         BNE   *+14                                                             
         MVC   BCWORK(L'TRNKULC),TRNKULC-TRNKEY(R3)                             
         B     WPTA34                                                           
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         USING SPDELD,R3           SEARCH FOR SPDEL IN TRANSACTION              
         XR    RF,RF                                                            
WPTA32   CLI   SPDEL,0                                                          
         BE    WPTA40                                                           
         IC    RF,SPDLN                                                         
         CLI   SPDEL,SPDELQ                                                     
         BE    *+8                                                              
         BXH   R3,RF,WPTA32                                                     
         CLC   INCSUSUL,SPDACCS    TEST FOR INCOME SUSPENSE LEDGER              
         BNE   WPTA40                                                           
         MVC   BCWORK,BCSPACES                                                  
         SH    RF,=Y(SPDLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   BCWORK(0),SPDACCS                                                
WPTA34   MVC   BCWORK(L'INCUL),INCUL  VALIDATE INCOME ACCOUNT                   
         OC    WPSTAT,=AL2(WOPSTMSK)                                            
         GOTO1 VALCOD,BCWORK                                                    
         BNE   ROUTL                                                            
*                                                                               
         MVC   WPSKANC(L'ACCOST),ACCOST                                         
         CLI   WPSKANC,C' '        TEST ANALYSIS IN ACCOST                      
         BH    WPTA38                                                           
         ICM   R1,15,ACASPA        LOOK FOR ANALYSIS IN SPAEL                   
         BZ    WPTA38                                                           
         USING SPAELD,R1                                                        
         XR    RF,RF                                                            
WPTA36   CLI   SPAEL,SPAELQ                                                     
         BNE   WPTA38                                                           
         CLI   SPATYPE,SPATANAL                                                 
         BE    *+12                                                             
         IC    RF,SPALN                                                         
         BXH   R1,RF,WPTA36                                                     
         MVC   WPSKANC,SPAAANAL                                                 
         DROP  R1                                                               
*                                                                               
WPTA38   CLC   WPSKANC,WPANC       TEST EQUAL ANALYSIS CODES                    
         BNE   *+10                                                             
         OC    WPSTAT,=AL2(WOPSEQAN)                                            
         CLI   WPSKANC,C' '                                                     
         BNH   *+10                                                             
         OC    WPSTAT,=AL2(WOPSKCST)                                            
         DROP  R3                                                               
*&&                                                                             
*                                                                               
WPTA40   GOTO1 VHELLO,BCPARM,(C'D',ACCMST),('SPAELQ',WPAPTA),0                  
         XC    BOELEM,BOELEM       CREATE NEW SPAELS FOR PTA RECORD             
         LA    R4,BOELEM                                                        
         USING SPAELD,R4                                                        
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         LA    R3,WOPTAB                                                        
         USING WOPTABD,R3          R3=A(WRITE-OFF POSTINGS TABLE)               
*                                                                               
WPTA42   CLI   WOPTABD,EOT                                                      
         BE    WPTA60                                                           
         GOTO1 TESTSC,WOPSYS       MATCH ON SYSTEM/COUNTRY                      
         BNE   WPTA58                                                           
*                                                                               
WPTA46   MVC   WPHALF,WOPSTATY     MATCH ON MUST BE TRUES                       
         NC    WPHALF,WPSTAT                                                    
         CLC   WPHALF,WOPSTATY                                                  
         BNE   WPTA58                                                           
         MVC   WPHALF,WOPSTATN     MATCH ON MUST BE FALSES                      
         NC    WPHALF,WPSTAT                                                    
         BNZ   WPTA58                                                           
*                                                                               
         MVC   SPATYPE,WOPTYPE                                                  
         MVC   SPAAULA,BCSPACES                                                 
         LA    R2,SPAAULA          R2=A(ACCOUNT CODE)                           
         TM    WOPINDS,WOPINOUL    TEST UNIT/LEDGER NOT REQUIRED                
         BO    WPTA47                                                           
         MVC   SPAAUNT(L'WOPUL),WOPUL                                           
         LA    R2,SPAAACT                                                       
WPTA47   LA    R1,WOPCODE          R1=A(ACCOUNT CODE DISPLACEMENTS)             
         XR    RF,RF                                                            
         LA    R0,WOPCODEN                                                      
WPTA48   ICM   RF,1,0(R1)          TEST ACCOUNT CODE BUILT                      
         BZ    WPTA50                                                           
         LA    RE,WPWORKD(RF)                                                   
         MVC   0(L'WPCODES,R2),0(RE)                                            
         LA    R2,SPAAACT+L'SPAAACT-1                                           
         CLI   0(R2),C' '          SET R2 TO END OF CURRENT CODE                
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
         LA    R1,L'WOPCODE(R1)                                                 
         BCT   R0,WPTA48                                                        
*                                                                               
WPTA50   TM    WOPINDS,WOPINOVA    TEST ACCOUNT VALIDATION REQUIRED             
         BO    WPTA52                                                           
         GOTO1 VALCOD,SPAAULA                                                   
         BE    WPTA52                                                           
         MVC   FVXTRA(L'SPAAACT),SPAAULA                                        
         L     R1,WPAR1                                                         
         MVC   0(1,R1),SPATYPE                                                  
         B     ROUTL                                                            
*                                                                               
WPTA52   GOTO1 VHELLO,BCPARM,(C'P',ACCMST),WPAPTA,SPAELD,ADDATEND               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WPTA58   LA    R3,WOPTABL(R3)                                                   
         B     WPTA42                                                           
         DROP  R3,R4                                                            
*                                                                               
WPTA60   B     ROUTE                                                            
         SPACE 1                                                                
         DROP  RC                                                               
         SPACE 1                                                                
WPWORKD  DSECT                     ** WOPPTA S/R LOCAL W/S **                   
WPPARMS  DS    0XL16                                                            
WPATRN   DS    A                                                                
WPAPTA   DS    A                                                                
WPADEPT  DS    A                                                                
WPASTAFF DS    A                                                                
*                                                                               
WPAR1    DS    A                                                                
WPHALF   DS    H 1                                                              
WPSTAT   DS    XL2                                                              
WPANLOFF DS    CL2                 ANALYSIS OFFICE                              
*                                                                               
WPCODES  DS    0CL12               ** ACCOUNT CODES **                          
*                                                                               
WP#9L2   EQU   *-WPWORKD           '99'                                         
WP9L2    DS    CL12                                                             
WP#9L3   EQU   *-WPWORKD           '999'                                        
WP9L3    DS    CL12                                                             
WP#9L4   EQU   *-WPWORKD           '9999'                                       
WP9L4    DS    CL12                                                             
WP#9L12  EQU   *-WPWORKD           '999999999999'                               
WP9L12   DS    CL12                                                             
WP#SEP   EQU   *-WPWORKD           '-'                                          
WPSEP    DS    CL12                                                             
WP#ANC   EQU   *-WPWORKD           ANALYSIS CODE                                
WPANC    DS    CL12                                                             
WP#DPT   EQU   *-WPWORKD           DEPARTMENT CODE                              
WPDPT    DS    CL12                                                             
WP#CLI   EQU   *-WPWORKD           COMPOSITE CLIENT COSTING ACCOUNT             
WPCLI    DS    CL12                                                             
WP#STF   EQU   *-WPWORKD           STAFF ACOUNT                                 
WPSTF    DS    CL12                                                             
WP#EXP   EQU   *-WPWORKD           EXPENSE ACCOUNT                              
WPEXP    DS    CL12                                                             
WP#OFF   EQU   *-WPWORKD           OFFICE CODE                                  
WPOFF    DS    CL12                                                             
WP#SKANC EQU   *-WPWORKD           SK ANALYSIS CODE                             
WPSKANC  DS    CL12                                                             
WP#DPT1P EQU   *-WPWORKD           DEPARTMENT CODE FOR 1P ACCOUNT               
WPDPT1P  DS    CL12                                                             
*                                                                               
WPCODESL EQU   *-WPCODES                                                        
*                                                                               
WPCATBLK DS    XL(CATLNQ)          ACCATCALLD                                   
*                                                                               
WPWORKL  EQU   *-WPWORKD                                                        
         SPACE 1                                                                
WOPTABD  DSECT                     ** WRITE-OFF POSTING TABLE **                
WOPTYPE  DS    XL1                 SPAEL ELEMENT TYPE (SEE SPATYPE)             
WOPSYS   DS    XL1                 SYSTEM FILTER                                
WOPCTRY  DS    XL1                 COUNTRY FILTER (SEE DDCTRYEQUS)              
WOPSTATY DS    XL2                 STATUS FILTER (MUST BE TRUE)                 
WOPSCOST EQU   X'8000'             COSTING GROUP DEFINED                        
WOPSDEPT EQU   X'4000'             DEPARTMENT REQUIRED                          
WOPSDEPD EQU   X'2000'             DEPARTMENT IS DEFINED                        
WOPSSTAF EQU   X'1000'             STAFF REQUIRED                               
WOPSNCST EQU   X'0800'             NEW COST SYSTEM IN USE                       
WOPSINCM EQU   X'0400'             ACCOUNT ON INCOME LEDGER                     
WOPSTMSK EQU   X'0200'             TIME TO SK IS WRITTEN OFF                    
WOPSEQAN EQU   X'0100'             EQUAL ANALYSIS CODES (WPANC=WPSKANC)         
WOPSKCST EQU   X'0080'             SK/SI IMAGE COSTING GROUP DEFINED            
WOPSNOU1 EQU   X'0040'             NO POSTINGS TO UNIT 1                        
WOPSTATN DS    XL2                 STATUS FILTER (MUST BE FALSE)                
WOPINDS  DS    XL1                 INDICATORS                                   
WOPINOVA EQU   X'80'               NO VALIDATION OF ACCOUNT REQUIRED            
WOPINOUL EQU   X'40'               UNIT/LEDGER NOT IN FRONT OF ACCOUNT          
         DS    XL2                 N/D                                          
WOPUL    DS    CL2                 UNIT/LEDGER CODE                             
WOPCODE  DS    5AL1                ACCOUNT CODES                                
WOPCODEN EQU   (*-WOPCODE)/L'WOPCODE                                            
WOPTABL  EQU   *-WOPTABD                                                        
CLB40    CSECT                                                                  
         SPACE 1                                                                
WOPTAB   DS    0X                                                               
*                                                                               
         DC    AL1(SPATW1PA,ALL,ALL)                                            
         DC    AL2(WOPSCOST,WOPSDEPT+WOPSNCST+WOPSINCM+WOPSNOU1)                
         DC    AL1(0,0,0)                                                       
         DC    C'1P',AL1(WP#9L2,WP#ANC,0,0,0)                                   
                                                                                
         DC    AL1(SPATW1PA,SYSUK,ALL)                                          
         DC    AL2(WOPSCOST+WOPSDEPT,WOPSNCST+WOPSINCM+WOPSNOU1)                
         DC    AL1(0,0,0)                                                       
         DC    C'1P',AL1(WP#DPT1P,WP#ANC,0,0,0)                                 
                                                                                
         DC    AL1(SPATW1PA,SYSUS,ALL)                                          
         DC    AL2(WOPSCOST+WOPSDEPT,WOPSNCST+WOPSINCM+WOPSNOU1)                
         DC    AL1(0,0,0)                                                       
         DC    C'1P',AL1(WP#OFF,WP#DPT,WP#ANC,0,0)                              
                                                                                
         DC    AL1(SPATW1PA,ALL,ALL)                                            
         DC    AL2(WOPSCOST+WOPSNCST,WOPSINCM+WOPSNOU1)                         
         DC    AL1(0,0,0)                                                       
         DC    C'1P',AL1(WP#9L12,0,0,0,0)                                       
*                                                                               
         DC    AL1(SPATW12A,ALL,ALL)                                            
         DC    AL2(WOPSCOST+WOPSINCM,WOPSNCST+WOPSEQAN+WOPSNOU1)                
         DC    AL1(0,0,0)                                                       
         DC    C'12',AL1(WP#ANC,0,0,0,0)                                        
*                                                                               
         DC    AL1(SPATW12K,ALL,ALL)                                            
         DC    AL2(WOPSTMSK+WOPSKCST,WOPSEQAN+WOPSNOU1)                         
         DC    AL1(0,0,0)                                                       
         DC    C'12',AL1(WP#SKANC,0,0,0,0)                                      
*                                                                               
         DC    AL1(SPATW1CA,ALL,ALL)                                            
         DC    AL2(WOPSCOST,WOPSEQAN+WOPSNOU1)                                  
         DC    AL1(0,0,0)                                                       
         DC    C'1C',AL1(WP#CLI,0,0,0,0)                                        
*                                                                               
         DC    AL1(SPATW1CA,ALL,ALL)                                            
         DC    AL2(WOPSKCST,WOPSEQAN+WOPSCOST+WOPSNOU1)                         
         DC    AL1(0,0,0)                                                       
         DC    C'1C',AL1(WP#CLI,0,0,0,0)                                        
*                                                                               
         DC    AL1(SPATW13A,ALL,ALL)                                            
         DC    AL2(WOPSCOST,WOPSINCM+WOPSNCST+WOPSNOU1)                         
         DC    AL1(0,0,0)                                                       
         DC    C'13',AL1(WP#ANC,0,0,0,0)                                        
                                                                                
         DC    AL1(SPATW13A,ALL,ALL)                                            
         DC    AL2(WOPSCOST+WOPSNCST,WOPSINCM+WOPSNOU1)                         
         DC    AL1(WOPINOVA,0,0)                                                
         DC    C'13',AL1(WP#ANC,0,0,0,0)                                        
*                                                                               
         DC    AL1(SPATW29A,SYSUK,ALL)                                          
         DC    AL2(WOPSSTAF,0)                                                  
         DC    AL1(0,0,0)                                                       
         DC    C'29',AL1(WP#CLI,0,0,0,0)                                        
                                                                                
         DC    AL1(SPATW29A,SYSUS,ALL)                                          
         DC    AL2(WOPSSTAF+WOPSCOST,0)                                         
         DC    AL1(0,0,0)                                                       
         DC    C'29',AL1(WP#CLI,0,0,0,0)                                        
                                                                                
         DC    AL1(SPATW29A,SYSUS,ALL)                                          
         DC    AL2(WOPSSTAF+WOPSNCST,WOPSCOST)                                  
         DC    AL1(0,0,0)                                                       
         DC    C'29',AL1(WP#9L12,0,0,0,0)                                       
                                                                                
         DC    AL1(SPATW29A,SYSUS,ALL)                                          
         DC    AL2(WOPSSTAF,WOPSNCST+WOPSCOST)                                  
         DC    AL1(0,0,0)                                                       
         DC    C'29',AL1(WP#OFF,WP#9L4,0,0,0)                                   
*                                                                               
         DC    AL1(SPATW2PA,SYSUK,ALL)                                          
         DC    AL2(WOPSSTAF,0)                                                  
         DC    AL1(0,0,0)                                                       
         DC    C'2P',AL1(WP#STF,0,0,0,0)                                        
                                                                                
         DC    AL1(SPATW2PA,SYSUS,ALL)                                          
         DC    AL2(WOPSSTAF,0)                                                  
         DC    AL1(0,0,0)                                                       
         DC    C'2P',AL1(WP#OFF,WP#DPT,WP#STF,0,0)                              
*                                                                               
         DC    AL1(SPATW2DA,ALL,ALL)                                            
         DC    AL2(WOPSDEPT,0)                                                  
         DC    AL1(0,0,0)                                                       
         DC    C'2D',AL1(WP#OFF,WP#DPT,0,0,0)                                   
                                                                                
         DC    AL1(SPATW2DA,ALL,ALL)                                            
         DC    AL2(WOPSDEPD,WOPSDEPT)                                           
         DC    AL1(0,0,0)                                                       
         DC    C'2D',AL1(WP#OFF,WP#DPT,0,0,0)                                   
*                                                                               
         DC    AL1(SPATW28A,ALL,ALL)                                            
         DC    AL2(WOPSDEPT,0)                                                  
         DC    AL1(0,0,0)                                                       
         DC    C'28',AL1(WP#EXP,0,0,0,0)                                        
*                                                                               
         DC    AL1(SPATW29C,SYSUK,ALL)                                          
         DC    AL2(WOPSSTAF,0)                                                  
         DC    AL1(WOPINOVA+WOPINOUL,0,0)                                       
         DC    C'29',AL1(WP#EXP,WP#SEP,WP#STF,0,0)                              
                                                                                
         DC    AL1(SPATW29C,SYSUS,ALL)                                          
         DC    AL2(WOPSSTAF,0)                                                  
         DC    AL1(WOPINOVA+WOPINOUL,0,0)                                       
         DC    C'29',AL1(WP#EXP,WP#SEP,WP#OFF,WP#DPT,WP#STF)                    
*                                                                               
         DC    AL1(SPATW2PC,SYSUK,ALL)                                          
         DC    AL2(WOPSSTAF,0)                                                  
         DC    AL1(WOPINOVA+WOPINOUL,0,0)                                       
         DC    C'2P',AL1(WP#EXP,WP#SEP,WP#CLI,0,0)                              
                                                                                
         DC    AL1(SPATW2PC,SYSUS,ALL)                                          
         DC    AL2(WOPSSTAF+WOPSCOST,0)                                         
         DC    AL1(WOPINOVA+WOPINOUL,0,0)                                       
         DC    C'2P',AL1(WP#EXP,WP#SEP,WP#CLI,0,0)                              
                                                                                
         DC    AL1(SPATW2PC,SYSUS,ALL)                                          
         DC    AL2(WOPSSTAF,WOPSCOST)                                           
         DC    AL1(WOPINOVA+WOPINOUL,0,0)                                       
         DC    C'2P',AL1(WP#EXP,WP#SEP,WP#9L3,0,0)                              
*                                                                               
WOPTABX  DC    AL1(EOT)                                                         
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET GROUPING BASIS DESCRIPTION                           *         
*                                                                     *         
* NTRY: P1=A(1 BYTE GROUPING BASIS NUMBER)                            *         
*       P2=(LENGTH OF OUTPUT, A(OUTPUT)) OR 0                         *         
* EXIT: CC=EQUAL IF GROUPING BASIS IS VALID                           *         
***********************************************************************         
         SPACE 1                                                                
GETGRB   DS    0H                                                               
         USING *,R8                                                             
         LM    R3,R4,0(R1)                                                      
         LTR   R4,R4                                                            
         BZ    GGRB01                                                           
         IC    RE,4(R1)            CLEAR OUTPUT                                 
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         XC    0(0,R4),0(R4)                                                    
*                                                                               
GGRB01   LA    R2,GRBTAB           LOCATE TABLE ENTRY                           
         USING GRBTABD,R2                                                       
GGRB02   CLI   GRBTABD,EOT                                                      
         BE    ROUTL                                                            
         CLC   GRBCODE,0(R3)                                                    
         BNE   GGRB08                                                           
         GOTO1 TESTSC,GRBSYS                                                    
         BE    GGRB10                                                           
GGRB08   LA    R2,GRBTABL(R2)                                                   
         B     GGRB02                                                           
*                                                                               
GGRB10   LTR   R4,R4                                                            
         BZ    ROUTE                                                            
         MVI   0(R4),AC#ESCL       TRANSLATE DICTIONARY                         
         MVC   1(2,R4),GRBDIC                                                   
         STCM  R4,8,3(R4)                                                       
         GOTO1 VDICTAT,BCPARM,C'SL  ',(R4)                                      
         B     ROUTE                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* GROUPING BASIS TABLE                                                *         
***********************************************************************         
         SPACE 1                                                                
GRBTABD  DSECT                                                                  
GRBCODE  DS    XL1                 GROUPING BASIS CODE                          
GRBSYS   DS    AL1                 SYSTEM CODE FILTER                           
GRBCTRY  DS    AL1                 COUNTRY CODE FILTER                          
         DS    XL3                 N/D                                          
GRBDIC   DS    AL2                 DICTIONARY REFERENCE NUMBER                  
GRBTABL  EQU   *-GRBTABD                                                        
         SPACE 1                                                                
CLB40    CSECT                                                                  
GRBTAB   DS    0X                                                               
*                                                                               
         DC    AL1(PBCKTRNQ,0,0,0,0,0),AL2(AC#GRB01)                            
         DC    AL1(PBCKWCQ,0,0,0,0,0),AL2(AC#GRB02)                             
         DC    AL1(PBCKWGQ,0,0,0,0,0),AL2(AC#GRB03)                             
         DC    AL1(PBCKHRSQ,0,0,0,0,0),AL2(AC#GRB04)                            
         DC    AL1(PBCKUSEQ,0,0,0,0,0),AL2(AC#GRB05)                            
*                                                                               
GRBTABX  DC    AL1(EOT)                                                         
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALIZE TSAR RECORD FOR TOTAL LINE                    *         
* NTRY: P1 BYTE 0 = C'I'                                              *         
*             1-3 = 0                                                 *         
*            TLST = TSAR RECORD FOR TOTAL LINE                        *         
*                                                                     *         
* ROUTINE TO UPDATE TOTAL LINES                                       *         
* NTRY: P1 BYTE 0 = C'U'                                              *         
*             1-3 = A(PRO-RATA BLOCK TO BE SUBTRACTED)                *         
*                OR 0 IF ONLY ADDING RECORD FOR CURRENT LIST          *         
*       P2 BYTE 0 = X'80' ON IF ONLY BUILDING CURRENT SCREEN          *         
*             1-3 = A(PRO-RATA BLOCK TO BE ADDED)                     *         
*            TLST = TSAR RECORD FOR TRANSACTION                       *         
*                                                                     *         
* ROUTINE TO DISPLAY TOTAL COLUMN                                     *         
* NTRY: P1 BYTE 0 = C'D'                                              *         
*             1-3 = 0                                                 *         
*        ACLMDATA = A(COLUMN TABE ENTRY)                              *         
*            TLST = TSAR RECORD FOR TOTAL LINE                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING STWORKD,RC                                                       
         USING PRORATAD,TLTPRATA                                                
SUBTOT   DS    0H                                                               
         USING *,R8                                                             
         MVC   STPARMS,0(R1)                                                    
         CLI   STPACT,STPAINI      TEST SET UP NEW W/C TOTAL LINE               
         BE    SUBINI                                                           
         CLI   STPACT,STPAUPD      TEST UPDATE TOTAL LINES                      
         BE    SUBUPD                                                           
         CLI   STPACT,STPADIS      DISPLAY TOTAL AMOUNT                         
         BE    SUBDIS                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* - INITIALIZE NEW SUB-TOTAL LINE                                     *         
***********************************************************************         
         SPACE 1                                                                
SUBINI   XC    TLDATA(TLTDSC-TLDATA),TLDATA                                     
         MVC   TLRLEN,=Y(TLTOTLNQ)                                              
         OI    TLINDS1,TLITOTL     INITIALIZE SUB-TOTAL LINE                    
         XC    PG$GEN(PA$VALS-PG$GEN),PG$GEN                                    
         LA    RF,PA$VALS                                                       
         LA    R0,PR$VALSQ                                                      
         ZAP   0(L'PA$VALS,RF),BCPZERO                                          
         LA    RF,L'PA$VALS(RF)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         MVC   TLTDSC,BCSPACES                                                  
         LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
         CLI   TLKSES,TLKSSTOT     TEST SCREEN TOTAL                            
         BNE   *+14                                                             
         MVC   TLTDSC+3(L'LC@TSCR),LC@TSCR                                      
         B     SUBINIX                                                          
         CLI   TLKSTYP1,TLKSLTOT   TEST LIST TOTAL                              
         BNE   *+14                                                             
         MVC   TLTDSC+3(L'LC@TLST),LC@TLST                                      
         B     SUBINIX                                                          
         CLI   TLKSTYP1,TLKSTTOT   TEST TIME TOTAL                              
         BNE   *+14                                                             
         MVC   TLTDSC+3(L'LC@TTIME),LC@TTIME                                    
         B     SUBINIX                                                          
         CLI   TLKSTYP1,TLKSOTOT   TEST OOPS TOTAL                              
         BNE   *+14                                                             
         MVC   TLTDSC+3(L'LC@TOOPS),LC@TOOPS                                    
         B     SUBINIX                                                          
         GOTO1 AGETWCD,STPARM,(X'80',TLKWC)                                     
         BL    SUBINIX                                                          
         L     RF,0(R1)            SET WORK-CODE DESCRIPTION                    
         USING WCDATA,RF                                                        
         USING WCOELD,WCEL                                                      
         MVC   TLTDSC(L'WCODESC),WCODESC                                        
         DROP  RF,R4                                                            
*                                                                               
SUBINIX  B     ROUTE                                                            
         SPACE 1                                                                
***********************************************************************         
* - UPDATE WORK-CODE TOTAL LINES                                      *         
***********************************************************************         
         SPACE 1                                                                
SUBUPD   MVC   STSAV1,TLST1        SAVE CURRENT TSAR RECORD                     
         MVC   STSAV2,TLST2                                                     
         MVC   STSAV3,TLST3                                                     
TX       USING TLSTD,STSAV         TX=TRANSACTION LIST RECORD                   
*                                                                               
         TM    STPINDS,STPIBSCR    TEST BUILDING CURRENT SCREEN                 
         BO    SUPD20                                                           
*                                                                               
         MVC   STSES,TX.TLKSES                                                  
         CLI   STSES,0                                                          
         BNE   *+10                                                             
         MVC   STSES,TWASESNL                                                   
SUPD02   MVC   TLKSES,STSES                                                     
         MVC   TLKSRT,TX.TLKSRT                                                 
         CLC   TLKSES,TX.TLKSES    TEST THIS IS THE RECORD GIVEN                
         BE    SUPD06                                                           
         XC    TLKSEQ,TLKSEQ       SEARCH LIST FOR RECORD                       
         LA    R1,TSARDH                                                        
         B     *+8                                                              
SUPD04   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO                                                          
         BL    SUPD08                                                           
         CLC   TLKSES,STSES        MATCH ON SESSION                             
         BNE   SUPD08                                                           
         CLC   TLKSRT,TX.TLKSRT    MATCH ON SORT KEY                            
         BNE   SUPD08                                                           
         CLC   TLDA,TX.TLDA        MATCH ON DISK ADDRESS                        
         BNE   SUPD04                                                           
*                                                                               
SUPD06   XC    TLKULC(TLDATA-TLKULC),TLKULC                                     
         MVI   TLKSTYP2,TLKSWTOT                                                
         BAS   RE,SUBADD           ADD WORK-CODE SUB-TOTALS                     
         BNE   SUPD08                                                           
*                                                                               
         XC    TLKWC(TLKSEQ-TLKWC),TLKWC                                        
*&&US                                                                           
         TM    LSSTAT1,LSSSEPTY    TEST SUB-TOTALLING TIME/OOPS                 
         BZ    *+12                                                             
         OI    TLKSTYP1,TLKSTTOT-TLKSTIME                                       
         BAS   RE,SUBADD           ADD TIME/OOPS SUB-TOTAL                      
*&&                                                                             
         MVI   TLKSTYP1,TLKSLTOT                                                
         BAS   RE,SUBADD           ADD LIST TOTALS                              
*                                                                               
SUPD08   OC    STPASUB,STPASUB     TEST BUILDING CURRENT LIST                   
         BZ    SUBUPDX                                                          
         XR    RE,RE                                                            
         IC    RE,STSES                                                         
         BCTR  RE,0                                                             
         STC   RE,STSES            BUMP DOWN A SESSION                          
         LTR   RE,RE                                                            
         BP    SUPD02                                                           
*                                                                               
         CLC   TX.TLNUM,CSPAG#LO   TEST RECORD ON CURRENT SCREEN                
         BL    *+14                                                             
         CLC   TX.TLNUM,CSPAG#HI                                                
         BNH   SUPD20                                                           
         OC    TX.TLNUM,TX.TLNUM   IF RECORD 'CONSTRUCTED' LOOK FOR IT          
         BNZ   SUBUPDX                                                          
*                                                                               
SRCH     USING TLSTD,STLST         SEARCH LIST FOR TRANSACTION                  
         MVC   SRCH.TLNUM,CSPAG#LO                                              
         LA    RF,TSAGET                                                        
         B     *+8                                                              
SUPD12   LA    RF,TSANXT                                                        
         GOTO1 ATSARIO,STPARM,((RF),SRCH.TLSTD)                                 
         CLC   SRCH.TLDA,TX.TLDA   MATCH ON DISK ADDRESS                        
         BE    SUPD20                                                           
         CLC   SRCH.TLNUM,CSPAG#HI                                              
         BL    SUPD12                                                           
         B     SUBUPDX                                                          
         DROP  SRCH                                                             
*                                                                               
SUPD20   XC    TLKEY,TLKEY         UPDATE SCREEN TOTAL                          
         MVI   TLKSES,TLKSSTOT                                                  
         BAS   RE,SUBADD                                                        
*                                                                               
SUBUPDX  MVC   TLST1,STSAV1        RESTORE CURRENT TSAR RECORD                  
         MVC   TLST2,STSAV2                                                     
         MVC   TLST3,STSAV3                                                     
         B     ROUTE                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO ADD IN TOTALS                                            *         
***********************************************************************         
         SPACE 1                                                                
SUBADD   NTR1  ,                                                                
         LA    R0,TSAPUT                                                        
         MVC   STKEY,TLKEY                                                      
         GOTO1 ATSARIO,TSARDH                                                   
         BE    SADD02                                                           
         MVC   TLKEY,STKEY                                                      
         CLC   TX.TLKSES,TLKSES   TEST CURRENT SESSION                          
         BNE   ROUTL                                                            
         CLI   LSWCTOT,C'Y'       TEST TOTALS REQUIRED                          
         BNE   ROUTL                                                            
         GOTO1 ASUBTOT,STPARM,(C'I',0)                                          
         LA    R0,TSAADD                                                        
*                                                                               
SADD02   LA    RE,PR$VALSQ         RE=NUMBER OF PRO-RATA AMOUNTS                
         LA    R3,PA$VALS          R3=A(1ST TOTAL PRORATA AMOUNT)               
         XR    R1,R1               R1=A(1ST ADD PRORATA MOUNT)                  
         ICM   R1,7,STPAADD                                                     
         LA    R1,PA$VALS-PRORATAD(R1)                                          
         XR    R2,R2               R2=A(1ST SUBTRACT AMOUNT)                    
         ICM   R2,7,STPASUB                                                     
         BZ    *+8                                                              
         LA    R2,PA$VALS-PRORATAD(R2)                                          
SADD04   AP    0(L'PA$VALS,R3),0(L'PA$VALS,R1)                                  
         LTR   R2,R2                                                            
         BZ    *+14                                                             
         SP    0(L'PA$VALS,R3),0(L'PA$VALS,R2)                                  
         LA    R2,L'PA$VALS(R2)                                                 
         LA    R3,L'PA$VALS(R3)                                                 
         LA    R1,L'PA$VALS(R1)                                                 
         BCT   RE,SADD04                                                        
*                                                                               
         GOTO1 ATSARIO,(R0)                                                     
         B     ROUTE                                                            
         DROP  TX                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TOTALS    ?? UNNECESSERY ??                                 *         
***********************************************************************         
         SPACE 1                                                                
SUBDIS   L     R2,ACLMDATA                                                      
         USING CLMTABD,R2                                                       
         XR    RF,RF                                                            
         ICM   RF,3,CLMAPROR                                                    
         LA    R0,PRORATAD(RF)                                                  
         ICM   RF,3,CLMFPROR                                                    
         BZ    *+8                                                              
         LA    RF,PRORATAD(RF)                                                  
         GOTO1 AEDTAMT,STPARM,(R0),(RF)                                         
SUBDISX  B     ROUTX                                                            
         SPACE 1                                                                
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* LOCAL W/S FOR SUBTOT ROUTINE                                        *         
***********************************************************************         
         SPACE 1                                                                
STWORKD  DSECT                                                                  
STPARMS  DS    0XL8                * INPUT PARAMETERS *                         
STPACT   DS    XL1                 ACTION                                       
STPAINI  EQU   C'I'                INITIALIZE NEW WORK-CODE TOTAL LINE          
STPAUPD  EQU   C'U'                UPDATE TOTAL LINES                           
STPADIS  EQU   C'D'                DISPLAY TOTAL COLUMN                         
STPAAMT  DS    0AL3                A(AMOUNT DO BE DISPLAYED)                    
STPASUB  DS    AL3                 A(PRORATA BLOCK TO BE SUBTRACTED)            
STPINDS  DS    XL1                 INDICATOR BYTE                               
STPIBSCR EQU   X'80'               ONLY UPDATE FOR BUILDING SCREEN              
STPAADD  DS    AL3                 A(PRORATA BLOCK TO BE ADDED)                 
         ORG   STPARMS+L'STPARMS                                                
STPARM   DS    6A                                                               
STDMCB   DS    6A                                                               
STSES    DS    XL1                 NTRSES LEVEL                                 
STKEY    DS    XL(L'TLKEY)         SAVED TSAR RECORD KEY                        
STSAV    DS    0XL(L'TLST)         SAVED TSAR LIST RECORD                       
STSAV1   DS    XL(L'TLST1)                                                      
STSAV2   DS    XL(L'TLST2)                                                      
STSAV3   DS    XL(L'TLST3)                                                      
STLST    DS    XL(L'TLST)          TSAR LIST RECORD                             
         DS    0D                                                               
STWORKL  EQU   *-STWORKD                                                        
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT PARAGRAPH FIELD HEADLINE                          *         
*                                                                     *         
* NTRY: P1 BYTE 0 = 1 OR 2 FOR HEADLINE 1 OR 2                        *         
*                   X'80' ON TO ALWAYS LEFT ALIGN HEADLINE            *         
*             1-3 = A(BLFELD ELEMENT)                                 *         
* EXIT:        CC = LOW IF NO HEADLINE,                               *         
*                 = HIGH IF HEADLINE DOES NOT FIT ON PRINT LINE       *         
*          BOELEM = HEADLINE                                          *         
*              P1 = COLUMN POSITION ON PRINT LINE (0-131)             *         
*              P2 = WIDTH OF HEADLINE                                 *         
*              P3 = A(FORMAT TABLE ENTRY FOR COLUMN)                  *         
***********************************************************************         
         SPACE 1                                                                
         USING FHWORKD,RC                                                       
FMTHED   DS    0H                                                               
         USING *,R8                                                             
         MVC   FHINDS,0(R1)                                                     
         NI    FHINDS,FHILEFT                                                   
         MVC   FHHED#,0(R1)                                                     
         NI    FHHED#,FF-FHILEFT                                                
         XR    R3,R3                                                            
         ICM   R3,7,1(R1)                                                       
         USING BLFELD,R3                                                        
         XC    0(8,R1),0(R1)                                                    
         ST    R1,FHAR1                                                         
*                                                                               
         L     R4,AFMTTAB          R4=A(FORMAT TABLE ENTRY)                     
         USING FMTTABD,R4                                                       
FHED02   CLC   FMTFLD,BLFFLD       MATCH ON FIELD CODE                          
         BNE   FHED08                                                           
         GOTO1 TESTSC,FMTSYS                                                    
         BE    FHED10                                                           
FHED08   LA    R4,FMTTABL(R4)                                                   
         CLI   FMTTABD,EOT                                                      
         BNE   FHED02                                                           
         DC    H'0'                                                             
FHED10   L     R1,FHAR1            GIVE CALLER A(FORMAT TABLE ENTRY)            
         ST    R4,8(R1)                                                         
*                                                                               
         MVI   BOELEM,C' '                                                      
         MVC   BOELEM+1(L'BOELEM-1),BOELEM                                      
         MVC   BOELEM(L'BLFHED1),BLFHED1                                        
         CLI   FHHED#,2                                                         
         BNE   *+10                                                             
         MVC   BOELEM(L'BLFHED2),BLFHED2                                        
         CLI   BOELEM,C'*'                                                      
         BE    ROUTL               CC=LOW IF NO HEADLINE                        
*                                                                               
         MVC   FHHEDF,BLFCOLF      SET START COLUMN                             
*                                                                               
         LA    R0,L'BLFHED1        SET HEADLINE WIDTH                           
         LA    RF,BOELEM+L'BLFHED1-1                                            
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,FHHEDN                                                        
*                                                                               
         IC    RE,BLFCOLN          SET EFFECTIVE FIELD WIDTH                    
         TM    FMTINDS1,FMTINUM                                                 
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         STC   RE,FHFLDN                                                        
*                                                                               
         TM    BLFOPT1,BLFOLFTQ    TEST LEFT ALIGNED                            
         BO    FHED20                                                           
         TM    FHINDS,FHILEFT                                                   
         BO    FHED20                                                           
*                                                                               
         TM    BLFOPT1,BLFORGTQ    TEST RIGHT ALIGNED                           
         BZ    FHED12                                                           
         IC    RE,FHHEDF           START=START                                  
         IC    RF,FHFLDN                + FIELD WIDTH                           
         AR    RE,RF                                                            
         IC    RF,FHHEDN                - HEADLINE WIDTH                        
         SR    RE,RF                                                            
         STC   RE,FHHEDF                                                        
         B     FHED20                                                           
*                                                                               
FHED12   TM    BLFOPT1,BLFOCTRQ+BLFOMAXQ TEST CENTRE/MAX                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XR    RF,RF                                                            
         IC    RF,FHFLDN                                                        
         XR    RE,RE                                                            
         IC    RE,FHHEDN                                                        
         SR    RF,RE               RF=FIELD WIDTH - HEADLINE WIDTH              
         BNM   *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                /2                                           
*                                                                               
         TM    BLFOPT1,BLFOMAXQ    TEST NOT MAXIMIING                           
         BZ    FHED14                                                           
         CLI   FHHED#,2                                                         
         BE    *+12                                                             
         CLI   BLFHED2,C'*'        MAXIMIZE HEADLINE 1 IF NO HEADLINE 2         
         BNE   FHED14                                                           
         CLC   FHFLDN,FHHEDN       TEST FIELD WIDER THAN HEADLINE               
         BH    FHED16                                                           
FHED14   IC    RE,FHHEDF           UPDATE START OF HEADLINE                     
         AR    RE,RF                                                            
         STC   RE,FHHEDF                                                        
         B     FHED20                                                           
*                                                                               
FHED16   MVC   FHHEDSAV,BOELEM     SAVE HEADLINE                                
         MVI   BOELEM,C'-'         DASHERIZE HEADLINE                           
         IC    RE,FHFLDN                                                        
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BOELEM+1(0),BOELEM                                               
         LA    RF,BOELEM(RF)       PLONK HEADLINE IN MIDDLE                     
         IC    RE,FHHEDN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FHHEDSAV                                                 
         MVC   FHHEDN,FHFLDN       UPDATE HEADLINE WIDTH                        
*                                                                               
FHED20   CLI   FHHEDF,0                                                         
         BE    FMTHEDN                                                          
         CLI   FHHEDF,L'REPP1                                                   
         BH    FMTHEDN                                                          
         XR    RE,RE                                                            
         IC    RE,FHHEDF                                                        
         XR    RF,RF                                                            
         IC    RF,FHHEDN                                                        
         AR    RE,RF                                                            
         LA    RF,L'REPP1+1                                                     
         CR    RE,RF                                                            
         BH    FMTHEDN                                                          
         L     R1,FHAR1                                                         
         IC    RE,FHHEDF                                                        
         BCTR  RE,0                                                             
         STC   RE,3(R1)            P1=POSITION ON LINE (0-131)                  
         MVC   7(1,R1),FHHEDN      P2=LENGTH OF HEADLINE IN BOELEM              
         B     ROUTE                                                            
*                                                                               
FMTHEDN  MVC   FVMSGNO,=AL2(AE$HNFPL)                                           
         MVC   FVXTRA(1),FHHED#                                                 
         OI    FVXTRA,C'0'                                                      
         B     ROUTH                                                            
         DROP  R3,R4,RC                                                         
         SPACE 1                                                                
FHWORKD  DSECT                     ** FMTHED LOCAL W/S **                       
FHAR1    DS    A                   A(CALLERS R1)                                
FHINDS   DS    XL1                 INDICATORS                                   
FHILEFT  EQU   X'80'               LEFT-ALIGN HEADLINE                          
FHHED#   DS    XL1                 HEADLINE NUMBER                              
FHHEDF   DS    XL1                 HEADLINE POSITION                            
FHHEDN   DS    XL1                 HEADLINE WIDTH                               
FHFLDN   DS    XL1                 FIELD WIDTH UNDER HEADLINE                   
FHHEDSAV DS    CL(L'BLFHED1)       SAVE AREA FOR HEADLINE                       
FHWORKL  EQU   *-FHWORKD                                                        
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD AND ADD TRANSACTION RECORDS AND PRINT WKLIST       *         
*                                                                     *         
* NTRY         P1 = A(TRANSACTION POSTING VALUES BLOCK)               *         
* EXIT         NO R/C ON EXIT - ALL ERRORS ABEND                      *         
***********************************************************************         
         SPACE 1                                                                
BLDTRN   DS    0H                                                               
         USING *,R8                                                             
         L     RC,0(R1)                                                         
         USING POSTVALS,RC         RC=A(POSTING VALUES BLOCK)                   
         TM    POSTSTA2,POSTNO     TEST POSTINGS NOT ACTUALLY WANTED            
         BO    ROUTE                                                            
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BE    BLDCLOSE            FINAL CALL                                   
         OC    POSTDR,POSTDR       TEST FIRST TIME                              
         BNZ   BLDTRN02                                                         
         ZAP   POSTDR,BCPZERO      ZEROIZE TOTAL DEBITS/CREDITS                 
         ZAP   POSTCR,BCPZERO                                                   
         L     R2,AREP             SET UP HEAD-LINES                            
         USING REPD,R2                                                          
         MVC   REPM1,JRNM1                                                      
         GOTO1 VDICTAT,BCPARM,C'TL  ',(L'REPM1,REPM1)                           
         MVC   REPM2,JRNM2                                                      
         GOTO1 (RF),(R1),C'TL  ',(L'REPM2,REPM2)                                
         DROP  R2                                                               
*                                                                               
BLDTRN02 L     R2,AIO3                                                          
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD)                     
         XC    TRNRECD(256),TRNRECD                                             
         MVC   TRNKEY,BCSPACES     BUILD TRANSACTION RECORD KEY                 
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKULA,POSTACT                                                  
         CLC   TRNKULA(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         BNE   *+10                                                             
         MVC   TRNKWORK,POSTOFFC                                                
         MVC   TRNKCULC,POSTCAC                                                 
         MVC   TRNKDATE,POSTDATE                                                
         MVC   TRNKREF,POSTREF                                                  
         MVI   TRNKSBR,0                                                        
         CLI   TRNKACT,C' '        TEST ACCOUNT/CONTRA EXISTS                   
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   TRNKCACT,C' '                                                    
*        BH    *+6                 TYPE 45 MAY HAVE CUL ONLY                    
*        DC    H'0'                                                             
*                                                                               
         USING TRNELD,TRNRFST                                                   
         MVI   TRNEL,TRNELQ                                                     
         MVC   TRNDATE,POSTDATE                                                 
         MVC   TRNREF,POSTREF                                                   
         MVC   TRNTYPE,POSTTYPE                                                 
         MVC   TRNSTAT,POSTSTAT                                                 
         ZAP   TRNAMNT,POSTAMNT                                                 
         MVC   TRNOFFC,POSTOFFC                                                 
*                                                                               
         MVC   TRNMOS,POSTBTMC                                                  
         MVC   TRNBREF,POSTBTRF                                                 
         XR    RE,RE                                                            
         ICM   RE,1,POSTNARL       TEST LENGTH OF NARRATIVE SET                 
         BNZ   BLDTRN06                                                         
         LA    RE,L'POSTNARR       CALCULATE LENGTH OF NARRATIVE                
         LA    RF,POSTNARR+L'POSTNARR-1                                         
BLDTRN04 CLI   0(RF),C' '                                                       
         BH    BLDTRN06                                                         
         BCTR  RF,0                                                             
         BCT   RE,BLDTRN04                                                      
*                                                                               
BLDTRN06 BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNNARR(0),POSTNARR                                              
         LA    RE,TRNLN1Q+1(RE)                                                 
         STC   RE,TRNLN            SET ELEMENT LENGTH                           
         LA    R3,TRNELD(RE)                                                    
*                                                                               
         ICM   RF,15,POSTXTRA      TEST ANY EXTRA ELEMENTS REQUIRED             
         BZ    BLDTRN14                                                         
         SR    RE,RE                                                            
BLDTRN12 CLI   0(RF),0             TEST END OF ELEMENT LIST                     
         BE    BLDTRN14                                                         
         IC    RE,1(RF)                                                         
         EX    RE,*+4              MOVE ELEMENT TO TRANSACTION RECORD           
         MVC   0(0,R3),0(RF)                                                    
         MVI   0(RF),0             USER MUST RESET NEXT TIME THROUGH            
         AR    RF,RE               BUMP TO NEXT INPUT ELEMENT                   
         AR    R3,RE               BUMP TO NEXT OUTPUT ELEMENT                  
         B     BLDTRN12                                                         
*                                                                               
BLDTRN14 ICM   R1,15,POSTPTRS      TEST ANALYSIS POINTERS PASSED                
         BZ    BLDTRN18                                                         
         CLI   0(R1),X'FF'         TEST NOTHING IN LIST                         
         BE    BLDTRN18                                                         
         USING APEELD,R3           R3=A(ANALYSIS POINTER ELEMENT)               
         XC    APEELD(256),APEELD                                               
         MVI   APEEL,APEELQ        INITIALISE ANALYSIS POINTER ELEMENT          
         MVI   APELN,APELN1Q                                                    
         MVI   APENUM,0                                                         
         LA    R4,APENTRY                                                       
         USING APENTRY,R4          R4=A(ANALYSIS POINTER SUB-ELEMENT)           
BLDTRN16 CLI   0(R1),X'FF'         TEST END OF POINTER LIST                     
         BNE   *+10                                                             
         LR    R3,R4               YES - POINT TO END OF RECORD                 
         B     BLDTRN18                                                         
         L     RE,0(R1)            RE=A(ANALYSIS UNIT/LEDGER/ACCOUNT)           
         LA    RE,0(RE)                                                         
         LA    RF,L'ACTKCULA-2(RE)                                              
         LA    R0,L'ACTKCULA-2                                                  
         CLI   0(RF),C' '          LOCATE END OF ACCOUNT CODE                   
         BNE   *+12                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         DC    H'0'                BAD ANALYSIS POINTER PASSED                  
         SR    RF,RE               RF=L'ACCOUNT-1                               
         EX    RF,*+4                                                           
         MVC   APENACT(0),0(RE)    MOVE ANALYSIS POINTER TO ELEMENT             
         LA    RF,APELN2Q+1(RF)                                                 
         STC   RF,APENLEN          SET LENGTH OF SUB-ELEMENT                    
         MVC   APENSTAT,0(R1)      SET DR/CR STATUS                             
         SR    RE,RE                                                            
         IC    RE,APELN            UPDATE TOTAL ELEMENT LENGTH                  
         AR    RE,RF                                                            
         STC   RE,APELN                                                         
         IC    RE,APENUM           INCREMENT NUMBER OF SUB-ELEMENTS             
         LA    RE,1(RE)                                                         
         STC   RE,APENUM                                                        
         AR    R4,RF               R4=A(NEXT ANALYSIS SUB-ELEMENT)              
         MVI   APENSTAT,0                                                       
         LA    R1,4(R1)            BUMP TO NEXT ANALYSIS POINTER                
         B     BLDTRN16                                                         
         DROP  R3,R4                                                            
*                                                                               
         USING DUEELD,R3                                                        
BLDTRN18 CLI   TRNTYPE,POSTBILL                                                 
         BNE   BLDTRN20                                                         
         XC    DUEELD(DUELNQ),DUEELD                                            
         MVI   DUEEL,DUEELQ        ADD DUE DATE ELEMENT                         
         MVI   DUELN,DUELNQ                                                     
         MVC   DUEDATE,POSTDUED    SET ORIGINAL/NEW DUE DATE                    
         LA    R3,DUEELD+DUELNQ    R3=A(NEW EOR)                                
         MVI   0(R3),0                                                          
         DROP  R3                                                               
*                                                                               
         USING TIDELD,R3                                                        
BLDTRN20 XC    TIDEL(TIDLNQ),TIDEL ADD TERMINAL ID ELEMENT FOR BG               
         MVI   TIDEL,TIDELQ                                                     
         MVI   TIDLN,TIDLNQ                                                     
         MVC   TID,CUTSYM                                                       
         LA    R3,TIDLNQ(R3)                                                    
         MVI   0(R3),0                                                          
*                                                                               
         USING PIDELD,R3                                                        
         XC    PIDEL(PIDLNQ),PIDEL ADD PERSON ID ELEMENT FOR BG                 
         MVI   PIDEL,PIDELQ                                                     
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,CUPASS                                                     
         LA    R3,PIDLNQ(R3)                                                    
         MVI   0(R3),0                                                          
         DROP  R3                                                               
*                                                                               
         LA    R0,TRNRECD          SET TRANSACTION RECORD LENGTH                
         LA    R3,1(R3)                                                         
         MVI   0(R3),0                                                          
         SR    R3,R0                                                            
         STCM  R3,3,TRNRLEN                                                     
*                                                                               
         TM    POSTSTA2,POSTSEC    TEST POSTAMNT IN SECONDARY CURRENCY          
         BZ    BLDTRN30                                                         
         MVC   BCWORK(L'TOBCURO),TOBCURO                                        
         MVC   BCWORK+L'TOBCURO(L'TOBCURI),TOBCURI                              
         GOTO1 VTOBACCO,BCPARM,('TOBAACNV',BCWORK),TRNRECD,ACOM,0,0             
         CLI   4(R1),0             AGENCY VALUES NOW IN OCAEL                   
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,TRNRFST                                                       
         USING OCAELD,R3                                                        
         XR    RF,RF                                                            
BLDTRN22 CLI   OCAEL,OCAELQ                                                     
         BE    BLDTRN24                                                         
         CLI   OCAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    RF,OCALN                                                         
         BXH   R3,RF,BLDTRN22                                                   
BLDTRN24 OI    OCAINDS,OCAIDSEC    SWAP VALUES ROUND                            
         GOTO1 VTOBACCO,BCPARM,('TOBAACVB',TOBCUR),TRNRECD,ACOM,0,0             
         DROP  R3                                                               
*                                                                               
BLDTRN30 DS    0H                                                               
         CLI   POSTMODE,POSTLVQ    TEST LIVE REQUEST                            
         BNE   BLDTRN32            NO - DO NOT ADD TRANSACTIONS                 
         L     R3,AADTBLK                                                       
         USING ADDTRND,R3          R3=A(ADDTRN BLOCK)                           
         STCM  R2,15,TRNREC        SET A(TRANSACTION RECORD)                    
         ICM   RE,3,TRNBSEQN       INCREMENT TRANSACTION SEQUENCE#              
         LA    RE,1(RE)                                                         
         STCM  RE,3,TRNBSEQN                                                    
         MVC   TRNCACNM,POSTCACN   SET CONTRA-ACCOUNT NAME                      
         MVI   TRNINDS,TRNICONV                                                 
         MVC   TRNINDS1,POSTIND1                                                
         MVC   TRNINDS2,POSTIND2                                                
         GOTO1 VDATCON,BCPARM,(1,POSTDATE),(2,TRNEFDT)                          
         GOTO1 VADDTRN,ADDTRND     ADD TRANSACTION                              
         BE    BLDTRN32                                                         
         CLI   TRNERRS,TRNETRNI    TEST INVALID TRANSACTION                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         MVC   FVXTRA(L'ACTKULA),TRNKULA                                        
         OI    CSINDSG1,CSINDUNW   SET UNWIND VIA $ABEND REQUIRED               
         B     ROUTH               RETURN CC HIGH                               
         DROP  R3                                                               
*                                                                               
BLDTRN32 TM    POSTIND2,TRNIUBKO   TEST DUMMY POSTING FOR BUCKETS               
         BO    BLDTRNX             THEN DO NOT PRINT                            
*                                                                               
         TM    TRNSTAT,TRNSDR      UPDATE DEBIT/CREDIT TOTAL                    
         BZ    *+14                                                             
         AP    POSTDR,TRNAMNT                                                   
         B     *+10                                                             
         AP    POSTCR,TRNAMNT                                                   
*                                                                               
         L     R2,AREP                                                          
         LA    R2,REPP1-REPD(R2)                                                
         USING JRNLINED,R2         R2=A(PRINT LINE 1)                           
         L     R3,AIO3                                                          
         USING TRNRECD,R3          R3=A(TRANSACTION RECORD)                     
*&&US                                                                           
         CLC   TRNKUNT,BCCPYEL+(CPYPROD-CPYELD)                                 
         BE    *+8                 YES - PRINT AS NORMAL                        
         LA    R2,1(R2)            NO - INDENT ACCOUNT AND CONTRA               
*&&                                                                             
         MVC   JRNACT,TRNKUNT                                                   
         MVC   JRNCAC,TRNKCUNT                                                  
         LA    R0,3                                                             
         CLI   JRNCAC,C' '                                                      
         BNE   *+14                                                             
         MVC   JRNCAC,JRNCAC+1                                                  
         BCT   R0,*-14                                                          
*&&US                                                                           
         CLC   TRNKUNT,BCCPYEL+(CPYPROD-CPYELD)                                 
         BE    *+6                                                              
         BCTR  R2,0                NO - RE-POSITION R2                          
*&&                                                                             
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3           R3=A(TRANSACTION ELEMENT)                    
         MVC   JRNOFFC,TRNOFFC                                                  
         GOTO1 VDATCON,BODMCB,(1,TRNDATE),(17,JRNDATE)                          
         MVC   JRNREF,TRNREF                                                    
         GOTO1 VCONVMOS,BODMCB,(X'FD',TRNELD),BOWORK1                           
         MVI   BOWORK1+2,X'01'                                                  
         GOTO1 VDATCON,BODMCB,(1,BOWORK1),(9,JRNMOA)                            
*                                                                               
         ZAP   BCDUB,TRNAMNT       FORMAT AMOUNT IN DEBIT/CREDIT COLUMN         
         LA    RF,JRNCR+L'JRNCR-1                                               
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+8                                                              
         LA    RF,JRNDR+L'JRNDR-1                                               
         GOTO1 BLDAMT,BCPARM,BCDUB,(RF)                                         
         L     RF,AIO3                                                          
         TM    POSTSTA2,POSTALCQ   TEST IF ITEM WAS ALLOCATED                   
         BNO   BLDTRN34                                                         
         LH    RF,=AL2(LC@ALCTD-TWAD)                                           
         AR    RF,RA                                                            
         MVC   JRNSTT(L'LC@ALCTD),0(RF)                                         
         B     BLDTRN36                                                         
BLDTRN34 TM    POSTSTA2,POSTNALQ   TEST ITEM SHOULD HAVE BEEN ALLOC             
         BNO   BLDTRN36                                                         
         LH    RF,=AL2(LC@NALLC-TWAD)                                           
         AR    RF,RA                                                            
         MVC   JRNSTT(L'LC@NALLC),0(RF)                                         
         B     BLDTRN36                                                         
*                                                                               
BLDTRN36 L     R1,AREP                                                          
         GOTO1 VREPORT             PRINT THE LINE                               
         B     BLDTRNX                                                          
         DROP  R2                                                               
*                                                                               
BLDCLOSE CLI   POSTMODE,POSTLVQ    TEST LIVE REQUEST                            
         BNE   BLDCLOS2            NO - DO NOT CALL ADDTRN                      
         L     RF,AADTBLK                                                       
         USING ADDTRND,RF          RF=A(ADDTRN BLOCK)                           
         OI    TRNINDS,TRNILAST                                                 
         GOTO1 VADDTRN,ADDTRND     ADD TRANSACTION                              
         BE    *+6                                                              
         DC    H'0'                                                             
BLDCLOS2 L     R4,AREP                                                          
         PUSH  USING                                                            
         USING REPD,R4                                                          
         USING JRNLINED,REPP1                                                   
         MVI   REPP1,C'-'                                                       
         MVC   REPP1+1(JRNLINEL-1),REPP1                                        
         MVCDD REPP2(JRNOFFC-JRNLINED),AC#TREQ                                  
         GOTO1 VDICTAT,BCPARM,C'SL  ',REPP2                                     
         GOTO1 BLDAMT,BCPARM,POSTDR,JRNDR+L'JRNDR-1+L'REPP1                     
         GOTO1 (RF),(R1),POSTCR,JRNCR+L'JRNCR-1+L'REPP1                         
         LA    RF,REPP3                                                         
         CLC   0(L'REPP1,RF),BCSPACES                                           
         BE    *+12                                                             
         LA    RF,L'REPP1(RF)                                                   
         B     *-14                                                             
         MVC   0(L'REPP1,RF),REPP1                                              
         MVI   REPACTN,REPAPUT                                                  
         GOTO1 VREPORT,REPD                                                     
         MVC   REPM1,BCSPACES                                                   
         MVC   REPM2,BCSPACES                                                   
         POP   USING                                                            
*                                                                               
BLDTRNX  B     ROUTE                                                            
         DROP  R3,RC,RF                                                         
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO FORMAT AN AMOUNT                                         *         
*                                                                     *         
* NTRY: P1 = A(PL8 AMOUNT)                                            *         
*       P2 = A(END OF OUTPUT AREA ON PRINT LINE)                      *         
* EXIT: AMOUNT IS FORMATTED ON TO CURRENT PRINT LINE OR NEXT ONE DOWN *         
***********************************************************************         
         SPACE 1                                                                
BLDAMT   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         CURED (P8,(R2)),(30,BCWORK),CSCURCPY,COMMAS=YES,MINUS=YES,    *        
               ALIGN=LEFT,DMCB=BODMCB                                           
         LR    RF,R0               RF=L'OUTPUT                                  
         LA    R1,BCWORK-1(RF)                                                  
         TM    0(R1),C'0'          TEST LAST FIGURE IS NUMERICAL                
         BNO   *+12                                                             
         MVI   1(R1),C' '          YES - OUTPUT SPACE (INSTEAD OF '-')          
         LA    RF,1(RF)                                                         
*                                                                               
         SR    R3,RF               R3=A(START OF OUTPUT-1)                      
BLDAMT02 EX    RF,*+8              TEST NUMBER FITS ON LINE                     
         BE    BLDAMT04                                                         
         CLC   0(0,R3),BCSPACES                                                 
         LA    R3,L'REPP1(R3)      NO - BUMP TO NEXT LINE                       
         B     BLDAMT02                                                         
*                                                                               
BLDAMT04 BCTR  RF,0                COPY TO OUTPUT                               
         EX    RF,*+4                                                           
         MVC   1(0,R3),BCWORK                                                   
*                                                                               
BLDAMTX  B     ROUTX                                                            
*                                                                               
JRNM1    DS    CL(L'REPM1)' '                                                   
JRNM2    DS    CL(L'REPM2)' '                                                   
         ORG   JRNM1+(JRNACT-JRNLINED)                                          
         DCDDL AC#ACC,L'JRNACT                                                  
         ORG   JRNM2+(JRNACT-JRNLINED)                                          
         DCDDL AC#ACC,L'JRNACT,LU                                               
         ORG   JRNM1+(JRNCAC-JRNLINED)                                          
         DCDDL AC#CTRA,L'JRNCAC                                                 
         ORG   JRNM2+(JRNCAC-JRNLINED)                                          
         DCDDL AC#CTRA,L'JRNCAC,LU                                              
         ORG   JRNM1+(JRNOFFC-JRNLINED)                                         
         DCDDL AC#OFF,L'JRNOFFC+1                                               
         ORG   JRNM2+(JRNOFFC-JRNLINED)                                         
         DCDDL AC#OFF,L'JRNOFFC+1,LU                                            
         ORG   JRNM1+(JRNDATE-JRNLINED)                                         
         DCDDL AC#DATE,L'JRNDATE                                                
         ORG   JRNM2+(JRNDATE-JRNLINED)                                         
         DCDDL AC#DATE,L'JRNDATE,LU                                             
         ORG   JRNM1+(JRNREF-JRNLINED)                                          
         DCDDL AC#REFN,L'JRNREF                                                 
         ORG   JRNM2+(JRNREF-JRNLINED)                                          
         DCDDL AC#REFN,L'JRNREF,LU                                              
         ORG   JRNM1+(JRNMOA-JRNLINED)                                          
         DCDDL AC#MOA,L'JRNMOA                                                  
         ORG   JRNM2+(JRNMOA-JRNLINED)                                          
         DCDDL AC#MOA,L'JRNMOA,LU                                               
         ORG   JRNM1+(JRNDR-JRNLINED)                                           
         DCDDL AC#DRS,L'JRNDR-1,R                                               
         ORG   JRNM2+(JRNDR-JRNLINED)                                           
         DCDDL AC#DRS,L'JRNDR-1,RU                                              
         ORG   JRNM1+(JRNCR-JRNLINED)                                           
         DCDDL AC#CRS,L'JRNDR-1,R                                               
         ORG   JRNM2+(JRNCR-JRNLINED)                                           
         DCDDL AC#CRS,L'JRNDR-1,RU                                              
         ORG   JRNM1+(JRNSTT-JRNLINED)                                          
         DCDDL AC#STT,L'JRNSTT,L                                                
         ORG   JRNM2+(JRNSTT-JRNLINED)                                          
         DCDDL AC#STT,L'JRNSTT,LU                                               
         ORG   JRNM2+L'JRNM2                                                    
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A COMPOSITE CLIENT/PRODUCT/JOB PROFILE ELEMENT     *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
CMPPRF   DS    0H                                                               
         USING *,R8                                                             
         MVC   BCCMPPRF,BCCLIPRF                                                
         LA    R0,2                ONCE FOR PROD AND ONCE FOR JOB               
         LA    RE,BCPROPRF         PRODUCT LEVEL                                
         USING PPRELD,RE                                                        
CMPPRF2  OC    PPRELD(L'BCCMPPRF),PPRELD                                        
         BZ    CMPPRF6                                                          
CMPPRF4  OC    PPRGRUP,PPRGRUP                                                  
         BZ    *+10                                                             
         MVC   BCCMPPRF+(PPRGRUP-PPRELD)(L'PPRGRUP),PPRGRUP                     
         OC    PPRRECV,PPRRECV                                                  
         BZ    *+10                                                             
         MVC   BCCMPPRF+(PPRRECV-PPRELD)(L'PPRRECV),PPRRECV                     
         OC    PPRCOST,PPRCOST                                                  
         BZ    *+10                                                             
         MVC   BCCMPPRF+(PPRCOST-PPRELD)(L'PPRCOST),PPRCOST                     
         OC    PPRBTYPE,PPRBTYPE                                                
         BZ    *+10                                                             
         MVC   BCCMPPRF+(PPRBTYPE-PPRELD)(L'PPRBTYPE),PPRBTYPE                  
         OC    PPRUFORA,PPRUFORA                                                
         BZ    *+10                                                             
         MVC   BCCMPPRF+(PPRUFORA-PPRELD)(L'PPRUFORA),PPRUFORA                  
         CLC   PPRGAOFF,BCSPACES                                                
         BNH   *+10                                                             
         MVC   BCCMPPRF+(PPRGAOFF-PPRELD)(L'PPRGAOFF),PPRGAOFF                  
CMPPRF6  LA    RE,BCJOBPRF         JOB LEVEL                                    
         BCT   R0,CMPPRF2                                                       
CMPPRFX  B     ROUTE                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OLD-STYLE BATCH HEADERS                              *         
*                                                                     *         
* NTRY: P1 BYTE 0 = BATCH TYPE                                        *         
*             1-3 = A(REF/MOA/MOACHARS)                               *         
*       P2        = 0 TO ADD HEADERS, NOT VALIDATE                    *         
*          BYTE 0 = X'80' ON IF VALID TO INCREMENT REFERENCE NUMBER   *         
*             1-3 = A(INPUT FIELD) - FOR CURSOR                       *         
***********************************************************************         
         SPACE 1                                                                
         USING BHWORKD,RC                                                       
ADDOBH   DS    0H                                                               
         USING *,R8                                                             
         MVC   BHPARMS,0(R1)       SAVE PARAMETERS                              
ADDO10   LA    R2,IOKEY                                                         
         USING BATRECD,R2                                                       
         MVC   BATKEY,BCSPACES                                                  
         XC    BATKEY(BATKEND),BATKEY                                           
         MVI   BATKTYP,BATKTYPQ                                                 
         MVC   BATKCPY,CUABIN                                                   
         MVC   BATKOFF,TWAUSRID                                                 
*&&UK*&& MVI   BATKGRUP,TBAGGENQ                                                
*&&US*&& MVI   BATKGRUP,TBAGPRDQ                                                
         MVC   BATKTYPE,BHPTYPE                                                 
         MVC   BATKDATE,BCTODAYP                                                
         ICM   RE,15,BHPABREF                                                   
         USING BHBATCHD,RE                                                      
         MVC   BATKREF(L'BHBMC),BHBMC                                           
         MVC   BATKREF+L'BHBMC(L'BHBREF),BHBREF                                 
         OC    BATKREF+L'BHBMC(L'BHBREF),BCSPACES                               
         DROP  RE                                                               
*                                                                               
         OC    BHPAFLDH,BHPAFLDH   TEST ADDING BATCH HEADERS                    
         BZ    ADDO20                                                           
*                                                                               
         GOTO1 AIO,IOACCDIR+IOREAD+IO1                                          
         BNE   ADDOOK              NEQ IS FINE                                  
         TM    BHPINDS,BHPIINC     TEST IF REF CAN BE INCREMENTED               
         BZ    ADDONOK             NO                                           
         OC    BATKREF+3(3),BATKREF+3                                           
         BZ    ADD015                                                           
         CLC   BATKREF+3(3),BCSPACES                                            
         BE    ADD015                                                           
         TM    BATKREF+3,C'0'      ENUSURE IS NUMERICAL                         
         BNO   ADDONOK                                                          
         TM    BATKREF+4,C'0'                                                   
         BNO   ADDONOK                                                          
         TM    BATKREF+5,C'0'                                                   
         BNO   ADDONOK                                                          
ADD015   PACK  BCFULL,BATKREF+3(3) INCREMENT NUMERIC PORTION                    
         AP    BCFULL,=P'1'                                                     
         OI    BCFULL+(L'BCFULL-1),X'0F'                                        
         ICM   RE,15,BHPABREF      SET NEW REF IN CALLERS AREA                  
         UNPK  1(L'BHBREF-1,RE),BCFULL                                          
         B     ADDO10              TRY AGAIN                                    
*                                                                               
ADDO20   L     R2,AIO1             BUILD ACCMST RECORD IN IO1                   
         MVC   BATKEY,IOKEY                                                     
         XC    BATRSTA,BATRSTA                                                  
         MVI   BATRSTAT,BATSUPD+BATSRECV                                        
         LA    R4,BATRFST                                                       
         USING BTHELD,R4                                                        
         MVI   BTHEL,BTHELQ                                                     
         MVI   BTHLN,BTHLNQ                                                     
         L     RE,BCAUTL                                                        
         MVC   BTHNAME,BCSPACES                                                 
         MVC   BTHNAME(L'TSYM),TSYM-UTLD(RE)                                    
         ZAP   BTHCASH,BCPZERO                                                  
         ZAP   BTHITEM,=P'1'                                                    
         LA    RF,BTHLNQ(R4)                                                    
         MVI   0(RF),0                                                          
         LA    RF,1(RF)                                                         
         SR    RF,R2                                                            
         STCM  RF,3,BATRLEN                                                     
*                                                                               
         GOTO1 AIO,IOACCMST+IOADD+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADDOOK   B     ROUTE                                                            
*                                                                               
ADDONOK  MVC   FVMSGNO,=AL2(AE$BATAE)                                           
         MVC   FVADDR,BHPAFLDH     SET CURSOR ADDRESS FOR ERRORS                
         B     ROUTH                                                            
         DROP  R2,R4,RC                                                         
BHWORKD  DSECT                                                                  
BHPARMS  DS    0XL8                                                             
BHPTYPE  DS    0XL1                BATCH TYPE                                   
BHPABREF DS    A                   A(REF/MOA/MOACHARS) SEE BHBATCHD             
BHPINDS  DS    0XL1                INDICATOR BYTE                               
BHPIINC  EQU   X'80'               OKAY TO INCREMENT REFERENCE                  
BHPAFLDH DS    A                   A(INPUT FIELD) - FOR CURSOR                  
BHWORKL  EQU   *-BHWORKD                                                        
BHBATCHD DSECT                                                                  
BHBREF   DS    CL4                 BATCH REF#                                   
BHBMP    DS    PL2                 PACKED MOA                                   
BHBMC    DS    CL2                 CHARACTER MOA                                
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST VALIDITY OF PFKEY TABLE ENTRY                       *         
*                                                                     *         
* NTRY: R1=A(PFK TABLE ENTRY)                                         *         
***********************************************************************         
         SPACE 1                                                                
TSTPFK   DS    0H                                                               
         USING *,R8                                                             
         LR    R3,R1                                                            
         USING PFKTABD,R3                                                       
         MVC   BCHALF,CSMASK       TEST INVALID PFKEY MASK                      
         NC    BCHALF,PFKRMASK                                                  
         BNZ   TSTPFKN                                                          
*                                                                               
         TM    PFKINDS1,PFKIACTN   TEST RECORD/ACTION PFKEY                     
         BO    *+12                                                             
         TM    PFKINDS3,PFKIPOS    TEST POSITIONAL PFKEY                        
         BZ    TPFK10                                                           
*                                                                               
         LA    RE,TWASESRA         TEST ACTION USED IN PREVIOUS SESSION         
         XR    RF,RF                                                            
         ICM   RF,1,TWASESNL                                                    
         BZ    TPFK02                                                           
         CLC   PFKRECA,0(RE)                                                    
         BE    TSTPFKN                                                          
         LA    RE,L'TWASESRA(RE)                                                
         BCT   RF,*-14                                                          
*                                                                               
TPFK02   GOTO1 ATSTACT,PFKACTN     TEST RECORD/ACTION VALID                     
         BNE   TSTPFKN                                                          
         GOTO1 ATSTMIX,PFKRECA     TEST RECORD/ACTION VALID                     
         BNE   TSTPFKN                                                          
         L     RF,AMIXNTRY                                                      
         USING MIXTABD,RF                                                       
         TM    PFKINDS3,PFKIPOS    OKAY IF POSTIONAL                            
         BO    TSTPFKY                                                          
         TM    MIXINDS2,MIXISETP   TEST SET-UP SCREEN MUST BE PROCESSED         
         BZ    *+12                                                             
         TM    CSINDSG1,CSINDSET                                                
         BZ    TSTPFKN                                                          
         TM    MIXINDS2,MIXIJCLB   TEST CLIENT BILLLING JOB REQUIRED            
         BZ    *+12                                                             
         TM    BCJOBSTA,BCJOBSCB                                                
         BZ    TSTPFKN                                                          
         TM    MIXINDS2,MIXIFMTC   TEST FORMAT CODE REQUIRED                    
         BZ    *+12                                                             
         CLI   CSFORMAT,0                                                       
         BE    TSTPFKN                                                          
         B     TSTPFKY                                                          
         DROP  RF                                                               
*                                                                               
TPFK10   TM    PFKINDS1,PFKISCRL+PFKIAPPL                                       
         BNZ   TSTPFKY             VALID FOR SCROLL/APPLICATION                 
*                                                                               
         TM    PFKINDS1,PFKIKAPA   TEST FOR KNOWN APPLICATION ACTION            
         BZ    TPFK12                                                           
         CLI   PFKSUBA,0                                                        
         BE    TSTPFKY                                                          
         CLC   PFKSUBA,CSSUBACT    MATCH ON SUB-ACTION                          
         BE    TSTPFKY                                                          
         B     TSTPFKN                                                          
*                                                                               
TPFK12   TM    PFKINDS1,PFKIQUIT   TEST QUIT TO PREVIOUS LEVEL                  
         BZ    TPFK14                                                           
         CLI   TWASESNL,0                                                       
         BNE   TSTPFKY                                                          
         B     TSTPFKN                                                          
*                                                                               
TPFK14   TM    PFKINDS1,PFKINEXT   TEST QUIT & PROCESS NEXT                     
         BZ    TPFK20                                                           
         XR    R1,R1                                                            
         ICM   R1,1,TWASESNL       ENSURE PREVIOUS LEVEL IS A LIST              
         BZ    TSTPFKN                                                          
         SLL   R1,1                                                             
         GOTO1 ATSTMIX,TWASESRA-L'TWASESRA(R1)                                  
         BNE   TSTPFKN                                                          
         L     RF,AMIXNTRY                                                      
         TM    MIXINDS1-MIXTABD(RF),MIXILST                                     
         BO    TSTPFKY                                                          
         B     TSTPFKN                                                          
*                                                                               
TPFK20   DS    0H                                                               
*                                                                               
TSTPFKY  B     ROUTE                                                            
TSTPFKN  B     ROUTL                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND COLUMN ON LIST SCREEN                               *         
*                                                                     *         
* NTRY: P1=A(1 OR 2 COLUMN CHARACTER)                                 *         
* EXIT: P1=A(FIELD HEADER ON SCREEN)                                  *         
*       P2=A(COLUMN TABLE ENTRY)                                      *         
*       P3=A(LINE TABLE ENTRY)                                        *         
*       CC=LOW IF COLUMN NOT ON LIST SCREEN                           *         
*       CC=HIGH IF COLUMN IS PROTECTED                                *         
*       CC=EQUAL IF COLUMN IS OPEN AND ON LIST SCREEN                 *         
***********************************************************************         
         SPACE 1                                                                
         USING FCWORKD,RC                                                       
FNDCLM   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         L     RF,0(R1)                                                         
         MVC   FCCODE,0(RF)        ASSUME 2 CHARACTER CODE                      
         LA    RE,PRE2CODE         SEARCH FOR PREFIX                            
         LA    R0,L'PRE2CODE                                                    
         CLC   0(1,RF),0(RE)                                                    
         BE    FCLM01                                                           
         LA    RE,1(RE)                                                         
         BCT   R0,*-14                                                          
         MVI   FCCODE,C' '         NOT FOUND - 1 CHARACTER CODE                 
         MVC   FCCODE+1(1),0(RF)                                                
FCLM01   OI    FCCODE+1,X'40'                                                   
*                                                                               
         LA    R4,LSLIN                                                         
         USING LINTABD,R4          R4=A(LINE TABLE ENTRY)                       
FCLM02   CLI   LINTABD,EOT                                                      
         BE    FNDCLMN                                                          
         L     R3,ACLMTAB                                                       
         AH    R3,LINCLM                                                        
         USING CLMTABD,R3          R3=A(COLUMN TABLE ENTRY)                     
         CLC   CLMCODE,FCCODE                                                   
         BE    *+12                                                             
         LA    R4,LINTABL(R4)                                                   
         B     FCLM02                                                           
*                                                                               
         XR    R2,R2               R2=A(FIELD HEADER)                           
         IC    R2,LINHDR                                                        
         AH    R2,CSSELACT                                                      
         LA    R2,TWAD(R2)                                                      
         USING FHD,R2                                                           
         STM   R2,R4,0(R1)                                                      
         TM    FHAT,FHATPR                                                      
         BO    ROUTH                                                            
         B     ROUTE                                                            
*                                                                               
FNDCLMN  XC    0(12,R1),0(R1)                                                   
         B     ROUTL                                                            
         DROP  R2,R3,R4                                                         
         SPACE 1                                                                
FCWORKD  DSECT                     ** FNDCLM LOCAL W/S **                       
FCCODE   DS    CL2                 COLUMN CODE                                  
FCWORKL  EQU   *-FCWORKD                                                        
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET WORK-CODE DATA (IN WCDATA)                           *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'40' TO CALL GETOPT (ON IO1) FOR W-C TYPE        *         
*             1-3 = A(WORK-CODE)                                      *         
* EXIT: P1        = A(WCDATA)                                         *         
*              CC = LOW IF INVALID WORK-CODE                          *         
*              CC = EQUAL IF VALID WORK-CODE AND NO IO NEEDED         *         
*              CC = HIGH IF VALID WORK-CODE AND WORK-CODE READ        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING GWWORKD,RC                                                       
GETWCD   DS    0H                                                               
         USING *,R8                                                             
         L     R3,=A(WCDATA-WORKD)                                              
         LA    R3,WORKD(R3)                                                     
         USING WCDATA,R3                                                        
         MVC   GWPARMS,0(R1)                                                    
         ST    R1,GWAR1                                                         
         ST    R3,0(R1)            RETURN A(WCDATA)                             
         XR    RE,RE                                                            
         ICM   RE,7,GWPAWC                                                      
         MVC   GWWC,0(RE)                                                       
         CLC   WCCODE,GWWC         TEST ALREADY HAVE W-C                        
         BE    GETWCDE                                                          
*                                                                               
         XC    WCDATA,WCDATA                                                    
*                                                                               
         MVC   GWIOKEY,IOKEY                                                    
         USING WCORECD,IOKEY       READ DIRECTORY RECORD                        
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   WCOKWRK,GWWC                                                     
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BNE   GETWCDL             INVALID WORK-CODE                            
*                                                                               
         MVC   IODAOVER,WCOKDA     GET ACCMST RECORD                            
         LA    RF,GWIO                                                          
         ST    RF,IOADDR                                                        
         GOTO1 AIO,IOGET+IOACCMST                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WCCODE,WCOKWRK                                                   
         LA    R1,GWIO+(WCORFST-WCORECD)                                        
         XR    RF,RF                                                            
GWCD02   CLI   0(R1),0             TEST END-OF-RECORD                           
         BE    GWCD10                                                           
         CLI   0(R1),WCOELQ        COPY WORK-CODE ELEMENT                       
         BNE   *+14                                                             
         MVC   WCEL,0(R1)                                                       
         B     GWCD08                                                           
         CLI   0(R1),NAMELQ        COPY EXTRA NAME                              
         BNE   GWCD08                                                           
         IC    RE,NAMLN-NAMELD(R1)                                              
         SH    RE,=Y(NAMLN1Q+1)                                                 
         MVC   WCNAME,BCSPACES                                                  
         EX    RE,*+4                                                           
         MVC   WCNAME,NAMEREC-NAMELD(R1)                                        
GWCD08   IC    RF,1(R1)                                                         
         BXH   R1,RF,GWCD02                                                     
*                                                                               
GWCD10   DS    0H                                                               
*&&US                                                                           
         TM    GWPINDS,GWPIGOPT    TEST GETOPT CALL REQUIRED                    
         BZ    GWCD12                                                           
         GOTO1 AGETOPT,BCPARM,AIO1                                              
         L     RF,AGOPBLK                                                       
         MVC   WCTYPE,GOWRKTY-GOBLOCK(RF)                                       
         CLI   WCTYPE,C' '                                                      
         BH    *+10                                                             
         MVC   WCTYPE,WCEL+(WCOTYPE-WCOELD)                                     
*&&                                                                             
GWCD12   MVC   IOKEY,GWIOKEY                                                    
         TM    LSINDS1,LSIBLST     TEST CURRENTLY BUILDING LIST                 
         BZ    GETWCDH                                                          
         LA    RF,IOKEY            YES - RE-READ DIRECTORY RECORD               
         USING TRNRECD,RF                                                       
         CLC   TRNKDATE,BCSPACES                                                
         BH    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETWCDH  L     R1,GWAR1            SET CC=HIGH FOR WORK-CODE READ               
         ST    R3,0(R1)                                                         
         B     ROUTH                                                            
*                                                                               
GETWCDE  L     R1,GWAR1            SET CC=EQUAL FOR NO READ                     
         ST    R3,0(R1)                                                         
         B     ROUTE                                                            
*                                                                               
GETWCDL  L     R1,GWAR1            SET CC=LOW FOR INVALIDE WORK CODE            
         ST    R3,0(R1)                                                         
         B     ROUTL                                                            
         SPACE 1                                                                
         POP   USING                                                            
         SPACE 1                                                                
GWWORKD  DSECT                     * GETWCD W/S *                               
GWPARMS  DS    0XL4                                                             
GWPINDS  DS    XL1                 RECORD FILTERING TYPE                        
GWPIGOPT EQU   X'40'               CALL GETOPT ON IO1                           
GWPAWC   DS    AL3                 A(WORK-CODE)                                 
GWAR1    DS    A                   A(R1)                                        
*                                                                               
GWWC     DS    CL2                 WORK CODE                                    
GWIOKEY  DS    XL(L'IOKEY)         SAVED IOKEY                                  
*                                                                               
GWIO     DS    XL2048              IO AREA                                      
GWWORKL  EQU   *-GWWORKD                                                        
CLB40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
ALL      EQU   0                                                                
         SPACE 1                                                                
         ORG   LTORG                                                            
         LTORG                                                                  
         SPACE 1                                                                
NINES    DC    C'999999999999'                                                  
DMREAD   DC    C'DMREAD  '                                                      
DMWRITE  DC    C'DMWRT   '                                                      
TEMPSTR  DC    C'TEMPSTR '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ADDATEND DC    C'ADD=END'                                                       
TRNORDER DC    C'**'                                                            
PZERO    DC    PL1'0'                                                           
PL8ZERO  DC    PL8'0'                                                           
PMINUS1  DC    PL1'-1'                                                          
EXPUL    DC    C'SE'               EXPENSE UNIT/LEDGER                          
INCUL    DC    C'SI'               INCOME UNIT/LEDGER                           
INCSUSUL DC    C'SK'               INCOME SUSPENSE UNIT/LEDGER                  
         SPACE 1                                                                
PRE2CODE DC    C' .<(!$@*)/%>'     VALID PREFIXES FOR 2 CHARACTER CODES         
         SPACE 1                                                                
         DS    (LTORGX-*)X                                                      
         ORG                                                                    
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDGETHELPD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGETHELPD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORKB                                                     
* ACCLBCOLS                                                                     
       ++INCLUDE ACCLBCOLS                                                      
         SPACE 1                                                                
CLB40    CSECT                     ALLOW EASIER RE-LOAD OF PHASE                
         ORG   CLB40+(((*-CLB40)/512)+1)*512                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'208ACCLB40B  08/17/00'                                      
         END                                                                    
