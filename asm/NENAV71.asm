*          DATA SET NENAV71    AT LEVEL 010 AS OF 03/16/18                      
*PHASE T31871B                                                                  
*INCLUDE TWABLD                                                                 
NENAV71  TITLE '- Network Navigator - server support routines 1'                
NENAV71  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NN71**,RR=RA                                                 
                                                                                
         USING WORKD,R9            R9=A(Global w/s)                             
         L     R7,ALP                                                           
         USING LP_D,R7             R7=A(LP_D)                                   
         LARL  R8,GLOBALS                                                       
         USING GLOBALS,R8          R8=A(Global literals)                        
         SR    RE,RE                                                            
         SLDL  RE,8                Branch index held in hob of RF               
         SLL   RE,2                                                             
         CHI   RE,ROUTABL          Ensure good index value                      
         JL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         JNZ   *+6                                                              
         DC    H'0'                Routine not defined                          
         AR    RF,RB               RF=A(routine)                                
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,2(RE)          R5=temporary w/s amount                      
         BZR   RF                                                               
                                                                                
         AHI   R5,7                Round amount to doublewords                  
         SRL   R5,3                                                             
         SLL   R5,3                                                             
         LR    R3,RD               Acquire storage from w/s chain               
         AR    R3,R5                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               and clear it                                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R4,R2                                                            
         BR    RF                  Go to routine                                
         DROP  RB                                                               
                                                                                
ROUTAB   DS    0XL4                                                             
         DC    AL2(WRKINI-NENAV71),AL2(0)                                       
         DC    AL2(IOEXEC-NENAV71),AL2(IOWORKL)                                 
         DC    AL2(NXTREC-NENAV71),AL2(0)                                       
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
***********************************************************************         
* Initialize working storage variables                                *         
***********************************************************************         
                                                                                
WRKINI   J     *+12                                                             
         DC    CL8'*WRKINI*'                                                    
         LA    R0,GLVALUES         Move literals to w/s                         
         LHI   R1,GLVALUEL                                                      
         LA    RE,LVALUES                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R1,RELOLST          Relocate adcons                              
         LHI   R0,RELOLSTN                                                      
         BASR  RE,0                                                             
         L     RF,0(R1)                                                         
         AR    RF,RA                                                            
         ST    RF,0(R1)                                                         
         AHI   R1,L'RELOLST                                                     
         BCTR  R0,RE                                                            
                                                                                
         LHI   RF,IODAWKA-WORKD    D/A & DMWORK AREA - regular I/O              
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIODAWKA                                                      
                                                                                
         LHI   RF,IODAWKB-WORKD    D/A & DMWORK AREA - block I/O                
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIODAWKB                                                      
                                                                                
         LHI   RF,IOAREAS-WORKD    Set addresses of I/O areas                   
         LA    RF,WORKD(RF)                                                     
         LHI   R0,AIONM                                                         
         LA    R1,AIO1             R1=A(A(I/O area))                            
         LA    R2,IOLS             R2=I/O area length table                     
         BASR  RE,0                                                             
         ST    RF,0(R1)                                                         
         AH    RF,0(R2)                                                         
         AHI   R1,L'AIO1                                                        
         AHI   R2,L'IOLS                                                        
         BCTR  R0,RE                                                            
                                                                                
         LA    R1,FACTAB                                                        
         USING FACTABD,R1          Extract COMFACS adcons                       
         LHI   R0,FACTABN                                                       
         BASR  RE,0                                                             
         SR    R2,R2                                                            
         ICM   R2,3,FACTDOUT                                                    
         LA    R2,WORKD(R2)                                                     
         SR    R3,R3                                                            
         ICM   R3,3,FACTDIN                                                     
         A     R3,ACOMFACS                                                      
         MVC   0(4,R2),0(R3)                                                    
         AHI   R1,FACTABL                                                       
         BCTR  R0,RE                                                            
                                                                                
         LA    R2,CORPHS           R2=A(core phase list)                        
         LA    R3,APHASES          R3=A(core phase address list)                
         LHI   R4,CORPHSN          R4=core phase count                          
         SR    R0,R0                                                            
         ICM   R0,14,T00A                                                       
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
WRKINI02 IC    R0,0(R2)                                                         
         GOTOR (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         AHI   R2,1                Bump to the next entry                       
         AHI   R3,L'APHASES        Bump to next address                         
         JCT   R4,WRKINI02         Do for number of phases                      
                                                                                
         LARL  R0,MEDTAB                                                        
         ST    R0,AMEDTAB          Set A(media table)                           
                                                                                
         MVC   LP_BLKS+((B#IOA1-1)*L'LP_BLKS)(AIONM*L'LP_BLKS),AIO1             
                                                                                
         LA    R0,DDMMI                                                         
         ST    R0,LP_ANDX          Set A(master map index)                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* I/O executive                                                       *         
***********************************************************************         
                                                                                
IOEXEC   J     *+12                                                             
         DC    CL8'*IOEXEC*'                                                    
         USING IOWORKD,RC          RC=A(local working storage)                  
         ST    R1,IOCTRL           Save I/O control bytes                       
         XC    IOADAWK,IOADAWK     Set no work area                             
         OC    IODAOVER,IODAOVER   Leave iowork intact if IODAOVER set          
         JNZ   *+10                                                             
         XC    IOWORK,IOWORK       Clear IOWORK area                            
                                                                                
         TM    IOCTRL4,IOLOCK      Test read-for-update                         
         JZ    *+8                                                              
         OI    IOQ,IOQLOCK                                                      
         TM    IOCTRL4,IORDEL      Test read for deleted records                
         JZ    *+8                                                              
         OI    IOQ,IOQRDEL                                                      
                                                                                
         LH    R1,=AL2(IOALL)      Establish I/O area address                   
         TM    IOCTRL4,IOBLK#      Test using block address format              
         JZ    *+8                                                              
         LH    R1,=AL2(IOBLKS)                                                  
         N     R1,IOCTRL                                                        
         JZ    IOEX020             I/O area not given                           
                                                                                
         TM    IOCTRL4,IOBLK#      Test using block address format              
         JZ    IOEX010                                                          
         SLL   R1,2                R1=block number*4                            
         L     RF,LP_BLKS-L'LP_BLKS(R1)                                         
         ST    RF,IOADDR                                                        
         OC    IOADDR,IOADDR                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SRL   R1,2                                                             
         BCTR  R1,0                                                             
         MHI   R1,L'IODAWKB                                                     
         A     R1,AIODAWKB                                                      
         ST    R1,IOADAWK          I/O D/A & DMWORK save area                   
         J     IOEX020                                                          
                                                                                
IOEX010  SRL   R1,12-2             R1=I/O area number*4                         
         LA    RF,AIO1-L'AIO1(R1)                                               
         MVC   IOADDR,0(RF)                                                     
         OC    IOADDR,IOADDR                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SRL   R1,2                R1=I/O area number                           
         BCTR  R1,0                                                             
         MHI   R1,L'IODAWKA                                                     
         A     R1,AIODAWKA                                                      
         ST    R1,IOADAWK          I/O D/A & DMWORK save area                   
                                                                                
IOEX020  LHI   R1,IOFILES          Establish file                               
         N     R1,IOCTRL                                                        
         JNZ   IOEX030                                                          
         OC    IOFILE,IOFILE                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOFILNM,IOFILE      Set file name                                
         OC    IOCMND,IOCMND       File given - so must command be              
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOCMDNM,IOCMND      Set command name                             
         J     IOEX130                                                          
                                                                                
IOEX030  SRL   R1,8                R1=File number                               
         LA    RE,FILTAB                                                        
         USING FILTABD,RE                                                       
IOEX040  CLI   FILNUM,0                                                         
         JNE   *+6                                                              
         DC    H'0'                Invalid file number                          
         CLM   R1,1,FILNUM         Match on file number                         
         JE    *+12                                                             
         AHI   RE,FILTABL                                                       
         J     IOEX040                                                          
         MVC   IOFILV,FILNUM       Extract file values                          
         LA    RE,CMDTAB           RE=A(I/O command table)                      
         USING CMDTABD,RE                                                       
         SR    RF,RF                                                            
         LHI   R1,IOCMNDS          Establish command                            
         TM    IOCTRL4,IOBLK#      Test using block address format              
         JZ    *+8                                                              
         LH    R1,=AL2(IOBCMDS)                                                 
         N     R1,IOCTRL                                                        
         JNZ   IOEX050                                                          
         OC    IOCMND,IOCMND       Not given - test command named               
         JNZ   IOEX130                                                          
         DC    H'0'                                                             
                                                                                
IOEX050  CLI   CMDFILT,0           Test end of file command table               
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(1),CMDFILT                                                 
         NC    IODUB(1),IOFILI                                                  
         CLC   IODUB(1),CMDFILT    Match file name to table                     
         JNE   *+12                                                             
         LA    RE,CMDNTRY                                                       
         J     IOEX060                                                          
         SR    RF,RF               Bump to next table entry                     
         ICM   RF,3,CMDTABL                                                     
         AR    RE,RF                                                            
         J     IOEX050                                                          
                                                                                
IOEX060  TM    IOCTRL4,IOBLK#      Test using block address format              
         JZ    IOEX070                                                          
         SRL   R1,16-4             Yes - shift command number to LOB            
                                                                                
         USING CMDNTRY,RE          RE=A(command table entry)                    
IOEX070  CLI   CMDNTRY,0                                                        
         JNE   *+6                                                              
         DC    H'0'                Invalid command                              
         CLM   R1,1,CMDNUM         Match on command number                      
         JE    *+12                                                             
         AHI   RE,CMDNTRYL                                                      
         J     IOEX070                                                          
                                                                                
         MVC   IOCMDV,CMDNAME      Extract command values                       
                                                                                
         TM    IOCMDI,CMDIDAXC     Test clear D/A now                           
         JZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IOCMDI,CMDIDADD     Test ADDREC                                  
         JO    IOEX100                                                          
         TM    IOCMDI,CMDIDARQ     Test D/A required for I/O                    
         JZ    IOEX110                                                          
         ICM   R1,15,IOADAWK       Point to I/O D/A & DMWORK area               
         JZ    IOEX090                                                          
         TM    IOCMDI,CMDIDAXC     Test clear D/A now (GETREC)                  
         JZ    *+10                                                             
         XC    0(L'IODA+L'IOWORK,R1),0(R1)                                      
         OC    IODAOVER,IODAOVER   Test override D/A passed                     
         JZ    IOEX080                                                          
         MVC   IODA,IODAOVER       Set IODA                                     
         XC    IODAOVER,IODAOVER   and clear iowork                             
         TM    IOCMDI,CMDIUPDT     Test PUTREC                                  
         JZ    IOEX090                                                          
         OC    IOWORK,IOWORK       Test IOWORK non-zero for PUTREC              
         JNZ   IOEX090                                                          
         DC    H'0'                                                             
                                                                                
IOEX080  MVC   IODA(L'IODA+L'IOWORK),0(R1)                                      
                                                                                
IOEX090  OC    IODA,IODA           Test D/A present                             
         JNZ   IOEX100                                                          
         TM    IOFILI,FILIIS       Test this is a D/A file                      
         JNZ   *+14                                                             
         TM    IOFILI2,FILIDI      and that an I/S file is attached             
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(4),IOCTRL                                                  
         NI    IODUB+2,X'F0'       Turn-off file indicators                     
         L     R0,IODUB                                                         
         SR    R1,R1                                                            
         IC    R1,IOFILN2                                                       
         SLL   R1,8                                                             
         OR    R1,R0                                                            
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    IOEX100             Successful I/O                               
         JL    IOEXX               Exit on bad I/S errors                       
         TM    IOERR,IOERNF        Test record-not-found                        
         JNZ   IOEXX                                                            
         TM    IOERR,IOEDEL        Test record is deleted                       
         JZ    IOEXX                                                            
         OC    IODA,IODA           Test disk address set                        
         JNZ   IOEX100                                                          
         DC    H'0'                Something bad happened                       
                                                                                
                                                                                
IOEX100  ICM   R0,15,IOADDR        Point to record                              
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR VDATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK,0           
         MVC   IOERR,8(R1)         Save return error byte                       
         GOTOR IOTRCE,IOTRFIL                                                   
         ICM   R1,15,IOADAWK       Park D/A & DMWORK for this I/O               
         JZ    IOEXX                                                            
         MVC   0(L'IODA+L'IOWORK,R1),IODA                                       
         J     IOEXX               Exit to caller                               
                                                                                
IOEX110  TM    IOFILI,FILIIS       Test index sequential file                   
         JZ    IOEX130                                                          
         MVC   IOKEYSAV,IOKEY      Save current I/O key                         
         LA    R0,IOKEY            FL I/S reads into IOKEY                      
         TM    IOFILI,FILIVL                                                    
         JZ    IOEX120                                                          
         ICM   R0,15,IOADDR        VL I/S reads into I/O area directly          
         JNZ   IOEX120                                                          
         DC    H'0'                                                             
                                                                                
IOEX120  GOTOR ,IOCB,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                           
         GOTOR IOTRCE,IOTRDIR+IOTRBEF                                           
         GOTOR VDATAMGR,IOCB                                                    
         MVC   IOERR,8(R1)         Save return error byte                       
         GOTOR IOTRCE,IOTRDIR+IOTRAFT                                           
         TM    IOFILI2,FILIID                                                   
         JNZ   *+14                                                             
         L     R1,IOADDR                                                        
         MVC   IOKEY,0(R1)         Return key if directory only                 
         TM    IOERR,IOERRS        Test any errors found                        
         JZ    *+12                                                             
         TM    IOERR,IOEDEL        Test deleted record found                    
         JZ    IOEXX               No - exit with error                         
         TM    IOFILI2,FILIID      Test D/A file attached to this file          
         JZ    IOEXX               No - exit                                    
         TM    IOCMDI,CMDIUPDT     Test updative I/O (DMWRT/DMADD)              
         JNZ   IOEXX               Yes - exit                                   
         LLC   R1,IOFILKL                                                       
         LLC   R0,IOFILCL                                                       
         AR    R1,R0                                                            
         LA    R1,IOKEY(R1)                                                     
         MVC   IODA,0(R1)                                                       
         ICM   R1,15,IOADAWK       Save D/A for this I/O & clear DMWORK         
         JZ    IOEXX                                                            
         MVC   0(L'IODA,R1),IODA                                                
         XC    L'IODA(L'IOWORK,R1),L'IODA(R1)                                   
         J     IOEXX                                                            
                                                                                
IOEX130  ICM   R0,15,IOADDR                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR VDATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                    
         MVC   IOERR,8(R1)                                                      
                                                                                
IOEXX    TM    IOERR,IOERRS        Any errors?                                  
         JZ    EXITY                                                            
         TM    IOERR,IOEEOF+IOERNF+IOEDEL                                       
         JNZ   EXITH               Logical error                                
         J     EXITL               Physical error                               
         EJECT                                                                  
***********************************************************************         
* Routine to print I/O trace                                          *         
***********************************************************************         
                                                                                
IOTRCE   CLI   IOTRACE,C' '        Test trace is active                         
         BNHR  RE                                                               
         CLI   IOTRACE,C'N'                                                     
         BER   RE                                                               
         STC   R1,IOTRIND                                                       
         ST    RE,IOSAVERE                                                      
         MVC   IOP,SPACES                                                       
         L     RF,LP_ARUNP                                                      
         L     RF,RUNPMODE-RUNPARMD(RF)                                         
         L     RF,RMASTC-RUNFACSD(RF)                                           
         MVC   IOVPRNT,MCVPRINT-MASTD(RF)                                       
                                                                                
         L     RF,IOCB+0                                                        
         MVC   IOP(L'IOCMDNM),0(RF)                                             
         L     RF,IOCB+4                                                        
         MVC   IOP+L'IOCMDNM+1(L'IOFILNM),0(RF)                                 
         SR    R2,R2                                                            
         IC    R2,IOFILKL          R2=Key length                                
         TM    IOTRIND,IOTRDIR     Test directory I/O                           
         JZ    IOTRCE04                                                         
         MVC   IOP+20(L'IOIKEYI),IOIKEYI                                        
         TM    IOTRIND,IOTRAFT                                                  
         JZ    IOTRCE02                                                         
         MVC   IOP+20(L'IOIKEYO),IOIKEYO                                        
         TM    IOFILI,FILIVL                                                    
         JNZ   IOTRCE02                                                         
         SR    R0,R0                                                            
         IC    R0,IOFILCL                                                       
         AR    R2,R0                                                            
         AHI   R2,4                                                             
                                                                                
IOTRCE02 BCTR  R2,0                                                             
         BASR  RB,0                                                             
         MVC   IOP+25(0),IOKEY                                                  
         EX    R2,0(RB)                                                         
         BASR  RB,0                                                             
         TR    IOP+25(0),IOTRTTAB                                               
         EX    R2,0(RB)                                                         
         GOTOR IOVPRNT,IOPARM,IOP-1,IOBL01                                      
         MVC   IOP,SPACES                                                       
         GOTOR VHEXOUT,IOPARM,IOKEY,IOHEXWRK,1(R2),IOHEXSEP                     
         BASR  RB,0                                                             
         MVC   IOP+25(0),IOHEXWRK                                               
         EX    R2,0(RB)                                                         
         GOTOR IOVPRNT,IOPARM,IOP-1,IOBL01                                      
         MVC   IOP,SPACES                                                       
         LA    R1,IOHEXWRK+1(R2)                                                
         BASR  RB,0                                                             
         MVC   IOP+25(0),0(R1)                                                  
         EX    R2,0(RB)                                                         
         GOTOR IOVPRNT,IOPARM,IOP-1,IOBL01                                      
         J     IOTRCEX                                                          
                                                                                
IOTRCE04 TM    IOTRIND,IOTRFIL                                                  
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         IC    R0,IOFILCL                                                       
         AR    R2,R0               Add on control length                        
         AHI   R2,6                Plus L'length and link area                  
         L     R0,IOCB+8                                                        
         GOTOR VHEXOUT,IOPARM,(R0),IOWORK,4,IOHEXTOG                            
         L     R3,IOCB+12                                                       
         BCTR  R2,0                                                             
         BASR  RB,0                                                             
         MVC   IOP+25(0),0(R3)                                                  
         EX    R2,0(RB)                                                         
         BASR  RB,0                                                             
         TR    IOP+25(0),IOTRTTAB                                               
         EX    R2,0(RB)                                                         
         GOTOR IOVPRNT,IOPARM,IOP-1,IOBL01                                      
         MVC   IOP,SPACES                                                       
         MVC   IOP(L'IODALIT),IODALIT                                           
         MVC   IOP+L'IODALIT(8),IOWORK                                          
         GOTOR VHEXOUT,IOPARM,IOCB+8,IOP+13,1,IOHEXTOG                          
         GOTOR VHEXOUT,IOPARM,(R3),IOHEXWRK,1(R2),IOHEXSEP                      
         BASR  RB,0                                                             
         MVC   IOP+25(0),IOHEXWRK                                               
         EX    R2,0(RB)                                                         
         GOTOR IOVPRNT,IOPARM,IOP-1,IOBL01                                      
         MVC   IOP,SPACES                                                       
         LA    R1,IOHEXWRK+1(R2)                                                
         BASR  RB,0                                                             
         MVC   IOP+25(0),0(R1)                                                  
         EX    R2,0(RB)                                                         
         GOTOR IOVPRNT,IOPARM,IOP-1,IOBL01                                      
                                                                                
IOTRCEX  L     RE,IOSAVERE                                                      
         BR    RE                                                               
         DROP  RC                                                               
         EJECT                                                                  
                                                                                
IOWORKD  DSECT                     ** IOEXEC s/r local w/s **                   
IODUB    DS    D                                                                
IOCB     DS    6F                                                               
IOPARM   DS    6F                                                               
IOSAVERE DS    A                                                                
IOVPRNT  DS    A                   V(PRINT)                                     
IOADAWK  DS    A                   A(I/O D/A & DMWORK save area)                
IOCTRL   DS    0XL4                ** I/O command word **                       
IOCTRL1  DS    X                                                                
IOCTRL2  DS    X                                                                
IOCTRL3  DS    X                   I/O area#/file#                              
IOCTRL4  DS    X                   Block#/Read-for-update/read deletes          
IOQ      DS    X                   ** I/O command qualifier **                  
IOQLOCK  EQU   X'80'               Read-for-uodate                              
IOQRDEL  EQU   X'08'               Read deletes                                 
IOFILV   DS    0XL15               ** Extracted file values **                  
IOFILNO  DS    X                   File number                                  
IOFILNM  DS    CL7                 Command name                                 
IOFILI   DS    X                   File indicators - 1                          
IOFILI2  DS    X                   File indicators - 2                          
IOFILN2  DS    X                   File number 2 (I/S D/A pair)                 
IOFILKL  DS    X                   Key length                                   
IOFILCL  DS    X                   Control length                               
IOFILDE  EQU   IOFILCL             Displacement to first element                
IOFILML  DS    XL2                 Maximum record length                        
IOTRIND  DS    X                   ** Trace control **                          
IOTRDIR  EQU   X'40'               I/O to directory                             
IOTRFIL  EQU   X'20'               I/O to file                                  
IOTRBEF  EQU   X'01'               Before I/O                                   
IOTRAFT  EQU   X'02'               After I/O                                    
IOCMDV   DS    0XL10               ** Extracted command values **               
IOCMDNM  DS    CL7                 Command name                                 
IOCMDNO  DS    X                   Command number                               
IOCMDI   DS    X                   Command indicators - 1                       
IOCMDI2  DS    X                   Command indicators - 2                       
IOP      DS    CL132                                                            
IOHEXWRK DS    XL220                                                            
IOWORKL  EQU   *-IOWORKD                                                        
NENAV71  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Get next record                                                     *         
*                                                                     *         
* Note:- P3/B0 has file equate/delete option (see $NXTRxxx equates)   *         
***********************************************************************         
                                                                                
NXTREC   J     *+12                                                             
         DC    CL8'*NXTREC*'                                                    
         L     R0,0(R1)            R0=A(key table)                              
         LM    R3,R6,4(R1)         R3=A(key save area), R4=A(work save)         
         LA    R3,0(R3)            R5=A(key filter routine)                     
         MVC   BYTE1,8(R1)         R6=A(record filter routine)                  
         SR    RE,RE                                                            
         ICM   RE,1,4(R1)                                                       
         SLL   RE,2                                                             
         L     RE,LP_BLKS-L'LP_BLKS(RE)                                         
         ST    RE,IOADDR                                                        
         TM    BYTE1,$NXTRXAD      Test inhibit setting LP_ADATA                
         JNZ   *+8                                                              
         ST    RE,LP_ADATA                                                      
         CLI   LP_RMODE,LP_RFRST   Test first time call                         
         JNE   NXTREC02                                                         
         MVI   LP_RMODE,LP_RNEXT   Reset first time call                        
         CLI   0(R1),NOQ           Test record set is wanted                    
         JE    NXTREC08                                                         
         LTR   R3,R3               Test record key save area provided           
         JZ    *+10                                                             
         MVC   0(L'IOKEY,R3),IOKEY Yes - save current record key                
         XC    IOKEY,IOKEY         Start with a clean key                       
                                                                                
NXTREC02 GOTOR LP_ASETK,DMCB,(0,(R0)),IOKEY,(R4),('FF',LP_D)                    
         JH    NXTREC06                                                         
         LHI   R1,IODIR                                                         
         TM    BYTE1,$NXTRSPT      Test I/O to spot file                        
         JZ    *+8                                                              
         LHI   R1,IOSPTDIR                                                      
         TM    BYTE1,$NXTRSTA      Test I/O to station file                     
         JZ    *+8                                                              
         LHI   R1,IOSTAFIL                                                      
         TM    BYTE1,$NXTRTRF      Test I/O to traffic file                     
         JZ    *+8                                                              
         LHI   R1,IOTRFDIR                                                      
         TM    BYTE1,$NXTRUNT      Test I/O to unit file                        
         JZ    *+8                                                              
         LHI   R1,IOUNTDIR                                                      
         TM    BYTE1,$NXTRXSP      Test I/O to xspot file                       
         JZ    *+8                                                              
         LHI   R1,IOXSPDIR                                                      
         AHI   R1,IOHI                                                          
         TM    BYTE1,$NXTRDEL      Test deleted records required                
         JZ    *+8                                                              
         AHI   R1,IORDEL                                                        
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         JE    *+14                                                             
         CLI   IOERR,IOEDEL        Test record is deleted                       
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR LP_ASETK,DMCB,(1,(R0)),IOKEY,(R4),('FF',LP_D)                    
         JNE   NXTREC02                                                         
                                                                                
         LTR   RF,R5               Test/set directory filter routine            
         JZ    *+12                                                             
         GOTOR GOFILT                                                           
         JNE   NXTREC02                                                         
                                                                                
         TM    BYTE1,$NXTRSTA+$NXTRXGR                                          
         JNZ   EXITY                                                            
         LHI   R1,IOFIL                                                         
         TM    BYTE1,$NXTRSPT      Test I/O to spot file                        
         JZ    *+8                                                              
         LHI   R1,IOSPTFIL                                                      
         TM    BYTE1,$NXTRTRF      Test I/O to traffic file                     
         JZ    *+8                                                              
         LHI   R1,IOTRFFIL                                                      
         TM    BYTE1,$NXTRXSP      Test I/O TO xspot file                       
         JZ    *+8                                                              
         LHI   R1,IOXSPFIL                                                      
         TM    BYTE1,$NXTRUNT      Test I/O to unit file                        
         JZ    *+8                                                              
         LHI   R1,IOUNTFIL                                                      
         AHI   R1,IOGET+IORDEL                                                  
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         JE    NXTREC04                                                         
         TM    IOERR,IOEDEL        Ignore deleted records                       
         JNZ   *+6                                                              
         DC    H'0'                Die on other errors                          
                                                                                
         LR    R1,R0               R1=A(key driver table)                       
         SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         BCTR  RE,0                RE=L'record key-1                            
         L     RF,IOADDR           RF=A(record)                                 
         BASR  RB,0                                                             
         CLC   IOKEY(0),0(RF)      Test key matches (active pointer)            
         EX    RE,0(RB)                                                         
         JNE   NXTREC02            No - ignore undeleted passives               
         TM    BYTE1,$NXTRDEL      Test deleted records wanted                  
         JZ    NXTREC02                                                         
                                                                                
NXTREC04 LTR   RF,R6               Set/test file filter routine                 
         JZ    EXITY                                                            
         L     R1,IOADDR           Pass A(record) in R1                         
         GOTOR GOFILT                                                           
         JNE   NXTREC02            Didn't pass filters - get next               
         J     EXITY                                                            
                                                                                
NXTREC06 LTR   R3,R3               Test any key saved                           
         JZ    NXTREC08                                                         
         MVC   IOKEY,0(R3)         Yes - restore it                             
                                                                                
NXTREC08 MVI   LP_RMODE,LP_RLAST   Set no more records to come                  
         J     EXITN               Exit with cc=not equal to caller             
                                                                                
GOFILT   NTR1  LABEL=NO            Call record filter routine                   
         LM    R2,RB,LP_R2RB                                                    
         BASR  RE,RF                                                            
         J     EXIT                                                             
         EJECT                                                                  
EXITN    DS    0H                  Set CC not equal                             
EXITL    LHI   RE,0                Set CC low                                   
         J     EXITCC                                                           
EXITH    LHI   RE,2                Set CC high                                  
         J     EXITCC                                                           
EXITY    LHI   RE,1                Set CC equal                                 
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                                                                
                                                                                
GLOBALS  DS    0D                  ** Global literals **                        
                                                                                
         LTORG                                                                  
                                                                                
LVALUES  DS    0D                  ** Literals moved to WORKD **                
         DC    V(TWABLD)                                                        
         DC    8A(0)                                                            
         DC    XL(L'T00A)'D9000A'                                               
         DC    CL(L'SPACES)' '                                                  
         DC    (L'EFFS)AL1(FF)                                                  
LVALUESL EQU   *-LVALUES                                                        
                                                                                
IOLS     DS    0H                  ** Lengths of I/O areas **                   
         DC    AL2(IO1LQ)          Length of I/O area 1                         
         DC    AL2(IO2LQ)          Length of I/O area 2                         
         DC    AL2(IO3LQ)          Length of I/O area 3                         
         DC    AL2(IO4LQ)          Length of I/O area 4                         
         DC    AL2(IO5LQ)          Length of I/O area 5                         
         DC    AL2(IO6LQ)          Length of I/O area 6                         
         DC    AL2(IO7LQ)          Length of I/O area 7                         
         DC    AL2(IO8LQ)          Length of I/O area 8                         
                                                                                
FACTAB   DS    0XL(FACTABL)        ** Extracted COMFACS addresses **            
         DC    AL2(VADDAY-WORKD,CADDAY-COMFACSD)                                
         DC    AL2(VBINSRCH-WORKD,CBINSRCH-COMFACSD)                            
         DC    AL2(VCALLOV-WORKD,CCALLOV-COMFACSD)                              
         DC    AL2(VCASHVAL-WORKD,CCASHVAL-COMFACSD)                            
         DC    AL2(VCUREDIT-WORKD,CCUREDIT-COMFACSD)                            
         DC    AL2(VDATCON-WORKD,CDATCON-COMFACSD)                              
         DC    AL2(VDATVAL-WORKD,CDATVAL-COMFACSD)                              
         DC    AL2(VDATAMGR-WORKD,CDATAMGR-COMFACSD)                            
         DC    AL2(VDDLINK-WORKD,CDDLINK-COMFACSD)                              
         DC    AL2(VGETDAY-WORKD,CGETDAY-COMFACSD)                              
         DC    AL2(VGETFACT-WORKD,CGETFACT-COMFACSD)                            
         DC    AL2(VGETPROF-WORKD,CGETPROF-COMFACSD)                            
         DC    AL2(VGLOBBER-WORKD,CGLOBBER-COMFACSD)                            
         DC    AL2(VHELLO-WORKD,CHELLO-COMFACSD)                                
         DC    AL2(VHEXIN-WORKD,CHEXIN-COMFACSD)                                
         DC    AL2(VHEXOUT-WORKD,CHEXOUT-COMFACSD)                              
         DC    AL2(VSECRET-WORKD,CSECRET-COMFACSD)                              
         DC    AL2(VSWITCH-WORKD,CSWITCH-COMFACSD)                              
         DC    AL2(VXSORT-WORKD,CXSORT-COMFACSD)                                
         DC    AL2(VLOCKET-WORKD,CLOCKET-COMFACSD)                              
FACTABN  EQU   (*-FACTAB)/FACTABL                                               
                                                                                
FACTABD  DSECT                     ** Layout of FACTAB above **                 
FACTDOUT DS    AL2                 Displacement to output address               
FACTDIN  DS    AL2                 Displacement to input address                
FACTABL  EQU   *-FACTABD                                                        
NENAV71  CSECT                                                                  
                                                                                
CORPHS   DS    0AL1                ** Coreres phases to load **                 
         DC    AL1(QFALINK)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(QTSAR)                                                       
         DC    AL1(QSTAVAL)                                                     
         DC    AL1(QQSORT)                                                      
         DC    AL1(QDAYVAL)                                                     
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QMOBILE)                                                     
         DC    AL1(QGETBRD)                                                     
         DC    AL1(QNETVALU)                                                    
CORPHSN  EQU   (*-CORPHS)/L'CORPHS                                              
                                                                                
FILTAB   DS    0X                  ** File definitions **                       
                                                                                
         DC    AL1(IOUNTDIR/256),C'UNTDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOUNTFIL/256,20,01),AL2(25)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOSPTDIR/256),C'SPTDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOSPTFIL/256,13,01),AL2(18)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOTRFDIR/256),C'TRFDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOTRFFIL/256,13,01),AL2(18)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOXSPDIR/256),C'XSPDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOXSPFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENDIR/256),C'GENDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOGENFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOSPTFIL/256),C'SPTFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOSPTDIR/256,13,01),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOUNTFIL/256),C'UNTFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOUNTDIR/256,20,01),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOTRFFIL/256),C'TRFFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOTRFDIR/256,13,01),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOXSPFIL/256),C'XSPFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOXSPDIR/256,32,04),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIL/256),C'GENFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOGENDIR/256,32,04),AL2(2000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOSTAFIL/256),C'STATION'                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,15,00),AL2(1000)                                           
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCTFILE/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,29),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(0)                                                           
                                                                                
CMDTAB   DS    0X                  ** I/O commands **                           
                                                                                
*                                  Index sequential commands                    
CMDIS    DC    AL1(FILIIS,0),AL2(CMDISX+1-CMDIS)                                
         DC    C'DMRDHI ',AL1(IOHI,0,0)                                         
         DC    C'DMREAD ',AL1(IORD,0,0)                                         
         DC    C'DMRSEQ ',AL1(IOSQ,0,0)                                         
         DC    C'DMADD  ',AL1(IOADD,CMDIUPDT,0)                                 
         DC    C'DMWRT  ',AL1(IOWRITE,CMDIUPDT,0)                               
CMDISX   DC    AL1(0)                                                           
                                                                                
*                                  Direct access commands                       
CMDDA    DC    AL1(FILIDA,0),AL2(CMDDAX+1-CMDDA)                                
         DC    C'GETREC ',AL1(IOHI,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IORD,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOSQ,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOGET,CMDIDARQ,0)                                 
         DC    C'ADDREC ',AL1(IOADDREC,CMDIDADD+CMDIUPDT,0)                     
         DC    C'PUTREC ',AL1(IOPUTREC,CMDIDARQ+CMDIUPDT,0)                     
CMDDAX   DC    AL1(0)                                                           
                                                                                
CMDTABX  DC    AL1(0)                                                           
                                                                                
IOHEXSEP DC    C'SEP'                                                           
IOHEXTOG DC    C'TOG'                                                           
IOBL01   DC    C'BL01'                                                          
IOIKEYI  DC    C'IKEY='                                                         
IOIKEYO  DC    C'OKEY='                                                         
IODALIT  DC    C'D/A='                                                          
                                                                                
IOTRTTAB DS    0XL256                                                           
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
                                                                                
         DS    0H                                                               
MEDTAB   DS    0XL(MEDTABL)        ** Media table **                            
         DC    AL1(MEDTMRAD),AL2(0000),CL18'Radio'                              
         DC    AL1(MEDTMHIS),AL2(0000),CL18'Hispanic'                           
         DC    AL1(MEDTMOTH),AL2(0000),CL18'Other'                              
         DC    AL1(MEDTMSYN),AL2(0774),CL18'Syndication'                        
         DC    AL1(MEDTMCBL),AL2(0775),CL18'Cable'                              
         DC    AL1(MEDTMNET),AL2(0777),CL18'Network'                            
         DC    AL1(MEDTMALL),AL2(7777),CL18'National Network'                   
MEDTABN  EQU   (*-MEDTAB)/MEDTABL                                               
MEDTABX  DC    AL1(MEDTMEOT)                                                    
                                                                                
DDMMI    LKMMI H,NETSYSQ           ** Master map index **                       
                                                                                
         LKMMI D,M#UNTDWN,P#UNTDWN,(*,UNTDLLIT),RUNNER=B                        
         LKMMI D,M#ALOINT,P#ALOINT,(*,ALOCOLIT),RUNNER=B                        
         LKMMI U,M#UPLCLI,P#CFMUPL,(*,CUCLILIT),UPDATIVE=Y                      
         LKMMI U,M#UPLPRD,P#CFMUPL,(*,CUPRDLIT),UPDATIVE=Y                      
         LKMMI D,M#CMMDWN,P#MMKDWN,(*,MMKDLLIT),RUNNER=B                        
         LKMMI D,M#UNORNR,P#UNORNR,(*,UONRULIT),RUNNER=B                        
         LKMMI D,M#FUADLD,P#FUADLD,(*,FUADLLIT),RUNNER=B                        
         LKMMI U,M#UPLBRU,P#UPLBRU,(*,UPBRULIT),UPDATIVE=Y                      
         LKMMI U,M#UPLLIM,P#UPLLIM,(*,UPLIMLIT),UPDATIVE=Y                      
         LKMMI D,I#CFMIDL,P#CFMINI,(*,NCFMILIT)                                 
         LKMMI D,I#CFMCDL,P#CFMCDL,(*,NCFMCLIT),RUNNER=B                        
         LKMMI D,M#MVDOWN,P#MVDOWN,(*,MVANDLIT),RUNNER=B                        
         LKMMI D,M#MFMNET,P#MFMNET,(*,MFMNELIT),RUNNER=B                        
         LKMMI D,M#MFMPRD,P#MFMPRD,(*,MFMPRLIT),RUNNER=B                        
         LKMMI D,M#MFMCLT,P#MFMCLT,(*,MFMCLLIT),RUNNER=B                        
         LKMMI D,M#MFMMED,P#MFMMED,(*,MFMMDLIT)                                 
         LKMMI D,M#NECLTD,P#NCLTDL,(*,NCLDLLIT)                                 
         LKMMI D,M#STALST,P#NSTADL,(*,STADLLIT)                                 
         LKMMI D,M#TRCMML,P#CMMLDL,(*,TCMLDLIT)                                 
         LKMMI D,M#TRHOUS,P#CMMLDL,(*,THOUDLIT)                                 
         LKMMI D,M#TRNRCP,P#CMMLDL,(*,TNRCDLIT)                                 
         LKMMI D,M#TRCPHS,P#CMMLDL,(*,TCPHDLIT)                                 
         LKMMI D,M#TCMLDL,P#CMMLDL,(*,CMLLDLIT)                                 
         LKMMI D,M#TPRFDL,P#CMMLDL,(*,TPRFDLIT)                                 
*                                                                               
         LKMMI D,M#TRCTXT,P#NTRADL,(*,CTXTDLIT)                                 
         LKMMI D,M#TRCCLA,P#NTRADL,(*,CCLADLIT)                                 
         LKMMI D,M#TRFEED,P#NTRADL,(*,FEEDDLIT)                                 
         LKMMI D,M#TRPTRN,P#NTRADL,(*,PTRNDLIT)                                 
         LKMMI D,M#NEPGRD,P#NTRADL,(*,NPGDLLIT)                                 
         LKMMI U,M#NECMLU,P#CMMLUP,(*,NCMLULIT),UPDATIVE=Y                      
         LKMMI U,M#NEPATU,P#PATUP,(*,NPATULIT),UPDATIVE=Y                       
*                                                                               
         LKMMI D,M#BLDNI,P#QNI,(*,NINSBLD)                                      
         LKMMI D,M#DOWNNI,P#QNI,(*,NINSBLDX)                                    
         LKMMI D,M#SUBNI,P#QNI,(*,NINSSUB),UPDATIVE=Y                           
         LKMMI D,M#ENDNI,P#QNI,(*,NINSSUBX)                                     
*                                                                               
         LKMMI D,M#BLDNL,P#QNA,(*,NLISBLD)                                      
         LKMMI D,M#DOWNNL,P#QNA,(*,NLISDOWN)                                    
*                                                                               
         LKMMI D,M#BLDNA,P#QNA,(*,NASGNBLD)                                     
         LKMMI D,M#DOWNNA,P#QNA,(*,NASGNDOWN)                                   
         LKMMI D,M#UPASGN,P#QNA,(*,NASGNUP),UPDATIVE=Y                          
         LKMMI D,M#SUBNA,P#QNA,(*,NASGNSUB)                                     
         LKMMI D,M#ENDASGN,P#QNA,(*,NASGNEND)                                   
*                                                                               
         LKMMI US,M#NSEED,P#NSEED,(*,NSEDDLIT),UPDATIVE=Y                       
         LKMMI D,M#RTNGS,P#NCLTDL,(*,RTNGSLIT),RUNNER=B                         
         LKMMI US,M#NPATG,P#NPATG,(*,NPATGLIT),UPDATIVE=Y                       
*                                                                               
                                                                                
                                                                                
         LKMMI E                                                                
                                                                                
ALOCOLIT DC    C'Allocation Optimizer Interface'                                
CUCLILIT DC    C'CFM Net Client Upload'                                         
CUPRDLIT DC    C'CFM Net Product Upload'                                        
FUADLLIT DC    C'FUA Download'                                                  
MFMCLLIT DC    C'MFM Client Download'                                           
MFMMDLIT DC    C'MFM Media Download'                                            
MFMNELIT DC    C'MFM Network Download'                                          
MFMPRLIT DC    C'MFM Product Download'                                          
MMKDLLIT DC    C'MatchMaker Download'                                           
MVANDLIT DC    C'MediaVantage Download'                                         
NCFMILIT DC    C'CFM Initial download'                                          
NCFMCLIT DC    C'CFM Client download'                                           
UNTDLLIT DC    C'Steward Unit Download'                                         
UONRULIT DC    C'Units Ordered Not Run Download'                                
UPBRULIT DC    C'BA Rules Record Upload'                                        
UPLIMLIT DC    C'Limit Record Upload'                                           
NCLDLLIT DC    C'Client Download'                                               
STADLLIT DC    C'Station Download'                                              
TCMLDLIT DC    C'Traffic - Commercial Download'                                 
THOUDLIT DC    C'Traffic - Production House Download'                           
TNRCDLIT DC    C'Traffic - Net Recap Download'                                  
TCPHDLIT DC    C'Traffic - Client Product House Download'                       
CMLLDLIT DC    C'Traffic - Commercial List Display Download'                    
CTXTDLIT DC    C'Net Traffic - Comtext List Display Download'                   
CCLADLIT DC    C'Net Traffic - Comclass List Display Download'                  
FEEDDLIT DC    C'Net Traffic - Feed List Display Download'                      
PTRNDLIT DC    C'Net Traffic - Pattern List Display Download'                   
NPGDLLIT DC    C'Product Group Display Download'                                
NCMLULIT DC    C'Net Traffic - Commercial Upload'                               
NPATULIT DC    C'Net Traffic - Pattern Upload'                                  
TPRFDLIT DC    C'Traffic - Profile Records Download'                            
NINSBLD  DC    C'Net/trf - Nins/gen build'                                      
NINSBLDX DC    C'Net/trf - Nins/gen build return'                               
NINSSUB  DC    C'Net/trf - Nins/gen submit'                                     
NINSSUBX DC    C'Net/trf - Nins/gen submit return'                              
NLISBLD   DC   C'Net/trf - Net/list build'                                      
NLISDOWN  DC   C'Net/trf - Net/list download'                                   
NASGNBLD  DC   C'Net/trf - Net/asgn build'                                      
NASGNDOWN DC   C'Net/trf - Net/asgn download'                                   
NASGNUP   DC   C'Net/trf - Net/asgn upload'                                     
NASGNSUB  DC   C'Net/trf - Net/asgn submit'                                     
NASGNEND  DC   C'Net/trf - Net/asgn end'                                        
*                                                                               
NSEDDLIT DC   C'Net/trf - Net/Seed request'                                     
NPATGLIT DC   C'Net/trf - Pat/Gen request'                                      
RTNGSLIT DC   C'Rating Source Download'                                         
                                                                                
* NENAVWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE NENAVWORKD                                                     
         PRINT ON                                                               
                                                                                
* GEMAPEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEMAPEQUS                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010NENAV71   03/16/18'                                      
         END                                                                    
