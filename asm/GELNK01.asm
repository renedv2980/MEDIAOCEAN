*          DATA SET GELNK01    AT LEVEL 001 AS OF 11/16/09                      
*PHASE TA0601A                                                                  
*INCLUDE TWABLD                                                                 
GELNK01  TITLE '- Control system server support routines 1'                     
GELNK01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GL01**,RR=RE                                                 
         USING WORKD,R9            R9=A(global w/s)                             
         LARL  R8,GLOBALS                                                       
         USING GLOBALS,R8          R8=A(global literals)                        
         L     RA,ALP                                                           
         USING LP_D,RA             RA=A(LP_D)                                   
         ST    RE,ROU1RELO         Save my relocation factor                    
         SR    RE,RE                                                            
         SLDL  RE,8                Branch index held in hob RF                  
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
         LR    R3,RD               Acquire storage from w/s pool                
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
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* Initialize working storage variables                                *         
***********************************************************************         
                                                                                
WRKINI   J     *+12                                                             
         DC    CL8'*WRKINI*'                                                    
                                                                                
         MVC   GLVALUES(GLVALUEL),LVALUES                                       
                                                                                
         LA    R1,RELOLST          Relocate address constants                   
         LHI   R0,RELOLSTN                                                      
         BASR  RE,0                                                             
         L     RF,0(R1)                                                         
         A     RF,ROU1RELO                                                      
         ST    RF,0(R1)                                                         
         AHI   R1,L'RELOLST                                                     
         BCTR  R0,RE                                                            
                                                                                
         LHI   RF,IOAREA1-WORKD    Set addresses of I/O areas                   
         LA    RF,WORKD(RF)                                                     
         LHI   R0,AIONM                                                         
         LA    R1,AIO1                                                          
         BASR  RE,0                                                             
         ST    RF,0(R1)                                                         
         AHI   RF,IOLENQ                                                        
         AHI   R1,L'AIO1                                                        
         BCTR  R0,RE                                                            
                                                                                
         LA    R1,FACTAB                                                        
         USING FACTABD,R1          Extract COMFACS addresses                    
         LHI   R0,FACTABN                                                       
         BASR  RE,0                                                             
         LLH   R2,FACTDOUT                                                      
         LA    R2,WORKD(R2)                                                     
         LLH   R3,FACTDIN                                                       
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
         AHI   R2,1                Bump to next entry                           
         AHI   R3,L'APHASES        Bump to next address                         
         JCT   R4,WRKINI02         Do for number of phases                      
                                                                                
         TM    LP_FLAG1,LP_FOFFL   Exit if off-line                             
         JNZ   EXITY                                                            
         L     R0,ATWA             Set A(SECRET control block)                  
         AHI   R0,SVSECRET-TWAD                                                 
         ST    R0,LP_ASECD                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* I/O executive                                                       *         
***********************************************************************         
                                                                                
IOEXEC   J     *+12                                                             
         DC    CL8'*IOEXEC*'                                                    
         USING IOWORKD,RC          RC=A(local working storage)                  
         ST    R1,IOCTRL           Save I/O control bytes                       
         MVI   IOQ,0               Establish command qualifier                  
         TM    IOCTRL+3,IOLOCK     Test read-for-update                         
         JZ    *+8                                                              
         OI    IOQ,X'80'                                                        
         TM    IOCTRL+3,IORDEL     Test deleted records wanted                  
         JZ    *+8                                                              
         OI    IOQ,X'08'                                                        
         LH    R1,=AL2(IOALL)      Establisg I/O area address                   
         N     R1,IOCTRL                                                        
         JZ    IOEX02                                                           
         SRL   R1,12               R1=I/O area number                           
         BCTR  R1,0                                                             
         MHI   R1,IOLENQ                                                        
         A     R1,AIO1                                                          
         STCM  R1,15,IOADDR        Set addesss of I/O area                      
IOEX02   LHI   R1,IOFILES          Establish file                               
         N     R1,IOCTRL                                                        
         JNZ   IOEX04                                                           
         OC    IOFILE,IOFILE                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOFILNM,IOFILE      Set file name                                
         OC    IOCMND,IOCMND       File given - so must command be              
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOCMDNM,IOCMND      Set command name                             
         J     IOEX20                                                           
IOEX04   SRL   R1,8                R1=file number                               
         LA    RE,FILTAB                                                        
         USING FILTABD,RE                                                       
IOEX06   CLI   FILNUM,0                                                         
         JNE   *+6                                                              
         DC    H'0'                Invalid file number                          
         CLM   R1,1,FILNUM         Match on file number                         
         JE    *+12                                                             
         AHI   RE,FILTABL                                                       
         J     IOEX06                                                           
         MVC   IOFILV,FILNUM       Extract file values                          
         LA    RE,CMDTAB           RE=A(I/O command table)                      
         USING CMDTABD,RE                                                       
         SR    RF,RF                                                            
         LHI   R1,IOCMNDS          Establish command                            
         N     R1,IOCTRL                                                        
         JNZ   IOEX08                                                           
         OC    IOCMND,IOCMND       Not given - test command named               
         JNZ   *+6                                                              
         DC    H'0'                                                             
         J     IOEX20                                                           
IOEX08   CLI   CMDFILT,0                                                        
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(1),CMDFILT                                                 
         NC    IODUB(1),IOFILI                                                  
         CLC   IODUB(1),CMDFILT                                                 
         JNE   *+12                                                             
         LA    RE,CMDNTRY                                                       
         J     IOEX10                                                           
         LLH   RF,CMDTABL                                                       
         AR    RE,RF                                                            
         J     IOEX08                                                           
         USING CMDNTRY,RE          RE=A(command table entry)                    
IOEX10   CLI   CMDNTRY,0                                                        
         JNE   *+6                                                              
         DC    H'0'                Invalid command                              
         CLM   R1,1,CMDNUM         Match on command number                      
         JE    *+12                                                             
         AHI   RE,CMDNTRYL                                                      
         J     IOEX10                                                           
         MVC   IOCMDV,CMDNAME      Extract command values                       
                                                                                
         TM    IOCMDI,CMDIDAXC     Test clear d/a now                           
         JZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IOCMDI,CMDIDADD     Test ADDREC                                  
         JO    IOEX14                                                           
         TM    IOCMDI,CMDIDARQ     Test D/A required for I/O                    
         JZ    IOEX16                                                           
         ICM   R1,15,IOADDR        Find this I/O area DA/work                   
         JZ    IOEX12                                                           
         AHI   R1,IODDWQ           Dispalce to DA/work in I/O area              
         TM    IOCMDI,CMDIDAXC     Test clear D/A now                           
         JZ    *+10                                                             
         XC    0(L'IODA,R1),0(R1)                                               
         OC    0(L'IODA,R1),0(R1)                                               
         JZ    *+10                                                             
         MVC   IODA,0(R1)          Yes - set D/A                                
         AHI   R1,L'IODA                                                        
         OC    0(L'IOWORK,R1),0(R1)                                             
         JZ    IOEX12                                                           
         MVC   IOWORK,0(R1)                                                     
                                                                                
IOEX12   OC    IODA,IODA           Test D/A present                             
         JNZ   IOEX14                                                           
         TM    IOFILI,FILIIS       Test this is a D/A file                      
         JNZ   *+14                                                             
         TM    IOFILI2,FILIDI      and that an I/S file is attached             
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(4),IOCTRL                                                  
         NI    IODUB+2,X'F0'       Turn off file indicators                     
         L     R0,IODUB                                                         
         LLC   R1,IOFILN2                                                       
         SLL   R1,8                                                             
         OR    R1,R0                                                            
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    IOEX14              Successful I/O                               
         JL    IOEXX               Exit on bad I/S errors                       
         TM    IOERR,IOERNF        Test record not found                        
         JNZ   IOEXX                                                            
         TM    IOERR,IOEDEL        Test record is deleted                       
         JZ    IOEXX                                                            
         OC    IODA,IODA           Test disk address set                        
         JNZ   IOEX14                                                           
         DC    H'0'                Something bad happened                       
IOEX14   ICM   R0,15,IOADDR                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR VDATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK             
         MVC   IOERR,8(R1)         Save error return byte                       
         GOTOR IOTRCE,IOTRFIL                                                   
         ICM   R1,15,IOADDR        Park DA/work for this I/O area               
         JZ    IOEXX                                                            
         AHI   R1,IODDWQ           Displace to DA/work in I/O area              
         MVC   0(L'IODA,R1),IODA                                                
         MVC   L'IODA(L'IOWORK,R1),IOWORK                                       
         J     IOEXX               Exit to caller                               
                                                                                
IOEX16   TM    IOFILI,FILIIS       Test index sequential file                   
         JZ    IOEX20                                                           
         MVC   IOKEYSAV,IOKEY      Save current I/O key                         
         LA    R0,IOKEY            FL I/S reads into IOKEY                      
         TM    IOFILI,FILIVL                                                    
         JZ    IOEX18                                                           
         ICM   R0,15,IOADDR        VL I/S reads into IOAREA                     
         JNZ   IOEX18                                                           
         DC    H'0'                                                             
                                                                                
IOEX18   GOTOR ,IOCB,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                           
         GOTOR IOTRCE,IOTRDIR+IOTRBEF                                           
         GOTOR VDATAMGR,IOCB                                                    
         MVC   IOERR,8(R1)         Save error return byte                       
         GOTOR IOTRCE,IOTRDIR+IOTRAFT                                           
         TM    IOERR,IOERRS        Test any errors found                        
         JZ    *+12                                                             
         TM    IOERR,IOEDEL        Test deleted record found                    
         JZ    IOEXX               No - exit with error                         
         TM    IOFILI2,FILIID      Test D/A file attached to this one           
         JZ    IOEXX               No - exit                                    
         LLC   R1,IOFILKL                                                       
         LLC   R0,IOFILCL                                                       
         AR    R1,R0                                                            
         LA    R1,IOKEY(R1)                                                     
         MVC   IODA,0(R1)                                                       
         ICM   R1,15,IOADDR        Park DA for this I/O area                    
         JZ    IOEXX                                                            
         AHI   R1,IODDWQ           Displace to DA/work in I/O area              
         MVC   0(L'IODA,R1),IODA                                                
         J     IOEXX                                                            
                                                                                
IOEX20   ICM   R0,15,IOADDR                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR VDATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                    
         MVC   IOERR,8(R1)                                                      
                                                                                
IOEXX    TM    IOERR,IOERRS        Test for errors                              
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
         LLC   R2,IOFILKL          R2=key length                                
         TM    IOTRIND,IOTRDIR     Test directory I/O                           
         JZ    IOTRCE04                                                         
         MVC   IOP+20(L'IOIKEYI),IOIKEYI                                        
         TM    IOTRIND,IOTRAFT                                                  
         JZ    IOTRCE02                                                         
         MVC   IOP+20(L'IOIKEYO),IOIKEYO                                        
         TM    IOFILI,FILIVL                                                    
         JNZ   IOTRCE02                                                         
         LLC   R0,IOFILCL                                                       
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
         LLC   R0,IOFILCL                                                       
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
                                                                                
IOWORKD  DSECT                     ** IOEXEC s/r local w/s **                   
IODUB    DS    D                                                                
IOCB     DS    6F                                                               
IOPARM   DS    6F                                                               
IOSAVERE DS    A                                                                
IOVPRNT  DS    A                   V(PRINT)                                     
IOCTRL   DS    XL4                 I/O COMMAND WORD                             
IOQ      DS    X                   I/O COMMAND QUALIFIER (RFU/DELETES)          
IOFILV   DS    0XL15               Extracted file values (this I/O)             
IOFILNO  DS    X                   File number                                  
IOFILNM  DS    CL7                 Command name                                 
IOFILI   DS    X                   File indicators - 1                          
IOFILI2  DS    X                   File indicators - 2                          
IOFILN2  DS    X                   File number 2 (I/S D/A pair)                 
IOFILKL  DS    X                   Key length                                   
IOFILCL  DS    X                   Control length                               
IOFILDE  EQU   IOFILCL             Displacement to first element                
IOFILML  DS    XL2                 Maximum record length                        
IOTRIND  DS    XL1                 Trace indicators                             
IOTRDIR  EQU   X'40'               I/O to directory                             
IOTRFIL  EQU   X'20'               I/O to file                                  
IOTRBEF  EQU   X'01'               Before I/O                                   
IOTRAFT  EQU   X'02'               After I/O                                    
IOCMDV   DS    0XL10               Extracted command values (this I/O)          
IOCMDNM  DS    CL7                 Command name                                 
IOCMDNO  DS    X                   Command number                               
IOCMDI   DS    X                   Command indicators - 1                       
IOCMDI2  DS    X                   Command indicators - 2                       
IOP      DS    CL132                                                            
IOHEXWRK DS    XL220                                                            
IOWORKL  EQU   *-IOWORKD                                                        
GELNK01  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Get next record                                                     *         
*                                                                     *         
* Note: read deletes flag (P3/B0) may have following values:-         *         
*                                                                     *         
*        X'0'  - exclude deleted records                              *         
*        C'O'  - include deleted records only                         *         
*        other - include deleted records                              *         
***********************************************************************         
                                                                                
NXTREC   J     *+12                                                             
         DC    CL8'*NXTREC*'                                                    
         L     R0,0(R1)            R0=A(key table)                              
         LM    R3,R6,4(R1)         R3=A(key save area), R4=A(work save)         
         LA    R3,0(R3)            R5=A(key filter routine)                     
*                                  R6=A(record filter routine)                  
         MVC   BYTE1,8(R1)         Set read deletes control byte                
         LLC   RE,4(R1)                                                         
         SLL   RE,2                                                             
         L     RE,LP_BLKS-L'LP_BLKS(RE)                                         
         ST    RE,LP_ADATA                                                      
         ST    RE,IOADDR                                                        
         CLI   LP_RMODE,LP_RFRST   Test first time call                         
         JNE   NXTREC02                                                         
         MVI   LP_RMODE,LP_RNEXT   Reset first time call                        
         LTR   R3,R3               Test record key save area provided           
         JZ    *+10                                                             
         MVC   0(L'IOKEY,R3),IOKEY Yes - save current record key                
         XC    IOKEY,IOKEY                                                      
                                                                                
NXTREC02 GOTOR LP_ASETK,DMCB,(0,(R0)),IOKEY,(R4),('FF',LP_D)                    
         JH    NXTREC04                                                         
         LHI   R1,IOHI+IODIR                                                    
         CLI   BYTE1,0             Test caller wants deleted records            
         JE    *+8                                                              
         AHI   R1,IORDEL           Yes - set control bit                        
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         JE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   BYTE1,C'O'          Test want deleted records only               
         JNE   *+12                                                             
         TM    IOERR,IOEDEL        Yes - then record must be deleted            
         JZ    NXTREC02                                                         
         GOTOR LP_ASETK,DMCB,(1,(R0)),IOKEY,(R4),('FF',LP_D)                    
         JNE   NXTREC02                                                         
         LTR   RF,R5               Test/set directory filter routine            
         JZ    *+12                                                             
         GOTOR GOFILT                                                           
         JNE   NXTREC02                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IORDEL'                           
         JE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LTR   RF,R6               Set/test file filter routine                 
         JZ    EXITY                                                            
         L     R1,IOADDR           Pass A(record) in R1                         
         GOTOR GOFILT                                                           
         JNE   NXTREC02            Didn't pass filters - get next               
         J     EXITY                                                            
                                                                                
NXTREC04 LTR   R3,R3               Test any key saved                           
         JZ    NXTREC06                                                         
         MVC   IOKEY,0(R3)         Yes - restore it                             
                                                                                
NXTREC06 MVI   LP_RMODE,LP_RLAST   Set no more records to come                  
         J     EXITN               Exit with CC=not equal to caller             
                                                                                
GOFILT   NTR1  LABEL=NO            Call record filter routine                   
         L     RE,ALP                                                           
         LM    R2,RB,LP_R2RB-LP_D(RE)                                           
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
         EJECT                                                                  
GLOBALS  DS    0D                                                               
         LTORG                                                                  
                                                                                
LVALUES  DS    0F                  ** Literals moved to WORKD **                
         DC    V(TWABLD)                                                        
         DC    5A(0)                                                            
         DC    CL(L'SPACES)' '                                                  
         DC    X'D9000A'                                                        
         DS    0F                                                               
         DC    (L'EFFS)AL1(FF)                                                  
LVALUESL EQU   *-LVALUES                                                        
                                                                                
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
                                                                                
ROUTAB   DS    0XL4                                                             
         DC    AL2(WRKINI-GELNK01),AL2(0)                                       
         DC    AL2(IOEXEC-GELNK01),AL2(IOWORKL)                                 
         DC    AL2(NXTREC-GELNK01),AL2(0)                                       
ROUTABL  EQU   *-ROUTAB                                                         
                                                                                
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
         DC    AL2(VRUNIT-WORKD,CRUNIT-COMFACSD)                                
         DC    AL2(VGETDAY-WORKD,CGETDAY-COMFACSD)                              
         DC    AL2(VGETFACT-WORKD,CGETFACT-COMFACSD)                            
         DC    AL2(VGETPROF-WORKD,CGETPROF-COMFACSD)                            
         DC    AL2(VGLOBBER-WORKD,CGLOBBER-COMFACSD)                            
         DC    AL2(VHELLO-WORKD,CHELLO-COMFACSD)                                
         DC    AL2(VHEXIN-WORKD,CHEXIN-COMFACSD)                                
         DC    AL2(VHEXOUT-WORKD,CHEXOUT-COMFACSD)                              
         DC    AL2(VPERVAL-WORKD,CPERVAL-COMFACSD)                              
         DC    AL2(VPERVERT-WORKD,CPERVERT-COMFACSD)                            
         DC    AL2(VRUNIT-WORKD,CRUNIT-COMFACSD)                                
         DC    AL2(VSECRET-WORKD,CSECRET-COMFACSD)                              
         DC    AL2(VSOFDAT-WORKD,CSOFDAT-COMFACSD)                              
         DC    AL2(VSWITCH-WORKD,CSWITCH-COMFACSD)                              
         DC    AL2(VXSORT-WORKD,CXSORT-COMFACSD)                                
FACTABN  EQU   (*-FACTAB)/FACTABL                                               
                                                                                
FACTABD  DSECT                     ** Layout of FACTAB above **                 
FACTDOUT DS    AL2                 Displacement to output address               
FACTDIN  DS    AL2                 Displacement to input address                
FACTABL  EQU   *-FACTABD                                                        
GELNK01  CSECT                                                                  
                                                                                
CORPHS   DS    0AL1                ** Core resident phase list **               
         DC    AL1(QFALINK)                                                     
CORPHSN  EQU   (*-CORPHS)/L'CORPHS                                              
                                                                                
FILTAB   DS    0X                  ** File definitions **                       
                                                                                
         DC    AL1(IOGENDIR/256),C'GENDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOGENFIL/256,32,04),AL2(42)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIL/256),C'GENFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOGENDIR/256,32,04),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOSPTDIR/256),C'SPTDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOSPTFIL/256,13,01),AL2(18)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOSPTFIL/256),C'SPTFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOSPTDIR/256,13,01),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOSTAFIL/256),C'STATION'                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,15,00),AL2(1000)                                           
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOPRTDIR/256),C'PRTDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOPRTFIL/256,25,02),AL2(31)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOPRTFIL/256),C'PRTFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOPRTDIR/256,25,02),AL2(2000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOPUBDIR/256),C'PUBDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOPUBFIL/256,25,02),AL2(31)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOPUBFIL/256),C'PUBFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOPUBDIR/256,25,02),AL2(4000)                                
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
         DC    C'DMADD  ',AL1(IOADD,0,0)                                        
         DC    C'DMWRT  ',AL1(IOWRITE,0,0)                                      
CMDISX   DC    AL1(0)                                                           
                                                                                
*                                  Direct access commands                       
CMDDA    DC    AL1(FILIDA,0),AL2(CMDDAX+1-CMDDA)                                
         DC    C'GETREC ',AL1(IOHI,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IORD,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOSQ,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOGET,CMDIDARQ,0)                                 
         DC    C'ADDREC ',AL1(IOADDREC,CMDIDADD,0)                              
         DC    C'PUTREC ',AL1(IOPUTREC,CMDIDARQ,0)                              
CMDDAX   DC    AL1(0)                                                           
                                                                                
CMDTABX  DC    AL1(0)                                                           
                                                                                
* GELNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE GELNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001GELNK01   11/16/09'                                      
         END                                                                    
