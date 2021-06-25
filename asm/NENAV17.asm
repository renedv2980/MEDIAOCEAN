*          DATA SET NENAV17    AT LEVEL 005 AS OF 12/04/12                      
*PHASE T31817A                                                                  
NENAV17  TITLE '- Network Navigator - Record Maintenance'                       
         PRINT NOGEN                                                            
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,WORKERKEY=NEAL,SEGMENT=Y,LINKIO=Y,               +        
               FACS=FACS,APPEND=Y,REQUEST=*,CODE=CODE,IDF=Y,           +        
               SYSPHASE=SYSPHASE,SYSTEM=NETSYSQ,FILES=FILES,           +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED),RLEN=2500,         +        
               LOADFACSOFF=Y                                                    
                                                                                
NETSFMU  EQU   C'I'                Locally defined server class for now         
                                                                                
B#BRUREC EQU   B#IOA3              I/O 3 sued for BA rules record               
ABRUREC  EQU   LP_BLKS+((B#BRUREC-1)*L'LP_BLKS),,C'A'                           
B#LIMREC EQU   B#IOA3              I/O 3 sued for Limit record                  
ALIMREC  EQU   LP_BLKS+((B#LIMREC-1)*L'LP_BLKS),,C'A'                           
B#SPTREC EQU   B#IOA3              I/O 3 sued for Spot record                   
ASPTREC  EQU   LP_BLKS+((B#SPTREC-1)*L'LP_BLKS),,C'A'                           
         EJECT                                                                  
                                                                                
CODE     NMOD1 0,**NN17**                                                       
                                                                                
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(Global literals)                        
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(DDLINK parameter block)                 
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JNZ   INIT02                                                           
         L     R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         L     R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         J     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE                                                      
         USING WORKD,R9            R9=A(Global W/S)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         LAY   R8,LINKW            Point to end of work area                    
         USING SAVED,R8                                                         
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         LA    R0,LP_D                                                          
         ST    R0,ALP              Set A(LP_D)                                  
                                                                                
INIT04   MVC   RUNMODE,RUNPMODE    Extract DDLINK/RUNNER calling mode           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
                                                                                
         L     RF,ACOMFACS                                                      
         MVC   LINKIO,CLINKIO-COMFACSD(RF)                                      
         MVC   ALIOB,LP_ALIOB      Set A(LINKIO control block)                  
         DROP  R6,R7,RB                                                         
                                                                                
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
         EJECT                                                                  
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    Test first for run                           
         JNE   PRCWRK                                                           
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    EXITY               No                                           
                                                                                
         L     RF,LP_ARUNP                                                      
         L     RF,RUNPMODE-RUNPARMD(RF)                                         
         L     RF,RMASTC-RUNFACSD(RF)                                           
         MVC   VPRINT,MCVPRINT-MASTD(RF)                                        
                                                                                
         L     RF,LP_ACOM                                                       
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   LP_AUIR1,0(R1)      Set A(Support overlay 1)                     
         MVC   AROUTS1,0(R1)                                                    
         GOTOR (RF),(R1),('P#ROUTS2',0),0,0                                     
         MVC   LP_AUIR2,0(R1)      Set A(Support overlay 2)                     
         MVC   AROUTS2,0(R1)                                                    
         GOTOR (#WRKINI,AWRKINI)   Initialize global working storage            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Initialize download/upload variables                                *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    Test 'process work' mode                     
         JNE   RUNREQ                                                           
                                                                                
         LA    R0,QVALUES                                                       
         LHI   R1,QVALUEL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,QVALS                                                         
         LHI   R1,L'QVALS                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   RUNINDS,0                                                        
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Run a request                                                       *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    Test 'run request' mode                      
         JNE   EXITY                                                            
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    RUNREQ02                                                         
                                                                                
         L     RF,AMASTC           Set trace option                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,SPTDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,SPTFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,STAFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,UNTDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,UNTFIL,(4,0),0                               
                                                                                
         GOTOR (#GETAGY,AGETAGY),LP_AGY                                         
                                                                                
RUNREQ02 L     RF,LP_ALPXD         Extract server id                            
         MVC   MQSVR,LP_MQSVR-LP_XD(RF)                                         
                                                                                
         GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         OC    LP_ERROR,LP_ERROR   Test error number is set                     
         JNZ   EXITN                                                            
         J     EXITY               Exit back to DDLINK                          
         EJECT                                                                  
***********************************************************************         
* Upload request - BA Rules record                                    *         
***********************************************************************         
                                                                                
UPLBRU   LKREQ H,M#UPLBRU,NEXTREQ=UPLLIM,ROUTINE=BRUUPL,NEWREC=Y                
                                                                                
RecAct   LKREQ F,001,(D,B#SAVED,QRECACTN),CHAR,TEXT=(*,BRECALIT),COL=*          
Media    LKREQ F,002,(I,B#SAVED,QMEDNDX),(U,#VALMED,$VALMED),          +        
               OLEN=L'BRULAGY,MAXLEN=L'QMEDA,TEXT=(*,BMEDLIT),COL=*             
Client   LKREQ F,003,(D,B#SAVED,QBRUCLI),CHAR,TEXT=(*,BCLILIT),COL=*            
UAC      LKREQ F,004,(D,B#SAVED,QBRUUAC),CHAR,TEXT=(*,BUACLIT),COL=*            
RSD      LKREQ F,005,(D,B#SAVED,QBRURSD),CHAR,TEXT=(*,BRSDLIT),COL=*            
IZU      LKREQ F,006,(D,B#SAVED,QBRUIZU),CHAR,TEXT=(*,BIZULIT),COL=*            
DWP      LKREQ F,007,(D,B#SAVED,QBRUDWP),LBIN,TEXT=(*,BDWPLIT),COL=*            
TBL      LKREQ F,008,(D,B#SAVED,QBRUTBL),LBIN,TEXT=(*,BTBLLIT),COL=*            
TBU      LKREQ F,009,(D,B#SAVED,QBRUTBU),LBIN,TEXT=(*,BTBULIT),COL=*            
GBL      LKREQ F,010,(D,B#SAVED,QBRUGBL),LBIN,TEXT=(*,BGBLLIT),COL=*            
GBU      LKREQ F,011,(D,B#SAVED,QBRUGBU),LBIN,TEXT=(*,BGBULIT),COL=*            
WBL      LKREQ F,012,(D,B#SAVED,QBRUWBL),LBIN,TEXT=(*,BWBLLIT),COL=*            
WBU      LKREQ F,013,(D,B#SAVED,QBRUWBU),LBIN,TEXT=(*,BWBULIT),COL=*            
NTI      LKREQ F,014,(D,B#SAVED,QBRUNTI),LBIN,TEXT=(*,BNTILIT),COL=*            
CTI      LKREQ F,015,(D,B#SAVED,QBRUCTI),LBIN,TEXT=(*,BCTILIT),COL=*            
MDS      LKREQ F,016,(D,B#SAVED,QBRUMDS),LBIN,TEXT=(*,BMDSLIT),COL=*            
D1DWP    LKREQ F,017,(D,B#SAVED,QBRU1DWP),LBIN,TEXT=(*,B1DWPLIT),COL=*          
D1TBL    LKREQ F,018,(D,B#SAVED,QBRU1TBL),LBIN,TEXT=(*,B1TBLLIT),COL=*          
D1TBU    LKREQ F,019,(D,B#SAVED,QBRU1TBU),LBIN,TEXT=(*,B1TBULIT),COL=*          
D1GBL    LKREQ F,020,(D,B#SAVED,QBRU1GBL),LBIN,TEXT=(*,B1GBLLIT),COL=*          
D1GBU    LKREQ F,021,(D,B#SAVED,QBRU1GBU),LBIN,TEXT=(*,B1GBULIT),COL=*          
D1WBL    LKREQ F,022,(D,B#SAVED,QBRU1WBL),LBIN,TEXT=(*,B1WBLLIT),COL=*          
D1WBU    LKREQ F,023,(D,B#SAVED,QBRU1WBU),LBIN,TEXT=(*,B1WBULIT),COL=*          
D2DWP    LKREQ F,024,(D,B#SAVED,QBRU2DWP),LBIN,TEXT=(*,B2DWPLIT),COL=*          
D2TBL    LKREQ F,025,(D,B#SAVED,QBRU2TBL),LBIN,TEXT=(*,B2TBLLIT),COL=*          
D2TBU    LKREQ F,026,(D,B#SAVED,QBRU2TBU),LBIN,TEXT=(*,B2TBULIT),COL=*          
D2GBL    LKREQ F,027,(D,B#SAVED,QBRU2GBL),LBIN,TEXT=(*,B2GBLLIT),COL=*          
D2GBU    LKREQ F,028,(D,B#SAVED,QBRU2GBU),LBIN,TEXT=(*,B2GBULIT),COL=*          
D2WBL    LKREQ F,029,(D,B#SAVED,QBRU2WBL),LBIN,TEXT=(*,B2WBLLIT),COL=*          
D2WBU    LKREQ F,030,(D,B#SAVED,QBRU2WBU),LBIN,TEXT=(*,B2WBULIT),COL=*          
D3DWP    LKREQ F,031,(D,B#SAVED,QBRU3DWP),LBIN,TEXT=(*,B3DWPLIT),COL=*          
D3TBL    LKREQ F,032,(D,B#SAVED,QBRU3TBL),LBIN,TEXT=(*,B3TBLLIT),COL=*          
D3TBU    LKREQ F,033,(D,B#SAVED,QBRU3TBU),LBIN,TEXT=(*,B3TBULIT),COL=*          
D3GBL    LKREQ F,034,(D,B#SAVED,QBRU3GBL),LBIN,TEXT=(*,B3GBLLIT),COL=*          
D3GBU    LKREQ F,035,(D,B#SAVED,QBRU3GBU),LBIN,TEXT=(*,B3GBULIT),COL=*          
D3WBL    LKREQ F,036,(D,B#SAVED,QBRU3WBL),LBIN,TEXT=(*,B3WBLLIT),COL=*          
D3WBU    LKREQ F,037,(D,B#SAVED,QBRU3WBU),LBIN,TEXT=(*,B3WBULIT),COL=*          
NTP      LKREQ F,038,(D,B#SAVED,QBRUNTP),LBIN,TEXT=(*,BNTPLIT),COL=*            
PGP      LKREQ F,039,(D,B#SAVED,QBRUPGP),LBIN,TEXT=(*,BPGPLIT),COL=*            
                                                                                
         LKREQ E                                                                
                                                                                
BRECALIT DC    C'Record Action'                                                 
BMEDLIT  DC    C'Media Code'                                                    
BCLILIT  DC    C'Client Code'                                                   
BUACLIT  DC    C'Use Assigned Cost'                                             
BRSDLIT  DC    C'Round Split Dollars'                                           
BIZULIT  DC    C'Include $0 Units'                                              
BDWPLIT  DC    C'Dollar Weight %'                                               
BNTPLIT  DC    C'Network %'                                                     
BPGPLIT  DC    C'Program %'                                                     
BTBLLIT  DC    C'Total Budget Lower'                                            
BTBULIT  DC    C'Total Budget Upper'                                            
BGBLLIT  DC    C'Goal Budget Lower'                                             
BGBULIT  DC    C'Goal Budget Upper'                                             
BWBLLIT  DC    C'Weekly Budget Lower'                                           
BWBULIT  DC    C'Weekly Budget Upper'                                           
BNTILIT  DC    C'Network Seperation Minutes'                                    
BCTILIT  DC    C'Cable Seperation Minutes'                                      
BMDSLIT  DC    C'Maximum Double Spotting Allower'                               
B1DWPLIT DC    C'Demo 1 Target Weight %'                                        
B1TBLLIT DC    C'Demo 1 Total Lower'                                            
B1TBULIT DC    C'Demo 1 Total Upper'                                            
B1GBLLIT DC    C'Demo 1 Goal Lower'                                             
B1GBULIT DC    C'Demo 1 Goal  Upper'                                            
B1WBLLIT DC    C'Demo 1 Weekly Lower'                                           
B1WBULIT DC    C'Demo 1 Weekly Upper'                                           
B2DWPLIT DC    C'Demo 2 Target Weight %'                                        
B2TBLLIT DC    C'Demo 2 Total Lower'                                            
B2TBULIT DC    C'Demo 2 Total Upper'                                            
B2GBLLIT DC    C'Demo 2 Goal Lower'                                             
B2GBULIT DC    C'Demo 2 Goal  Upper'                                            
B2WBLLIT DC    C'Demo 2 Weekly Lower'                                           
B2WBULIT DC    C'Demo 2 Weekly Upper'                                           
B3DWPLIT DC    C'Demo 3 Target Weight %'                                        
B3TBLLIT DC    C'Demo 3 Total Lower'                                            
B3TBULIT DC    C'Demo 3 Total Upper'                                            
B3GBLLIT DC    C'Demo 3 Goal Lower'                                             
B3GBULIT DC    C'Demo 3 Goal  Upper'                                            
B3WBLLIT DC    C'Demo 3 Weekly Lower'                                           
B3WBULIT DC    C'Demo 3 Weekly Upper'                                           
         EJECT                                                                  
***********************************************************************         
* Upload request - Limit record                                       *         
***********************************************************************         
                                                                                
UPLLIM   LKREQ H,M#UPLLIM,NEXTREQ=REQEND,ROUTINE=LIMUPL,NEWREC=Y                
                                                                                
RecAct   LKREQ F,001,(D,B#SAVED,QRECACTN),CHAR,TEXT=(*,LRECALIT),COL=*          
Media    LKREQ F,002,(I,B#SAVED,QMEDNDX),(U,#VALMED,$VALMED),          +        
               OLEN=L'BRULAGY,MAXLEN=L'QMEDA,TEXT=(*,LMEDLIT),COL=*             
Client   LKREQ F,003,(D,B#SAVED,QLIMCLI),CHAR,TEXT=(*,LCLILIT),COL=*            
Product  LKREQ F,004,(D,B#SAVED,QLIMPRD),CHAR,TEXT=(*,LPRDLIT),COL=*            
Estimate LKREQ F,005,(D,B#SAVED,QLIMEST),LBIN,TEXT=(*,LESTLIT),COL=*            
Network  LKREQ F,006,(D,B#SAVED,QLIMNET),CHAR,TEXT=(*,LNETLIT),COL=*            
Status   LKREQ F,007,(I,B#SAVED,I$LIEARY),CHAR,ARRAY=S,SORT=NO,        *        
               OLEN=L'LIESTAT,TEXT=(*,LSTALIT),COL=*                            
Type     LKREQ F,008,,CHAR,OLEN=L'LIETYPE,TEXT=(*,LTYPLIT),COL=*                
Text     LKREQ F,009,,CHAR,OLEN=L'LIEDATA,TEXT=(*,LDATALIT),COL=*               
Time     LKREQ F,010,,CHAR,OLEN=L'LIETIME,TEXT=(*,LTIMLIT),COL=*                
StartDat LKREQ F,011,,CDAT,OLEN=L'LIESTD,TEXT=(*,LSTDLIT),COL=*                 
EndDat   LKREQ F,012,,CDAT,OLEN=L'LIEETD,TEXT=(*,LETDLIT),COL=*,ARRAY=E         
                                                                                
         LKREQ E                                                                
                                                                                
LRECALIT DC    C'Record Action'                                                 
LMEDLIT  DC    C'Media Code'                                                    
LCLILIT  DC    C'Client Code'                                                   
LPRDLIT  DC    C'Product'                                                       
LESTLIT  DC    C'Estimate'                                                      
LNETLIT  DC    C'Network'                                                       
LSTALIT  DC    C'Status'                                                        
LTYPLIT  DC    C'Type'                                                          
LDATALIT DC    C'Data'                                                          
LTIMLIT  DC    C'Time (Y type only)'                                            
LSTDLIT  DC    C'Start Date'                                                    
LETDLIT  DC    C'End Date'                                                      
         EJECT                                                                  
***********************************************************************         
* Upload - BA Rules record                                            *         
***********************************************************************         
                                                                                
BRUUPL   SR    RE,RE               Set media                                    
         ICM   RE,7,QAMED                                                       
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
                                                                                
B        USING BRULESD,IOKEY                                                    
         XC    B.BRULKEY,B.BRULKEY                                              
         MVI   B.BRULKTYP,BRULKTYQ                                              
         MVI   B.BRULKSTY,BRULKSTQ                                              
         MVC   B.BRULAGY,QMEDX                                                  
                                                                                
         OC    QBRUCLI,QBRUCLI     Test agency level                            
         JZ    BRUUPL02                                                         
         CLC   QBRUCLI,=C'***'                                                  
         JE    BRUUPL02                                                         
         GOTOR VCLPACK,DMCB,QBRUCLI,B.BRULCLT                                   
         DROP  B                                                                
                                                                                
BRUUPL02 CLI   QRECACTN,QRA_ADDQ   Test action add                              
         JNE   BRUUPL04                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOSPTDIR+B#BRUREC'                   
         CLC   IOKEY(L'BRULKEY),IOKEYSAV   Test record already exists           
         JE    RAEERR                                                           
                                                                                
         L     R0,ABRUREC          Build BA rules record from scratch           
         LHI   R1,2000                                                          
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     BRUUPL06                                                         
                                                                                
BRUUPL04 GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOSPTDIR+B#BRUREC'                   
         CLC   IOKEY(L'BRULKEY),IOKEYSAV                                        
         JNE   RDNEERR                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOSPTFIL+B#BRUREC'                   
         JNL   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   QRECACTN,QRA_DELQ   Test action delete                           
         JE    BRUDEL                                                           
         CLI   QRECACTN,QRA_RESQ   Test action restore                          
         JE    BRURES                                                           
         J     BRUUPL06                                                         
                                                                                
BRUDEL   OI    IOKEY+13,X'80'      Delete key                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOBWRITE+IOSPTDIR'                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,ABRUREC          Delete record                                
         OI    15(R2),X'80'                                                     
         J     BRUUPPUT                                                         
                                                                                
BRURES   NI    IOKEY+13,X'FF'-X'80'   Restore key                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBWRITE+IOSPTDIR'                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,ABRUREC          Restore record                               
         NI    15(R2),X'FF'-X'80'                                               
         J     BRUUPPUT                                                         
                                                                                
BRUUPL06 L     R2,ABRUREC                                                       
         USING BRULESD,R2                                                       
         XC    BRULKEY,BRULKEY                                                  
         MVI   BRULKTYP,BRULKTYQ                                                
         MVI   BRULKSTY,BRULKSTQ                                                
         MVC   BRULAGY,QMEDX                                                    
         MVI   BRULRLEN+1,SPTRECLN                                              
                                                                                
         OC    QBRUCLI,QBRUCLI     Test agency level                            
         JZ    BRUUPL08                                                         
         CLC   QBRUCLI,=C'***'                                                  
         JE    BRUUPL08                                                         
         GOTOR VCLPACK,DMCB,QBRUCLI,BRULCLT                                     
                                                                                
BRUUPL08 GOTOR VHELLO,DMCB,(C'D',SPTFIL),(X'01',ABRUREC)                        
         GOTOR VHELLO,DMCB,(C'D',SPTFIL),(X'02',ABRUREC)                        
         DROP  R2                                                               
                                                                                
         LA    R2,WORK                                                          
         USING BR01ID,R2                                                        
         MVI   BR01ID,BR01IDQ                                                   
         MVI   BR01LN,BR01LNQN                                                  
         GOTOR VDATCON,DMCB,(5,0),(2,BR01ACT)                                   
         MVC   BR01UAD,QBRUUAC                                                  
         MVC   BR01RSD,QBRURSD                                                  
         MVC   BR01DWP,QBRUDWP                                                  
         MVC   BR01TBL,QBRUTBL                                                  
         MVC   BR01TBU,QBRUTBU                                                  
         MVC   BR01GBL,QBRUGBL                                                  
         MVC   BR01GBU,QBRUGBU                                                  
         MVC   BR01WBL,QBRUWBL                                                  
         MVC   BR01WBU,QBRUWBU                                                  
         MVC   BR01IZU,QBRUIZU                                                  
         MVC   BR01NTI,QBRUNTI                                                  
         MVC   BR01CTI,QBRUCTI                                                  
         MVC   BR01MDS,QBRUMDS                                                  
         MVC   BR01NTP,QBRUNTP                                                  
         MVC   BR01PGP,QBRUPGP                                                  
         DROP  R2                                                               
                                                                                
         GOTOR VHELLO,DMCB,(C'P',SPTFIL),(X'01',ABRUREC),WORK,0                 
                                                                                
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING BDEMD,R2                                                         
         MVI   BDEMID,BDEMIDQ                                                   
         MVI   BDEMLN,BDEMLNQ                                                   
         MVI   BDEMNUM,1                                                        
         MVC   BDEMTWP,QBRU1DWP                                                 
         MVC   BDEMTDL,QBRU1TBL                                                 
         MVC   BDEMTDU,QBRU1TBU                                                 
         MVC   BDEMBGL,QBRU1GBL                                                 
         MVC   BDEMBGU,QBRU1GBU                                                 
         MVC   BDEMWBL,QBRU1WBL                                                 
         MVC   BDEMWBU,QBRU1WBU                                                 
                                                                                
         GOTOR VHELLO,DMCB,(C'P',SPTFIL),(X'02',ABRUREC),WORK,0                 
                                                                                
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         MVI   BDEMID,BDEMIDQ                                                   
         MVI   BDEMLN,BDEMLNQ                                                   
         MVI   BDEMNUM,2                                                        
         MVC   BDEMTWP,QBRU2DWP                                                 
         MVC   BDEMTDL,QBRU2TBL                                                 
         MVC   BDEMTDU,QBRU2TBU                                                 
         MVC   BDEMBGL,QBRU2GBL                                                 
         MVC   BDEMBGU,QBRU2GBU                                                 
         MVC   BDEMWBL,QBRU2WBL                                                 
         MVC   BDEMWBU,QBRU2WBU                                                 
                                                                                
         GOTOR VHELLO,DMCB,(C'P',SPTFIL),(X'02',ABRUREC),WORK,0                 
                                                                                
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         MVI   BDEMID,BDEMIDQ                                                   
         MVI   BDEMLN,BDEMLNQ                                                   
         MVI   BDEMNUM,3                                                        
         MVC   BDEMTWP,QBRU3DWP                                                 
         MVC   BDEMTDL,QBRU3TBL                                                 
         MVC   BDEMTDU,QBRU3TBU                                                 
         MVC   BDEMBGL,QBRU3GBL                                                 
         MVC   BDEMBGU,QBRU3GBU                                                 
         MVC   BDEMWBL,QBRU3WBL                                                 
         MVC   BDEMWBU,QBRU3WBU                                                 
         DROP  R2                                                               
                                                                                
         GOTOR VHELLO,DMCB,(C'P',SPTFIL),(X'02',ABRUREC),WORK,0                 
                                                                                
         CLI   QRECACTN,QRA_ADDQ   Test action add                              
         JNE   BRUUPPUT                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOSPTFIL+B#BRUREC'                     
         JE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
BRUUPPUT GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOSPTFIL+B#BRUREC'                     
         JE    EXITY                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Upload - Limit record                                               *         
***********************************************************************         
                                                                                
LIMUPL   SR    RE,RE               Set media                                    
         ICM   RE,7,QAMED                                                       
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
                                                                                
P        USING PRDRECD,IOKEY       Validate product code                        
         XC    IOKEY,IOKEY                                                      
         MVI   P.PLSTTYPE,PLSTTYPQ                                              
         MVI   P.PLSTSUB,PLSTSUBQ                                               
         MVC   P.PLSTAM,QMEDX                                                   
         GOTOR VCLPACK,DMCB,QLIMCLI,P.PLSTCLT                                   
         MVC   P.PLSTPRD,QLIMPRD                                                
         OC    P.PLSTPRD,SPACES                                                 
         CLC   P.PLSTPRD,=C'POL'                                                
         JNE   *+8                                                              
         OI    P.PLSTXFF,X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOSPTDIR+B#SPTREC'                      
         CLC   IOKEY(P.PLSTBPRD-P.PLSTTYPE),IOKEYSAV                            
         JNE   INVPRDER                                                         
                                                                                
E        USING ESTRECD,IOKEY       Validate estimate number                     
         XC    IOKEY,IOKEY                                                      
         MVC   E.EKEYAM,QMEDX                                                   
         GOTOR VCLPACK,DMCB,QLIMCLI,E.EKEYCLT                                   
         MVC   E.EKEYPRD,=C'POL'                                                
         MVC   E.EKEYEST,QLIMEST                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOSPTDIR+B#SPTREC'                      
         CLC   IOKEY(L'EKEY),IOKEYSAV                                           
         JNE   INVESTER                                                         
                                                                                
S        USING STARECD,IOKEY       Validate network                             
         CLC   =C'ALL',QLIMNET     Test all networks                            
         JE    LIMUPL02                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   S.STAKTYPE,STAKTYPQ                                              
         MVI   S.STAKMED,C'N'                                                   
         MVC   S.STAKCALL,QLIMNET                                               
         MVI   S.STAKCALL+4,C'N'                                                
         MVC   S.STAKAGY,LP_AGY                                                 
         MVC   S.STAKCLT,=C'000'                                                
         MVC   S.STAKFILL,=C'000'                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOSTAFIL+B#SPTREC'                      
         CLC   IOKEY(STAKEYLN),IOKEYSAV                                         
         JNE   INVNETER                                                         
                                                                                
L        USING PXCRECD,IOKEY                                                    
LIMUPL02 XC    L.PXCKEY,L.PXCKEY                                                
         MVI   L.PXCKTYPE,PXCKTYPQ                                              
         MVI   L.PXCKSUB,PXCKSUBQ                                               
         MVC   L.PXCKAGM,QMEDX                                                  
         GOTOR VCLPACK,DMCB,QLIMCLI,L.PXCKCLT                                   
         MVC   L.PXCKPRD,QLIMPRD                                                
         MVC   L.PXCKEST,QLIMEST                                                
         OI    L.PXCKSTAT,PXCKLIMQ                                              
                                                                                
         MVC   QLIMQSTA,=X'FFFFFF' Set network to all default                   
         CLC   =C'ALL',QLIMNET     Test all networks                            
         JE    LIMUPL04                                                         
                                                                                
         USING STAPACKD,WORK                                                    
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVI   STAPMED,C'N'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0001'   Set dummy market                             
         MVC   STAPQSTA,QLIMNET                                                 
         MVI   STAPQSTA+4,C'N'                                                  
         GOTOR VSTAPACK,STAPACKD                                                
         MVC   QLIMQSTA,STAPSTA    Set packed network                           
LIMUPL04 MVC   L.PXCKSTA,QLIMQSTA                                               
         DROP  L                                                                
                                                                                
         CLI   QRECACTN,QRA_ADDQ   Test action add                              
         JNE   LIMUPL06                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOSPTDIR+B#LIMREC'                   
         CLC   IOKEY(L'PXCKEY),IOKEYSAV   Test record already exists            
         JE    RAEERR                                                           
                                                                                
         L     R0,ALIMREC          Build Limit record from scratch              
         LHI   R1,4000                                                          
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     LIMUPL08                                                         
                                                                                
LIMUPL06 GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOSPTDIR+B#LIMREC'                   
         CLC   IOKEY(L'PXCKEY),IOKEYSAV                                         
         JNE   RDNEERR                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOSPTFIL+B#LIMREC'                   
         JNL   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   QRECACTN,QRA_DELQ   Test action delete                           
         JE    LIMDEL                                                           
         CLI   QRECACTN,QRA_RESQ   Test action restore                          
         JE    LIMRES                                                           
         J     LIMUPL08                                                         
                                                                                
LIMDEL   OI    IOKEY+13,X'80'      Delete key                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOBWRITE+IOSPTDIR'                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,ALIMREC          Delete record                                
         OI    15(R2),X'80'                                                     
         J     LIMUPPUT                                                         
                                                                                
LIMRES   NI    IOKEY+13,X'FF'-X'80'   Restore key                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBWRITE+IOSPTDIR'                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,ALIMREC          Restore record                               
         NI    15(R2),X'FF'-X'80'                                               
         J     LIMUPPUT                                                         
                                                                                
LIMUPL08 L     R2,ALIMREC                                                       
         USING PXCRECD,R2                                                       
         XC    PXCKEY,PXCKEY                                                    
         MVI   PXCKTYPE,PXCKTYPQ                                                
         MVI   PXCKSUB,PXCKSUBQ                                                 
         MVC   PXCKAGM,QMEDX                                                    
         GOTOR VCLPACK,DMCB,QLIMCLI,PXCKCLT                                     
         MVC   PXCKPRD,QLIMPRD                                                  
         MVC   PXCKEST,QLIMEST                                                  
         OI    PXCKSTAT,PXCKLIMQ                                                
         MVC   PXCKSTA,QLIMQSTA                                                 
         MVI   PXCLEN+1,SPTRECLN                                                
         DROP  R2                                                               
                                                                                
         GOTOR VHELLO,DMCB,(C'D',SPTFIL),(X'01',ALIMREC)                        
         GOTOR VHELLO,DMCB,(C'D',SPTFIL),(X'06',ALIMREC)                        
         GOTOR VHELLO,DMCB,(C'D',SPTFIL),(X'07',ALIMREC)                        
                                                                                
         LA    R2,WORK                                                          
         USING PXCEL01,R2                                                       
         MVI   PXCEL01,PXCEL01Q                                                 
         MVI   PXC01LEN,PXCEL01L                                                
         GOTOR VDATCON,DMCB,(5,0),(2,PXCACDAT)                                  
         DROP  R2                                                               
                                                                                
         GOTOR VHELLO,DMCB,(C'P',SPTFIL),(X'01',ALIMREC),WORK,0                 
                                                                                
         ICM   R3,15,I$LIEARY      Test any inclusion/exclusions                
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    R4,R4                                                            
         ICM   R4,3,LW_NUMN-LW_D(R3)                                            
         LA    R3,LW_DATA2-LW_D(R3)                                             
         USING LIED,R3                                                          
                                                                                
         MVI   LIEDATAL,L'LIEDATA                                               
         CLI   LIETYPE,LIETPGMQ    Test program                                 
         JE    LIMUPL14                                                         
                                                                                
LIMUPL10 SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVI   LIEDATAL,0                                                       
         LA    R1,L'LIEDATA                                                     
         LA    RF,LIEDATA                                                       
LIMUPL12 CLI   0(RF),C' '                                                       
         JE    LIMUPL14                                                         
         AHI   RF,1                                                             
         AHI   R0,1                                                             
         STC   R0,LIEDATAL         Set data input length                        
         JCT   R1,LIMUPL12                                                      
                                                                                
LIMUPL14 SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVI   LIETIMEL,0                                                       
         LA    R1,L'LIETIME                                                     
         LA    RF,LIETIME                                                       
LIMUPL16 CLI   0(RF),C' '                                                       
         JE    LIMUPL18                                                         
         AHI   RF,1                                                             
         AHI   R0,1                                                             
         STC   R0,LIETIMEL         Set time input length                        
         JCT   R1,LIMUPL16                                                      
                                                                                
LIMUPL18 LA    R2,WORK                                                          
         USING L07ELD,R2                                                        
         XC    WORK,WORK                                                        
         MVI   L07ELLN,L07ELL                                                   
         MVC   L07STDTE,LIESTD     Set start date                               
         MVC   L07ENDTE,LIEETD     Set end date                                 
                                                                                
         CLI   LIETYPE,LIETNETQ    Test network                                 
         JNE   LIMUPL20                                                         
         OI    L07STAT,L07STNET                                                 
         MVC   L07LIM,LIEDATA      Set network                                  
         J     LIMUPL28                                                         
                                                                                
LIMUPL20 CLI   LIETYPE,LIETPGMQ    Test program                                 
         JNE   LIMUPL22                                                         
         OI    L07STAT,L07STPRG                                                 
         MVC   L07LIM,LIEDATA      Set program                                  
         J     LIMUPL28                                                         
                                                                                
LIMUPL22 CLI   LIETYPE,LIETDAYQ    Test day                                     
         JNE   LIMUPL24                                                         
         OI    L07STAT,L07STDAY                                                 
                                                                                
         MVI   WORK1,0                                                          
         GOTOR VDAYVAL,DMCB,(LIEDATAL,LIEDATA),WORK1,WORK1+16                   
         CLI   WORK1,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   L07LIM(1),WORK1     Set day                                      
         J     LIMUPL28                                                         
                                                                                
LIMUPL24 CLI   LIETYPE,LIETTIMQ    Test time                                    
         JNE   LIMUPL26                                                         
         OI    L07STAT,L07STTIM                                                 
                                                                                
         GOTOR VTIMVAL,DMCB,(LIEDATAL,LIEDATA),WORK1                            
         CLI   DMCB,X'FF'                                                       
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   L07LIM(4),WORK1     Set time                                     
         J     LIMUPL28                                                         
                                                                                
LIMUPL26 CLI   LIETYPE,LIETDTMQ    Test day/time                                
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    L07STAT,L07STDTM                                                 
                                                                                
         XC    WORK1,WORK1                                                      
         GOTOR VDAYVAL,DMCB,(LIEDATAL,LIEDATA),WORK1,WORK1+16                   
         CLI   WORK1,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   L07LIM(1),WORK1     Set day                                      
                                                                                
         GOTOR VTIMVAL,DMCB,(LIETIMEL,LIETIME),WORK1                            
         CLI   DMCB,X'FF'                                                       
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   L07LIM+1(4),WORK1   Set time                                     
                                                                                
LIMUPL28 CLI   LIESTAT,LIEINCQ     Test inclusion                               
         JNE   LIMUPL30                                                         
         MVI   L07ELID,L06ELIDQ    Set inclusion                                
         GOTOR VHELLO,DMCB,(C'P',SPTFIL),(X'06',ALIMREC),WORK,0                 
         J     LIMUPL32                                                         
                                                                                
LIMUPL30 MVI   L07ELID,L07ELIDQ    Set exclusion                                
         GOTOR VHELLO,DMCB,(C'P',SPTFIL),(X'07',ALIMREC),WORK,0                 
LIMUPL32 LA    R3,LIEDL(R3)        Process next entry                           
         JCT   R4,LIMUPL10                                                      
         DROP  R2                                                               
                                                                                
         CLI   QRECACTN,QRA_ADDQ   Test action add                              
         JNE   LIMUPPUT                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOSPTFIL+B#LIMREC'                     
         JE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
LIMUPPUT GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOSPTFIL+B#LIMREC'                     
         JE    EXITY                                                            
         DC    H'0'                                                             
         EJECT                                                                  
                                                                                
SPTRECLN EQU   24                  Initial spot record length                   
                                                                                
RAEERR   MVI   LNKSEQ,1            Record already exists error                  
         MVI   LNKMAPN,0                                                        
         MVC   LNKMTXT,SPACES                                                   
         MVI   LNKMSGN,0                                                        
                                                                                
         MVC   LNKMSG,=C'Record already exists         '                        
         TM    IOKEY+13,X'80'      Test record deleted                          
         JZ    *+10                                                             
         MVC   LNKMSG,=C'Deleted record already exists '                        
         GOTOR PCREPLY                                                          
         J     EXITY                                                            
                                                                                
RDNEERR  MVI   LNKSEQ,1            Record does not exist error                  
         MVI   LNKMAPN,0                                                        
         MVC   LNKMTXT,SPACES                                                   
         MVI   LNKMSGN,0                                                        
         MVC   LNKMSG,=C'Record does not exist         '                        
         GOTOR PCREPLY                                                          
         J     EXITY                                                            
                                                                                
INVESTER MVI   LNKSEQ,1            Invalid estimate error                       
         MVI   LNKMAPN,0                                                        
         MVC   LNKMTXT,SPACES                                                   
         MVI   LNKMSGN,0                                                        
         MVC   LNKMSG,=C'Invalid estimate              '                        
         GOTOR PCREPLY                                                          
         J     EXITY                                                            
                                                                                
INVPRDER MVI   LNKSEQ,1            Invalid product error                        
         MVI   LNKMAPN,0                                                        
         MVC   LNKMTXT,SPACES                                                   
         MVI   LNKMSGN,0                                                        
         MVC   LNKMSG,=C'Invalid product               '                        
         GOTOR PCREPLY                                                          
         J     EXITY                                                            
                                                                                
INVNETER MVI   LNKSEQ,1            Invalid network error                        
         MVI   LNKMAPN,0                                                        
         MVC   LNKMTXT,SPACES                                                   
         MVI   LNKMSGN,0                                                        
         MVC   LNKMSG,=C'Invalid network               '                        
         GOTOR PCREPLY                                                          
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* Send reply to PC                                                    *         
***********************************************************************         
PCREPLY  NTR1  BASE=*,LABEL=*                                                   
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',336)                    
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',1),            *        
               ('LD_HEXDQ',LNKSEQ),(L'LNKSEQ,0)                                 
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',2),            *        
               ('LD_HEXDQ',LNKMAPN),(L'LNKMAPN,0)                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',3),            *        
               ('LD_CHARQ',LNKMTXT),(L'LNKMTXT,0)                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',4),            *        
               ('LD_HEXDQ',LNKMSGN),(L'LNKMSGN,0)                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',5),            *        
               ('LD_CHARQ',LNKMSG),(L'LNKMSG,0)                                 
                                                                                
         XC    LNKVALS(LNKVALSL),LNKVALS                                        
         MVC   LNKMSG,SPACES       Reset PC reply message                       
PUTERRX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Print to server log                                                 *         
***********************************************************************         
*&&DO                                                                           
PRTLOG   NTR1  LABEL=*                                                          
         MVC   IOP,SPACES                                                       
         MVC   IOP(3),0(R1)                                                     
         GOTOR VHEXOUT,DMCB,AUNTREC,IOP+5,20                                    
         GOTOR VPRINT,DMCB,IOP-1,=C'BL01'                                       
         J     EXITY                                                            
*&&                                                                             
                                                                                
REQEND   LKREQ X                                                                
         EJECT                                                                  
EXITN    LHI   RE,0                Set condition code to not equal              
         J     EXITCC                                                           
EXITY    LHI   RE,1                Set condition code to equal                  
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                   General exit point                           
         EJECT                                                                  
                                                                                
FACS     DS    0XL(RFACTABL)       ** System facilities **                      
         DC    AL1(RFACEOTQ)                                                    
                                                                                
GLOBALS  DS    0D                  ** Global literals **                        
         LTORG ,                                                                
                                                                                
PZERO    DC    P'0'                                                             
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
DMCOMMIT DC    C'COMMIT  '                                                      
STAFIL   DC    C'STATION'                                                       
                                                                                
FILES    DS    0X                  ** File info **                              
         DC    C'SPOT   '          System name for open                         
         DC    C'N'                                                             
SPTDIR   DC    C'SPTDIR '                                                       
         DC    C'N'                                                             
SPTFIL   DC    C'SPTFILE'                                                       
         DC    C'N'                                                             
XSPDIR   DC    C'XSPDIR '                                                       
         DC    C'N'                                                             
XSPFIL   DC    C'XSPFIL '                                                       
         DC    C'U'                                                             
UNTDIR   DC    C'UNTDIR '                                                       
         DC    C'U'                                                             
UNTFIL   DC    C'UNTFIL '                                                       
         DC    C'N'                                                             
         DC    C'STAFILE'                                                       
         DC    C'U'                                                             
         DC    C'RECV   '                                                       
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'N'                                                             
GENDIR   DC    C'GENDIR '                                                       
         DC    C'N'                                                             
GENFIL   DC    C'GENFIL '                                                       
         DC    C'X'                                                             
         EJECT                                                                  
SAVED    DSECT ,                   ** Saved storage **                          
                                                                                
AMASTC   DS    A                   A(MASTC)                                     
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER calling mode                   
VPRINT   DS    A                   A(Print routine)                             
LINKIO   DS    A                   A(LINKIO)                                    
ALIOB    DS    A                   A(LINKIO control block)                      
                                                                                
RUNINDS  DS    X                   Local indicators                             
RUNINIT  EQU   X'01'               1st time building disk address list          
                                                                                
MQSVR    DS    CL(L'LP_MQSVR)      External service call                        
                                                                                
QVALUES  DS    0F                  ** Request values **                         
                                                                                
QAGY     DS    CL(L'LP_AGY)                                                     
QC_TOKEN DS    CL8                                                              
QP_TOKEN DS    CL8                                                              
                                                                                
QRECACTN DS    X                   Record action                                
QRA_ADDQ EQU   C'A'                Add                                          
QRA_CHGQ EQU   C'C'                Change                                       
QRA_DELQ EQU   C'D'                Delete                                       
QRA_RESQ EQU   C'R'                Restore                                      
                                                                                
QMEDNDX  DS    0XL4                Media index                                  
QMEDIND  DS    X                                                                
QAMED    DS    AL3                                                              
                                                                                
* BA Rules record request fields                                                
                                                                                
QBRUCLI  DS    CL3                 Client code                                  
QBRUUAC  DS    C                   Use assigned cost                            
QBRURSD  DS    C                   Round split dollars                          
QBRUIZU  DS    C                   Include $0 units                             
QBRUDWP  DS    XL(L'BR01DWP)       Dollar weight %                              
QBRUTBL  DS    XL(L'BR01TBL)       Total budget lower                           
QBRUTBU  DS    XL(L'BR01TBU)       Total budget upper                           
QBRUGBL  DS    XL(L'BR01GBL)       Goal budget lower                            
QBRUGBU  DS    XL(L'BR01GBU)       Goal budget upper                            
QBRUWBL  DS    XL(L'BR01WBL)       Weekly budget lower                          
QBRUWBU  DS    XL(L'BR01WBU)       Weekly budget upper                          
QBRUNTP  DS    XL(L'BR01NTP)       Network %                                    
QBRUPGP  DS    XL(L'BR01PGP)       Program %                                    
                                                                                
QBRUNTI  DS    XL(L'BR01NTI)       Network seperation minutes                   
QBRUCTI  DS    XL(L'BR01CTI)       Cable seperation minutes                     
QBRUMDS  DS    XL(L'BR01MDS)       Maximum double spotting allowed              
                                                                                
QBRU1DWP DS    XL(L'BR01DWP)       Demo weight %                                
QBRU1TBL DS    XL(L'BR01TBL)       Total budget lower                           
QBRU1TBU DS    XL(L'BR01TBU)       Total budget upper                           
QBRU1GBL DS    XL(L'BR01GBL)       Goal budget lower                            
QBRU1GBU DS    XL(L'BR01GBU)       Goal budget upper                            
QBRU1WBL DS    XL(L'BR01WBL)       Weekly budget lower                          
QBRU1WBU DS    XL(L'BR01WBU)       Weekly budget upper                          
QBRU2DWP DS    XL(L'BR01DWP)       Demo weight %                                
QBRU2TBL DS    XL(L'BR01TBL)       Total budget lower                           
QBRU2TBU DS    XL(L'BR01TBU)       Total budget upper                           
QBRU2GBL DS    XL(L'BR01GBL)       Goal budget lower                            
QBRU2GBU DS    XL(L'BR01GBU)       Goal budget upper                            
QBRU2WBL DS    XL(L'BR01WBL)       Weekly budget lower                          
QBRU2WBU DS    XL(L'BR01WBU)       Weekly budget upper                          
QBRU3DWP DS    XL(L'BR01DWP)       Demo weight %                                
QBRU3TBL DS    XL(L'BR01TBL)       Total budget lower                           
QBRU3TBU DS    XL(L'BR01TBU)       Total budget upper                           
QBRU3GBL DS    XL(L'BR01GBL)       Goal budget lower                            
QBRU3GBU DS    XL(L'BR01GBU)       Goal budget upper                            
QBRU3WBL DS    XL(L'BR01WBL)       Weekly budget lower                          
QBRU3WBU DS    XL(L'BR01WBU)       Weekly budget upper                          
                                                                                
* Limit record request fields                                                   
                                                                                
QLIMCLI  DS    CL3                 Client code                                  
QLIMPRD  DS    CL(L'PXCKPRD)       Product                                      
QLIMEST  DS    XL(L'PXCKEST)       Estimate                                     
QLIMNET  DS    CL4                 Network                                      
QLIMQSTA DS    XL(L'PXCKSTA)       Packed network                               
                                                                                
QVALUEL  EQU   *-QVALUES                                                        
                                                                                
LNKVALS  DS    0F                  LNKIO values                                 
LNKSEQ   DS    X                   Sequence #                                   
LNKMAPN  DS    X                   Upload mapcode #                             
LNKMTXT  DS    CL30                Upload mapcode text                          
LNKMSGN  DS    X                   Message #                                    
LNKMSG   DS    CL30                PC reply message                             
LNKVALSL EQU   *-LNKVALS                                                        
                                                                                
I$LIEARY DS    A                   A(Limit incl/excl request array)             
                                                                                
LIEDATAL DS    X                   Data input length                            
LIETIMEL DS    X                   Time input length                            
                                                                                
IOP      DS    CL132               Print line                                   
                                                                                
* Limit record inclusion/exclusions input array layout                          
                                                                                
LIED     DSECT                                                                  
LIESTAT  DS    C                   Status                                       
LIEINCQ  EQU   C'+'                Inclusion                                    
LIEEXCQ  EQU   C'-'                Exclusion                                    
LIETYPE  DS    C                   Type                                         
LIETNETQ EQU   C'N'                Net                                          
LIETDAYQ EQU   C'D'                Day                                          
LIETTIMQ EQU   C'T'                Time                                         
LIETPGMQ EQU   C'P'                Program                                      
LIETDTMQ EQU   C'Y'                Day/Time                                     
LIEDATA  DS    CL(L'L06LIM)        Data                                         
LIETIME  DS    CL(L'L06LIM)        Time (for status Y only)                     
LIESTD   DS    XL(L'L06STDTE)      Start Date                                   
LIEETD   DS    XL(L'L06ENDTE)      End date                                     
LIEDL    EQU   *-LIESTAT                                                        
                                                                                
         EJECT                                                                  
* Other included books                                                          
       ++INCLUDE NENAVWORKD                                                     
       ++INCLUDE SPGENBARU                                                      
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE DDLINKIOD                                                      
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPSTAPACKD                                                     
STAPAKL  EQU   *-STAPACKD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NENAV17   12/04/12'                                      
         END                                                                    
