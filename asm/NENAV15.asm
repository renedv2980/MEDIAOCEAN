*          DATA SET NENAV15    AT LEVEL 014 AS OF 11/20/17                      
*PHASE T31815A                                                                  
NENAV15  TITLE '- Network Navigator - Client/Product CFM Upload'                
         PRINT NOGEN                                                            
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,WORKERKEY=NEAL,SEGMENT=Y,LINKIO=Y,               +        
               FACS=FACS,APPEND=Y,REQUEST=*,CODE=CODE,IDF=Y,           +        
               SYSPHASE=SYSPHASE,SYSTEM=NETSYSQ,FILES=FILES,           +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED),RLEN=2500,         +        
               LOADFACSOFF=Y                                                    
                                                                                
NETCFMU  EQU   C'I'                Locally defined server class for now         
                                                                                
B#PRDREC EQU   B#IOA3              I/O 3 used for product reading               
APRDREC  EQU   LP_BLKS+((B#PRDREC-1)*L'LP_BLKS),,C'A'                           
B#CLIREC EQU   B#IOA6              I/O 6 used for client reading                
ACLIREC  EQU   LP_BLKS+((B#CLIREC-1)*L'LP_BLKS),,C'A'                           
         EJECT                                                                  
CODE     NMOD1 0,**NN15**                                                       
                                                                                
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
         USING WORKD,R9            R9=A(Global w/s)                             
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
         MVC   LP_AUIR1,0(R1)      Set A(support overlay 1)                     
         MVC   AROUTS1,0(R1)                                                    
         GOTOR (RF),(R1),('P#ROUTS2',0),0,0                                     
         MVC   LP_AUIR2,0(R1)      Set A(support overlay 2)                     
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
* Run a download request                                              *         
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
* CFM upload - Client record                                          *         
***********************************************************************         
                                                                                
UPLCLI   LKREQ H,M#UPLCLI,NEXTREQ=UPLPRD,ROUTINE=CLIUPL,NEWREC=Y                
                                                                                
Media    LKREQ F,001,(I,B#SAVED,QMEDNDX),(U,#VALMED,$VALMED),          +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=(*,CMEDLIT),COL=*              
CliCode  LKREQ F,002,(D,B#SAVED,QCLICOD),CHAR,TEXT=(*,CCODELIT),COL=*           
Name     LKREQ F,003,(D,B#SAVED,QCLINAME),CHAR,TEXT=(*,CNAMELIT),COL=*          
Office   LKREQ F,004,(D,B#SAVED,QCLIOFF),CHAR,TEXT=(*,COFFLIT),COL=*            
AccOff   LKREQ F,005,(D,B#SAVED,QCLIAOFF),CHAR,TEXT=(*,CAOFFLIT),COL=*          
AccAgy   LKREQ F,006,(D,B#SAVED,QCLIACC),CHAR,TEXT=(*,CACCLIT),COL=*            
                                                                                
Overflow LKREQ F,020,(D,B#SAVED,QCLIOVFL),CHAR,TEXT=(*,COVFLLIT),COL=*          
                                                                                
Traffic  LKREQ F,030,(D,B#SAVED,QCLITRF),CHAR,TEXT=(*,CTRFLIT),COL=*            
InterFac LKREQ F,031,(D,B#SAVED,QCLIINTF),CHAR,TEXT=(*,CINTFLIT),COL=*          
CorpPrd  LKREQ F,032,(D,B#SAVED,QCLICORP),CHAR,TEXT=(*,CCORPLIT),COL=*          
                                                                                
Frozen   LKREQ F,044,(D,B#SAVED,QCLIFRZN),CHAR,TEXT=(*,CFRZNLIT),COL=*          
                                                                                
Cost2    LKREQ F,053,(D,B#SAVED,QCLICOS2),CHAR,TEXT=(*,CCOS2LIT),COL=*          
Time     LKREQ F,055,(D,B#SAVED,QCLITIME),CHAR,TEXT=(*,CTIMELIT),COL=*          
TimeInt  LKREQ F,056,(D,B#SAVED,QCLITINT),CHAR,TEXT=(*,CTINTLIT),COL=*          
                                                                                
Midas    LKREQ F,062,(D,B#SAVED,QCLIMIDS),CHAR,TEXT=(*,CMIDSLIT),COL=*          
Ucomm    LKREQ F,065,(D,B#SAVED,QCLIUCOM),CHAR,TEXT=(*,CUCOMLIT),COL=*          
Ecost    LKREQ F,067,(D,B#SAVED,QCLIECOS),CHAR,TEXT=(*,CECOSLIT),COL=*          
                                                                                
Cost2    LKREQ F,070,(D,B#SAVED,QCLICOSF),(R,VALCOS2F),                *        
               TEXT=(*,CCOSFLIT),COL=*                                          
BillEst  LKREQ F,071,(D,B#SAVED,QCLIBILL),CHAR,TEXT=(*,CBILLLIT),COL=*          
AAN      LKREQ F,072,(D,B#SAVED,QCLIAAN),CHAR,TEXT=(*,CAANLIT),COL=*            
RateCntr LKREQ F,073,(D,B#SAVED,QCLIRCTL),CHAR,TEXT=(*,CRCTLLIT),COL=*          
RateCov  LKREQ F,074,(D,B#SAVED,QCLIRCOV),CHAR,TEXT=(*,CRCOVLIT),COL=*          
EstFilt? LKREQ F,075,(D,B#SAVED,QCLIESTF),CHAR,TEXT=(*,CESTFLIT),COL=*          
Lock     LKREQ F,076,(D,B#SAVED,QCLILOCK),CHAR,TEXT=(*,CLOCKLIT),COL=*          
IDTitle  LKREQ F,077,(D,B#SAVED,QCLIIDTI),CHAR,TEXT=(*,CIDTILIT),COL=*          
                                                                                
PU1desc  LKREQ F,090,(D,B#SAVED,QCLIP1D),CHAR,TEXT=(*,CP1DLIT),COL=*            
PU1type  LKREQ F,091,(D,B#SAVED,QCLIP1T),CHAR,TEXT=(*,CP1TLIT),COL=*            
PU1len   LKREQ F,092,(D,B#SAVED,QCLIP1L),LBIN,TEXT=(*,CP1LLIT),COL=*            
PU1req   LKREQ F,093,(D,B#SAVED,QCLIP1R),CHAR,TEXT=(*,CP1RLIT),COL=*            
PU1A2    LKREQ F,094,(D,B#SAVED,QCLIP1A2),CHAR,TEXT=(*,CP1A2LIT),COL=*          
PU1ibill LKREQ F,095,(D,B#SAVED,QCLIP1IB),CHAR,TEXT=(*,CP1IBLIT),COL=*          
PU1sbill LKREQ F,096,(D,B#SAVED,QCLIP1SB),CHAR,TEXT=(*,CP1SBLIT),COL=*          
PU1MX    LKREQ F,097,(D,B#SAVED,QCLIP1MX),CHAR,TEXT=(*,CP1MXLIT),COL=*          
PU1cbill LKREQ F,098,(D,B#SAVED,QCLIP1CB),CHAR,TEXT=(*,CP1CBLIT),COL=*          
PU1fbill LKREQ F,099,(D,B#SAVED,QCLIP1FB),CHAR,TEXT=(*,CP1FBLIT),COL=*          
PU1hbill LKREQ F,100,(D,B#SAVED,QCLIP1HB),CHAR,TEXT=(*,CP1HBLIT),COL=*          
                                                                                
PU2desc  LKREQ F,110,(D,B#SAVED,QCLIP2D),CHAR,TEXT=(*,CP2DLIT),COL=*            
PU2type  LKREQ F,111,(D,B#SAVED,QCLIP2T),CHAR,TEXT=(*,CP2TLIT),COL=*            
PU2len   LKREQ F,112,(D,B#SAVED,QCLIP2L),LBIN,TEXT=(*,CP2LLIT),COL=*            
PU2req   LKREQ F,113,(D,B#SAVED,QCLIP2R),CHAR,TEXT=(*,CP2RLIT),COL=*            
PU2A2    LKREQ F,114,(D,B#SAVED,QCLIP2A2),CHAR,TEXT=(*,CP2A2LIT),COL=*          
PU2ibill LKREQ F,115,(D,B#SAVED,QCLIP2IB),CHAR,TEXT=(*,CP2IBLIT),COL=*          
PU2sbill LKREQ F,116,(D,B#SAVED,QCLIP2SB),CHAR,TEXT=(*,CP2SBLIT),COL=*          
PU2MX    LKREQ F,117,(D,B#SAVED,QCLIP2MX),CHAR,TEXT=(*,CP2MXLIT),COL=*          
PU2cbill LKREQ F,118,(D,B#SAVED,QCLIP2CB),CHAR,TEXT=(*,CP2CBLIT),COL=*          
PU2fbill LKREQ F,119,(D,B#SAVED,QCLIP2FB),CHAR,TEXT=(*,CP2FBLIT),COL=*          
PU2hbill LKREQ F,120,(D,B#SAVED,QCLIP2HB),CHAR,TEXT=(*,CP2HBLIT),COL=*          
                                                                                
EU1desc  LKREQ F,130,(D,B#SAVED,QCLIE1D),CHAR,TEXT=(*,CE1DLIT),COL=*            
EU1type  LKREQ F,131,(D,B#SAVED,QCLIE1T),CHAR,TEXT=(*,CE1TLIT),COL=*            
EU1len   LKREQ F,132,(D,B#SAVED,QCLIE1L),LBIN,TEXT=(*,CE1LLIT),COL=*            
EU1req   LKREQ F,133,(D,B#SAVED,QCLIE1R),CHAR,TEXT=(*,CE1RLIT),COL=*            
EU1A2    LKREQ F,134,(D,B#SAVED,QCLIE1A2),CHAR,TEXT=(*,CE1A2LIT),COL=*          
EU1ibill LKREQ F,135,(D,B#SAVED,QCLIE1IB),CHAR,TEXT=(*,CE1IBLIT),COL=*          
EU1sbill LKREQ F,136,(D,B#SAVED,QCLIE1SB),CHAR,TEXT=(*,CE1SBLIT),COL=*          
EU1MX    LKREQ F,137,(D,B#SAVED,QCLIE1MX),CHAR,TEXT=(*,CE1MXLIT),COL=*          
EU1cbill LKREQ F,138,(D,B#SAVED,QCLIE1CB),CHAR,TEXT=(*,CE1CBLIT),COL=*          
EU1fbill LKREQ F,139,(D,B#SAVED,QCLIE1FB),CHAR,TEXT=(*,CE1FBLIT),COL=*          
EU1hbill LKREQ F,140,(D,B#SAVED,QCLIE1HB),CHAR,TEXT=(*,CE1HBLIT),COL=*          
                                                                                
EU1F2X80 LKREQ F,141,(D,B#SAVED,QCEU1FG2),MB80,TEXT=(*,CEU1F2XX),COL=*          
EU1F2X40 LKREQ F,142,(D,B#SAVED,QCEU1FG2),MB40,TEXT=(*,CEU1F2XX),COL=*          
EU1F2X20 LKREQ F,143,(D,B#SAVED,QCEU1FG2),MB20,TEXT=(*,CEU1F2XX),COL=*          
EU1F2X10 LKREQ F,144,(D,B#SAVED,QCEU1FG2),MB10,TEXT=(*,CEU1F2XX),COL=*          
EU1F2X08 LKREQ F,145,(D,B#SAVED,QCEU1FG2),MB08,TEXT=(*,CEU1F2XX),COL=*          
EU1F2X04 LKREQ F,146,(D,B#SAVED,QCEU1FG2),MB04,TEXT=(*,CEU1F2XX),COL=*          
EU1F2X02 LKREQ F,147,(D,B#SAVED,QCEU1FG2),MB02,TEXT=(*,CEU1F2XX),COL=*          
EU1F2X01 LKREQ F,148,(D,B#SAVED,QCEU1FG2),MB01,TEXT=(*,CEU1F201),COL=*          
                                                                                
EU2desc  LKREQ F,150,(D,B#SAVED,QCLIE2D),CHAR,TEXT=(*,CE2DLIT),COL=*            
EU2type  LKREQ F,151,(D,B#SAVED,QCLIE2T),CHAR,TEXT=(*,CE2TLIT),COL=*            
EU2len   LKREQ F,152,(D,B#SAVED,QCLIE2L),LBIN,TEXT=(*,CE2LLIT),COL=*            
EU2req   LKREQ F,153,(D,B#SAVED,QCLIE2R),CHAR,TEXT=(*,CE2RLIT),COL=*            
EU2A2    LKREQ F,154,(D,B#SAVED,QCLIE2A2),CHAR,TEXT=(*,CE2A2LIT),COL=*          
EU2ibill LKREQ F,155,(D,B#SAVED,QCLIE2IB),CHAR,TEXT=(*,CE2IBLIT),COL=*          
EU2sbill LKREQ F,156,(D,B#SAVED,QCLIE2SB),CHAR,TEXT=(*,CE2SBLIT),COL=*          
EU2MX    LKREQ F,157,(D,B#SAVED,QCLIE2MX),CHAR,TEXT=(*,CE2MXLIT),COL=*          
EU2cbill LKREQ F,158,(D,B#SAVED,QCLIE2CB),CHAR,TEXT=(*,CE2CBLIT),COL=*          
EU2fbill LKREQ F,159,(D,B#SAVED,QCLIE2FB),CHAR,TEXT=(*,CE2FBLIT),COL=*          
EU2hbill LKREQ F,160,(D,B#SAVED,QCLIE2HB),CHAR,TEXT=(*,CE2HBLIT),COL=*          
                                                                                
EU2F2X80 LKREQ F,161,(D,B#SAVED,QCEU2FG2),MB80,TEXT=(*,CEU2F2XX),COL=*          
EU2F2X40 LKREQ F,162,(D,B#SAVED,QCEU2FG2),MB40,TEXT=(*,CEU2F2XX),COL=*          
EU2F2X20 LKREQ F,163,(D,B#SAVED,QCEU2FG2),MB20,TEXT=(*,CEU2F2XX),COL=*          
EU2F2X10 LKREQ F,164,(D,B#SAVED,QCEU2FG2),MB10,TEXT=(*,CEU2F2XX),COL=*          
EU2F2X08 LKREQ F,165,(D,B#SAVED,QCEU2FG2),MB08,TEXT=(*,CEU2F2XX),COL=*          
EU2F2X04 LKREQ F,166,(D,B#SAVED,QCEU2FG2),MB04,TEXT=(*,CEU2F2XX),COL=*          
EU2F2X02 LKREQ F,167,(D,B#SAVED,QCEU2FG2),MB02,TEXT=(*,CEU2F2XX),COL=*          
EU2F2X01 LKREQ F,168,(D,B#SAVED,QCEU2FG2),MB01,TEXT=(*,CEU2F201),COL=*          
                                                                                
LimitAcc LKREQ F,170,(D,B#SAVED,QCLILIMA),(R,VALACCS),                 +        
               TEXT=(*,CLIMALIT),COL=*                                          
FRunSub  LKREQ F,171,(D,B#SAVED,QCLIFRSB),LBIN,TEXT=(*,CFRSBLIT),COL=*          
RotStDy  LKREQ F,172,(D,B#SAVED,QCLISCJR),CHAR,TEXT=(*,CROSDLIT),COL=*          
                                                                                
C_Token  LKREQ F,300,(D,B#SAVED,QC_TOKEN),CHAR,TEXT=(*,CTOKNLIT),COL=*          
CRecAct  LKREQ F,350,(D,B#SAVED,QRECACTN),CHAR,TEXT=(*,RECACLIT),COL=*          
                                                                                
         LKREQ E                                                                
                                                                                
CMEDLIT  DC    C'Media Code'                                                    
CCODELIT DC    C'Client Code'                                                   
CNAMELIT DC    C'Name'                                                          
COFFLIT  DC    C'Office Number'                                                 
CAOFFLIT DC    C'Acc Office Code'                                               
CACCLIT  DC    C'Acc Agency Override'                                           
                                                                                
COVFLLIT DC    C'Overflow Products?'                                            
                                                                                
CTRFLIT  DC    C'Traffic Office Code'                                           
CINTFLIT DC    C'Client Interface Code'                                         
CCORPLIT DC    C'Corporate Product'                                             
                                                                                
CFRZNLIT DC    C'Frozen?'                                                       
                                                                                
CCOS2LIT DC    C'Cost2 Applies to Integration?'                                 
CTIMELIT DC    C'Time?'                                                         
CTINTLIT DC    C'Time and Integration?'                                         
                                                                                
CMIDSLIT DC    C'Midas Barter Client'                                           
CUCOMLIT DC    C'UCOM Bill Control'                                             
CECOSLIT DC    C'Run ECOST billing'                                             
                                                                                
CCOSFLIT DC    C'Cost2 Factor'                                                  
CBILLLIT DC    C'Bill Estimate Control'                                         
CAANLIT  DC    C'Print Client Code AAN'                                         
CRCTLLIT DC    C'Client Rate Control'                                           
CRCOVLIT DC    C'Client Rate Coverage'                                          
CESTFLIT DC    C'Estimate Filters Required?'                                    
CLOCKLIT DC    C'Client Lock'                                                   
CIDTILIT DC    C'ID Title'                                                      
                                                                                
CP1DLIT  DC    C'Product user field description 1'                              
CP1TLIT  DC    C'User type (A/C/N)'                                             
CP1LLIT  DC    C'User length (max=32)'                                          
CP1RLIT  DC    C'Required'                                                      
CP1A2LIT DC    C'Show on A2'                                                    
CP1IBLIT DC    C'(NET) integrated bills'                                        
CP1SBLIT DC    C'(SPOT) show on bills'                                          
CP1MXLIT DC    C'Transer on MX'                                                 
CP1CBLIT DC    C'(NET) spec charge bills'                                       
CP1FBLIT DC    C'Print in front of bills'                                       
CP1HBLIT DC    C'Print in headlines of bills'                                   
                                                                                
CP2DLIT  DC    C'Product user field description 2'                              
CP2TLIT  DC    C'User type (A/C/N)'                                             
CP2LLIT  DC    C'User length (max=32)'                                          
CP2RLIT  DC    C'Required'                                                      
CP2A2LIT DC    C'Show on A2'                                                    
CP2IBLIT DC    C'(NET) integrated bills'                                        
CP2SBLIT DC    C'(SPOT) show on bills'                                          
CP2MXLIT DC    C'Transer on MX'                                                 
CP2CBLIT DC    C'(NET) spec charge bills'                                       
CP2FBLIT DC    C'Print in front of bills'                                       
CP2HBLIT DC    C'Print in headlines of bills'                                   
                                                                                
CE1DLIT  DC    C'Estimate user field description 1'                             
CE1TLIT  DC    C'User type (A/C/N)'                                             
CE1LLIT  DC    C'User length (max=32)'                                          
CE1RLIT  DC    C'Required'                                                      
CE1A2LIT DC    C'Show on A2'                                                    
CE1IBLIT DC    C'(NET) integrated bills'                                        
CE1SBLIT DC    C'(SPOT) show on bills'                                          
CE1MXLIT DC    C'Transer on MX'                                                 
CE1CBLIT DC    C'(NET) spec charge bills'                                       
CE1FBLIT DC    C'Print in front of bills'                                       
CE1HBLIT DC    C'Print in headlines of bills'                                   
CEU1F201 DC    C'EU1F2 - Required on billing'                                   
CEU1F2XX DC    C'EU1F2 - FREE'                                                  
                                                                                
CE2DLIT  DC    C'Estimate user field description 2'                             
CE2TLIT  DC    C'User type (A/C/N)'                                             
CE2LLIT  DC    C'User length (max=32)'                                          
CE2RLIT  DC    C'Required'                                                      
CE2A2LIT DC    C'Show on A2'                                                    
CE2IBLIT DC    C'(NET) integrated bills'                                        
CE2SBLIT DC    C'(SPOT) show on bills'                                          
CE2MXLIT DC    C'Transer on MX'                                                 
CE2CBLIT DC    C'(NET) spec charge bills'                                       
CE2FBLIT DC    C'Print in front of bills'                                       
CE2HBLIT DC    C'Print in headlines of bills'                                   
CEU2F201 DC    C'EU2F2 - Required on billing'                                   
CEU2F2XX DC    C'EU2F2 - FREE'                                                  
                                                                                
CLIMALIT DC    C'Limited Access Code'                                           
CFRSBLIT DC    C'FrontRunner Sub Line'                                          
CROSDLIT DC    C'Rotation Start Day'                                            
                                                                                
CTOKNLIT DC    C'Token'                                                         
RECACLIT DC    C'Record Action'                                                 
                                                                                
***********************************************************************         
* PROCESS COST2 FACTOR INPUT                                                    
***********************************************************************         
VALCOS2F LM    R2,R4,LP_AINP                                                    
         XC    0(4,R4),0(R4)                                                    
         CHI   R3,0                Test any input                               
         JE    EXITY                                                            
                                                                                
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         PACK  DUB,0(0,R2)                                                      
         EX    R3,0(RE)                                                         
         CVB   R1,DUB                                                           
         ST    R1,0(R4)                                                         
                                                                                
         OC    0(4,R4),0(R4)       Test 0.0                                     
         JNZ   EXITY                                                            
         OI    0(R4),X'80'         Set 0.0 bit                                  
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS LIMIT ACCESS INPUT (COMMA SEPARATED)                                  
***********************************************************************         
VALACCS  DS    0H                                                               
         LM    R2,R4,LP_AINP                                                    
         XC    0(L'CACCESS,R4),0(R4)                                            
         CHI   R3,0                ANY INPUT?                                   
         JE    VACCSX              NONE.  NULLS = NO LIMIT ACCESS               
*                                                                               
         MVC   0(1,R4),0(R2)       COPY THE FIRST BYTE OF LIMIT ACCESS          
         AHI   R3,-2                                                            
         JM    VACCSX                                                           
         MVC   1(1,R4),2(R2)                2ND  BYTE                           
         AHI   R3,-2                                                            
         JM    VACCSX                                                           
         MVC   2(1,R4),4(R2)                3RD BYTE                            
*                                                                               
VACCSX   J     EXITY                                                            
***********************************************************************         
* Client upload                                                                 
***********************************************************************         
CLIUPL   SR    RE,RE               Set media                                    
         ICM   RE,7,QAMED                                                       
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
                                                                                
C        USING CLTRECD,IOKEY                                                    
         XC    C.CKEY,C.CKEY                                                    
         MVC   C.CKEYAM,QMEDX                                                   
         GOTOR VCLPACK,DMCB,(QCLIAAN,QCLICOD),C.CKEYCLT                         
         DROP  C                                                                
                                                                                
         CLI   QRECACTN,QRA_ADDQ   Test action add                              
         JNE   CLIUPL02                                                         
         L     R0,ACLIREC          Build client record from scratch             
         LHI   R1,CLIRECLN                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     CLIUPL04                                                         
                                                                                
CLIUPL02 GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOSPTDIR+B#CLIREC'                   
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOSPTFIL+B#CLIREC'                   
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
CLIUPL04 L     R2,ACLIREC                                                       
         USING CLTRECD,R2                                                       
                                                                                
         MVC   CKEY,IOKEY          Set client key                               
         MVC   CLEN,=AL2(CLIRECLN) Set record length                            
                                                                                
         MVC   CNAME,QCLINAME      Set client name                              
         CLC   CNAME,SPACES        If no name, default to client code           
         JH    *+10                                                             
         MVC   CNAME(L'QCLICOD),QCLICOD                                         
         MVC   CACCOFC,QCLIAOFF    Acc office                                   
         MVC   CACCAGY,QCLIACC     Acc agency code                              
                                                                                
         NI    CINDS1,X'FF'-CIN1OVP                                             
         CLI   QCLIOVFL,YESQ       Test overflow products                       
         JNE   *+8                                                              
         OI    CINDS1,CIN1OVP                                                   
                                                                                
         MVC   CTRAFOFC,QCLITRF    Traffic office code                          
         MVC   CCLTIFC,QCLIINTF    Interface code                               
         MVC   CPRPRD,QCLICORP     Corporate product                            
                                                                                
         NI    COPT2,X'FF'-COP2FRZ                                              
         CLI   QCLIFRZN,YESQ       Test client frozen                           
         JNE   *+8                                                              
         OI    COPT2,COP2FRZ                                                    
                                                                                
         NI    COPT3,X'FF'-COP3CS2I                                             
         CLI   QCLICOS2,YESQ       Test cos2 applies to integration             
         JNE   *+8                                                              
         OI    COPT3,COP3CS2I                                                   
                                                                                
         NI    COPT3,X'FF'-COP3T                                                
         CLI   QCLITIME,YESQ       Test time                                    
         JNE   *+8                                                              
         OI    COPT3,COP3T                                                      
                                                                                
         NI    COPT3,X'FF'-COP3TI                                               
         CLI   QCLITINT,YESQ       Test time and integration                    
         JNE   *+8                                                              
         OI    COPT3,COP3TI                                                     
                                                                                
         NI    COPT4,X'FF'-COP4UCOM                                             
         CLI   QCLIUCOM,YESQ       Test UCOM bill control                       
         JNE   *+8                                                              
         OI    COPT4,COP4UCOM                                                   
                                                                                
         NI    COPT4,X'FF'-COP4TIS                                              
         CLI   QCLIECOS,YESQ       Test run ECOST billing                       
         JNE   *+8                                                              
         OI    COPT4,COP4TIS                                                    
                                                                                
         NI    COPT4,X'FF'-COP4MIDS                                             
         CLI   QCLIMIDS,YESQ       Test Midas Client                            
         JNE   *+8                                                              
         OI    COPT4,COP4MIDS                                                   
                                                                                
         MVC   CCOST2,QCLICOSF     COST2 factor                                 
         MVC   CPROF+5(1),QCLIBILL    Bill estimate control                     
         MVC   CPROF+6(1),QCLIAAN     Print client code as AAN                  
         MVC   CPROF+14(1),QCLIRCTL   Rate control                              
         MVC   CEXTRA+14(1),QCLIRCOV  Rate coverage                             
         MVC   CEXTRA+3(1),QCLIESTF   Estimate filters required                 
         MVC   CLOCK,QCLILOCK      Lock                                         
         MVC   CTITLE,QCLIIDTI     ID title                                     
                                                                                
         MVC   CPU1,QCLIP1D        Product user description 1                   
         MVC   CPU1TYPE,QCLIP1T    User type                                    
         MVC   CPU1LEN,QCLIP1L     User length                                  
         MVI   CPU1FLG1,0                                                       
                                                                                
         CLI   QCLIP1R,YESQ        Required                                     
         JNE   *+8                                                              
         OI    CPU1FLG1,CFLGREQQ                                                
                                                                                
         CLI   QCLIP1A2,YESQ       Show on A2                                   
         JNE   *+8                                                              
         OI    CPU1FLG1,CFLGA2Q                                                 
                                                                                
         CLI   QCLIP1IB,YESQ       (NET) integrated bills                       
         JNE   *+8                                                              
         OI    CPU1FLG1,CFLGNIBQ                                                
                                                                                
         CLI   QCLIP1SB,YESQ       (SPOT) show on bills                         
         JNE   *+8                                                              
         OI    CPU1FLG1,CFLGSPQ                                                 
                                                                                
         CLI   QCLIP1MX,YESQ       Transfer on MX                               
         JNE   *+8                                                              
         OI    CPU1FLG1,CFLGMXQ                                                 
                                                                                
         CLI   QCLIP1CB,YESQ       (NET) special charge bills                   
         JNE   *+8                                                              
         OI    CPU1FLG1,CFLGNSBQ                                                
                                                                                
         CLI   QCLIP1FB,YESQ       Print in front of bills                      
         JNE   *+8                                                              
         OI    CPU1FLG1,CFLGBFRQ+CFLGSPQ                                        
                                                                                
         CLI   QCLIP1HB,YESQ       Print in headlines of bills                  
         JNE   *+8                                                              
         OI    CPU1FLG1,CFLGBHLQ+CFLGSPQ                                        
                                                                                
         MVC   CPU2,QCLIP2D        Product user description 2                   
         MVC   CPU2TYPE,QCLIP2T    User type                                    
         MVC   CPU2LEN,QCLIP2L     User length                                  
         MVI   CPU2FLG1,0                                                       
                                                                                
         CLI   QCLIP2R,YESQ        Required                                     
         JNE   *+8                                                              
         OI    CPU2FLG1,CFLGREQQ                                                
                                                                                
         CLI   QCLIP2A2,YESQ       Show on A2                                   
         JNE   *+8                                                              
         OI    CPU2FLG1,CFLGA2Q                                                 
                                                                                
         CLI   QCLIP2IB,YESQ       (NET) integrated bills                       
         JNE   *+8                                                              
         OI    CPU2FLG1,CFLGNIBQ                                                
                                                                                
         CLI   QCLIP2SB,YESQ       (SPOT) show on bills                         
         JNE   *+8                                                              
         OI    CPU2FLG1,CFLGSPQ                                                 
                                                                                
         CLI   QCLIP2MX,YESQ       Transfer on MX                               
         JNE   *+8                                                              
         OI    CPU2FLG1,CFLGMXQ                                                 
                                                                                
         CLI   QCLIP2CB,YESQ       (NET) special charge bills                   
         JNE   *+8                                                              
         OI    CPU2FLG1,CFLGNSBQ                                                
                                                                                
         CLI   QCLIP2FB,YESQ       Print in front of bills                      
         JNE   *+8                                                              
         OI    CPU2FLG1,CFLGBFRQ+CFLGSPQ                                        
                                                                                
         CLI   QCLIP2HB,YESQ       Print in headlines of bills                  
         JNE   *+8                                                              
         OI    CPU2FLG1,CFLGBHLQ+CFLGSPQ                                        
                                                                                
         MVC   CEU1,QCLIE1D        Estimate user description 1                  
         MVC   CEU1TYPE,QCLIE1T    User type                                    
         MVC   CEU1LEN,QCLIE1L     User length                                  
         MVI   CEU1FLG1,0                                                       
                                                                                
         CLI   QCLIE1R,YESQ        Required                                     
         JNE   *+8                                                              
         OI    CEU1FLG1,CFLGREQQ                                                
                                                                                
         CLI   QCLIE1A2,YESQ       Show on A2                                   
         JNE   *+8                                                              
         OI    CEU1FLG1,CFLGA2Q                                                 
                                                                                
         CLI   QCLIE1IB,YESQ       (NET) integrated bills                       
         JNE   *+8                                                              
         OI    CEU1FLG1,CFLGNIBQ                                                
                                                                                
         CLI   QCLIE1SB,YESQ       (SPOT) show on bills                         
         JNE   *+8                                                              
         OI    CEU1FLG1,CFLGSPQ                                                 
                                                                                
         CLI   QCLIE1MX,YESQ       Transfer on MX                               
         JNE   *+8                                                              
         OI    CEU1FLG1,CFLGMXQ                                                 
                                                                                
         CLI   QCLIE1CB,YESQ       (NET) special charge bills                   
         JNE   *+8                                                              
         OI    CEU1FLG1,CFLGNSBQ                                                
                                                                                
         CLI   QCLIE1FB,YESQ       Print in front of bills                      
         JNE   *+8                                                              
         OI    CEU1FLG1,CFLGBFRQ+CFLGSPQ                                        
                                                                                
         CLI   QCLIE1HB,YESQ       Print in headlines of bills                  
         JNE   *+8                                                              
         OI    CEU1FLG1,CFLGBHLQ+CFLGSPQ                                        
                                                                                
         MVC   CEU1FLG2,QCEU1FG2   Update bits                                  
                                                                                
         MVC   CEU2,QCLIE2D        Estimate user description 2                  
         MVC   CEU2TYPE,QCLIE2T    User type                                    
         MVC   CEU2LEN,QCLIE2L     User length                                  
         MVI   CEU2FLG1,0                                                       
                                                                                
         CLI   QCLIE2R,YESQ        Required                                     
         JNE   *+8                                                              
         OI    CEU2FLG1,CFLGREQQ                                                
                                                                                
         CLI   QCLIE2A2,YESQ       Show on A2                                   
         JNE   *+8                                                              
         OI    CEU2FLG1,CFLGA2Q                                                 
                                                                                
         CLI   QCLIE2IB,YESQ       (NET) integrated bills                       
         JNE   *+8                                                              
         OI    CEU2FLG1,CFLGNIBQ                                                
                                                                                
         CLI   QCLIE2SB,YESQ       (SPOT) show on bills                         
         JNE   *+8                                                              
         OI    CEU2FLG1,CFLGSPQ                                                 
                                                                                
         CLI   QCLIE2MX,YESQ       Transfer on MX                               
         JNE   *+8                                                              
         OI    CEU2FLG1,CFLGMXQ                                                 
                                                                                
         CLI   QCLIE2CB,YESQ       (NET) special charge bills                   
         JNE   *+8                                                              
         OI    CEU2FLG1,CFLGNSBQ                                                
                                                                                
         CLI   QCLIE2FB,YESQ       Print in front of bills                      
         JNE   *+8                                                              
         OI    CEU2FLG1,CFLGBFRQ+CFLGSPQ                                        
                                                                                
         CLI   QCLIE2HB,YESQ       Print in headlines of bills                  
         JNE   *+8                                                              
         OI    CEU2FLG1,CFLGBHLQ+CFLGSPQ                                        
                                                                                
         MVC   CEU2FLG2,QCEU2FG2   Update bits                                  
                                                                                
         MVC   CACCESS,QCLILIMA    Limited access                               
         MVC   CLTSLLMT,QCLIFRSB   FrontRunner subline                          
         MVC   CSCJROT,QCLISCJR    Rotation Start Day                           
                                                                                
         MVC   COFFICE,QCLIOFF     Office number                                
                                                                                
         XC    OFCBLK,OFCBLK                                                    
         USING OFFICED,OFCBLK      Setup officer control block                  
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,LP_AGY                                                    
         MVC   OFCAUTH,LP_ACCS                                                  
         MVC   OFCCLT,QCLICOD                                                   
         MVC   OFCSAGMD,QMEDX                                                   
         MVC   OFCLMT,LP_ACCS                                                   
         MVC   OFCSECD,LP_ASECD                                                 
         MVC   OFCACCSC(L'CACCESS),CACCESS                                      
         MVI   OFCACCSM,X'FF'                                                   
         MVI   OFCINDS,OFCI2CSC                                                 
         MVC   OFCCLT2,CKEYCLT                                                  
                                                                                
         MVC   OFCOFC2,QCLIOFF                                                  
         CLI   QCLIOFF+1,0                                                      
         JNE   *+8                                                              
         MVI   OFCOFC+1,C' '                                                    
                                                                                
         GOTOR VOFFICER,DMCB,(C'2',OFFICED),ACOMFACS                            
         MVC   COFFICE,OFCOFC                                                   
                                                                                
         CLI   QRECACTN,QRA_ADDQ   Test action add                              
         JNE   CLIUPL08                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOSPTFIL+B#CLIREC'                     
         JE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
CLIUPL08 GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOSPTFIL+B#CLIREC'                     
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
CLIRECLN EQU   1500                Client record length                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* CFM upload - Product record                                         *         
***********************************************************************         
                                                                                
UPLPRD   LKREQ H,M#UPLPRD,NEXTREQ=REQEND,ROUTINE=PRDUPL,NEWREC=Y                
                                                                                
Media    LKREQ F,001,(I,B#SAVED,QMEDNDX),(U,#VALMED,$VALMED),          +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=(*,PMEDLIT),COL=*              
Client   LKREQ F,002,(I,B#SAVED,QCLTNDX),(U,#VALCLT,$VALCLT),          +        
               OLEN=L'QCLTX,TEXT=(*,PCLILIT),COL=*                              
PrdCode  LKREQ F,003,(D,B#SAVED,QPRDCODE),CHAR,TEXT=(*,PCODLIT),COL=*           
AccNum   LKREQ F,004,(D,B#SAVED,QPRDACC),CHAR,TEXT=(*,PACCLIT),COL=*            
PrdName  LKREQ F,005,(D,B#SAVED,QPRDNAME),CHAR,TEXT=(*,PNAMLIT),COL=*           
PrdClass LKREQ F,006,(D,B#SAVED,QPRDCLSS),CHAR,TEXT=(*,PCLSLIT),COL=*           
Lock     LKREQ F,007,(D,B#SAVED,QPRDLOCK),CHAR,TEXT=(*,PLOCLIT),COL=*           
LockDate LKREQ F,008,(D,B#SAVED,QPRDLKDT),CDAT,TEXT=(*,PLKDLIT),COL=*           
BillName LKREQ F,009,(D,B#SAVED,QPRDBLNM),CHAR,TEXT=(*,PBLNMLIT),COL=*          
BillAdd1 LKREQ F,010,(D,B#SAVED,QPRDBAD1),CHAR,TEXT=(*,PBAD1LIT),COL=*          
BillAdd2 LKREQ F,011,(D,B#SAVED,QPRDBAD2),CHAR,TEXT=(*,PBAD2LIT),COL=*          
BillAdd3 LKREQ F,012,(D,B#SAVED,QPRDBAD3),CHAR,TEXT=(*,PBAD3LIT),COL=*          
BILLBASE LKREQ F,013,(I,B#SAVED,AQPRDBAS),LBIN,TEXT=(*,PBASELIT),OLEN=1         
BillComm LKREQ F,014,(D,B#SAVED,QPRDBCOM),CBIN,TEXT=(*,PBCOMLIT),COL=*          
AgyFee   LKREQ F,015,(D,B#SAVED,QPRDAGYF),SPAK,TEXT=(*,PAGYFLIT),COL=*          
MOS      LKREQ F,016,(D,B#SAVED,QPRDMOS),BMON,TEXT=(*,PMOSLIT),COL=*            
User1    LKREQ F,017,(D,B#SAVED,QPRDUSR1),CHAR,TEXT=(*,PUSR1LIT),COL=*          
User2    LKREQ F,018,(D,B#SAVED,QPRDUSR2),CHAR,TEXT=(*,PUSR2LIT),COL=*          
                                                                                
DontBill LKREQ F,020,(D,B#SAVED,QPRDDBIL),CHAR,TEXT=(*,PDBILLIT),COL=*          
RFC=Y    LKREQ F,021,(D,B#SAVED,QPRDRFC),CHAR,TEXT=(*,PRFCLIT),COL=*            
Theater  LKREQ F,022,(D,B#SAVED,QPRDTHTR),CHAR,TEXT=(*,PTHTRLIT),COL=*          
                                                                                
PBillBas LKREQ F,80,(D,B#SAVED,QBILLBAS),CHAR,TEXT=(*,PBASELIT),COL=*           
PCommBil LKREQ F,81,(D,B#SAVED,QCOMMBIL),CHAR,TEXT=(*,PBASELIT),COL=*           
PCommBas LKREQ F,82,(D,B#SAVED,QCOMMBAS),CHAR,TEXT=(*,PBASELIT),COL=*           
                                                                                
P_Token  LKREQ F,300,(D,B#SAVED,QP_TOKEN),CHAR,TEXT=(*,PTOKNLIT),COL=*          
PRecAct  LKREQ F,350,(D,B#SAVED,QRECACTN),CHAR,TEXT=(*,RECAPLIT),COL=*          
                                                                                
         LKREQ E                                                                
                                                                                
PMEDLIT  DC    C'Media Code'                                                    
PCLILIT  DC    C'Client Code'                                                   
PCODLIT  DC    C'Product Code'                                                  
PACCLIT  DC    C'Account Number'                                                
PNAMLIT  DC    C'Product Name'                                                  
PCLSLIT  DC    C'Product Class'                                                 
PLOCLIT  DC    C'Product Locked'                                                
PLKDLIT  DC    C'Product Locked Date'                                           
PBLNMLIT DC    C'Bill Receipt Name'                                             
PBAD1LIT DC    C'Bill Address Line 1'                                           
PBAD2LIT DC    C'Bill Address Line 2'                                           
PBAD3LIT DC    C'Bill Address Line 3'                                           
PBASELIT DC    C'Bill Base/Commission Base'                                     
PBCOMLIT DC    C'Bill Commission'                                               
PAGYFLIT DC    C'Other Agency Fee'                                              
PMOSLIT  DC    C'Effective Y/M of service'                                      
PUSR1LIT DC    C'User field 1'                                                  
PUSR2LIT DC    C'User field 2'                                                  
PDBILLIT DC    C'Do not bill this product'                                      
PRFCLIT  DC    C'RFC=Y'                                                         
PTHTRLIT DC    C'Theatrical'                                                    
                                                                                
PTOKNLIT DC    C'Token'                                                         
RECAPLIT DC    C'Record Action'                                                 
                                                                                
PRDUPL   SR    RE,RE               Set media and client                         
         ICM   RE,7,QAMED                                                       
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
         ICM   RE,7,QACLT                                                       
         MVC   QCLTX,LW_DATA1-LW_D(RE)                                          
                                                                                
P        USING PRDRECD,IOKEY                                                    
         XC    P.PKEY,P.PKEY                                                    
         MVC   P.PKEYAM,QMEDX                                                   
         MVC   P.PKEYCLT,QCLTX                                                  
         MVC   P.PKEYPRD,QPRDCODE                                               
         DROP  P                                                                
                                                                                
         CLI   QRECACTN,QRA_ADDQ   Test action add                              
         JNE   PRDUPL10                                                         
         L     R0,APRDREC          Build product record from scratch            
         LHI   R1,PRDRECLN                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     PRDUPL15                                                         
                                                                                
PRDUPL10 GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOSPTDIR+B#PRDREC'                   
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOSPTFIL+B#PRDREC'                   
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
PRDUPL15 L     R2,APRDREC                                                       
         USING PRDRECD,R2                                                       
                                                                                
         MVC   PKEY,IOKEY                                                       
         MVC   PLEN,=AL2(PRDRECLN)                                              
                                                                                
         MVC   PACCT,QPRDACC       Account number                               
         MVC   PNAME,QPRDNAME      Name                                         
         CLC   PNAME,SPACES        If no name, default to prd code              
         JH    *+10                                                             
         MVC   PNAME(L'QPRDCODE),QPRDCODE                                       
         MVC   PLOCK,QPRDLOCK      Lock                                         
         MVC   PLKDAT,QPRDLKDT     Lock date                                    
         MVC   PADDR1,QPRDBLNM     Bill receipt name                            
         MVC   PADDR2,QPRDBAD1     Address line 1                               
         MVC   PADDR3,QPRDBAD2     Address line 2                               
         MVC   PADDR4,QPRDBAD3     Address line 3                               
*****    MVC   PBILLBAS,QPRDBASE   BILL BASE/COMMISSION BASE                    
         MVC   PBILLCOM,QPRDBCOM   Bill commission                              
         MVC   PAGYFEE,QPRDAGYF    Other agency fee                             
         MVC   PBILLDT,QPRDMOS     Effection Y/M of service                     
         MVC   PUSER1,QPRDUSR1     User field 1                                 
         MVC   PUSER2,QPRDUSR2     User field 2                                 
                                                                                
         OC    AQPRDBAS,AQPRDBAS   ANY PBILLBAS SENT?                           
         JZ    PRDUPL20                                                         
         L     RE,AQPRDBAS                                                      
         USING LW_D,RE                                                          
         MVC   QPRDBASE,LW_DATA1                                                
         J     PRDUPL60                                                         
*                                                                               
****************************************                                        
* NEW STYLE: PROGRAM TO AN INTERFACE, NOT AN IMPLEMENTATION                     
*   JAVA SHOULD NOT CARE ABOUT THE BITS THAT NEED TO BE SET.  ALL IT            
*     SHOULD CARE ABOUT IS THE INFORMATION THAT MF NEEDS TO COMPOSE             
*     THE SETTINGS IN PBILLBAS.  THAT WOULD BE: BILL BASIS, COMM ONLY,          
*     COMM BASIS, & COMM PCT                                                    
****************************************                                        
PRDUPL20 MVI   QPRDBASE,0                                                       
*                                                                               
         CLI   QBILLBAS,0               ANY BILL BASIS?                         
         JNE   PRDUPL25                 YES, WE HAVE SOMETHING                  
         CLI   QCOMMBAS,0               ANY COMM BASIS?                         
         JE    PRDUPL60                 NEITHER COMM NOR BILL BASIS             
         J     PRDUPL30                 NO, BILL BASIS, BUT COMM BASIS          
*                                                                               
PRDUPL25 CLI   QBILLBAS,C'N'            BILL BASIS IS NET?                      
         JNE   PRDUPL30                 NO, HAS TO BE GROSS                     
         OI    QPRDBASE,X'10'           YES                                     
         J     PRDUPL40                                                         
*                                                                               
PRDUPL30 OC    QPRDBCOM,QPRDBCOM        ANY COMM PCT?                           
         JNZ   PRDUPL40                 YES, THEN GROSS IS OKAY                 
         OI    QPRDBASE,X'80'           NO, GROSS IS SPECIAL THEN               
*                                                                               
PRDUPL40 CLI   QCOMMBIL,C'Y'            COMM ONLY BILLING?                      
         JNE   PRDUPL50                                                         
         OI    QPRDBASE,X'40'                                                   
*                                                                               
PRDUPL50 CLI   QCOMMBAS,C'N'            COMM BASIS IS NET?                      
         JNE   PRDUPL60                                                         
         OI    QPRDBASE,X'01'                                                   
*                                                                               
PRDUPL60 MVC   PBILLBAS,QPRDBASE   BILL BASE/COMMISSION BASE                    
*                                                                               
         MVI   PCLASS,0                                                         
         MVC   BYTE,QPRDCLSS                                                    
         MVC   PCLASS,BYTE         Set class                                    
         CLI   QPRDCLSS+1,C' '                                                  
         JNH   PRDUPL70                                                         
         CLC   QPRDCLSS(1),QPRDCLSS+1                                           
         JE    PRDUPL70                                                         
         NI    QPRDCLSS,X'0F'                                                   
         NI    QPRDCLSS+1,X'0F'                                                 
         PACK  BYTE,QPRDCLSS(1)                                                 
         OC    BYTE,QPRDCLSS+1                                                  
         MVC   PCLASS,BYTE         Set class                                    
                                                                                
PRDUPL70 NI    POPT1,X'FF'-POPT1_NOBILL                                         
         CLI   QPRDDBIL,YESQ       Test do not bill this product                
         JNE   *+8                                                              
         OI    POPT1,POPT1_NOBILL                                               
                                                                                
         NI    POPT1,X'FF'-POPT1_RFC                                            
         CLI   QPRDRFC,YESQ        Test RFC=Y                                   
         JNE   *+8                                                              
         OI    POPT1,POPT1_RFC                                                  
                                                                                
         NI    POPT1,X'FF'-POPT1_THTR                                           
         CLI   QPRDTHTR,YESQ       Test theatrical                              
         JNE   *+8                                                              
         OI    POPT1,POPT1_THTR                                                 
                                                                                
         CLI   QRECACTN,QRA_CHGQ   Test action change                           
         JNE   PRDUPL80                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOSPTFIL+B#PRDREC'                     
         JE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
PRDUPL80 GOTOR UPDCLI              Update client record w/ new prd num          
         JNE   EXITN               If error, exit                               
                                                                                
         CLC   QPRDCODE,=C'POL'                                                 
         JNE   *+8                                                              
         MVI   PRDNUM,X'FF'                                                     
         MVC   PCODE+1(1),PRDNUM                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOSPTFIL+B#PRDREC'                     
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
P        USING PLSTPSSV,IOKEY      Add product passive                          
         XC    P.PLSTPSSV,P.PLSTPSSV                                            
         MVI   P.PLSTTYPE,PLSTTYPQ                                              
         MVI   P.PLSTSUB,PLSTSUBQ                                               
         MVC   P.PLSTAM,QMEDX                                                   
         MVC   P.PLSTCLT,QCLTX                                                  
         MVC   P.PLSTPRD,QPRDCODE                                               
         MVC   P.PLSTBPRD+1(L'PRDNUM),PRDNUM                                    
         CLC   QPRDCODE,=C'POL'                                                 
         JNE   *+8                                                              
         MVI   P.PLSTXFF,X'FF'                                                  
         MVC   P.PKEYDA,IODA                                                    
         DROP  P                                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOSPTDIR'                              
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
PRDRECLN EQU   336                 Product record length                        
                                                                                
***********************************************************************         
* Update client record with new product number                        *         
***********************************************************************         
                                                                                
UPDCLI   NTR1  LABEL=*                                                          
         MVI   MOREBRND,NOQ                                                     
         MVI   SETPOL,NOQ                                                       
         MVI   PRDNUM,0                                                         
                                                                                
         XC    PRDNUMTB,PRDNUMTB                                                
         MVI   PRDNUMTB+218,X'FF'  Set product numbers reserved                 
         MVI   PRDNUMTB+219,X'FF'                                               
         MVI   PRDNUMTB+251,X'FF'                                               
         MVI   PRDNUMTB+252,X'FF'                                               
         MVI   PRDNUMTB+253,X'FF'                                               
         MVI   PRDNUMTB+254,X'FF'                                               
                                                                                
         XC    IOKEY,IOKEY                                                      
C        USING CLTRECD,IOKEY                                                    
         XC    C.CKEY,C.CKEY                                                    
         MVC   C.CKEYAM,QMEDX                                                   
         MVC   C.CKEYCLT,QCLTX                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOSPTDIR+B#CLIREC'                   
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOSPTFIL+B#CLIREC'                   
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  C                                                                
                                                                                
         L     R2,ACLIREC                                                       
         USING CLTRECD,R2                                                       
                                                                                
         CLC   QPRDCODE,=C'POL'    Test adding POL product                      
         JE    *+12                                                             
         TM    CINDS1,CIN1OVP      Test overflow products                       
         JO    EXITY               Don't need to update client record           
                                                                                
         LA    R3,CLIST                                                         
         USING PRDLST,R3                                                        
         LHI   R4,CPLDMAXN                                                      
                                                                                
UPDCLI02 CLI   0(R3),0             Test available entry                         
         JE    UPDCLI06            Add new product to CLIST/2                   
                                                                                
         CLC   PRDLPRD,QPRDCODE    Test already in CLIST/2                      
         JE    DUPERR                                                           
                                                                                
         CLC   PRDLPRD,=C'POL'     Test POL entry in CLIST/2                    
         JNE   *+12                                                             
         MVI   SETPOL,YESQ                                                      
         J     UPDCLI06                                                         
                                                                                
         ZIC   RF,PRDLNUM                                                       
         LA    RF,PRDNUMTB-1(RF)                                                
         MVI   0(RF),X'FF'         Set product number used                      
         AHI   R3,PRDLSTL                                                       
         JCT   R4,UPDCLI02                                                      
                                                                                
         LA    RF,CLIST2           Test processing CLIST2                       
         CR    R3,RF                                                            
         JNL   UPDCLI04            CLIST/CLIST2 full-set more brands            
                                                                                
         LA    R3,CLIST2                                                        
         LHI   R4,35               Max number of entries in CLIST2              
         J     UPDCLI02                                                         
                                                                                
UPDCLI04 MVI   MOREBRND,YESQ       Set product to more brands                   
         J     UPDCLI20                                                         
                                                                                
UPDCLI06 CLC   QPRDCODE,=C'POL'    Test adding POL product                      
         JE    UPDCLI18                                                         
                                                                                
         LA    RF,PRDNUMTB         Get available product number                 
         LA    R1,255                                                           
         SR    R4,R4                                                            
         LHI   R4,1                                                             
                                                                                
UPDCLI08 CLI   0(RF),X'FF'         Test product number used                     
         JNE   UPDCLI10                                                         
         AHI   RF,1                                                             
         AHI   R4,1                                                             
         JCT   R1,UPDCLI08                                                      
                                                                                
UPDCLI10 LA    RF,CLIST2           Test last product entry in CLIST2            
         LA    R0,29                                                            
         MHI   R0,4                                                             
         AR    RF,R0                                                            
         CR    R3,RF                                                            
         JL    UPDCLI12                                                         
         MVI   MOREBRND,YESQ       Reached last entry in CLIST2                 
         J     UPDCLI20            Reserved for POL                             
                                                                                
UPDCLI12 STC   R4,PRDNUM                                                        
         MVC   PRDLPRD,QPRDCODE    Set product code in CLIST/2                  
         MVC   PRDLNUM,PRDNUM      Set produce number in CLIST/2                
                                                                                
         CLI   SETPOL,YESQ         Test POL entry must be re-added              
         JNE   UPDCLI20                                                         
                                                                                
UPDCLI14 LA    RF,CCLTIFC-PRDLSTL  Test last entry of CLIST                     
         CR    R3,RF                                                            
         JNE   UPDCLI16                                                         
         LA    R3,CLIST2           CLIST full, add POL to CLIST2                
         J     UPDCLI18                                                         
                                                                                
UPDCLI16 AHI   R3,PRDLSTL                                                       
UPDCLI18 MVC   PRDLPRD,=C'POL'     Set POL code                                 
         MVI   PRDLNUM,X'FF'       Set POL number                               
                                                                                
UPDCLI20 CLI   MOREBRND,YESQ       Test more brands                             
         JNE   *+12                                                             
         OI    CINDS1,CIN1OVP      Set overflow products                        
         MVI   PRDNUM,0                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOSPTFIL+B#CLIREC'                     
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         J     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
                                                                                
DUPERR   MVI   LNKSEQ,1            Product already exists in CLIST              
         MVI   LNKMAPN,0                                                        
         MVC   LNKMTXT,SPACES                                                   
         MVI   LNKMSGN,0                                                        
                                                                                
         MVC   LNKMSG,=C'      Product already exists  '                        
         MVC   LNKMSG(L'QPRDCODE),QPRDCODE                                      
         GOTOR PCREPLY                                                          
         J     EXITN                                                            
                                                                                
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
                                                                                
QRECACTN DS    X                   Record Action                                
QRA_ADDQ EQU   C'A'                Add                                          
QRA_CHGQ EQU   C'C'                Change                                       
                                                                                
QMEDNDX  DS    0XL4                Media index                                  
QMEDIND  DS    X                                                                
QAMED    DS    AL3                                                              
                                                                                
QCLTNDX  DS    0XL4                Client index                                 
QCLTIND  DS    X                                                                
QACLT    DS    AL3                                                              
                                                                                
AQPRDBAS DS    A                   A(PRODUCT BILL BASIS)                        
                                                                                
QCLICOD  DS    CL3                                                              
QCLINAME DS    CL(L'CNAME)                                                      
QCLIOFF  DS    CL2                                                              
QCLIAOFF DS    CL(L'CACCOFC)                                                    
QCLIACC  DS    CL(L'CACCAGY)                                                    
                                                                                
QCLIOVFL DS    C                                                                
                                                                                
QCLITRF  DS    CL(L'CTRAFOFC)                                                   
QCLIINTF DS    CL(L'CCLTIFC)                                                    
QCLICORP DS    CL(L'CPRPRD)                                                     
                                                                                
QCLIFRZN DS    C                                                                
                                                                                
QCLICOS2 DS    C                                                                
QCLITIME DS    C                                                                
QCLITINT DS    C                                                                
                                                                                
QCLIMIDS DS    C                                                                
QCLIUCOM DS    C                                                                
QCLIECOS DS    C                                                                
                                                                                
QCLICOSF DS    XL(L'CCOST2)                                                     
QCLIBILL DS    C                                                                
QCLIAAN  DS    C                                                                
QCLIRCTL DS    C                                                                
QCLIRCOV DS    C                                                                
QCLIESTF DS    C                                                                
QCLILOCK DS    C                                                                
QCLIIDTI DS    CL(L'CTITLE)                                                     
                                                                                
QCLIP1D  DS    CL(L'CPU1)                                                       
QCLIP1T  DS    C                                                                
QCLIP1L  DS    X                                                                
QCLIP1R  DS    C                                                                
QCLIP1A2 DS    C                                                                
QCLIP1IB DS    C                                                                
QCLIP1SB DS    C                                                                
QCLIP1MX DS    C                                                                
QCLIP1CB DS    C                                                                
QCLIP1FB DS    C                                                                
QCLIP1HB DS    C                                                                
                                                                                
QCLIP2D  DS    CL(L'CPU1)                                                       
QCLIP2T  DS    C                                                                
QCLIP2L  DS    X                                                                
QCLIP2R  DS    C                                                                
QCLIP2A2 DS    C                                                                
QCLIP2IB DS    C                                                                
QCLIP2SB DS    C                                                                
QCLIP2MX DS    C                                                                
QCLIP2CB DS    C                                                                
QCLIP2FB DS    C                                                                
QCLIP2HB DS    C                                                                
                                                                                
QCLIE1D  DS    CL(L'CPU1)                                                       
QCLIE1T  DS    C                                                                
QCLIE1L  DS    X                                                                
QCLIE1R  DS    C                                                                
QCLIE1A2 DS    C                                                                
QCLIE1IB DS    C                                                                
QCLIE1SB DS    C                                                                
QCLIE1MX DS    C                                                                
QCLIE1CB DS    C                                                                
QCLIE1FB DS    C                                                                
QCLIE1HB DS    C                                                                
                                                                                
QCEU1FG2 DS    CL(L'CPU1FLG2)                                                   
                                                                                
QCLIE2D  DS    CL(L'CPU2)                                                       
QCLIE2T  DS    C                                                                
QCLIE2L  DS    X                                                                
QCLIE2R  DS    C                                                                
QCLIE2A2 DS    C                                                                
QCLIE2IB DS    C                                                                
QCLIE2SB DS    C                                                                
QCLIE2MX DS    C                                                                
QCLIE2CB DS    C                                                                
QCLIE2FB DS    C                                                                
QCLIE2HB DS    C                                                                
                                                                                
QCEU2FG2 DS    CL(L'CPU2FLG2)                                                   
                                                                                
QCLILIMA DS    CL(L'CACCESS)                                                    
QCLIFRSB DS    X                                                                
QCLISCJR DS    CL(L'CSCJROT)                                                    
                                                                                
QPRDCLI  DS    XL(L'PKEYCLT)                                                    
QPRDCODE DS    CL(L'PKEYPRD)                                                    
QPRDACC  DS    CL(L'PACCT)                                                      
QPRDNAME DS    CL(L'PNAME)                                                      
QPRDCLSS DS    CL2                                                              
QPRDLOCK DS    CL(L'PLOCK)                                                      
QPRDLKDT DS    XL(L'PLKDAT)                                                     
QPRDBLNM DS    CL(L'PADDR1)                                                     
QPRDBAD1 DS    CL(L'PADDR2)                                                     
QPRDBAD2 DS    CL(L'PADDR3)                                                     
QPRDBAD3 DS    CL(L'PADDR4)                                                     
QPRDBASE DS    XL(L'PBILLBAS)                                                   
QPRDBCOM DS    XL(L'PBILLCOM)                                                   
QPRDAGYF DS    XL(L'PAGYFEE)                                                    
QPRDMOS  DS    XL(L'PBILLDT)                                                    
QPRDUSR1 DS    CL(L'PUSER1)                                                     
QPRDUSR2 DS    CL(L'PUSER2)                                                     
                                                                                
QPRDDBIL DS    C                                                                
QPRDRFC  DS    C                                                                
QPRDTHTR DS    C                                                                
                                                                                
QBILLBAS DS    C                   BILL BASIS, X'00', C'G', OR C'N'             
QCOMMBIL DS    C                   COMMISSION ONLY BILLING   Y/N                
QCOMMBAS DS    C                   COMM BASIS, X'00', C'G', OR C'N'             
                                                                                
QVALUEL  EQU   *-QVALUES                                                        
                                                                                
LNKVALS  DS    0F                  LNKIO values                                 
LNKSEQ   DS    X                   Sequence #                                   
LNKMAPN  DS    X                   Upload mapcode #                             
LNKMTXT  DS    CL30                Upload mapcode text                          
LNKMSGN  DS    X                   Message #                                    
LNKMSG   DS    CL30                PC reply message                             
LNKVALSL EQU   *-LNKVALS                                                        
                                                                                
IOP      DS    CL132               Print line                                   
MOREBRND DS    C                   More brands (Y/N)                            
SETPOL   DS    C                   Reset POL to end of table                    
OFCBLK   DS    XL(OFCLENQ)         Officer block                                
PRDNUMTB DS    XL255               Product number table                         
PRDNAMXN EQU   249                 Maximum product number                       
PRDNUM   DS    X                   Product number                               
                                                                                
         ORG   SAVED+(4*ONEK)      Position to product list                     
PRDLST   DS    0X                  ** Product list entry **                     
PRDLPRD  DS    CL(L'PKEYPRD)       Product code                                 
PRDLNUM  DS    X                   Product number                               
PRDLSTL  EQU   *-PRDLST            Length of product list entry                 
         EJECT                                                                  
* Other included books                                                          
         PRINT OFF                                                              
       ++INCLUDE NENAVWORKD                                                     
       ++INCLUDE SPGENESTD                                                      
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE DDOFFICED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014NENAV15   11/20/17'                                      
         END                                                                    
