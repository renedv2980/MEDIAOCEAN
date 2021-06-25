*          DATA SET PPLNK22    AT LEVEL 015 AS OF 05/19/15                      
*PHASE T41422A                                                                  
PPLNK22  TITLE '- PrintPak CFM Upload'                                          
         PRINT NOGEN                                                            
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,WORKERKEY=PCLU,SEGMENT=Y,LINKIO=Y,               +        
               APPEND=Y,REQUEST=*,CODE=CODE,IDF=Y,                     +        
               SYSPHASE=SYSPHASE,SYSTEM=PRTSYSQ,FILES=FILES,           +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#CLT,PCLTRECD,     +        
               B#PRD,PPRDRECD),RLEN=2500,LOADFACSOFF=Y                          
                                                                                
B#CLT    EQU   3                   PCLTRECD                                     
ACLTREC  EQU   LP_BLKS+((B#CLT-1)*L'LP_BLKS),,C'A'                              
B#PRD    EQU   4                   PPRDRECD                                     
APRDREC  EQU   LP_BLKS+((B#PRD-1)*L'LP_BLKS),,C'A'                              
         EJECT                                                                  
                                                                                
CODE     NMOD1 0,**PL22**                                                       
                                                                                
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
         USING OFFICED,OFCBLK                                                   
                                                                                
INIT04   MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   LINKIO,CLINKIO-COMFACSD(RF)                                      
         MVC   RECUP,CRECUP-COMFACSD(RF)                                        
         MVI   FATALERR,C'N'                                                    
         DROP  R6,R7,RB                                                         
                                                                                
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
         EJECT                                                                  
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   LP_CMODE,RRUNSTRQ   Test first for run                           
         JNE   INIREQ                                                           
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    EXITY               No                                           
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('O#ROU1',0),0,0                                       
         MVC   AROUT1,0(R1)                                                     
         MVC   LP_AUIR1,AROUT1     Set A(index routines 1)                      
         GOTOR (RF),DMCB,('O#ROU2',0),0,0                                       
         MVC   AROUT2,0(R1)                                                     
         MVC   LP_AUIR2,AROUT2     Set A(index routines 2)                      
                                                                                
         GOTOR (#WRKINI,AWRKINI)   Initialize working storage                   
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Initialize download/upload variables                                *         
***********************************************************************         
                                                                                
INIREQ   CLI   LP_CMODE,RINIREQQ   Test 'initialize request'                    
         JE    *+12                                                             
         CLI   LP_CMODE,RPRCWRKQ   Test 'process work' mode                     
         JNE   RUNREQ                                                           
         MVC   LP_BLKS+((B#CLT-01)*L'LP_BLKS)(L'AIOS),AIO3                      
         MVC   LP_BLKS+((B#PRD-01)*L'LP_BLKS)(L'AIOS),AIO4                      
                                                                                
         LA    R0,QVALUES          Clear request values                         
         LHI   R1,QVALUEL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   RUNINDS,0                                                        
         MVI   RPYIND1,0                                                        
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Run a download request                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   LP_CMODE,RRUNREQQ   Test 'run request' mode                      
         JNE   RUNEND                                                           
                                                                                
         CLI   FATALERR,C'Y'       Fatal error occurred?                        
         JE    EXITY                                                            
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    RUNREQ02                                                         
                                                                                
         L     RF,AMASTC           Set trace option                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,PRTDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,PRTFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,PUBDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,PUBFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,GENDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,GENFIL,(4,0),0                               
                                                                                
RUNREQ02 L     RF,LP_ALPXD         Extract server id                            
         MVC   MQSVR,LP_MQSVR-LP_XD(RF)                                         
                                                                                
         GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         OC    LP_ERROR,LP_ERROR   Test error number is set                     
         JNZ   EXITN                                                            
         J     EXITY               Exit back to DDLINK                          
         EJECT                                                                  
***********************************************************************         
* Last for upload                                                     *         
***********************************************************************         
                                                                                
RUNEND   CLI   LP_CMODE,RRUNENDQ   Test last time request to server             
         JNE   EXITY                                                            
                                                                                
         GOTOR VDATAMGR,DMCB,DMCOMMIT,0                                         
                                                                                
         L     R3,LP_ALPXD         Point to LP_XD                               
         USING LP_XD,R3                                                         
         OC    LP_LKEY1,LP_LKEY1   Test any key to unlock                       
         JZ    EXITY                                                            
         GOTOR VLOCKET,DMCB,('LKUNGLQ',LP_LKEY1)                                
         CLI   4(R1),0                                                          
         JE    EXITY                                                            
         DC    H'0'                Die for now                                  
         DROP  R3                                                               
                                                                                
***********************************************************************         
* CFM Client record upload                                            *         
***********************************************************************         
                                                                                
         DS    0H                                                               
CLTCFMU  LKREQ H,M#UL_CLT,NEXTREQ=PRDCFMU,ROUTINE=CLTRECU,NEWREC=Y              
                                                                                
CMedCod  LKREQ F,001,(D,B#SAVED,QCLTKMED),CHAR,TEXT=(*,MEDCDLIT),COL=*          
CCltCod  LKREQ F,002,(D,B#SAVED,QCLTKCLT),CHAR,TEXT=(*,CCLTCLIT),COL=*          
CCltNam  LKREQ F,003,(D,B#SAVED,QCLTNAME),CHAR,TEXT=(*,CCLTNLIT),COL=*          
CBilRNm  LKREQ F,004,(D,B#SAVED,QCLTBNAM),CHAR,TEXT=(*,CBILRLIT),COL=*          
CAdrLn1  LKREQ F,005,(D,B#SAVED,QCLTLIN1),CHAR,TEXT=(*,CADR1LIT),COL=*          
CAdrLn2  LKREQ F,006,(D,B#SAVED,QCLTLIN2),CHAR,TEXT=(*,CADR2LIT),COL=*          
CAttntn  LKREQ F,007,(D,B#SAVED,QCLTATTN),CHAR,TEXT=(*,CATTNLIT),COL=*          
                                                                                
CProf01  LKREQ F,020,(D,B#SAVED,QCLTPROF+00),CHAR,TEXT=(*,CDIVNLIT),   +        
               OLEN=1,COL=*                                                     
CProf02  LKREQ F,021,(D,B#SAVED,QCLTPROF+01),CHAR,TEXT=(*,CBDCOLIT),   +        
               OLEN=1,COL=*                                                     
CProf03  LKREQ F,022,(D,B#SAVED,QCLTPROF+02),CHAR,TEXT=(*,CBDBDLIT),   +        
               OLEN=1,COL=*                                                     
CProf04  LKREQ F,023,(D,B#SAVED,QCLTPROF+03),CHAR,TEXT=(*,CBDDALIT),   +        
               OLEN=1,COL=*                                                     
CProf05  LKREQ F,024,(D,B#SAVED,QCLTPROF+04),CHAR,TEXT=(*,CNNAJLIT),   +        
               OLEN=1,COL=*                                                     
CProf06  LKREQ F,025,(D,B#SAVED,QCLTPROF+05),CHAR,TEXT=(*,CCTYPLIT),   +        
               OLEN=1,COL=*                                                     
CProf07  LKREQ F,026,(D,B#SAVED,QCLTPROF+06),CHAR,TEXT=(*,CMASTLIT),   +        
               OLEN=3,COL=*                                                     
CProf08  LKREQ F,027,(D,B#SAVED,QCLTPROF+10),CHAR,TEXT=(*,CPEBFLIT),   +        
               OLEN=1,COL=*                                                     
CProf09  LKREQ F,028,(D,B#SAVED,QCLTPROF+12),CHAR,TEXT=(*,CCONRLIT),   +        
               OLEN=1,COL=*                                                     
CProf10  LKREQ F,029,(D,B#SAVED,QCLTPROF+14),CHAR,TEXT=(*,CBUDTLIT),   +        
               OLEN=1,COL=*                                                     
CProf11  LKREQ F,030,(D,B#SAVED,QCLTPROF+18),CHAR,TEXT=(*,CESTRLIT),   +        
               OLEN=1,COL=*                                                     
                                                                                
COffCoD  LKREQ F,055,(D,B#SAVED,QCLTOFF_),CHAR,TEXT=(*,COFFCLIT),COL=*          
CGSTCoD  LKREQ F,056,(D,B#SAVED,QCLTGST_),CHAR,TEXT=(*,CGSTCLIT),COL=*          
CBilGrp  LKREQ F,057,(D,B#SAVED,QCLTBLGP),CHAR,TEXT=(*,CBGRPLIT),COL=*          
CCltNum  LKREQ F,058,(D,B#SAVED,QCLTNUMB),CHAR,TEXT=(*,CCLT#LIT),COL=*          
CAccAgy  LKREQ F,059,(D,B#SAVED,QCLTACCA),CHAR,TEXT=(*,CACCOLIT),COL=*          
                                                                                
CCltSt0  LKREQ F,060,(D,B#SAVED,QCLTSX80),CHAR,TEXT=(*,CSTA0LIT),COL=*          
CCltSt1  LKREQ F,061,(D,B#SAVED,QCLTSX40),CHAR,TEXT=(*,CSTA1LIT),COL=*          
CCltSt2  LKREQ F,062,(D,B#SAVED,QCLTSX20),CHAR,TEXT=(*,CSTA2LIT),COL=*          
CCltSt3  LKREQ F,063,(D,B#SAVED,QCLTSX10),CHAR,TEXT=(*,CSTA3LIT),COL=*          
CCltSt4  LKREQ F,064,(D,B#SAVED,QCLTSX08),CHAR,TEXT=(*,CSTA4LIT),COL=*          
CCltSt5  LKREQ F,065,(D,B#SAVED,QCLTSX04),CHAR,TEXT=(*,CSTA5LIT),COL=*          
CCltSt6  LKREQ F,066,(D,B#SAVED,QCLTSX02),CHAR,TEXT=(*,CSTA6LIT),COL=*          
CCltSt7  LKREQ F,067,(D,B#SAVED,QCLTSX01),CHAR,TEXT=(*,CSTA7LIT),COL=*          
                                                                                
CFinClt  LKREQ F,068,(D,B#SAVED,QCLTFIN_),CHAR,TEXT=(*,CFINCLIT),COL=*          
CAccOfC  LKREQ F,069,(D,B#SAVED,QCLTAOFC),CHAR,TEXT=(*,CAOFCLIT),COL=*          
                                                                                
CConSCm  LKREQ F,070,(D,B#SAVED,QCONSTCM),CHAR,TEXT=(*,CCSC#LIT),COL=*          
CIOComm  LKREQ F,071,(I,B#SAVED,QIOCIND),CHAR,OLEN=L'PCLTINUM,         +        
               LIST=F,SORT=N,TEXT=(*,CIOC#LIT),COL=*                            
                                                                                
CAgyRec  LKREQ F,072,(D,B#SAVED,QAGYOREC),CHAR,TEXT=(*,CAGYRLIT),COL=*          
CAdvtsr  LKREQ F,073,(D,B#SAVED,QADVTISR),CHAR,TEXT=(*,CADVRLIT),COL=*          
CAdvClt  LKREQ F,074,(D,B#SAVED,QADVTCLT),CHAR,TEXT=(*,CADVCLIT),COL=*          
CAORSdt  LKREQ F,075,(D,B#SAVED,QAORSTDT),CHAR,TEXT=(*,CAORSLIT),COL=*          
CAOREdt  LKREQ F,076,(D,B#SAVED,QAORENDT),CHAR,TEXT=(*,CAORELIT),COL=*          
CAORSE#  LKREQ F,077,(D,B#SAVED,QAORSENO),CHAR,TEXT=(*,CAOR#LIT),COL=*          
                                                                                
CAORC10  LKREQ F,078,(D,B#SAVED,QCBY1X80),CHAR,TEXT=(*,CAORILIT),COL=*          
CAORC11  LKREQ F,079,(D,B#SAVED,QCBY1X40),CHAR,TEXT=(*,CAORCLIT),COL=*          
CAORC12  LKREQ F,080,(D,B#SAVED,QCBY1X20),CHAR,TEXT=(*,CAORDLIT),COL=*          
CAORC13  LKREQ F,081,(D,B#SAVED,QCBY1X10),CHAR,TEXT=(*,CADVSLIT),COL=*          
CAORC14  LKREQ F,082,(D,B#SAVED,QCBY1X08),CHAR,TEXT=(*,CBCRLLIT),COL=*          
CAORC15  LKREQ F,083,(D,B#SAVED,QCBY1X04),CHAR,TEXT=(*,CCCLLLIT),COL=*          
CAORC16  LKREQ F,084,(D,B#SAVED,QCBY1X02),CHAR,TEXT=(*,CCCRLLIT),COL=*          
CAORC17  LKREQ F,085,(D,B#SAVED,QCBY1X01),CHAR,TEXT=(*,CPUBLLIT),COL=*          
                                                                                
CAORC27  LKREQ F,093,(D,B#SAVED,QCBY2X01),CHAR,TEXT=(*,CNBAALIT),COL=*          
                                                                                
CCltPU1  LKREQ F,110,(D,B#SAVED,QCLTPU1_),CHAR,TEXT=(*,CPF1DLIT),COL=*          
CP1Type  LKREQ F,111,(D,B#SAVED,QCLTP1TY),CHAR,TEXT=(*,CPF1TLIT),COL=*          
CP1Leng  LKREQ F,112,(D,B#SAVED,QCLTP1LN),UBIN,TEXT=(*,CPF1LLIT),COL=*          
CP1F180  LKREQ F,113,(D,B#SAVED,QP1F1X80),CHAR,TEXT=(*,CPF10LIT),COL=*          
CP1F140  LKREQ F,114,(D,B#SAVED,QP1F1X40),CHAR,TEXT=(*,CPF11LIT),COL=*          
CP1F120  LKREQ F,115,(D,B#SAVED,QP1F1X20),CHAR,TEXT=(*,CPF12LIT),COL=*          
CP1F110  LKREQ F,116,(D,B#SAVED,QP1F1X10),CHAR,TEXT=(*,CPF13LIT),COL=*          
CP1F108  LKREQ F,117,(D,B#SAVED,QP1F1X08),CHAR,TEXT=(*,CPF14LIT),COL=*          
CP1F104  LKREQ F,118,(D,B#SAVED,QP1F1X04),CHAR,TEXT=(*,CPF15LIT),COL=*          
CP1F102  LKREQ F,119,(D,B#SAVED,QP1F1X02),CHAR,TEXT=(*,CPF16LIT),COL=*          
CP1F101  LKREQ F,120,(D,B#SAVED,QP1F1X01),CHAR,TEXT=(*,CPF17LIT),COL=*          
                                                                                
CCltPU2  LKREQ F,130,(D,B#SAVED,QCLTPU2_),CHAR,TEXT=(*,CPF2DLIT),COL=*          
CP2Type  LKREQ F,131,(D,B#SAVED,QCLTP2TY),CHAR,TEXT=(*,CPF2TLIT),COL=*          
CP2Leng  LKREQ F,132,(D,B#SAVED,QCLTP2LN),UBIN,TEXT=(*,CPF2LLIT),COL=*          
CP2F180  LKREQ F,133,(D,B#SAVED,QP2F1X80),CHAR,TEXT=(*,CPF20LIT),COL=*          
CP2F140  LKREQ F,134,(D,B#SAVED,QP2F1X40),CHAR,TEXT=(*,CPF21LIT),COL=*          
CP2F120  LKREQ F,135,(D,B#SAVED,QP2F1X20),CHAR,TEXT=(*,CPF22LIT),COL=*          
CP2F110  LKREQ F,136,(D,B#SAVED,QP2F1X10),CHAR,TEXT=(*,CPF23LIT),COL=*          
CP2F108  LKREQ F,137,(D,B#SAVED,QP2F1X08),CHAR,TEXT=(*,CPF24LIT),COL=*          
CP2F104  LKREQ F,138,(D,B#SAVED,QP2F1X04),CHAR,TEXT=(*,CPF25LIT),COL=*          
CP2F102  LKREQ F,139,(D,B#SAVED,QP2F1X02),CHAR,TEXT=(*,CPF26LIT),COL=*          
CP2F101  LKREQ F,140,(D,B#SAVED,QP2F1X01),CHAR,TEXT=(*,CPF27LIT),COL=*          
                                                                                
CCltEU1  LKREQ F,150,(D,B#SAVED,QCLTEU1_),CHAR,TEXT=(*,CEF1DLIT),COL=*          
CE1Type  LKREQ F,151,(D,B#SAVED,QCLTE1TY),CHAR,TEXT=(*,CEF1TLIT),COL=*          
CE1Leng  LKREQ F,152,(D,B#SAVED,QCLTE1LN),UBIN,TEXT=(*,CEF1LLIT),COL=*          
CE1F180  LKREQ F,153,(D,B#SAVED,QE1F1X80),CHAR,TEXT=(*,CEF10LIT),COL=*          
CE1F140  LKREQ F,154,(D,B#SAVED,QE1F1X40),CHAR,TEXT=(*,CEF11LIT),COL=*          
CE1F120  LKREQ F,155,(D,B#SAVED,QE1F1X20),CHAR,TEXT=(*,CEF12LIT),COL=*          
CE1F110  LKREQ F,156,(D,B#SAVED,QE1F1X10),CHAR,TEXT=(*,CEF13LIT),COL=*          
CE1F108  LKREQ F,157,(D,B#SAVED,QE1F1X08),CHAR,TEXT=(*,CEF14LIT),COL=*          
CE1F104  LKREQ F,158,(D,B#SAVED,QE1F1X04),CHAR,TEXT=(*,CEF15LIT),COL=*          
CE1F102  LKREQ F,159,(D,B#SAVED,QE1F1X02),CHAR,TEXT=(*,CEF16LIT),COL=*          
CE1F101  LKREQ F,160,(D,B#SAVED,QE1F1X01),CHAR,TEXT=(*,CEF17LIT),COL=*          
                                                                                
CCltEU2  LKREQ F,170,(D,B#SAVED,QCLTEU2_),CHAR,TEXT=(*,CEF2DLIT),COL=*          
CE2Type  LKREQ F,171,(D,B#SAVED,QCLTE2TY),CHAR,TEXT=(*,CEF2TLIT),COL=*          
CE2Leng  LKREQ F,172,(D,B#SAVED,QCLTE2LN),UBIN,TEXT=(*,CEF2LLIT),COL=*          
CE2F180  LKREQ F,173,(D,B#SAVED,QE2F1X80),CHAR,TEXT=(*,CEF20LIT),COL=*          
CE2F140  LKREQ F,174,(D,B#SAVED,QE2F1X40),CHAR,TEXT=(*,CEF21LIT),COL=*          
CE2F120  LKREQ F,175,(D,B#SAVED,QE2F1X20),CHAR,TEXT=(*,CEF22LIT),COL=*          
CE2F110  LKREQ F,176,(D,B#SAVED,QE2F1X10),CHAR,TEXT=(*,CEF23LIT),COL=*          
CE2F108  LKREQ F,177,(D,B#SAVED,QE2F1X08),CHAR,TEXT=(*,CEF24LIT),COL=*          
CE2F104  LKREQ F,178,(D,B#SAVED,QE2F1X04),CHAR,TEXT=(*,CEF25LIT),COL=*          
CE2F102  LKREQ F,179,(D,B#SAVED,QE2F1X02),CHAR,TEXT=(*,CEF26LIT),COL=*          
CE2F101  LKREQ F,180,(D,B#SAVED,QE2F1X01),CHAR,TEXT=(*,CEF27LIT),COL=*          
                                                                                
CPSTCod  LKREQ F,190,(D,B#SAVED,QCPSTOLD),CHAR,TEXT=(*,CPSTCLIT),COL=*          
CDRDCod  LKREQ F,191,(D,B#SAVED,QDRDCODE),CHAR,TEXT=(*,CDRDCLIT),COL=*          
CZENCod  LKREQ F,192,(D,B#SAVED,QZENCODE),CHAR,TEXT=(*,CZENCLIT),COL=*          
CMedOvN  LKREQ F,193,(D,B#SAVED,QMEDOVNM),CHAR,TEXT=(*,CMEDNLIT),COL=*          
CCos2Fc  LKREQ F,194,(D,B#SAVED,QCOS2FAC),SPAK,TEXT=(*,CC2FALIT),COL=*          
CRFPCod  LKREQ F,195,(D,B#SAVED,QRFPCODE),CHAR,TEXT=(*,CRFPGLIT),COL=*          
                                                                                
CFrzX08  LKREQ F,205,(D,B#SAVED,QFRZIX08),CHAR,TEXT=(*,CICB4LIT),COL=*          
CFrzX04  LKREQ F,206,(D,B#SAVED,QFRZIX04),CHAR,TEXT=(*,CICB5LIT),COL=*          
CFrzX02  LKREQ F,207,(D,B#SAVED,QFRZIX02),CHAR,TEXT=(*,CICB6LIT),COL=*          
                                                                                
CFrzDte  LKREQ F,215,(D,B#SAVED,QFRZDATE),BMON,TEXT=(*,CFRZDLIT),COL=*          
CTraOCd  LKREQ F,216,(D,B#SAVED,QTRAOFCD),CHAR,TEXT=(*,CTRFCLIT),COL=*          
                                                                                
C_trash  LKREQ F,217,(D,B#SAVED,Q_TRASH_),CHAR,TEXT=(*,CTRSHLIT),COL=*          
                                                                                
CPSTBC   LKREQ F,220,(D,B#SAVED,QCPST0BC),CHAR,TEXT=(*,CPSTCLIT),COL=*          
CPSTAL   LKREQ F,221,(D,B#SAVED,QCPST1AL),CHAR,TEXT=(*,CPSTCLIT),COL=*          
CPSTSA   LKREQ F,222,(D,B#SAVED,QCPST2SA),CHAR,TEXT=(*,CPSTCLIT),COL=*          
CPSTMA   LKREQ F,223,(D,B#SAVED,QCPST3MA),CHAR,TEXT=(*,CPSTCLIT),COL=*          
CPSTON   LKREQ F,224,(D,B#SAVED,QCPST4ON),CHAR,TEXT=(*,CPSTCLIT),COL=*          
CPSTPQ   LKREQ F,225,(D,B#SAVED,QCPST5PQ),CHAR,TEXT=(*,CPSTCLIT),COL=*          
CPSTNB   LKREQ F,226,(D,B#SAVED,QCPST6NB),CHAR,TEXT=(*,CPSTCLIT),COL=*          
CPSTNS   LKREQ F,227,(D,B#SAVED,QCPST7NS),CHAR,TEXT=(*,CPSTCLIT),COL=*          
CPSTPE   LKREQ F,228,(D,B#SAVED,QCPST8PE),CHAR,TEXT=(*,CPSTCLIT),COL=*          
CPSTNF   LKREQ F,229,(D,B#SAVED,QCPST9NF),CHAR,TEXT=(*,CPSTCLIT),COL=*          
                                                                                
CMPSTC   LKREQ F,230,(D,B#SAVED,QCMPSTPC),CHAR,TEXT=(*,CPSTCLIT),COL=*          
CMPSTC   LKREQ F,231,(D,B#SAVED,QCMPSTCD),CHAR,TEXT=(*,CPSTCLIT),COL=*          
                                                                                
C_Token  LKREQ F,300,(D,B#SAVED,QC_TOKEN),CHAR,TEXT=(*,CTOKNLIT),COL=*          
CRecAct  LKREQ F,350,(D,B#SAVED,QRECACTN),CHAR,TEXT=(*,RECACLIT),COL=*          
                                                                                
         LKREQ E                                                                
                                                                                
MEDCDLIT DC    C'Media Code'                                                    
CCLTCLIT DC    C'Client Code'                                                   
CCLTNLIT DC    C'Client Name'                                                   
CBILRLIT DC    C'Bill Receipt Name'                                             
CADR1LIT DC    C'Address Line 1'                                                
CADR2LIT DC    C'Address Line 2'                                                
CATTNLIT DC    C'Attention'                                                     
CDIVNLIT DC    C'Client Divisions'                                              
CBDCOLIT DC    C'Bill. Date Calc. Ovrd'                                         
CBDBDLIT DC    C'Bill. Date - Base Dte'                                         
CBDDALIT DC    C'Bill. Date - Date Adj'                                         
CNNAJLIT DC    C'Non-newspaper Adj'                                             
CCTYPLIT DC    C'Client Type'                                                   
CMASTLIT DC    C'Master Clt Code'                                               
CPEBFLIT DC    C'Prd/Est Bill. Formula'                                         
CCONRLIT DC    C'Contract Required'                                             
CBUDTLIT DC    C'Budget Amount Type'                                            
CESTRLIT DC    C'Estimate Rounding'                                             
COFFCLIT DC    C'Office Code'                                                   
CGSTCLIT DC    C'Canadian GST Code'                                             
CBGRPLIT DC    C'Billing Group Code'                                            
CCLT#LIT DC    C'Client Number'                                                 
CACCOLIT DC    C'Acc Office Agency'                                             
                                                                                
CSTA0LIT DC    C'X80 Est UCOM Bill. Cnt'                                        
CSTA1LIT DC    C'X80 Midas Client'                                              
CSTA2LIT DC    C'X20 Prd Office Allowed'                                        
CSTA3LIT DC    C'X10 Frozen Clt w/ Date'                                        
CSTA4LIT DC    C'X08 Cost 2 Factor flag'                                        
CSTA5LIT DC    C'X04 Cost 2 Dollar flag'                                        
CSTA6LIT DC    C'X02 Frozen Client'                                             
CSTA7LIT DC    C'X01 SFH'                                                       
                                                                                
CFINCLIT DC    C'Financial Client'                                              
CAOFCLIT DC    C'Acc Office Code'                                               
CCSC#LIT DC    C'Contract Standard Com#'                                        
CIOC#LIT DC    C'I/O Comment Number'                                            
CAGYRLIT DC    C'Agency of Record'                                              
CADVRLIT DC    C'Advertiser'                                                    
CADVCLIT DC    C'Advertiser Client Code'                                        
CAORSLIT DC    C'AOR Start Date'                                                
CAORELIT DC    C'AOR End Date'                                                  
CAOR#LIT DC    C'AOR SE Number'                                                 
CAORILIT DC    C'AOR Issue Rec Date Req'                                        
CAORCLIT DC    C'AOR Contract Lck/Unlck'                                        
CAORDLIT DC    C'AOR Contract Display'                                          
CADVSLIT DC    C'ADV Schedule Checking'                                         
CBCRLLIT DC    C'BUY Contract Rate'                                             
CCCLLLIT DC    C'CON Contract Level'                                            
CCCRLLIT DC    C'CON Contract Rate'                                             
CPUBLLIT DC    C'Pub Link Required'                                             
CNBAALIT DC    C'No Brand Agency Access'                                        
                                                                                
CPF1DLIT DC    C'Prd Fld 1 Description'                                         
CPF1TLIT DC    C'Prd Fld 1 Type'                                                
CPF1LLIT DC    C'Prd Fld 1 Length'                                              
CPF10LIT DC    C'PF1 - Required'                                                
CPF11LIT DC    C'PF1 - Show on 52'                                              
CPF12LIT DC    C'PF1 - Show on EC'                                              
CPF13LIT DC    C'PF1 - Show on Bills'                                           
CPF14LIT DC    C'PF1 - Show on MX'                                              
CPF15LIT DC    C'PF1 - Headline'                                                
CPF16LIT DC    C'PF1 - Show in front'                                           
CPF17LIT DC    C'PF1 - Not Used'                                                
                                                                                
CPF2DLIT DC    C'Prd Fld 2 Description'                                         
CPF2TLIT DC    C'Prd Fld 2 Type'                                                
CPF2LLIT DC    C'Prd Fld 2 Length'                                              
CPF20LIT DC    C'PF2 - Required'                                                
CPF21LIT DC    C'PF2 - Show on 52'                                              
CPF22LIT DC    C'PF2 - Show on EC'                                              
CPF23LIT DC    C'PF2 - Show on Bills'                                           
CPF24LIT DC    C'PF2 - Show on MX'                                              
CPF25LIT DC    C'PF2 - Headline'                                                
CPF26LIT DC    C'PF2 - Show in front'                                           
CPF27LIT DC    C'PF2 - Not Used'                                                
                                                                                
CEF1DLIT DC    C'Est Fld 1 Description'                                         
CEF1TLIT DC    C'Est Fld 1 Type'                                                
CEF1LLIT DC    C'Est Fld 1 Length'                                              
CEF10LIT DC    C'EF1 - Required'                                                
CEF11LIT DC    C'EF1 - Show on 52'                                              
CEF12LIT DC    C'EF1 - Show on EC'                                              
CEF13LIT DC    C'EF1 - Show on Bills'                                           
CEF14LIT DC    C'EF1 - Show on MX'                                              
CEF15LIT DC    C'EF1 - Headline'                                                
CEF16LIT DC    C'EF1 - Show in front'                                           
CEF17LIT DC    C'EF1 - Required on billing'                                     
                                                                                
CEF2DLIT DC    C'Est Fld 2 Description'                                         
CEF2TLIT DC    C'Est Fld 2 Type'                                                
CEF2LLIT DC    C'Est Fld 2 Length'                                              
CEF20LIT DC    C'EF2 - Required'                                                
CEF21LIT DC    C'EF2 - Show on 52'                                              
CEF22LIT DC    C'EF2 - Show on EC'                                              
CEF23LIT DC    C'EF2 - Show on Bills'                                           
CEF24LIT DC    C'EF2 - Show on MX'                                              
CEF25LIT DC    C'EF2 - Headline'                                                
CEF26LIT DC    C'EF2 - Show in front'                                           
CEF27LIT DC    C'EF2 - Required on billing'                                     
                                                                                
CPSTCLIT DC    C'PST Codes'                                                     
CDRDCLIT DC    C'DRD Client Code'                                               
CZENCLIT DC    C'Zen Client Code'                                               
CMEDNLIT DC    C'Media Name Override'                                           
CC2FALIT DC    C'Cost 2 Factor'                                                 
CRFPGLIT DC    C'T/A RFP Group Code'                                            
                                                                                
CICB4LIT DC    C'Lock from this month+'                                         
CICB5LIT DC    C'Lock from this month-'                                         
CICB6LIT DC    C'Lock this month only'                                          
                                                                                
CFRZDLIT DC    C'Freeze Date'                                                   
CTRFCLIT DC    C'Traffic Office Code'                                           
                                                                                
CTRSHLIT DC    C'Trash'                                                         
                                                                                
CTOKNLIT DC    C'Token'                                                         
RECACLIT DC    C'Record Action'                                                 
                                                                                
CLTRECU  CLI   FATALERR,C'Y'       Fatal error occurred?                        
         JE    EXITY                                                            
         MVI   SVREQTYP,CLTREC_Q   Set to client record upload                  
         MVI   SVOFCCOD,0          Init saved client office code                
         MVI   WKOFCCOD,0          Iniit validated client office code           
         GOTOR R_INI                                                            
         GOTOR BLDCKEY             Build client key                             
         GOTOR INIREC,IOKEY        Init record                                  
         GOTOR VALCFLD             Validate client fields                       
         JNE   CLTRECUX                                                         
         GOTOR BLDCELM             Build client elements                        
         CLI   QRECACTN,QRA_ADDQ   Record action add?                           
         JE    CLTRECU4                                                         
         CLI   QRECACTN,QRA_CHGQ   Record action change?                        
         JE    CLTRECU6                                                         
         DC    H'0'                                                             
                                                                                
CLTRECU4 GOTOR R_ADD                                                            
         J     CLTRECU8                                                         
                                                                                
CLTRECU6 GOTOR R_GET                                                            
         JNE   CLTRECUX                                                         
*                                                                               
         L     RE,AIO1                                                          
         LA    RE,(PCLTELEM-PCLTREC)(RE)                                        
         USING PCLTELEM,RE                                                      
         CLI   PCLTELEM,X'02'                                                   
         JE    *+6                                                              
         DC    H'0'                Bad client record                            
         MVC   SVOFCCOD,PCLTOFF    Save client office code                      
         DROP  RE                                                               
*                                                                               
         GOTOR ADJCELEM            Adjust client record elements                
*                                                                               
         GOTOR R_PUT                                                            
*                                                                               
CLTRECU8 GOTOR R_PPV               Process passive pointers                     
                                                                                
CLTRECUX J     EXITY                                                            
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
R_INI    MVC   QAGY,LP_AGY                                                      
         XC    ERRNUM,ERRNUM                                                    
         BR    RE                                                               
                                                                                
R_GET    LR    R0,RE                                                            
         XC    ERRNUM,ERRNUM                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOPRTDIR+IO1'                         
         JE    R_GET02                                                          
         MVI   SVERRIND,0                                                       
         GOTOR RPYERR                                                           
         J     R_GETX                                                           
R_GET02  GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOPRTFIL+IO1'                        
R_GETX   CLI   ERRNUM,0            Error?  To set condition code                
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
R_ADD    LR    R0,RE                                                            
         XC    ERRNUM,ERRNUM                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTFIL+IO3'                            
         JE    R_ADD05                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOPRTFIL+IO3'                           
         JE    R_ADD10                                                          
R_ADD05  MVI   SVERRIND,0                                                       
         GOTOR RPYERR                                                           
         J     R_ADDX                                                           
R_ADD10  L     RE,AIO3                                                          
         XC    SVIOKEY_,SVIOKEY_                                                
         MVC   SVIOKEY_(25),0(RE)                                               
         MVC   SVIOKEY_+27(4),IODA                                              
R_ADDX   CLI   ERRNUM,0            Error?  To set condition code                
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
R_PUT    ST    RE,SAVE_RE                                                       
         L     R0,AIO1             Copy updated record to AIO1                  
         LHI   R1,IOLENQ                                                        
         L     RE,AIO3                                                          
         LHI   RF,IOLENQ                                                        
         MVCL  R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOPRTFIL+IO1'                        
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AIO1                                                          
         XC    SVIOKEY_,SVIOKEY_                                                
         MVC   SVIOKEY_(25),0(RE)                                               
         MVC   SVIOKEY_+27(4),IODA                                              
         L     RE,SAVE_RE                                                       
         BR    RE                                                               
                                                                                
R_DEL    DS    0H                                                               
         BR    RE                                                               
                                                                                
R_PPV    ST    RE,SAVE_RE                                                       
         MVC   WKIOKEY_,IOKEY                                                   
         OC    SVIOKEY_+27(4),SVIOKEY_+27                                       
         JZ    R_PPV_X                                                          
         CLI   SVREQTYP,CLTREC_Q   Client record upload?                        
         JNE   R_PPV_X                                                          
         CLC   SVOFCCOD,WKOFCCOD   Client office code changed?                  
         JE    R_PPV_X                                                          
*                                                                               
         CLI   SVOFCCOD,C' '       Need to delete old passive?                  
         JNH   R_PPV30                                                          
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING POFCKEY,RE                                                       
         MVC   POFCKAGY,QAGY                                                    
         MVC   POFCKMED,QCLTKMED                                                
         MVI   POFCKRCD,POFCKIDQ                                                
         MVC   POFCKCLT,QCLTKCLT                                                
         MVC   POFCKOFF,SVOFCCOD                                                
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOPRTDIR+IO3'                           
         CLC   IOKEY(L'POFCKEY),IOKEYSAV                                        
         JNE   R_PPV30                                                          
         TM    IOKEY+25,X'80'      Already deleted?                             
         JO    R_PPV30                                                          
         OI    IOKEY+25,X'80'      Flag for deletion                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOPRTDIR'                             
*                                                                               
R_PPV30  CLI   WKOFCCOD,C' '       Have validated client office code?           
         JNH   R_PPV_X                                                          
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING POFCKEY,RE                                                       
         MVC   POFCKAGY,QAGY                                                    
         MVC   POFCKMED,QCLTKMED                                                
         MVI   POFCKRCD,POFCKIDQ                                                
         MVC   POFCKCLT,QCLTKCLT                                                
         MVC   POFCKOFF,WKOFCCOD                                                
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOPRTDIR+IO3'                           
         CLC   IOKEY(L'POFCKEY),IOKEYSAV                                        
         JNE   R_PPV40                                                          
         TM    IOKEY+25,X'80'      Need to undelete?                            
         JZ    R_PPV50                                                          
         NI    IOKEY+25,X'FF'-X'80'                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOPRTDIR'                             
         J     R_PPV50                                                          
*                                                                               
R_PPV40  MVC   IOKEY,IOKEYSAV                                                   
         MVC   IOKEY+27(4),SVIOKEY_+27                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOPRTDIR'                               
*                                                                               
R_PPV50  DS    0H                                                               
*                                                                               
R_PPV_X  MVC   IOKEY,WKIOKEY_                                                   
         L     RE,SAVE_RE                                                       
         BR    RE                                                               
                                                                                
BLDCKEY  XC    IOKEY,IOKEY         Prepare to build client key                  
         LA    R1,IOKEY                                                         
         USING PCLTKEY,R1                                                       
         MVC   PCLTKAGY,QAGY                                                    
         MVC   PCLTKMED,QCLTKMED                                                
         MVI   PCLTKRCD,PCLTRECQ                                                
         MVC   PCLTKCLT,QCLTKCLT                                                
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
BLDPKEY  XC    IOKEY,IOKEY         Prepare to build product key                 
         LA    R1,IOKEY                                                         
         USING PPRDKEY,R1                                                       
         MVC   PPRDKAGY,QAGY                                                    
         MVC   PPRDKMED,QPRDKMED                                                
         MVI   PPRDKRCD,PPRDRECQ                                                
         MVC   PPRDKCLT,QPRDKCLT                                                
         MVC   PPRDKPRD,QPRDKPRD                                                
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
INIREC   STCM  RE,15,DUB+0         Initialize a record                          
         STCM  R1,15,DUB+4                                                      
         L     R0,AIO3                                                          
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ICM   RE,15,DUB+0                                                      
         ICM   R1,15,DUB+4                                                      
         L     RF,AIO3                                                          
         MVC   0(L'PCLTKEY,RF),0(R1)                                            
         LHI   R0,PCLTELEM+1-PCLTKEY                                            
         STCM  R0,3,PCLTLEN-PCLTRECD(RF)                                        
         BR    RE                                                               
*                                                                               
ADJCELEM ST    RE,SAVE_RE                                                       
         L     RF,AIO1             Point to original client record              
         LA    RF,PCLTELEM-PCLTREC(RF)                                          
ADJCEL10 CLI   0(RF),0                                                          
         JE    ADJCEL20                                                         
         CLI   0(RF),PCLTPOEQ      Have billed by PO# element?                  
         JE    ADJCEL12                                                         
         LLC   R0,1(RF)                                                         
         AR    RF,R0                                                            
         J     ADJCEL10                                                         
ADJCEL12 L     R1,AIO3             Point to updated client record               
         LA    R1,PCLTELEM-PCLTREC(R1)                                          
ADJCEL14 CLI   0(R1),0                                                          
         JE    ADJCEL16                                                         
         LLC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         J     ADJCEL14                                                         
ADJCEL16 CLI   1(RF),PCLTPOLQ      PO# element without override name?           
         JNE   *+14                                                             
         MVC   0(PCLTPOLQ,R1),0(RF)                                             
         J     ADJCEL18                                                         
         CLI   1(RF),PCLTPOXQ      PO# element with override name?              
         JNE   *+14                                                             
         MVC   0(PCLTPOXQ,R1),0(RF)                                             
         J     ADJCEL18                                                         
         DC    H'0'                Bad element length                           
ADJCEL18 L     R1,AIO3                                                          
         LLC   R0,1(RF)            Length of PO# element just copied            
         LLH   RE,PCLTLEN-PCLTRECD(R1)                                          
         AR    RE,R0                                                            
         STCM  RE,3,PCLTLEN-PCLTRECD(R1)                                        
*                                                                               
ADJCEL20 DS    0H                  For future element adjustments               
*                                                                               
ADJCEL_X L     RE,SAVE_RE                                                       
         BR    RE                                                               
*                                                                               
RPYERR   NTR1                                                                   
         CLC   LP_QMAPN,=AL2(M#UL_CLT)                                          
         JNE   RPYERR12                                                         
         LHI   RF,339                                                           
         MVC   SVREQTOK,QC_TOKEN                                                
         CLI   SVERRIND,YESQ       Have error fld & msg?                        
         JE    RPYERR22                                                         
         LHI   RE,350              Record action                                
         STCM  RE,3,SVERRFLD                                                    
         XC    SVERRMSG,SVERRMSG                                                
         CLI   QRECACTN,QRA_ADDQ                                                
         JNE   *+10                                                             
         MVC   SVERRMSG(L'TXERRADD),TXERRADD                                    
         CLI   QRECACTN,QRA_CHGQ                                                
         JNE   *+10                                                             
         MVC   SVERRMSG(L'TXERRCHG),TXERRCHG                                    
         J     RPYERR22                                                         
                                                                                
RPYERR12 CLC   LP_QMAPN,=AL2(M#UL_PRD)                                          
         JNE   RPYERR18                                                         
         LHI   RF,349                                                           
         MVC   SVREQTOK,QP_TOKEN                                                
         CLI   SVERRIND,YESQ       Have error fld & msg?                        
         JE    RPYERR22                                                         
         LHI   RE,350              Record action                                
         STCM  RE,3,SVERRFLD                                                    
         XC    SVERRMSG,SVERRMSG                                                
         CLI   QRECACTN,QRA_ADDQ                                                
         JNE   *+10                                                             
         MVC   SVERRMSG(L'TXERRADD),TXERRADD                                    
         CLI   QRECACTN,QRA_CHGQ                                                
         JNE   *+10                                                             
         MVC   SVERRMSG(L'TXERRCHG),TXERRCHG                                    
         J     RPYERR22                                                         
RPYERR18 DC    H'0'                                                             
                                                                                
RPYERR22 TM    RPYIND1,RPYHDRTQ                                                 
         JNZ   RPYERR24                                                         
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',(RF))                   
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',300),          +        
               ('LD_CHARQ',SVREQTOK),(L'SVREQTOK,0)                             
         OI    RPYIND1,RPYHDRTQ                                                 
RPYERR24 GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',311),          +        
               ('LD_UBINQ',SVERRFLD),(L'SVERRFLD,0)                             
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',312),          +        
               ('LD_CHARQ',SVERRMSG),(L'SVERRMSG,0)                             
         MVI   ERRNUM,X'FF'        Error is logged                              
         MVI   FATALERR,C'Y'       Error occurred                               
         J     EXIT                                                             
                                                                                
***********************************************************************         
* CFM Product record upload                                           *         
***********************************************************************         
                                                                                
         DS    0H                                                               
PRDCFMU  LKREQ H,M#UL_PRD,NEXTREQ=REQEND,ROUTINE=PRDRECU,NEWREC=Y               
                                                                                
PMedCod  LKREQ F,001,(D,B#SAVED,QPRDKMED),CHAR,TEXT=(*,MEDCDLIT),COL=*          
PCltCod  LKREQ F,002,(D,B#SAVED,QPRDKCLT),CHAR,TEXT=(*,CCLTCLIT),COL=*          
PPrdCod  LKREQ F,003,(D,B#SAVED,QPRDKPRD),CHAR,TEXT=(*,PPRDCLIT),COL=*          
PPrdNam  LKREQ F,004,(D,B#SAVED,QPRDNAME),CHAR,TEXT=(*,PPRDNLIT),COL=*          
PBilNam  LKREQ F,005,(D,B#SAVED,QPRDBILL),CHAR,TEXT=(*,CBILRLIT),COL=*          
PAdrLn1  LKREQ F,006,(D,B#SAVED,QPRDLIN1),CHAR,TEXT=(*,CADR1LIT),COL=*          
PAdrLn2  LKREQ F,007,(D,B#SAVED,QPRDLIN2),CHAR,TEXT=(*,CADR2LIT),COL=*          
PAttntn  LKREQ F,008,(D,B#SAVED,QPRDATTN),CHAR,TEXT=(*,CATTNLIT),COL=*          
PDivisn  LKREQ F,009,(D,B#SAVED,QPRDDIV),CHAR,TEXT=(*,PDIVCLIT),COL=*           
PAcctNo  LKREQ F,010,(D,B#SAVED,QPRDACCT),CHAR,TEXT=(*,PACC#LIT),COL=*          
PExclus  LKREQ F,011,(D,B#SAVED,QPRDEXCL),CHAR,TEXT=(*,PEXCCLIT),COL=*          
POANAgy  LKREQ F,012,(D,B#SAVED,QPRDOAN),CHAR,TEXT=(*,POANCLIT),COL=*           
PBilRN2  LKREQ F,013,(D,B#SAVED,QPRDBIL2),CHAR,TEXT=(*,PBRN2LIT),COL=*          
PGSTTax  LKREQ F,014,(D,B#SAVED,QPRDGST),CHAR,TEXT=(*,CGSTCLIT),COL=*           
                                                                                
PBilBsA  LKREQ F,020,(D,B#SAVED,QPBILP00),UBIN,TEXT=(*,PBBSALIT),COL=*          
PBilBsB  LKREQ F,021,(D,B#SAVED,QPBILP01),UBIN,TEXT=(*,PBBSBLIT),COL=*          
PBilPAj  LKREQ F,022,(D,B#SAVED,QPBILP02),CBIN,TEXT=(*,PPCTALIT),COL=*          
PAdjDat  LKREQ F,023,(D,B#SAVED,QPBILP05),BMON,TEXT=(*,PAJDTLIT),COL=*          
PColPrt  LKREQ F,024,(D,B#SAVED,QPBILP09),CHAR,TEXT=(*,PCOLPLIT),COL=*          
PBilFPc  LKREQ F,025,(D,B#SAVED,QPBILP10),CBIN,TEXT=(*,PBFPCLIT),COL=*          
PBilCmm  LKREQ F,026,(I,B#SAVED,QPBCIND),CHAR,OLEN=6,LIST=F,SORT=N,    +        
               TEXT=(*,PCOM#LIT),COL=*                                          
PComOnl  LKREQ F,027,(D,B#SAVED,QPBILP34),CHAR,TEXT=(*,PCOMSLIT),COL=*          
PN4BilS  LKREQ F,028,(D,B#SAVED,QPBILP35),CHAR,TEXT=(*,PN4BSLIT),COL=*          
PBilFCB  LKREQ F,029,(D,B#SAVED,QPBILP36),UBIN,TEXT=(*,PBFCBLIT),COL=*          
PBilSta  LKREQ F,030,(I,B#SAVED,QPBSIND),CHAR,OLEN=3,LIST=F,SORT=N,    +        
               TEXT=(*,PBLS#LIT),COL=*                                          
PBilCmm  LKREQ F,031,(I,B#SAVED,QPBTIND),CHAR,OLEN=4,LIST=F,SORT=N,    +        
               TEXT=(*,PBLT#LIT),COL=*                                          
                                                                                
PExCl80  LKREQ F,040,(D,B#SAVED,QEXCLX80),CHAR,TEXT=(*,PEXBELIT),COL=*          
PExCl40  LKREQ F,041,(D,B#SAVED,QEXCLX40),CHAR,TEXT=(*,PEXWILIT),COL=*          
PExCl20  LKREQ F,042,(D,B#SAVED,QEXCLX20),CHAR,TEXT=(*,PEXLILIT),COL=*          
PExCl10  LKREQ F,043,(D,B#SAVED,QEXCLX10),CHAR,TEXT=(*,PEXTOLIT),COL=*          
PExCl08  LKREQ F,044,(D,B#SAVED,QEXCLX08),CHAR,TEXT=(*,PEXCILIT),COL=*          
                                                                                
PNoTr20  LKREQ F,050,(D,B#SAVED,QPSTAX20),CHAR,TEXT=(*,PNOTRLIT),COL=*          
                                                                                
PPOffCd  LKREQ F,060,(D,B#SAVED,QPRDOFFC),CHAR,TEXT=(*,PPOFCLIT),COL=*          
PTrffCd  LKREQ F,061,(D,B#SAVED,QPRDTRAF),CHAR,TEXT=(*,PPTOCLIT),COL=*          
                                                                                
PPrdUr1  LKREQ F,062,(D,B#SAVED,QPRDUSR1),CHAR,TEXT=(*,PUD_1LIT),COL=*          
PPrdUr2  LKREQ F,063,(D,B#SAVED,QPRDUSR2),CHAR,TEXT=(*,PUD_2LIT),COCPST         
PPSTCod  LKREQ F,064,(D,B#SAVED,QPPSTOLD),CHAR,TEXT=(*,CPSTCLIT),COL=*          
PIntCod  LKREQ F,065,(D,B#SAVED,QINTFCOD),CHAR,TEXT=(*,PINFCLIT),COL=*          
PAccOAg  LKREQ F,066,(D,B#SAVED,QACCOFCA),CHAR,TEXT=(*,PAOAYLIT),COL=*          
PAccOCd  LKREQ F,067,(D,B#SAVED,QACCOFCC),CHAR,TEXT=(*,PAOCDLIT),COL=*          
PRotOrd  LKREQ F,068,(D,B#SAVED,QROTAORD),CHAR,TEXT=(*,PROTOLIT),COL=*          
PEffDat  LKREQ F,069,(D,B#SAVED,QEFFDATE),BMON,TEXT=(*,PBPCELIT),COL=*          
PBPCPid  LKREQ F,070,(D,B#SAVED,QBPC_PID),CHAR,TEXT=(*,PBPCPLIT),COL=*          
PBPCLCD  LKREQ F,071,(D,B#SAVED,QBPCLCDT),BDAT,TEXT=(*,PBPCDLIT),COL=*          
                                                                                
PPSTBC   LKREQ F,220,(D,B#SAVED,QPPST0BC),CHAR,TEXT=(*,CPSTCLIT),COL=*          
PPSTAL   LKREQ F,221,(D,B#SAVED,QPPST1AL),CHAR,TEXT=(*,CPSTCLIT),COL=*          
PPSTSA   LKREQ F,222,(D,B#SAVED,QPPST2SA),CHAR,TEXT=(*,CPSTCLIT),COL=*          
PPSTMA   LKREQ F,223,(D,B#SAVED,QPPST3MA),CHAR,TEXT=(*,CPSTCLIT),COL=*          
PPSTON   LKREQ F,224,(D,B#SAVED,QPPST4ON),CHAR,TEXT=(*,CPSTCLIT),COL=*          
PPSTPQ   LKREQ F,225,(D,B#SAVED,QPPST5PQ),CHAR,TEXT=(*,CPSTCLIT),COL=*          
PPSTNB   LKREQ F,226,(D,B#SAVED,QPPST6NB),CHAR,TEXT=(*,CPSTCLIT),COL=*          
PPSTNS   LKREQ F,227,(D,B#SAVED,QPPST7NS),CHAR,TEXT=(*,CPSTCLIT),COL=*          
PPSTPE   LKREQ F,228,(D,B#SAVED,QPPST8PE),CHAR,TEXT=(*,CPSTCLIT),COL=*          
PPSTNF   LKREQ F,229,(D,B#SAVED,QPPST9NF),CHAR,TEXT=(*,CPSTCLIT),COL=*          
                                                                                
PMPSTC   LKREQ F,230,(D,B#SAVED,QPMPSTPC),CHAR,TEXT=(*,CPSTCLIT),COL=*          
PMPSTC   LKREQ F,231,(D,B#SAVED,QPMPSTCD),CHAR,TEXT=(*,CPSTCLIT),COL=*          
                                                                                
P_Token  LKREQ F,300,(D,B#SAVED,QP_TOKEN),CHAR,TEXT=(*,CTOKNLIT),COL=*          
PRecAct  LKREQ F,350,(D,B#SAVED,QRECACTN),CHAR,TEXT=(*,RECACLIT),COL=*          
                                                                                
         LKREQ E                                                                
                                                                                
PPRDCLIT DC    C'Product Code'                                                  
PPRDNLIT DC    C'Product Name'                                                  
PDIVCLIT DC    C'Division Code'                                                 
PACC#LIT DC    C'Account Number'                                                
PEXCCLIT DC    C'Exclusion Code'                                                
POANCLIT DC    C'OAN Agency Code'                                               
PBRN2LIT DC    C'Bill Receipt Name 2'                                           
PBBSALIT DC    C'Billing Base A'                                                
PBBSBLIT DC    C'Billing Base B'                                                
PPCTALIT DC    C'Percentage Adjustment'                                         
PAJDTLIT DC    C'Adjustment Date'                                               
PCOLPLIT DC    C'Columns to Print on Bill'                                      
PBFPCLIT DC    C'Billing Formula Percent'                                       
PCOM#LIT DC    C'Comment Number'                                                
PBLS#LIT DC    C'Bill Controls'                                                 
PBLT#LIT DC    C'Bill Types'                                                    
PCOMSLIT DC    C'Commission Only Switch'                                        
PN4BSLIT DC    C'Not for Billing Switch'                                        
PBFCBLIT DC    C'Bill Formula Commission Basis'                                 
PEXBELIT DC    C'Exclusion Class - Beer'                                        
PEXWILIT DC    C'Exclusion Class - Wine'                                        
PEXLILIT DC    C'Exclusion Class - Liquor'                                      
PEXTOLIT DC    C'Exclusion Class - Tobacco'                                     
PEXCILIT DC    C'Exclusion Class - Cigarettes'                                  
PNOTRLIT DC    C'No Traffic'                                                    
PPOFCLIT DC    C'Product Office Code'                                           
PPTOCLIT DC    C'Product Traffic Office Code'                                   
PUD_1LIT DC    C'User Description 1'                                            
PUD_2LIT DC    C'User Description 2'                                            
PINFCLIT DC    C'Interface Code'                                                
PAOAYLIT DC    C'Acc Office Agency'                                             
PAOCDLIT DC    C'Acc Office Code'                                               
PROTOLIT DC    C'Rotation Order'                                                
PBPCELIT DC    C'BPC Effective Date'                                            
PBPCPLIT DC    C'BPC Pid'                                                       
PBPCDLIT DC    C'BPC date last changed'                                         
                                                                                
PRDRECU  CLI   FATALERR,C'Y'       Fatal error occurred?                        
         JE    EXITY                                                            
         MVI   SVREQTYP,PRDREC_Q   Set to product record upload                 
         GOTOR R_INI                                                            
         GOTOR BLDPKEY             Build product key                            
         GOTOR INIREC,IOKEY        Init record                                  
         GOTOR BLDPELM             Build product elements                       
         GOTOR VALPFLD             Validate product fields                      
         JNE   PRDRECUX                                                         
         CLI   QRECACTN,QRA_ADDQ   Record action add?                           
         JE    PRDRECU4                                                         
         CLI   QRECACTN,QRA_CHGQ   Record action change?                        
         JE    PRDRECU6                                                         
         DC    H'0'                                                             
                                                                                
PRDRECU4 GOTOR R_ADD                                                            
         J     EXITY                                                            
                                                                                
PRDRECU6 GOTOR R_GET                                                            
         JNE   PRDRECUX                                                         
         GOTOR R_ADJ               Adjust fields before saving record           
         GOTOR R_PUT                                                            
PRDRECUX J     EXITY                                                            
                                                                                
REQEND   LKREQ X                                                                
         EJECT                                                                  
                                                                                
EXITN    LHI   RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
VALCFLD  NTR1  BASE=*,LABEL=*      Validate client fields                       
         MVI   SVERRIND,0          Init error switch                            
                                                                                
         MVI   WKOFCCOD,0          Init validated office code                   
         CLC   QCLTOFF_,SPACES                                                  
         JNH   VALCF10                                                          
                                                                                
         XC    OFCBLK,OFCBLK       Init officer block                           
         MVI   OFCSYS,SYSPRTQ      Print system                                 
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC2,QCLTOFF_                                                 
         GOTOR VOFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                        
         MVC   WKOFCCOD,OFCOFC     Save 1 byte internal office code             
         TM    OFCINDS,OFCINOLA    Using 2 chars office code?                   
         JNZ   VALCF10                                                          
         CLI   0(R1),0                                                          
         JE    VALCF10                                                          
         MVI   SVERRIND,YESQ       Set error fld & msg                          
         LHI   RE,055              Map code for Client Office Code              
         STCM  RE,3,SVERRFLD                                                    
         XC    SVERRMSG,SVERRMSG                                                
         MVC   SVERRMSG(L'TXEOFCOD),TXEOFCOD                                    
         GOTOR RPYERR                                                           
         J     EXITN                                                            
                                                                                
VALCF10  DS    0H                                                               
                                                                                
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
VALPFLD  NTR1  BASE=*,LABEL=*      Validate product fields                      
         MVI   SVERRIND,0          Init error switch                            
                                                                                
         L     R2,AIO3            Point to updated product record               
         LA    R2,(PPRDELEM-PPRDREC)(R2)                                        
         USING PPRDELEM,R2                                                      
         LA    RF,PPRDBILP         Point to product bill profile                
         USING BILPROF,RF                                                       
         OC    BILBASA(5),BILBASA  Have billing formula?                        
         JNZ   VALPF10                                                          
         OC    BILADAT,BILADAT     Have billing date?                           
         JZ    VALPF10                                                          
         DROP  R2,RF                                                            
                                                                                
         MVI   SVERRIND,YESQ       Set error fld & msg                          
         LHI   RE,023              Map code for product bill date               
         STCM  RE,3,SVERRFLD                                                    
         XC    SVERRMSG,SVERRMSG                                                
         MVC   SVERRMSG(L'TXBILFOR),TXBILFOR                                    
                                                                                
         DC    H'0'                Dump for now so that ORG display err         
                                                                                
         GOTOR RPYERR                                                           
         J     EXITN                                                            
                                                                                
VALPF10  DS    0H                                                               
                                                                                
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
R_ADJ    NTR1  BASE=*,LABEL=*      Adjust fields before saving record           
                                                                                
         L     R2,AIO1             Point to original record                     
         USING PPRDREC,R2                                                       
         CLI   PPRDKRCD,X'06'      Product record?                              
         JNE   R_ADJ20                                                          
         LA    R2,(PPRDELEM-PPRDREC)(R2)                                        
         MVI   ELCODE,PPBPCECQ                                                  
         BRAS  RE,NXTEL            Find any Billed Planned Cost elem?           
         JNE   R_ADJ20                                                          
         USING PPRDBPCE,R2                                                      
         MVC   SVPBPCEF,PPBPCEFF   Save Billed Planned Cost Eff Date            
         MVC   SVPBPCPI,PPBPCPID   Save last changed PID                        
         MVC   SVPBPCDT,PPBPCCHG   Save last changed date                       
                                                                                
         L     R2,AIO3             Point to new record                          
         LA    R2,(PPRDELEM-PPRDREC)(R2)                                        
         BRAS  RE,NXTEL            Find any Billed Planned Cost elem?           
         JNE   R_ADJ20                                                          
         CLC   SVPBPCEF,PPBPCEFF   Billed PC Eff Date changed?                  
         JNE   R_ADJ20                                                          
         MVC   PPBPCPID,SVPBPCPI   Do not update last changed PID               
         MVC   PPBPCCHG,SVPBPCDT   Do not update last changed date              
                                                                                
R_ADJ20  DS    0H                                                               
                                                                                
         J     EXITY                                                            
                                                                                
NXTEL    SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         JE    NXTELX              CC is equal - found element                  
         CLI   0(R2),0                                                          
         JNE   NXTEL                                                            
         LTR   R2,R2               CC is not equal - element not found          
NXTELX   BR    RE                                                               
                                                                                
         DROP  RB,R2                                                            
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
BLDCELM  NTR1  BASE=*,LABEL=*      Build client elements                        
                                                                                
         L     R2,AIO3                                                          
         MVC   0(L'PCLTKEY,R2),IOKEY                                            
         LLH   RE,PCLTLEN-PCLTRECD(R2)                                          
         STH   RE,WKRECLEN                                                      
                                                                                
         LA    R2,(PCLTELEM-PCLTREC)(R2)                                        
         USING PCLTELEM,R2                                                      
         MVI   PCLTELEM+0,X'02'                                                 
         MVI   PCLTELEM+1,L'PCLTELEM                                            
         MVC   PCLTNAME,QCLTNAME                                                
         CLC   PCLTNAME,SPACES     If no name, default to client code           
         JH    *+10                                                             
         MVC   PCLTNAME(L'QCLTKCLT),QCLTKCLT                                    
         MVC   PCLTBNAM,QCLTBNAM                                                
         MVC   PCLTLIN1,QCLTLIN1                                                
         MVC   PCLTLIN2,QCLTLIN2                                                
         MVC   PCLTATTN,QCLTATTN                                                
         MVC   PCLTPROF,QCLTPROF                                                
                                                                                
         MVC   PCLTOFF,WKOFCCOD                                                 
                                                                                
         MVC   PCLTGST,QCLTGST_                                                 
         MVC   PCLTBLGP,QCLTBLGP                                                
         GOTOR TRNCLT#,QCLTNUMB                                                 
         MVC   PCLTNUM,WORK                                                     
         MVC   PCLTACCA,QCLTACCA                                                
         MVC   PCLTFIN,QCLTFIN_                                                 
         MVC   PCLTAOFC,QCLTAOFC                                                
         GOTOR TRNBITS,QCLTSTAT                                                 
         MVC   PCLTSTAT,BYTE1                                                   
         LA    RE,PCLTSTAT                                                      
         STCM  RE,15,ACLTSTAT      Save address of PCLTSTAT                     
         MVC   SVCLTSTA,PCLTSTAT                                                
         LH    RF,WKRECLEN         Adjust record length                         
         AHI   RF,L'PCLTELEM                                                    
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         CLC   QCONSTCM,SPACES     Have Standard Comment Number?                
         JNH   BLDCE11                                                          
         USING PCLTCSCM,R2                                                      
         MVI   PCLTCSCM,X'10'                                                   
         MVI   1(R2),8                                                          
         MVC   PCLTCNUM,QCONSTCM                                                
         GOTOR COMMCODF,PCLTCNUM   Right align Comment Code                     
         LH    RF,WKRECLEN                                                      
         AHI   RF,8                                                             
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDCE11  SR    RE,RE                                                            
         ICM   RE,7,AIOC           Have I/O Comment numbers?                    
         JZ    BLDCE15                                                          
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RE)                                            
         AHI   RE,LW_LN2Q                                                       
BLDCE11D CHI   R0,0                                                             
         JNH   BLDCE15                                                          
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PCLTISCM,R2                                                      
         MVI   PCLTISCM,X'11'                                                   
         MVI   1(R2),8                                                          
         MVC   PCLTINUM,0(RE)                                                   
         ST    RE,FULL1                                                         
         GOTOR COMMCODF,PCLTINUM   Right align Comment Code                     
         L     RE,FULL1                                                         
         LH    RF,WKRECLEN                                                      
         AHI   RF,8                                                             
         STH   RF,WKRECLEN                                                      
         BCTR  R0,0                                                             
         AHI   RE,L'PCLTINUM                                                    
         J     BLDCE11D                                                         
         DROP  R2                                                               
                                                                                
BLDCE15  CLC   QAORELST(QAORELLN),SPACES                                        
         JNH   BLDCE20                                                          
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PCLTADVE,R2                                                      
         MVI   PCLTADVE,X'15'                                                   
         MVI   1(R2),20                                                         
         MVC   PCLTAOR,QAGYOREC                                                 
         MVC   PCLTADV,QADVTISR                                                 
         MVC   PCLTADVC,QADVTCLT                                                
         MVC   PCLTASDT,QAORSTDT                                                
         MVC   PCLTAEDT,QAORENDT                                                
         MVC   PCLTAORS,QAORSENO                                                
         GOTOR TRNBITS,QAORCBY1                                                 
         MVC   PCLTACON+0(1),BYTE1                                              
         GOTOR TRNBITS,QAORCBY2                                                 
         MVC   PCLTACON+1(1),BYTE1                                              
         LH    RF,WKRECLEN                                                      
         AHI   RF,20                                                            
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDCE20  OC    QCUSERFS(QCUSERFL),QCUSERFS                                      
         JZ    BLDCE25                                                          
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PCLTUDEF,R2                                                      
         MVI   PCLTUDEF,X'20'                                                   
         MVI   1(R2),98                                                         
                                                                                
         MVC   PCLTPU1,QCLTPU1_                                                 
         MVC   PCLTP1TY,QCLTP1TY                                                
         MVC   PCLTP1LN,QCLTP1LN                                                
         GOTOR TRNBITS,QCLTP1F1                                                 
         MVC   PCLTP1F1,BYTE1                                                   
         TM    PCLTP1F1,X'02'      Show on front of bills?                      
         JZ    *+8                                                              
         OI    PCLTP1F1,X'10'      Show on bills                                
         TM    PCLTP1F1,X'04'      Show in headlines on bills?                  
         JZ    *+8                                                              
         OI    PCLTP1F1,X'10'      Show on bills                                
                                                                                
         MVC   PCLTPU2,QCLTPU2_                                                 
         MVC   PCLTP2TY,QCLTP2TY                                                
         MVC   PCLTP2LN,QCLTP2LN                                                
         GOTOR TRNBITS,QCLTP2F1                                                 
         MVC   PCLTP2F1,BYTE1                                                   
         MVC   PCLTP2F1,BYTE1                                                   
         TM    PCLTP2F1,X'02'      Show on front of bills?                      
         JZ    *+8                                                              
         OI    PCLTP2F1,X'10'      Show on bills                                
         TM    PCLTP2F1,X'04'      Show in headlines on bills?                  
         JZ    *+8                                                              
         OI    PCLTP2F1,X'10'      Show on bills                                
                                                                                
         MVC   PCLTEU1,QCLTEU1_                                                 
         MVC   PCLTE1TY,QCLTE1TY                                                
         MVC   PCLTE1LN,QCLTE1LN                                                
         GOTOR TRNBITS,QCLTE1F1                                                 
         MVC   PCLTE1F1,BYTE1                                                   
         TM    PCLTE1F1,X'02'      Show on front of bills?                      
         JZ    *+8                                                              
         OI    PCLTE1F1,X'10'      Show on bills                                
         TM    PCLTE1F1,X'04'      Show in headlines on bills?                  
         JZ    *+8                                                              
         OI    PCLTE1F1,X'10'      Show on bills                                
                                                                                
         MVC   PCLTEU2,QCLTEU2_                                                 
         MVC   PCLTE2TY,QCLTE2TY                                                
         MVC   PCLTE2LN,QCLTE2LN                                                
         GOTOR TRNBITS,QCLTE2F1                                                 
         MVC   PCLTE2F1,BYTE1                                                   
         TM    PCLTE2F1,X'02'      Show on front of bills?                      
         JZ    *+8                                                              
         OI    PCLTE2F1,X'10'      Show on bills                                
         TM    PCLTE2F1,X'04'      Show in headlines on bills?                  
         JZ    *+8                                                              
         OI    PCLTE2F1,X'10'      Show on bills                                
                                                                                
         LH    RF,WKRECLEN                                                      
         AHI   RF,98                                                            
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDCE25  CLC   LP_VRSN,=AL1(01,00,05,00)                                        
         JNL   BLDCE25E                                                         
         CLC   QCPSTOLD,SPACES                                                  
         JNH   BLDCE26                                                          
         LA    R1,QCPSTOLD                                                      
         J     BLDCE25H                                                         
BLDCE25E CLC   QPSTCODE,SPACES                                                  
         JNH   BLDCE26                                                          
         LA    R1,QPSTCODE                                                      
BLDCE25H CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PCLTPST,R2                                                       
         MVI   PCLTPST,X'25'                                                    
         MVI   1(R2),12                                                         
         MVC   PCLTPSTC,0(R1)                                                   
         SR    R0,R0                                                            
         AHI   R0,L'PCLTPSTC                                                    
         LA    RE,PCLTPSTC                                                      
BLDCE25M CLI   0(RE),C' '                                                       
         JH    *+8                                                              
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         JCT   R0,BLDCE25M                                                      
         LH    RF,WKRECLEN                                                      
         AHI   RF,12                                                            
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDCE26  CLC   QCMPSTPC,SPACES                                                  
         JNH   BLDCE30                                                          
         LAY   RE,PSTVTAB                                                       
         XC    TEMP,TEMP                                                        
         LA    R1,TEMP                                                          
BLDCE26G CLI   0(RE),0             End of table?                                
         JE    BLDCE30                                                          
         CLC   QCMPSTPC,0(RE)                                                   
         JE    *+16                                                             
         LA    RE,2(RE)            Next entry in table                          
         LA    R1,1(R1)                                                         
         J     BLDCE26G                                                         
         MVC   0(1,R1),QCMPSTCD                                                 
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PCLTMPS,R2                                                       
         MVI   PCLTMPS,PCLTMPEQ                                                 
         MVI   PCLTMPSL,PCLTMPLQ                                                
         MVC   PCLTMPSC,TEMP                                                    
         LH    RF,WKRECLEN                                                      
         AHI   RF,PCLTMPLQ                                                      
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDCE30  CLC   QDRDCODE,SPACES                                                  
         JNH   BLDCE32                                                          
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PCLTDRD,R2                                                       
         MVI   PCLTDRD,X'30'                                                    
         MVI   PCLTLN,8                                                         
         MVC   PCLTDRDC,QDRDCODE                                                
         LH    RF,WKRECLEN                                                      
         AHI   RF,8                                                             
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDCE32  CLC   QZENCODE,SPACES                                                  
         JNH   BLDCE41                                                          
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PCLTZEL,R2                                                       
         MVI   PCLTZEL,X'32'                                                    
         MVI   PCLTZLEN,5                                                       
         MVC   PCLTZEN,QZENCODE                                                 
         LH    RF,WKRECLEN                                                      
         AHI   RF,5                                                             
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDCE41  CLC   QMEDOVNM,SPACES                                                  
         JNH   BLDCE45                                                          
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PCLTMEL,R2                                                       
         MVI   PCLTMEL,X'41'                                                    
         MVI   PCLTMLEN,12                                                      
         MVC   PCLTMNAM,QMEDOVNM                                                
         LH    RF,WKRECLEN                                                      
         AHI   RF,12                                                            
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDCE45  OC    QCOS2FAC,QCOS2FAC                                                
         JZ    BLDCE46                                                          
                                                                                
* Following code is to get around Print Organizer upload defect                 
                                                                                
         XC    TEMP,TEMP                                                        
         MVI   TEMP+00,C'P'                                                     
         MVC   TEMP+02(2),=C'F0'                                                
         MVC   TEMP+04(L'QAGY),QAGY                                             
         MVC   TEMP+06(L'QCLTKMED),QCLTKMED                                     
         MVC   TEMP+07(L'QCLTKCLT),QCLTKCLT                                     
         CLI   WKOFCCOD,C' '                                                    
         JNH   *+14                                                             
         MVI   TEMP+10,C'*'                                                     
         MVC   TEMP+11(1),WKOFCCOD                                              
         GOTOR VGETPROF,DMCB,TEMP,WORK,VDATAMGR                                 
         CLI   WORK+4,C'$'                                                      
         JNE   *+12                                                             
         NI    SVCLTSTA,X'FF'-X'08'                                             
         J     BLDCE45M                                                         
         CLI   WORK+4,C'F'                                                      
         JE    *+12                                                             
         NI    SVCLTSTA,X'FF'-(X'08'+X'04')                                     
         J     BLDCE45M                                                         
                                                                                
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PCLTCFEL,R2                                                      
         MVI   PCLTCFEL,X'45'                                                   
         MVI   PCLTCFLN,7                                                       
         ZAP   PCLTCF,QCOS2FAC                                                  
         LH    RF,WKRECLEN                                                      
         AHI   RF,7                                                             
         STH   RF,WKRECLEN                                                      
         OI    SVCLTSTA,X'08'                                                   
         NI    SVCLTSTA,X'FF'-X'04'                                             
BLDCE45M L     RE,ACLTSTAT                                                      
         MVC   0(L'PCLTSTAT,RE),SVCLTSTA                                        
         DROP  R2                                                               
                                                                                
BLDCE46  CLC   QRFPCODE,SPACES                                                  
         JNH   BLDCE47                                                          
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PCLTTAEL,R2                                                      
         MVI   PCLTTAEL,X'46'                                                   
         MVI   PCLTTALN,8                                                       
         MVC   PCLTTAGRP,QRFPCODE                                               
         LH    RF,WKRECLEN                                                      
         AHI   RF,8                                                             
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDCE47  OC    QCFRZEST(QCFRZELN),QCFRZEST                                      
         JZ    BLDCE50                                                          
         XC    ELEM2,ELEM2                                                      
         GOTOR TRNBITS,QCFRZIND                                                 
         MVC   ELEM2(L'PCLTFIND),BYTE1                                          
         MVC   ELEM2+L'PCLTFIND(L'PCLTFDTE),QFRZDATE                            
         OC    ELEM2,ELEM2                                                      
         JZ    BLDCE50             Do not add empty element                     
                                                                                
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PCLTFEL,R2                                                       
         MVI   PCLTFEL,X'47'                                                    
         MVI   PCLTFLEN,8                                                       
         MVC   PCLTFIND,BYTE1                                                   
         MVC   PCLTFDTE,QFRZDATE   Binary YM date                               
         LH    RF,WKRECLEN                                                      
         AHI   RF,8                                                             
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDCE50  CLC   QTRAOFCD,SPACES                                                  
         JNH   BLDCE51                                                          
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PCLTTOEL,R2                                                      
         MVI   PCLTTOEL,X'50'                                                   
         MVI   PCLTTOLN,5                                                       
         MVC   PCLTTOFC,QTRAOFCD                                                
         LH    RF,WKRECLEN                                                      
         AHI   RF,5                                                             
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDCE51  DS    0H                                                               
                                                                                
         USING PCLTREC,R2                                                       
BLDCE_X  L     R2,AIO3                                                          
         MVC   PCLTLEN,WKRECLEN                                                 
         J     EXIT                                                             
         DROP  RB,R2                                                            
                                                                                
TRNBITS  MVI   BYTE1,0                                                          
         CLI   0(R1),C'Y'                                                       
         JNE   *+8                                                              
         OI    BYTE1,X'80'                                                      
         CLI   1(R1),C'Y'                                                       
         JNE   *+8                                                              
         OI    BYTE1,X'40'                                                      
         CLI   2(R1),C'Y'                                                       
         JNE   *+8                                                              
         OI    BYTE1,X'20'                                                      
         CLI   3(R1),C'Y'                                                       
         JNE   *+8                                                              
         OI    BYTE1,X'10'                                                      
         CLI   4(R1),C'Y'                                                       
         JNE   *+8                                                              
         OI    BYTE1,X'08'                                                      
         CLI   5(R1),C'Y'                                                       
         JNE   *+8                                                              
         OI    BYTE1,X'04'                                                      
         CLI   6(R1),C'Y'                                                       
         JNE   *+8                                                              
         OI    BYTE1,X'02'                                                      
         CLI   7(R1),C'Y'                                                       
         JNE   *+8                                                              
         OI    BYTE1,X'01'                                                      
         BR    RE                                                               
                                                                                
TRNCLT#  SR    R0,R0                                                            
         LR    RF,R1                                                            
         XC    WORK,WORK                                                        
         CLC   0(L'QCLTNUMB,R1),SPACES                                          
         JNH   TRNCLT#X                                                         
TRNCLT#2 CHI   R0,L'QCLTNUMB                                                    
         JNL   TRNCLT#4                                                         
         CLI   0(R1),C' '                                                       
         JNH   TRNCLT#4                                                         
         AHI   R0,1                                                             
         LA    R1,1(R1)                                                         
         J     TRNCLT#2                                                         
TRNCLT#4 LR    R1,RF                                                            
         CHI   R0,3                                                             
         JH    TRNCLT#6                                                         
         MVC   WORK(3),0(R1)                                                    
         J     TRNCLT#X                                                         
TRNCLT#6 CHI   R0,4                                                             
         JNE   TRNCLT#8                                                         
         PACK  DUB,0(4,R1)                                                      
         OI    DUB+7,X'0F'         Pack without sign                            
         UNPK  WORK(3),DUB+6(2)                                                 
         MVI   WORK,X'FF'                                                       
         J     TRNCLT#X                                                         
TRNCLT#8 CHI   R0,5                                                             
         JNE   *+6                                                              
         DC    H'0'                Bad client number                            
         PACK  DUB,0(5,R1)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,7,WORK                                                        
         OI    WORK,X'80'          Number is in binary                          
TRNCLT#X BR    RE                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
COMMCODF NTR1  BASE=*,LABEL=*      Comment code fix (right align)               
                                                                                
         CLC   0(L'PCLTCNUM,R1),SPACES                                          
         JNH   COMMCF_X                                                         
         MVC   DUB1,SPACES         Init space padded Comment Code               
         SR    R0,R0                                                            
         LR    RE,R1                                                            
         AHI   RE,L'PCLTCNUM-1     Point to last char in comment code           
                                                                                
         LHI   RF,L'PCLTCNUM                                                    
COMMCF10 CLI   0(RE),C' '                                                       
         JH    COMMCF20                                                         
         AHI   R0,1                                                             
         AHI   RE,-1                                                            
         JCT   RF,COMMCF10                                                      
                                                                                
COMMCF20 LA    R2,DUB1                                                          
         AR    R2,R0               Pass padding spaces - right align            
         LHI   RF,L'PCLTCNUM                                                    
         SR    RF,R0               # of non-space comment code chars            
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R2),0(R1)                                                    
         EX    RF,0(RE)                                                         
                                                                                
         MVC   0(L'PCLTCNUM,R1),DUB1                                            
                                                                                
COMMCF_X J     EXIT                                                             
         DROP  RB                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
BLDPELM  NTR1  BASE=*,LABEL=*      Build product elements                       
                                                                                
         L     R2,AIO3                                                          
         MVC   0(L'PPRDKEY,R2),IOKEY                                            
         LLH   RE,PPRDLEN-PPRDRECD(R2)                                          
         STH   RE,WKRECLEN                                                      
                                                                                
         LA    R2,(PPRDELEM-PPRDREC)(R2)                                        
         USING PPRDELEM,R2                                                      
         MVI   PPRDELEM,X'06'                                                   
         MVI   1(R2),L'PPRDELEM                                                 
         MVC   PPRDNAME,QPRDNAME                                                
         CLC   PPRDNAME,SPACES     If no name, default to prd code              
         JH    *+10                                                             
         MVC   PPRDNAME(L'QPRDKPRD),QPRDKPRD                                    
         MVC   PPRDBILL,QPRDBILL                                                
         MVC   PPRDLIN1,QPRDLIN1                                                
         MVC   PPRDLIN2,QPRDLIN2                                                
         MVC   PPRDATTN,QPRDATTN                                                
         MVC   PPRDDIV,QPRDDIV                                                  
                                                                                
         OC    QPRDACCT,SPACES                                                  
         CLC   QPRDACCT,SPACES     Have product account number?                 
         JNH   BLDPE20                                                          
         MVC   PPRDACCT,QPRDACCT                                                
         LA    RE,QPRDACCT+L'QPRDACCT-1                                         
         LHI   R0,L'QPRDACCT                                                    
         CLI   0(RE),C' '          Input is < 5 chars?                          
         JNH   BLDPE20                                                          
         PACK  DUB,QPRDACCT                                                     
         CVB   R0,DUB                                                           
         STCM  R0,7,PPRDACCT+1                                                  
         MVI   PPRDACCT,X'FF'      Flag it as numeric (binary)                  
                                                                                
BLDPE20  GOTOR SETPBILC                                                         
         MVC   PPRDBILP,QPRDBILP                                                
*                                                                               
         LA    RF,PPRDBILP         COPIED FROM PRSFM1F:VR24H                    
         USING BILPROF,RF                                                       
         TM    BILBASA,X'10'       COMMISSION ONLY?                             
         JZ    *+12                                                             
         NI    BILBASA,X'FF'-X'10'                                              
         MVI   BILCMSW,C'C'                                                     
*                                                                               
         TM    BILBASA,X'20'       NOT FOR BILLING?                             
         JZ    *+12                                                             
         NI    BILBASA,X'FF'-X'20'                                              
         MVI   BILNBSW,C'Y'                                                     
         DROP  RF                                                               
*                                                                               
         MVC   PPRDEXCL,QPRDEXCL                                                
         MVC   PPRDOAN,QPRDOAN                                                  
         MVC   PPRDBIL2,QPRDBIL2                                                
         MVC   PPRDGST,QPRDGST                                                  
         GOTOR TRNBITS,QEXCLASS                                                 
         MVC   PPRDEXC,BYTE1                                                    
         GOTOR TRNBITS,QPRDSTAT                                                 
         MVC   PPRDSTAT,BYTE1                                                   
         MVC   PPRDOFFC,QPRDOFFC                                                
         MVC   PPRDTRAF,QPRDTRAF                                                
         LH    RF,WKRECLEN         Adjust record length                         
         AHI   RF,L'PPRDELEM                                                    
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         OC    QPRDUDST(QPRDUDLN),QPRDUDST                                      
         JZ    BLDPE25                                                          
         USING PPRDUDEF,R2                                                      
         MVI   PPRDUDEF,X'08'                                                   
         MVI   1(R2),L'PPRDUDEF                                                 
         MVC   PUSER1,QPRDUSR1                                                  
         MVC   PUSER2,QPRDUSR2                                                  
         LH    RF,WKRECLEN                                                      
         AHI   RF,L'PPRDUDEF                                                    
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDPE25  CLC   LP_VRSN,=AL1(01,00,05,00)                                        
         JNL   BLDPE25E                                                         
         CLC   QPPSTOLD,SPACES                                                  
         JNH   BLDPE26                                                          
         LA    R1,QPPSTOLD                                                      
         J     BLDPE25H                                                         
BLDPE25E CLC   QPRDPSTC,SPACES                                                  
         JNH   BLDPE26                                                          
         LA    R1,QPRDPSTC                                                      
BLDPE25H CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PPRDPST,R2                                                       
         MVI   PPRDPST,X'25'                                                    
         MVI   1(R2),L'PPRDPST                                                  
         MVC   PPRDPSTC,0(R1)                                                   
         SR    R0,R0                                                            
         AHI   R0,L'PPRDPSTC                                                    
         LA    RE,PPRDPSTC                                                      
BLDPE25M CLI   0(RE),C' '                                                       
         JH    *+8                                                              
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         JCT   R0,BLDPE25M                                                      
         LH    RF,WKRECLEN                                                      
         AHI   RF,12                                                            
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDPE26  CLC   QPMPSTPC,SPACES                                                  
         JNH   BLDPE30                                                          
         LAY   RE,PSTVTAB                                                       
         XC    TEMP,TEMP                                                        
         LA    R1,TEMP                                                          
BLDPE26G CLI   0(RE),0             End of table?                                
         JE    BLDPE30                                                          
         CLC   QPMPSTPC,0(RE)                                                   
         JE    *+16                                                             
         LA    RE,2(RE)            Next entry in table                          
         LA    R1,1(R1)                                                         
         J     BLDPE26G                                                         
         MVC   0(1,R1),QPMPSTCD                                                 
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PPRDMPS,R2                                                       
         MVI   PPRDMPS,PPRDMPEQ                                                 
         MVI   PPRDMPSL,PPRDMPLQ                                                
         MVC   PPRDMPSC,TEMP                                                    
         LH    RF,WKRECLEN                                                      
         AHI   RF,PPRDMPLQ                                                      
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDPE30  CLC   QINTFCOD,SPACES                                                  
         JNH   BLDPE35                                                          
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PPRDICEL,R2                                                      
         MVI   PPRDICEL,X'30'                                                   
         MVI   1(R2),L'PPRDICEL                                                 
         MVC   PPRDINFC,QINTFCOD                                                
         LH    RF,WKRECLEN                                                      
         AHI   RF,L'PPRDICEL                                                    
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDPE35  CLC   QPAOCEST(QPAOCELN),SPACES                                        
         JNH   BLDPE40                                                          
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PPRDAOEL,R2                                                      
         MVI   PPRDAOEL,X'35'                                                   
         MVI   1(R2),L'PPRDAOEL                                                 
         MVC   PPRDACCA,QACCOFCA                                                
         MVC   PPRDAOFC,QACCOFCC                                                
         LH    RF,WKRECLEN                                                      
         AHI   RF,L'PPRDAOEL                                                    
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDPE40  CLC   QROTAORD,SPACES                                                  
         JNH   BLDPE45                                                          
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PPRDLWEL,R2                                                      
         MVI   PPRDLWEL,X'40'                                                   
         MVI   1(R2),L'PPRDLWEL                                                 
         MVC   PPRDROTA,QROTAORD                                                
         LH    RF,WKRECLEN                                                      
         AHI   RF,L'PPRDLWEL                                                    
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDPE45  OC    QBPCELST(QBPCELLN),QBPCELST                                      
         JZ    BLDPE46                                                          
         CLI   0(R2),0             Already at end of record?                    
         JE    *+12                                                             
         LLC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         USING PPRDBPCE,R2                                                      
         MVI   PPBPCELC,PPBPCECQ                                                
         MVI   PPBPCELN,PPBPCELQ                                                
         MVC   PPBPCEFF,QEFFDATE                                                
         GOTOR GET_BPID                                                         
         MVC   PPBPCPID,HALF1                                                   
         MVC   PPBPCCHG,QBPCLCDT                                                
         LH    RF,WKRECLEN                                                      
         AHI   RF,PPBPCELQ                                                      
         STH   RF,WKRECLEN                                                      
         DROP  R2                                                               
                                                                                
BLDPE46  DS    0H                                                               
                                                                                
         USING PPRDREC,R2                                                       
BLDPE_X  L     R2,AIO3                                                          
         MVC   PPRDLEN,WKRECLEN                                                 
         J     EXIT                                                             
         DROP  RB,R2                                                            
                                                                                
GET_BPID LR    R0,RE                                                            
         XC    HALF1,HALF1         Binary PID to be returned                    
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTOR (RF),DMCB,(2,0),0,0                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       Have secret code?                            
         JZ    *+10                                                             
         MVC   HALF1,FAPASSWD      Return password ID number                    
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
SETPBILC NTR1                      Set product bill profile                     
                                                                                
         SR    RF,RF                                                            
         ICM   RF,7,APBC           Have product bill comments?                  
         JZ    S_PBC20                                                          
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)                                            
         AHI   RF,LW_LN2Q                                                       
                                                                                
         CHI   R0,0                                                             
         JNH   S_PBC20                                                          
         MVC   QPBILP14,0(RF)      Set 1st bill comment number                  
         CLC   QPBILP14,SPACES                                                  
         JNE   *+10                                                             
         XC    QPBILP14,QPBILP14   Cannot have spaces as comment codes          
         GOTOR COMMCODF,QPBILP14   Right align Comment Code                     
         BCTR  R0,0                                                             
         AHI   RF,6                Bump to next comment number                  
                                                                                
         CHI   R0,0                                                             
         JNH   S_PBC20                                                          
         MVC   QPBILP21,0(RF)      Set 2nd bill comment number                  
         CLC   QPBILP21,SPACES                                                  
         JNE   *+10                                                             
         XC    QPBILP21,QPBILP21   Cannot have spaces as comment codes          
         GOTOR COMMCODF,QPBILP21   Right align Comment Code                     
         BCTR  R0,0                                                             
         AHI   RF,6                Bump to next comment number                  
                                                                                
         CHI   R0,0                                                             
         JNH   S_PBC20                                                          
         MVC   QPBILP28,0(RF)      Set 3rd bill comment number                  
         CLC   QPBILP28,SPACES                                                  
         JNE   *+10                                                             
         XC    QPBILP28,QPBILP28   Cannot have spaces as comment codes          
         GOTOR COMMCODF,QPBILP28   Right align Comment Code                     
         BCTR  R0,0                                                             
         AHI   RF,6                Bump to next comment number                  
                                                                                
S_PBC20  SR    RF,RF                                                            
         ICM   RF,7,APBS           Have product bill controls?                  
         JZ    S_PBC30                                                          
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)                                            
         AHI   RF,LW_LN2Q                                                       
                                                                                
         CHI   R0,0                                                             
         JNH   S_PBC30                                                          
         GOTOR SET_PBS,QPBILP13    Set 1st bill control byte                    
         BCTR  R0,0                                                             
                                                                                
         CHI   R0,0                                                             
         JNH   S_PBC30                                                          
         GOTOR SET_PBS,QPBILP20    Set 2nd bill control byte                    
         BCTR  R0,0                                                             
                                                                                
         CHI   R0,0                                                             
         JNH   S_PBC30                                                          
         GOTOR SET_PBS,QPBILP27    Set 3rd bill control byte                    
         BCTR  R0,0                                                             
                                                                                
S_PBC30  SR    RF,RF                                                            
         ICM   RF,7,APBT           Have product bill types?                     
         JZ    S_PBC_X                                                          
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)                                            
         AHI   RF,LW_LN2Q                                                       
                                                                                
         CHI   R0,0                                                             
         JNH   S_PBC_X                                                          
         GOTOR SET_PBT,QPBILP13    Set 1st bill type                            
         BCTR  R0,0                                                             
                                                                                
         CHI   R0,0                                                             
         JNH   S_PBC_X                                                          
         GOTOR SET_PBT,QPBILP20    Set 2nd bill type                            
         BCTR  R0,0                                                             
                                                                                
         CHI   R0,0                                                             
         JNH   S_PBC_X                                                          
         GOTOR SET_PBT,QPBILP27    Set 3rd bill type                            
         BCTR  R0,0                                                             
                                                                                
S_PBC_X  J     EXIT                                                             
                                                                                
SET_PBS  LHI   R2,3                                                             
SET_PBS2 CLI   0(RF),C'R'          Regular?                                     
         JNE   *+8                                                              
         OI    0(R1),X'80'                                                      
         CLI   0(RF),C'C'          Cash Discount?                               
         JNE   *+8                                                              
         OI    0(R1),X'40'                                                      
         CLI   0(RF),C'A'          Adjustment?                                  
         JNE   *+8                                                              
         OI    0(R1),X'20'                                                      
         LA    RF,1(RF)            Next control byte                            
         JCT   R2,SET_PBS2                                                      
SET_PBSX BR    RE                                                               
                                                                                
SET_PBT  LHI   R2,4                                                             
         CLC   =C'ALL',0(RF)                                                    
         JNE   SET_PBT4                                                         
         NI    0(R1),X'FF'-(X'01'+X'02'+X'04'+X'08')                            
         AHI   RF,4                                                             
         J     SET_PBTX                                                         
SET_PBT4 OI    0(R1),X'0F'         Lower 4 bits on for XI instruction           
SET_PBT6 CLI   0(RF),C'4'                                                       
         JNE   *+8                                                              
         XI    0(R1),X'01'                                                      
         CLI   0(RF),C'5'                                                       
         JNE   *+8                                                              
         XI    0(R1),X'02'                                                      
         CLI   0(RF),C'6'                                                       
         JNE   *+8                                                              
         XI    0(R1),X'04'                                                      
         CLI   0(RF),C'7'                                                       
         JNE   *+8                                                              
         XI    0(R1),X'08'                                                      
         LA    RF,1(RF)            Next bill type                               
         JCT   R2,SET_PBT6                                                      
SET_PBTX BR    RE                                                               
                                                                                
PSTVTAB  DC    C'BC'                                                            
         DC    C'AL'                                                            
         DC    C'SA'                                                            
         DC    C'MA'                                                            
         DC    C'ON'                                                            
         DC    C'PQ'                                                            
         DC    C'NB'                                                            
         DC    C'NS'                                                            
         DC    C'PE'                                                            
         DC    C'NF'                                                            
         DC    X'00'               End of table                                 
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
GLOBALS  DS    0D                  ** Global literals **                        
         LTORG ,                                                                
                                                                                
CLTKEYT  LKKEY H,PCLTKEY,SAVED     ** Client key driver **                      
         LKKEY SIN,PCLTKAGY,QAGY                                                
         LKKEY SIN,PCLTKMED,QCLTKMED                                            
         LKKEY LIT,PCLTKRCD,PCLTRECQ                                            
         LKKEY SIN,PCLTKCLT,QCLTKCLT                                            
         LKKEY E                                                                
                                                                                
PRDKEYT  LKKEY H,PPRDKEY,SAVED     ** Product key driver **                     
         LKKEY SIN,PPRDKAGY,QAGY                                                
         LKKEY SIN,PPRDKMED,QPRDKMED                                            
         LKKEY LIT,PPRDKRCD,PPRDRECQ                                            
         LKKEY SIN,PPRDKCLT,QPRDKCLT                                            
         LKKEY SIN,PPRDKPRD,QPRDKPRD                                            
         LKKEY E                                                                
                                                                                
TXERRADD DC    C'Cannot add, record already exist'                              
TXERRCHG DC    C'Cannot change, record not found'                               
TXEOFCOD DC    C'Invalid Office Code'                                           
TXBILFOR DC    C'Cannot have bill date without formula'                         
PZERO    DC    P'0'                                                             
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
DMCOMMIT DC    C'COMMIT  '                                                      
                                                                                
FILES    DS    0X                  ** System/file list **                       
         DC    C'PRINT  '          System for open calls                        
         DC    C'N'                                                             
PRTDIR   DC    C'PRTDIR '                                                       
         DC    C'N'                                                             
PRTFIL   DC    C'PRTFIL '                                                       
         DC    C'N'                                                             
PUBDIR   DC    C'PUBDIR '                                                       
         DC    C'N'                                                             
PUBFIL   DC    C'PUBFIL '                                                       
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'N'                                                             
GENDIR   DC    C'GENDIR '                                                       
         DC    C'N'                                                             
GENFIL   DC    C'GENFIL '                                                       
         DC    C'X'                                                             
                                                                                
NOQ      EQU   C'N'                                                             
YESQ     EQU   C'Y'                                                             
SYSPRTQ  EQU   C'P'                Print system letter                          
EOR      EQU   0                   End of record element code                   
         EJECT                                                                  
                                                                                
SAVED    DSECT ,                   ** Saved storage **                          
                                                                                
AMASTC   DS    A                   A(MASTC)                                     
LINKIO   DS    A                   A(LINKIO)                                    
ALIOB    DS    A                   A(LINKIO CONTROL BLOCK)                      
RECUP    DS    A                   A(RECUP)                                     
                                                                                
ALASTELM DS    A                   A(last element)                              
                                                                                
ACLTSTAT DS    A                   A(PCLTSTAT)                                  
                                                                                
SAVE_RE  DS    F                                                                
WKRECLEN DS    H                   Record length                                
                                                                                
RUNINDS  DS    X                   Local indicators                             
RUNINIT  EQU   X'01'               1st time building disk address list          
                                                                                
RPYIND1  DS    X                                                                
RPYHDRTQ EQU   X'80'               Header and token are replied                 
                                                                                
MQSVR    DS    XL(L'LP_MQSVR)      External service call                        
                                                                                
SVREQTYP DS    XL1                 Request type                                 
CLTREC_Q EQU   C'C'                Client record upload                         
PRDREC_Q EQU   C'P'                Product record upload                        
                                                                                
SVERRIND DS    XL1                 Error indicator                              
SVREQTOK DS    CL(L'QC_TOKEN)                                                   
SVERRFLD DS    AL2                 Map code of error field                      
SVERRMSG DS    CL60                Error message                                
SVCLTSTA DS    XL(L'PCLTSTAT)                                                   
SVOFCCOD DS    XL(L'PCLTOFF)       Saved client office code                     
SVIOKEY_ DS    XL31                Length of Print directory key                
                                                                                
WKOFCCOD DS    XL(L'PCLTOFF)       Validated client office code                 
WKIOKEY_ DS    XL(L'IOKEY)         Working IO key                               
                                                                                
ERRNUM   DS    XL2                 Error number                                 
FATALERR DS    C                                                                
ELCODE   DS    X                   Element code                                 
                                                                                
SVPBPCEF DS    XL(L'PPBPCEFF)      Save Prd Billed PC Eff Date                  
SVPBPCPI DS    XL(L'PPBPCPID)      Save Prd Billed PC last chg'd PID            
SVPBPCDT DS    XL(L'PPBPCCHG)      Save Prd Billed PC last chg'd Date           
                                                                                
OFCBLK   DS    XL(OFCLENQ)         OFFICER block                                
                                                                                
QVALUES  DS    0F                  ** Request values **                         
                                                                                
QAGY     DS    CL(L'LP_AGY)                                                     
QC_TOKEN DS    CL8                 Request token for client record              
QP_TOKEN DS    CL(L'QC_TOKEN)      Request token for Product record             
                                                                                
QRECACTN DS    X                   Record Action                                
QRA_ADDQ EQU   C'A'                Add                                          
QRA_CHGQ EQU   C'C'                Change                                       
                                                                                
Q_TRASH_ DS    C                                                                
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
QCLTVALS DS    0X                  Start of client data fields                  
QCLTKMED DS    CL(L'PCLTKMED)                                                   
QCLTKCLT DS    CL(L'PCLTKCLT)                                                   
QCLTNAME DS    CL(L'PCLTNAME)                                                   
QCLTBNAM DS    CL(L'PCLTBNAM)                                                   
QCLTLIN1 DS    CL(L'PCLTLIN1)                                                   
QCLTLIN2 DS    CL(L'PCLTLIN2)                                                   
QCLTATTN DS    CL(L'PCLTATTN)                                                   
QCLTPROF DS    CL(L'PCLTPROF)                                                   
QCLTOFF_ DS    CL2                 1 or 2 character office code                 
QCLTGST_ DS    CL(L'PCLTGST)                                                    
QCLTBLGP DS    CL(L'PCLTBLGP)                                                   
QCLTNUMB DS    CL5                                                              
QCLTACCA DS    CL(L'PCLTACCA)                                                   
QCLTFIN_ DS    CL(L'PCLTFIN)                                                    
QCLTAOFC DS    CL(L'PCLTAOFC)                                                   
                                                                                
QCLTSTAT DS    0CL8                                                             
QCLTSX80 DS    C                                                                
QCLTSX40 DS    C                                                                
QCLTSX20 DS    C                                                                
QCLTSX10 DS    C                                                                
QCLTSX08 DS    C                                                                
QCLTSX04 DS    C                                                                
QCLTSX02 DS    C                                                                
QCLTSX01 DS    C                                                                
                                                                                
QCONSTCM DS    CL(L'PCLTCNUM)      Contract Standard Comment Number             
                                                                                
QIOCIND  DS    X                   I/O Standard Comment Number                  
AIOC     DS    AL3                                                              
                                                                                
QAORELST DS    0X                                                               
QAGYOREC DS    CL(L'PCLTAOR)       AOR ADV System Control Fields                
QADVTISR DS    CL(L'PCLTADV)                                                    
QADVTCLT DS    CL(L'PCLTADVC)                                                   
QAORSTDT DS    XL(L'PCLTASDT)                                                   
QAORENDT DS    XL(L'PCLTAEDT)                                                   
QAORSENO DS    CL(L'PCLTAORS)                                                   
                                                                                
QAORCBY1 DS    0CL8                                                             
QCBY1X80 DS    C                                                                
QCBY1X40 DS    C                                                                
QCBY1X20 DS    C                                                                
QCBY1X10 DS    C                                                                
QCBY1X08 DS    C                                                                
QCBY1X04 DS    C                                                                
QCBY1X02 DS    C                                                                
QCBY1X01 DS    C                                                                
                                                                                
QAORCBY2 DS    0CL8                                                             
QCBY2X80 DS    C                                                                
QCBY2X40 DS    C                                                                
QCBY2X20 DS    C                                                                
QCBY2X10 DS    C                                                                
QCBY2X08 DS    C                                                                
QCBY2X04 DS    C                                                                
QCBY2X02 DS    C                                                                
QCBY2X01 DS    C                                                                
QAORELLN EQU   *-QAORELST                                                       
                                                                                
QCUSERFS DS    0X                                                               
QCLTPU1_ DS    CL(L'PCLTPU1)       User Definition Fields                       
QCLTP1TY DS    CL(L'PCLTP1TY)                                                   
QCLTP1LN DS    XL(L'PCLTP1LN)                                                   
QCLTP1F1 DS    0CL8                                                             
QP1F1X80 DS    C                                                                
QP1F1X40 DS    C                                                                
QP1F1X20 DS    C                                                                
QP1F1X10 DS    C                                                                
QP1F1X08 DS    C                                                                
QP1F1X04 DS    C                                                                
QP1F1X02 DS    C                                                                
QP1F1X01 DS    C                                                                
                                                                                
QCLTPU2_ DS    CL(L'PCLTPU2)                                                    
QCLTP2TY DS    CL(L'PCLTP2TY)                                                   
QCLTP2LN DS    XL(L'PCLTP2LN)                                                   
QCLTP2F1 DS    0CL8                                                             
QP2F1X80 DS    C                                                                
QP2F1X40 DS    C                                                                
QP2F1X20 DS    C                                                                
QP2F1X10 DS    C                                                                
QP2F1X08 DS    C                                                                
QP2F1X04 DS    C                                                                
QP2F1X02 DS    C                                                                
QP2F1X01 DS    C                                                                
                                                                                
QCLTEU1_ DS    CL(L'PCLTEU1)                                                    
QCLTE1TY DS    CL(L'PCLTE1TY)                                                   
QCLTE1LN DS    XL(L'PCLTE1LN)                                                   
QCLTE1F1 DS    0CL8                                                             
QE1F1X80 DS    C                                                                
QE1F1X40 DS    C                                                                
QE1F1X20 DS    C                                                                
QE1F1X10 DS    C                                                                
QE1F1X08 DS    C                                                                
QE1F1X04 DS    C                                                                
QE1F1X02 DS    C                                                                
QE1F1X01 DS    C                                                                
                                                                                
QCLTEU2_ DS    CL(L'PCLTEU2)                                                    
QCLTE2TY DS    CL(L'PCLTE2TY)                                                   
QCLTE2LN DS    XL(L'PCLTE2LN)                                                   
QCLTE2F1 DS    0CL8                                                             
QE2F1X80 DS    C                                                                
QE2F1X40 DS    C                                                                
QE2F1X20 DS    C                                                                
QE2F1X10 DS    C                                                                
QE2F1X08 DS    C                                                                
QE2F1X04 DS    C                                                                
QE2F1X02 DS    C                                                                
QE2F1X01 DS    C                                                                
                                                                                
QCUSERFL EQU   *-QCUSERFS                                                       
*                                                                               
QCPSTOLD DS    0CL(L'PCLTPSTC)     Old style (need to support it)               
*                                                                               
QPSTCODE DS    0CL(L'PCLTPSTC)                                                  
QCPST0BC DS    CL1                                                              
QCPST1AL DS    CL1                                                              
QCPST2SA DS    CL1                                                              
QCPST3MA DS    CL1                                                              
QCPST4ON DS    CL1                                                              
QCPST5PQ DS    CL1                                                              
QCPST6NB DS    CL1                                                              
QCPST7NS DS    CL1                                                              
QCPST8PE DS    CL1                                                              
QCPST9NF DS    CL1                                                              
*                                                                               
QCMPSTPC DS    CL2                 Main PST province                            
QCMPSTCD DS    CL1                 Main PST code                                
*                                                                               
QDRDCODE DS    CL(L'PCLTDRDC)                                                   
QZENCODE DS    CL(L'PCLTZEN)                                                    
QMEDOVNM DS    CL(L'PCLTMNAM)                                                   
QCOS2FAC DS    PL(L'PCLTCF)                                                     
QRFPCODE DS    CL(L'PCLTTAGRP)                                                  
                                                                                
QCFRZEST DS    0X                                                               
QCFRZIND DS    0CL8                                                             
QFRZIX80 DS    C                                                                
QFRZIX40 DS    C                                                                
QFRZIX20 DS    C                                                                
QFRZIX10 DS    C                                                                
QFRZIX08 DS    C                                                                
QFRZIX04 DS    C                                                                
QFRZIX02 DS    C                                                                
QFRZIX01 DS    C                                                                
                                                                                
QFRZDATE DS    XL2                                                              
QCFRZELN EQU   *-QCFRZEST                                                       
                                                                                
QTRAOFCD DS    CL(L'PCLTTOFC)                                                   
                                                                                
QCLTVALX DS    0X                  End of client data fields                    
QCLTVALL EQU   QCLTVALX-QCLTVALS                                                
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
QPRDVALS DS    0X                 Start of product data fields                  
QPRDKMED DS    CL(L'PPRDKMED)                                                   
QPRDKCLT DS    CL(L'PPRDKCLT)                                                   
QPRDKPRD DS    CL(L'PPRDKPRD)                                                   
                                                                                
QPRDNAME DS    CL(L'PPRDNAME)                                                   
QPRDBILL DS    CL(L'PPRDBILL)                                                   
QPRDLIN1 DS    CL(L'PPRDLIN1)                                                   
QPRDLIN2 DS    CL(L'PPRDLIN2)                                                   
QPRDATTN DS    CL(L'PPRDATTN)                                                   
QPRDDIV  DS    CL(L'PPRDDIV)                                                    
QPRDACCT DS    CL5                                                              
QPRDEXCL DS    CL(L'PPRDEXCL)                                                   
QPRDOAN  DS    CL(L'PPRDOAN)                                                    
QPRDBIL2 DS    CL(L'PPRDBIL2)                                                   
QPRDGST  DS    CL(L'PPRDGST)                                                    
                                                                                
QPRDBILP DS    0XL(L'PPRDBILP)                                                  
QPBILP00 DS    XL1                 Product billing profile fields               
QPBILP01 DS    XL1                                                              
QPBILP02 DS    XL3                                                              
QPBILP05 DS    XL2                                                              
         DS    CL1                                                              
         DS    CL1                                                              
QPBILP09 DS    CL1                                                              
QPBILP10 DS    XL3                                                              
QPBILP13 DS    CL1                                                              
QPBILP14 DS    CL6                                                              
QPBILP20 DS    CL1                                                              
QPBILP21 DS    CL6                                                              
QPBILP27 DS    CL1                                                              
QPBILP28 DS    CL6                                                              
QPBILP34 DS    CL1                                                              
QPBILP35 DS    CL1                                                              
QPBILP36 DS    CL1                                                              
                                                                                
QPBCIND  DS    X                   Billing profile comment array                
APBC     DS    AL3                                                              
QPBSIND  DS    X                   Billing profile status array                 
APBS     DS    AL3                                                              
QPBTIND  DS    X                   Billing profile type array                   
APBT     DS    AL3                                                              
                                                                                
QEXCLASS DS    0CL8                Product exclusion class bits                 
QEXCLX80 DS    C                                                                
QEXCLX40 DS    C                                                                
QEXCLX20 DS    C                                                                
QEXCLX10 DS    C                                                                
QEXCLX08 DS    C                                                                
QEXCLX04 DS    C                                                                
QEXCLX02 DS    C                                                                
QEXCLX01 DS    C                                                                
                                                                                
QPRDSTAT DS    0CL8                Product status byte bits                     
QPSTAX80 DS    C                                                                
QPSTAX40 DS    C                                                                
QPSTAX20 DS    C                   - No Traffic                                 
QPSTAX10 DS    C                                                                
QPSTAX08 DS    C                                                                
QPSTAX04 DS    C                                                                
QPSTAX02 DS    C                                                                
QPSTAX01 DS    C                                                                
                                                                                
QPRDOFFC DS    CL(L'PPRDOFFC)                                                   
QPRDTRAF DS    CL(L'PPRDTRAF)                                                   
                                                                                
QPRDUDST DS    0X                                                               
QPRDUSR1 DS    CL(L'PUSER1)                                                     
QPRDUSR2 DS    CL(L'PUSER2)                                                     
QPRDUDLN EQU   *-QPRDUDST                                                       
*                                                                               
QPPSTOLD DS    0CL(L'PPRDPSTC)     Old style (need to support it)               
*                                                                               
QPRDPSTC DS    0CL(L'PPRDPSTC)                                                  
QPPST0BC DS    CL1                                                              
QPPST1AL DS    CL1                                                              
QPPST2SA DS    CL1                                                              
QPPST3MA DS    CL1                                                              
QPPST4ON DS    CL1                                                              
QPPST5PQ DS    CL1                                                              
QPPST6NB DS    CL1                                                              
QPPST7NS DS    CL1                                                              
QPPST8PE DS    CL1                                                              
QPPST9NF DS    CL1                                                              
*                                                                               
QPMPSTPC DS    CL2                 Main PST province                            
QPMPSTCD DS    CL1                 Main PST code                                
*                                                                               
QINTFCOD DS    CL(L'PPRDINFC)                                                   
                                                                                
QPAOCEST DS    0X                                                               
QACCOFCA DS    CL(L'PPRDACCA)                                                   
QACCOFCC DS    CL(L'PPRDAOFC)                                                   
QPAOCELN EQU   *-QPAOCEST                                                       
                                                                                
QROTAORD DS    CL(L'PPRDROTA)                                                   
                                                                                
QBPCELST DS    0X                                                               
QEFFDATE DS    XL(L'PPBPCEFF)                                                   
QBPC_PID DS    CL8                                                              
QBPCLCDT DS    XL(L'PPBPCCHG)                                                   
QBPCELLN EQU   *-QBPCELST                                                       
                                                                                
QPRDVALX DS    0X                 End of product data fields                    
QPRDVALL EQU   QPRDVALX-QPRDVALS                                                
                                                                                
QVALUEL  EQU   *-QVALUES                                                        
                                                                                
         EJECT                                                                  
                                                                                
* Other included books follow                                                   
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE FALOCKETD                                                      
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PBILPROF                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015PPLNK22   05/19/15'                                      
         END                                                                    
