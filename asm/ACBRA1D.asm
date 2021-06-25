*          DATA SET ACBRA1D    AT LEVEL 016 AS OF 05/14/20                      
*PHASE T6241DA                                                                  
                                                                                
ACBRA1D  TITLE '- BRA Estimates combined download Server'                       
                                                                                
***********************************************************************         
* Level change comments                                               *         
* ---------------------                                               *         
* UK Levels                                                           *         
* ---------                                                           *         
* MPEN 001 16MAY06 Est combined downl to ext database initial version *         
* NSHE 003 19JUL13 Set SETFAC earlier when processing online          *         
* NSHE 004 09JUL14 Set LKREQ X to end requests                        *         
* NSHE 005 25SEP14 <DSRD-2733> Estimate audit download                *         
* NSHE 006 24Apr15 <DSBO-1406> Remove SETFAC routine                  *         
* NSHE 007 26Oct15 <DSRD-9227> Add new approver comments for internal *         
* NSHE     30Nov15 <DSRD-9614> Rewrite item list                      *         
* TKLU 008 27Jan15 <RD010160>  Performance improvements (GETOPT)      *         
* MPEN 009 18May16 <DSMU-0101> New NXTPDF Request                     *         
* MPEN 010 15Jul16 <DSMU-0101> Fix for returned created date          *         
* MPEN 012 03Aug16 <SPEC-2981> Fix for pdfgen created date            *         
* YNGX 013 31Mar17 <DSRD-15058> Fix 2co bug in item download          *         
* NSHE 014 14Feb18 <DSRD-18205> audit changes for electronic sigs     *         
* TKLU 015 07May18 DSPCA-2844   RNSPASD Adjustments                             
*NRAK 016 14Nov19 SPEC-40879 support non-sr debtor a/cs                         
* MPEN     30Jan20 <SPEC-42808> Fix for resend bill on global logon   *         
* MPEN     16Mar20 <DSRD-25729> Estimate internal/external cost brk   *         
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,           x        
               SLOWLIST=SLOWS,WORKERKEY=ACBO,ABENDLIST=FAILS,          x        
               SYSPHASE=X'0624',SYSTEM=ACCSYSQ,APPEND=Y,               x        
               SERVERTYPE=TSTACBO,SEGMENT=Y,LOADFACSOFF=Y,             x        
               BLOCKS=(B#WORKD,WORKD,                                  x        
               B#SAVED,SAVED,                                          x        
               B#TWA,TWAD,                                             x        
               B#AUD,AUDRECD,                                          x        
               B#ITM,ARTRECD,                                          x        
               B#EST,ESTRECD)                                                   
                                                                                
SLOWS    DC    C':'                                                             
FAILS    DC    C':'                                                             
                                                                                
         EJECT                                                                  
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO1D**,CLEAR=YES,RR=RE                                       
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R7,LP_ARUNP         R7=A(RUNPARMS)                               
         USING RUNPARMD,R7                                                      
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING SAVED,R8            R8=A(SAVE W/S)                               
         USING OB_D,GBUFAREA                                                    
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   INIT02                                                           
         L     R9,LP_ABLK1         ONLINE - ROOT PROVIDES WORKD/SAVED           
         ICM   R8,15,RSVRSAVE      R8=A(4K SAVE AREA)                           
         J     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE         A(64K SAVE AREA)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)              BLOCK #1         
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)                                                     
INIT04   ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         MVC   ATWA,LP_ATWA        OFFLINE TWA AREA SET BY RUNNER               
                                                                                
         MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         DROP  R6,R7                                                            
                                                                                
         MVI   TWAMODE,0                                                        
                                                                                
         ST    R5,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         EJECT                                                                  
         L     R1,LP_ARUNP         R1=A(RUNPARMS)                               
         USING RUNPARMD,R1                                                      
         CLI   RUNPMODE,RRUNSTRQ   FIRST FOR RUN                                
         JE    RUNSTR                                                           
         CLI   RUNPMODE,RPRCWRKQ   PROCESS WORK                                 
         JE    PRCWRK                                                           
         CLI   RUNPMODE,RRUNREQQ   RUN REQUEST                                  
         JE    RUNREQ                                                           
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
* INITIALISE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   TM    LP_FLAG,LP_FOFFL    TEST RUNNING OFFLINE                         
         JZ    RUNSTR04                                                         
                                                                                
         L     RF,ACOMFACS         YES - LOAD FACILITIES OVERLAYS               
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,(1,0),0,0                                              
         MVC   AROUT1,0(R1)                                                     
         GOTOR (RF),DMCB,(2,0),0,0                                              
         MVC   AROUT2,0(R1)                                                     
                                                                                
RUNSTR04 GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
         MVC   LP_BLKS+((B#EST-1)*L'LP_BLKS)(L'LP_BLKS),AIO2                    
         MVC   LP_BLKS+((B#AUD-1)*L'LP_BLKS)(L'LP_BLKS),AIO4                    
         OI    LP_AIND1,LP_AICOM   COMMA OK FOR LIST DELIMITERS                 
                                                                                
         MVC   WVALUES(WVALUEL),LVALUES                                         
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         LHI   R0,ADCONSN                                                       
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         A     R2,SRVRRELO                                                      
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   LA    R0,SAVEVAR                                                       
         LHI   R1,SAVEVARL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AGENAXTN         CLEAR GETOPT BUFFER                          
         LHI   R1,L'GENAEXTN                                                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    PRCWRK02                                                         
                                                                                
         LA    R0,WORKD            CLEAR I/O AREAS                              
         AHI   R0,IOAREA1-WORKD                                                 
         LHI   R1,IOAREASL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
PRCWRK02 J     EXITY                                                            
                                                                                
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   MVI   GIND2,GI2EEST                                                    
                                                                                
         GOTOR (#CPYINI,ACPYINI)   (RE)INITIALISE COMPANY VALUES                
         GOTO1 VDICTATE,DMCB,C'LL  ',DCDICTL,DSDICTL                            
         MVC   AGENCY,CUXCPY                                                    
         USING CPXELD,SCPXEL                                                    
         USING CPYELD,SCPYEL                                                    
                                                                                
         GOTOR VDATCON,DMCB,(5,0),(1,XL#TODP)                                   
         GOTOR (RF),(R1),,(2,XL#TODC)                                           
*                                                                               
         GOTOR INIOBUF             initialise buffer                            
         GOTOR SETDTS              read fee system                              
*                                                                               
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    RUNREQ02                                                         
                                                                                
         L     R0,AGOBLOCB                                                      
         AHI   R0,GOADM-GOBLOCK                                                 
         LHI   R1,GOBLOCKX-GOBLOCKD                                             
         SHI   R0,GOADM-GOBLOCK                                                 
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,AMASTC           SET TRACE OPTION                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
                                                                                
RUNREQ02 DS    0H                                                               
                                                                                
RUNREQX  GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY               EXIT BACK TO DDLINK                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST DEFINITIONS (REQUEST and OUTPUT)                            *         
***********************************************************************         
*** Estimate Combined Download ****************************************         
                                                                                
REQELIS  LKREQ H,A#ESCD,OUTECD,NEXTREQ=REQEAUD                                  
                                                                                
Type     LKREQ F,01,(D,B#SAVED,RQ_ECTYP),CHAR,OLEN=L'RQ_ECTYP,         X        
               MAXLEN=L'RQ_ECTYP,TEXT=AC#TYPE,COL=*                             
CliC     LKREQ F,02,(D,B#SAVED,RQ_ECCLI),CHAR,OLEN=L'RQ_ECCLI,         X        
               MAXLEN=L'RQ_ECCLI,TEXT=AC#CLIC,COL=*                             
ProC     LKREQ F,03,(D,B#SAVED,RQ_ECPRO),CHAR,OLEN=L'RQ_ECPRO,         X        
               MAXLEN=L'RQ_ECPRO,TEXT=AC#PROC,COL=*                             
JobC     LKREQ F,04,(D,B#SAVED,RQ_ECJOB),CHAR,OLEN=L'RQ_ECJOB,         X        
               MAXLEN=L'RQ_ECJOB,TEXT=AC#JOBC,COL=*                             
Stat     LKREQ F,05,(D,B#SAVED,RQ_ECSTA),CHAR,OLEN=L'RQ_ECSTA,         X        
               MAXLEN=L'RQ_ECSTA,TEXT=AC#STT,COL=*                              
Appr     LKREQ F,06,(D,B#SAVED,RQ_ECAPP),CHAR,OLEN=L'RQ_ECAPP,         X        
               MAXLEN=L'RQ_ECAPP,TEXT=AC#APRVR,COL=*                            
GAOV     LKREQ F,07,(D,B#SAVED,RQ_ECGAO),CHAR,OLEN=L'RQ_ECGAO,         X        
               MAXLEN=L'RQ_ECGAO,TEXT=AC#GAOV,COL=*                             
OvLL     LKREQ F,08,(D,B#SAVED,RQ_ECOVL),CHAR,OLEN=L'RQ_ECOVL,         X        
               MAXLEN=L'RQ_ECOVL,TEXT=AC#LIMLS,COL=*                            
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS                                                         *         
***********************************************************************         
                                                                                
*** Estimate Combined download ****************************************         
                                                                                
OUTECD   LKOUT H                                                                
                                                                                
OUTECDS  LKOUT R,R#CLSD                                                         
Array    LKOUT C,1,(A,ARYECD)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYECD   LKOUT A,(R,NXTECD),MULTIROW=Y,ROWNAME=ESTRECD                          
PRout    LKOUT P,ESTKSEQ,SETTTYPE                                               
Loc#     LKOUT C,01,ESTKLNO,LBIN,ND=Y,FILTROUT=TSTSEQ,SKIPCOLS=HDRSKIPS         
HDRSKIP  EQU   *                                                                
Stat     LKOUT C,02,ESTRECD,(R,EDTSTAT)                                         
GLang    LKOUT C,03,ESTRECD,(R,EDTLANG)                                         
OffC     LKOUT C,04,ESTRSOFF,CHAR,ND=Y                                          
MedC     LKOUT C,05,ESTKJOB,LEN=1,CHAR,ND=Y                                     
MedN     LKOUT C,06,ESTKJOB,(R,EDTMEDN),CHAR,ND=Y                               
CliC     LKOUT C,07,ESTKCLI,CHAR,ND=Y                                           
PRout    LKOUT P,ESTRECD,EDTCLIN                                                
CliN     LKOUT C,08,(D,B#SAVED,X#NAM),CHAR,ND=Y                                 
GClPN    LKOUT C,09,(D,B#SAVED,X#CLIPN),CHAR,ND=Y                               
ProC     LKOUT C,10,ESTKPRO,CHAR,ND=Y                                           
PRout    LKOUT P,ESTRECD,EDTPRON                                                
ProN     LKOUT C,11,(D,B#SAVED,X#NAM),CHAR,ND=Y                                 
GPrPN    LKOUT C,12,(D,B#SAVED,X#PROPN),CHAR,ND=Y                               
JobC     LKOUT C,13,ESTKJOB,CHAR,ND=Y                                           
PRout    LKOUT P,ESTRECD,EDTJOBN                                                
JobN     LKOUT C,14,(D,B#SAVED,X#NAM),CHAR,ND=Y                                 
JobL     LKOUT C,15,(D,B#SAVED,X#JOBDL),CHAR,ND=Y                               
JSta     LKOUT C,16,(D,B#SAVED,X#JOBDS),CHAR,ND=Y                               
GAdd1    LKOUT C,17,(D,B#SAVED,X#SJAD1),CHAR,ND=Y                               
GAdd2    LKOUT C,18,(D,B#SAVED,X#SJAD2),CHAR,ND=Y                               
GAdd3    LKOUT C,19,(D,B#SAVED,X#SJAD3),CHAR,ND=Y                               
GAdd4    LKOUT C,20,(D,B#SAVED,X#SJAD4),CHAR,ND=Y                               
GAdd5    LKOUT C,21,(D,B#SAVED,X#SJAD5),CHAR,ND=Y                               
LckOrd   LKOUT C,22,(D,B#SAVED,X#JLO),CHAR,ND=Y                                 
PRout    LKOUT P,ESTRECD,RDOPT                                                  
JobLE    LKOUT C,23,(D,B#SAVED,X#NJLE),CHAR,ND=Y                                
BilOrd   LKOUT C,24,(D,B#SAVED,X#BILO),CHAR,ND=Y                                
XdtCop   LKOUT C,25,(D,B#SAVED,X#CECE),CHAR,ND=Y                                
TxtCop   LKOUT C,26,(D,B#SAVED,X#CTEO),CHAR,ND=Y                                
UnCom    LKOUT C,27,(D,B#SAVED,X#ACUA),CHAR,ND=Y                                
PRout    LKOUT P,,SETOEST                                                       
OrInPr   LKOUT C,28,(D,B#SAVED,X#ORIPR),CHAR,ND=Y                               
OrSubI   LKOUT C,29,(D,B#SAVED,X#ORSUI),CHAR,ND=Y                               
OrIapp   LKOUT C,30,(D,B#SAVED,X#ORIAP),CHAR,ND=Y                               
OrSubC   LKOUT C,31,(D,B#SAVED,X#ORSUC),CHAR,ND=Y                               
OrClAp   LKOUT C,32,(D,B#SAVED,X#ORCAP),CHAR,ND=Y                               
OrSwpd   LKOUT C,33,(D,B#SAVED,X#SWPD),HEXD,ND=Y                                
Eactu    LKOUT C,34,(D,B#SAVED,X#ACUA),CHAR,ND=Y                                
LGAPyn   LKOUT C,35,(D,B#SAVED,X#GAPS),CHAR,ND=Y                                
LGdexp   LKOUT C,36,(D,B#SAVED,X#GDES),LBIN,ND=Y                                
LGndve   LKOUT C,37,(D,B#SAVED,X#DNDV),LBIN,ND=Y                                
LGapar   LKOUT C,38,(D,B#SAVED,X#GARA),CHAR,ND=Y                                
LGemex   LKOUT C,39,(D,B#SAVED,X#GEMX),CHAR,ND=Y                                
LGemds   LKOUT C,40,(D,B#SAVED,X#GEMD),CHAR,ND=Y                                
HDRSKIPS EQU   (*-HDRSKIP)/LX_COLSL                                             
Array    LKOUT C,01,(A,ARYENM)                                                  
Array    LKOUT C,02,(A,ARYEMD)                                                  
Array    LKOUT C,03,(A,ARYGDA)                                                  
LGemar   LKOUT C,04,(A,ARYLEML)                                                 
ArrayHT  LKOUT C,05,(A,ARYHEAT)                                                 
ArrayC   LKOUT C,06,(A,ARYCATG)                                                 
ArrayCT  LKOUT C,07,(A,ARYCATT)                                                 
ArrayW   LKOUT C,08,(A,ARYWCOD)                                                 
ArrayWT  LKOUT C,09,(A,ARYWCOT)                                                 
ArrayIM  LKOUT C,10,(A,ARYITEM)                                                 
ArrayIH  LKOUT C,11,(A,ARYITEH)                                                 
ArrayIT  LKOUT C,12,(A,ARYITET)                                                 
ArrayX   LKOUT C,13,(A,ARYXDAT)                                                 
ArrayA   LKOUT C,14,(A,ARYACTV)                                                 
ArrayFT  LKOUT C,15,(A,ARYFOOT)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYENM   LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(ENMELD,ENMELQ),      +        
               ROWWIDTH=(V,ENMLN),NEWEL=B                                       
                                                                                
Name     LKOUT C,01,ENMNAME,CHAR,ND=Y,LEN=V                                     
                                                                                
         LKOUT E                                                                
                                                                                
ARYEMD   LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(EMDELD,EMDELQ),      +        
               ROWWIDTH=(V,EMDLN),NEWEL=B                                       
                                                                                
Glo#     LKOUT C,01,EMDGNO,CHAR,ND=Y                                            
Adde     LKOUT C,02,EMDADT,PDAT,ND=Y                                            
Acty     LKOUT C,03,EMDLDT,PDAT,ND=Y,FILTROUT=TSTGAOV                           
ActT     LKOUT C,04,EMDLTI,(R,EDTACT),CHAR,ND=Y,FILTROUT=TSTGAOV                
Media    LKOUT C,05,EMDBMC,CHAR,ND=Y                                            
SupEL    LKOUT C,06,EMDSTA,(R,SETSEL),CHAR,ND=Y                                 
SupDL    LKOUT C,07,EMDSTA,(R,SETSDL),CHAR,ND=Y                                 
GTVRT    LKOUT C,08,EMDTVR,SPAK,ND=Y,PZERO=S                                    
LockF    LKOUT C,09,EMDELD,(R,SETLOCK),CHAR,ND=Y                                
FrmtC    LKOUT C,10,EMDFMT,CHAR,ND=Y                                            
GExch    LKOUT C,11,EMDRAT,HEXD,ND=Y                                            
TotA     LKOUT C,12,EMDAMT,SPAK,ND=Y                                            
TotF     LKOUT C,13,EMDFCA,SPAK,ND=Y                                            
AddP     LKOUT C,14,EMDAPI,(R,EDTPER),CHAR,ND=Y                                 
AddF     LKOUT C,15,(D,B#SAVED,X#FSTNM),CHAR,ND=Y                               
AddL     LKOUT C,16,(D,B#SAVED,X#LSTNM),CHAR,ND=Y                               
ActP     LKOUT C,17,EMDLPI,(R,EDTPER),CHAR,ND=Y                                 
ActF     LKOUT C,18,(D,B#SAVED,X#FSTNM),CHAR,ND=Y                               
ActL     LKOUT C,19,(D,B#SAVED,X#LSTNM),CHAR,ND=Y                               
CurC     LKOUT C,20,EMDCUR,CHAR,ND=Y                                            
Nodp     LKOUT C,21,EMDNDP,CHAR,ND=Y                                            
IDNo     LKOUT C,22,EMDIDN,HEXD,ND=Y                                            
SCAP     LKOUT C,23,EMDSCA,(R,EDTPER),ND=Y,FILTROUT=TSTGAOV                     
SCAF     LKOUT C,24,(D,B#SAVED,X#FSTNM),CHAR,ND=Y                               
SCAL     LKOUT C,25,(D,B#SAVED,X#LSTNM),CHAR,ND=Y                               
SIAP     LKOUT C,26,EMDSIA,(R,EDTPER),ND=Y,FILTROUT=TSTGAOV                     
GIAFN    LKOUT C,27,(D,B#SAVED,X#FSTNM),CHAR,ND=Y                               
GIALN    LKOUT C,28,(D,B#SAVED,X#LSTNM),CHAR,ND=Y                               
RaisC    LKOUT C,29,EMDAPI,(R,EDTPER),CHAR,ND=Y                                 
RaisF    LKOUT C,30,(D,B#SAVED,X#FSTNM),CHAR,ND=Y                               
RaisL    LKOUT C,31,(D,B#SAVED,X#LSTNM),CHAR,ND=Y                               
RaisE    LKOUT C,32,(D,B#SAVED,X#MRAIE),CHAR,ND=Y                               
RaisT    LKOUT C,33,(D,B#SAVED,X#MRAIT),CHAR,ND=Y                               
SchC     LKOUT C,34,EMDSCH,CHAR,ND=Y                                            
IntU     LKOUT C,35,EMDIUS,(R,EDTIUS),CHAR,ND=Y                                 
LGAPst   LKOUT C,36,EMDGSTAT,HEXD,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYGDA   LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(GDAELD,GDAELQ),      +        
               ROWWIDTH=(V,GDALN),NEWEL=B                                       
                                                                                
PRout    LKOUT P,GDATYPE,SETTTYPE                                               
LGAPsd   LKOUT C,01,GDADATE,PDAT,ND=Y,FILTROUT=TSTSEN                           
LGAPed   LKOUT C,02,GDADATE,PDAT,ND=Y,FILTROUT=TSTEXP                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYLEML  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(FFTELD,FFTELQ),      +        
               ROWWIDTH=(V,FFTLN),NEWEL=B                                       
                                                                                
PRout    LKOUT P,FFTTYPE,SETTTYPE                                               
GAPSKIP  EQU   *                                                                
LGAPem1  LKOUT C,01,FFTELD,(R,EDTEMA),CHAR,ND=Y,SKIPCOLS=GAPSKIPS               
LGAPem2  LKOUT C,01,(D,B#SAVED,X#GAPE2),CHAR,ND=Y                               
LGAPem3  LKOUT C,01,(D,B#SAVED,X#GAPE3),CHAR,ND=Y                               
LGAPem4  LKOUT C,01,(D,B#SAVED,X#GAPE4),CHAR,ND=Y                               
LGAPem5  LKOUT C,01,(D,B#SAVED,X#GAPE5),CHAR,ND=Y                               
GAPSKIPS EQU   (*-GAPSKIP)/LX_COLSL                                             
LGAPBI   LKOUT C,02,FFTDATA,CHAR,ND=Y,FILTROUT=TSTGAPBI                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYHEAT  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(ERDEL,ERDELQ),       +        
               ROWWIDTH=(V,ERDLN),NEWEL=B                                       
                                                                                
PRout    LKOUT P,ERDTYP,SETTTYPE                                                
HTxt1    LKOUT C,01,ERDTEXT,CHAR,ND=Y,FILTROUT=TSTHEAD,LEN=V                    
                                                                                
         LKOUT E                                                                
                                                                                
ARYCATG  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(ERDEL,ERDELQ),       +        
               ROWWIDTH=(V,ERDLN),NEWEL=B                                       
                                                                                
PRout    LKOUT P,ERDTYP,SETTTYPE                                                
CATSKIP  EQU   *                                                                
CCode    LKOUT C,01,ERDCCOD,CHAR,ND=Y,FILTROUT=TSTCATG,                +        
               SKIPCOLS=CATSKIPS                                                
CName    LKOUT C,02,ERDCNAM,CHAR,ND=Y                                           
CTotN    LKOUT C,03,ERDCTNA,CHAR,ND=Y                                           
CInst    LKOUT C,04,ERDCINS,CHAR,ND=Y                                           
CType    LKOUT C,05,ERDCTYP,CHAR,ND=Y                                           
CElse    LKOUT C,06,ERDCELS,CHAR,ND=Y                                           
CATSKIPS EQU   (*-CATSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYCATT  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(ERDEL,ERDELQ),       +        
               ROWWIDTH=(V,ERDLN),NEWEL=B                                       
                                                                                
PRout    LKOUT P,ERDTYP,SETTTYPE                                                
CTxt1    LKOUT C,01,ERDTEXT,CHAR,ND=Y,FILTROUT=TSTCATT,LEN=V                    
                                                                                
         LKOUT E                                                                
                                                                                
ARYWCOD  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(ERDEL,ERDELQ),       +        
               ROWWIDTH=(V,ERDLN),NEWEL=B                                       
                                                                                
PRout    LKOUT P,ERDTYP,SETTTYPE                                                
WCatC    LKOUT C,01,ERDWCAT,CHAR,ND=Y,FILTROUT=TSTWCOD,                +        
               SKIPCOLS=WCOSKIPS                                                
WCOSKIP  EQU   *                                                                
WCode    LKOUT C,02,ERDWCOD,CHAR,ND=Y                                           
WName    LKOUT C,03,ERDWNAM,CHAR,ND=Y                                           
WAmnt    LKOUT C,04,ERDWAMT,SPAK,ND=Y,PZERO=S                                   
WFAmt    LKOUT C,05,ERDWFCA,SPAK,ND=Y,PZERO=S                                   
WCAmt    LKOUT C,06,ERDWCAM,SPAK,ND=Y,PZERO=S                                   
WCFCA    LKOUT C,07,ERDWCFC,SPAK,ND=Y,PZERO=S                                   
WCoRa    LKOUT C,08,ERDWCRA,SPAK,ND=Y,PZERO=S                                   
PRout    LKOUT P,ERDWIND,SETWINDS                                               
Itemwc   LKOUT C,09,(D,B#SAVED,X#ITEWC),CHAR,ND=Y                               
COMM     LKOUT C,10,(D,B#SAVED,X#COMMY),CHAR,ND=Y                               
Cont     LKOUT C,12,(D,B#SAVED,X#CONTI),CHAR,ND=Y                               
MrgE     LKOUT C,13,(D,B#SAVED,X#MRGES),CHAR,ND=Y                               
Ovit     LKOUT C,14,(D,B#SAVED,X#OVITT),CHAR,ND=Y                               
EAct     LKOUT C,15,(D,B#SAVED,X#ESACT),CHAR,ND=Y                               
WVATR    LKOUT C,16,ERDWVRA,LBIN,ND=Y                                           
WVATA    LKOUT C,17,ERDWVAM,SPAK,ND=Y,PZERO=S                                   
WVATF    LKOUT C,18,ERDWVFC,SPAK,ND=Y,PZERO=S                                   
WVATC    LKOUT C,19,ERDWVCO,CHAR,ND=Y                                           
WNICA    LKOUT C,20,ERDWNIC,SPAK,ND=Y,PZERO=S                                   
WNICF    LKOUT C,21,ERDWNFC,SPAK,ND=Y,PZERO=S                                   
WCoWC    LKOUT C,22,ERDELD,(R,EDTCON),CHAR,ND=Y                                 
WCoRa    LKOUT C,23,(D,B#SAVED,X#WCESP),SPAK,ND=Y                               
WCOSKIPS EQU   (*-WCOSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYWCOT  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(ERDEL,ERDELQ),       +        
               ROWWIDTH=(V,ERDLN),NEWEL=B                                       
                                                                                
PRout    LKOUT P,ERDTYP,SETTTYPE                                                
WTxt1    LKOUT C,01,ERDTEXT,CHAR,ND=Y,FILTROUT=TSTWCOT,LEN=V                    
                                                                                
         LKOUT E                                                                
                                                                                
ARYITEM  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(ERDEL,ERDELQ),       +        
               ROWWIDTH=(V,ERDLN),NEWEL=B                                       
PRout    LKOUT P,ERDTYP,SETTTYPE                                                
ICode    LKOUT C,01,ERDICOD,CHAR,ND=Y,FILTROUT=TSTITEM,                +        
               SKIPCOLS=ITESKIPS                                                
ITESKIP  EQU   *                                                                
ISeqN    LKOUT C,02,ERDISEQ,LBIN,ND=Y                                           
IDesc    LKOUT C,03,ERDIDES,CHAR,ND=Y                                           
IPriA    LKOUT C,04,ERDIAPR,SPAK,ND=Y,PZERO=S                                   
IPriF    LKOUT C,05,ERDIFPR,SPAK,ND=Y,PZERO=S                                   
IMult    LKOUT C,06,ERDIMUL,SPAK,ND=Y,PZERO=S                                   
PRout    LKOUT P,ERDIIND,SETIIND                                                
Idesc    LKOUT C,09,(D,B#SAVED,X#IDESC),CHAR,ND=Y                               
Iflex    LKOUT C,10,(D,B#SAVED,X#IFLEX),CHAR,ND=Y                               
Inopr    LKOUT C,11,(D,B#SAVED,X#INOPR),CHAR,ND=Y                               
Iprov    LKOUT C,12,(D,B#SAVED,X#IPROV),CHAR,ND=Y                               
Itime    LKOUT C,13,(D,B#SAVED,X#ITIME),CHAR,ND=Y                               
INICA    LKOUT C,14,ERDINIC,SPAK,ND=Y,PZERO=S                                   
INICF    LKOUT C,15,ERDINFC,SPAK,ND=Y,PZERO=S                                   
PRout    LKOUT P,ERDELD,RDPAS                                                   
ITxt1    LKOUT C,16,(D,B#SAVED,X#IETX1),CHAR,ND=Y                               
ITxt2    LKOUT C,17,(D,B#SAVED,X#IETX2),CHAR,ND=Y                               
IUnit    LKOUT C,18,(D,B#SAVED,X#UNIT),CHAR,ND=Y                                
IUnLa    LKOUT C,19,(D,B#SAVED,X#UNLA),CHAR,ND=Y                                
ITESKIPS EQU   (*-ITESKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYITEH  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(ERDEL,ERDELQ),       +        
               ROWWIDTH=(V,ERDLN),NEWEL=B                                       
                                                                                
PRout    LKOUT P,ERDTYP,SETTTYPE                                                
ITHSKIP  EQU   *                                                                
ITRAC    LKOUT C,01,ERDITAC,CHAR,ND=Y,FILTROUT=TSTITEH,                +        
               SKIPCOLS=ITHSKIPS                                                
ITSEQ    LKOUT C,02,ERDITSQ,LBIN,ND=Y                                           
PRout    LKOUT P,ERDITAC,SET1RACT                                               
ITRAN    LKOUT C,03,(D,B#WORKD,TEMP2),(U,#EDTANM,$EDTANM)                       
ITHRT    LKOUT C,04,ERDIRAT,SPAK,ND=Y                                           
ITNHR    LKOUT C,05,ERDIHRS,SPAK,ND=Y                                           
ITRATO   LKOUT C,06,ERDITND,(R,SETHIND),CHAR,ND=Y                               
ITHSKIPS EQU   (*-ITHSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYITET  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(ERDEL,ERDELQ),       +        
               ROWWIDTH=(V,ERDLN),NEWEL=B                                       
                                                                                
PRout    LKOUT P,ERDTYP,SETTTYPE                                                
IText1   LKOUT C,01,ERDTEXT,CHAR,ND=Y,FILTROUT=TSTITET,LEN=V                    
                                                                                
         LKOUT E                                                                
                                                                                
ARYXDAT  LKOUT A,(D,B#EST,ESTRFST),NEWEL=B,EOT=EOR,                    +        
               ROWID=(XDFEL,XDFELQ),ROWWIDTH=(V,XDFLN)                          
                                                                                
PRout    LKOUT P,XDFOTYP,SETTTYPE                                               
XCode    LKOUT C,01,XDFOCOD,HEXD,ND=Y                                           
XType    LKOUT C,02,XDFOTYP,CHAR,ND=Y                                           
Data     LKOUT C,03,XDFODTA,CHAR,FILTROUT=TSTXDFC,LEN=V                         
Date     LKOUT C,03,XDFODTA,PDAT,LEN=3,FILTROUT=TSTXDFD                         
Amnt     LKOUT C,04,XDFODTA,SPAK,LEN=6,FILTROUT=TSTXDFA,               +        
               ND=Y,PZERO=S                                                     
                                                                                
         LKOUT E                                                                
                                                                                
ARYACTV  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(STCEL,STCELQ),       +        
               ROWWIDTH=(V,STCLN),NEWEL=B                                       
                                                                                
PRout    LKOUT P,STCIND,SETTTYPE                                                
ACTSKIP  EQU   *                                                                
CCDat    LKOUT C,01,STCDATE,PDAT,ND=Y,FILTROUT=TSTESCD,                +        
               SKIPCOLS=ACTSKIPS                                                
CCNam    LKOUT C,02,STCELD,(R,EDTCNM),CHAR,ND=Y                                 
ACTSKIPS EQU   (*-ACTSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYFOOT  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,ROWID=(ERDEL,ERDELQ),       +        
               ROWWIDTH=(V,ERDLN),NEWEL=B                                       
                                                                                
PRout    LKOUT P,ERDTYP,SETTTYPE                                                
FTxt1    LKOUT C,01,ERDTEXT,CHAR,ND=Y,FILTROUT=TSTFOOT,LEN=V                    
                                                                                
         LKOUT E                                                                
                                                                                
SETTTYPE L     R1,LP_AINP          Set type of time element                     
         MVC   WORK(L'TIMETYP),0(R1)                                            
         J     EXIT                                                             
                                                                                
SET1RACT L     R1,LP_AINP          Set type of time element                     
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),=C'1R'                                
         MVC   TEMP2+L'ACTKUNT+L'ACTKLDG(L'ACTKACT),0(R1)                       
         J     EXIT                                                             
                                                                                
TSTHEAD  CLI   WORK,ERDTHTQ        Header text                                  
         BR    RE                                                               
                                                                                
TSTCATG  CLI   WORK,ERDTCSQ        Category data                                
         BR    RE                                                               
                                                                                
TSTCATT  CLI   WORK,ERDTCTQ        Category text                                
         BR    RE                                                               
                                                                                
TSTWCOD  CLI   WORK,ERDTWDQ        Workcode data                                
         BR    RE                                                               
                                                                                
TSTWCOT  CLI   WORK,ERDTWTQ        Workcode text                                
         BR    RE                                                               
                                                                                
TSTITEM  CLI   WORK,ERDTIDQ        Item data                                    
         BR    RE                                                               
                                                                                
TSTITEH  CLI   WORK,ERDTIRQ        Item hours/rates                             
         BR    RE                                                               
                                                                                
TSTITET  CLI   WORK,ERDTITQ        Item text                                    
         BR    RE                                                               
                                                                                
TSTIT1R  CLI   WORK,ERDTIRQ        Item 1R person rate hours                    
         BR    RE                                                               
                                                                                
TSTFOOT  CLI   WORK,ERDTFTQ        Footer text                                  
         BR    RE                                                               
                                                                                
TSTGAOV  CLI   RQ_ECGAO,YESQ       Test if GAOV                                 
         BR    RE                                                               
                                                                                
TSTSEN   CLI   WORK,GDAGAPST       Test if sent date                            
         BR    RE                                                               
                                                                                
TSTEXP   CLI   WORK,GDAGAPEX       Test if expiry date                          
         BR    RE                                                               
*                                                                               
TSTESCD  CLI   WORK,STCIECD        Test if estimates client detail              
         BR    RE                                                               
*                                                                               
TSTGAPBI CLI   WORK,FFTTGAPD       Test if GAP document business ID             
         BR    RE                                                               
                                                                                
TSTXDFC  CLI   WORK,XDFEDCQ        Test if character string for xdata           
         BER   RE                                                               
         CLI   WORK,XDFEDNQ        Test if numerical string for xdata           
         BER   RE                                                               
         CLI   WORK,XDFEDXQ        Test if dropdown for xdata                   
         BER   RE                                                               
         CLI   WORK,XDFEDYQ        Test if yes/no field for xdata               
         BR    RE                                                               
                                                                                
TSTXDFA  CLI   WORK,XDFEDAQ        Test if amount field for xdata               
         BR    RE                                                               
                                                                                
TSTXDFD  CLI   WORK,XDFEDDQ        Test if date field for xdata                 
         BR    RE                                                               
                                                                                
TSTSEQ   CLI   WORK,ESTKSMQ        Test if main record                          
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Read estimate records                                               *         
***********************************************************************         
                                                                                
NXTECD   MVC   IOKEY,SAVEKEY                                                    
         GOTOR (#NXTREC,ANXTREC),DMCB,ESTKEYT,('B#EST',0),             +        
               (0,SAVED),0,0                                                    
         JNE   EXITY                                                            
         L     RF,X#ESTCNT         INCREMENT COUNT                              
         AHI   RF,1                                                             
         ST    RF,X#ESTCNT                                                      
         MVC   SAVEKEY,IOKEY                                                    
         CLI   RQ_ECGAO,YESQ       TEST GAOV REQUEST                            
         JNE   EXITY                                                            
         L     RF,X#ESTCNT                                                      
         CHI   RF,X#MAXGAO         TEST MAX N'ESTIMATES OUTPUT                  
         JL    EXITY               FINISH                                       
         MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
*   Estimate audit Download                                           *         
***********************************************************************         
                                                                                
REQEAUD  LKREQ H,A#ESAUD,OUTAUD,NEXTREQ=REQITMS                                 
                                                                                
Act      LKREQ F,01,(D,B#SAVED,RQ_EACT),CHAR,OLEN=L'RQ_EACT,           +        
               MAXLEN=L'RQ_EACT,TEXT=AC#CLPJO,COL=*                             
Seq      LKREQ F,02,(D,B#SAVED,RQ_ESEQ),LBIN,OLEN=L'RQ_ESEQ,           +        
               MAXLEN=3,TEXT=AC#SEQ,COL=*                                       
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS                                                         *         
***********************************************************************         
***********************************************************************         
*   Estimate Audit download                                           *         
***********************************************************************         
OUTAUD   LKOUT H                                                                
                                                                                
OUTAUDS  LKOUT R,R#ESAUD                                                        
Array    LKOUT C,1,(A,ARYAUD)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYAUD   LKOUT A,(R,NXTAUD),MULTIROW=Y,ROWNAME=AUDRECD                          
Array    LKOUT C,R#ESAUD,(A,ARYSTC)                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYSTC   LKOUT A,(D,B#AUD,AUDRFST),EOT=EOR,ROWID=(STCELD,STCELQ),      +        
               ROWWIDTH=(V,STCLN),NEWEL=B                                       
                                                                                
PRout    LKOUT P,STCIND,SETTTYPE                                                
Date     LKOUT C,01,STCDATE,PDAT,ND=Y,FILTROUT=TSTAUD,SKIPCOLS=AUDSKIPS         
AUDSKIP  EQU   *                                                                
PRout    LKOUT P,STCELD,PROCSTC                                                 
Time     LKOUT C,02,(D,B#SAVED,OA_TIME),CHAR,ND=Y                               
Action   LKOUT C,03,(D,B#SAVED,OA_ACTN),LBIN,ND=Y                               
User     LKOUT C,04,STCUSER,(U,#EDTUSR,$EDTUSR),ND=Y                            
Appl     LKOUT C,05,(D,B#SAVED,OA_APPL),LBIN,ND=Y                               
PID      LKOUT C,06,(D,B#SAVED,OA_PID),CHAR,ND=Y,FILTROUT=TSTAPPL,     +        
               SKIPCOLS=AUDSKP2S                                                
AUDSKP2  EQU   *                                                                
FirstNam LKOUT C,07,(D,B#SAVED,OA_FNM),CHAR,LEN=16,ND=Y                         
MidNam   LKOUT C,08,(D,B#SAVED,OA_MNM),CHAR,LEN=16,ND=Y                         
LastNam  LKOUT C,09,(D,B#SAVED,OA_LNM),CHAR,LEN=58,ND=Y                         
AUDSKP2S EQU   (*-AUDSKP2)/LX_COLSL                                             
Type     LKOUT C,10,(D,B#SAVED,OA_TYPE),LBIN,ND=Y                               
Status   LKOUT C,11,(D,B#SAVED,OA_STAT),LBIN,ND=Y                               
ChrDta   LKOUT C,12,(D,B#SAVED,OA_CHARD),CHAR,ND=Y                              
Date     LKOUT C,13,(D,B#SAVED,OA_DATE),PDAT,ND=Y                               
Amnt     LKOUT C,14,(D,B#SAVED,OA_AMNT),SPAK,ND=Y                               
Catg     LKOUT C,15,(D,B#SAVED,OA_CATG),CHAR,ND=Y                               
WC       LKOUT C,16,(D,B#SAVED,OA_WC),CHAR,ND=Y                                 
Item     LKOUT C,17,(D,B#SAVED,OA_ITEM),CHAR,ND=Y                               
Cur      LKOUT C,18,(D,B#SAVED,OA_CURC),CHAR,ND=Y                               
Xrat     LKOUT C,19,(D,B#SAVED,OA_XRAT),HEXD,ND=Y                               
XHdr     LKOUT C,20,(D,B#SAVED,OA_XHDR),CHAR,ND=Y                               
XChk     LKOUT C,21,(D,B#SAVED,OA_XCHK),CHAR,ND=Y                               
Pid      LKOUT C,22,(D,B#SAVED,OA_PER),CHAR,ND=Y                                
FName    LKOUT C,23,(D,B#SAVED,OA_FNAM),CHAR,ND=Y                               
MName    LKOUT C,24,(D,B#SAVED,OA_MNAM),CHAR,ND=Y                               
LName    LKOUT C,25,(D,B#SAVED,OA_LNAM),CHAR,ND=Y                               
ElStat   LKOUT C,26,(D,B#SAVED,OA_ELST),LBIN,ND=Y                               
IDnum    LKOUT C,27,(D,B#SAVED,OA_IDNO),LBIN,ND=Y                               
Sequnce  LKOUT C,28,(D,B#SAVED,OA_SEQ),LBIN,ND=Y                                
AUDSKIPS EQU   (*-AUDSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
         USING STCELD,R3                                                        
PROCSTC  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*PROCSTC'                                                    
         L     R3,LP_AINP                                                       
         LA    R0,OVALUES                                                       
         LHI   R1,OVALUESL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         UNPK  DUB2,STCTIME                                                     
         OI    DUB2+7,C'0'                                                      
         MVC   OA_TIME,DUB2+2                                                   
         MVI   OA_APPL,X'01'       BrandOcean                                   
         CLI   STCIND,STCIEST2     Is it new style audit                        
         JNE   PRSTC02                                                          
         MVI   OA_APPL,X'02'       Must be Aura                                 
         MVC   OA_ACTN,STCETYP                                                  
         NI    OA_ACTN,X'0F'                                                    
         MVC   BYTE1,STCETYP                                                    
         NI    BYTE1,X'F0'                                                      
         OC    BYTE1,BYTE1                                                      
         JZ    PRSTC02                                                          
         LLC   RF,BYTE1         Shift the left nibble to right nibble           
         SRL   RF,4                                                             
         STC   RF,OA_APPL                                                       
                                                                                
PRSTC02  MVC   TEMP2(L'SA0KNUM),STCPERS                                         
         GOTOR (#GETPID,AGETPID)                                                
         JE    PRSTC04                                                          
         MVI   OA_PID,C'<'         Pass <user> if no name found                 
         MVI   OA_PID+7,C'>'                                                    
         XOUT  STCPERS,OA_PID+1,2                                               
         J     *+10                                                             
PRSTC04  MVC   OA_PID,TEMP2                                                     
         GOTOR (#GETPIN,AGETPIN)                                                
         MVC   OA_FNM,TEMP2                                                     
         MVC   OA_MNM,TEMP2+32                                                  
         MVC   OA_LNM,WORK2                                                     
         CLI   STCIND,STCIEST2     Is it new style audit                        
         JNE   PRSTC52                                                          
         MVC   OA_TYPE,STCETY2                                                  
         LA    R2,AUDTAB                                                        
         USING AUDTABD,R2                                                       
PRSTC06  CLI   AUDATYP,X'FF'                                                    
         JE    PROCSTCX                                                         
         CLC   OA_TYPE,AUDATYP                                                  
         JNE   PRSTC50                                                          
         XR    RF,RF                                                            
         ICM   RF,1,AUDDSTY                                                     
         JZ    PRSTC08                                                          
         AR    RF,R3                                                            
         CLC   AUDSTYP,0(RF)                                                    
         JNE   PRSTC50                                                          
                                                                                
PRSTC08  XR    RF,RF                                                            
         ICM   RF,1,AUDDCHR        Do we have any character data                
         JZ    PRSTC12             No - check for next type of data             
         CLM   RF,1,1(R3)          check displace is not higher than            
         JNL   PRSTC12                      total element length                
         XR    R1,R1               Is the data fixed length                     
         ICM   R1,1,AUDLCHR                                                     
         JZ    PRSTC10             No - variable length                         
         AR    RF,R3               RF=A(character data)                         
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   OA_CHARD(0),0(RF)                                                
         EX    R1,0(RE)                                                         
         J     PRSTC12                                                          
                                                                                
PRSTC10  IC    R1,STCLN                                                         
         SR    R1,RF               R1=Length of data to move                    
         SHI   R1,1                                                             
         AR    RF,R3               RF=A(character data)                         
         BASR  RE,0                                                             
         MVC   OA_CHARD(0),0(RF)                                                
         EX    R1,0(RE)                                                         
                                                                                
PRSTC12  XR    RF,RF                                                            
         ICM   RF,1,AUDDDTE        Do we have any date data to show             
         JZ    PRSTC14             No - check for next type of data             
         AR    RF,R3               RF=A(date data)                              
         MVC   OA_DATE,0(RF)                                                    
                                                                                
PRSTC14  XR    RF,RF                                                            
         ICM   RF,1,AUDDAMT        Do we have any amount data to show           
         JZ    PRSTC16             No - check for next type of data             
         AR    RF,R3               RF=A(amount data)                            
         ZAP   OA_AMNT,0(L'OA_AMNT,RF)                                          
                                                                                
PRSTC16  XR    RF,RF                                                            
         ICM   RF,1,AUDDCAT        Do we have any category data to              
         JZ    PRSTC18             No - check for next type of data             
         AR    RF,R3               RF=A(category data)                          
         MVC   OA_CATG,0(RF)                                                    
                                                                                
PRSTC18  XR    RF,RF                                                            
         ICM   RF,1,AUDDWC         Do we have any work code data to             
         JZ    PRSTC20             No - check for next type of data             
         AR    RF,R3               RF=A(work code data)                         
         MVC   OA_WC,0(RF)                                                      
                                                                                
PRSTC20  XR    RF,RF                                                            
         ICM   RF,1,AUDDITM        Do we have any item code data to             
         JZ    PRSTC22             No - check for next type of data             
         AR    RF,R3               RF=A(item code data)                         
         MVC   OA_ITEM,0(RF)                                                    
                                                                                
PRSTC22  XR    RF,RF                                                            
         ICM   RF,1,AUDDCUR        Do we have any currency code data            
         JZ    PRSTC24             No - check for next type of data             
         AR    RF,R3               RF=A(currency code data)                     
         MVC   OA_CURC,0(RF)                                                    
                                                                                
PRSTC24  XR    RF,RF                                                            
         ICM   RF,1,AUDDCRT        Do we have any exchange rate data            
         JZ    PRSTC26             No - check for next type of data             
         AR    RF,R3               RF=A(exchange rate data)                     
         MVC   OA_XRAT,0(RF)                                                    
                                                                                
PRSTC26  XR    RF,RF                                                            
         ICM   RF,1,AUDDPID        Do we have any person data to show           
         JZ    PRSTC30             No - check for next type of data             
         AR    RF,R3               RF=A(PID data)                               
         MVC   DA_PID,0(RF)                                                     
         MVC   TEMP2(L'SA0KNUM),0(RF)                                           
         GOTOR (#GETPID,AGETPID)                                                
         JE    PRSTC28                                                          
         MVI   OA_PER,C'<'         Pass <user> if no name found                 
         MVI   OA_PER+7,C'>'                                                    
         XOUT  DA_PID,OA_PER+1,2                                                
         J     *+10                                                             
PRSTC28  MVC   OA_PER,TEMP2                                                     
         GOTOR (#GETPIN,AGETPIN)                                                
         MVC   OA_FNAM,TEMP2                                                    
         MVC   OA_MNAM,TEMP2+32                                                 
         MVC   OA_LNAM,WORK2                                                    
                                                                                
PRSTC30  XR    RF,RF                                                            
         ICM   RF,1,AUDDHDR        Do we have any extra data header             
         JZ    PRSTC38             No - check for next type of data             
         AR    RF,R3               RF=A(header pointer data)                    
         MVC   DA_XHDR,0(RF)                                                    
                                                                                
         USING XDFPASD,R4                                                       
         LA    R4,IOKEY            Read for extra data header name              
         XC    XDFPAS,XDFPAS                                                    
         MVI   XDFPTYP,XDFPTYPQ                                                 
         MVI   XDFPSUB,XDFPSUBQ                                                 
         MVC   XDFPCPY,CUXCPY                                                   
         MVC   XDFPPTR(L'XDFPPTR+L'XDFPSEQ),DA_XHDR                             
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         L     R4,AIO3                                                          
         AHI   R4,XDFRFST-XDFRECD                                               
         USING XDFELD,R4                                                        
PRSTC32  CLI   XDFEL,0                                                          
         JE    PROCSTCX                                                         
         CLI   XDFEL,XDFELQ                                                     
         JE    PRSTC36                                                          
PRSTC34  IC    R0,XDFLN                                                         
         AR    R4,R0                                                            
         J     PRSTC32                                                          
                                                                                
PRSTC36  CLC   XDFSEQ,DA_XHDR+L'XDFPPTR                                         
         JNE   PRSTC34                                                          
         XR    R1,R1                                                            
         IC    R1,XDFLN                                                         
         SHI   R1,1+(XDFNAME-XDFELD)                                            
         BASR  RE,0                                                             
         MVC   OA_XHDR(0),XDFNAME                                               
         EX    R1,0(RE)                                                         
                                                                                
PRSTC38  XR    RF,RF                                                            
         ICM   RF,1,AUDDCHK        Do we have any check box data                
         JZ    PRSTC40             No - check for next type of data             
         AR    RF,R3               RF=A(Checkbox data)                          
         MVC   OA_XCHK,0(RF)                                                    
                                                                                
PRSTC40  XR    RF,RF                                                            
         ICM   RF,1,AUDDSTA        Do we have any status data to show           
         JZ    PRSTC42             No - check for next type of data             
         AR    RF,R3               RF=A(Status data)                            
         MVC   OA_STAT,0(RF)                                                    
                                                                                
PRSTC42  XR    RF,RF                                                            
         ICM   RF,1,AUDDESTA       Do we have any status data to show           
         JZ    PRSTC44             No - check for next type of data             
         CLM   RF,1,1(R3)                                                       
         JNL   PRSTC44                                                          
         AR    RF,R3               RF=A(Electronic status data)                 
         MVC   OA_ELST,0(RF)                                                    
                                                                                
PRSTC44  XR    RF,RF                                                            
         ICM   RF,1,AUDDIDN        Do we have text ID number to show            
         JZ    PRSTC46             No - check for next type of data             
         CLM   RF,1,1(R3)                                                       
         JNL   PRSTC46                                                          
         AR    RF,R3               RF=A(text ID number data)                    
         MVC   OA_IDNO,0(RF)                                                    
                                                                                
PRSTC46  XR    RF,RF                                                            
         ICM   RF,1,AUDDSEQ        Do we have text sequence number              
         JZ    PROCSTCX            No - check for next type of data             
         CLM   RF,1,1(R3)                                                       
         JNL   PROCSTCX                                                         
         AR    RF,R3               RF=A(text sequence data)                     
         MVC   OA_SEQ,0(RF)                                                     
         J     PROCSTCX                                                         
                                                                                
PRSTC50  LA    R2,AUDTABL(R2)                                                   
         J     PRSTC06                                                          
                                                                                
PRSTC52  CLI   STCIND,STCIEST      Is it old style audit                        
         JNE   PROCSTCX            Not interested in anything else              
                                                                                
         CLI   STCLN,STCLN1Q       Do we have any comments to show              
         JNH   PRSTC54                                                          
         MVI   OA_ACTN,STCECHG     Set default action                           
         LLC   R1,STCLN            Extract comments                             
         SHI   R1,1+STCLN1Q                                                     
         BASR  RE,0                                                             
         MVC   OA_CHARD(0),STCCOMM                                              
         EX    R1,0(RE)                                                         
                                                                                
PRSTC54  CLC   STCDFR,STCDTO       Status different?                            
         JE    PROCSTCX                                                         
         LA    R2,AUSTAB                                                        
         USING AUSTABD,R2                                                       
PRSTC56  CLI   AUSTCHAR,X'FF'      Have we reached end of table                 
         JNE   PRSTC58             No                                           
         DC    H'0'                Yes - shouldn't happen                       
PRSTC58  CLC   AUSTCHAR,STCDTO     Does the status match table entry            
         JE    PRSTC60             Yes - extract values                         
         LA    R2,AUSTABL(R2)      No - bump to next entry                      
         J     PRSTC56                                                          
                                                                                
PRSTC60  MVC   OA_STAT,AUSTENUM    Extract new status to be shown               
         MVC   OA_TYPE,AUSTTYPE    Extract new type to be shown                 
         MVC   OA_ACTN,AUSTACTN    Extract new action to be shown               
                                                                                
PROCSTCX J     EXIT                                                             
         DROP  R3,R4,R2                                                         
                                                                                
TSTAUD   CLI   WORK,STCIEST        Estimate audit                               
         BER   RE                                                               
         CLI   WORK,STCIEST2       Estimate audit                               
         BR    RE                                                               
                                                                                
TSTAPPL  CLI   OA_APPL,5           Are we electronic signatures                 
         J     SETCCC                                                           
                                                                                
***********************************************************************         
* Read estimate audit records                                         *         
***********************************************************************         
                                                                                
NXTAUD   MVC   IOKEY,SAVEKEY                                                    
         GOTOR (#NXTREC,ANXTREC),DMCB,AUDKEYT,('B#AUD',0),             +        
               (0,SAVED),0,0                                                    
         MVC   SAVEKEY,IOKEY                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Items List Download                                                 *         
***********************************************************************         
                                                                                
REQITMS  LKREQ H,A#ITML,OUTITM,NEXTREQ=REQPDFL                                  
                                                                                
Office   LKREQ F,01,(D,B#SAVED,RQ_OFFC),CHAR,OLEN=L'RQ_OFFC,           +        
               MAXLEN=L'RQ_OFFC,TEXT=AC#OFFC,COL=*                              
Client   LKREQ F,02,(D,B#SAVED,RQ_CLI),CHAR,OLEN=L'RQ_CLI,             +        
               MAXLEN=L'RQ_CLI,TEXT=AC#CLIC,COL=*                               
WC       LKREQ F,03,(D,B#SAVED,RQ_WC),CHAR,OLEN=L'RQ_WC,               +        
               MAXLEN=L'RQ_WC,TEXT=AC#WC,COL=*                                  
Appl     LKREQ F,04,(D,B#SAVED,RQ_APL),LBIN,OLEN=L'RQ_APL,             +        
               MAXLEN=L'RQ_APL,TEXT=AC#APPLI,COL=*                              
Suplr    LKREQ F,05,(D,B#SAVED,RQ_SUPC),CHAR,OLEN=L'RQ_SUPC,           +        
               MAXLEN=L'RQ_SUPC,TEXT=AC#SUPC,COL=*                              
WCTyp    LKREQ F,06,(D,B#SAVED,RQ_TYPE),CHAR,OLEN=L'RQ_TYPE,           +        
               MAXLEN=L'RQ_TYPE,TEXT=(*,WCTYLIT),COL=*                          
Curr     LKREQ F,07,(D,B#SAVED,RQ_CURR),CHAR,OLEN=L'RQ_CURR,           +        
               MAXLEN=L'RQ_CURR,TEXT=(*,CURRLIT),COL=*                          
FXrate   LKREQ F,08,(D,B#SAVED,RQ_XRAT),CHAR,OLEN=L'RQ_XRAT,           +        
               MAXLEN=L'RQ_XRAT,TEXT=(*,EXRATLIT),COL=*                         
Lang     LKREQ F,09,(D,B#SAVED,RQ_LANG),CHAR,OLEN=L'RQ_LANG,           +        
               MAXLEN=L'RQ_LANG,TEXT=(*,LANGLIT),COL=*                          
CodeFlt  LKREQ F,10,(D,B#SAVED,RQ_CODE),CHAR,OLEN=L'RQ_CODE,           +        
               MAXLEN=L'RQ_CODE,TEXT=(*,CODELIT),COL=*                          
EstDte   LKREQ F,11,(D,B#SAVED,RQ_DTE),PDAT,OLEN=L'RQ_DTE,             +        
               MAXLEN=8,TEXT=(*,EDTELIT),COL=*                                  
Seq      LKREQ F,12,(D,B#SAVED,RQ_SEQ),CHAR,OLEN=L'RQ_SEQ,             +        
               MAXLEN=L'RQ_SEQ,TEXT=(*,SEQULIT),COL=*                           
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS                                                         *         
***********************************************************************         
***********************************************************************         
*   Items download                                                    *         
***********************************************************************         
OUTITM   LKOUT H                                                                
                                                                                
OUTITMS  LKOUT R,R#ITML                                                         
Array    LKOUT C,1,(A,ARYITM)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYITM   LKOUT A,(R,NXTITM),MULTIROW=Y,ROWNAME=ARTRECD                          
Code     LKOUT C,01,ARTKART,CHAR,ND=Y                                           
Off      LKOUT C,02,ARTKOFF,CHAR,ND=Y                                           
Client   LKOUT C,03,ARTKCLI,CHAR,ND=Y                                           
Supplr   LKOUT C,04,ARTKSUP,CHAR,ND=Y                                           
WC       LKOUT C,05,ARTRSWC,CHAR,ND=Y                                           
PRout    LKOUT P,ARTKEY,SETSTAT                                                 
PrSt     LKOUT C,06,(D,B#SAVED,OI_PRST),LBIN,ND=Y                               
DscAm    LKOUT C,07,(D,B#SAVED,OI_DSAM),CHAR,ND=Y                               
Array    LKOUT C,08,(A,ARYAFD)                                                  
Array    LKOUT C,11,(A,ARYNAM)                                                  
Array    LKOUT C,12,(A,ARYXNM)                                                  
Array    LKOUT C,13,(A,ARYSCM)                                                  
Array    LKOUT C,15,(A,ARYPRI)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYAFD   LKOUT A,(D,B#ITM,ARTRFST),EOT=EOR,ROWID=(AFDELD,AFDELQ),      +        
               ROWWIDTH=(V,AFDLN),NEWEL=N                                       
                                                                                
Sequ     LKOUT C,08,AFDSEQ,HEXD,ND=Y                                            
Usr      LKOUT C,09,AFDUNIT,CHAR,ND=Y                                           
LUsr     LKOUT C,10,AFDUNLA,CHAR,ND=Y,FILTROUT=TESTLANG                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYNAM   LKOUT A,(D,B#ITM,ARTRFST),EOT=EOR,ROWID=(NAMELD,NAMELQ),      +        
               ROWWIDTH=(V,NAMLN),NEWEL=N                                       
                                                                                
Name     LKOUT C,11,NAMEREC,CHAR,LEN=V,ND=Y                                     
                                                                                
         LKOUT E                                                                
                                                                                
ARYXNM   LKOUT A,(D,B#ITM,ARTRFST),EOT=EOR,ROWID=(XNMELD,XNMELQ),      +        
               ROWWIDTH=(V,XNMLN),NEWEL=N                                       
                                                                                
LName    LKOUT C,12,XNMSUBN,CHAR,LEN=V,ND=Y,FILTROUT=TESTLANG                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYSCM   LKOUT A,(D,B#ITM,ARTRFST),EOT=EOR,ROWID=(SCMELD,SCMELQ),      +        
               ROWWIDTH=(V,SCMLN),NEWEL=N                                       
                                                                                
Desc     LKOUT C,13,SCMNARR,CHAR,LEN=V,ND=Y,FILTROUT=TESTSCMN                   
LDesc    LKOUT C,14,SCMNARR,CHAR,LEN=V,ND=Y,FILTROUT=TESTSCML                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYPRI   LKOUT A,(D,B#ITM,ARTRFST),EOT=EOR,ROWID=(PRIELD,PRIELQ),      +        
               ROWWIDTH=(V,PRILN),NEWEL=N                                       
                                                                                
PRout    LKOUT P,PRIELD,PROCPRI                                                 
Price    LKOUT C,15,(D,B#SAVED,OI_PRICE),SPAK,ND=Y                              
PDrvd    LKOUT C,16,(D,B#SAVED,OI_PDRID),CHAR,ND=Y                              
                                                                                
         LKOUT E                                                                
                                                                                
         USING PRIELD,R3                                                        
PROCPRI  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*PROCPRI'                                                    
         L     R3,LP_AINP                                                       
         ZAP   OI_PRICE,PZERO                                                   
         MVI   OI_PDRID,NOQ                                                     
         LLC   R1,PRICNTR           R1=number of price/date entries             
         LA    RE,PRINTRY           RE=A(of each entry                          
PR       USING PRINTRY,RE                                                       
PPRI002  CLC   PR.PRIDAT,RQ_DTE                                                 
         JNH   PPRI004                                                          
         LA    RE,PRINTRQ(RE)                                                   
         JCT   R1,PPRI002                                                       
         J     PROCPRIX                                                         
PPRI004  ZAP   OI_PRICE,PR.PRIAMT        Check for right date                   
                                                                                
*&&UK                                                                           
         CLC   RQ_CURR,SPACES       Are we foreign currency request             
         JNH   PROCPRIX             No                                          
         CLC   RQ_CURR,AGYCURR                                                  
         JE    PROCPRIX             No                                          
                                                                                
         CLC   PRICURR,SPACES       Do we have currency item                    
         JNH   PPRI006              No                                          
         CLC   PRICURR,AGYCURR                                                  
         JNE   PROCPRIX             Yes and must be match from FLTRART          
         USING EURKBLKD,R2                                                      
PPRI006  LA    R2,ELEMENT           get rate from eureka                        
         XC    0(EURKBLKL,R2),0(R2)                                             
         ZAP   DUB,PR.PRIAMT                                                    
         MVC   EURKCUFR,AGYCURR                                                 
         MVC   EURKCUTO,RQ_CURR                                                 
         MVC   EURKALPH,CUAALF                                                  
         MVC   EURKDATE,DI_DTEC                                                 
         MVC   EURKAFAC,ACOMFACS                                                
         MVI   EURKTYPE,ACCQ                                                    
         TM    CPYSTAT6,CPYSFTXR       FT rates permitted?                      
         JZ    *+8                                                              
         OI    EURKTYPE,ALLOWFTQ+SWAPQ                                          
         SPACE 1                                                                
         GOTO1 VEUREKA,DMCB,('GETQ',EURKBLKD)                                   
         CLI   0(R1),0                                                          
         JE    PPRI008                                                          
                                                                                
         LA    R2,ELEMENT          Get rate from eureka the other way           
         NI    EURKTYPE,X'FF'-SWAPQ                                             
         GOTO1 VEUREKA,DMCB,('GETQ',EURKBLKD)                                   
         CLI   0(R1),0                                                          
         JNE   *+2                                                              
                                                                                
PPRI008  MVC   EURKRLRT,DI_AFCX+1  set rate now passed                          
         GOTO1 VEUREKA,DMCB,('APPLYQ',EURKBLKD),DUB,DUB2                        
         CLI   0(R1),0                                                          
         JNE   *+2                 Dump as rate not applied                     
         ZAP   OI_PRICE,DUB2                                                    
*&&                                                                             
                                                                                
PROCPRIX J     EXIT                                                             
         DROP  R3,PR                                                            
                                                                                
         USING ARTRECD,R3                                                       
SETSTAT  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*SETSTAT'                                                    
         L     R3,LP_AINP                                                       
         MVI   OI_DSAM,NOQ         Initialise amend description                 
         MVI   OI_PRST,OI_FIXDQ     and price fixed                             
         TM    ARTRSTA,ARTKDAMQ    Can this item have the description           
         JZ    *+8                            amended                           
         MVI   OI_DSAM,YESQ        Set can be amended                           
         TM    ARTRSTA,ARTKFLXQ    Does this item have flexible price           
         JZ    *+8                                                              
         MVI   OI_PRST,OI_FLXQ                                                  
         TM    ARTRSTA,ARTKNOPQ    Does this item have no price                 
         JZ    *+8                                                              
         MVI   OI_PRST,OI_FLXQ                                                  
                                                                                
SETSTATX J     EXIT                                                             
         DROP  R3                                                               
TESTLANG CLI   RQ_LANG,YESQ        Are we wanting foreign language              
         BR    RE                                                               
                                                                                
         USING SCMELD,R1                                                        
TESTSCML L     R1,LP_AINP                                                       
         TM    SCMSEQ,SCMSEQLQ     Is this the foregin language                 
         JZ    SETCCC                             description                   
         CLI   RQ_LANG,YESQ        Are we requesting foreign language           
         BR    RE                                                               
                                                                                
         USING SCMELD,R1                                                        
TESTSCMN L     R1,LP_AINP                                                       
         TM    SCMSEQ,SCMSEQLQ                                                  
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Read item (article) records                                         *         
***********************************************************************         
                                                                                
NXTITM   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NITM04              No                                           
                                                                                
         GOTO1 VHEXIN,DMCB,RQ_XRAT,DI_AFCX,L'RQ_XRAT                            
         GOTO1 VHEXIN,DMCB,RQ_SEQ,DI_SEQ,L'RQ_SEQ                               
         GOTO1 VDATCON,DMCB,(1,RQ_DTE),(2,DI_DTEC)                              
*&&UK                                                                           
         CLC   RQ_CURR,SPACES       Are we foreign currency request             
         JNH   NITM04               No                                          
         CLC   RQ_CURR,AGYCURR                                                  
         JE    NITM04               No                                          
         XC    DI_FCUR,DI_FCUR                                                  
         GOTO1 VBLDCUR,DMCB,RQ_CURR,DI_FCUR,ACOMFACS                            
         CLI   0(R1),0                                                          
         JE    NITM02                                                           
         MVC   LP_ERROR,=AL2(AE$INCUR)                                          
         J     XERROR                                                           
                                                                                
NITM02   XC    DI_ACUR,DI_ACUR                                                  
         GOTO1 VBLDCUR,DMCB,AGYCURR,DI_ACUR,ACOMFACS                            
         CLI   0(R1),0                                                          
         JE    NITM04                                                           
         MVC   LP_ERROR,=AL2(AE$INCUR)                                          
         J     XERROR                                                           
*&&                                                                             
NITM04   MVC   IOKEY,SAVEKEY                                                    
         OC    RQ_SEQ,RQ_SEQ                                                    
         JZ    NITM06                                                           
         GOTOR (#NXTREC,ANXTREC),DMCB,PASKEYT,('B#ITM',0),             +        
               ('$NXTRXGR',SAVED),AFLTKPAS,0                                    
         LA    R2,IOKEY                                                         
         USING PASRECD,R2                                                       
         MVC   IODA,PASKDA                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    NITM50                                                           
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
NITM06   OC    RQ_CODE,RQ_CODE                                                  
         JZ    NITM48                                                           
         GOTOR (#NXTREC,ANXTREC),DMCB,ARTKEYT2,('B#ITM',0),            +        
               (0,SAVED),AFLTKART,0                                             
         J     NITM50                                                           
                                                                                
                                                                                
NITM08   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NITM10              No                                           
         USING ARTRECD,R2                                                       
         LA    R2,IOKEY            read article key                             
         XC    ARTKEY,ARTKEY                                                    
         MVI   ARTKTYP,ARTKTYPQ                                                 
         MVI   ARTKSUB,ARTKAQ                                                   
         MVC   ARTKCPY,CUXCPY                                                   
                                                                                
NITM10   GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   ARTKEY(ARTKART-ARTKEY),IOKEYSAV                                  
         JNE   NITMX                                                            
         MVC   SAVEKEY,IOKEY                                                    
                                                                                
NITM48   GOTOR (#NXTREC,ANXTREC),DMCB,ARTKEYT,('B#ITM',0),             +        
               (0,SAVED),AFLTKART,AFLTRART                                      
NITM50   MVC   SAVEKEY,IOKEY                                                    
         LA    R0,OVALUES                                                       
         LHI   R1,OVALUESL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
                                                                                
NITMX    MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
***********************************************************************         
* PdfGen bill lookup request                                          *         
***********************************************************************         
                                                                                
REQPDFL  LKREQ H,A#PDFA,OUTPDF,NEXTREQ=REQUESTX                                 
                                                                                
Bilno    LKREQ F,01,(D,B#SAVED,RQ_BILNO),CHAR,OLEN=L'RQ_BILNO,         X        
               MAXLEN=L'RQ_BILNO,TEXT=AC#BILC,COL=*                             
Credt    LKREQ F,02,(D,B#SAVED,RQ_CRDAT),CDAT,OLEN=L'RQ_CRDAT,         X        
               MAXLEN=8,TEXT=AC#CRTD,COL=*                                      
Usrid    LKREQ F,03,(D,B#SAVED,RQ_USRID),LBIN,TEXT=AC#USRID                     
Billty   LKREQ F,04,(D,B#SAVED,RQ_BILLT),CHAR,OLEN=L'RQ_BILLT,         X        
               MAXLEN=L'RQ_BILLT,TEXT=(*,BILTLIT),COL=*                         
                                                                                
         LKREQ E                                                                
***********************************************************************         
* OUTPUT MAPS                                                         *         
***********************************************************************         
                                                                                
*** PdfGen bill lookup request ****************************************         
                                                                                
OUTPDF   LKOUT H                                                                
                                                                                
OUTPDFS  LKOUT R,R#PDFA                                                         
Array    LKOUT C,1,(A,ARYPDF)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYPDF   LKOUT A,(R,NXTPDF),NROWS=1,ROWNAME=SAVED                               
Alpha    LKOUT C,01,OP_ALPHA,CHAR,ND=Y                                          
Crdat    LKOUT C,02,OP_CRDAT,CDAT,ND=Y                                          
Usrid    LKOUT C,03,OP_USRID,LBIN,ND=Y                                          
Billt    LKOUT C,04,OP_BILLT,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
***********************************************************************         
* NXTPDF bill no. request                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RNSPASD,R2                                                       
NXTPDF   ST    R8,LP_ADATA                                                      
         USING PDFTYPD,RF                                                       
         LA    RF,PDFTYPT                                                       
*&&UK*&& MVC   HALF1,=C'SR'                                                     
*                                                                               
NXTPDF02 OC    RQ_BILLT,RQ_BILLT   If not calling with bill type                
         JZ    NXTPDF04                                                         
         CLC   RQ_BILLT,PDFDESC    Check bill type is valid                     
         JE    NXTPDF04                                                         
         LHI   R0,PDFTYPL                                                       
         AR    RF,R0                                                            
         CLI   0(RF),X'FF'                                                      
         JNE   NXTPDF02                                                         
         MVC   LP_ERROR,=AL2(AE$INBTY)                                          
         J     QERROR              Invalid bill type passed                     
*                                                                               
NXTPDF04 LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   RNSPTYP,RNSPTYPQ                                                 
         MVI   RNSPSUB,RNSPSUBQ                                                 
         MVC   RNSPCPY,CUXCPY                                                   
         MVI   RNSPIND,RNSPRFQ     Set reference number                         
         MVC   RNSPREF,RQ_BILNO                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JNE   *+2                                                              
         J     NXTPDF08                                                         
*                                                                               
NXTPDF06 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JNE   *+2                                                              
*                                                                               
NXTPDF08 CLC   RNSPASD(RNSPBTY-RNSPASD),IOKEYSAV                                
*&&US                                                                           
         JE    NXTPDF30                                                         
         MVC   LP_ERROR,=AL2(AE$BLNOF)                                          
         J     QERROR              Bill record not found                        
*&&                                                                             
*&&UK                                                                           
* UK can have SQ debtors (Flexibill)                                            
         JE    NXTPDF20                                                         
         CLC   HALF1,=C'SR'                                                     
         JNE   NXTPDF10                                                         
* No SR match, loop around again for SQ.                                        
         MVC   HALF1,=C'SQ'                                                     
         J     NXTPDF04                                                         
* No match at all                                                               
NXTPDF10 DS    0H                                                               
         MVC   LP_ERROR,=AL2(AE$BLNOF)                                          
         J     QERROR                                                           
*                                                                               
NXTPDF20 CLC   RNSPUL,HALF1                                                     
         JNE   NXTPDF06                                                         
*&&                                                                             
*                                                                               
NXTPDF30 DS    0H                                                               
         L     R1,=AL4(IOGET+IOMST+IO2)                                         
         CLI   RNSPFIL,RNSPMSQ     Check archive/master                         
         JE    *+8                                                              
         L     R1,=AL4(IOGET+IOARC+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R2,AIO2                                                          
*&&US*&& CLC   =C'SR',TRNKUNT-TRNRECD(R2)   Find debtor posting                 
*&&US*&& JNE   NXTPDF06                                                         
*                                                                               
* Check whether it's eligible.                                                  
         LA    R3,TRNRFST-TRNRECD(R2)                                           
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ                                                     
         JNE   NXTPDF06                                                         
         MVC   X#HALF,TRNOFFC-TRNELD(R3)                                        
         GOTO1 VDATCON,DMCB,(1,TRNDATE-TRNELD(R3)),(2,X#CDAT)                   
*                                                                               
         USING PDFTYPD,RF                                                       
         LA    RF,PDFTYPT                                                       
*                                                                               
NXTPDF40 OC    RQ_BILLT,RQ_BILLT   If no calling with bill type                 
         JZ    *+14                                                             
         CLC   RQ_BILLT,PDFDESC    For M14 type 9 only                          
         JNE   NXTPDF50                                                         
         MVC   OP_BILLT,PDFDESC                                                 
         CLC   TRNTYPE,PDFTYP1     match on type                                
         JE    NXTPDF60                                                         
         CLC   TRNTYPE,PDFTYP2                                                  
         JE    NXTPDF60                                                         
*                                                                               
NXTPDF50 LHI   R0,PDFTYPL                                                       
         AR    RF,R0                                                            
         CLI   0(RF),X'FF'                                                      
         JNE   NXTPDF40                                                         
         J     NXTPDF06                                                         
         DROP  RF                                                               
*                                                                               
         USING TRSELD,R3                                                        
NXTPDF60 CLI   TRSEL,0                                                          
         JE    NXTPDF06                                                         
         CLI   TRSEL,TRSELQ                                                     
         JE    NXTPDF65                                                         
         LLC   R0,TRSLN                                                         
         AR    R3,R0                                                            
         J     NXTPDF60                                                         
*                                                                               
NXTPDF65 LA    RF,TRSDATE                                                       
         OC    RQ_CRDAT,RQ_CRDAT   Any created date?                            
         JZ    NXTPDF75                                                         
         CLC   TRSDATE,RQ_CRDAT    Match on added date                          
         JE    NXTPDF75                                                         
         LA    RF,X#CDAT                                                        
         CLC   X#CDAT,RQ_CRDAT     Match on added date                          
         JNE   NXTPDF06                                                         
*                                                                               
* FOUND                                                                         
NXTPDF75 MVC   OP_CRDAT,0(RF)      Populate data                                
*                                                                               
NXTPDF80 OC    RQ_USRID,RQ_USRID                                                
         JZ    NXTPDF82                                                         
         CLC   TRSUSER,RQ_USRID    Any user id?                                 
         JE    NXTPDF82                                                         
         CLI   CUACCS,0            Test any user limit access                   
         JNE   NXTPDF06            Ok to have any userid on global              
*                                                                               
NXTPDF82 MVC   OP_USRID,TRSUSER                                                 
*                                                                               
         L     R1,AOFFAREA                                                      
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,X#HALF     Move in office and validate                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTOR VOFFAL                                                           
         JE    NXTPDF90                                                         
         MVC   LP_ERROR,=AL2(AE$SECLK)                                          
         J     QERROR              Security lockout                             
*                                                                               
NXTPDF90 MVC   OP_ALPHA,CUAALF     Return the alpha id                          
         J     EXITY                                                            
         EJECT                                                                  
         DROP  R1,R2,R3                                                         
                                                                                
***********************************************************************         
* End of requests                                                     *         
***********************************************************************         
                                                                                
REQUESTX LKREQ X                                                                
                                                                                
         LKARY T                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* LOCAL ROUTINES                                                      *         
***********************************************************************         
***********************************************************************         
* Output indicators                                                   *         
***********************************************************************         
         SPACE 1                                                                
SETWINDS LM    R2,R4,LP_AINP                                                    
         MVC   X#WINDS+1(L'X#WINDS-1),X#WINDS                                   
                                                                                
         MVI   X#ITEWC,NOQ                                                      
         TM    0(R2),ERDWIIQ                                                    
         JZ    *+8                                                              
         MVI   X#ITEWC,YESQ                                                     
*                                                                               
         MVI   X#COMMY,NOQ                                                      
         TM    0(R2),ERDWICQ                                                    
         JZ    *+8                                                              
         MVI   X#COMMY,YESQ                                                     
*                                                                               
         MVI   X#CONTI,NOQ                                                      
         TM    0(R2),ERDWIXQ                                                    
         JZ    *+8                                                              
         MVI   X#CONTI,YESQ                                                     
*                                                                               
         MVI   X#MRGES,NOQ                                                      
         TM    0(R2),ERDWIMQ                                                    
         JZ    *+8                                                              
         MVI   X#MRGES,YESQ                                                     
*                                                                               
         MVI   X#OVITT,NOQ                                                      
         TM    0(R2),ERDWIOQ                                                    
         JZ    *+8                                                              
         MVI   X#OVITT,YESQ                                                     
*                                                                               
         MVI   X#ESACT,NOQ                                                      
         TM    0(R2),ERDWIAC                                                    
         JZ    *+8                                                              
         MVI   X#ESACT,YESQ                                                     
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* Filter item sequence key                                           *          
**********************************************************************          
                                                                                
FLTKPAS  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*FLTKPAS'                                                      
         LA    R2,IOKEY                                                         
         USING PASRECD,R2                                                       
         TM    PASKSTAT,ARTKLOCK+ARTKNOPQ Is item locked or no price            
         JNZ   EXITN               Don't show these items                       
         TM    PASKSTA2,ARTKSART+ARTKSTIM Is the item artist or time            
         JNZ   EXITN               Don't show these items                       
                                                                                
         J     EXITY                                                            
         DROP  R2                                                               
**********************************************************************          
* Filter item by key                                                 *          
**********************************************************************          
                                                                                
FLTKART  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*FLTKART'                                                      
         LA    R2,IOKEY                                                         
         USING ARTRECD,R2                                                       
         CLC   ARTKOFF,SPACES                                                   
         JNH   FKART04                                                          
         CLI   CUACCS,0            If limit access check office                 
         JE    FKART02                                                          
         L     R1,AOFFAREA                                                      
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,ARTKOFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office code                         
         JNE   EXITN                                                            
                                                                                
FKART02  CLC   RQ_OFFC,SPACES      If an office was provided in the             
         JNH   FKART04              request does it match the key               
         CLC   RQ_OFFC,ARTKOFF       if not don't show this item                
         JNE   EXITN                                                            
                                                                                
FKART04  CLC   ARTKCLI,SPACES      If a client was provided in the              
         JNH   FKART06              request does it match the                   
         CLC   RQ_CLI,SPACES         client on the key if it exists             
         JNH   FKART06                if not don't show this item               
         CLC   ARTKCLI,RQ_CLI                                                   
         JNE   EXITN                                                            
                                                                                
FKART06  CLC   ARTKSWC,SPACES      If a WC was provided in the                  
         JNH   FKART08              request does it match the                   
         CLC   RQ_WC,SPACES         WC on the key if it exists                  
         JNH   FKART08                if not don't show this item               
         CLC   RQ_WC,ARTKSWC                                                    
         JNE   EXITN                                                            
                                                                                
FKART08  CLC   RQ_SUPC,SPACES       Does the request contain supplier           
         JNH   FKART10              no therefore see all items                  
         CLC   RQ_SUPC,ARTKSUP      If request has supplier check it            
         JNE   EXITN                 matches                                    
         J     FKART12                                                          
                                                                                
FKART10  CLI   RQ_APL,RQ_ESTMQ      If estimates don't want any items           
         JNE   FKART12               with a supplier code                       
         CLC   ARTKSUP,SPACES                                                   
         JH    EXITN                                                            
                                                                                
FKART12  TM    ARTKSTAT,ARTKLOCK+ARTKNOPQ Is item locked or no price            
         JNZ   EXITN               Don't show these items                       
         TM    ARTKSTA2,ARTKSART+ARTKSTIM Is the item artist or time            
         JNZ   EXITN               Don't show these items                       
         J     EXITY                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
**********************************************************************          
* Filter item by record                                              *          
**********************************************************************          
                                                                                
FLTRART  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*FLTRART'                                                      
*&&UK                                                                           
         L     R2,AIO4                                                          
         USING ARTRECD,R2                                                       
         LA    R3,ARTRFST                                                       
         USING PRIELD,R3                                                        
FRART02  CLI   PRIEL,0                                                          
         JE    FRARTY                                                           
         CLI   PRIEL,PRIELQ                                                     
         JE    FRART06                                                          
FRART04  LLC   R0,PRILN                                                         
         AR    R3,R0                                                            
         J     FRART02                                                          
*                                                                               
FRART06  CLC   PRICURR,SPACES       Always allow items with agency              
         JNH   FRARTY                                    curreny                
         CLC   PRICURR,AGYCURR                                                  
         JE    FRARTY                                                           
         CLC   RQ_CURR,SPACES      If not agency check request currency         
         JNH   FRARTN               if agency reject items in other             
         CLC   RQ_CURR,AGYCURR       currencies                                 
         JE    FRARTN                                                           
         CLC   RQ_CURR,PRICURR    If other currency check it matches            
         JNE   FRARTN                                                           
         DROP  R2,R3                                                            
*&&                                                                             
                                                                                
FRARTY   J     EXITY                                                            
FRARTN   J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Edit out activity date                                              *         
***********************************************************************         
         SPACE 1                                                                
EDTACT   LM    R2,R4,LP_AINP                                                    
         XC    WORK(L'EMDLTI+1),WORK                                            
         MVC   WORK(L'EMDLTI),0(R2)                                             
         MVI   WORK+L'EMDLTI,X'0D'    trick into being packed decimal           
         UNPK  0(7,R4),WORK(L'EMDLTI+1)                                         
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Edit out status                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING ESTRECD,R2                                                       
EDTSTAT  LM    R2,R4,LP_AINP                                                    
         LA    RE,STATAB                                                        
                                                                                
EDTSTA02 CLI   0(RE),FF                                                         
         JE    EDTSTA04                                                         
         CLC   ESTRSTA1,0(RE)                                                   
         JE    EDTSTA04                                                         
         AHI   RE,2                                                             
         J     EDTSTA02                                                         
                                                                                
EDTSTA04 TM    ESTRSTA1,ESTKLOGD                                                
         JZ    EDTSTA06                                                         
         TM    ESTRSTA2,ESTKMERG   Merged?                                      
         JNZ   EDTSTA08                                                         
         MVI   0(R4),RQ_ECSDQ      No, must be logically deleted                
         J     EDTSTAX                                                          
                                                                                
EDTSTA06 TM    ESTRSTA1,ESTKSUBM                                                
         JZ    EDTSTA08                                                         
         TM    ESTRSTA2,ESTKSINA   Submitted to internal approver?              
         JNZ   EDTSTA08                                                         
         MVI   0(R4),RQ_ECSSQ      No, must be submitted to client appr         
         J     EDTSTAX                                                          
                                                                                
EDTSTA08 MVC   0(1,R4),1(RE)                                                    
                                                                                
EDTSTAX  J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Edit out whether foreign language estimate or not                   *         
***********************************************************************         
         SPACE  1                                                               
         USING ESTRECD,R2                                                       
EDTLANG  LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    ESTRSTA2,ESTKLANG                                                
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Edit out media name                                                 *         
***********************************************************************         
         SPACE 1                                                                
EDTMEDN  LM    R2,R4,LP_AINP                                                    
         MVC   X#NAM,SPACES                                                     
         CLI   RQ_ECGAO,YESQ       set indirect values (unless GAOV)            
         JE    EXIT                                                             
         MVC   X#TEMP(14),SPACES                                                
         MVI   X#TEMP,C'M'         look for media                               
         MVC   X#TEMP+1(1),0(R2)                                                
         GOTOR GOLIOB              get from buffer                              
         MVC   0(L'PMDDESC,R4),X#NAM                                            
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Read for client name and edit out                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING ESTRECD,R2                                                       
EDTCLIN  LM    R2,R4,LP_AINP                                                    
         MVC   X#NAM,SPACES                                                     
         MVC   X#CLIPN,SPACES                                                   
         MVC   X#TEMP(14),SPACES                                                
         MVI   X#TEMP,C'C'         look for client                              
         LLC   RF,PCLILEN                                                       
         BCTR  RF,0                                                             
         BASR  R7,0                                                             
         MVC   X#TEMP+1(0),ESTKCLI                                              
         EX    RF,0(R7)                                                         
         GOTOR GOLIOB              get from buffer                              
         J     EXIT                                                             
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* Read for product name and edit out                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING ESTRECD,R2                                                       
EDTPRON  LM    R2,R4,LP_AINP                                                    
         MVC   X#NAM,SPACES                                                     
         MVC   X#PROPN,SPACES                                                   
         CLC   ESTKPRO,SPACES                                                   
         JNH   EXIT                                                             
         MVC   X#TEMP(14),SPACES                                                
         MVI   X#TEMP,C'P'         look for product                             
*                                                                               
         LA    R1,X#TEMP+1                                                      
         LLC   RF,PCLILEN                                                       
         BCTR  RF,0                                                             
         BASR  R7,0                                                             
         MVC   0(0,R1),ESTKCLI                                                  
         EX    RF,0(R7)                                                         
         AHI   RF,1                                                             
         AR    R1,RF                                                            
*                                                                               
         LLC   RF,PPROLEN                                                       
         LLC   RE,PCLILEN                                                       
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         BASR  R7,0                                                             
         MVC   0(0,R1),ESTKPRO                                                  
         EX    RF,0(R7)                                                         
*                                                                               
         GOTOR GOLIOB              get from buffer                              
         J     EXIT                                                             
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* Read for job name and edit out                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING ESTRECD,R2                                                       
EDTJOBN  LM    R2,R4,LP_AINP                                                    
         MVC   X#NAM,SPACES                                                     
         MVC   X#JOBPN,SPACES                                                   
         MVC   X#JLO,SPACES                                                     
         MVC   X#JOBDS,SPACES                                                   
         MVC   X#JOBDL,SPACES                                                   
         MVC   X#SJAD1(5*L'ADRADD1),SPACES                                      
         CLC   ESTKJOB,SPACES                                                   
         JNH   EXIT                                                             
         MVC   X#TEMP(14),SPACES                                                
         MVI   X#TEMP,C'J'         look for job                                 
*                                                                               
         LA    R1,X#TEMP+1                                                      
         LLC   RF,PCLILEN                                                       
         BCTR  RF,0                                                             
         BASR  R7,0                                                             
         MVC   0(0,R1),ESTKCLI                                                  
         EX    RF,0(R7)                                                         
         AHI   RF,1                                                             
         AR    R1,RF                                                            
*                                                                               
         LLC   RF,PPROLEN                                                       
         LLC   RE,PCLILEN                                                       
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         BASR  R7,0                                                             
         MVC   0(0,R1),ESTKPRO                                                  
         EX    RF,0(R7)                                                         
         AHI   RF,1                                                             
         AR    R1,RF                                                            
*                                                                               
         LHI   RF,L'ESTKJOB                                                     
         BCTR  RF,0                                                             
         BASR  R7,0                                                             
         MVC   0(0,R1),ESTKJOB                                                  
         EX    RF,0(R7)                                                         
*                                                                               
         GOTOR GOLIOB              get from buffer                              
         J     EXIT                                                             
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* Set order status from estimate status getopt setting                *         
***********************************************************************         
         SPACE 1                                                                
SETOEST  LM    R2,R4,LP_AINP                                                    
         LA    R3,X#COES                                                        
         LHI   R1,L'GOCOES                                                      
         MVI   X#OREST,NOQ                                                      
         MVC   X#OREST+1(X#ORESL-1),X#OREST                                     
*                                                                               
SETOE02  CLI   0(R3),C'P'                                                       
         JNE   *+8                                                              
         MVI   X#ORIPR,YESQ                                                     
         CLI   0(R3),C'B'                                                       
         JNE   *+8                                                              
         MVI   X#ORSUI,YESQ                                                     
         CLI   0(R3),C'I'                                                       
         JNE   *+8                                                              
         MVI   X#ORIAP,YESQ                                                     
         CLI   0(R3),C'S'                                                       
         JNE   *+8                                                              
         MVI   X#ORSUC,YESQ                                                     
         CLI   0(R3),C'A'                                                       
         JNE   *+8                                                              
         MVI   X#ORCAP,YESQ                                                     
         LA    R3,1(R3)                                                         
         JCT   R1,SETOE02                                                       
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Set suppress empty lines on print                                   *         
***********************************************************************         
         SPACE 1                                                                
SETSEL   LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),EMDSPQ                                                     
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Set suppress description lines on print                             *         
***********************************************************************         
         SPACE 1                                                                
SETSDL   LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),EMDSDQ                                                     
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Set lock status for tv estimates                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING EMDELD,R2                                                        
SETLOCK  LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         CP    EMDTVR,PZERO        apply lock for TV estimates                  
         JE    SETLOCKX                                                         
         CLC   EMDDAT,X#NICST      if outside date ranges                       
         JL    SETLOCK2                                                         
         CLC   EMDDAT,X#NICEN                                                   
         JH    SETLOCK2                                                         
         CLC   EMDDAT,X#TVRST                                                   
         JL    SETLOCK2                                                         
         CLC   EMDDAT,X#TVREN                                                   
         JNH   SETLOCKX                                                         
*                                                                               
SETLOCK2 MVI   0(R4),YESQ                                                       
*                                                                               
SETLOCKX J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Set item hour rate overriden                                        *         
***********************************************************************         
         SPACE 1                                                                
SETHIND  LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),ERDITOQ                                                    
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Edit person code out                                                *         
***********************************************************************         
         SPACE 1                                                                
EDTPER   LM    R2,R4,LP_AINP                                                    
         XC    X#PID,X#PID                                                      
         MVC   X#FSTNM,SPACES                                                   
         MVC   X#LSTNM,SPACES                                                   
         MVC   X#MRAIE,SPACES                                                   
         MVC   X#MRAIT,SPACES                                                   
         MVC   X#TEMP(14),SPACES                                                
         MVI   X#TEMP,C'X'         look for person                              
         MVC   X#TEMP+1(2),0(R2)                                                
         GOTOR GOLIOB              get from buffer                              
         MVC   0(8,R4),X#PID                                                    
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Edit out internal approval in use for this estimate                 *         
***********************************************************************         
         SPACE 1                                                                
EDTIUS   LM    R2,R4,LP_AINP                                                    
         TM    G#OFSTA2,OFFSIAEQ   Internal approvals in use?                   
         JZ    *+10                                                             
         MVC   0(L'EMDIUS,R4),0(R2)                                             
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Edit client contact name                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING STCELD,R2                                                        
EDTCNM   LM    R2,R4,LP_AINP                                                    
         CLI   STCLN,STCLNSQ       Any client contact name?                     
         JNH   EXIT                                                             
         LLC   RF,STCLN                                                         
         SHI   RF,STCLNSQ+1                                                     
         LTR   RF,RF                                                            
         JM    EXIT                                                             
         BASR  R7,0                                                             
         MVC   0(0,R4),STCCLNAM                                                 
         EX    RF,0(R7)                                                         
         J     EXIT                                                             
         EJECT                                                                  
         DROP  R2                                                               
***********************************************************************         
* Edit out email addresses                                            *         
***********************************************************************         
         USING FFTELD,R2                                                        
         SPACE 1                                                                
EDTEMA   LM    R2,R4,LP_AINP       Email element?                               
*&&UK*&& CLI   WORK,FFTTPEML                                                    
*&&US*&& CLI   WORK,FFTTEML                                                     
         JNE   EXIT                                                             
         MVC   X#GAPE2,SPACES                                                   
         MVC   X#GAPE3,SPACES                                                   
         MVC   X#GAPE4,SPACES                                                   
         MVC   X#GAPE5,SPACES                                                   
         CLI   FFTLN,FFEM1LL       Check length of element to check             
         JL    EXIT                how many addresses we have                   
         MVC   0(L'FFTEML1,R4),FFTEML1                                          
         CLI   FFTLN,FFEM2LL                                                    
         JNH   EXIT                                                             
         MVC   X#GAPE2,FFTEML2                                                  
         CLI   FFTLN,FFEM3LL                                                    
         JNH   EXIT                                                             
         MVC   X#GAPE3,FFTEML3                                                  
         CLI   FFTLN,FFEM4LL                                                    
         JNH   EXIT                                                             
         MVC   X#GAPE4,FFTEML4                                                  
         CLI   FFTLN,FFEM5LL                                                    
         JNH   EXIT                                                             
         MVC   X#GAPE5,FFTEML5                                                  
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Edit contingency category workcode                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING ERDELD,R2                                                        
EDTCON   LM    R2,R4,LP_AINP                                                    
         ZAP   X#WCESP,PZERO       clear out ESP%                               
         CLI   ERDLN,ERDWLXQ                                                    
         JL    EXIT                                                             
         CLC   ERDWCCC,SPACES                                                   
         JNH   EXIT                                                             
         MVC   0(2,R4),ERDWCCC                                                  
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(2),ERDWCCC    ESP % always retrieved                       
         GOTOR WGETOPT                                                          
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Set NIC and TVR dates                                               *         
***********************************************************************         
                                                                                
SETDTS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETDTS*'                                                      
                                                                                
         XC    X#NICST,X#NICST                                                  
         XC    X#NICEN,FFS                                                      
         XC    X#TVRST,X#TVRST                                                  
         XC    X#TVREN,FFS                                                      
                                                                                
*&&UK                                                                           
         CLI   CUCTRY,CTRYDFL      default country?                             
         JE    SETDTS05                                                         
*&&                                                                             
         CLI   CUCTRY,CTRYGBR      UK?                                          
         JNE   SETDTSX                                                          
                                                                                
         USING GFEED,R2                                                         
SETDTS05 DS    0H                  read fee sys to get rates                    
         LA    R2,IOKEY                                                         
         XC    GFKEY,GFKEY                                                      
         MVI   GFKMIN,GFKMINQ                                                   
         MVI   GFKREC,GFKTVRQ                                                   
         MVI   GFKCSEQ,AGREEM1Q                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOGND+IO1'                               
         JNE   SETDTS20                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGNF+IO1'                              
         JNE   SETDTS20                                                         
                                                                                
         USING GRECD,R2                                                         
         L     R2,AIO1                                                          
         LA    R3,GFIRST(R2)                                                    
         USING GFAGRD,R3                                                        
SETDTS10 CLI   GFAGREL,0                                                        
         JE    SETDTS20                                                         
         CLI   GFAGREL,GFAGRELQ                                                 
         JE    SETDTS15                                                         
         LLC   R1,GFAGRELL                                                      
         AR    R3,R1                                                            
         J     SETDTS10                                                         
                                                                                
SETDTS15 MVC   X#TVREN,GFAGRTOD                                                 
         MVC   X#TVRST,=X'911101'                                               
                                                                                
SETDTS20 MVC   FULL1,XL#TODP                                                    
                                                                                
         CLC   FULL1+1(2),=X'0405' all depends on tax year                      
         JH    SETDTS25                                                         
                                                                                
         GOTO1 VDATCON,DMCB,(1,FULL1),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,FULL1)                                
                                                                                
SETDTS25 MVC   X#NICST+1(2),=X'0405'                                            
         MVC   X#NICST(1),FULL1                                                 
                                                                                
         GOTO1 VDATCON,DMCB,(1,FULL1),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,FULL1)                                
                                                                                
         MVC   X#NICEN+1(2),=X'0404'                                            
         MVC   X#NICEN(1),FULL1                                                 
                                                                                
SETDTSX  J     EXITY                                                            
AGREEM1Q EQU   1                                                                
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Set item indicators                                                 *         
***********************************************************************         
         SPACE 1                                                                
SETIIND  LM    R2,R4,LP_AINP                                                    
         MVI   X#IINDS,NOQ         clear indicators                             
         MVC   X#IINDS+1(X#IINDL-1),X#IINDS                                     
                                                                                
         TM    0(R2),ERDIIDQ                                                    
         JZ    *+8                                                              
         MVI   X#IDESC,YESQ                                                     
                                                                                
         TM    0(R2),ERDIIFQ                                                    
         JZ    *+8                                                              
         MVI   X#IFLEX,YESQ                                                     
                                                                                
         TM    0(R2),ERDIIPQ                                                    
         JZ    *+8                                                              
         MVI   X#INOPR,YESQ                                                     
                                                                                
         TM    0(R2),ERDIIOQ                                                    
         JZ    *+8                                                              
         MVI   X#IPROV,YESQ                                                     
                                                                                
         TM    0(R2),ERDIITQ                                                    
         JZ    *+8                                                              
         MVI   X#ITIME,YESQ                                                     
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Read cli/pro/job or lookup from buffer                              *         
***********************************************************************         
                                                                                
         USING ACTRECD,R3                                                       
GOLIOB   NTR1  LABEL=NO,WORK=(RC,VOWORKL)                                       
         J     *+12                                                             
         DC    C'*GOLIOB*'                                                      
                                                                                
         USING VOWORKD,RC          RC=A(local working storage)                  
         GOTOR CLRWRK,VOWORKL      Clear work area                              
         USING OB_D,VORBAREA                                                    
         MVI   OB_OTYP,OB_OTYPQ    Build key of buffer record                   
         MVC   OB_OSUB,X#TEMP      Subtype                                      
         MVC   OB_OJOB,X#TEMP+1                                                 
         GOTOR GETBUF,OB_D                                                      
         JH    EXITN                                                            
         JE    GOLIOB30            If found have all values set                 
*                                                                               
GOLIOB02 CLI   OB_OSUB,C'C'        client                                       
         JNE   GOLIOB04                                                         
         MVC   TEMP2(14),SPACES                                                 
         MVC   TEMP2(2),PRODUL                                                  
         MVC   TEMP2+2(CLLQ),OB_OJOB                                            
         GOTOR (#GETACN,AGETACN)                                                
         MVC   OB_NAM(36),TEMP2                                                 
         MVC   OB_PNAM(36),TEMP2+40                                             
         J     GOLIOB28                                                         
                                                                                
GOLIOB04 CLI   OB_OSUB,C'P'        product                                      
         JNE   GOLIOB06                                                         
         MVC   TEMP2(14),SPACES                                                 
         MVC   TEMP2(2),PRODUL                                                  
         LLC   RF,PPROLEN                                                       
         BCTR  RF,0                                                             
         BASR  R7,0                                                             
         MVC   TEMP2+2(0),OB_OJOB                                               
         EX    RF,0(R7)                                                         
         GOTOR (#GETACN,AGETACN)                                                
         MVC   OB_NAM(36),TEMP2                                                 
         MVC   OB_PNAM(36),TEMP2+40                                             
         J     GOLIOB28                                                         
                                                                                
GOLIOB06 CLI   OB_OSUB,C'J'        job                                          
         JNE   GOLIOB18                                                         
         MVC   TEMP2(14),SPACES                                                 
         MVC   TEMP2(2),PRODUL                                                  
         LLC   RF,PJOBLEN                                                       
         BCTR  RF,0                                                             
         BASR  R7,0                                                             
         MVC   TEMP2+2(0),OB_OJOB                                               
         EX    RF,0(R7)                                                         
         GOTOR (#GETACN,AGETACN)                                                
         MVC   OB_NAM(36),TEMP2                                                 
         MVC   OB_PNAM(36),TEMP2+40                                             
         MVI   OB_JLO,NOQ          job locked from orders                       
         MVI   OB_JDR,NOQ          not draft                                    
         MVI   OB_JLK,NOQ          not locked                                   
         L     R3,AIO3                                                          
         TM    ACTRSTAT,ACTSLOCK                                                
         JZ    *+8                                                              
         MVI   OB_JLK,YESQ         locked                                       
         TM    ACTRSTAT,ACTSDRFT                                                
         JZ    *+8                                                              
         MVI   OB_JDR,YESQ         draft                                        
         LA    R4,ACTRFST                                                       
*                                                                               
         USING ADRELD,R4                                                        
GOLIOB08 CLI   ADREL,0                                                          
         JE    GOLIOB28                                                         
         CLI   ADREL,ADRELQ                                                     
         JE    GOLIOB12                                                         
         CLI   ADREL,RSTELQ                                                     
         JE    GOLIOB16                                                         
GOLIOB10 LLC   R0,ADRLN                                                         
         AR    R4,R0                                                            
         J     GOLIOB08                                                         
                                                                                
GOLIOB12 MVC   OB_ADD1(5*L'ADRADD1),SPACES                                      
         LLC   R0,ADRNUM           job address                                  
         LTR   R0,R0                                                            
         JZ    GOLIOB10                                                         
         LA    RF,ADRADD1                                                       
         LA    R1,OB_ADD1                                                       
GOLIOB14 MVC   0(L'ADRADD1,R1),0(RF)                                            
         AHI   R1,L'ADRADD1                                                     
         AHI   RF,L'ADRADD1                                                     
         JCT   R0,GOLIOB14                                                      
         J     GOLIOB28                                                         
                                                                                
         USING RSTELD,R2                                                        
GOLIOB16 CLI   RSTLN,RSTLN3Q                                                    
         JL    GOLIOB10                                                         
         TM    RSTLSTAT,RSTLSORQ   job locked from orders?                      
         JZ    GOLIOB10                                                         
         MVI   OB_JLO,YESQ                                                      
         J     GOLIOB10                                                         
                                                                                
GOLIOB18 CLI   OB_OSUB,C'M'        media                                        
         JNE   GOLIOB24                                                         
         USING PMDRECD,RF                                                       
         LA    RF,IOKEY            read media code record                       
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUXCPY                                                   
         MVC   PMDKMED,OB_OJOB                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   GOLIOB28                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   GOLIOB28                                                         
         L     RF,AIO3                                                          
         LA    RF,PMDRFST                                                       
         USING PMDELD,RF                                                        
GOLIOB20 CLI   PMDEL,PMDELQ                                                     
         JE    GOLIOB22                                                         
         CLI   PMDEL,0                                                          
         JE    GOLIOB28                                                         
         LLC   R0,PMDLN                                                         
         AR    RF,R0                                                            
         J     GOLIOB20                                                         
                                                                                
GOLIOB22 MVC   OB_NAM(L'PMDDESC),PMDDESC                                        
         J     GOLIOB28                                                         
         DROP  RF                                                               
                                                                                
GOLIOB24 CLI   OB_OSUB,C'X'        person                                       
         JNE   GOLIOB26                                                         
         MVC   TEMP2(2),OB_OJOB                                                 
         GOTOR (#GETPID,AGETPID)                                                
         JNE   GOLIOB28                                                         
         MVC   OB_PID(8),TEMP2                                                  
         GOTOR (#GETPIN,AGETPIN)                                                
         JNE   GOLIOB28                                                         
         MVC   OB_FSTN(16),TEMP2                                                
         MVC   OB_LSTN(16),TEMP2+16                                             
         MVC   OB_MRAIE,APPEMAIL                                                
         MVC   OB_MRAIT,TEMP2+52                                                
         J     GOLIOB28                                                         
                                                                                
GOLIOB26 DC    H'0'                unknown                                      
*                                                                               
GOLIOB28 GOTOR ADDBUF,OB_D         Add option buffer record                     
*                                                                               
GOLIOB30 CLI   OB_OSUB,C'C'        client                                       
         JE    GOLIOB32                                                         
         CLI   OB_OSUB,C'P'        product                                      
         JE    GOLIOB34                                                         
         CLI   OB_OSUB,C'J'        job                                          
         JE    GOLIOB36                                                         
         CLI   OB_OSUB,C'M'        media                                        
         JE    GOLIOB38                                                         
         CLI   OB_OSUB,C'X'        PID                                          
         JE    GOLIOB38                                                         
         DC    H'0'                unknown type                                 
*                                                                               
GOLIOB32 MVC   X#NAM,OB_NAM        client                                       
         MVC   X#CLIPN,OB_PNAM                                                  
         J     GOLIOBX                                                          
*                                                                               
GOLIOB34 MVC   X#NAM,OB_NAM        product                                      
         MVC   X#PROPN,OB_PNAM                                                  
         J     GOLIOBX                                                          
*                                                                               
GOLIOB36 MVC   X#JLO,OB_JLO        job                                          
         MVC   X#JOBDS,OB_JDR                                                   
         MVC   X#JOBDL,OB_JLK                                                   
         MVC   X#SJAD1(5*L'ADRADD1),OB_ADD1                                     
         MVC   X#NAM,OB_NAM                                                     
         MVC   X#JOBPN,OB_PNAM                                                  
         J     GOLIOBX                                                          
*                                                                               
GOLIOB38 MVC   X#NAM,OB_NAM        media                                        
         J     GOLIOBX                                                          
*                                                                               
GOLIOB40 MVC   X#PID,OB_PID        person                                       
         MVC   X#FSTNM,OB_FSTN                                                  
         MVC   X#LSTNM,OB_LSTN                                                  
         MVC   X#MRAIE,OB_MRAIE                                                 
         MVC   X#MRAIT,OB_MRAIT                                                 
*                                                                               
GOLIOBX  J     EXIT                                                             
                                                                                
         DROP  RC                                                               
                                                                                
VOWORKD  DSECT                  ** GOLIOB local w/s **                          
VORBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VORBAREA                                                         
OB_OTYP  DS    X                   ** Buffer key **                             
OB_OTYPQ EQU   X'FF'               Buffer type                                  
OB_OSUB  DS    C                                                                
OB_CLIQ  EQU   C'C'                Client                                       
OB_PROQ  EQU   C'P'                Product                                      
OB_JOBQ  EQU   C'J'                Job                                          
OB_MEDQ  EQU   C'M'                Media                                        
OB_PERQ  EQU   C'X'                Person                                       
OB_OJOB  DS    CL(L'ACTKULA)       Job code                                     
*                                                                               
         ORG   VORBAREA+(OB_DATA-OB_D) ** CPJ Buffer **                         
OB_NAM   DS    CL36                Name                                         
OB_JLO   DS    CL1                 Job locked from orders                       
OB_JDR   DS    CL1                 Job draft status                             
OB_JLK   DS    CL1                 Job lock status                              
OB_PNAM  DS    CL36                printed name                                 
OB_ADD1  DS    CL(L'ADRADD1)       Address 1 (26 BYTES)                         
OB_ADD2  DS    CL(L'ADRADD2)       Address 2                                    
OB_ADD3  DS    CL(L'ADRADD3)       Address 3                                    
OB_ADD4  DS    CL(L'ADRADD4)       Address 4                                    
OB_ADD5  DS    CL(L'ADRADD5)       Address 5                                    
         ORG   OB_NAM             **PID definition**                            
OB_PID   DS    CL8                 PID                                          
OB_FSTN  DS    CL16                First name                                   
OB_LSTN  DS    CL16                Last name                                    
OB_MRAIE DS    CL52                Email address                                
OB_MRAIT DS    CL5                 Phone extension                              
         DS    CL(OB_DATAL-(*-OB_NAM)-L'OB_ERROR)                               
         ORG                                                                    
VOWORKL  EQU   *-VOWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Initialise optimisation buffer for list                             *         
* Used for CPJ GETOPT and extra text                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING TSARD,TSAROBUF                                                   
INIOBUF  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**INIO**'                                                      
         XC    TSAROBUF(TSPNEWL),TSAROBUF                                       
         MVI   TSACTN,TSAINI      Init TSAR - CLEAR SPACE                       
         MVI   TSRECI,TSRXTN+TSRTSAB1+TSRTSAB2+TSRWSSVR  Grab as much           
         OI    TSRECI,TSINODSK+TSRMINB1+TSRMINB2         as poss                
         MVI   TSKEYL,OB_KEYL                                                   
         MVI   TSIND2,TSI2MANY    Allow more than 64K records                   
         LHI   R0,8*ONEK                                                        
         OC    TSBUFFL,TSBUFFL                                                  
         JNZ   *+8                                                              
         STCM  R0,3,TSBUFFL       Set require 2MB off-line                      
         LHI   R0,OB_LNQ                                                        
         STCM  R0,3,TSRECL                                                      
         MVC   TSACOM,ACOMFACS                                                  
         GOTO1 VTSAR,TSARD                                                      
         JE    EXITY                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Read getopt to get options                                          *         
***********************************************************************         
         SPACE 1                                                                
RDOPT    LM    R2,R4,LP_AINP                                                    
         GOTOR LGETOPT,(R2)                                                     
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ OPTIONS FOR ESTIMATE LIST                                      *         
* USES AIO1 to read options into                                      *         
***********************************************************************         
         USING ESTRECD,R2                                                       
LGETOPT  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**LGET**'                                                      
         LR    R2,R1                                                            
                                                                                
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCB                                                      
                                                                                
         XC    GOADM(GOABUFF-GOADM),GOADM                                       
         XC    GOADETS(GOACOVL-GOADETS),GOADETS                                 
                                                                                
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK     US uses 1st extension block                  
*&&UK*&& MVC   GOABEXT,AGOBBLCK    UK uses 2nd extension block                  
                                                                                
         MVC   GOSELCUL(L'CUXCPY),CUXCPY                                        
         MVC   GOSELCUL+L'ACTKCPY(L'ACTKUNT+L'ACTKLDG),PRODUL                   
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOABUFF,AGENAREA                                                 
         LHI   RE,GENAREAX-GENAREA                                              
         ST    RE,GOLBUFF                                                       
                                                                                
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELJOB,SPACES                                                  
                                                                                
         CLC   ESTKCLI,SPACES                                                   
         JNH   LGETOP02                                                         
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   GOSELCLI(0),ESTKCLI                                              
         EX    RE,0(R1)                                                         
         OC    GOSELCLI,SPACES                                                  
                                                                                
*&&US*&& CLC   GOSELCLI,SPACES                                                  
*&&US*&& JNH   LGETOP02                                                         
                                                                                
         CLC   ESTKPRO,SPACES                                                   
         JNH   LGETOP02                                                         
         LLC   RE,PPROLEN                                                       
         LLC   RF,PCLILEN                                                       
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   GOSELPRO(0),ESTKPRO                                              
         EX    RE,0(R1)                                                         
         OC    GOSELPRO,SPACES                                                  
                                                                                
         CLC   ESTKJOB,SPACES                                                   
         JNH   LGETOP02                                                         
         LHI   RE,L'ESTKJOB                                                     
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   GOSELJOB(0),ESTKJOB                                              
         EX    RE,0(R1)                                                         
         OC    GOSELJOB,SPACES                                                  
         MVC   GOSELMED,ESTKJOB                                                 
                                                                                
LGETOP02 L     RF,ACOMFACS                                                      
         XC    GOACOVL,GOACOVL                                                  
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    LGETOP04                                                         
         MVC   GOACOVL,VCOVAIL                                                  
                                                                                
LGETOP04 MVC   GOABINSR,CBINSRCH-COMFACSD(RF)                                   
         OI    GOSPEC3,GOSPNERQ+GOSPSWLQ                                        
                                                                                
         GOTOR VGETOPT,DMCB,GOBLOCKD                                            
                                                                                
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
         MVC   X#GAPS,GOGAPS                                                    
         MVC   X#GDES,GOGDES                                                    
         MVC   X#DNDV,GODNDV                                                    
         MVC   X#GARA,GOGARA                                                    
         MVC   X#GEMX,GOGEMX                                                    
         OC    X#GEMX,SPACES                                                    
         MVC   X#GEMD,GOGEMD       Email disclaimer                             
         OC    X#GEMD,SPACES                                                    
         MVC   X#NJLE,GONJLE                                                    
         MVC   X#BILO,GOBILO                                                    
         MVC   X#CECE,GOCECE                                                    
         MVC   X#CTEO,GODCTEO                                                   
         MVC   X#ACUA,GOACUA                                                    
         MVC   X#SWPD,GOSWPD                                                    
         MVC   X#COES,GOCOES                                                    
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Clear work area (RC=A(Work area), R1=L'Work area)                   *         
***********************************************************************         
                                                                                
CLRWRK   STM   RE,R1,12(RD)                                                     
         LR    R0,RC                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Read estimate percentage for a workcode                             *         
* USES AIO1 to read options into                                      *         
***********************************************************************         
         USING ESTRECD,R2                                                       
WGETOPT  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**CGET**'                                                      
                                                                                
         L     R2,AIO2                                                          
                                                                                
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCB                                                      
                                                                                
         XC    GOADM(GOABUFF-GOADM),GOADM                                       
         XC    GOADETS(GOACOVL-GOADETS),GOADETS                                 
                                                                                
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK     US uses 1st extension block                  
*&&UK*&& MVC   GOABEXT,AGOBBLCK    UK uses 2nd extension block                  
                                                                                
         MVC   GOSELCUL(L'CUXCPY),CUXCPY                                        
         MVC   GOSELCUL+L'ACTKCPY(L'ACTKUNT+L'ACTKLDG),PRODUL                   
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOABUFF,AGENAREA                                                 
         LHI   RE,GENAREAX-GENAREA                                              
         ST    RE,GOLBUFF                                                       
                                                                                
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELJOB,SPACES                                                  
                                                                                
         CLC   ESTKCLI,SPACES                                                   
         JNH   WGETOP02                                                         
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   GOSELCLI(0),ESTKCLI                                              
         EX    RE,0(R1)                                                         
         OC    GOSELCLI,SPACES                                                  
                                                                                
*&&US*&& CLC   GOSELCLI,SPACES                                                  
*&&US*&& JNH   WGETOP02                                                         
                                                                                
         CLC   ESTKPRO,SPACES                                                   
         JNH   WGETOP02                                                         
         LLC   RE,PPROLEN                                                       
         LLC   RF,PCLILEN                                                       
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   GOSELPRO(0),ESTKPRO                                              
         EX    RE,0(R1)                                                         
         OC    GOSELPRO,SPACES                                                  
                                                                                
         CLC   ESTKJOB,SPACES                                                   
         JNH   WGETOP02                                                         
         LHI   RE,L'ESTKJOB                                                     
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   GOSELJOB(0),ESTKJOB                                              
         EX    RE,0(R1)                                                         
         OC    GOSELJOB,SPACES                                                  
         MVC   GOSELMED,ESTKJOB                                                 
                                                                                
WGETOP02 MVC   GOSELWC,TEMP2                                                    
                                                                                
         L     RF,ACOMFACS                                                      
         XC    GOACOVL,GOACOVL                                                  
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    WGETOP04                                                         
         MVC   GOACOVL,VCOVAIL                                                  
                                                                                
WGETOP04 MVC   GOABINSR,CBINSRCH-COMFACSD(RF)                                   
         OI    GOSPEC3,GOSPNERQ                                                 
                                                                                
         GOTOR VGETOPT,DMCB,GOBLOCKD                                            
                                                                                
         ZAP   X#WCESP,GOESTPER    Estimate WC %                                
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Read record from buffer                                             *         
***********************************************************************         
T        USING TSARD,TSAROBUF                                                   
GETBUF   NTR1  LABEL=NO,WORK=(RC,OB_LNQ)                                        
         J     *+12                                                             
         DC    C'*GETBUF*'                                                      
W        USING OB_D,RC                                                          
         LR    R2,R1                                                            
P        USING OB_D,R2             R2=A(caller's OB_D)                          
         MVC   W.OB_KEY,P.OB_KEY                                                
*                                                                               
         MVI   T.TSACTN,TSARDH     Set action to 'Read High'                    
         LA    R0,W.OB_D                                                        
         ST    R0,T.TSAREC         Read into acquired storage                   
         GOTOR VTSAR,T.TSARD                                                    
         JNE   EXITL               Not found/EOF                                
                                                                                
         LA    R0,P.OB_D                                                        
         LHI   R1,OB_LNQ                                                        
         LA    RE,W.OB_D                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   ROUERRV,P.OB_ERROR  Set/test record in error                     
         OC    ROUERRV,ROUERRV                                                  
         JNZ   EXITH                                                            
         J     EXITY                                                            
         DROP  W,P,T                                                            
         EJECT                                                                  
***********************************************************************         
* Read article passives                                               *         
***********************************************************************         
         SPACE 1                                                                
RDPAS    LM    R2,R4,LP_AINP                                                    
         GOTOR GETPAS,(R2)                                                      
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Read article passives for item extra text                           *         
***********************************************************************         
         SPACE 1                                                                
         USING PASRECD,R3                                                       
         USING ERDELD,R2                                                        
GETPAS   NTR1  LABEL=NO,WORK=(RC,VPWORKL)                                       
         J     *+12                                                             
         DC    C'**GETP**'                                                      
         LR    R2,R1                                                            
         USING VPWORKD,RC                                                       
         GOTOR CLRWRK,VPWORKL      Clear work area                              
         USING OB_D,VAPAREA                                                     
                                                                                
         MVC   X#IETX1,SPACES                                                   
         MVC   X#IETX2,SPACES                                                   
         MVC   X#UNIT,SPACES                                                    
         MVC   X#UNLA,SPACES                                                    
         MVI   OB_PTYP,OB_PTYPQ    Build key of buffer record                   
         MVI   OB_PSUB,OB_PSUBQ                                                 
         MVC   OB_PSEQ,ERDISEQ                                                  
         MVC   OB_ITEX1,SPACES                                                  
         MVC   OB_ITEX2,SPACES                                                  
         MVC   OB_UNIT,SPACES                                                   
         MVC   OB_UNLA,SPACES                                                   
         GOTOR GETBUF,OB_D                                                      
         JH    EXITN                                                            
         JE    GETPASX             If found have all values set                 
                                                                                
         LA    R3,IOKEY                                                         
         XC    PASKEY,PASKEY                                                    
         MVI   PASKTYP,PASKTYPQ                                                 
         MVI   PASKSUB,PASKSQ                                                   
         MVC   PASKCPY,CUXCPY                                                   
         MVC   PASKSEQ,ERDISEQ                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   GETPASX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                Bad master record                            
                                                                                
         USING ARTRECD,R2                                                       
         L     R2,AIO3                                                          
         LA    R3,ARTRFST                                                       
                                                                                
         USING SCMELD,R3                                                        
GETPAS02 CLI   SCMEL,0             Look for text entries                        
         JE    GETPAS10                                                         
         CLI   SCMEL,SCMELQ                                                     
         JE    GETPAS06                                                         
         CLI   SCMEL,AFDELQ                                                     
         JE    GETPAS08                                                         
GETPAS04 LLC   R0,SCMLN                                                         
         AR    R3,R0                                                            
         J     GETPAS02                                                         
                                                                                
GETPAS06 LA    RE,OB_ITEX1                                                      
         TM    SCMSEQ,SCMSEQLQ     Language entry?                              
         JZ    *+8                                                              
         LA    RE,OB_ITEX2                                                      
         LLC   RF,SCMLN                                                         
         SHI   RF,SCMLN1Q+1                                                     
         BASR  R7,0                                                             
         MVC   0(0,RE),SCMNARR                                                  
         EX    RF,0(R7)                                                         
         J     GETPAS04                                                         
                                                                                
         USING AFDELD,R3                                                        
GETPAS08 CLI   AFDLN,AFDLNXQ                                                    
         JL    GETPAS04                                                         
         MVC   X#UNIT,AFDUNIT                                                   
         MVC   X#UNLA,AFDUNLA                                                   
         J     GETPAS04                                                         
                                                                                
GETPAS10 GOTOR ADDBUF,OB_D         Add option buffer record                     
                                                                                
GETPASX  MVC   X#IETX1,OB_ITEX1                                                 
         MVC   X#IETX2,OB_ITEX2                                                 
         MVC   X#UNIT,OB_UNIT                                                   
         MVC   X#UNLA,OB_UNLA                                                   
         J     EXIT                                                             
         DROP  R3,RC                                                            
                                                                                
VPWORKD  DSECT                  ** GETPAS local w/s **                          
VAPAREA  DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VAPAREA                                                          
OB_PTYP  DS    X                   ** Buffer key **                             
OB_PTYPQ EQU   X'FF'               Buffer type                                  
OB_PSUB  DS    C                   Subtype                                      
OB_PSUBQ EQU   C'A'                                                             
OB_PSEQ  DS    CL1                 Sequence                                     
         ORG   VAPAREA+(OB_DATA-OB_D) ** Article buffer **                      
OB_ITEX1 DS    CL180               Item text 1                                  
OB_ITEX2 DS    CL180               Item text 2                                  
OB_UNIT  DS    CL15                Item unit                                    
OB_UNLA  DS    CL15                Item unit language                           
         DS    CL(OB_DATAL-(*-OB_ITEX1)-L'OB_ERROR)                             
         ORG                                                                    
VPWORKL  EQU   *-VPWORKD                                                        
         EJECT                                                                  
                                                                                
SVRDEF   CSECT ,                                                                
***********************************************************************         
* Add a record to optimisation buffer                                 *         
*                                                                     *         
* Ntry   - R1 points to caller's OB_D                                 *         
***********************************************************************         
                                                                                
ADDBUF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ADDBUF*'                                                      
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSAADD     Set action to 'Add'                          
         ST    R1,T.TSAREC                                                      
         GOTOR VTSAR,T.TSARD                                                    
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* GENERAL EXIT AND DECLARATIONS                                       *         
***********************************************************************         
                                                                                
SETCCC   JE    *+8                 SET CONVERSE CONDITION CODE                  
         CR    RE,RE               NOT EQUAL TO EQUAL/NOT ZERO TO ZERO          
         BR    RE                                                               
         LTR   RE,RE               EQUAL TO NOT EQUAL/ZERO TO NOT ZERO          
         BR    RE                                                               
                                                                                
QERROR   MVI   LP_RMODE,LP_RERRR   SET REQUEST ERROR & EXIT                     
         J     EXITY                                                            
                                                                                
XERROR   DS    0H                                                               
         MVI   LP_RMODE,LP_RERRR                                                
         MVI   LP_EMSYS,6                                                       
         J     EXITN                                                            
                                                                                
EXITH    LHI   RE,2                SET CC=HIGH                                  
         J     EXITCC                                                           
                                                                                
EXITN    TM    TWAMODE,TWAMUWD                                                  
         JZ    EXITL                                                            
         NI    TWAMODE,FF-TWAMUWD                                               
         DC    H'0'                Unwind here|                                 
EXITL    LHI   RE,0                SET CC=LOW                                   
         J     EXITCC                                                           
                                                                                
EXITY    TM    TWAMODE,TWAMUWD                                                  
         JZ    EXITYX                                                           
         NI    TWAMODE,FF-TWAMUWD                                               
         DC    H'0'                Unwind here|                                 
EXITYX   LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LIST OF ACCOUNT FILES TO OPEN IN ALL SYSTEMS                        *         
***********************************************************************         
                                                                                
FILES    DS    0X                  ** FILE INFO **                              
         DC    C'ACCOUNT'          SYSTEM NAME FOR OPEN                         
                                                                                
         DC    C'nCTFILE '         FILE LIST                                    
         DC    C'NGENFIL '                                                      
         DC    C'NGENDIR '                                                      
         DC    C'UACCDIR '                                                      
         DC    C'UACCMST '                                                      
         DC    C'UACCARC '                                                      
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
GLOBALS  DS    0D                                                               
         LTORG                                                                  
DMKEY    DC    CL8'DMKEY'                                                       
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
ACCARC   DC    CL8'ACCARC'                                                      
PZERO    DC    PL1'0'                                                           
FFS      DC    XL4'FFFFFFFF'                                                    
ZEROES   DC    CL4'0000'                                                        
WORKLEN  DC    AL2(WORKL)                                                       
SJUL     DC    C'SJ'                                                            
                                                                                
B#EST    EQU   3                       IO2 Estimate record                      
B#AUD    EQU   5                       IO4 Estimate audit record                
B#ITM    EQU   5                       IO4 Item record                          
EOR      EQU   0                                                                
MAXEML   EQU   10                    Max no of email addresses for GAP          
*&&US                                                                           
CLLQ     EQU   3                   CLIENT CODE LENGTH                           
PRLQ     EQU   3                   PRODCT CODE LENGTH                           
*&&                                                                             
*&&UK                                                                           
CLLQ     EQU   5                   CLIENT CODE LENGTH                           
PRLQ     EQU   2                   PRODCT CODE LENGTH                           
*&&                                                                             
CPLQ     EQU   CLLQ+PRLQ           CLIENT AND PRODUCT                           
                                                                                
OLOGDEL  EQU   5                   Logically deleted                            
OREJECT  EQU   4                   Rejected                                     
OINTAPP  EQU   7                   Internally approved                          
OCLIAPP  EQU   3                   Client approved                              
OSUBINT  EQU   6                   Submitted for internal                       
OSUBCLI  EQU   2                   Submitted for client approval                
OINPROG  EQU   1                   In progress                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* Key drivers                                                         *         
***********************************************************************         
                                                                                
ESTKEYT  LKKEY H,ESTKEY,SAVED      ** ESTIMATE KEY DRIVER **                    
         LKKEY LIT,ESTKTYP,ESTKTYPQ                                             
         LKKEY LIT,ESTKSUB,ESTKSUBQ                                             
         LKKEY SIN,ESTKCPY,AGENCY                                               
         LKKEY LIT,ESTKREM,0                                                    
         LKKEY ALL,ESTKSEQ                                                      
         LKKEY E                                                                
                                                                                
ESTKEYC  LKKEY H,ESTKEY            ** ESTIMATE KEY DRIVER FOR CLI **            
         LKKEY LIT,ESTKTYP,ESTKTYPQ                                             
         LKKEY LIT,ESTKSUB,ESTKSUBQ                                             
         LKKEY SIN,ESTKCPY,AGENCY                                               
         LKKEY LIT,ESTKREM,0                                                    
         LKKEY SIN,ESTKCLI,RQ_ECCLI                                             
         LKKEY ALL,ESTKSEQ                                                      
         LKKEY E                                                                
                                                                                
ESTKEYP  LKKEY H,ESTKEY            ** ESTIMATE KEY DRIVER FOR PRO **            
         LKKEY LIT,ESTKTYP,ESTKTYPQ                                             
         LKKEY LIT,ESTKSUB,ESTKSUBQ                                             
         LKKEY SIN,ESTKCPY,AGENCY                                               
         LKKEY LIT,ESTKREM,0                                                    
         LKKEY SIN,ESTKCLI,RQ_ECCLI                                             
         LKKEY SIN,ESTKPRO,RQ_ECPRO                                             
         LKKEY ALL,ESTKSEQ                                                      
         LKKEY E                                                                
                                                                                
ESTKEYJ  LKKEY H,ESTKEY            ** ESTIMATE KEY DRIVER FOR JOB **            
         LKKEY LIT,ESTKTYP,ESTKTYPQ                                             
         LKKEY LIT,ESTKSUB,ESTKSUBQ                                             
         LKKEY SIN,ESTKCPY,AGENCY                                               
         LKKEY LIT,ESTKREM,0                                                    
         LKKEY SIN,ESTKCLI,RQ_ECCLI                                             
         LKKEY SIN,ESTKPRO,RQ_ECPRO                                             
         LKKEY SIN,ESTKJOB,RQ_ECJOB                                             
         LKKEY ALL,ESTKSEQ                                                      
         LKKEY E                                                                
                                                                                
AUDKEYT  LKKEY H,AUDKEY            ** ESTIMATE KEY DRIVER FOR AUDIT **          
         LKKEY LIT,AUDKTYP,AUDKTYPQ                                             
         LKKEY LIT,AUDKSUB,AUDKSUBQ                                             
         LKKEY SIN,AUDKCPY,AGENCY                                               
         LKKEY LIT,AUDKAUDT,AUDKEST                                             
         LKKEY LIT,AUDKREME,0                                                   
         LKKEY SIN,AUDKECPJ,RQ_EACT                                             
         LKKEY SIN,AUDKELNO,RQ_ESEQ                                             
         LKKEY ALL,AUDKSEQ                                                      
         LKKEY E                                                                
                                                                                
ARTKEYT  LKKEY H,ARTKEY            ** ITEM KEY DRIVER **                        
         LKKEY LIT,ARTKTYP,ARTKTYPQ                                             
         LKKEY LIT,ARTKSUB,ARTKAQ                                               
         LKKEY SIN,ARTKCPY,AGENCY                                               
         LKKEY ALL,ARTKART                                                      
         LKKEY ALL,ARTKARTF                                                     
         LKKEY ALL,ARTKOFF                                                      
         LKKEY ALL,ARTKCLI                                                      
         LKKEY ALL,ARTKSUP                                                      
         LKKEY LIT,ARTKREM,0                                                    
         LKKEY E                                                                
                                                                                
ARTKEYT2 LKKEY H,ARTKEY            ** ITEM KEY DRIVER **                        
         LKKEY LIT,ARTKTYP,ARTKTYPQ                                             
         LKKEY LIT,ARTKSUB,ARTKAQ                                               
         LKKEY SIN,ARTKCPY,AGENCY                                               
         LKKEY SIN,ARTKART,RQ_CODE                                              
         LKKEY ALL,ARTKARTF                                                     
         LKKEY ALL,ARTKOFF                                                      
         LKKEY ALL,ARTKCLI                                                      
         LKKEY ALL,ARTKSUP                                                      
         LKKEY LIT,ARTKREM,0                                                    
         LKKEY E                                                                
                                                                                
PASKEYT  LKKEY H,PASKEY            ** ITEM SEQUENCE KEY DRIVER **               
         LKKEY LIT,PASKTYP,PASKTYPQ                                             
         LKKEY LIT,PASKSUB,PASKSQ                                               
         LKKEY SIN,PASKCPY,AGENCY                                               
         LKKEY LIT,PASKREM,0                                                    
         LKKEY SIN,PASKSEQ,DI_SEQ                                               
         LKKEY E                                                                
                                                                                
DCDICTL  DS    0X                                                               
         DCDDL AC#TIME,L'AC@TIME                                                
         DCDDL AC#MAT,L'AC@MAT                                                  
         DCDDL AC#EST,L'AC@EST                                                  
DCDICTLX DC    X'FF'                                                            
                                                                                
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
                                                                                
STATAB   DS    0X                                                               
         DC    AL1(ESTKCREA),AL1(RQ_ECSCQ)                                      
         DC    AL1(ESTKSUBM),AL1(RQ_ECSBQ) ESTKSUBM:RQ_ELSBQ & RQ_ELSSQ         
         DC    AL1(ESTKINTA),AL1(RQ_ECSIQ)                                      
         DC    AL1(ESTKSUBM),AL1(RQ_ECSSQ)                                      
         DC    AL1(ESTKCAPP),AL1(RQ_ECSAQ)                                      
         DC    AL1(ESTKLOGD),AL1(RQ_ECSMQ) ESTKLOGD:RQ_ELSMQ & RQ_ELSDQ         
         DC    AL1(ESTKREJE),AL1(RQ_ECSRQ)                                      
         DC    AL1(ESTKLOGD),AL1(RQ_ECSDQ)                                      
         DC    AL1(FF),CL1'*'                                                   
                                                                                
AUDTAB   DS    0X                                                               
* Estimate added                                                                
         DC    AL1(STCEAEA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)                 
* Estimate deleted                                                              
         DC    AL1(STCEDEE,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)                 
* Estimate status                                                               
         DC    AL1(STCESTA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,STCECES-STCELD)          
         DC    AL1(0,0,0)                                                       
* Estimate name                                                                 
         DC    AL1(STCEESN,0,0,STCENAM-STCELD,0,0,0,0,0,0)                      
         DC    AL1(0,0,0,0,0,0,0,0,0)                                           
* Estimate date                                                                 
         DC    AL1(STCEESD,0,0,0,0,STCEDAT-STCELD,0,0,0,0)                      
         DC    AL1(0,0,0,0,0,0,0,0,0)                                           
* Internal use switched on                                                      
         DC    AL1(STCEINO,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)                 
* Internal use switched off                                                     
         DC    AL1(STCEINF,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)                 
* Client contact approver name                                                  
         DC    AL1(STCECCN,0,0,STCECNAM-STCELD,0,0,0,0,0,0)                     
         DC    AL1(0,0,0,0,0,0,0,0,0)                                           
* Work code added/deleted/changed                                               
         DC    AL1(STCEWCO,0,0,0,0,0,0,STCECATC-STCELD)                         
         DC    AL1(STCEWRKC-STCELD,0,0,0,0,0,0,0,0,0,0)                         
* Work code name                                                                
         DC    AL1(STCEWCN,0,0,STCCWINM-STCELD,0,0,0,STCECATC-STCELD)           
         DC    AL1(STCEWRKC-STCELD,0,0,0,0,0,0,0,0,0,0)                         
* Work code text                                                                
         DC    AL1(STCEWCT,0,0,0,0,0,0,STCECATC-STCELD)                         
         DC    AL1(STCEWRKC-STCELD,0,0,0,0,0,0,0,0,0,0)                         
* Work code amount                                                              
         DC    AL1(STCEWAM,0,0,0,0,0,STCEAMNT-STCELD,STCECATC-STCELD)           
         DC    AL1(STCEWRKC-STCELD,0,0,0,0,0,0,0,0,0,0)                         
* Item added/deleted/changed                                                    
         DC    AL1(STCEITE,0,0,0,0,0,0,STCECATC-STCELD)                         
         DC    AL1(STCEWRKC-STCELD,STCEITEC-STCELD,0,0,0,0,0,0,0,0,0)           
* Item name                                                                     
         DC    AL1(STCEINM,0,0,STCCWINM-STCELD,0,0,0,STCECATC-STCELD)           
         DC    AL1(STCEWRKC-STCELD,STCEITEC-STCELD,0,0,0,0,0,0,0,0,0)           
* Item text                                                                     
         DC    AL1(STCEITT,0,0,0,0,0,0,STCECATC-STCELD)                         
         DC    AL1(STCEWRKC-STCELD,STCEITEC-STCELD,0,0,0,0,0,0,0,0,0)           
* Item multiplier                                                               
         DC    AL1(STCEIMU,0,0,0,0,0,STCEAMNT-STCELD,STCECATC-STCELD)           
         DC    AL1(STCEWRKC-STCELD,STCEITEC-STCELD,0,0,0,0,0,0,0,0,0)           
* Item price                                                                    
         DC    AL1(STCEITP,0,0,0,0,0,STCEAMNT-STCELD,STCECATC-STCELD)           
         DC    AL1(STCEWRKC-STCELD,STCEITEC-STCELD,0,0,0,0,0,0,0,0,0)           
* Header text                                                                   
         DC    AL1(STCEHTX,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)                 
* Footer text                                                                   
         DC    AL1(STCEFTX,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)                 
* Category added/deleted/changed                                                
         DC    AL1(STCECAT,0,0,0,0,0,0,STCECATC-STCELD)                         
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Category name                                                                 
         DC    AL1(STCECTN,0,0,STCCWINM-STCELD,0,0,0,STCECATC-STCELD)           
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Category text                                                                 
         DC    AL1(STCECTX,0,0,0,0,0,0,STCECATC-STCELD)                         
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Extra information - date                                                      
         DC    AL1(STCEEXT,STCETDT,STCEXDTT-STCELD,0,0)                         
         DC    AL1(STCEXDAT-STCELD,0,0,0,0,0,0,STCEXDTC-STCELD,0,0)             
         DC    AL1(0,0,0,0)                                                     
* Extra information - character                                                 
         DC    AL1(STCEEXT,STCETCHR,STCEXDTT-STCELD,STCEXDAT-STCELD,0)          
         DC    AL1(0,0,0,0,0,0,0,STCEXDTC-STCELD,0,0,0,0,0,0)                   
* Extra information - numeric                                                   
         DC    AL1(STCEEXT,STCETNUM,STCEXDTT-STCELD,STCEXDAT-STCELD,0)          
         DC    AL1(0,0,0,0,0,0,0,STCEXDTC-STCELD,0,0,0,0,0,0)                   
* Extra information - amount                                                    
         DC    AL1(STCEEXT,STCETAMT,STCEXDTT-STCELD,0,0)                        
         DC    AL1(0,STCEXDAT-STCELD,0,0,0,0,0,STCEXDTC-STCELD,0,0)             
         DC    AL1(0,0,0,0)                                                     
* Extra information - checkbox Y/N                                              
         DC    AL1(STCEEXT,STCETYN,STCEXDTT-STCELD,0,0)                         
         DC    AL1(0,0,0,0,0,0,0,STCEXDTC-STCELD,STCEXDAT-STCELD,0)             
         DC    AL1(0,0,0,0)                                                     
* Extra information - dropdown list                                             
         DC    AL1(STCEEXT,STCETDRP,STCEXDTT-STCELD,STCEXDAT-STCELD,0)          
         DC    AL1(0,0,0,0,0,0,0,STCEXDTC-STCELD,0,0,0,0,0,0)                   
* Foreign currency rate                                                         
         DC    AL1(STCEFCR,0,0,0,0,0,0,0)                                       
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Foreign currency code                                                         
         DC    AL1(STCEFCC,0,0,0,0,0,0,0)                                       
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Commission rate                                                               
         DC    AL1(STCECRA,0,0,0,0,0,0,0)                                       
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Suppress print of lines if zero amount                                        
         DC    AL1(STCEPR0,0,0,0,0,0,0,0)                                       
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Suppress print of lines if zero amount and no description                     
         DC    AL1(STCEPRD,0,0,0,0,0,0,0)                                       
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Estimate copied                                                               
         DC    AL1(STCECOP,0,0,STCEMRGN-STCELD,L'STCEMRGN,0,0,0)                
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Client approver                                                               
         DC    AL1(STCECAP,0,0,0,0,0,0,0)                                       
         DC    AL1(0,0,0,0,0,0,STCEAPID-STCELD,0,0,0,0)                         
* Internal approver                                                             
         DC    AL1(STCEIAP,0,0,0,0,0,0,0)                                       
         DC    AL1(0,0,0,0,0,0,STCEAPID-STCELD,0,0,0,0)                         
* PDF attachment                                                                
         DC    AL1(STCEATA,0,0,0,0,0,0,0)                                       
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Unassigned checkbox                                                           
         DC    AL1(STCEUNA,0,0,0,0,0,0,0)                                       
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* PDF attachment description                                                    
         DC    AL1(STCEDES,0,0,0,0,0,0,0)                                       
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Approval comments                                                             
         DC    AL1(STCAPCMT,0,0,STCECOMM-STCELD,0,0,0,0)                        
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Client contact approver date                                                  
         DC    AL1(STCAPDTE,0,0,0,0,STCECAPD-STCELD,0,0)                        
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Print all lines                                                               
         DC    AL1(STCPRNAL,0,0,0,0,0,0,0)                                      
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Estimate replaced                                                             
         DC    AL1(STCREPCD,0,0,STCECOMM-STCELD,0,0,0,0)                        
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Approval comments for internal                                                
         DC    AL1(STCAPCMI,0,0,STCECOMM-STCELD,0,0,0,0)                        
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Additional text                                                               
         DC    AL1(STCEETX,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)                 
* Print time rates                                                              
         DC    AL1(STCEPTRT,0,0,0,0,0,0,0)                                      
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Suppress time rates                                                           
         DC    AL1(STCESTRT,0,0,0,0,0,0,0)                                      
         DC    AL1(0,0,0,0,0,0,0,0,0,0,0)                                       
* Electronic status/comments                                                    
         DC    AL1(STCSTATE,0,0,STCECOMT-STCELD,0,STCEEXPD-STCELD,0,0)          
         DC    AL1(0,0,0,0,0,0,0,0,STCELSTC-STCELD,STCECIDN-STCELD)             
         DC    AL1(STCECSEQ-STCELD)                                             
AUDTABX  DC    X'FF'                                                            
                                                                                
AUSTAB   DS    0X                                                               
         DC    AL1(RQ_ECSCQ,ESTKCREA,0,OINPROG,STCEAEA,STCEADD)                 
         DC    AL1(RQ_ECSBQ,ESTKSUBM,ESTKSINA,OSUBINT,STCESTA,STCECHG)          
         DC    AL1(RQ_ECSIQ,ESTKINTA,0,OINTAPP,STCESTA,STCECHG)                 
         DC    AL1(RQ_ECSSQ,ESTKSUBM,0,OSUBCLI,STCESTA,STCECHG)                 
         DC    AL1(RQ_ECSAQ,ESTKCAPP,0,OCLIAPP,STCESTA,STCECHG)                 
         DC    AL1(RQ_ECSMQ,0,ESTKMERG,0,STCESMR,STCECHG)                       
         DC    AL1(RQ_ECSRQ,ESTKREJE,0,OREJECT,STCESTA,STCECHG)                 
         DC    AL1(RQ_ECSDQ,ESTKLOGD,0,OLOGDEL,STCEDEE,STCEDEL)                 
AUSTABX  DC    X'FF'                                                            
                                                                                
WCTYLIT  DC    C'Work code type'                                                
EXRATLIT DC    C'Exchange rate'                                                 
CURRLIT  DC    C'Currency'                                                      
LANGLIT  DC    C'Language'                                                      
CODELIT  DC    C'Item code'                                                     
WCRQLIT  DC    C'Work code request'                                             
EDTELIT  DC    C'Estimate date'                                                 
SEQULIT  DC    C'Item sequence number'                                          
BILTLIT  DC    C'Billing type'                                                  
                                                                                
LVALUES  DS    0D                                                               
         DC    A(FLTKPAS)                                                       
         DC    A(FLTKART)                                                       
         DC    A(FLTRART)                                                       
LVALUESL EQU   *-LVALUES                                                        
                                                                                
PDFTYPT  DS    0D                                                               
*                                                                               
         DC    C'FXB'                                                           
         DC    AL1(TRNTBILL)                                                    
         DC    AL1(TRNTCLBL)                                                    
*                                                                               
         DC    C'M14'                                                           
         DC    AL1(TRNTMEBL)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
         SPACE 1                                                                
SAVED    DSECT                                                                  
WVALUES  DS    0X                                                               
ADCONS   DS    0A                                                               
AFLTKPAS DS    A                                                                
AFLTKART DS    A                                                                
AFLTRART DS    A                                                                
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
WVALUEL  EQU   *-WVALUES                                                        
AMASTC   DS    A                   A(MASTC)                                     
RUNMODE  DS    XL(L'RUNPMODE)      RUNNER/DDLINK MODE                           
TSAROBUF DS    XL(TSPXTNL)         TSAR block for optimisation buffer           
AGENCY   DS    XL(L'CUXCPY)        Saved agency code for key reading            
                                                                                
***********************************************************************         
* REQUEST IN/OUT DEFINITION IN STORAGE                                *         
***********************************************************************         
SAVEVAR  DS    0D                                                               
                                                                                
QVALUES  DS    0X                  ** REQUEST VALUES                            
RQ_DATA  DS    0XL1                - Request data                               
RQ_ECTYP DS    CL1                 - type                                       
RQ_ECTSQ EQU   C'S'                  * by status                                
RQ_ECTAQ EQU   C'A'                  * by account                               
RQ_ECTLQ EQU   C'L'                  * list for summary (recent ests)           
*&&UK                                                                           
RQ_ECCLI DS    CL5                 - client code                                
RQ_ECPRO DS    CL2                 - product code                               
*&&                                                                             
*&&US                                                                           
RQ_ECCLI DS    CL(CLLQ)            - client code                                
RQ_ECPRO DS    CL(PRLQ)            - product code                               
*&&                                                                             
RQ_ECJOB DS    CL7                 - job code                                   
RQ_ECSTA DS    CL1                 - status                                     
RQ_ECSCQ EQU   C'C'                  * created                                  
RQ_ECSBQ EQU   C'B'                  * submitted to internal approver           
RQ_ECSIQ EQU   C'I'                  * internally approved                      
RQ_ECSSQ EQU   C'S'                  * submitted (to client)                    
RQ_ECSAQ EQU   C'A'                  * (client) approved                        
RQ_ECSMQ EQU   C'M'                  * merged                                   
RQ_ECSRQ EQU   C'R'                  * rejected                                 
RQ_ECSDQ EQU   C'D'                  * deleted                                  
RQ_ECAPP DS    CL1                 - awaiting approval view (Y/N/I)?            
RQ_ECGAO DS    CL1                 - GOAV call Y/N                              
RQ_ECOVL DS    CL1                 - override limit list Y/N                    
         ORG   RQ_DATA                                                          
RQ_EACT  DS    CL12                Client product job                           
RQ_ESEQ  DS    XL1                 Estimate sequence number                     
         ORG   RQ_DATA                                                          
RQ_OFFC  DS    CL2                 Office                                       
RQ_CLI   DS    CL6                 Client                                       
RQ_WC    DS    CL2                 Work code                                    
RQ_APL   DS    XL1                 Application                                  
RQ_ESTMQ EQU   1                   - estimates                                  
RQ_ORDRQ EQU   2                   - orders                                     
RQ_SUPC  DS    CL14                Supplier code including unit ledger          
RQ_TYPE  DS    CL1                 Work code type filter                        
RQ_INTQ  EQU   C'I'                - Internal                                   
RQ_EXTQ  EQU   C'E'                - External                                   
RQ_CURR  DS    CL3                 Currency                                     
RQ_XRAT  DS    CL14                Exchange rate                                
RQ_LANG  DS    CL1                 Language Y/N                                 
RQ_CODE  DS    CL4                 Item code                                    
RQ_DTE   DS    XL3                 Date                                         
RQ_SEQ   DS    CL6                 Item sequence number                         
         ORG   RQ_DATA         **  PDFGEN request **                            
RQ_BILNO DS    CL6                 Bill number                                  
RQ_CRDAT DS    XL2                 Created date                                 
RQ_USRID DS    XL2                 User id                                      
RQ_BILLT DS    CL3                 Bill type                                    
         ORG                                                                    
RQMAXQ   EQU   *-RQ_DATA+64        <--- adjust for big RQ entries               
                                                                                
QVALUESL EQU   *-QVALUES                                                        
                                                                                
         EJECT                                                                  
DVALUES  DS    0D                  Local storage                                
X#TEMP   DS    CL64                                                             
X#HALF   DS    H                                                                
X#CDAT   DS    XL2                 Temporary compressed date field              
X#ESTCNT DS    XL4                 Estimate count                               
XL#TODP  DS    XL3                 Today's date packed                          
XL#TODC  DS    XL2                 Today's date compressed                      
                                                                                
X#OREST DS     0X                  Order from estimate statuses                 
X#ORIPR DS     CL1                 Create order from in progress est            
X#ORSUI DS     CL1                 Create order from internally sub             
X#ORIAP DS     CL1                 Create order from internally appr            
X#ORSUC DS     CL1                 Create order from client submitted           
X#ORCAP DS     CL1                 Create order from client approved            
X#ORESL EQU    *-X#OREST                                                        
*                                                                               
*&&UK                                                                           
X#MAXIOQ EQU   3000                                                             
*&&                                                                             
*&&US                                                                           
X#MAXIOQ EQU   2000                                                             
*&&                                                                             
X#MAXGAO EQU   400                 MAX N'ESTS RETURNED FOR GAOV REQUEST         
*                                  (ELSE OTHER OVLS RUNPARMS CLOBBERED)         
X#NICST  DS    PL3                                                              
X#NICEN  DS    PL3                                                              
X#TVRST  DS    PL3                                                              
X#TVREN  DS    PL3                                                              
                                                                                
X#PID    DS    CL8                 PID                                          
X#NAM    DS    CL36                Name                                         
X#CLIPN  DS    CL36                Client printed name                          
X#PROPN  DS    CL36                Product printed name                         
X#JOBPN  DS    CL36                Job printed name                             
X#FSTNM  DS    CL16                First name                                   
X#LSTNM  DS    CL16                Last name                                    
X#MRAIE  DS    CL52                Email address                                
X#MRAIT  DS    CL5                 Phone extension                              
X#WCESP  DS    PL6                 Work-Code Est % for Contingency              
X#JLO    DS    CL1                 Jobs order lock                              
X#JOBDS  DS    CL1                 Job draft status                             
X#JOBDL  DS    CL1                 Job lock status                              
X#SJAD1  DS    CL(L'ADRADD1)       Job address 1                                
X#SJAD2  DS    CL(L'ADRADD1)       Job address 2                                
X#SJAD3  DS    CL(L'ADRADD1)       Job address 3                                
X#SJAD4  DS    CL(L'ADRADD1)       Job address 4                                
X#SJAD5  DS    CL(L'ADRADD1)       Job address 5                                
X#GAPS   DS    CL(L'GOGAPS)        GAP in use                                   
X#GDES   DS    CL(L'GOGDES)        Default expiration period estimates          
X#DNDV   DS    CL(L'GODNDV)        Default no. of days to view est              
X#GARA   DS    CL(L'GOGARA)        GAP approval required by all receip          
X#GEMX   DS    CL(L'GOGEMX)        GAP email extension                          
X#GEMD   DS    CL(L'GOGEMD)        GAP email disclaimer                         
X#NJLE   DS    CL(L'GONJLE)        No job level entry                           
X#BILO   DS    CL(L'GOBILO)        Billable only                                
X#CECE   DS    CL(L'GOCECE)        Copy estimate copy extra data                
X#CTEO   DS    CL(L'GODCTEO)       Default copy text from est to ord            
X#ACUA   DS    CL(L'GOACUA)        Auto copy from uncommitted amounts           
X#SWPD   DS    CL(L'GOSWPD)        Suppress w/c printing default                
X#COES   DS    CL(L'GOCOES)        Create estimate from status                  
*                                                                               
X#WINDS  DS    0X                  W/C INDICATORS                               
X#ITEWC  DS    CL1                 W/C ITEM FLAG                                
X#COMMY  DS    CL1                 W/C COMMISSION FLAG                          
X#CONTI  DS    CL1                 CONTINGENCY                                  
X#MRGES  DS    CL1                 MERGED FROM ESTIMATE                         
X#OVITT  DS    CL1                 OVERRIDE ITEM TEXT                           
X#ESACT  DS    CL1                 ESTIMATE USED ACTUALS                        
X#WINDL  EQU   *-X#WINDS                                                        
*                                                                               
X#IINDS  DS    0X                  ITEM INDICATORS                              
X#IDESC  DS    CL1                 DESCRIPTION AMEND                            
X#IFLEX  DS    CL1                 FLEXIBLE PRICE                               
X#INOPR  DS    CL1                 NO PRICE                                     
X#IPROV  DS    CL1                 PRICE OVERRIDEN                              
X#ITIME  DS    CL1                 TIME ITEM                                    
X#IINDL  EQU   *-X#IINDS                                                        
*                                                                               
X#GAPE1  DS    CL(L'FFTEML1)       GAP email address 1                          
X#GAPE2  DS    CL(L'FFTEML1)       GAP email address 2                          
X#GAPE3  DS    CL(L'FFTEML1)       GAP email address 3                          
X#GAPE4  DS    CL(L'FFTEML1)       GAP email address 4                          
X#GAPE5  DS    CL(L'FFTEML1)       GAP email address 5                          
X#IETX1  DS    CL180               Item additional text 1                       
X#IETX2  DS    CL180               Item additional text 2                       
X#UNIT   DS    CL15                                                             
X#UNLA   DS    CL15                                                             
                                                                                
DSDICTL  DS    0C                                                               
AC@TIME  DS    CL20                                                             
AC@MAT   DS    CL20                                                             
AC@EST   DS    CL20                                                             
                                                                                
SVSJACT  DS    CL(L'ACTKULA)       Saved SJ account for key reading             
SVGSKY   DS    CL(STCCLNAM-STCUSER)  Saved key for main aud elem                
SAVEKEY  DS    CL64                Saved iokey                                  
                                                                                
DA_PID   DS    XL(L'SA0KNUM)                                                    
DA_XHDR  DS    XL(L'XDFOCOD)                                                    
GBUFAREA DS    XL(OB_LNQ)          Area for buffering GETOPT                    
         ORG   DA_PID                                                           
DI_AFCX  DS    XL7                 Exchange rate                                
DI_SEQ   DS    XL3                 Hex item sequence number                     
DI_DTEC  DS    XL2                 Compressed date of item request              
DI_FCUR  DS    CL(CURTABL)         Saved F/C CURTAB entry (no names)            
DI_ACUR  DS    CL(CURTABL)         Saved AGY CURTAB entry (no names)            
DVALUESL EQU   *-DVALUES                                                        
                                                                                
OVALUES  DS    0D                                                               
OA_TIME  DS    CL6                 Audit time                                   
OA_FNM   DS    CL16                First name  - user making change             
OA_MNM   DS    CL16                Middle name                                  
OA_LNM   DS    CL58                Last name                                    
OA_FNAM  DS    CL16                First name  - approver details               
OA_MNAM  DS    CL16                Middle name                                  
OA_LNAM  DS    CL58                Last name                                    
OA_SEQ   DS    XL1                 Text sequence                                
OA_IDNO  DS    XL2                 Text ID number sequence                      
OA_ELST  DS    XL1                 Electronic status                            
OA_APPL  DS    XL1                 Application                                  
OA_ACTN  DS    XL1                 Action                                       
OA_TYPE  DS    XL1                 Type of change                               
OA_STAT  DS    XL1                 Status                                       
OA_CHARD DS    CL200               Character data                               
OA_DATE  DS    XL3                 Date                                         
OA_AMNT  DS    PL6                 Amount                                       
OA_CATG  DS    CL2                 Category                                     
OA_WC    DS    CL2                 Work code                                    
OA_ITEM  DS    CL12                Item code or 1R account                      
OA_CURC  DS    CL3                 Currency code                                
OA_XRAT  DS    XL7                 Foreign currency exchange rate               
OA_XHDR  DS    CL30                Extra data header value                      
OA_XCHK  DS    CL1                 Y/N checkbox                                 
OA_PER   DS    CL8                 PID code of person quoted                    
OA_PID   DS    CL8                 PID code of person making change             
         ORG   OVALUES                                                          
OI_PRST  DS    XL1                 Price status                                 
OI_FIXDQ EQU   1                   - fixed price                                
OI_FLXQ  EQU   2                   - flexible price                             
OI_NOPRQ EQU   3                   - no price                                   
OI_DSAM  DS    CL1                 Description can be amended - Y/N             
OI_PRICE DS    PL6                 Item price                                   
OI_PDRID DS    CL1                 Item price has been derived - Y/N            
         ORG   OVALUES                                                          
OP_ALPHA DS    CL2                 Alpha id                                     
OP_CRDAT DS    XL2                 Created date                                 
OP_USRID DS    XL2                 User id                                      
OP_BILLT DS    CL3                 Bill type                                    
         ORG                                                                    
OVALUESL EQU   *-OVALUES                                                        
*                                                                               
SAVEVARL EQU   *-SAVEVAR                                                        
         ORG   SAVED+(L'SVSERVER-(*-SAVED))  Online SAVED overflow trap         
         EJECT                                                                  
***********************************************************************         
* included books and DSECTS                                           *         
***********************************************************************         
                                                                                
* Audit status table                                                            
                                                                                
AUSTABD  DSECT                                                                  
AUSTCHAR DS    XL1                 Old character status                         
AUSTSTB1 DS    XL1                 Status byte 1 on estimate record             
AUSTSTB2 DS    XL1                 Status byte 2 on estimate record             
AUSTENUM DS    XL1                 New enumerated status                        
AUSTTYPE DS    XL1                 New type                                     
AUSTACTN DS    XL1                 New action                                   
AUSTABL  EQU   *-AUSTABD           Length of audit status entry                 
                                                                                
                                                                                
* Audit table                                                                   
                                                                                
AUDTABD  DSECT                                                                  
AUDATYP  DS    XL1                 Data type                                    
AUDSTYP  DS    XL1                 Sub data type                                
AUDDSTY  DS    XL1                 Displacement to sub data type                
AUDDCHR  DS    XL1                 Displacement to char/numeric data            
AUDLCHR  DS    XL1                 Length of char/numeric data                  
AUDDDTE  DS    XL1                 Displacement to date data                    
AUDDAMT  DS    XL1                 Displacement to amount data                  
AUDDCAT  DS    XL1                 Displacement to category data                
AUDDWC   DS    XL1                 Displacement to work code data               
AUDDITM  DS    XL1                 Displacement to item data                    
AUDDCUR  DS    XL1                 Displacement to currency code                
AUDDCRT  DS    XL1                 Displacement to currency rate                
AUDDHDR  DS    XL1                 Displacement to header data                  
AUDDCHK  DS    XL1                 Displacement to check box data               
AUDDPID  DS    XL1                 Displacement to person/PID data              
AUDDSTA  DS    XL1                 Displacement to status                       
AUDDESTA DS    XL1                 Displacement to electronic status            
AUDDIDN  DS    XL1                 Displacement to ID number for text           
AUDDSEQ  DS    XL1                 Displacement to sequence for text            
AUDTABL  EQU   *-AUDTABD                                                        
                                                                                
SCHCATD  DSECT                                                                  
SCHCELE  DS    XL1                 element                                      
SCHCEOT  EQU   FF                                                               
SCHCLEN  DS    XL1                 length                                       
         ORG   SCHCLEN+L'SCHCLEN   category:                                    
SCHCELQ  EQU   C'C'                                                             
SCHCACO  DS    CL2                 - code                                       
SCHCANA  DS    CL20                - name                                       
SCHCATN  DS    CL20                - total name                                 
SCHCAST  DS    XL1                 - status                                     
SCHCATY  DS    XL1                 - type                                       
SCHCAIN  DS    CL26                - instructions                               
SCHCFNA  DS    CL20                - foreign name                               
SCHCFNT  DS    CL20                - foreign total name                         
SCHCLNQ  EQU   *-SCHCATD                                                        
         ORG   SCHCLEN+L'SCHCLEN   work code:                                   
SCHWELQ  EQU   C'W'                                                             
SCHWTYP  DS    XL1                 - type                                       
SCHWCOD  DS    CL2                 - work code                                  
SCHWCAT  DS    CL2                 - cal category                               
SCHWESP  DS    PL4                 - SCHWCOD ESP percentage                     
SCHWDES  DS    CL36                - override description                       
SCHWLNQ  EQU   *-SCHCATD                                                        
                                                                                
WCSTABD  DSECT                     Work code summary table                      
WCSTWC   DS    CL2                 - w/c                                        
WCSTEOT  EQU   FF                  - end of table                               
WCSTEA   DS    PL6                 - estimate amount                            
WCSTAC   DS    PL6                 - actual charges                             
WCSTPO   DS    PL6                 - purchase orders                            
WCSTHR   DS    PL6                 - all hours                                  
WCSETI   DS    PL6                 - estimated time via items                   
WCSTLNQ  EQU   *-WCSTABD                                                        
                                                                                
WCDTABD  DSECT                     Work code details table                      
WCDTWC   DS    CL2                 - w/c                                        
WCDTEOT  EQU   FF                  - end of table                               
WCDTWN   DS    CL36                - w/c name                                   
WCDTEA   DS    PL6                 - estimate amount                            
WCDTEN   DS    XL1                 - estimate local number                      
WCDTIS   DS    CL1                 - item status Y/N                            
WCDTLNQ  EQU   *-WCDTABD                                                        
                                                                                
CWITABD  DSECT                     Category/Work code/Item table                
CWITCAT  DS    CL2                 - category code                              
CWITWCD  DS    CL2                 - work code                                  
CWITITM  DS    CL4                 - item code                                  
CWITCSN  DS    XL2                 - category sequence no.                      
CWITWSN  DS    XL2                 - work code sequence no.                     
CWITISN  DS    XL2                 - item sequence no.                          
         ORG   CWITISN                                                          
CWITWSB  DS    XL2                 - wc subsequence no.                         
CWITLN1  EQU   *-CWITABD                                                        
CWITIND1 DS    XL1                 - indicator 1                                
CWITI1Q  EQU   X'01'                 (used for est 1)                           
CWITI2Q  EQU   X'02'                 (used for est 2)                           
CWITI3Q  EQU   X'04'                 (used for est 3)                           
CWITI4Q  EQU   X'08'                 (used for est 4)                           
CWITI5Q  EQU   X'10'                 (used for est 5)                           
CWITI6Q  EQU   X'20'                 (used for est 6)                           
*                                                                               
CWITIND2 DS    XL1                 - indicator 2 (other/extra columns)          
CWITPO   EQU   X'01'                 Purchase Orders (POs)                      
MAPPO    EQU   1                                                                
CWITAC   EQU   X'02'                 Actuals (total postings for job)           
MAPAC    EQU   2                                                                
CWITAH   EQU   X'04'                 Actual Hours                               
MAPAH    EQU   3                                                                
CWITBA   EQU   X'08'                 Balance (CE-Actuals)                       
MAPBA    EQU   4                                                                
CWITUN   EQU   X'10'                 Uncommitted (CE-[POs+Actuals])             
MAPUN    EQU   5                                                                
CWITCE   EQU   X'20'                 Current Estimate (CE)                      
MAPCE    EQU   6                                                                
CWITCEH  EQU   X'40'                 Current Estimated Hours (CEH)              
MAPCEH   EQU   7                                                                
CWITOE   EQU   X'80'                 Original Estimate (OE)                     
MAPOE    EQU   8                                                                
*                                                                               
CWITIND3 DS    XL1                 - indicator 3 (other/extra columns)          
CWITOEH  EQU   X'01'                 Original Estimated Hours (OEH)             
MAPOEH   EQU   9                                                                
*&&US                                                                           
CWITHR   EQU   X'02'                 Highest Revision (HR) US only              
MAPHR    EQU   10                                                               
*&&                                                                             
CWITHEH  EQU   X'04'                 Highest Estimated Hours (HEH)              
MAPHEH   EQU   11                                                               
CWITOECE EQU   X'08'                 OE - CE                                    
MAPOECE  EQU   12                                                               
CWITOEAC EQU   X'10'                 OE - Actuals                               
MAPOEAC  EQU   13                                                               
CWITCEOE EQU   X'20'                 CE - OE                                    
MAPCEOE  EQU   14                                                               
*&&US                                                                           
CWITHRCE EQU   X'40'                 Highest revision - CE                      
MAPHRCE  EQU   15                                                               
CWITHRAC EQU   X'80'                 Highest revision - Actuals                 
MAPHRAC  EQU   16                                                               
CWITIND4 DS    XL1                 - indicator 4 (other/extra columns)          
CWITHROE EQU   X'01'                 Highest revision - OE                      
MAPHROE  EQU   17                                                               
*&&                                                                             
CWITLNQ  EQU   *-CWITABD                                                        
                                                                                
MRGTABD  DSECT                     Scheme/Category/Work code/Item table         
MRGTSCH  DS    CL8                 - scheme code                                
MRGTCAT  DS    CL2                 - category code                              
MRGTWCD  DS    CL2                 - work code                                  
*RGTITM  DS    CL4                 - item code                                  
MRGTCSN  DS    XL2                 - category sequence no.                      
MRGTWSN  DS    XL2                 - work code sequence no.                     
MRGTWSB  DS    XL2                 - wc subsequence no.                         
MRGTLNQ  EQU   *-MRGTABD                                                        
                                                                                
PDFTYPD  DSECT                   * Pdf bill type table *                        
PDFDESC  DS    CL3                 Description                                  
PDFTYP1  DS    XL1                 Transaction type 1                           
PDFTYP2  DS    XL1                 Transaction type 2                           
PDFTYPL  EQU   *-PDFTYPD                                                        
******************************************************************              
* Getopt optimisation buffer                                                    
******************************************************************              
OB_D     DSECT                                                                  
OB_KEY   DS    XL64                Record key                                   
OB_KEYL  EQU   *-OB_D                                                           
                                                                                
OB_ERROR DS    XL2                 Error number (Zero=OK)                       
                                                                                
OB_DATA  DS    0X                  Data starts here                             
OB_NAME  DS    CL(L'NAMEREC)       Record name                                  
OB_OTHER DS    0X                  Other data                                   
         ORG   OB_D+512                                                         
                                                                                
OB_DATAL EQU   *-OB_ERROR                                                       
OB_LNQ   EQU   *-OB_D                                                           
*                                                                               
*        PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
       ++INCLUDE ACVATICAND                                                     
       ++INCLUDE GEGENGEN                                                       
       ++INCLUDE GEGENFEE                                                       
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACBRA1D   05/14/20'                                      
         END                                                                    
