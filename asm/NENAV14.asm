*          DATA SET NENAV14    AT LEVEL 013 AS OF 06/22/20                      
*PHASE T31814B                                                                  
NENAV14  TITLE '- Network Navigator - Allocation Interface Download'            
         PRINT NOGEN                                                            
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,SERVERTYPE=TSTALOD,WORKERKEY=NEAL,SEGMENT=Y,     +        
               FACS=FACS,APPEND=Y,REQUEST=*,CODE=CODE,IDF=Y,           +        
               SYSPHASE=SYSPHASE,SYSTEM=NETSYSQ,FILES=FILES,           +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED),LOADFACSOFF=Y,     +        
               BLOCKS2=(B#GWKREC,GWEEKD,B#PXCREC,PXCRECD,              +        
               B#UNTREC,NURECD,B#NETIOD,NETIOD)                                 
                                                                                
TSTALOD  EQU   C'I'                Locally defined server class for now         
                                                                                
B#PRDREC EQU   B#IOA3              I/O 3 used for product reading               
APRDREC  EQU   AIO3,,C'A'                                                       
B#GOLREC EQU   B#IOA3              I/O 3 used for goal reading                  
AGOLREC  EQU   AIO3,,C'A'                                                       
B#PXCREC EQU   B#IOA3              I/O 3 used for exclusion reading             
APXCREC  EQU   AIO3,,C'A'                                                       
B#ESTREC EQU   B#IOA4              I/O 4 used for estimate reading              
AESTREC  EQU   AIO4,,C'A'                                                       
B#NTIREC EQU   B#IOA5              I/O 5 used for NTI reading                   
ANTIREC  EQU   AIO5,,C'A'                                                       
B#UNTREC EQU   B#IOA6              I/O 6 used for unit reading                  
AUNTREC  EQU   AIO6,,C'A'                                                       
B#NETIOD EQU   B#IOA7              I/O 7 used for netio netblock                
ANETIOD  EQU   AIO7,,C'A'                                                       
B#GWKREC EQU   B#IOA8              I/O 8 used for goal week array               
AGWKREC  EQU   AIO8,,C'A'                                                       
         EJECT                                                                  
CODE     NMOD1 0,**NN14**                                                       
                                                                                
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
         DROP  R6,R7,RB                                                         
                                                                                
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
         USING STAPACKD,WORK                                                    
         EJECT                                                                  
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    Test first for run                           
         JNE   PRCWRK                                                           
                                                                                
         MVC   WVALUES(WVALUEL),LVALUES                                         
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    EXITY               No                                           
                                                                                
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
* First for new work                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    Test 'process work' mode                     
         JNE   RUNREQ                                                           
                                                                                
         XC    QVALUES(QVALUEL),QVALUES                                         
         XC    DVALUES(DVALUEL),DVALUES                                         
         MVC   DESTEEND,EFFS                                                    
         LA    R0,QVALS                                                         
         LHI   R1,L'QVALS                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R4,ANETIOD                                                       
         USING NETIOD,R4                                                        
                                                                                
         LA    R0,NETIOD           Initialize NETBLOCK for processing           
         LHI   R1,NETBLKL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   NBSELAGY,LP_AGY                                                  
         MVC   NBACOM,ACOMFACS                                                  
         MVI   NBTRCOPT,NOQ                                                     
         MVC   NBACLI,ACLTREC                                                   
         MVC   NBAIO,AUNTREC                                                    
         LA    R0,XDDEMOS                                                       
         ST    R0,NBADEM                                                        
                                                                                
         L     RF,LP_ACOM                                                       
         USING COMFACSD,RF         RF=A(COMFACS)                                
         MVC   NBDM,CDATAMGR                                                    
         MVC   NBCALLOV,CCALLOV                                                 
         MVC   NBDATCON,CDATCON                                                 
         MVC   NBGETDAY,CGETDAY                                                 
         MVC   NBADDAY,CADDAY                                                   
         MVC   NBHEXOUT,CHEXOUT                                                 
         MVC   NBHELLO,CHELLO                                                   
         MVC   NBDEMCON,VDEMOCON                                                
         MVC   NBDEMADR,CDEMADDR                                                
         MVC   NBDEMAIN,CDEMAINT                                                
         MVC   NBDEMAND,CDEMAND                                                 
         MVC   NBDEMEL,CDEMEL                                                   
         MVC   NBDEMMTH,CDEMOMTH                                                
         MVC   NBDEMOUT,CDEMOUT                                                 
         MVC   NBGTPROF,VGETPROF                                                
                                                                                
         OI    NBSBKEND,NBNODPT2   Set don't read dayparts                      
         OI    NBINDS6,NBI6XDEM    Set extended demo block                      
         MVI   NBWHERE,NBWNETIO                                                 
         DROP  R4,RF                                                            
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    EXITY                                                            
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,SPTDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,SPTFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,STAFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,UNTDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,UNTFIL,(4,0),0                               
                                                                                
         GOTOR (#GETAGY,AGETAGY),LP_AGY                                         
                                                                                
         L     RF,AMASTC           Set trace option                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Run a download request                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    Test 'run request' mode                      
         JNE   EXITY                                                            
                                                                                
         XC    WORK(16),WORK                                                    
         MVC   WORK(3),=C'S0N'                                                  
         MVC   WORK+4(L'LP_AGY),LP_AGY                                          
         MVI   WORK+6,C'N'                                                      
         MVI   WORK+10,C'*'                                                     
                                                                                
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
         MVC   WORK+11(L'COFFICE),COFFICE                                       
         GOTOR VCLUNPK,DMCB,(CPROF+6,CKEYCLT),WORK+7                            
         DROP  R2                                                               
                                                                                
         L     R4,ANETIOD                                                       
         USING NETIOD,R4                                                        
         MVI   WORK+3,C'0'                                                      
         GOTOR VGETPROF,DMCB,WORK,NBUSER,VDATAMGR                               
         MVI   WORK+3,C'1'                                                      
         GOTOR VGETPROF,DMCB,WORK,NBUSER1,VDATAMGR                              
         MVI   WORK+3,C'2'                                                      
         GOTOR VGETPROF,DMCB,WORK,NBUSER2,VDATAMGR                              
         LLC   R0,NBUSER2                                                       
         CVD   R0,RTGEQU                                                        
         DROP  R4                                                               
                                                                                
         MVC   AGENCY,LP_AGY       Set agency alpha id                          
         MVC   AGENCYB,LP_AGYB     Set agency binary value                      
                                                                                
         L     RF,LP_ALPXD         Extract server id                            
         MVC   MQSVR,LP_MQSVR-LP_XD(RF)                                         
         GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         OC    LP_ERROR,LP_ERROR   Test error number is set                     
         JNZ   EXITN                                                            
         J     EXITY               Exit back to DDLINK                          
         EJECT                                                                  
***********************************************************************         
* Allocation Interface request map                                    *         
***********************************************************************         
                                                                                
REQALC   LKREQ H,M#ALOINT,OUTALC,NEXTREQ=REQEND                                 
                                                                                
Media    LKREQ F,01,(I,B#SAVED,QMEDNDX),(U,#VALMED,$VALMED),           +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=(*,MEDCDLIT),COL=*             
Client   LKREQ F,02,(I,B#SAVED,QCLTNDX),(U,#VALCLT,$VALCLT),           +        
               OLEN=L'QCLTX,TEXT=(*,CLTCDLIT),COL=*                             
StrDate  LKREQ F,03,(D,B#SAVED,QSTRDTE),EDAT,TEXT=(*,STRDTLIT),COL=*            
EndDate  LKREQ F,04,(D,B#SAVED,QENDDTE),EDAT,TEXT=(*,ENDDTLIT),COL=*            
Estmates LKREQ F,05,(I,B#SAVED,QESTNDX),LBIN,TEXT=(*,ESTNOLIT),COL=*,  +        
               LIST=Y,RANGE=Y,OLEN=L'EKEYEST                                    
ZeroCost LKREQ F,08,(D,B#SAVED,QZEROCST),CHAR,TEXT=(*,ZEROCLIT),COL=*           
SubMedia LKREQ F,09,(I,B#SAVED,QSMDNDX),CHAR,TEXT=(*,SUBMDLIT),COL=*,  +        
               OLEN=L'MEDTMED,LIST=F                                            
Dayparts LKREQ F,10,(I,B#SAVED,QDPTNDX),(R,VALDPT),TEXT=(*,DPTFTLIT),  +        
               COL=*,OLEN=L'NUKDP,LIST=Y,DEFAULT=Y                              
Lengths  LKREQ F,11,(I,B#SAVED,QSLNNDX),LBIN,TEXT=(*,LENFTLIT),COL=*,  +        
               OLEN=L'NULEN,LIST=Y,RANGE=Y,DEFAULT=Y                            
AlodModl LKREQ F,12,(D,B#SAVED,QALODMOD),LBIN,TEXT=(*,ALODMLIT),COL=*           
CostType LKREQ F,13,(D,B#SAVED,QCOSTTY),CHAR,TEXT=(*,COSTTLIT),COL=*            
Integ?   LKREQ F,14,(D,B#SAVED,QINTEG),CHAR,TEXT=(*,INTEGLIT),COL=*             
WkNoGoal LKREQ F,15,(D,B#SAVED,QWKNGOAL),CHAR,TEXT=(*,WNOGLLIT),COL=*           
MidFlt   LKREQ F,16,(D,B#SAVED,QMIDFLT),CDAT,TEXT=(*,MDFLTLIT),COL=*            
Retain   LKREQ F,17,(D,B#SAVED,QRETAIN),CHAR,TEXT=(*,RALLOLIT),COL=*            
Flavor   LKREQ F,18,(D,B#SAVED,QDEMFLAV),CHAR,TEXT=(*,DMFLALIT),COL=*           
EquRat   LKREQ F,19,(D,B#SAVED,QDEMREQ),CHAR,TEXT=(*,DREQULIT),COL=*            
EquImp   LKREQ F,20,(D,B#SAVED,QDEMIEQ),CHAR,TEXT=(*,DIEQULIT),COL=*            
DskAddr  LKREQ F,50,(D,B#SAVED,QUDSKA),CHAR,TEXT=(*,UDSKALIT),COL=*             
         LKREQ E                                                                
                                                                                
VALDPT   LM    R2,R4,LP_AINP                                                    
         CHI   R3,L'NDPTDPTA                                                    
         JH    EXITN                                                            
         OC    0(2,R2),SPACES                                                   
                                                                                
D        USING NDPTHDRD,IOKEY      Build key of client level daypart            
         XC    IOKEY,IOKEY                                                      
         MVI   D.NDPTKTY,NDPTKTYQ                                               
         MVI   D.NDPTKST,NDPTKSTQ                                               
         MVC   D.NDPTAGM,QMEDX                                                  
         MVC   D.NDPTCLT,QCLTX                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOUNTDIR+B#UNTREC'                      
         JE    VALDPT04                                                         
         DC    H'0'                                                             
                                                                                
VALDPT02 GOTOR (#IOEXEC,AIOEXEC),'IOBSQ+IOUNTDIR+B#UNTREC'                      
VALDPT04 CLC   IOKEY(D.NDPTDPTE-D.NDPTKEY),IOKEYSAV                             
         JNE   VALDPT06                                                         
         CLC   D.NDPTDPTA,0(R2)    Test client level daypart found              
         JE    VALDPT12                                                         
         J     VALDPT02                                                         
                                                                                
VALDPT06 XC    IOKEY,IOKEY         Build key of agency level daypart            
         MVI   D.NDPTKTY,NDPTKTYQ                                               
         MVI   D.NDPTKST,NDPTKSTQ                                               
         MVC   D.NDPTAGM,QMEDX                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOUNTDIR+B#UNTREC'                      
         JE    VALDPT10                                                         
         DC    H'0'                                                             
                                                                                
VALDPT08 GOTOR (#IOEXEC,AIOEXEC),'IOBSQ+IOUNTDIR+B#UNTREC'                      
VALDPT10 CLC   IOKEY(D.NDPTDPTE-D.NDPTKEY),IOKEYSAV                             
         JNE   EXITN                                                            
         CLC   D.NDPTDPTA,0(R2)    Test agency level daypart found              
         JNE   VALDPT08                                                         
                                                                                
VALDPT12 MVC   0(L'NDPTDPTE,R4),D.NDPTDPTE                                      
         J     EXITY                                                            
                                                                                
MEDCDLIT DC    C'Media code'                                                    
CLTCDLIT DC    C'Client code'                                                   
STRDTLIT DC    C'Start date'                                                    
ENDDTLIT DC    C'End date'                                                      
ESTNOLIT DC    C'Estimate number(s)'                                            
ZEROCLIT DC    C'Include zero cost units?'                                      
SUBMDLIT DC    C'Sub-media(s) (for goals)'                                      
DPTFTLIT DC    C'Daypart filter(s)'                                             
LENFTLIT DC    C'Unit length filter(s)'                                         
ALODMLIT DC    C'Allocation model number'                                       
COSTTLIT DC    C'Use Assigned Cost?'                                            
INTEGLIT DC    C'Include integration cost?'                                     
WNOGLLIT DC    C'Weeks with no goals'                                           
MDFLTLIT DC    C'Mid flight date'                                               
RALLOLIT DC    C'Reallocate all units?'                                         
DMFLALIT DC    C'Demo flavor'                                                   
DREQULIT DC    C'Demo ratings equivalency'                                      
DIEQULIT DC    C'Demo impressions equivalency'                                  
UDSKALIT DC    C'Unit disk address'                                             
                                                                                
OUTALC   LKOUT H                                                                
                                                                                
***********************************************************************         
* Run control values record                                           *         
***********************************************************************         
                                                                                
ALCRUN   LKOUT R,X'FA01'                                                        
SvrType  LKOUT C,01,(D,B#SAVED,MQSVR),CHAR                                      
Model    LKOUT C,02,(D,B#SAVED,QALODMOD),LBIN,SENDCHAR=Y                        
         LKOUT E                                                                
                                                                                
ALCGUX   LKOUT R,X'0002'           ** Goals **                                  
Array    LKOUT C,255,(A,ARYGOL)    Goals                                        
         LKOUT E                                                                
                                                                                
ALCDEM   LKOUT R,X'0004'           ** Demo Codes **                             
Array    LKOUT C,255,(A,ARYDEM)    Demo codes                                   
         LKOUT E                                                                
                                                                                
ALCPXC   LKOUT R,X'0006'           ** Exclusion records **                      
Array    LKOUT C,255,(A,ARYPXC)    Exclusion records                            
         LKOUT E                                                                
                                                                                
ALCUNT   LKOUT R,X'0005'           ** Unit records **                           
Array    LKOUT C,255,(A,ARYUNT)    Unit records                                 
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* Goal header record                                                  *         
***********************************************************************         
                                                                                
ARYGOL   LKOUT A,(R,GETGOL),ROWNAME=GXKEY,MULTIROW=Y                            
PRout    LKOUT P,GXKEYMKT,SETMED                                                
SMedCode LKOUT C,01,(D,B#WORKD,BYTE),CHAR                                       
PrdCode  LKOUT C,02,GXKPRDA,CHAR,ND=Y                                           
EstNo    LKOUT C,03,GXKEYEST,LBIN,ND=Y                                          
SecLen   LKOUT C,04,GXKEYSLN,LBIN,ND=Y                                          
DptCode  LKOUT C,05,GXKEYDPT,LBIN,ND=Y                                          
PRout    LKOUT P,GXKPRDA,SETCLS                                                 
PrdClass LKOUT C,06,(D,B#WORKD,BYTE),LBIN,ND=Y                                  
Prout    LKOUT P,GXKEY,SETTRG                                                   
TrgDemo1 LKOUT C,07,(D,B#WORKD,FULL),HEXD,ND=Y,SENDCHAR=Y,             +        
               LEN=L'ESTVTRG1                                                   
TrgDemo2 LKOUT C,08,(D,B#WORKD,FULL1),HEXD,ND=Y,SENDCHAR=Y,            +        
               LEN=L'ESTVTRG1                                                   
TrgDemo3 LKOUT C,09,(D,B#WORKD,FULL2),HEXD,ND=Y,SENDCHAR=Y,            +        
               LEN=L'ESTVTRG1                                                   
Array    LKOUT C,X'0003',(A,ARYGWK)                                             
         LKOUT E                                                                
                                                                                
GETGOL   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JE    GOLINI              Yes - initialize                             
         J     GOLNXT              Else get next record                         
                                                                                
SETMED   L     R1,LP_AINP          Look up sub-media for goal market            
         L     RF,AMEDTAB                                                       
         USING MEDTABD,RF          RF=A(media table)                            
SETMED02 CLI   MEDTABD,MEDTMEOT                                                 
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MEDTGMKT,0(R1)      Match goal market to table                   
         JE    *+12                                                             
         AHI   RF,MEDTABL                                                       
         J     SETMED02                                                         
         MVC   BYTE,MEDTMED        Set media                                    
         DROP  RF                                                               
                                                                                
         LA    RF,GOLSMEDS         Set submedia for unit filter                 
         LA    R0,GOLSMED#                                                      
SETMED04 CLI   0(RF),0             Test already in table                        
         JE    SETMED06                                                         
         CLC   BYTE,0(RF)                                                       
         JE    SETMED06                                                         
         AHI   RF,L'BYTE                                                        
         JCT   R0,SETMED04                                                      
SETMED06 MVC   0(L'BYTE,RF),BYTE                                                
         BR    RE                                                               
                                                                                
SETCLS   L     R1,LP_AINP          Set product class                            
         LAY   RF,PRDLST           Set product classes from PRDTAB              
         USING PRDLST,RF           R4=A(product list)                           
SETCLS02 OC    PRDLPRD,PRDLPRD     Test end of list                             
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   PRDLPRD,0(R1)       Match on product code                        
         JE    *+12                                                             
         AHI   RF,PRDLSTL                                                       
         J     SETCLS02                                                         
         MVC   BYTE,PRDLCLS                                                     
         NI    BYTE,X'0F'          Only leave low order bits on                 
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
SETTRG   L     R1,LP_AINP          Set product/estimate demo targets            
         USING GXKEY,R1                                                         
         MVC   ESTVPRD,GXKPRDA     Build key of TSAR record                     
         MVC   ESTVEST,GXKEYEST                                                 
         DROP  R1                                                               
         GOTOR BUFFER,DMCB,('ESTBUFQ',TSARDH)                                   
         JE    *+6                                                              
         DC    H'0'                Product/estimate not found                   
         MVC   FULL(L'ESTVTRG1),ESTVTRG1                                        
         MVC   FULL1(L'ESTVTRG2),ESTVTRG2                                       
         MVC   FULL2(L'ESTVTRG3),ESTVTRG3                                       
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Initialize for goal and unit reading                                *         
***********************************************************************         
                                                                                
GOLINI   SR    RE,RE               Set media and client                         
         ICM   RE,7,QAMED                                                       
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
         ICM   RE,7,QACLT                                                       
         MVC   QCLTX,LW_DATA1-LW_D(RE)                                          
         XC    GOLSMEDS,GOLSMEDS                                                
         GOTOR (#GETCLT,AGETCLT)   Get client record                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OC    QUDSKA,QUDSKA       Test filter unit                             
         JZ    GOLINI02                                                         
         GOTOR VHEXIN,DMCB,QUDSKA,QUDSKADD,8                                    
                                                                                
GOLINI02 OC    QSTRDTE,QSTRDTE     Test date given                              
         JZ    GOLINI04                                                         
         GOTOR VDATCON,DMCB,(0,QSTRDTE),(2,DSTRDTC)                             
         MVC   DUNTSTDC,DSTRDTC    Set date range for unit driver               
                                                                                
GOLINI04 OC    QENDDTE,QENDDTE                                                  
         JZ    BLDPRD                                                           
         GOTOR VDATCON,DMCB,(0,QENDDTE),(2,DENDDTC)                             
         MVC   DUNTENDC,DENDDTC                                                 
                                                                                
***********************************************************************         
* Build product list (PRDLST) for product class/code conversion       *         
***********************************************************************         
                                                                                
B        USING PLSTPSSV,IOKEY      Build key of product list passive            
BLDPRD   XC    B.PLSTPSSV,B.PLSTPSSV                                            
         MVI   B.PLSTTYPE,PLSTTYPQ                                              
         MVI   B.PLSTSUB,PLSTSUBQ                                               
         MVC   B.PLSTAM,QMEDX                                                   
         MVC   B.PLSTCLT,QCLTX                                                  
         LAY   R4,PRDLST                                                        
         USING PRDLST,R4           R4=A(product list)                           
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOSPTDIR+B#PRDREC'                      
         JE    BLDPRD04                                                         
         DC    H'0'                                                             
                                                                                
BLDPRD02 GOTOR (#IOEXEC,AIOEXEC),'IOBSQ+IOSPTDIR+B#PRDREC'                      
         JE    BLDPRD04                                                         
         DC    H'0'                                                             
                                                                                
BLDPRD04 CLC   B.PLSTPSSV(PLSTXFF-PLSTPSSV),IOKEYSAV                            
         JNE   BLDMKT                                                           
         MVC   PRDLPRD,B.PLSTPRD                                                
         MVC   PRDLNUM,B.PLSTBPRD+(L'PLSTBPRD-L'PRDLNUM)                        
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOSPTFIL+B#PRDREC'                     
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,APRDREC                                                       
         MVC   PRDLCLS,PCLASS-PRDRECD(R1)                                       
         AHI   R4,PRDLSTL          Bump to next list entry                      
         XC    PRDLST(PRDLSTL),PRDLST                                           
         J     BLDPRD02                                                         
         DROP  R4                                                               
                                                                                
***********************************************************************         
* Build market look-up list (DMKTNUM/DMKTLST)                         *         
***********************************************************************         
                                                                                
BLDMKT   L     R2,AMEDTAB          Build a list of goal markets                 
         USING MEDTABD,R2          R2=A(media table)                            
         LA    R3,DMKTLST          R3=A(market list)                            
         SR    R4,R4               R4=N'markets in market list                  
BLDMKT02 CLC   MEDTGMKT,BZEROES    Test goal market set                         
         JE    BLDMKT06                                                         
         CLI   MEDTMED,MEDTMALL    Always send all market goals                 
         JE    BLDMKT04                                                         
         ICM   R1,7,QASMD          RF=A(sub-media filters)                      
         JZ    BLDMKT04                                                         
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R1)                                            
         AHI   R1,LW_LN2Q          Point to sub-media list                      
         BASR  RE,0                                                             
         CLC   MEDTMED,0(R1)       Match to sub-media filter                    
         JE    BLDMKT04                                                         
         AHI   R1,L'MEDTMED                                                     
         BCTR  R0,RE                                                            
         J     BLDMKT06                                                         
                                                                                
BLDMKT04 MVC   0(L'DMKTLST,R3),MEDTGMKT                                         
         AHI   R3,L'DMKTLST                                                     
         AHI   R4,1                                                             
                                                                                
BLDMKT06 AHI   R2,MEDTABL          Bump to next media table entry               
         CLI   MEDTMED,MEDTMEOT                                                 
         JNE   BLDMKT02                                                         
         STCM  R4,3,DMKTNUM        Set number of goal markets in list           
         DROP  R2                                                               
                                                                                
***********************************************************************         
* If no estimates have been given build a list of estimates in the    *         
* WMP (DESTNDX/DAEST) using the estimate date passives                *         
***********************************************************************         
                                                                                
BLDEST   OC    DESTNDX,QESTNDX     Test estimates given                         
         JNZ   BLDEST04                                                         
         OC    QSTRDTE,QSTRDTE     No estimates - start date                    
         JZ    NOMORE                                                           
         OC    QENDDTE,QENDDTE     and end date must be given                   
         JZ    NOMORE                                                           
K        USING EPKEY,IOKEY                                                      
         XC    K.EPKEY,K.EPKEY     Initialize key for reading                   
                                                                                
BLDEST02 GOTOR LP_ASETK,DMCB,(0,ESPKEYT),K.EPKEY,SAVED,('FF',LP_D)              
         JH    BLDEST04                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOSPTDIR+B#ESTREC'                      
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR LP_ASETK,DMCB,(1,ESPKEYT),K.EPKEY,SAVED,('FF',LP_D)              
         JNE   BLDEST02                                                         
         GOTOR LP_AAWMP,DMCB,(L'EPKEYEST,K.EPKEYEST),DESTNDX,255,LP_D           
         J     BLDEST02                                                         
                                                                                
BLDEST04 OC    DAEST,DAEST         Test any estimates found                     
         JZ    NOMORE                                                           
                                                                                
***********************************************************************         
* Build a TSAR buffer of estimate records (ESTRECD) from product      *         
* estimate records - these are read to establish the demo targets     *         
* which are passed in the goal header records - also keep             *         
* track of lowest estimate start date and highest estimate end date   *         
***********************************************************************         
                                                                                
BLDEPR   GOTOR BUFFER,DMCB,('ESTBUFQ',TSAINI),('ESTVKEYL',ESTVALL)              
         MVI   DEMOLIST,FF         Set target demos list is empty               
         XC    #DEMOS,#DEMOS                                                    
                                                                                
K        USING EKEY,IOKEY                                                       
         XC    K.EKEY,K.EKEY       Read product/estimate records                
                                                                                
BLDEPR02 GOTOR (#NXTREC,ANXTREC),DMCB,ESTKEYT,('B#ESTREC',0),          +        
               ('$NXTRSPT',SAVED),0,0                                           
         JNE   BLDWKL                                                           
                                                                                
         L     R1,AESTREC                                                       
         USING ESTRECD,R1          Build TSAR estimate record                   
                                                                                
         OC    DSTRDTC,DSTRDTC     Test start date given                        
         JNZ   BLDEPR04                                                         
         OC    QSTRDTE,QSTRDTE     Seed first value                             
         JZ    *+14                                                             
         CLC   ESTART,QSTRDTE      Test lowest                                  
         JH    BLDEPR04                                                         
         MVC   QSTRDTE,ESTART      Set lowest start date                        
                                                                                
BLDEPR04 OC    DENDDTC,DENDDTC     Test end date given                          
         JNZ   BLDEPR06                                                         
         OC    QENDDTE,QENDDTE     Seed first value                             
         JZ    *+14                                                             
         CLC   EEND,QENDDTE        Test highest                                 
         JNH   BLDEPR06                                                         
         MVC   QENDDTE,EEND        Set highest end date                         
                                                                                
BLDEPR06 MVC   ESTVPRD,EKEYPRD     Set product/estimate/targets                 
         MVC   ESTVEST,EKEYEST                                                  
         MVC   ESTVTRG1(ESTVTRGL),EDEMLIST                                      
         DROP  R1                                                               
                                                                                
         GOTOR BUFFER,DMCB,('ESTBUFQ',TSAADD)                                   
                                                                                
         GOTOR ADDDEM,ESTVTRG1     Add target 1 to demo list                    
         GOTOR ADDDEM,ESTVTRG2     Add target 2 to demo list                    
         GOTOR ADDDEM,ESTVTRG3     Add target 3 to demo list                    
                                                                                
         J     BLDEPR02            Get next product estimate                    
                                                                                
***********************************************************************         
* Add demo to list of target demos (DEMOLIST)                         *         
***********************************************************************         
                                                                                
ADDDEM   OC    0(L'DEMOLIST,R1),0(R1)                                           
         BZR   RE                                                               
         CLI   0(R1),FF                                                         
         BER   RE                                                               
         LA    RF,DEMOLIST                                                      
         LHI   R0,DEMOMAXN                                                      
ADDDEM02 CLI   0(RF),FF            Test end of list                             
         JE    ADDDEM04                                                         
         CLC   0(L'DEMOLIST,RF),0(R1)                                           
         BER   RE                                                               
         AHI   RF,L'DEMOLIST                                                    
         JCT   R0,ADDDEM02                                                      
         DC    H'0'                Too many demos                               
ADDDEM04 MVC   0(L'DEMOLIST,RF),0(R1)                                           
         MVI   L'DEMOLIST(RF),FF                                                
                                                                                
         LH    RF,#DEMOS                                                        
         AHI   RF,1                                                             
         STH   RF,#DEMOS                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* Build list of weeks (WEEKLST)                                       *         
***********************************************************************         
                                                                                
BLDWKL   OC    DSTRDTC,DSTRDTC     Test start date given                        
         JNZ   BLDWKL02                                                         
         OC    QSTRDTE,QSTRDTE     Any estimates found?                         
         JZ    NOMORE              No - Can't do anything                       
         GOTOR VDATCON,DMCB,(0,QSTRDTE),(2,DSTRDTC)                             
         GOTOR (RF),(R1),(0,QENDDTE),(2,DENDDTC)                                
         MVC   DUNTSTDC,DSTRDTC    Set date range for unit driver               
         MVC   DUNTENDC,DENDDTC                                                 
                                                                                
BLDWKL02 MVC   WORK+00(4),VGETBRD  Build list of weeks                          
         MVC   WORK+04(4),VADDAY                                                
         MVC   WORK+08(4),VGETDAY                                               
         MVC   WORK+12(4),VDATCON                                               
         XC    WORK1,WORK1                                                      
         MVI   WORK1+8,1           DEFAULT START ROTATION DAY TO MON            
         L     RE,ACLTREC                                                       
         USING CLTRECD,RE                                                       
         CLI   CSCJROT,0                                                        
         JE    *+10                                                             
         MVC   WORK1+8(1),CSCJROT  GET ROTATION START FROM CLIENT REC           
         DROP  RE                                                               
         GOTOR VMOBILE,DMCB,('WEEKMAX',QSTRDTE),(5,WEEKLST),WORK,WORK1          
                                                                                
         MVC   DGOLSTDC,DSTRDTC    Set goal date                                
         GOTOR VDATCON,DMCB,(2,DSTRDTC),(0,DSTRDTE)                             
         GOTOR VGETDAY,DMCB,DSTRDTE,WORK                                        
         CLI   0(R1),1             Test start day is monday                     
         JE    BLDWKL04                                                         
         SR    R0,R0               No - get monday date for goal weeks          
         ICM   R0,1,0(R1)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SHI   R0,1                                                             
         LNR   R0,R0                                                            
         GOTOR VADDAY,DMCB,DSTRDTE,WORK,(R0)                                    
         GOTOR VDATCON,DMCB,WORK,(2,DGOLSTDC)                                   
                                                                                
K        USING GXKEY,IOKEY                                                      
BLDWKL04 MVI   LP_RMODE,LP_RFRST   Set first time mode for NXTREC               
                                                                                
***********************************************************************         
* Read first/next goal record                                         *         
***********************************************************************         
                                                                                
GOLNXT   GOTOR (#NXTREC,ANXTREC),DMCB,GOLKEYT,('B#GOLREC',0),          +        
               ('$NXTRXSP',SAVED),0,0                                           
         JE    BLDGWK                                                           
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Goal week record                                                    *         
***********************************************************************         
                                                                                
ARYGWK   LKOUT A,(D,B#GWKREC,GWEEKD),ROWWIDTH=GWEEKL,EOT=0,NEWEL=B              
WeekNo   LKOUT C,01,GWEEKNO,LBIN,ND=Y                                           
Dollars  LKOUT C,02,GWDOLLS,SPAK,ND=Y                                           
GRP1     LKOUT C,03,GWGRP1,SPAK,FILTROUT=SETGRPPR,ND=Y                          
GRP2     LKOUT C,04,GWGRP2,SPAK,ND=Y                                            
         LKOUT E                                                                
                                                                                
         USING GWEEKD,R1                                                        
TSTZGOL  L     R1,LP_AINP                                                       
         CP    GWDOLLS,PZERO       Test goal dollars                            
         JNE   EXITY                                                            
         CP    GWGRP1,PZERO        Test GRP                                     
         JNE   EXITY                                                            
         CP    GWGRP2,PZERO        Test GRP                                     
         JNE   EXITY                                                            
         J     EXITN                                                            
                                                                                
SETGRPPR L     R1,LP_AINP                                                       
*        CP    GWDOLLS,PZERO       Test goal dollars                            
*        JNE   SETGRP02                                                         
*        CP    GWGRP1,PZERO        Test GRP                                     
*        JNE   SETGRP02                                                         
*        CP    GWGRP2,PZERO        Test GRP                                     
*        JNE   SETGRP02                                                         
*        J     EXITN                                                            
         DROP  R1                                                               
                                                                                
SETGRP02 L     RF,AGOLREC                                                       
         USING GOALRECD,RF                                                      
         SR    R0,R0                                                            
         ICM   R0,1,GXKEYSLN                                                    
         JZ    EXITY                                                            
         CVD   R0,DUB              Set goal length in DUB                       
         DROP  RF                                                               
                                                                                
         L     R1,LP_AINP                                                       
         USING GWEEKD,R1                                                        
         MP    GWGRP1,PTEN         Set GRP precision for download               
         MP    GWGRP2,PTEN         Set GRP precision for download               
                                                                                
         CLI   QDEMREQ,QDREPRFQ    Test use equivalence profile                 
         JNE   SETGRP04                                                         
         CP    RTGEQU,PZERO        Test rating equivalency                      
         JE    EXITY                                                            
         J     SETGRP06                                                         
                                                                                
SETGRP04 CLI   QDEMREQ,QDREEQUQ    Test rating equivalency requested            
         JNE   EXITY                                                            
         LA    R0,30               Set rating point equivalence                 
         CVD   R0,RTGEQU                                                        
                                                                                
SETGRP06 ZAP   PL16,DUB            Set rating point equivalence                 
         SRP   PL16,4,0                                                         
         DP    PL16,RTGEQU         goal length / rating equivalence             
         ZAP   DUB1,PL16(8)                                                     
                                                                                
         ZAP   PL16,GWGRP1(8)                                                   
         MP    PL16,DUB1                                                        
         SRP   PL16,64-4,5                                                      
         ZAP   GWGRP1,PL16         Set equivalenced GRP precision               
                                                                                
         ZAP   PL16,GWGRP2(8)                                                   
         MP    PL16,DUB1                                                        
         SRP   PL16,64-4,5                                                      
         ZAP   GWGRP2,PL16         Set equivalenced GRP precision               
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
GWEEKD   DSECT ,                   ** Layout of goal week array **              
GWEEKNO  DS    X                   Week number                                  
GWDOLLS  DS    PL8                 Dollars                                      
GWGRP1   DS    PL8                 Target demo 1 value                          
GWGRP2   DS    PL8                 Target demo 2 value                          
GWEEKL   EQU   *-GWEEKD                                                         
SVRDEF   CSECT ,                                                                
                                                                                
***********************************************************************         
* Build goal week array (GWEEEKD) in B#GWKREC                         *         
***********************************************************************         
                                                                                
BLDGWK   ZAP   GWTPOSTS,PZERO      Number of weeks posted                       
         ZAP   GWTDOLLS,PZERO      Total dollars for all posted weeks           
         ZAP   GWTGRP1,PZERO       Total GRP1 for all posted weeks              
         ZAP   GWTGRP2,PZERO       Total GRP2 for all posted weeks              
                                                                                
         L     R0,AGWKREC          Initialize week table                        
         LHI   R1,WEEKMAX*GWEEKL+1                                              
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   BYTE,0                                                           
         L     R2,AGOLREC                                                       
         AHI   R2,GXDATA-GXKEY                                                  
         USING GLEMENT,R2          R2=A(goal week element)                      
BLDGWK02 CLI   GLEMENT,0           Test end of record                           
         JE    BLDGWK14                                                         
         CLI   GLEMENT,GLCODEQ     Test goal week element                       
         JE    BLDGWK06                                                         
                                                                                
BLDGWK04 LLC   R0,GLEN             Bump to next element                         
         AR    R2,R0                                                            
         J     BLDGWK02                                                         
                                                                                
BLDGWK06 CLC   GLWEEK,DGOLSTDC     Test before monday period start              
         JL    BLDGWK04                                                         
         CLC   GLWEEK,DENDDTC      Test after period end                        
         JH    BLDGWK04                                                         
                                                                                
         LA    R1,WEEKLST          Point to week list                           
         L     RE,AGWKREC          Point to goal week array                     
E        USING GWEEKD,RE                                                        
         LHI   R0,1                R0=Week counter                              
BLDGWK08 CLC   GLWEEK,2(R1)        Test within current week                     
         JNH   BLDGWK10            Yes                                          
         AHI   R1,L'WEEKLST        Point to next week                           
         AHI   RE,GWEEKL                                                        
         AHI   R0,1                                                             
         J     BLDGWK08                                                         
                                                                                
BLDGWK10 CLI   E.GWEEKNO,0         Test first post for this week                
*        JE    *+6                                                              
*        DC    H'0'                How could we have two?                       
         STC   R0,E.GWEEKNO        Set week number                              
                                                                                
         ICM   R0,15,GLBUDGET      Budget dollars                               
         CVD   R0,DUB                                                           
         ICM   R0,15,GLGRP         GRP1                                         
         CVD   R0,DUB1                                                          
         ZAP   DUB2,PZERO                                                       
         CLI   GLEN,GLEN1Q                                                      
         JNH   BLDGWK12                                                         
         ICM   R0,15,GLGRP2        GRP2                                         
         CVD   R0,DUB2                                                          
                                                                                
BLDGWK12 CP    DUB,PZERO           Test dollars/GRPs on goal                    
         JE    *+8                                                              
         MVI   BYTE,1                                                           
         CP    DUB1,PZERO                                                       
         JE    *+8                                                              
         MVI   BYTE,1                                                           
         CP    DUB2,PZERO                                                       
         JE    *+8                                                              
         MVI   BYTE,1                                                           
                                                                                
         ZAP   E.GWDOLLS,DUB       Set week values                              
         ZAP   E.GWGRP1,DUB1                                                    
         ZAP   E.GWGRP2,DUB2                                                    
         DROP  E                                                                
                                                                                
         AP    GWTPOSTS,PONE       Update totals                                
         AP    GWTDOLLS,DUB                                                     
         AP    GWTGRP1,DUB1                                                     
         AP    GWTGRP2,DUB2                                                     
                                                                                
         J     BLDGWK04            Bump to next week element                    
                                                                                
BLDGWK14 CP    GWTPOSTS,PZERO      Test any weeks posted                        
         JE    GOLNXT              No - get next goal record                    
         CLI   BYTE,0              Test any dollars/GRPs on goal                
         JE    GOLNXT                                                           
         ZAP   DUB,GWTPOSTS                                                     
                                                                                
         ZAP   WORK(16),GWTDOLLS                                                
         SRP   WORK(16),1,0                                                     
         DP    WORK(16),DUB                                                     
         SRP   WORK(8),64-1,5                                                   
         ZAP   GWTDOLLS,WORK(8)    Average dollars                              
                                                                                
         ZAP   WORK(16),GWTGRP1                                                 
         SRP   WORK(16),1,0                                                     
         DP    WORK(16),DUB                                                     
         SRP   WORK(8),64-1,5                                                   
         ZAP   GWTGRP1,WORK(8)     Average GRP1                                 
                                                                                
         ZAP   WORK(16),GWTGRP2                                                 
         SRP   WORK(16),1,0                                                     
         DP    WORK(16),DUB                                                     
         SRP   WORK(8),64-1,5                                                   
         ZAP   GWTGRP2,WORK(8)     Average GRP2                                 
                                                                                
         L     RE,AGWKREC          Point to goal week array                     
E        USING GWEEKD,RE                                                        
         LA    R1,WEEKLST                                                       
         ZAP   DUB,PZERO                                                        
         ZAP   DUB1,PZERO                                                       
         ZAP   DUB2,PZERO                                                       
         LHI   R0,1                R0=Week counter                              
BLDGWK16 CLC   0(2,R1),DENDDTC     Test after period end                        
         JH    EXITY               Exit to send goal headers & weeks            
         CLI   E.GWEEKNO,0         Test current week posted                     
         JNE   BLDGWK22            Yes                                          
         STC   R0,E.GWEEKNO        Build missing week entry                     
                                                                                
         CLI   QWKNGOAL,QWKNPRVQ   Option to use previous week                  
         JNE   BLDGWK18                                                         
         ZAP   E.GWDOLLS,DUB                                                    
         ZAP   E.GWGRP1,DUB1                                                    
         ZAP   E.GWGRP2,DUB2                                                    
         J     BLDGWK24                                                         
                                                                                
BLDGWK18 CLI   QWKNGOAL,QWKNAVGQ   Option to use average week                   
         JNE   BLDGWK20                                                         
         ZAP   E.GWDOLLS,GWTDOLLS                                               
         ZAP   E.GWGRP1,GWTGRP1                                                 
         ZAP   E.GWGRP2,GWTGRP2                                                 
         J     BLDGWK24                                                         
                                                                                
BLDGWK20 ZAP   E.GWDOLLS,PZERO     No option - set no goals for week            
         ZAP   E.GWGRP1,PZERO                                                   
         ZAP   E.GWGRP2,PZERO                                                   
         J     BLDGWK24                                                         
                                                                                
BLDGWK22 ZAP   DUB,E.GWDOLLS       Save for 'use previous week' option          
         ZAP   DUB1,E.GWGRP1                                                    
         ZAP   DUB2,E.GWGRP2                                                    
                                                                                
BLDGWK24 AHI   RE,GWEEKL           Bump to next week                            
         AHI   R1,L'WEEKLST                                                     
         AHI   R0,1                                                             
         J     BLDGWK16            Go process it                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Demo record                                                         *         
***********************************************************************         
                                                                                
ARYDEM   LKOUT A,(D,B#SAVED,DEMOLIST),ROWWIDTH=L'DEMOLIST,EOT=FF                
DemCd    LKOUT C,01,DEMOLIST,HEXD                                               
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* Unit records                                                        *         
***********************************************************************         
                                                                                
ARYUNT   LKOUT A,(R,NXTUNT),ROWNAME=UNTREC,MULTIROW=Y                           
Array    LKOUT C,255,(A,ARYUNT01)                                               
         LKOUT E                                                                
                                                                                
NXTUNT   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTUNT02                                                         
         MVI   NXTUFLAG,NXTUFUNT   Set unit read mode                           
         CLI   QPOD,YESQ           Test POD run                                 
         JNE   NXTUNT02                                                         
         XC    LASTNET,LASTNET     Set first for new network                    
         GOTOR BUFFER,DMCB,('PODBUFQ',TSAINI),('PODKEYL',PODVALL)               
                                                                                
NXTUNT02 TM    NXTUFLAG,NXTUFBUF   Test in buffer read mode                     
         JNZ   NXTUNT32            Yes                                          
                                                                                
         GOTOR (#NXTREC,ANXTREC),DMCB,UNTKEYT,('B#UNTREC',0),          +        
               ('$NXTRUNT',SAVED),0,FLTUNT                                      
         JNE   NXTUNT28            Jump if all data read                        
                                                                                
NXTUNT04 L     R3,AUNTREC                                                       
         USING NURECD,R3           R3=A(Unit record)                            
                                                                                
         CLI   QPOD,YESQ           Test POD run                                 
         JNE   NXTUNT06                                                         
         CLC   LASTNET,NUKNET      Test change of network                       
         JE    NXTUNT06            No - post this unit                          
         OC    LASTNET,LASTNET     Test first for new network                   
         JNZ   NXTUNT30            Yes - send last network                      
                                                                                
         MVC   LASTNET,NUKNET      Set network code & initialize buffer         
         GOTOR BUFFER,DMCB,('PODBUFQ',TSAINI),('PODKEYL',PODVALL)               
                                                                                
NXTUNT06 L     R2,ANETIOD                                                       
         USING NETIOD,R2           R2=A(NETBLOCK)                               
                                                                                
         LH    RF,#DEMOS           Set demo list                                
         MHI   RF,L'EDEMLIST                                                    
         SHI   RF,1                                                             
         BASR  RB,0                                                             
         MVC   XDDEMOS(0),DEMOLIST                                              
         EX    RF,0(RB)                                                         
                                                                                
         MVI   NBHUNOPT,C'Y'       Set want impressions in thousands            
         MVI   NBPREOPT,C'Y'       Set want ratings to 2dp                      
         MVI   NBESTOPT,NBESTOUQ   Set estimate default                         
         MVI   NBACTOPT,0                                                       
         MVI   NBDEMRAW,0          Set default to return guarantees             
         MVC   NBNTISTA,UNTNTI     Set NTI station                              
                                                                                
         CLI   QDEMFLAV,QDFACTQ    Test actuals                                 
         JNE   *+12                                                             
         MVI   NBESTOPT,0                                                       
         MVI   NBACTOPT,YESQ                                                    
                                                                                
         CLI   QDEMFLAV+1,NOQ      Test do not apply guarantees                 
         JNE   *+8                                                              
         MVI   NBDEMRAW,YESQ       Set to do not return guarantees              
                                                                                
         CLI   QDEMREQ,QDREPRFQ    Test use equivalence profile                 
         JE    NXTUNT08                                                         
         MVI   NBUSER2+0,30        Set default                                  
         CLI   QDEMREQ,QDREEQUQ    Test equivalence                             
         JE    *+8                                                              
         MVI   NBUSER2+0,0                                                      
                                                                                
NXTUNT08 CLI   QDEMIEQ,QDIEPRFQ    Test use equivalence profile                 
         JE    NXTUNT10                                                         
         MVI   NBUSER+1,30         Set default                                  
         CLI   QDEMIEQ,QDIEEQUQ    Test equivalence                             
         JE    *+8                                                              
         MVI   NBUSER+1,0                                                       
                                                                                
NXTUNT10 GOTOR VNETVALU,DMCB,NETIOD                                             
                                                                                
         LA    RE,UNTRRTG          Set UNTRRTG with raw ratings                 
         LA    RF,XDESTDEM                                                      
         CLI   QDEMFLAV,QDFACTQ    Test actuals                                 
         JNE   *+8                                                              
         LA    RF,XDACTDEM                                                      
         LH    R0,#DEMOS                                                        
NXTUNT12 MVC   0(L'XDESTRTG,RE),L'XDESTVPH(RF)                                  
                                                                                
         CLI   UNTPOSTY,C'N'       Test post type                               
         JE    *+12                                                             
         CLI   UNTPOSTY,C'S'                                                    
         JNE   NXTUNT14                                                         
         SR    R4,R4                                                            
         ICM   R4,3,L'XDESTVPH(RF)                                              
         MHI   R4,10               Set rating to 2 decimals                     
         STCM  R4,3,0(RE)                                                       
                                                                                
NXTUNT14 AHI   RE,L'XDESTRTG                                                    
         MVI   0(RE),FF                                                         
         AHI   RF,XDESTLNQ                                                      
         JCT   R0,NXTUNT12                                                      
         DROP  R2                                                               
                                                                                
         XC    PODKEY(PODKEYL),PODKEY                                           
                                                                                
         ICM   R0,15,NUACTUAL                                                   
         CVD   R0,DUB                                                           
         ZAP   UNTVACTC,DUB        Set actual cost                              
                                                                                
         ICM   R0,15,NUASSIGN                                                   
         CVD   R0,DUB                                                           
         ZAP   UNTVASGC,DUB        Set assigned cost                            
                                                                                
         ICM   R0,15,NUINTEG                                                    
         CVD   R0,DUB                                                           
         ZAP   UNTVINTC,DUB        Set integration cost                         
                                                                                
         ICM   R0,15,NUACTUAL      Cost is actual                               
         CLI   QCOSTTY,YESQ                                                     
         JNE   *+8                                                              
         ICM   R0,15,NUASSIGN      or assigned                                  
         SR    RF,RF                                                            
         ICM   RF,15,NUINTEG                                                    
         CLI   QINTEG,YESQ         Test including integration                   
         JNE   *+6                                                              
         AR    R0,RF                                                            
         CVD   R0,DUB                                                           
         ZAP   UNTVCOST,DUB        Set unit cost                                
                                                                                
         LTR   R0,R0               Set CC based on value                        
         MVI   PODKCSW,0           Zero                                         
         JZ    NXTUNT16                                                         
         MVI   PODKCSW,C'+'        Positive                                     
         JP    NXTUNT16                                                         
         MVI   PODKCSW,C'-'        Negative                                     
                                                                                
NXTUNT16 LLC   R0,NULEN                                                         
         STCM  R0,3,UNTVPLEN       Set unit length                              
                                                                                
         LHI   R0,1                                                             
         STCM  R0,3,PODDDA#                                                     
         MVC   PODDUDA,IODA        Set unit disk address                        
         MVC   PODDULEN,NULEN      Set unit length                              
         MVC   PODDUSUB,NUKSUB     Set unit sub-line                            
         CLI   UNTPRD,0            Test unit allocated                          
         JE    *+8                                                              
         OI    PODDUFLG,PODDUFPA   Set unit pre allocated                       
                                                                                
         CLI   QPOD,YESQ           Test POD run                                 
         JNE   EXITY                                                            
                                                                                
         MVI   BYTE,0              Set buffer entry not found                   
                                                                                
         MVC   PODKPGM,NUKPROG     Set program code                             
         MVC   PODKPROG,NUPROGNM   Set program name                             
         MVC   PODKDATE,NUKDATE    Set date                                     
         MVC   PODKDPT,NUKDP       Set daypart                                  
         MVC   PODKPKG,NUPACK      Set package                                  
         TM    UNTFLG,UNTFFRZ      Test allocation frozen                       
         JZ    *+8                                                              
         MVI   PODKSTAT,PODKSFRZ   Set not available for allocation             
                                                                                
         LA    RE,UNTRRTG                                                       
         LH    RF,#DEMOS                                                        
         MHI   RF,L'XDESTRTG                                                    
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,PODKDCSM      Set demo checksum                            
                                                                                
NXTUNT18 MVC   IOKEYSAV(PODKEYL),PODKEY                                         
         GOTOR BUFFER,DMCB,('PODBUFQ',TSARDH)                                   
         JE    NXTUNT20            Test buffer entry found                      
         MVC   PODKEY(PODKEYL),IOKEYSAV                                         
         LA    R0,PODDATA                                                       
         LHI   R1,PODDATAL                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LHI   R0,1                                                             
         STCM  R0,3,PODDDA#                                                     
         MVC   PODDUDA,IODA        Set unit disk address                        
         MVC   PODDULEN,NULEN      Set unit length                              
         MVC   PODDUSUB,NUKSUB     Set unit sub-line                            
         CLI   UNTPRD,0            Test unit allocated                          
         JE    *+8                                                              
         OI    PODDUFLG,PODDUFPA   Set unit pre allocated                       
         J     NXTUNT24                                                         
                                                                                
NXTUNT20 CLC   PODDRDEM(UNTRRTGL),UNTRRTG                                       
         JE    NXTUNT22                                                         
         SR    R0,R0                                                            
         ICM   R0,3,PODKSUB                                                     
         AHI   R0,1                                                             
         STCM  R0,3,PODKSUB                                                     
         J     NXTUNT18                                                         
                                                                                
NXTUNT22 MVI   BYTE,1              Set buffer entry found                       
                                                                                
NXTUNT24 MVC   PODDRDEM(UNTRRTGL),UNTRRTG    Set raw demos                      
                                                                                
         CLI   BYTE,0              Test buffer entry found?                     
         JNE   NXTUNT26            Yes                                          
         ZAP   PODDACTC,UNTVACTC   Set actual cost                              
         ZAP   PODDASGC,UNTVASGC   Set assigned cost                            
         ZAP   PODDINTC,UNTVINTC   Set integration cost                         
         ZAP   PODDCOST,UNTVCOST   Set unit cost                                
         MVC   PODDPLEN,UNTVPLEN   Set POD length                               
         GOTOR BUFFER,DMCB,('PODBUFQ',TSAADD)                                   
         JE    NXTUNT02                                                         
         DC    H'0'                                                             
                                                                                
NXTUNT26 AP    PODDACTC,UNTVACTC   Add actual cost                              
         AP    PODDASGC,UNTVASGC   Add assigned cost                            
         CP    PODDINTC,PZERO      Test have established integration            
         JNE   *+10                                                             
         ZAP   PODDINTC,UNTVINTC   No - set integration cost                    
         AP    PODDCOST,UNTVCOST   Add unit cost                                
         SR    R0,R0                                                            
         ICM   R0,3,UNTVPLEN                                                    
         SR    R1,R1                                                            
         ICM   R1,3,PODDPLEN                                                    
         AR    R0,R1               Add unit seconds                             
         STCM  R0,3,PODDPLEN                                                    
         SR    R1,R1                                                            
         ICM   R1,3,PODDDA#        Bump number of units                         
         LA    R0,1(R1)                                                         
         CHI   R0,PODDMAXN         Test disk address array is full              
         JNH   *+6                                                              
         DC    H'0'                Disk address array is full                   
         STCM  R0,3,PODDDA#                                                     
         MHI   R1,L'PODDDAS                                                     
         LA    R1,PODDDAS(R1)      Point to new entry                           
                                                                                
E        USING PODDNTRY,R1                                                      
         MVC   E.PODDUDA,IODA      Set unit disk address                        
         MVC   E.PODDULEN,NULEN    Set unit length                              
         MVC   E.PODDUSUB,NUKSUB   Set unit sub-line                            
         CLI   UNTPRD,0            Test unit allocated                          
         JE    *+8                                                              
         OI    E.PODDUFLG,PODDUFPA Set unit pre allocated                       
         DROP  E                                                                
                                                                                
         GOTOR BUFFER,DMCB,('PODBUFQ',TSAPUT)                                   
         JE    NXTUNT02                                                         
         DC    H'0'                                                             
         DROP  R3                                                               
                                                                                
NXTUNT28 CLI   QPOD,YESQ           Test POD run                                 
         JNE   EXITY                                                            
                                                                                
         OI    NXTUFLAG,NXTUFEOF   Set end of data reached                      
         MVI   LP_RMODE,LP_RNEXT   Reset mode                                   
                                                                                
NXTUNT30 XC    PODKEY(PODKEYL),PODKEY                                           
         GOTOR BUFFER,DMCB,('PODBUFQ',TSARDH)                                   
         TM    BUFFRET,TSEEOF      Test any records found                       
         JNZ   NXTUNT36            No                                           
         OI    NXTUFLAG,NXTUFBUF   Set buffer read mode                         
         J     NXTUNT34                                                         
                                                                                
NXTUNT32 GOTOR BUFFER,DMCB,('PODBUFQ',TSANXT)                                   
         JNE   NXTUNT36                                                         
                                                                                
NXTUNT34 MVC   IODAOVER,PODDDAS    Read prototype unit record                   
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOUNTFIL+B#UNTREC'                     
         JE    *+6                                                              
         DC    H'0'                                                             
         ZAP   UNTVACTC,PODDACTC                                                
         ZAP   UNTVASGC,PODDASGC                                                
         ZAP   UNTVINTC,PODDINTC                                                
         ZAP   UNTVCOST,PODDCOST                                                
         MVC   UNTVPLEN,PODDPLEN                                                
         MVC   UNTRRTG(UNTRRTGL),PODDRDEM                                       
         MVC   LP_ADATA,AUNTREC                                                 
         J     EXITY                                                            
                                                                                
NXTUNT36 TM    NXTUFLAG,NXTUFEOF   Test end of data reached                     
         JNZ   NOMORE                                                           
         XC    LASTNET,LASTNET     Set first for new network                    
         MVI   NXTUFLAG,NXTUFUNT   Set unit read mode                           
         MVC   IODAOVER,UNTDA      Re-read unit record                          
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOUNTFIL+B#UNTREC'                     
         JE    NXTUNT04                                                         
         DC    H'0'                                                             
                                                                                
ARYUNT01 LKOUT A,(D,B#UNTREC,NUDATA),ROWID=(NUMAINEL,NUMAINEQ),        +        
               ROWWIDTH=(V,NUMAINLN),EOT=0                                      
Network  LKOUT C,02,(D,B#UNTREC,NUKNET),CHAR,ND=Y                               
Estimate LKOUT C,03,(D,B#UNTREC,NUKEST),LBIN,ND=Y                               
PRout    LKOUT P,(B#UNTREC,NUKDATE),EDTUWK   Edit days into WORK                
WeekNo   LKOUT C,04,(D,B#WORKD,WORK),LBIN,LEN=1,ND=Y                            
PRout    LKOUT P,(B#UNTREC,NUDAY),EDTDAY     Edit day into BYTE                 
Day      LKOUT C,05,(D,B#WORKD,BYTE),LBIN,ND=Y                                  
PRout    LKOUT P,NUMAINEL,BLDUTIM    Edit times into HALF1/HALF2                
MinStart LKOUT C,06,(D,B#WORKD,HALF1),LBIN                                      
MinEnd   LKOUT C,07,(D,B#WORKD,HALF2),LBIN                                      
Array    LKOUT C,255,(A,ARYUNT02)                                               
DptCode  LKOUT C,09,(D,B#UNTREC,NUKDP),LBIN,ND=Y                                
ProgName LKOUT C,10,NUPROGNM,CHAR,ND=Y                                          
SecLen   LKOUT C,11,(D,B#SAVED,UNTVPLEN),LBIN,ND=Y                              
Cost     LKOUT C,12,(D,B#SAVED,UNTVCOST),SPAK,ND=Y                              
PrdCd    LKOUT C,13,(D,B#SAVED,UNTPRD),CHAR,ND=Y                                
PrdShare LKOUT C,14,(D,B#SAVED,UNTPRDSH),LBIN,ND=Y                              
PigCd    LKOUT C,15,(D,B#SAVED,UNTPIG),CHAR,ND=Y                                
Array    LKOUT C,255,(A,ARYDSKAD)                                               
Array    LKOUT C,255,(A,ARYUDEM)                                                
ActCost  LKOUT C,18,(D,B#SAVED,UNTVACTC),SPAK,ND=Y                              
AsgCost  LKOUT C,19,(D,B#SAVED,UNTVASGC),SPAK,ND=Y                              
IntCost  LKOUT C,20,(D,B#SAVED,UNTVINTC),SPAK,ND=Y                              
Allocate LKOUT C,21,(D,B#SAVED,UNTALLO),CHAR,ND=Y                               
Progcode LKOUT C,22,(D,B#UNTREC,NUKPROG),CHAR,ND=Y                              
         LKOUT E                                                                
                                                                                
ARYUNT02 LKOUT A,(D,B#UNTREC,NUDATA),ROWID=(NUSDREL,NUSDRELQ),         +        
               ROWWIDTH=(V,NUSDRLEN),EOT=0                                      
StaType  LKOUT C,01,NUSTATYP,CHAR,FILTROUT=SETSTTY                              
PRout    LKOUT P,NUSDREL,BLDROT      Edit rotation into DUB                     
Rotation LKOUT C,08,(D,B#WORKD,DUB),CHAR,ND=Y,LEN=UNTNDAYS                      
         LKOUT E                                                                
                                                                                
ARYUDEM  LKOUT A,(D,B#SAVED,UNTRRTG),ROWWIDTH=L'XDESTRTG,              +        
               NROWS=(B#SAVED,#DEMOS)                                           
Demos    LKOUT C,17,(D,,UNTRRTG),UBIN                                           
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
ARYDSKAD LKOUT A,(D,B#SAVED,PODDDAS),ROWWIDTH=L'PODDDAS,               +        
               NROWS=(B#SAVED,PODDDA#)                                          
DskAdd   LKOUT C,16,(D,,PODDDAS),HEXD,LEN=L'PODDUDA                             
         LKOUT E                                                                
                                                                                
         USING NUSDREL,R1                                                       
SETSTTY  L     R1,LP_AINP                                                       
         CLI   NUSTATYP,C'C'       Test cable                                   
         JE    *+8                                                              
         MVI   NUSTATYP,C'N'                                                    
         CR    RE,RE                                                            
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Unit record filter                                                  *         
***********************************************************************         
                                                                                
FLTUNT00 L     R3,AUNTREC                                                       
         USING NURECD,R3                                                        
         L     R4,ANETIOD                                                       
         USING NETIOD,R4                                                        
                                                                                
         LA    R3,NUDATA                                                        
         OC    QUDSKADD,QUDSKADD   Test filter unit                             
         JZ    *+14                                                             
         CLC   QUDSKADD,IODA       Test requested unit                          
         JNE   EXITN                                                            
                                                                                
         XC    UNTVALS(UNTVALL),UNTVALS                                         
         MVC   UNTDA,IODA                                                       
         MVC   UNTKEY,IOKEY                                                     
         MVI   UNTALLO,YESQ        Set unit reallocatable                       
                                                                                
FLTUNT02 CLI   0(R3),0             Test end of unit record                      
         JE    FLTUNT06                                                         
                                                                                
         USING NUMAINEL,R3                                                      
         CLI   NUMAINEL,NUMAINEQ                                                
         JNE   *+12                                                             
         ST    R3,UNTEL01                                                       
         J     FLTUNT04                                                         
                                                                                
         USING NUSDRD,R3                                                        
         CLI   NUSDREL,NUSDRELQ                                                 
         JNE   *+12                                                             
         ST    R3,UNTEL02                                                       
         J     FLTUNT04                                                         
                                                                                
         USING NUPRDD,R3                                                        
         CLI   NUPRDEL,NUPRDELQ    Test old style product element               
         JNE   *+12                                                             
         ST    R3,UNTEL14                                                       
         J     FLTUNT04                                                         
                                                                                
         USING NUPDED,R3                                                        
         CLI   NUPDEEL,NUPDEELQ    Test new style product element               
         JNE   *+12                                                             
         ST    R3,UNTEL19                                                       
         J     FLTUNT04                                                         
                                                                                
         USING NUBILD,R3                                                        
         CLI   NUBILEL,NUBILELQ    Test billed element                          
         JNE   *+12                                                             
         MVI   UNTALLO,NOQ         Set unit unallocatable                       
         J     FLTUNT04                                                         
                                                                                
         USING NUPAYD,R3                                                        
         CLI   NUPAYEL,NUPAYELQ    Test paid element                            
         JNE   *+12                                                             
         MVI   UNTALLO,NOQ         Set unit unallocatable                       
         J     FLTUNT04                                                         
                                                                                
FLTUNT04 LLC   R0,1(R3)            Bump to next element in record               
         AR    R3,R0                                                            
         J     FLTUNT02                                                         
                                                                                
FLTUNT06 MVI   NBFILE,NBFILUNT                                                  
         MVI   NBSURVEY,0                                                       
                                                                                
         L     R3,AUNTREC                                                       
         USING NURECD,R3                                                        
                                                                                
         XC    IOKEY,IOKEY                                                      
         LA    RF,IOKEY                                                         
         USING SLSRECD,RF                                                       
         MVI   SLSKTYP,SLSKTYPQ                                                 
         MVI   SLSKSUB,SLSKSUBQ                                                 
         MVC   SLSKAGMD,NUKAM                                                   
         MVC   SLSKSTA(L'NUKNET),NUKNET                                         
         MVI   SLSKSTA+4,C'N'                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOSPTDIR+B#NTIREC'                      
         CLC   IOKEY(L'SLSKEY),IOKEYSAV                                         
         JNE   FLTUNT08                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOSPTFIL+B#NTIREC'                     
         JNL   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RF,ANTIREC                                                       
         MVC   UNTNTI,SLSNTI       Set NTI station                              
         DROP  RF                                                               
                                                                                
FLTUNT08 XC    IOKEY,IOKEY         Restore unit sequence                        
         MVC   IOKEY(L'NUKEY),UNTKEY                                            
         MVC   IODA,UNTDA                                                       
         ST    R3,IOADDR                                                        
                                                                                
         ICM   R3,15,UNTEL01                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R3                                                      
                                                                                
         TM    NUPACKST,NUPACKLO   Test locked unit                             
         JNZ   EXITN                                                            
                                                                                
         OC    NUAFFTIM,NUAFFTIM   Test affid time seeded                       
         JZ    *+12                                                             
         OI    UNTFLG,UNTAFTM      Set unit has affid time                      
         MVI   UNTALLO,NOQ         Set unit unallocatable                       
                                                                                
         TM    NUUNITST,NUUNIMSD   Test missed                                  
         JNZ   EXITN                                                            
         TM    NUUNITST,NUUNIPRE   Test preempt                                 
         JZ    *+16                                                             
         CLI   NULEN,0                                                          
         JE    FLTUNT10                                                         
         J     EXITN                                                            
                                                                                
         TM    NUUNST2,NUST2PFR+NUST2LFR  Test prd/len allocation frzn          
         JZ    *+12                                                             
         OI    UNTFLG,UNTFFRZ      Set unit prd/len allocation frozen           
         MVI   UNTALLO,NOQ         Set unit unallocatable                       
                                                                                
         CLI   QZEROCST,YESQ       Test include zero cost                       
         JE    FLTUNT10                                                         
         OC    NUACTUAL,NUACTUAL                                                
         JNZ   FLTUNT10                                                         
         TM    NUUNITST,NUUNIACI   Test $0 unit                                 
         JNZ   EXITN                                                            
                                                                                
FLTUNT10 MVC   UNTPRDSH,NUP1SHR    Set product share                            
                                                                                
         ICM   R3,15,UNTEL02                                                    
         JZ    FLTUNT18                                                         
         USING NUSDRD,R3                                                        
                                                                                
         OC    NUSDAFDT,NUSDAFDT   Test affid date seeded                       
         JZ    FLTUNT12                                                         
         TM    UNTFLG,UNTAFTM      Test affid time                              
         JZ    FLTUNT12                                                         
         MVI   UNTALLO,NOQ         Set unit unallocatable                       
                                                                                
FLTUNT12 CLI   NUSDSBMD,C' '       Test all media                               
         JE    FLTUNT16                                                         
         LA    RF,GOLSMEDS                                                      
         LA    R0,GOLSMED#                                                      
FLTUNT14 CLI   0(RF),MEDTMALL      Test goal for all media in table             
         JE    FLTUNT16                                                         
         CLC   NUSTATYP,0(RF)      Test match on goal media                     
         JE    FLTUNT16                                                         
         AHI   RF,L'NUSDSBMD                                                    
         JCT   R0,FLTUNT14                                                      
         J     EXITN                                                            
                                                                                
FLTUNT16 MVC   UNTPOSTY,NUPOSTYP   Set post type                                
         MVC   NBSURVEY,NUPOSTYP   Set survey type                              
         CLI   NUBKTYP,C' '                                                     
         JNH   FLTUNT18                                                         
         MVC   NBSURVEY,NUBKTYP    Set survey type = book type                  
                                                                                
FLTUNT18 ICM   R3,15,UNTEL19       Test allocated                               
         JZ    FLTUNT22                                                         
         USING NUPDED,R3                                                        
         TM    NUPDEIND,NUPDTRI    Test triback                                 
         JNZ   EXITN                                                            
         CLI   NUPDELEN,NUPDEDL    Test one product in element                  
         JE    FLTUNT20                                                         
         OC    UNTPRDSH,UNTPRDSH   Test product 1 share is set                  
         JNZ   FLTUNT20                                                         
         LHI   R0,5000                                                          
         STCM  R0,3,UNTPRDSH       No - set to 50%                              
FLTUNT20 LLC   R0,NUPDELEN                                                      
         SHI   R0,NUPDEPR-NUPDED   R0=Length of product entries                 
         LA    R1,NUPDEPR                                                       
E        USING NUPDEPR,R1          R1=A(Product entry)                          
         MVC   UNTPRD,E.NUPDEPR    Set product code                             
         CLI   NUPDELEN,NUPDEDL    Test one product in element                  
         JE    FLTUNT32                                                         
         AHI   R1,NUPDTLEN         Bump to next product in element              
         MVC   UNTPIG,E.NUPDEPR    Set piggy                                    
         J     FLTUNT32                                                         
         DROP  E                                                                
                                                                                
FLTUNT22 ICM   R3,15,UNTEL14       Test allocated                               
         JZ    FLTUNT26                                                         
         USING NUPRDD,R3                                                        
         TM    NUPRDIND,NUPRTRI    Test triback                                 
         JNZ   EXITN                                                            
         CLI   NUPRDLEN,NUPRDDL    Test one product in element                  
         JE    FLTUNT24                                                         
         OC    UNTPRDSH,UNTPRDSH   Test product 1 share is set                  
         JNZ   FLTUNT24                                                         
         LHI   R0,5000                                                          
         STCM  R0,3,UNTPRDSH       No - set to 50%                              
FLTUNT24 LLC   R0,NUPRDLEN                                                      
         SHI   R0,NUPRDPR-NUPRDD   R0=Length of product entries                 
         LA    R2,NUPRDPR                                                       
E        USING NUPRDPR,R2          R3=A(Product entry)                          
         GOTOR GETPRC,E.NUPRDPR    Look-up product code                         
         MVC   UNTPRD,FULL         Set product code                             
         CLI   NUPRDLEN,NUPRDDL    Test one product in element                  
         JE    FLTUNT32                                                         
         AHI   R2,NUPRTLEN         Bump to next product in element              
         GOTOR GETPRC,E.NUPRDPR    Look-up product code                         
         MVC   UNTPIG,FULL         Set piggy code                               
         J     FLTUNT32                                                         
         DROP  E                                                                
                                                                                
FLTUNT26 ICM   R3,15,UNTEL01                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R3                                                      
         CLI   NUPRD,0             Test allocated                               
         JE    FLTUNT32                                                         
         CLI   NUPRD2,0            Test second product allocated                
         JE    FLTUNT28                                                         
         CLC   NUPRD,NUPRD2        or first=second product                      
         JE    FLTUNT28                                                         
         TM    NUUNST2,NUST2ZAL    Test product 1 zero allocation               
         JNZ   FLTUNT30                                                         
         GOTOR GETPRC,NUPRD        Build entry for product 1                    
         MVC   UNTPRD,FULL                                                      
         MVC   UNTPRDSH,NUP1SHR                                                 
         LHI   R0,5000                                                          
         OC    UNTPRDSH,UNTPRDSH                                                
         JNZ   *+8                                                              
         STCM  R0,3,UNTPRDSH       Set 50% if none specified                    
         GOTOR GETPRC,NUPRD2                                                    
         MVC   UNTPIG,FULL                                                      
         J     FLTUNT32                                                         
                                                                                
FLTUNT28 GOTOR GETPRC,NUPRD        Build single entry for product 1             
         MVC   UNTPRD,FULL                                                      
         J     FLTUNT32                                                         
                                                                                
FLTUNT30 GOTOR GETPRC,NUPRD2       Build single entry for product 2             
         MVC   UNTPIG,FULL                                                      
         J     FLTUNT32                                                         
                                                                                
FLTUNT32 OC    QMIDFLT,QMIDFLT     Test mid flight restriction                  
         JZ    FLTUNT34                                                         
         L     R3,AUNTREC                                                       
         USING NURECD,R3                                                        
         CLC   NUKDATE,QMIDFLT     Test after mid flight restriction            
         JH    FLTUNT34                                                         
         OC    UNTPRD,UNTPRD       Test unit allocated                          
         JZ    EXITN                                                            
         J     EXITY                                                            
                                                                                
FLTUNT34 CLI   UNTALLO,NOQ         Test unit unallocatable                      
         JE    EXITY                                                            
         CLI   QRETAIN,YESQ        Test retain future allocations               
         JE    EXITY                                                            
         XC    UNTPRDS(UNTPRDSL),UNTPRDS  No - Clear allocations                
         J     EXITY                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* Look-up product code in table - on entry R1=A(Product number)       *         
*                                 on exit FULL=Product code           *         
***********************************************************************         
                                                                                
GETPRC   LAY   RF,PRDLST                                                        
         USING PRDLST,RF           RF=A(product list)                           
GETPRC02 OC    PRDLPRD,PRDLPRD     Test end of list                             
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   PRDLNUM,0(R1)       Match on product code                        
         JE    *+12                                                             
         AHI   RF,PRDLSTL                                                       
         J     GETPRC02                                                         
         MVC   FULL(L'PRDLPRD),PRDLPRD  Set product code from table             
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* Edit unit week number into WORK, LP_AINP points to unit date in key *         
***********************************************************************         
                                                                                
EDTUWK   L     R1,LP_AINP                                                       
                                                                                
         LA    RE,WEEKLST          Point to week list                           
         LHI   R0,1                R0=week number                               
EDTUWK02 CLC   0(L'NUKDATE,R1),2(RE)  Test within current week                  
         JNH   EDTUWK04            Yes                                          
         AHI   RE,4                Point to next week                           
         AHI   R0,1                                                             
         J     EDTUWK02                                                         
                                                                                
EDTUWK04 STC   R0,WORK             Set week number                              
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit unit day number into BYTE, LP_AINP points to unit day          *         
***********************************************************************         
                                                                                
EDTDAY   L     R1,LP_AINP                                                       
                                                                                
         LAY   RE,DAYTAB           Point to day conversion table                
         LHI   R0,7                R0=number of entries in table                
         MVC   BYTE,1(RE)          Init to Monday                               
EDTDAY02 CLC   0(L'NUDAY,R1),0(RE) Test match on day                            
         JE    EDTDAY04            Yes                                          
         AHI   RE,2                Point to day                                 
         JCT   R0,EDTDAY02                                                      
                                                                                
EDTDAY04 MVC   BYTE,1(RE)          Set day number                               
         J     EXITY                                                            
                                                                                
DAYTAB   DC    X'4001'             Monday                                       
         DC    X'2002'             Tuesday                                      
         DC    X'1003'             Wednesday                                    
         DC    X'0804'             Thursday                                     
         DC    X'0405'             Friday                                       
         DC    X'0206'             Saturday                                     
         DC    X'0107'             Sunday                                       
                                                                                
***********************************************************************         
* Edit unit start/end relative minutes into HALF1/HALF2               *         
* - LP_AINP points to unit start time                                 *         
***********************************************************************         
                                                                                
         USING NUMAINEL,R1                                                      
BLDUTIM  L     R1,LP_AINP                                                       
         SR    RF,RF                                                            
         ICM   RF,3,NUTIMEST       Start time                                   
         SR    RE,RE                                                            
         CHI   RF,2400             Test midnight                                
         JE    BLDUT02                                                          
         LHI   R0,100                                                           
         DR    RE,R0                                                            
         MHI   RF,60                                                            
         AR    RE,RF                                                            
BLDUT02  STH   RE,HALF1            Set start relative minute                    
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,NUTIMEEN       End time                                     
         CLC   NUTIMEST,NUTIMEEN   Test end time < start time                   
         JNH   *+8                                                              
         AHI   RF,24*100                                                        
         SR    RE,RE                                                            
         LHI   R0,100                                                           
         DR    RE,R0                                                            
         MHI   RF,60                                                            
         AR    RE,RF                                                            
         STH   RE,HALF2            Set end relative minute                      
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit unit rotation into DUB - LP_AINP points to data element        *         
***********************************************************************         
                                                                                
BLDROT   L     R1,LP_AINP                                                       
         USING NUSDRD,R1                                                        
         LA    RE,DUB                                                           
         LHI   R0,UNTNDAYS                                                      
         LLC   RF,NUSDROT                                                       
         SLL   RF,24                                                            
         L     R2,AUNTREC                                                       
         USING NURECD,R2                                                        
         CLC   NUSDROT,NUDAY                                                    
         JNE   BLDROT02                                                         
         SR    RF,RF                                                            
BLDROT02 SLL   RF,1                Build day Y/N mask                           
         MVI   0(RE),C'N'                                                       
         TMH   RF,X'8000'                                                       
         JZ    *+8                                                              
         MVI   0(RE),C'Y'                                                       
         AHI   RE,1                                                             
         JCT   R0,BLDROT02                                                      
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Exclusion records                                                   *         
***********************************************************************         
                                                                                
ARYPXC   LKOUT A,(R,NXTPXC),ROWNAME=PXCREC,MULTIROW=Y                           
Array    LKOUT C,255,(A,ARYPXC1)                                                
Array    LKOUT C,255,(A,ARYPXC2)                                                
         LKOUT E                                                                
                                                                                
NXTPXC   GOTOR (#NXTREC,ANXTREC),DMCB,PXCKEYT,('B#PXCREC',0),          +        
               ('$NXTRSPT',SAVED),0,0                                           
         J     EXITY                                                            
                                                                                
ARYPXC1  LKOUT A,(D,B#PXCREC,PXCEL01),ROWID=(L07ELID,L07ELIDQ),        +        
               ROWWIDTH=(V,L07ELLN),EOT=0                                       
Array    LKOUT C,X'0006',(A,ARYXNET),FILTROUT=TSTXNET                           
Array    LKOUT C,X'0006',(A,ARYXPRG),FILTROUT=TSTXPRG                           
Array    LKOUT C,X'0007',(A,ARYXDTM),FILTROUT=TSTXDTM                           
         LKOUT E                                                                
                                                                                
ARYPXC2  LKOUT A,(D,B#PXCREC,PXCEL01),ROWID=(L06ELID,L06ELIDQ),        +        
               ROWWIDTH=(V,L06ELLN),EOT=0                                       
Array    LKOUT C,X'0009',(A,ARYXNET),FILTROUT=TSTXNET                           
         LKOUT E                                                                
                                                                                
         USING L07ELD,R1                                                        
TSTXPRG  GOTOR TSTPXCD             Test within period                           
         TM    L07STAT,L07STPRG    Test program                                 
         JO    EXITY                                                            
         J     EXITN                                                            
                                                                                
TSTXDTM  GOTOR TSTPXCD             Test within period                           
         TM    L07STAT,L07STDAY    Test day                                     
         JO    EXITY                                                            
         TM    L07STAT,L07STTIM    Test time                                    
         JO    EXITY                                                            
         TM    L07STAT,L07STDTM    Test day/time                                
         JO    EXITY                                                            
         J     EXITN                                                            
                                                                                
TSTXNET  GOTOR TSTPXCD             Test within period                           
         TM    L07STAT,L07STNET    Test network                                 
         JO    EXITY                                                            
         J     EXITN                                                            
                                                                                
TSTPXCD  L     R1,LP_AINP          Point to element                             
                                                                                
         OC    L07STDTE,L07STDTE   Test any date range                          
         JNZ   TSTPXC02                                                         
         MVC   L07STDTE,DSTRDTC    Set estimate start date                      
         MVC   L07ENDTE,DENDDTC    Set estimate end date                        
                                                                                
TSTPXC02 CLC   L07STDTE,DENDDTC    Test overlaps request dates                  
         JH    EXITN                                                            
         CLC   L07ENDTE,DSTRDTC                                                 
         JL    EXITN                                                            
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
ARYXPRG  LKOUT A,(*,L07ELID),ROWNAME=L07ELID,ROWWIDTH=L07ELL,NEWEL=Y            
Network  LKOUT C,01,(D,B#PXCREC,PXCKSTA),(R,EDTSTA),ND=Y                        
Product  LKOUT C,02,(D,B#PXCREC,PXCKPRD),CHAR,ND=Y                              
Estimate LKOUT C,03,(D,B#PXCREC,PXCKEST),LBIN,ND=Y                              
PRout    LKOUT P,L07ELID,BLDWKS    Edit weeks into WORK                         
WeekNo   LKOUT C,255,(A,ARYXWKS)                                                
Net/Prog LKOUT C,05,L07LIM,CHAR,ND=Y                                            
         LKOUT E                                                                
                                                                                
ARYXDTM  LKOUT A,(*,L07ELID),ROWNAME=L07ELID,ROWWIDTH=L07ELL,NEWEL=Y            
Network  LKOUT C,01,(D,B#PXCREC,PXCKSTA),(R,EDTSTA),ND=Y                        
Product  LKOUT C,02,(D,B#PXCREC,PXCKPRD),CHAR,ND=Y                              
Estimate LKOUT C,03,(D,B#PXCREC,PXCKEST),LBIN,ND=Y                              
PRout    LKOUT P,L07ELID,BLDWKS    Edit weeks into WORK                         
WeekNo   LKOUT C,255,(A,ARYXWKS)                                                
PRout    LKOUT P,L07ELID,BLDDAY    Edit days into DUB                           
Days     LKOUT C,05,(D,B#WORKD,DUB),CHAR,ND=Y,LEN=LIMNDAYS                      
PRout    LKOUT P,L07ELID,BLDTIM    Edit times into HALF1/HALF2                  
MinStart LKOUT C,06,(D,B#WORKD,HALF1),LBIN                                      
MinEnd   LKOUT C,07,(D,B#WORKD,HALF2),LBIN                                      
         LKOUT E                                                                
                                                                                
ARYXNET  LKOUT A,(*,L07ELID),ROWNAME=L07ELID,ROWWIDTH=L07ELL,NEWEL=Y            
Network  LKOUT C,01,L07LIM,CHAR,ND=Y                                            
Product  LKOUT C,02,(D,B#PXCREC,PXCKPRD),CHAR,ND=Y                              
Estimate LKOUT C,03,(D,B#PXCREC,PXCKEST),LBIN,ND=Y                              
PRout    LKOUT P,L07ELID,BLDWKS    Edit weeks into WORK                         
WeekNo   LKOUT C,255,(A,ARYXWKS)                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Edit packed station code                                            *         
***********************************************************************         
                                                                                
EDTSTA   L     R1,LP_AINP                                                       
         OC    0(L'STAPSTA,R1),0(R1)                                            
         JZ    XCOLEN                                                           
         CLC   =X'FFFFFF',0(R1)                                                 
         JNE   EDTSTA02                                                         
         L     R1,LP_AOUT                                                       
         MVC   0(L'STAPQSTA,R1),=C'ALL  '                                       
         J     EDTSTA04                                                         
                                                                                
EDTSTA02 XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVC   STAPMED,QMEDA                                                    
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPSTA,0(R1)                                                    
         GOTOR VSTAPACK,STAPACKD                                                
         L     R1,LP_AOUT                                                       
         MVC   0(L'STAPQSTA,R1),STAPQSTA                                        
EDTSTA04 LHI   R0,L'STAPQSTA-1                                                  
         STCM  R0,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
ARYXWKS  LKOUT A,(D,B#WORKD,WORK),ROWWIDTH=1,EOT=0                              
WeekNo   LKOUT C,04,WORK,LBIN,LEN=1                                             
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Edit exclusion days into DUB - LP_AINP points to exclusion element  *         
***********************************************************************         
                                                                                
BLDDAY   L     R1,LP_AINP                                                       
         USING L07ELD,R1                                                        
         LA    RE,DUB                                                           
         LHI   R0,LIMNDAYS                                                      
         OILH  GRF,X'7F00'         Set all day bits on                          
         TM    L07STAT,L07STDTM    Test day/time                                
         JO    BLDDAY02                                                         
         TM    L07STAT,L07STDAY    Test day                                     
         JZ    BLDDAY04                                                         
BLDDAY02 LLC   RF,L07LIM                                                        
         SLL   RF,24                                                            
BLDDAY04 SLL   RF,1                Build day Y/N mask                           
         MVI   0(RE),C'N'                                                       
         TMH   RF,X'8000'                                                       
         JZ    *+8                                                              
         MVI   0(RE),C'Y'                                                       
         AHI   RE,1                                                             
         JCT   R0,BLDDAY04                                                      
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit week numbers into WORK - LP_AINP points to exclusion element   *         
***********************************************************************         
                                                                                
BLDWKS   L     R1,LP_AINP                                                       
                                                                                
         LA    RE,WEEKLST          Point to week list                           
         LA    RF,WORK             Point to exclusion's week list               
         LHI   R0,1                R0=week number                               
BLDWKS02 CLC   L07STDTE,2(RE)      Test within current week                     
         JNH   BLDWKS04            Yes                                          
         AHI   RE,4                Point to next week                           
         AHI   R0,1                                                             
         J     BLDWKS02                                                         
                                                                                
BLDWKS04 STC   R0,0(RF)            Set first week number                        
         AHI   RF,1                Bump week                                    
         MVI   0(RF),0             Set new end of list                          
         AHI   R0,1                                                             
         CLC   0(2,RE),DENDDTC     Test current week past end date              
         JH    EXITY                                                            
         CLC   L07ENDTE,2(RE)      Test ends in current week                    
         JNH   EXITY                                                            
         AHI   RE,L'WEEKLST        Bump to next week                            
         J     BLDWKS04                                                         
                                                                                
***********************************************************************         
* Edit exclusion start/end relative minutes into HALF1/HALF2          *         
* - LP_AINP points to exclusion element                               *         
***********************************************************************         
                                                                                
BLDTIM   L     R1,LP_AINP                                                       
         TM    L07STAT,L07STDTM    Test day/time                                
         JZ    BLDTIM02                                                         
         SR    RF,RF                                                            
         ICM   RF,3,L07LIM+1       Start time                                   
         J     BLDTIM04                                                         
                                                                                
BLDTIM02 TM    L07STAT,L07STTIM    Test time                                    
         JZ    BLDTIM10                                                         
         SR    RF,RF                                                            
         ICM   RF,3,L07LIM         Start time                                   
BLDTIM04 SR    RE,RE                                                            
         LHI   R0,100                                                           
         DR    RE,R0                                                            
         MHI   RF,60                                                            
         AR    RE,RF                                                            
         STH   RE,HALF1            Set start relative minute                    
                                                                                
         SR    RF,RF                                                            
         TM    L07STAT,L07STDTM    Test day/time                                
         JZ    BLDTIM06                                                         
         ICM   RF,3,L07LIM+3       Start time                                   
         CLC   L07LIM+3(2),L07LIM+1  Test end time < start time                 
         J     BLDTIM08                                                         
                                                                                
BLDTIM06 ICM   RF,3,L07LIM+2       End time                                     
         CLC   L07LIM+2(2),L07LIM  Test end time < start time                   
BLDTIM08 JNL   *+8                                                              
         AHI   RF,24*100                                                        
         SR    RE,RE                                                            
         LHI   R0,100                                                           
         DR    RE,R0                                                            
         MHI   RF,60                                                            
         AR    RE,RF                                                            
         STH   RE,HALF2            Set end relative minute                      
         J     EXITY                                                            
                                                                                
BLDTIM10 XC    HALF1,HALF1                                                      
         MVC   HALF2,=AL2(30*60)                                                
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
REQEND   LKREQ X                                                                
         EJECT                                                                  
***********************************************************************         
* Interface to TSAR to manage large arrays                            *         
*                                                                     *         
* Ntry:- R1 points to a parameter list as follows:-                   *         
*                                                                     *         
*        P1/0   - Buffer number                                       *         
*        P1/1-3 - TSAR action code                                    *         
*        P2/0   - Key length (initialization call)                    *         
*        P2/2-3 - Record length (initialization call)                 *         
*        - or -                                                       *         
*        P2     - Optional address of record (buy buffer)             *         
***********************************************************************         
                                                                                
BUFFER   NTR1  LABEL=*                                                          
         LR    R2,R1               R2=A(Parameter list)                         
                                                                                
         LARL  R3,BUFFTAB                                                       
         USING BUFFTABD,R3         Locate buffer definition                     
         LHI   R0,BUFFTABN                                                      
BUFFER02 CLC   BUFFTBUF,0(R2)      Match on buffer number                       
         JE    BUFFER04                                                         
         AHI   R3,BUFFTABL                                                      
         JCT   R0,BUFFER02                                                      
         DC    H'0'                                                             
                                                                                
BUFFER04 SR    R1,R1               Get displacement to record                   
         ICM   R1,3,BUFFTREC                                                    
         JZ    *+12                                                             
         LA    RE,SAVED(R1)                                                     
         J     *+8                                                              
         L     RE,4(R2)            Use caller's area if not defined             
                                                                                
         ICM   R1,3,BUFFTBLK       Point to block                               
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R4,SAVED(R1)                                                     
                                                                                
         USING TSARD,R4            R4=A(TSAR control block)                     
         MVC   TSACTN,3(R2)        Set action code                              
         ST    RE,TSAREC           Set A(record)                                
         CLI   TSACTN,TSAINI       Test initialization call                     
         JNE   BUFFER06                                                         
         XC    TSARD(TSPNEWL),TSARD                                             
         MVI   TSACTN,TSAINI                                                    
         MVC   TSACOM,LP_ACOM                                                   
         MVC   TSKEYL,4(R2)        Set key length                               
         MVC   TSRECL,6(R2)        Set record size                              
         MVI   TSINDS,TSINODSK                                                  
         MVI   TSIND2,TSI2BIGN+TSI2MANY                                         
         MVC   TSRECI,BUFFTIND     Set TSAR buffer flag                         
         MVC   TSBUFFL,BUFFTSIZ    Set buffer size (nK)                         
                                                                                
BUFFER06 GOTOR LP_ATSAR,TSARD      Call TSAR to process action                  
         MVC   BUFFRET,TSERRS                                                   
         TM    BUFFRET,TSEINIF     Test initialization failure                  
         JZ    *+6                                                              
         DC    H'0'                Yes - take a hit                             
         CLI   BUFFRET,0           Set condition code                           
         J     EXIT                                                             
         DROP  R3,R4                                                            
                                                                                
         DS    0H                                                               
BUFFTAB  DS    0XL(BUFFTABL)       ** Buffer table **                           
                                                                                
ESTBUFQ  EQU   1                   Estimate buffer number                       
         DC    AL1(ESTBUFQ),AL2(TSARBLK1-SAVED,ESTVALS-SAVED)                   
         DC    AL1(TSRTSAB1+TSRXTN),AL2(ONEK)                                   
                                                                                
PODBUFQ  EQU   2                   POD buffer                                   
         DC    AL1(PODBUFQ),AL2(TSARBLK2-SAVED,PODVALS-SAVED)                   
         DC    AL1(TSRMINB1+TSRXTN),AL2(ONEK*8)                                 
                                                                                
BUFFTABN EQU   (*-BUFFTAB)/L'BUFFTAB                                            
                                                                                
BUFFTABD DSECT ,                   ** Buffer table layout **                    
BUFFTBUF DS    X                   Buffer number                                
BUFFTBLK DS    AL2                 Displacement to TSAR block                   
BUFFTREC DS    AL2                 Displacement to record                       
BUFFTIND DS    X                   TSAR indicator byte                          
BUFFTSIZ DS    AL2                 Buffer size when running off-line            
BUFFTABL EQU   *-BUFFTABD          Length of table entry                        
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
MORE     MVI   LP_RMODE,LP_RMORE   Set more to come                             
         J     EXITY                                                            
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   Set no more records & exit                   
         J     EXITY                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     Clear output field length & exit             
         J     EXITY                                                            
                                                                                
EXITN    LHI   RE,0                Set condition code to not equal              
         J     EXITCC                                                           
EXITY    LHI   RE,1                Set condition code to equal                  
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                   General exit point                           
         EJECT                                                                  
***********************************************************************         
* System facilities list                                              *         
***********************************************************************         
                                                                                
FACS     DS    0XL(RFACTABL)       ** System facilities **                      
         DC    AL1(QDEMADDR),AL2(CDEMADDR-COMFACSD,0)                           
         DC    AL1(QDEMOUT),AL2(CDEMOUT-COMFACSD,0)                             
         DC    AL1(QDEMEL),AL2(CDEMEL-COMFACSD,0)                               
         DC    AL1(QDEMAINT),AL2(CDEMAINT-COMFACSD,0)                           
         DC    AL1(QDEMAND),AL2(CDEMAND-COMFACSD,0)                             
         DC    AL1(QDEMOMTH),AL2(CDEMOMTH-COMFACSD,0)                           
         DC    AL1(QDEMOVAL),AL2(CDEMOVAL-COMFACSD,0)                           
         DC    AL1(RFACEOTQ)                                                    
                                                                                
GLOBALS  DS    0D                  ** Global literals **                        
                                                                                
         LTORG ,                                                                
                                                                                
FLTUNT   J     FLTUNT00                                                         
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
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
                                                                                
BZEROES  DC    XL4'00'                                                          
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
PTEN     DC    P'10'                                                            
                                                                                
LVALUES  DS    0X                  ** Values moves to WVALUES **                
         DC    C'POL'              POLPRD                                       
                                                                                
ESPKEYT  LKKEY H,EPKEY,SAVED       ** Estimate passive driver **                
         LKKEY LIT,EPKEYTYP,EPKEYTYQ                                            
         LKKEY LIT,EPKEYSUB,EPKEYSBQ                                            
         LKKEY WMP,EPKEYAM,QAMED                                                
         LKKEY WMP,EPKEYCLT,QACLT                                               
         LKKEY RNG,EPKEYSDT,DESTSTDR                                            
         LKKEY RNG,EPKEYEDT,DESTENDR                                            
         LKKEY ALL,EPKEYEST                                                     
         LKKEY SIN,EPKEYPRD,POLPRD                                              
         LKKEY E                                                                
                                                                                
ESTKEYT  LKKEY H,EKEY              ** Estimate driver **                        
         LKKEY LIT,EKEYTYPE,EKEYTYPQ                                            
         LKKEY WMP,EKEYAM,QAMED                                                 
         LKKEY WMP,EKEYCLT,QACLT                                                
         LKKEY ALL,EKEYPRD                                                      
         LKKEY WMP,EKEYEST,DAEST                                                
         LKKEY LIT,EKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
GOLKEYT  LKKEY H,GXKEY             ** Goal driver **                            
         LKKEY LIT,GXKEYTYP,GKEYTYPQ                                            
         LKKEY WMP,GXKEYAM,QAMED                                                
         LKKEY WMP,GXKEYCLT,QACLT                                               
         LKKEY ALL,GXKEYPRD                                                     
         LKKEY LST,GXKEYMKT,DMKTNUM                                             
         LKKEY WMP,GXKEYEST,DAEST                                               
         LKKEY WMP,GXKEYDPT,QADPT                                               
         LKKEY WMP,GXKEYSLN,QASLN                                               
         LKKEY ALL,GXKEYSEC                                                     
         LKKEY LIT,GXKEYAGY,0                                                   
         LKKEY LIT,GXKPKGE,0                                                    
         LKKEY ALL,GXKPRDA                                                      
         LKKEY E                                                                
                                                                                
                                                                                
PXCKEYT  LKKEY H,PXCKEY            ** Exclusion driver **                       
         LKKEY LIT,PXCKTYP,PXCKTYPQ,1                                           
         LKKEY LIT,PXCKTYP+1,PXCKSUBQ,1                                         
         LKKEY WMP,PXCKAGM,QAMED                                                
         LKKEY WMP,PXCKCLT,QACLT                                                
         LKKEY ALL,PXCKSTA                                                      
         LKKEY NZR,PXCKPRD                                                      
         LKKEY ALL,PXCKEST                                                      
         LKKEY E                                                                
                                                                                
UNTKEYT  LKKEY H,NUKDKEY           ** Unit driver **                            
         LKKEY LIT,NUKDTYPE,X'94'                                               
         LKKEY WMP,NUKDAM,QAMED                                                 
         LKKEY WMP,NUKDCLT,QACLT                                                
         LKKEY WMP,NUKDEST,DAEST                                                
         LKKEY ALL,NUKDNET                                                      
         LKKEY ALL,NUKDDAY                                                      
         LKKEY ALL,NUKDTIME                                                     
         LKKEY ALL,NUKDPROG                                                     
         LKKEY RNG,NUKDDATE,DUNTSTDC                                            
         LKKEY ALL,NUKDSUB                                                      
         LKKEY E                                                                
         EJECT                                                                  
SAVED    DSECT ,                   ** Saved storage **                          
                                                                                
RTGEQU   DS    D                   Rating equivalence (N2 profile)              
                                                                                
WVALUES  DS    0X                  ** Values set from LVALUES **                
POLPRD   DS    CL3                 C'POL'                                       
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
AMASTC   DS    A                   A(MASTC)                                     
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER calling mode                   
AGENCY   DS    CL(L'LP_AGY)        Agency alpha id                              
AGENCYB  DS    CL(L'LP_AGYB)       Agency binary value                          
MQSVR    DS    CL(L'LP_MQSVR)      External service call                        
                                                                                
         DS    0F                                                               
TSARBLK1 DS    XL(TSPXTNL)         Estimate buffer                              
TSARBLK2 DS    XL(TSPXTNL)         POD buffer                                   
TSARBLK3 DS    XL(TSPXTNL)         n/d                                          
TSARBLK4 DS    XL(TSPXTNL)         n/d                                          
                                                                                
BUFFRET  DS    X                   BUFFER s/r return byte                       
                                                                                
NXTUFLAG DS    X                   ** NXTUNT flag byte **                       
NXTUFUNT EQU   X'00'               Unit read mode                               
NXTUFBUF EQU   X'01'               Buffer read mode                             
NXTUFEOF EQU   X'80'               All units read                               
                                                                                
QVALUES  DS    0F                  ** Request values **                         
                                                                                
QMEDNDX  DS    0XL4                Media index                                  
QMEDIND  DS    X                                                                
QAMED    DS    AL3                                                              
                                                                                
QCLTNDX  DS    0XL4                Client index                                 
QCLTIND  DS    X                                                                
QACLT    DS    AL3                                                              
                                                                                
QPRDNDX  DS    0XL4                Product index                                
QPRDIND  DS    X                                                                
QAPRD    DS    AL3                                                              
                                                                                
QNETNDX  DS    0XL4                Network index                                
QNETIND  DS    X                                                                
QANET    DS    AL3                                                              
                                                                                
QESTNDX  DS    0XL4                Estimate index                               
QESTIND  DS    X                                                                
QAEST    DS    AL3                                                              
                                                                                
QDPTNDX  DS    0XL4                Daypart index                                
QDPTIND  DS    X                                                                
QADPT    DS    AL3                                                              
                                                                                
QSLNNDX  DS    0XL4                Seconds length index                         
QSLNIND  DS    X                                                                
QASLN    DS    AL3                                                              
                                                                                
QSMDNDX  DS    0XL4                Sub-media index                              
QSMDIND  DS    X                                                                
QASMD    DS    AL3                                                              
                                                                                
QSTRDTE  DS    CL6                 Start date (ebcdic)                          
QENDDTE  DS    CL6                 End date (ebcdic)                            
                                                                                
QLIVE    DS    C                   Live run                                     
QPOD     DS    C                   POD run                                      
QALODMOD DS    AL2                 Allocation download model number             
QZEROCST DS    C                   Exclude zero cost units                      
QCOSTTY  DS    C                   Use assigned cost?                           
QINTEG   DS    C                   Include integration cost?                    
QWKNGOAL DS    C                   ** Weeks with no goals **                    
QWKNPRVQ EQU   C'P'                Use previous week's goals                    
QWKNAVGQ EQU   C'A'                Use average of all posted weeks              
QMIDFLT  DS    XL(L'NUKDATE)       Mid flight date restriction                  
QRETAIN  DS    C                   Retain future allocations?                   
QDEMFLAV DS    CL2                 Demo flavor/Apply Gaurantee                  
QDFACTQ  EQU   C'A'                Actuals                                      
                                                                                
QDEMREQ  DS    C                   Demo ratings equivalencing                   
QDREEQUQ EQU   C'E'                Equivalence                                  
QDRERAWQ EQU   C'R'                Raw value                                    
QDREPRFQ EQU   0                   Use profile value                            
                                                                                
QDEMIEQ  DS    C                   Demo impressions equivalencing               
QDIEEQUQ EQU   C'E'                Equivalence                                  
QDIERAWQ EQU   C'R'                Raw value                                    
QDIEPRFQ EQU   0                   Use profile value                            
                                                                                
QUDSKA   DS    CL8                 Unit disk address                            
QUDSKADD DS    F                   Unit disk address (hex)                      
                                                                                
QVALUEL  EQU   *-QVALUES                                                        
                                                                                
DVALUES  DS    0F                  ** Derived/built values **                   
                                                                                
DESTSTDR DS    0XL(L'EPKEYSDT*2)   ** Start date driver range **                
DESTSSTR DS    XL(L'EPKEYSDT)      X'0000'                                      
DENDDTC  DS    XL(L'EPKEYSDT)      Request estimate end date                    
DESTENDR DS    0XL(L'EPKEYEDT*2)   ** End date driver range **                  
DSTRDTC  DS    XL(L'EPKEYEDT)      Request estimate start date                  
DESTEEND DS    XL(L'EPKEYEDT)      X'FFFF'                                      
                                                                                
DSTRDTE  DS    XL6                 Request estimate start date ebcidic          
DGOLSTDC DS    XL(L'GLWEEK)        Request goal start date                      
                                                                                
DUNTSTDC DS    XL(L'NUKDATE)       Request unit start date                      
DUNTENDC DS    XL(L'NUKDATE)       Request unit end date                        
                                                                                
DESTNDX  DS    0XL4                Estimate index                               
DESTIND  DS    X                                                                
DAEST    DS    AL3                                                              
                                                                                
DMKTNUM  DS    AL2                 N'entries in MKTLST                          
DMKTLST  DS    12XL(L'GKEYMKT)     Market list                                  
                                                                                
DVALUEL  EQU   *-DVALUES                                                        
                                                                                
WEEKMAX  EQU   64                  Maximum number of weeks supported            
WEEKLST  DS    (WEEKMAX)XL4,X      List of broadcast weeks                      
                                                                                
ESTVALS  DS    0X                  ** Estimate record values **                 
ESTVPRD  DS    CL(L'EKEYPRD)       Product code                                 
ESTVEST  DS    XL(L'EKEYEST)       Estimate number                              
ESTVKEYL EQU   *-ESTVALS           TSAR key length                              
ESTVTRG1 DS    XL(L'EDEMLIST)      Target demo 1                                
ESTVTRG2 DS    XL(L'EDEMLIST)      Target demo 2                                
ESTVTRG3 DS    XL(L'EDEMLIST)      Target demo 3                                
ESTVTRGL EQU   *-ESTVTRG1                                                       
ESTVALL  EQU   *-ESTVALS           TSAR record length                           
                                                                                
GOLSMEDS DS    XL(GOLSMED#)        List of valid goal submedias                 
GOLSMED# EQU   16                  Maximum number of goal submedias             
                                                                                
PODVALS  DS    0X                  ** POD unit records **                       
                                                                                
PODKEY   DS    0X                  ** POD record key **                         
PODKPGM  DS    CL(L'NUKPROG)       Program Code                                 
PODKPROG DS    CL(L'NUPROGNM)      Program                                      
PODKDATE DS    XL(L'NUKDATE)       Air date                                     
PODKDPT  DS    XL(L'NUKDP)         Daypart                                      
PODKCSW  DS    X                   Cost type                                    
PODKPKG  DS    XL(L'NUPACK)        Package                                      
PODKDCSM DS    XL4                 Demo checksum                                
PODKSTAT DS    X                   Allocation status                            
PODKSAVA EQU   0                   Available for allocation                     
PODKSFRZ EQU   1                   Not available for allocation                 
PODKSUB  DS    XL2                 Sub-reference number                         
PODKEYL  EQU   *-PODVALS                                                        
                                                                                
PODDATA  DS    0X                  ** POD record data **                        
PODDCOST DS    PL8                 Cost                                         
PODDACTC DS    PL8                 Actual cost                                  
PODDASGC DS    PL8                 Assigned cost                                
PODDINTC DS    PL8                 Integration cost                             
PODDPLEN DS    XL2                 POD length                                   
                                                                                
PODDRDEM DS    (DEMOMAXN)XL(L'UNTRRTG)                                          
PODDRDML EQU   *-PODDRDEM          Length of demo values                        
                                                                                
PODDDA#  DS    XL2                 Number of disk addresses                     
                                                                                
PODDNTRY DS    0X                  ** POD record entry **                       
PODDUDA  DS    XL(L'NUDA)          Unit disk address                            
PODDULEN DS    X                   Unit length                                  
PODDUSUB DS    X                   Unit sub-line                                
PODDUFLG DS    X                   Unit flag                                    
PODDUFPA EQU   X'FF'               Unit pre-allocated                           
PODDLNQ  EQU   *-PODDUDA                                                        
         ORG   PODDUDA                                                          
PODDDAS  DS    (PODDMAXN)XL(PODDLNQ)                                            
PODDMAXN EQU   256                 Maximum number of units in POD               
                                                                                
PODDATAL EQU   *-PODDATA                                                        
PODVALL  EQU   *-PODVALS                                                        
                                                                                
LASTNET  DS    XL(L'NUKNET)        Last network code                            
N2PROF   DS    XL(L'NBUSER2)       N2 profile                                   
PL16     DS    PL16                                                             
                                                                                
UNTVALS  DS    0X                  ** Unit record values **                     
                                                                                
UNTEL01  DS    A                   A(NUMAINEL)                                  
UNTEL02  DS    A                   A(NUSDRD)                                    
UNTEL14  DS    A                   A(NUPRDD)                                    
UNTEL19  DS    A                   A(NUPDED)                                    
                                                                                
UNTFLG   DS    X                   Unit flags                                   
UNTFFRZ  EQU   X'01'               Unit allocation frozen                       
UNTAFTM  EQU   X'02'               Unit has an affid time                       
                                                                                
UNTALLO  DS    C                   Unit allocatable                             
                                                                                
UNTDA    DS    XL(L'IODA)          Unit disk address                            
UNTKEY   DS    XL(L'NUKEY)         Unit passive key                             
                                                                                
UNTPOSTY DS    XL(L'NUPOSTYP)      Unit post type                               
                                                                                
UNTPRDS  DS    0X                  ** Allocation values **                      
UNTPRD   DS    CL(L'NUPDEPR)       Pre-allocated product                        
UNTPRDSH DS    XL(L'NUP1SHR)       Pre-allocated product share                  
UNTPIG   DS    CL(L'NUPDEPR)       Pre-allocated piggyback product              
UNTPRDSL EQU   *-UNTPRDS                                                        
                                                                                
UNTVCOST DS    PL8                 Unit cost                                    
UNTVACTC DS    PL8                 Unit actual cost                             
UNTVASGC DS    PL8                 Unit assigned cost                           
UNTVINTC DS    PL8                 Unit integration cost                        
UNTVPLEN DS    XL2                 Unit length                                  
                                                                                
UNTNTI   DS    CL(L'SLSNTI)        Unit NTI station                             
                                                                                
UNTVALL  EQU   *-UNTVALS                                                        
                                                                                
UNTNDAYS EQU   7                   Number of day bits in NUSDROT                
                                                                                
MAXUPRD  EQU   6                   Maximum number of products on unit           
                                                                                
#DEMOS   DS    H                   N'target demos                               
DEMOMAXN EQU   64                  Maximum N'target demos supported             
DEMOLIST DS    (DEMOMAXN)XL(L'EDEMLIST)                                         
DEMOLISL EQU   *-DEMOLIST                                                       
                                                                                
UNTRRTG  DS    (DEMOMAXN)XL(L'XDESTRTG)                                         
UNTRRTGL EQU   *-UNTRRTG                                                        
                                                                                
OVALUES  DS    0D                  ** Output values **                          
                                                                                
GWTOTS   DS    0X                  ** Goal posting totals **                    
GWTPOSTS DS    PL2                 Number of weeks posted                       
GWTDOLLS DS    PL8                 Total dollars for all posted weeks           
GWTGRP1  DS    PL8                 Total GRP1 for all posted weeks              
GWTGRP2  DS    PL8                 Total GRP2 for all posted weeks              
                                                                                
         ORG   SAVED+(4*ONEK)      Position to product list                     
PRDLST   DS    0X                  ** Product list entry **                     
PRDLPRD  DS    CL(L'PKEYPRD)       Product code                                 
PRDLNUM  DS    X                   Product number                               
PRDLCLS  DS    X                   Product class                                
PRDLSTL  EQU   *-PRDLST            Length of product list entry                 
         EJECT                                                                  
* Other include books                                                           
       ++INCLUDE NENAVWORKD                                                     
       ++INCLUDE SPGENESTD                                                      
NDPTHDRD DSECT ,                                                                
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE SPGENSLST                                                      
       ++INCLUDE SPSTAPACKD                                                     
************STAPACKL EQU   *-STAPACKD                                           
                                                                                
NETIOD   DSECT ,                                                                
       ++INCLUDE NETBLOCKN                                                      
NBWNETIO EQU   C'I'                Called from NETIO                            
NBFILUNT EQU   C'U'                Unit file                                    
NBESTOUQ EQU   C'M'                Return demos unconditionally                 
       ++INCLUDE NETDEMOP                                                       
NETBLKL  EQU   *-NETBLOCK                                                       
         ORG   XDESTDEM                                                         
XDESTVAL DS    0X                  ** Returned demo array **                    
XDESTVPH DS    XL2                 VPH                                          
XDESTRTG DS    XL2                 Rating value (1 decimal place)               
XDESTIMP DS    XL4                 Impressions                                  
XDESTLNQ EQU   *-XDESTVAL                                                       
                                                                                
       ++INCLUDE FALOCKETD                                                      
                                                                                
WORKD    DSECT ,                   ** Redefine OVERWORK **                      
         ORG   OVERWORK                                                         
                                                                                
PXCRECD  DSECT ,                   ** Redefine PXCPGM **                        
         ORG   PXCPGM              ** Exclusion program name field **           
PXCPGIND DS    X                   ** Indicator **                              
PXCTIMQ  EQU   0                   Time exclusion element                       
PXCDAYQ  EQU   1                   Day exclusion element                        
PXCPGDAY DS    0X                  Day mask                                     
LIMNDAYS EQU   7                   Number of day bits in PXCPGIND               
PXCPGSTM DS    XL2                 Start time                                   
PXCPGETM DS    XL2                 End time                                     
         ORG                                                                    
                                                                                
NUPDED   DSECT ,                                                                
NUPDEDL  EQU   *-NUPDED                                                         
NUPRDD   DSECT ,                                                                
NUPRPLEN EQU   *-NUPRDPR                                                        
NUPRDDL  EQU   *-NUPRDD                                                         
         ORG                                                                    
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013NENAV14   06/22/20'                                      
         END                                                                    
