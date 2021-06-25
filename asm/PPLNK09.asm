*          DATA SET PPLNK09    AT LEVEL 008 AS OF 01/23/13                      
*PHASE T41409A                                                                  
PPLNK09  TITLE '- Printpak CFM downloads'                                       
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,REQUEST=*,CODE=CODE,SYSTEM=PRTSYSQ,FILES=FILES,  +        
               LOADFACSOFF=Y,SERVERTYPE=TSTADBY,IDF=Y,WORKERKEY=PPCF,  +        
               SYSPHASE=SYSPHASE,                                      +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#SECD,SECD,        +        
               B#LPD,LP_D,B#CLT,PCLTRECD,B#PRD,PPRDRECD,               +        
               B#COM,PCOMRECD,B#AGY,PAGYRECD,B#OFC,MOFRECD,            +        
               B#DIV,PDIVRECD,B#OTH,POTHRECD,B#SDR,SDRRECD)                     
                                                                                
B#SECD   EQU   3                   SECD                                         
B#LPD    EQU   4                   LP_D                                         
B#SDR    EQU   5                   PCOMRECD                                     
ASDRREC  EQU   LP_BLKS+((B#SDR-1)*L'LP_BLKS),,C'A'                              
B#CLT    EQU   5                   PCLTRECD                                     
ACLTREC  EQU   LP_BLKS+((B#CLT-1)*L'LP_BLKS),,C'A'                              
B#PRD    EQU   6                   PPRDRECD                                     
APRDREC  EQU   LP_BLKS+((B#PRD-1)*L'LP_BLKS),,C'A'                              
B#COM    EQU   6                   PCOMRECD                                     
ACOMREC  EQU   LP_BLKS+((B#COM-1)*L'LP_BLKS),,C'A'                              
B#AGY    EQU   6                   PAGYRECD                                     
AAGYREC  EQU   LP_BLKS+((B#AGY-1)*L'LP_BLKS),,C'A'                              
B#OFC    EQU   6                   MOFRECD                                      
AOFCREC  EQU   LP_BLKS+((B#OFC-1)*L'LP_BLKS),,C'A'                              
B#DIV    EQU   6                   PCOMRECD                                     
ADIVREC  EQU   LP_BLKS+((B#DIV-1)*L'LP_BLKS),,C'A'                              
B#OTH    EQU   6                   PCOMRECD                                     
AOTHREC  EQU   LP_BLKS+((B#OTH-1)*L'LP_BLKS),,C'A'                              
                                                                                
CODE     NMOD1 0,**PL09**                                                       
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(global literals)                        
                                                                                
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(DDLINK control block)                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test offline                                 
         BNZ   INIT02                                                           
         L     R9,LP_ABLK1         Online - root provides WORKD/SAVED           
         L     R8,LP_ABLK2                                                      
         B     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE                                                      
         USING WORKD,R9            R9=A(global w/s)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         LAY   R8,WORKD+(((WORKL+7)/8)*8)                                       
         USING SAVED,R8            R8=A(save w/s)                               
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         USING OFFICED,OFCBLK                                                   
                                                                                
INIT04   MVC   GETUID,RGETUID      Set A(GETUID)                                
         MVC   RUNMODE,RUNPMODE    Extract DDLINK calling mode                  
         MVC   ACOMFACS,RCOMFACS                                                
         ST    R5,ALP              Save A(DDLINK parameter list)                
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
         DROP  R6,R7,RB                                                         
         EJECT                                                                  
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    Test first for run                           
         JNE   PRCWRK                                                           
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test offline                                 
         JZ    RUNSTR02            No                                           
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('O#ROU1',0),0,0                                       
         MVC   AROUT1,0(R1)                                                     
         MVC   LP_AUIR1,AROUT1     Set A(index routines 1)                      
         GOTOR (RF),DMCB,('O#ROU2',0),0,0                                       
         MVC   AROUT2,0(R1)                                                     
         MVC   LP_AUIR2,AROUT2     Set A(index routines 2)                      
                                                                                
         GOTOR (#WRKINI,AWRKINI)   Initialize working storage                   
                                                                                
RUNSTR02 MVC   LP_BLKS+((B#SECD-1)*L'LP_BLKS),LP_ASECD                          
         MVC   LP_BLKS+((B#LPD-01)*L'LP_BLKS),ALP                               
         MVC   LP_BLKS+((B#CLT-01)*L'LP_BLKS),AIO5                              
         MVC   LP_BLKS+((B#PRD-01)*L'LP_BLKS),AIO6                              
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CRECUP-COMFACSD(RF)                                           
         ST    RF,VRECUP                                                        
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* First for new work                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    Test 'process work' mode                     
         JNE   RUNREQ                                                           
         LA    R0,QVALUES                                                       
         LHI   R1,QVALUEL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   SYSLET,SYSPRTQ                                                   
         MVC   QAGY,LP_AGY                                                      
         MVI   MEDRSTR,C'A'        Media code start range                       
         MVI   MEDREND,C'Z'        Media code end range                         
         XC    CLTRSTR,CLTRSTR     Client code start range                      
         MVC   CLTREND,=X'FFFFFF'  Client code end range                        
         XC    PRDRSTR,PRDRSTR     Product code start range                     
         MVC   PRDREND,=X'FFFFFF'  Product code end range                       
         MVC   DIVRSTR,=C'000'     Division code start range                    
         MVC   DIVREND,=C'999'     Division code start range                    
         MVI   QANYREQ,0           Init any request y/n switch                  
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Run a download request                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    Test 'run request' mode                      
         JNE   EXITY                                                            
         TM    LP_FLAG,LP_FOFFL    Test offline                                 
         JZ    RUNREQ02                                                         
         CLC   LP_QMAPN,=AL2(I#CFMCDL)                                          
         JE    RUNREQ02                                                         
         GOTOR VDATAMGR,DMCB,DMKEY,PRTDIR,(4,0),0                               
         GOTOR (RF),(R1),DMKEY,PRTFIL,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,PUBDIR,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,PUBFIL,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,GENDIR,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,GENFIL,(4,0),0                                   
                                                                                
RUNREQ02 LA    R1,LP_D                                                          
         GOTOR LP_APUTO            Call DDLINK output processor                 
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* CFM initial download (X'FFF1')                                      *         
***********************************************************************         
                                                                                
REQCFMI  LKREQ *,I#CFMIDL,OUTCFMI,NEXTREQ=REQCFMC                               
                                                                                
OUTCFMI  LKOUT H                                                                
CFMI     LKOUT R,001                                                            
LimAcs   LKOUT C,001,(D,B#LPD,LP_ACCS),HEXD                                     
UserId   LKOUT C,002,(D,B#LPD,LP_USRID),HEXD                                    
PIDNum   LKOUT C,003,(D,B#SECD,SECOPASS),HEXD                                   
AccGrp   LKOUT C,004,(D,B#SECD,SECOSAGN),HEXD                                   
PerAgy   LKOUT C,005,(D,B#SECD,SECOAGPE),CHAR                                   
SecSys   LKOUT C,006,(D,B#SECD,SECOSYS),HEXD                                    
SecPrg   LKOUT C,007,(D,B#SECD,SECOPRG),HEXD                                    
SysLet   LKOUT C,008,(D,B#SAVED,SYSLET),CHAR                                    
         LKOUT E                                                                
                                                                                
AGYR     LKOUT R,X'0041'                                                        
Array    LKOUT C,X'0041',(A,ARYAGY)                                             
         LKOUT E                                                                
                                                                                
OFC2     LKOUT R,X'0042'                                                        
Array    LKOUT C,X'0042',(A,ARY2OF)                                             
         LKOUT E                                                                
                                                                                
OFC1     LKOUT R,X'0042'                                                        
Array    LKOUT C,X'0042',(A,ARY1OF),FILTROUT=TST1OF                             
         LKOUT E                                                                
                                                                                
SDREC    LKOUT R,X'0046'                                                        
PRout    LKOUT P,,GETSDR                                                        
Array    LKOUT C,X'0046',(A,ARYSDR)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYAGY   LKOUT A,(R,NXTAGY),MULTIROW=Y,ROWNAME=PAGYRECD                         
PMedCod  LKOUT C,001,PAGYKMED,CHAR                                              
Array    LKOUT C,010,(A,ARYAMDD)                                                
Array    LKOUT C,040,(A,ARYA_FX)                                                
Array    LKOUT C,060,(A,ARYAAAG)                                                
         LKOUT E                                                                
                                                                                
ARYAMDD  LKOUT A,(D,B#AGY,PAGYELEM),EOT=EOR,ROWID=(PAGYELEM,X'01'),    +        
               ROWWIDTH=(V,PAGYELEM+1)                                          
AMedDsp  LKOUT C,010,PAGYMED,CHAR                                               
AAgyPrf  LKOUT C,011,PAGYPROF,CHAR                                              
AAgyNat  LKOUT C,012,PAGYNAT,CHAR                                               
         LKOUT E                                                                
                                                                                
ARYA_FX  LKOUT A,(D,B#AGY,PAGYELEM),EOT=EOR,ROWID=(PAGFXEL,PAGFXIDQ),  +        
               ROWWIDTH=(V,PAGFXLEN)                                            
AAgyFXR  LKOUT C,040,PAGFXREP,CHAR                                              
         LKOUT E                                                                
                                                                                
ARYAAAG  LKOUT A,(D,B#AGY,PAGYELEM),EOT=EOR,ROWID=(PAGYACCEL,X'03'),   +        
               ROWWIDTH=(V,PAGYACCEL+1)                                         
AAccAgy  LKOUT C,060,PAGYACCAG,CHAR                                             
         LKOUT E                                                                
                                                                                
ARY2OF   LKOUT A,(R,NXT2OF),MULTIROW=Y,ROWNAME=MOFRECD                          
OFCAlph  LKOUT C,010,MOFFC2OF,(R,EDTOFC)                                        
Array    LKOUT C,011,(A,ARYOFNM)                                                
         LKOUT E                                                                
                                                                                
ARY1OF   LKOUT A,(R,NXT1OF),MULTIROW=Y,ROWNAME=MOFRECD                          
Array    LKOUT C,010,(D,B#WORKD,BYTE1),CHAR                                     
         LKOUT E                                                                
                                                                                
ARYOFNM  LKOUT A,(D,B#OFC,MOFFSYS+L'MOFFSYS),EOT=EOR,                  +        
               ROWID=(MONAMEL,MONAMELQ),ROWWIDTH=(V,MONAMLN)                    
OFCNmSh  LKOUT C,011,MONAMSH,CHAR                                               
OFCNMLg  LKOUT C,012,MONAMLO,CHAR                                               
         LKOUT E                                                                
                                                                                
EDTOFC   LM    R2,R4,LP_AINP                                                    
         MVC   0(L'MOFK2OF,R4),0(R2)                                            
         LHI   R0,2                                                             
         CLI   1(R2),C' '          1 character office code?                     
         JH    SETOLENX                                                         
         LA    R1,ONEOFCTB         Point to 1 char office code table            
EDTOFC4  CLI   0(R1),X'FF'         End of table?                                
         JE    SETOLENX                                                         
         CLC   0(1,R1),0(R2)       Match 2 chars office code from rec?          
         JNE   *+12                                                             
         MVI   0(R1),0             Blank it out                                 
         J     SETOLENX                                                         
         LA    R1,1(R1)            next entry in table                          
         J     EDTOFC4                                                          
                                                                                
TST1OF   CLI   QONEOFC,YESQ        Test ONE character office code               
         BR    RE                                                               
                                                                                
***********************************************************************         
* Read agency/media records                                           *         
***********************************************************************         
                                                                                
NXTAGY   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',AGYKEYT),                +        
               ('B#AGY',0),SAVED,0,0                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Read office records                                                 *         
***********************************************************************         
                                                                                
NXT2OF   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXT2OF10                                                         
                                                                                
         MVI   QONEOFC,NOQ         Set ONE char office code to NO               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING CT5KEY,RE           Read access record                           
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,QAGY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO1'                            
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AIO1                                                          
         AHI   RE,CT5DATA-CT5REC                                                
         USING CTAADD,RE           Locate Agency Access Details                 
         SR    R0,R0                                                            
NXT2OF02 CLI   CTAADEL,0           Test end of record                           
         JE    NXT2OF03                                                         
         CLI   CTAADEL,CTAADELQ    Agency Access Details element?               
         JE    *+14                                                             
         IC    R0,CTAADLEN         No - bump to next                            
         AR    RE,R0                                                            
         J     NXT2OF02                                                         
         TM    CTAADFLG,CTAAD2OF   Agy using 2 chars media offices?             
         JNZ   *+12                                                             
NXT2OF03 MVI   QONEOFC,YESQ        Set ONE char office code to YES              
         J     NOMORE                                                           
         DROP  RE                                                               
                                                                                
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         L     RE,AIO3                                                          
         ST    RE,AOFCREC                                                       
         LA    RE,IOKEY                                                         
         USING MOFKEY,RE                                                        
         MVI   MOFKTYP,MOFKTYPQ                                                 
         MVI   MOFKSUB,MOFKS2Q                                                  
         MVC   MOFKAGY,QAGY                                                     
         DROP  RE                                                               
NXT2OF05 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGENDIR+IO3'                            
         JNE   NOMORE                                                           
NXT2OF07 LA    RE,IOKEY                                                         
         USING MOFKEY,RE                                                        
         CLI   MOFKTYP,MOFKTYPQ    Office limit access record?                  
         JNE   NOMORE                                                           
         CLC   MOFKAGY,QAGY        Same request agency?                         
         JNE   NOMORE                                                           
         CLI   MOFKSUB,MOFKS2Q     Two bytes office code?                       
         JNE   *+12                                                             
         CLI   MOFKSYS,PRTSYSQ     Print system?                                
         JE    NXT2OF20                                                         
         DROP  RE                                                               
NXT2OF10 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOGENDIR+IO3'                            
         JE    NXT2OF07                                                         
         J     NOMORE                                                           
NXT2OF20 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGENFIL+IO3'                           
         JNE   NOMORE                                                           
         MVC   LP_ADATA,AOFCREC    Point to office record                       
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Process one character office code table                             *         
***********************************************************************         
                                                                                
NXT1OF   MVI   BYTE1,0                                                          
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXT1OF10                                                         
         XC    ELEM,ELEM                                                        
         MVC   ELEM(ONEOFCLN),ONEOFCTB                                          
NXT1OF10 LA    RE,ELEM                                                          
NXT1OF15 CLI   0(RE),X'FF'         End of table?                                
         JE    NOMORE                                                           
         CLI   0(RE),0             Blank entry?                                 
         JNE   *+12                                                             
         LA    RE,1(RE)                                                         
         J     NXT1OF15                                                         
         MVC   BYTE1,0(RE)                                                      
         MVI   0(RE),0             Clear it                                     
         MVC   LP_ADATA,AOFCREC    Point to office record                       
         J     EXITY                                                            
                                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Call DDLINK to get Self-defining record                             *         
*                                                                     *         
* Note:  AIO3  used for Generic SDR                                   *         
*        AIO5  used for Agency specific SDR                           *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETSDR   GOTOR LP_AGSDR,DMCB,LP_D,AIO3,0                                        
*                                                                               
*                                                                               
* Agency Alpha SDR override                                                     
*                                                                               
         L     R2,AIO5                                                          
         USING SDRRECD,R2                                                       
         XC    0(L'SDRKEY,R2),0(R2)                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'LP_AGY),LP_AGY                                            
         GOTOR LP_AGSDR,DMCB,LP_D,AIO5,WORK                                     
*                                                                               
         L     RE,AIO3             Need this check as GETSDR does a             
         L     RF,AIO5             default READ by clearing SDRKFFL             
         CLC   0(SDRKKEY-SDRKEY,RE),0(RF)                                       
         JNE   GTSDR10                                                          
*                                                                               
         BRAS  RE,SDROVRW                                                       
*                                                                               
* Environment SDR override                                                      
*                                                                               
GTSDR10  L     R2,AIO5                                                          
         USING SDRRECD,R2                                                       
         XC    0(L'SDRKEY,R2),0(R2)                                             
*                                                                               
         CLI   ENVIRO,0              Enviroment override?                       
         JE    GTSDRX                No                                         
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'ENVIRO),ENVIRO                                          
         GOTOR LP_AGSDR,DMCB,LP_D,AIO5,WORK                                     
*                                                                               
         L     RE,AIO3             Need this check as GETSDR does a             
         L     RF,AIO5             default READ by clearing SDRKFFL             
         CLC   0(SDRKKEY-SDRKEY,RE),0(RF)                                       
         JNE   GTSDRX                                                           
*                                                                               
         BRAS  RE,SDROVRW                                                       
*                                                                               
GTSDRX   J     EXITCC                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Overwrite values in Generic SDR with that of Specific SDR           *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SDROVRW  NTR1  LABEL=*                                                          
         L     R2,AIO5                                                          
         OC    0(2,R2),0(R2)       Have Agency specific SDR?                    
         JZ    SDROVX                                                           
         LA    R2,SDRRFRST                                                      
SDROV010 CLI   0(R2),0             End of Agency specific SDR?                  
         JE    SDROVX                                                           
*                                                                               
         L     R6,AIO3                                                          
         LA    R6,SDRRFRST-SDRKEY(R6)                                           
SDROV020 CLI   0(R6),0             Matching elem found?                         
         JE    SDROV040            No, add it to Generic SDR                    
         CLC   0(1,R6),0(R2)                                                    
         JE    SDROV030                                                         
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         J     SDROV020                                                         
*                                                                               
* DELETE THE ELEM IN GENERIC                                                    
SDROV030 GOTO1 VRECUP,DMCB,(X'FE',AIO3),0(R6),0(R6),=X'002A002007D0'            
*                                                                               
* ADD ELEM IN GENERIC                                                           
SDROV040 GOTO1 VRECUP,DMCB,(X'FE',AIO3),0(R2),0(R6),=X'002A002007D0'            
*                                                                               
         LLC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         J     SDROV010                                                         
*                                                                               
SDROVX   J     EXITCC                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Send Self-Defining Elements                                         *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ARYSDR   LKOUT A,(D,B#SDR,SDRRFRST),EOT=EOR,ROWWIDTH=(V,SDELEN),       +        
               ROWID=(SDELD,0)                                                  
                                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* FIELDS SENT FROM SELF-DEFINING RECORD ARE AS FOLLOWS:-              *         
*                                                                     *         
*        10    Agency uses Midas Feature                              *         
*        11    (do not use - Pinergy feature)                         *         
*        20    (do not use - Maximum # of prd record allowed)         *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
Defvl    LKOUT C,255,SDELD,SDEL                                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* CFM client download (X'FFF2')                                       *         
***********************************************************************         
                                                                                
REQCFMC  LKREQ H,I#CFMCDL,OUTCFMC,NEXTREQ=REQEND                                
                                                                                
***********************************************************************         
* Agency connection array                                             *         
***********************************************************************         
                                                                                
System   LKREQ F,01,(I,B#SAVED,QAGYIND),CHAR,TEXT=(*,SYS1LIT),         +        
               OLEN=L'AA_SYS,ARRAY=S                                            
Agency   LKREQ F,02,,CHAR,TEXT=(*,AGY1LIT),OLEN=L'AA_OAGY                       
LimAccs  LKREQ F,03,,HEXD,TEXT=(*,LIMALIT),OLEN=L'AA_ACCS                       
UserId#  LKREQ F,04,,HEXD,TEXT=(*,USIDLIT),OLEN=L'AA_USID                       
PID#     LKREQ F,05,,HEXD,TEXT=(*,PIDNLIT),OLEN=L'AA_PASS                       
SAGRP    LKREQ F,06,,HEXD,TEXT=(*,SAGRLIT),OLEN=L'AA_SAGN                       
PSAGY    LKREQ F,07,,CHAR,TEXT=(*,PSAGLIT),OLEN=L'AA_AGPE                       
SSYSM    LKREQ F,08,,HEXD,TEXT=(*,SSYSLIT),OLEN=L'AA_OSYS                       
SPRGM    LKREQ F,09,,HEXD,TEXT=(*,SPRGLIT),OLEN=L'AA_OPRG,ARRAY=E               
                                                                                
AA_D     DSECT ,                   ** Agency connection array **                
AA_SYS   DS    C                   System letter                                
AA_OAGY  DS    CL(L'SECOAGY)       Agency alpha id                              
AA_ACCS  DS    CL(L'LP_ACCS)       Limit access value                           
AA_USID  DS    XL(L'LP_USRID)      User id number                               
AA_PASS  DS    XL(L'SECOPASS)      PID number                                   
AA_SAGN  DS    XL(L'SECOSAGN)      Security agency group                        
AA_AGPE  DS    CL(L'SECOAGPE)      Person security agency                       
AA_OSYS  DS    XL(L'SECOSYS)       Security system number                       
AA_OPRG  DS    XL(L'SECOPRG)       Security program number                      
AA_LNQ   EQU   *-AA_D              Width of array row                           
SVRDEF   CSECT ,                                                                
                                                                                
***********************************************************************         
* Client request array                                                *         
***********************************************************************         
                                                                                
System   LKREQ F,10,(I,B#SAVED,QCLTIND),CHAR,TEXT=(*,SYS2LIT),         +        
               OLEN=L'CA_SYS,ARRAY=S                                            
Agency   LKREQ F,11,,CHAR,TEXT=(*,AGY2LIT),OLEN=L'CA_AGYA                       
Media    LKREQ F,12,,CHAR,TEXT=(*,MEDCLIT),OLEN=L'CA_MEDC                       
Client   LKREQ F,13,,CHAR,TEXT=(*,CLTCLIT),OLEN=L'CA_CLTC                       
Produc   LKREQ F,14,,CHAR,TEXT=(*,PRDCLIT),OLEN=L'CA_PRDC                       
Token    LKREQ F,30,,CHAR,TEXT=(*,TOKNLIT),OLEN=L'CA_TOKEN,ARRAY=E              
                                                                                
CA_D     DSECT ,                   ** Client request array **                   
CA_SYS   DS    C                   System letter                                
CA_AGYA  DS    CL2                 Agency alpha id                              
CA_MEDC  DS    C                   Media code                                   
CA_CLTC  DS    CL5                 Client code                                  
CA_PRDC  DS    CL5                 Product code                                 
CA_TOKEN DS    CL8                 PC token value                               
CA_LNQ   EQU   *-CA_D              Width of array row                           
SVRDEF   CSECT ,                                                                
                                                                                
***********************************************************************         
* Other options                                                       *         
***********************************************************************         
                                                                                
DLClts?  LKREQ F,40,(D,B#SAVED,QCLTOPT),CHAR,TEXT=(*,DCLTLIT)                   
DLPrds?  LKREQ F,41,(D,B#SAVED,QPRDOPT),CHAR,TEXT=(*,DPRDLIT)                   
DLComs?  LKREQ F,42,(D,B#SAVED,QCOMOPT),CHAR,TEXT=(*,DCOMLIT)                   
Option04 LKREQ F,43,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option05 LKREQ F,44,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
DLDivs?  LKREQ F,45,(D,B#SAVED,QDIVOPT),CHAR,TEXT=(*,DDIVLIT)                   
DLOthA?  LKREQ F,46,(D,B#SAVED,QOTHOPT),CHAR,TEXT=(*,DOTHLIT)                   
Option08 LKREQ F,47,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option09 LKREQ F,48,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option10 LKREQ F,49,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option11 LKREQ F,50,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option12 LKREQ F,51,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option13 LKREQ F,52,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option14 LKREQ F,53,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option15 LKREQ F,54,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option16 LKREQ F,55,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
                                                                                
         LKREQ E                                                                
                                                                                
SYS1LIT  DC    C'Agency system'                                                 
AGY1LIT  DC    C'       agency alpha id'                                        
LIMALIT  DC    C'       limit access'                                           
USIDLIT  DC    C'       user id#'                                               
PIDNLIT  DC    C'       PID#'                                                   
SAGRLIT  DC    C'       access group#'                                          
PSAGLIT  DC    C'       person agency'                                          
SSYSLIT  DC    C'       security system#'                                       
SPRGLIT  DC    C'       security program#'                                      
SYS2LIT  DC    C'Client system'                                                 
AGY2LIT  DC    C'       agency alpha id'                                        
MEDCLIT  DC    C'       media code'                                             
CLTCLIT  DC    C'       client code'                                            
PRDCLIT  DC    C'       product code'                                           
TOKNLIT  DC    C'       PC request token'                                       
DCLTLIT  DC    C'Download clients?'                                             
DPRDLIT  DC    C'Download products?'                                            
DCOMLIT  DC    C'Download comments?'                                            
DDIVLIT  DC    C'Download divisions?'                                           
DOTHLIT  DC    C'Download Other Agy?'                                           
DOPTLIT  DC    C'Download Options'                                              
                                                                                
OUTCFMC  LKOUT H                                                                
                                                                                
CFMC     LKOUT R,X'0041'                                                        
Array    LKOUT C,X'0041',(A,ARYCLT)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYCLT   LKOUT A,(R,NXTCLT),MULTIROW=Y,ROWNAME=PCLTRECD                         
CMedCod  LKOUT C,001,PCLTKMED,CHAR,FILTROUT=TSTCLT,SKIPCOLS=CLTSKIPN            
CLTSKIPS DS    0X                  Start of columns to skip                     
CCltCod  LKOUT C,002,PCLTKCLT,CHAR                                              
CCltNam  LKOUT C,003,PCLTNAME,CHAR                                              
CBilRNm  LKOUT C,004,PCLTBNAM,CHAR,ND=Y                                         
CCltLn1  LKOUT C,005,PCLTLIN1,CHAR,ND=Y                                         
CCltLN2  LKOUT C,006,PCLTLIN2,CHAR,ND=Y                                         
CCltAtt  LKOUT C,007,PCLTATTN,CHAR,ND=Y                                         
                                                                                
CCltDiv  LKOUT C,020,PCLTPROF+00,CHAR,LEN=1,ND=Y                                
CBilDCa  LKOUT C,021,PCLTPROF+01,CHAR,LEN=1,ND=Y                                
CBilDBa  LKOUT C,022,PCLTPROF+02,CHAR,LEN=1,ND=Y                                
CBilDAj  LKOUT C,023,PCLTPROF+03,CHAR,LEN=1,ND=Y                                
CNewsAj  LKOUT C,024,PCLTPROF+04,CHAR,LEN=1,ND=Y                                
CCltTyp  LKOUT C,025,PCLTPROF+05,CHAR,LEN=1,ND=Y                                
CMstClt  LKOUT C,026,PCLTPROF+06,CHAR,LEN=3,ND=Y                                
CPEBilF  LKOUT C,027,PCLTPROF+10,CHAR,LEN=1,ND=Y                                
CConReq  LKOUT C,028,PCLTPROF+12,CHAR,LEN=1,ND=Y                                
CBudAmt  LKOUT C,029,PCLTPROF+14,CHAR,LEN=1,ND=Y                                
CEstRnd  LKOUT C,030,PCLTPROF+18,CHAR,LEN=1,ND=Y                                
                                                                                
CCltOff  LKOUT C,055,PCLTOFF,(R,EDTCOF),ND=Y                                    
CCanGST  LKOUT C,056,PCLTGST,CHAR,ND=Y                                          
CBilGrp  LKOUT C,057,PCLTBLGP,CHAR,ND=Y                                         
CCltNum  LKOUT C,058,PCLTNUM,(R,EDTCLN),ND=Y                                    
CAgyRec  LKOUT C,059,PCLTAGYR,CHAR,ND=Y                                         
PRout    LKOUT P,PCLTSTAT,SETBITS                                               
         LKOUT C,060,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,061,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,062,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,063,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,064,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,065,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,066,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,067,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
CFinClt  LKOUT C,068,PCLTFIN,CHAR,ND=Y                                          
CAgyOfC  LKOUT C,069,PCLTAOFC,CHAR,ND=Y                                         
                                                                                
Array    LKOUT C,070,(A,ARYCSCM)                                                
Array    LKOUT C,071,(A,ARYIOCM)                                                
Array    LKOUT C,072,(A,ARYAGYR)                                                
Array    LKOUT C,110,(A,ARYUDEF)                                                
* * * *  LKOUT C,190,(A,ARYCPST)                                                
Array    LKOUT C,191,(A,ARYDRDC)                                                
Array    LKOUT C,192,(A,ARYZENC)                                                
Array    LKOUT C,193,(A,ARYMEDN)                                                
Array    LKOUT C,194,(A,ARYCOS2)                                                
Array    LKOUT C,195,(A,ARYRFPG)                                                
Array    LKOUT C,205,(A,ARYFRZS)                                                
Array    LKOUT C,216,(A,ARYTRAF)                                                
Array    LKOUT C,220,(A,ARYCPS_),PCVERSION=1.0.5.0                              
Array    LKOUT C,230,(A,ARYCMPS),PCVERSION=1.0.5.0                              
                                                                                
CEstRSw  LKOUT C,217,(D,B#SAVED,SVESTRSW),CHAR                                  
                                                                                
PRout    LKOUT P,PCLTKCLT,SETAAA                                                
         LKOUT C,218,(D,B#WORKD,WORK+00),CHAR,LEN=20,ND=Y                       
         LKOUT C,219,(D,B#WORKD,WORK+20),CHAR,LEN=20,ND=Y                       
         LKOUT C,220,(D,B#WORKD,WORK2+00),CHAR,LEN=30,ND=Y                      
         LKOUT C,221,(D,B#WORKD,WORK2+30),CHAR,LEN=30,ND=Y                      
         LKOUT C,222,(D,B#WORKD,WORK+40),CHAR,LEN=24,ND=Y                       
                                                                                
CCltTok  LKOUT C,300,(D,B#SAVED,SVREQTOK),CHAR                                  
CLTSKIPN EQU   (*-CLTSKIPS)/LX_COLSL                                            
                                                                                
Array    LKOUT C,X'0049',(A,ARYPRF),FILTROUT=TSTSYS                             
Array    LKOUT C,X'0042',(A,ARYPRD),FILTROUT=TSTPRD                             
Array    LKOUT C,X'0043',(A,ARYCOM),FILTROUT=TSTCOM                             
Array    LKOUT C,X'0044',(A,ARYDIV),FILTROUT=TSTDIV                             
Array    LKOUT C,X'0045',(A,ARYOTH),FILTROUT=TSTOTH                             
         LKOUT E                                                                
                                                                                
ARYCSCM  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTCSCM,X'10'),    +        
               ROWWIDTH=(V,PCLTCSCM+1)                                          
CConSCm  LKOUT C,070,PCLTCNUM,(R,EDTCM#),ND=Y                                   
         LKOUT E                                                                
                                                                                
ARYIOCM  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTISCM,X'11'),    +        
               ROWWIDTH=(V,PCLTISCM+1)                                          
CIOComm  LKOUT C,071,PCLTINUM,(R,EDTCM#),ND=Y                                   
         LKOUT E                                                                
                                                                                
ARYAGYR  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTADVE,X'15'),    +        
               ROWWIDTH=(V,PCLTADVE+1)                                          
CAgyRec  LKOUT C,072,PCLTAOR,CHAR,ND=Y                                          
CAdvtsr  LKOUT C,073,PCLTADV,CHAR,ND=Y                                          
CAdvClt  LKOUT C,074,PCLTADVC,CHAR,ND=Y                                         
CAoRSDt  LKOUT C,075,PCLTASDT,BDAT,ND=Y                                         
CAoREDt  LKOUT C,076,PCLTAEDT,BDAT,ND=Y                                         
CAoRSE#  LKOUT C,077,PCLTAORS,CHAR,ND=Y                                         
PRout    LKOUT P,PCLTACON+0,SETBITS                                             
         LKOUT C,078,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,079,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,080,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,081,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,082,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,083,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,084,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,085,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
PRout    LKOUT P,PCLTACON+1,SETBITS                                             
         LKOUT C,093,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
         LKOUT E                                                                
                                                                                
ARYUDEF  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTUDEF,X'20'),    +        
               ROWWIDTH=(V,PCLTUDEF+1)                                          
CPr1Dsp  LKOUT C,110,PCLTPU1,CHAR,ND=Y                                          
CPr1Typ  LKOUT C,111,PCLTP1TY,CHAR,ND=Y                                         
CPr1Len  LKOUT C,112,PCLTP1LN,UBIN,ND=Y                                         
PRout    LKOUT P,PCLTP1F1,SETBITS                                               
         LKOUT C,113,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,114,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,115,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,116,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,117,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,118,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,119,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,120,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CPr2Dsp  LKOUT C,130,PCLTPU2,CHAR,ND=Y                                          
CPr2Typ  LKOUT C,131,PCLTP2TY,CHAR,ND=Y                                         
CPr2Len  LKOUT C,132,PCLTP2LN,UBIN,ND=Y                                         
PRout    LKOUT P,PCLTP2F1,SETBITS                                               
         LKOUT C,133,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,134,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,135,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,136,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,137,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,138,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,139,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,140,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CEs1Dsp  LKOUT C,150,PCLTEU1,CHAR,ND=Y                                          
CEs1Typ  LKOUT C,151,PCLTE1TY,CHAR,ND=Y                                         
CEs1Len  LKOUT C,152,PCLTE1LN,UBIN,ND=Y                                         
PRout    LKOUT P,PCLTE1F1,SETBITS                                               
         LKOUT C,153,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,154,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,155,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,156,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,157,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,158,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,159,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,160,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CEs2Dsp  LKOUT C,170,PCLTEU2,CHAR,ND=Y                                          
CEs2Typ  LKOUT C,171,PCLTE2TY,CHAR,ND=Y                                         
CEs2Len  LKOUT C,172,PCLTE2LN,UBIN,ND=Y                                         
PRout    LKOUT P,PCLTE2F1,SETBITS                                               
         LKOUT C,173,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,174,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,175,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,176,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,177,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,178,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,179,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,180,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
         LKOUT E                                                                
                                                                                
ARYCPST  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTPST,X'25'),     +        
               ROWWIDTH=(V,PCLTPST+1)                                           
CCanPST  LKOUT C,190,PCLTPSTC,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYCPS_  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTPST,X'25'),     +        
               ROWWIDTH=(V,PCLTPST+1)                                           
CCanPST  LKOUT C,220,PCLTPSTC+00,CHAR,LEN=1,ND=Y                                
         LKOUT C,221,PCLTPSTC+01,CHAR,LEN=1,ND=Y                                
         LKOUT C,222,PCLTPSTC+02,CHAR,LEN=1,ND=Y                                
         LKOUT C,223,PCLTPSTC+03,CHAR,LEN=1,ND=Y                                
         LKOUT C,224,PCLTPSTC+04,CHAR,LEN=1,ND=Y                                
         LKOUT C,225,PCLTPSTC+05,CHAR,LEN=1,ND=Y                                
         LKOUT C,226,PCLTPSTC+06,CHAR,LEN=1,ND=Y                                
         LKOUT C,227,PCLTPSTC+07,CHAR,LEN=1,ND=Y                                
         LKOUT C,228,PCLTPSTC+08,CHAR,LEN=1,ND=Y                                
         LKOUT C,229,PCLTPSTC+09,CHAR,LEN=1,ND=Y                                
         LKOUT E                                                                
                                                                                
ARYCMPS  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTMPS,PCLTMPEQ),  +        
               ROWWIDTH=(V,PCLTMPSL)                                            
CCanMPS  LKOUT C,230,PCLTMPSC,(R,EDTMPS),ND=Y                                   
         LKOUT C,231,(D,B#WORKD,BYTE1),CHAR,LEN=1,ND=Y                          
         LKOUT E                                                                
                                                                                
ARYDRDC  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTDRD,X'30'),     +        
               ROWWIDTH=(V,PCLTLN)                                              
CDRDCod  LKOUT C,191,PCLTDRDC,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYZENC  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTZEL,X'32'),     +        
               ROWWIDTH=(V,PCLTZLEN)                                            
CZenCod  LKOUT C,192,PCLTZEN,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
ARYMEDN  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTMEL,X'41'),     +        
               ROWWIDTH=(V,PCLTMLEN)                                            
CMedNam  LKOUT C,193,PCLTMNAM,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYCOS2  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTCFEL,X'45'),    +        
               ROWWIDTH=(V,PCLTCFLN)                                            
CCos2Fc  LKOUT C,194,PCLTCF,SPAK,ND=Y                                           
         LKOUT E                                                                
                                                                                
ARYRFPG  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTTAEL,X'46'),    +        
               ROWWIDTH=(V,PCLTTALN)                                            
CRFPGCd  LKOUT C,195,PCLTTAGRP,CHAR,ND=Y                                        
         LKOUT E                                                                
                                                                                
ARYFRZS  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTFEL,X'47'),     +        
               ROWWIDTH=(V,PCLTFLEN)                                            
PRout    LKOUT P,PCLTFIND,SETBITS                                               
         LKOUT C,205,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,206,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,207,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
CFRZDat  LKOUT C,215,PCLTFDTE,BMON,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYTRAF  LKOUT A,(D,B#CLT,PCLTELEM),EOT=EOR,ROWID=(PCLTTOEL,X'50'),    +        
               ROWWIDTH=(V,PCLTTOLN)                                            
CTrfOCd  LKOUT C,216,PCLTTOFC,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYPRF   LKOUT A,(R,NXTPRF),MULTIROW=Y,ROWNAME=DUMMY_D                          
         LKOUT C,001,(D,B#WORKD,FULL1),CHAR,LEN=2                               
         LKOUT C,002,(D,B#SAVED,SVREQTOK),CHAR                                  
         LKOUT C,011,(D,B#WORKD,WORK+00),CHAR,LEN=1                             
         LKOUT C,012,(D,B#WORKD,WORK+01),CHAR,LEN=1                             
         LKOUT C,013,(D,B#WORKD,WORK+02),CHAR,LEN=1                             
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1                             
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1                             
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1                             
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1                             
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1                             
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1                             
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1                             
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1                             
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1                             
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1                             
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1                             
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1                             
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1                             
         LKOUT E                                                                
                                                                                
TSTCLT   CLI   DUMMYCLI,YESQ       Test dummy client                            
         JNE   *+8                                                              
         LTR   RE,RE               Yes - don't send these                       
         BR    RE                                                               
         CLI   QCLTOPT,YESQ        Test downloading client details              
         BR    RE                                                               
                                                                                
TSTPRD   CLI   QPRDOPT,YESQ        Test downloading product details             
         BR    RE                                                               
                                                                                
TSTCOM   CLI   QANYREQ,YESQ        Test any request for this system             
         BNER  RE                                                               
         CLI   QCOMOPT,YESQ        Test downloading comment details             
         BR    RE                                                               
                                                                                
TSTDIV   CLI   QANYREQ,YESQ        Test any request for this system             
         BNER  RE                                                               
         CLI   QDIVOPT,YESQ        Test downloading division details            
         BR    RE                                                               
                                                                                
TSTOTH   CLI   QANYREQ,YESQ        Test any request for this system             
         BNER  RE                                                               
         CLI   QOTHOPT,YESQ        Test downloading Other Agency recs           
         BR    RE                                                               
                                                                                
TSTSYS   CLI   QANYREQ,YESQ        Test any request for this system             
         BR    RE                                                               
                                                                                
SETBITS  L     R1,LP_AINP                                                       
         LLC   RF,0(R1)                                                         
         SLL   RF,32-8                                                          
         LA    RE,WORK                                                          
         LHI   R0,8                                                             
SETBITS2 MVI   0(RE),C'N'                                                       
         TMH   RF,X'8000'                                                       
         JZ    *+8                                                              
         MVI   0(RE),C'Y'                                                       
         AHI   RE,1                                                             
         SLL   RF,1                                                             
         JCT   R0,SETBITS2                                                      
         J     EXITY                                                            
                                                                                
EDTCLN   LM    R2,R4,LP_AINP                                                    
         OC    0(L'PCLTNUM,R2),0(R2)                                            
         JZ    XCOLEN                                                           
         CLI   0(R2),X'FF'         Test client number is packed                 
         JNE   EDTCLN2                                                          
         UNPK  WORK(5),1(2,R2)                                                  
         MVC   0(4,R4),WORK                                                     
         LHI   R0,4                                                             
         J     SETOLENX                                                         
EDTCLN2  MVC   BYTE1,0(R2)                                                      
         NI    BYTE1,X'F0'                                                      
         CLI   BYTE1,X'80'         Test number is in binary format              
         JNE   EDTCLN4                                                          
         MVI   FULL1,0                                                          
         MVC   FULL1+1(3),0(R2)                                                 
         NI    FULL1+1,X'FF'-X'80'                                              
         L     R0,FULL1                                                         
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(5,R4),DUB                                                      
         LHI   R0,5                                                             
         J     SETOLENX                                                         
EDTCLN4  MVC   0(L'PCLTNUM,R4),0(R2)                                            
         LHI   R0,L'PCLTNUM                                                     
         J     SETOLENX                                                         
                                                                                
EDTCOF   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'PCLTOFF,R2),SPACES                                           
         JNH   XCOLEN                                                           
         XC    OFCBLK,OFCBLK       Init officer block                           
         MVI   OFCSYS,SYSPRTQ      Print system                                 
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,0(R2)                                                     
         GOTOR VOFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                        
         TM    OFCINDS,OFCINOLA    Using two chars office codes?                
         JNZ   EDTCOF6                                                          
         MVC   0(L'OFCOFC2,R4),OFCOFC2                                          
         LHI   R0,L'OFCOFC2                                                     
         J     SETOLENX                                                         
EDTCOF6  MVC   0(L'PCLTOFF,R4),0(R2)                                            
         LHI   R0,L'PCLTOFF                                                     
         J     SETOLENX                                                         
                                                                                
EDTMPS   LM    R2,R4,LP_AINP                                                    
         MVI   BYTE1,0             Init Main PST Code                           
         OC    0(L'PCLTMPSC,R2),0(R2)                                           
         JZ    XCOLEN                                                           
         LAY   R1,PSTVTAB                                                       
EDTMPS3  CLI   0(R1),X'00'         End of table?                                
         JE    XCOLEN                                                           
         CLI   0(R2),C' '                                                       
         JH    EDTMPS5                                                          
         LA    R1,2(R1)            Point to next province in table              
         LA    R2,1(R2)            Point to next province in data               
         J     EDTMPS3                                                          
EDTMPS5  MVC   0(2,R4),0(R1)                                                    
         LHI   R0,2                                                             
         MVC   BYTE1,0(R2)         Save provincial tax code                     
         J     SETOLENX                                                         
                                                                                
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
                                                                                
SETAAA   XC    WORK,WORK           Set prd AAA's name & address                 
         XC    WORK2,WORK2         Clear work areas                             
         L     R1,LP_AINP                                                       
         OC    0(L'PCLTKCLT,R1),0(R1)                                           
         JZ    XCOLEN                                                           
         USING EPWORKD,RC                                                       
         MVC   EPIOSAVE,IOVALS                                                  
K        USING PPRDKEY,IOKEY                                                    
         XC    K.PPRDKEY,K.PPRDKEY                                              
         MVC   K.PPRDKAGY,SVCLTKEY+(PCLTKAGY-PCLTKEY)                           
         MVC   K.PPRDKMED,SVCLTKEY+(PCLTKMED-PCLTKEY)                           
         MVI   K.PPRDKRCD,PPRDRECQ                                              
         MVC   K.PPRDKCLT,0(R1)                                                 
         MVC   K.PPRDKPRD,=C'AAA'                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   SETAAAN                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         JNE   SETAAAN                                                          
         L     R2,AIO4                                                          
         USING PPRDREC,R2                                                       
         MVC   WORK(20),PPRDBILL   Bill name                                    
         MVC   WORK+20(20),PPRDBIL2   Bill name 2                               
         MVC   WORK2(60),PPRDLIN1  Bill address lines 1 & 2                     
         MVC   WORK+40(24),PPRDATTN   Attention                                 
         MVC   IOVALS(IOVALL),EPIOSAVE                                          
         J     EXITY                                                            
                                                                                
SETAAAN  MVC   IOVALS(IOVALL),EPIOSAVE                                          
         J     XCOLEN                                                           
         DROP  R2,RC                                                            
                                                                                
***********************************************************************         
* Get profiles                                                        *         
***********************************************************************         
                                                                                
NXTPRF   XC    FULL1,FULL1         Init output profile name                     
         XC    WORK,WORK           Init output profile values                   
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRF06                                                         
         LA    RF,PROFTAB          Point to profile table                       
NXTPRF02 STCM  RF,15,PREVPROF                                                   
         J     NXTPRF12                                                         
                                                                                
NXTPRF06 ICM   RF,15,PREVPROF                                                   
         LA    RF,2(RF)            Next entry in table                          
         J     NXTPRF02                                                         
                                                                                
NXTPRF12 ICM   RF,15,PREVPROF      Get previous profile entry                   
         CLC   0(2,RF),=X'0000'    End of profile table?                        
         JE    NOMORE                                                           
                                                                                
         MVC   FULL1(2),0(RF)      Set output profile name                      
         XC    TEMP,TEMP                                                        
         MVI   TEMP+00,C'P'                                                     
         MVC   TEMP+02(2),0(RF)                                                 
         MVC   TEMP+04(L'QAGY),QAGY                                             
         MVC   TEMP+06(L'SVREQMED),SVREQMED                                     
         MVC   TEMP+07(L'SVREQCLT),SVREQCLT                                     
         CLI   SVREQCOF,C' '                                                    
         JNH   *+14                                                             
         MVI   TEMP+10,C'*'                                                     
         MVC   TEMP+11(1),SVREQCOF                                              
         GOTOR VGETPROF,DMCB,TEMP,WORK,VDATAMGR                                 
         MVC   LP_ADATA,AOFCREC                                                 
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Process first/next client array entry                               *         
***********************************************************************         
                                                                                
NXTCLT   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTCLT02                                                         
         SR    RE,RE                                                            
         ICM   RE,7,QACLT                                                       
         JZ    NOMORE                                                           
         MVC   NUMCLT,LW_NUMN-LW_D(RE)                                          
         AHI   RE,LW_LN2Q                                                       
         ST    RE,ANXTCLT          Set A(next client request)                   
         XC    LAGY,LAGY           Initialize agency control                    
                                                                                
NXTCLT02 SR    R0,R0                                                            
         ICM   R0,3,NUMCLT         R0=remaining client count                    
         JZ    NOMORE              Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMCLT                                                      
         L     R2,ANXTCLT                                                       
         USING CA_D,R2             R2=A(client row)                             
         MVC   SVREQTOK,CA_TOKEN   Set return token                             
         MVC   SVREQMED,CA_MEDC                                                 
         MVC   SVREQCLT,CA_CLTC                                                 
         XC    SVREQCOF,SVREQCOF                                                
         MVI   SVESTRSW,NOQ        Set to "no estimate rec" for client          
         LA    R0,CA_D+CA_LNQ                                                   
         ST    R0,ANXTCLT                                                       
         CLC   CA_SYS,SYSLET       Test for my system                           
         JNE   NXTCLT02                                                         
         MVI   QANYREQ,YESQ        Any request y/n switch for this sys          
         CLC   LAGY,CA_AGYA        Test change of agency                        
         JE    NXTCLT08                                                         
         MVC   LAGY,CA_AGYA        Set current agency code                      
                                                                                
         SR    R3,R3                                                            
         ICM   R3,7,QAAGY                                                       
         JNZ   *+6                                                              
         DC    H'0'                Agency definitions not passed                
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R3)                                            
         AHI   R3,LW_LN2Q                                                       
         USING AA_D,R3             Look up connection table entry               
                                                                                
NXTCLT04 CLC   AA_SYS,CA_SYS       Match system                                 
         JNE   *+14                                                             
         CLC   AA_OAGY,CA_AGYA     and agency alpha id                          
         JE    NXTCLT06                                                         
         AHI   R3,AA_LNQ           Bump to next entry                           
         JCT   R0,NXTCLT04                                                      
         DC    H'0'                System/agency not provided                   
                                                                                
NXTCLT06 TM    LP_FLAG,LP_FOFFL    Test offline                                 
         JZ    NXTCLT08                                                         
         GOTOR GETUID,DMCB,AA_USID,AA_PASS                                      
         JNE   NXTCLT02                                                         
                                                                                
         MVC   LP_ACCS,AA_ACCS     Set limit access                             
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,PRTDIR,(4,0),0                               
         GOTOR (RF),(R1),DMKEY,PRTFIL,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,PUBDIR,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,PUBFIL,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,GENDIR,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,GENFIL,(4,0),0                                   
                                                                                
NXTCLT08 MVI   DUMMYCLI,NOQ        Set not a dummy client request               
         MVI   MEDRSTR,C'A'        Media start range                            
         MVI   MEDREND,C'Z'        Media end range                              
         OC    CA_MEDC,SPACES                                                   
         CLI   CA_MEDC,C' '        ALL Media?                                   
         JNH   *+12                                                             
         CLI   CA_MEDC,C'*'        ALL Media?                                   
         JNE   *+18                                                             
NXTCLT10 MVI   DUMMYCLI,YESQ       Set dummy client request                     
         MVC   LP_ADATA,AIO1                                                    
         J     EXITY                                                            
                                                                                
         CLC   CA_MEDC,SPACES      Filtering on media code?                     
         JNH   *+16                                                             
         MVC   MEDRSTR,CA_MEDC                                                  
         MVC   MEDREND,CA_MEDC                                                  
                                                                                
         XC    CLTRSTR,CLTRSTR     Client start range                           
         MVC   CLTREND,=X'FFFFFF'  Client end range                             
         OC    CA_CLTC,SPACES                                                   
         CLC   =C'ALL',CA_CLTC     ALL Client?                                  
         JE    NXTCLT12                                                         
         CLC   CA_CLTC,SPACES      Filtering on client code?                    
         JNH   *+16                                                             
         MVC   CLTRSTR,CA_CLTC                                                  
         MVC   CLTREND,CA_CLTC                                                  
                                                                                
NXTCLT12 GOTOR (#GETCLT,AGETCLT),DMCB,CA_AGYA,CA_MEDC,CA_CLTC                   
         JNE   NXTCLT10                                                         
         L     RE,AIO1                                                          
         ST    RE,ACLTREC                                                       
         USING PCLTREC,RE                                                       
         MVC   SVREQMED,PCLTKMED                                                
         MVC   SVREQCLT,PCLTKCLT                                                
         MVC   SVREQCOF,PCLTOFF                                                 
         MVC   SVCLTKEY,0(RE)      Save key of client record                    
                                                                                
         CLI   PCLTPROF+05,C'2'    Sub client?                                  
         JE    NXTCLT16                                                         
         CLC   PCLTPROF+06(03),=C'000'                                          
         JNE   NXTCLT16            '000' maybe used as default value            
         XC    PCLTPROF+06(03),PCLTPROF+06                                      
         DROP  RE                                                               
                                                                                
NXTCLT16 XC    PRDRSTR,PRDRSTR     Product start range                          
         MVC   PRDREND,=X'FFFFFF'  Product end range                            
         OC    CA_PRDC,SPACES                                                   
         CLC   CA_PRDC,SPACES      Filtering on prduct code?                    
         JNH   *+16                                                             
         MVC   PRDRSTR,CA_PRDC                                                  
         MVC   PRDREND,CA_PRDC                                                  
                                                                                
         MVC   SVIOKEY_,IOKEY      Save key to read estimate records            
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'SVCLTKEY),SVCLTKEY                                       
         MVI   IOKEY+L'PESTKAGY+L'PESTKMED,PESTRECQ                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR'                                   
         CLC   IOKEY(PESTKPRD-PESTKEY),IOKEYSAV                                 
         JNE   *+8                                                              
         MVI   SVESTRSW,YESQ       Estimate records exist under client          
         MVC   IOKEY,SVIOKEY_      Restore key                                  
                                                                                
         MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
ARYPRD   LKOUT A,(R,NXTPRD),MULTIROW=Y,ROWNAME=PPRDRECD                         
PMedCod  LKOUT C,001,PPRDKMED,CHAR                                              
PCltCod  LKOUT C,002,PPRDKCLT,CHAR                                              
PPrdCod  LKOUT C,003,PPRDKPRD,CHAR                                              
PPrdNam  LKOUT C,004,PPRDNAME,CHAR                                              
PPrdBil  LKOUT C,005,PPRDBILL,CHAR,ND=Y                                         
PPrdLn1  LKOUT C,006,PPRDLIN1,CHAR,ND=Y                                         
PPrdLn2  LKOUT C,007,PPRDLIN2,CHAR,ND=Y                                         
PPrdAtt  LKOUT C,008,PPRDATTN,CHAR,ND=Y                                         
PPrdDiv  LKOUT C,009,PPRDDIV,CHAR,ND=Y                                          
PPrdAcc  LKOUT C,010,PPRDACCT,(R,EDTPAN),ND=Y                                   
PPrdExl  LKOUT C,011,PPRDEXCL,CHAR,ND=Y                                         
PPrdOAN  LKOUT C,012,PPRDOAN,CHAR,ND=Y                                          
PPrdBi2  LKOUT C,013,PPRDBIL2,CHAR,ND=Y                                         
PPrdGST  LKOUT C,014,PPRDGST,CHAR,ND=Y                                          
                                                                                
*PBILBSA  LKOUT C,020,PPRDBILP+00,LEN=1,UBIN,ND=Y                               
PBILBSA  LKOUT C,020,PPRDBILP+00,(R,EDTBBA),ND=Y                                
PBilBsB  LKOUT C,021,PPRDBILP+01,LEN=1,UBIN,ND=Y                                
PBilPAj  LKOUT C,022,PPRDBILP+00,(R,EDTPAJ),ND=Y                                
PAdjDat  LKOUT C,023,PPRDBILP+05,LEN=2,BMON,ND=Y                                
PColPrt  LKOUT C,024,PPRDBILP+09,LEN=1,CHAR,ND=Y                                
PBilFPc  LKOUT C,025,PPRDBILP+00,(R,EDTBFP),ND=Y                                
PBilCm1  LKOUT C,026,PPRDBILP+14,LEN=6,(R,EDTCM#),ND=Y                          
PBilCm2  LKOUT C,026,PPRDBILP+21,LEN=6,(R,EDTCM#),ND=Y                          
PBilCm3  LKOUT C,026,PPRDBILP+28,LEN=6,(R,EDTCM#),ND=Y                          
PComOnl  LKOUT C,027,PPRDBILP+34,LEN=1,CHAR,ND=Y                                
PN4BilS  LKOUT C,028,PPRDBILP+35,LEN=1,CHAR,ND=Y                                
PBilFCB  LKOUT C,029,PPRDBILP+36,LEN=1,UBIN,ND=Y                                
PBilCn1  LKOUT C,030,PPRDBILP+13,(R,EDTPBC),ND=Y                                
PBilCn2  LKOUT C,030,PPRDBILP+20,(R,EDTPBC),ND=Y                                
PBilCn3  LKOUT C,030,PPRDBILP+27,(R,EDTPBC),ND=Y                                
PBilTy1  LKOUT C,031,PPRDBILP+13,(R,EDTPBT),ND=Y                                
PBilTy2  LKOUT C,031,PPRDBILP+20,(R,EDTPBT),ND=Y                                
PBilTy3  LKOUT C,031,PPRDBILP+27,(R,EDTPBT),ND=Y                                
                                                                                
PRout    LKOUT P,PPRDEXC,SETBITS                                                
         LKOUT C,040,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,041,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,042,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,043,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,044,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
                                                                                
PRout    LKOUT P,PPRDSTAT,SETBITS                                               
         LKOUT C,050,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
                                                                                
PPrdOfC  LKOUT C,060,PPRDOFFC,CHAR,ND=Y                                         
PPrdTrf  LKOUT C,061,PPRDTRAF,CHAR,ND=Y                                         
                                                                                
Array    LKOUT C,062,(A,ARYPUDE)                                                
Array    LKOUT C,064,(A,ARYPPST)                                                
Array    LKOUT C,065,(A,ARYPINT)                                                
Array    LKOUT C,066,(A,ARYPAOC)                                                
Array    LKOUT C,068,(A,ARYPROT)                                                
Array    LKOUT C,069,(A,ARYPBPC)                                                
Array    LKOUT C,220,(A,ARYPPS_),PCVERSION=1.0.5.0                              
Array    LKOUT C,230,(A,ARYPMPS),PCVERSION=1.0.5.0                              
                                                                                
CPrdTok  LKOUT C,300,(D,B#SAVED,SVREQTOK),CHAR                                  
         LKOUT E                                                                
                                                                                
ARYPUDE  LKOUT A,(D,B#PRD,PPRDELEM),EOT=EOR,ROWID=(PPRDUDEF,X'08'),    +        
               ROWWIDTH=(V,PPRDUDEF+1)                                          
PPrUsr1  LKOUT C,062,PUSER1,CHAR,ND=Y                                           
PPrUsr2  LKOUT C,063,PUSER2,CHAR,ND=Y                                           
         LKOUT E                                                                
                                                                                
ARYPPST  LKOUT A,(D,B#PRD,PPRDELEM),EOT=EOR,ROWID=(PPRDPST,X'25'),     +        
               ROWWIDTH=(V,PPRDPST+1)                                           
PPrdPST  LKOUT C,064,PPRDPSTC,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYPPS_  LKOUT A,(D,B#PRD,PPRDELEM),EOT=EOR,ROWID=(PPRDPST,X'25'),     +        
               ROWWIDTH=(V,PPRDPST+1)                                           
PCanPST  LKOUT C,220,PPRDPSTC+00,CHAR,LEN=1,ND=Y                                
         LKOUT C,221,PPRDPSTC+01,CHAR,LEN=1,ND=Y                                
         LKOUT C,222,PPRDPSTC+02,CHAR,LEN=1,ND=Y                                
         LKOUT C,223,PPRDPSTC+03,CHAR,LEN=1,ND=Y                                
         LKOUT C,224,PPRDPSTC+04,CHAR,LEN=1,ND=Y                                
         LKOUT C,225,PPRDPSTC+05,CHAR,LEN=1,ND=Y                                
         LKOUT C,226,PPRDPSTC+06,CHAR,LEN=1,ND=Y                                
         LKOUT C,227,PPRDPSTC+07,CHAR,LEN=1,ND=Y                                
         LKOUT C,228,PPRDPSTC+08,CHAR,LEN=1,ND=Y                                
         LKOUT C,229,PPRDPSTC+09,CHAR,LEN=1,ND=Y                                
         LKOUT E                                                                
                                                                                
ARYPMPS  LKOUT A,(D,B#PRD,PPRDELEM),EOT=EOR,ROWID=(PPRDMPS,PPRDMPEQ),  +        
               ROWWIDTH=(V,PPRDMPSL)                                            
PCanMPS  LKOUT C,230,PPRDMPSC,(R,EDTMPS),ND=Y                                   
         LKOUT C,231,(D,B#WORKD,BYTE1),CHAR,LEN=1,ND=Y                          
         LKOUT E                                                                
                                                                                
ARYPINT  LKOUT A,(D,B#PRD,PPRDELEM),EOT=EOR,ROWID=(PPRDICEL,X'30'),    +        
               ROWWIDTH=(V,PPRDICEL+1)                                          
PPrdInt  LKOUT C,065,PPRDINFC,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYPAOC  LKOUT A,(D,B#PRD,PPRDELEM),EOT=EOR,ROWID=(PPRDAOEL,X'35'),    +        
               ROWWIDTH=(V,PPRDAOEL+1)                                          
PPrAccA  LKOUT C,066,PPRDACCA,CHAR,ND=Y                                         
PPrAOfC  LKOUT C,067,PPRDAOFC,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYPROT  LKOUT A,(D,B#PRD,PPRDELEM),EOT=EOR,ROWID=(PPRDLWEL,X'40'),    +        
               ROWWIDTH=(V,PPRDLWEL+1)                                          
PPrRotO  LKOUT C,068,PPRDROTA,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYPBPC  LKOUT A,(D,B#PRD,PPRDELEM),EOT=EOR,ROWID=(PPBPCELC,PPBPCECQ), +        
               ROWWIDTH=(V,PPBPCELN)                                            
PPrBEfD  LKOUT C,069,PPBPCEFF,BMON,ND=Y                                         
PPrBPid  LKOUT C,070,PPBPCPID,(R,EDTPID),ND=Y                                   
PPrBPCh  LKOUT C,071,PPBPCCHG,BDAT,ND=Y                                         
         LKOUT E                                                                
                                                                                
                                                                                
EDTPAN   LM    R2,R4,LP_AINP                                                    
         OC    0(L'PPRDACCT,R2),0(R2)                                           
         JZ    XCOLEN                                                           
         CLI   0(R2),X'FF'         Product account number is numeric?           
         JE    EDTPAN2                                                          
         MVC   0(L'PPRDACCT,R4),0(R2)                                           
         LHI   R0,L'PPRDACCT                                                    
         J     SETOLENX                                                         
EDTPAN2  MVC   FULL1(L'PPRDACCT),0(R2)                                          
         MVI   FULL1,0                                                          
         L     R0,FULL1                                                         
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(5,R4),DUB+5(3)                                                 
         LHI   R0,5                                                             
         J     SETOLENX                                                         
                                                                                
         USING BILPROF,R2                                                       
EDTPAJ   LM    R2,R4,LP_AINP                                                    
         CLI   BILBASA,0           Have formula?                                
         JE    XCOLEN                                                           
*        OC    BILADJ,BILADJ       Have Pct Adjustment?                         
*        JNZ   EDTPAJ4                                                          
*        MVC   0(2,R4),=C'+0'                                                   
*        LHI   R0,2                                                             
*        J     SETOLENX                                                         
EDTPAJ4  CURED (B3,BILADJ),(8,0(R4)),0,ALIGN=LEFT,FLOAT=-                       
         J     SETOLENX                                                         
         DROP  R2                                                               
                                                                                
         USING BILPROF,R2                                                       
EDTBFP   LM    R2,R4,LP_AINP                                                    
         CLI   BILPBASB,0          Have formula?                                
         JE    XCOLEN                                                           
*        OC    BILPADJ,BILPADJ     Have Pct Adjustment?                         
*        JNZ   EDTBFP4                                                          
*        MVC   0(2,R4),=C'+0'                                                   
*        LHI   R0,2                                                             
*        J     SETOLENX                                                         
EDTBFP4  CURED (B3,BILPADJ),(8,0(R4)),0,ALIGN=LEFT,FLOAT=-                      
         J     SETOLENX                                                         
         DROP  R2                                                               
                                                                                
         USING BILPROF,R2                                                       
EDTBBA   LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             Any control bits on?                         
         JZ    XCOLEN                                                           
*                                                                               
         CLI   BILCMSW,C'C'        COMMISSION ONLY?                             
         JNE   *+12                                                             
         OI    BILBASA,X'10'       YES                                          
         MVI   BILCMSW,0                                                        
*                                                                               
         CLI   BILNBSW,C'Y'        NOT FOR BILLING OPTION?                      
         JNE   *+12                                                             
         OI    BILBASA,X'20'       YES                                          
         MVI   BILNBSW,0                                                        
*                                                                               
         LLC   R0,BILBASA                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         LHI   R0,3                                                             
         J     SETOLENX                                                         
                                                                                
EDTPBC   LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             ANY CONTROL BITS ON?                         
         JZ    XCOLEN                                                           
         SR    R0,R0                                                            
         TM    0(R2),X'80'         REGULAR?                                     
         JZ    *+16                                                             
         MVI   0(R4),C'R'                                                       
         LA    R4,1(R4)                                                         
         AHI   R0,1                                                             
         TM    0(R2),X'40'         CASH DISCOUNT?                               
         JZ    *+16                                                             
         MVI   0(R4),C'C'                                                       
         LA    R4,1(R4)                                                         
         AHI   R0,1                                                             
         TM    0(R2),X'20'         ADJUSTMENT?                                  
         JZ    *+16                                                             
         MVI   0(R4),C'A'                                                       
         LA    R4,1(R4)                                                         
         AHI   R0,1                                                             
         CHI   R0,0                ANY CONTROL DATA?                            
         JE    XCOLEN                                                           
         J     SETOLENX                                                         
                                                                                
EDTPBT   LM    R2,R4,LP_AINP                                                    
         TM    0(R2),X'EE'         No comment and bill type?                    
         JZ    XCOLEN                                                           
         TM    0(R2),X'0F'                                                      
         JZ    EDTPBT2                                                          
         SR    R0,R0                                                            
         TM    0(R2),X'01'                                                      
         JO    *+16                                                             
         MVI   0(R4),C'4'                                                       
         LA    R4,1(R4)                                                         
         AHI   R0,1                                                             
         TM    0(R2),X'02'                                                      
         JO    *+16                                                             
         MVI   0(R4),C'5'                                                       
         LA    R4,1(R4)                                                         
         AHI   R0,1                                                             
         TM    0(R2),X'04'                                                      
         JO    *+16                                                             
         MVI   0(R4),C'6'                                                       
         LA    R4,1(R4)                                                         
         AHI   R0,1                                                             
         TM    0(R2),X'08'                                                      
         JO    *+16                                                             
         MVI   0(R4),C'7'                                                       
         LA    R4,1(R4)                                                         
         AHI   R0,1                                                             
         J     EDTPBT4                                                          
*                                                                               
EDTPBT2  MVC   0(3,R4),=C'ALL'                                                  
         LHI   R0,3                                                             
EDTPBT4  CHI   R0,0                Any control data?                            
         JE    XCOLEN                                                           
         J     SETOLENX                                                         
                                                                                
                                                                                
**********************************************************************          
* Edit person id                                                     *          
**********************************************************************          
                                                                                
         USING EPWORKD,RC                                                       
         USING SA0REC,EPIO                                                      
EDTPID   MVC   EPIOSAVE,IOVALS     Save current i/o values                      
         LHI   R0,L'SAPALPID                                                    
         ST    R0,LP_OLEN                                                       
         L     R1,LP_AINP          R1=A(Password Number)                        
         OC    0(L'SA0KNUM,R1),0(R1)                                            
         JZ    EDTPIDN                                                          
                                                                                
         XC    SA0KEY,SA0KEY       Read person password record                  
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KNUM,0(R1)                                                    
         L     R1,LP_ASECD                                                      
         MVC   SA0KAGY,SECAGY-SECD(R1)                                          
         MVC   IOKEY,SA0KEY                                                     
         LA    R1,EPIO                                                          
         ST    R1,IOADDR                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE'                                
         JNE   EDTPIDN                                                          
                                                                                
         LA    R1,SA0DATA                                                       
         USING SAPALD,R1                                                        
EDTPID02 CLI   SAPALEL,EOR         Test end of record                           
         JE    EDTPIDN                                                          
         CLI   SAPALEL,SAPALELQ    Test new security person element             
         JE    EDTPID04                                                         
         LLC   R0,SAPALLN          Bump to next element                         
         AR    R1,R0                                                            
         J     EDTPID02                                                         
                                                                                
EDTPID04 L     RF,LP_AOUT                                                       
         MVC   0(L'SAPALPID,RF),SAPALPID                                        
         J     EDTPIDY                                                          
         DROP  R1                                                               
                                                                                
EDTPIDN  MVC   IOVALS(IOVALL),EPIOSAVE                                          
         J     XCOLEN                                                           
                                                                                
EDTPIDY  MVC   IOVALS(IOVALL),EPIOSAVE                                          
         J     EXITY                                                            
         DROP  RC                                                               
                                                                                
EPWORKD  DSECT ,                   ** EDTPID local working storage **           
EPIOSAVE DS    XL(IOVALL)          Saved i/o values                             
EPIO     DS    XL1000              I/O area                                     
SVRDEF   CSECT ,                                                                
                                                                                
***********************************************************************         
* Read Product records for current client                             *         
***********************************************************************         
                                                                                
NXTPRD   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',PRDKEYT),                +        
               ('B#PRD',0),SAVED,0,0                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
                                                                                
ARYCOM   LKOUT A,(R,NXTCOM),MULTIROW=Y,ROWNAME=PCOMRECD                         
PMedCod  LKOUT C,001,PCOMKMED,CHAR                                              
PComNum  LKOUT C,002,PCOMKNUM,(R,EDTCM#)                                        
PComTok  LKOUT C,300,(D,B#SAVED,SVREQTOK),CHAR                                  
         LKOUT E                                                                
                                                                                
EDTCM#   LM    R2,R4,LP_AINP                                                    
         OC    0(L'PCOMKNUM,R2),0(R2)                                           
         JZ    XCOLEN                                                           
         CLC   0(L'PCOMKNUM,R2),SPACES                                          
         JNH   XCOLEN                                                           
         MVC   0(L'PCOMKNUM,R4),0(R2)                                           
         LHI   R0,L'PCOMKNUM                                                    
EDTCM#2  CLI   0(R4),C' '          Need to left justify?                        
         JH    SETOLENX                                                         
         MVC   0(L'PCOMKNUM-1,R4),1(R4)                                         
         MVI   0+L'PCOMKNUM-1(R4),C' '                                          
         J     EDTCM#2             Overlapping move loop                        
                                                                                
***********************************************************************         
* Read Comment records for current media                              *         
***********************************************************************         
                                                                                
NXTCOM   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',COMKEYT),                +        
               ('B#COM',0),SAVED,0,0                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
                                                                                
ARYDIV   LKOUT A,(R,NXTDIV),MULTIROW=Y,ROWNAME=PDIVRECD                         
PMedCod  LKOUT C,001,PDIVKMED,CHAR                                              
PCltCod  LKOUT C,002,PDIVKCLT,CHAR                                              
PDivNum  LKOUT C,003,PDIVKDIV,CHAR                                              
PDivNam  LKOUT C,004,PDIVNAME,CHAR                                              
PDivTok  LKOUT C,300,(D,B#SAVED,SVREQTOK),CHAR                                  
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read Division records for current media                             *         
***********************************************************************         
                                                                                
NXTDIV   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',DIVKEYT),                +        
               ('B#DIV',0),SAVED,0,0                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
                                                                                
ARYOTH   LKOUT A,(R,NXTOTH),MULTIROW=Y,ROWNAME=POTHRECD                         
PMedCod  LKOUT C,001,POTHKMED,CHAR                                              
POthAgC  LKOUT C,002,POTHCODE,CHAR                                              
POthNam  LKOUT C,003,POTHNAME,CHAR                                              
POthTok  LKOUT C,300,(D,B#SAVED,SVREQTOK),CHAR                                  
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read Other Agency records for current media                         *         
***********************************************************************         
                                                                                
NXTOTH   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',OTHKEYT),                +        
               ('B#OTH',0),SAVED,0,0                                            
         J     EXITY                                                            
                                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
                                                                                
REQEND   LKREQ X                                                                
         EJECT                                                                  
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   Set no more data                             
         J     EXITY                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     Clear output field length & exit             
         J     EXITY                                                            
                                                                                
SETOLENX STCM  R0,15,LP_OLEN       Set output field length & exit               
         J     EXITY                                                            
                                                                                
EXITN    LHI   RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
GLOBALS  DS    0D                  ** Global literals **                        
         LTORG                                                                  
                                                                                
ONEOFCTB DC    C'ABCDEFGH'                                                      
         DC    C'IJKLMNOP'                                                      
         DC    C'QRSTUVWX'                                                      
         DC    C'YZ123456'         0 is removed                                 
         DC    C'789'                                                           
         DC    C'<>?/`~!@'         = - , .  not allowed                         
         DC    C'#$%^*()_'                                                      
         DC    C'+{}º|\"'                                                      
         DC    C'&&'                                                            
         DC    C''''                                                            
         DC    X'FF'                                                            
ONEOFCLN EQU   *-ONEOFCTB                                                       
                                                                                
                                                                                
PROFTAB  DC    C'F0'               Profile table                                
         DC    X'0000'                                                          
                                                                                
PZERO    DC    P'0'                                                             
                                                                                
PRDKEYT  LKKEY H,PPRDKEY,SAVED     ** Product key driver **                     
         LKKEY SIN,PPRDKAGY,SVCLTKEY+(PCLTKAGY-PCLTKEY)                         
         LKKEY SIN,PPRDKMED,SVCLTKEY+(PCLTKMED-PCLTKEY)                         
         LKKEY LIT,PPRDKRCD,PPRDRECQ                                            
         LKKEY SIN,PPRDKCLT,SVCLTKEY+(PCLTKCLT-PCLTKEY)                         
         LKKEY RNG,PPRDKPRD,PRDRNGE                                             
         LKKEY E                                                                
                                                                                
COMKEYT  LKKEY H,PCOMKEY,SAVED     ** Comment key driver **                     
         LKKEY SIN,PCOMKAGY,QAGY                                                
         LKKEY RNG,PCOMKMED,MEDRNGE                                             
         LKKEY LIT,PCOMKRCD,PCOMRECQ                                            
         LKKEY ALL,PCOMKNUM                                                     
         LKKEY E                                                                
                                                                                
AGYKEYT  LKKEY H,PAGYKEY,SAVED     ** Agency key driver **                      
         LKKEY SIN,PAGYKAGY,QAGY                                                
         LKKEY RNG,PAGYKMED,MEDRNGE                                             
         LKKEY LIT,PAGYKRCD,PAGYKIDQ                                            
         LKKEY E                                                                
                                                                                
DIVKEYT  LKKEY H,PDIVKEY,SAVED     ** Division key driver **                    
         LKKEY SIN,PDIVKAGY,QAGY                                                
         LKKEY RNG,PDIVKMED,MEDRNGE                                             
         LKKEY LIT,PDIVKRCD,PDIVRECQ                                            
         LKKEY RNG,PDIVKCLT,CLTRNGE                                             
         LKKEY RNG,PDIVKDIV,DIVRNGE                                             
         LKKEY E                                                                
                                                                                
OTHKEYT  LKKEY H,POTHKEY,SAVED     ** Other Agency key driver **                
         LKKEY SIN,POTHKAGY,QAGY                                                
         LKKEY RNG,POTHKMED,MEDRNGE                                             
         LKKEY LIT,POTHKRCD,POTHRECQ                                            
         LKKEY ALL,POTHCODE                                                     
         LKKEY E                                                                
                                                                                
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
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
                                                                                
NOQ      EQU   C'N'                                                             
YESQ     EQU   C'Y'                                                             
EOR      EQU   0                   End of record element code                   
EFF      EQU   X'FF'               End of table marker                          
                                                                                
SAVED    DSECT ,                   ** Dsect to cover saved storage **           
                                                                                
GETUID   DS    A                   A(GETUID)                                    
VRECUP   DS    V                                                                
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK calling mode                          
                                                                                
SYSLET   DS    C                   ** System letter **                          
SYSPRTQ  EQU   C'P'                Print system letter                          
                                                                                
OFCBLK   DS    XL(OFCLENQ)         OFFICER block                                
                                                                                
QVALUES  DS    0F                  ** Request values **                         
                                                                                
                                                                                
QAGY     DS    CL(L'LP_AGY)                                                     
QAGYIND  DS    X                   Agency array                                 
QAAGY    DS    AL3                                                              
                                                                                
QCLTIND  DS    X                   Client request array                         
QACLT    DS    AL3                                                              
                                                                                
QONEOFC  DS    C                   Agy using ONE char media office?             
QANYREQ  DS    C                   Any request processed for this sys?          
QCLTOPT  DS    C                   Download client? Y/N option                  
QPRDOPT  DS    C                   Download product? Y/N option                 
QCOMOPT  DS    C                   Download comment? Y/N option                 
QDIVOPT  DS    C                   Download divisions? Y/N option               
QOTHOPT  DS    C                   Download Other Agency? Y/N option            
QOPTION  DS    C                   Download options not used in Print           
                                                                                
QVALUEL  EQU   *-QVALUES                                                        
                                                                                
ENVIRO   DS    X                   Enviroment to use                            
LAGY     DS    CL(L'CA_AGYA)       Last (previous) agency                       
ANXTCLT  DS    A                   A(next client code in work map pool)         
NUMCLT   DS    XL(L'LW_NUMN)       N'client codes left to process               
                                                                                
DUMMYCLI DS    C                   Current client is 'dummy'                    
SVIOKEY_ DS    XL(L'IOKEY)                                                      
SVCLTKEY DS    CL(L'PCLTKEY)       Client record key                            
SVREQTOK DS    CL(L'CA_TOKEN)      Request token                                
SVREQMED DS    CL(L'PCLTKMED)      Request media code                           
SVREQCLT DS    CL(L'PCLTKCLT)      Request client code                          
SVREQCOF DS    CL(L'PCLTOFF)       Request client office code                   
SVESTRSW DS    C                   Estimate record Y/N switch                   
PREVPROF DS    XL4                 Previous profile value in table              
                                                                                
MEDRNGE  DS    0XL(2*1)            ** Media key reading range **                
MEDRSTR  DS    XL1                 Start of range                               
MEDREND  DS    XL1                 End of range                                 
                                                                                
CLTRNGE  DS    0XL(3*2)            ** Client key reading range **               
CLTRSTR  DS    XL3                 Start of range                               
CLTREND  DS    XL3                 End of range                                 
                                                                                
PRDRNGE  DS    0XL(3*2)            ** Product key reading range **              
PRDRSTR  DS    XL3                 Start of range                               
PRDREND  DS    XL3                 End of range                                 
                                                                                
DIVRNGE  DS    0XL(2*L'PDIVKDIV)   ** Division key reading range **             
DIVRSTR  DS    CL(L'PDIVKDIV)      Start of range                               
DIVREND  DS    CL(L'PDIVKDIV)      End of range                                 
                                                                                
DUMMY_D  DSECT                                                                  
DUM_LIN1 DS    CL1                                                              
                                                                                
* Other included books follow                                                   
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
PCOMRECD DSECT                                                                  
PCOMRECQ EQU   X'40'                                                            
       ++INCLUDE PCOMREC                                                        
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
PDIVRECD DSECT                                                                  
PDIVRECQ EQU   X'03'                                                            
       ++INCLUDE PDIVREC                                                        
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
POTHRECD DSECT                                                                  
POTHRECQ EQU   X'16'                                                            
       ++INCLUDE POTHAGY                                                        
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
PBILPRFD DSECT                                                                  
       ++INCLUDE PBILPROF                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE GEGENOFF                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE GEMAPEQUS                                                      
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE GEGENSDR                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008PPLNK09   01/23/13'                                      
         END                                                                    
