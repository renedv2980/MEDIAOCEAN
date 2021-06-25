*          DATA SET NENAV16    AT LEVEL 030 AS OF 03/01/18                      
*PHASE T31816B                                                                  
NENAV16  TITLE 'T31816 - Netpak CFM downloads'                                  
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,REQUEST=*,CODE=CODE,SYSTEM=NETSYSQ,FILES=FILES,  +        
               LOADFACSOFF=Y,SERVERTYPE=TSTSTEW,IDF=Y,WORKERKEY=NECF,  +        
               SYSPHASE=SYSPHASE,SEGMENT=Y,APPEND=Y,                   +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#SECD,SECD,        +        
               B#LPD,LP_D,B#CLTREC,CLTRECD,B#SDRREC,SDRRECD,           +        
               B#PRDREC,PRDRECD,B#AGYREC,AGYHDR,B#OFC,MOFRECD,         +        
               B#STAREC,STARECD,B#TCMREC,CMLRECD,B#TPHREC,PRHRECD,     +        
               B#TNRREC,NRCPRECD)                                               
                                                                                
B#SECD   EQU   3                   SECD                                         
                                                                                
B#LPD    EQU   4                   LP_D                                         
                                                                                
B#CLTREC EQU   5                   IO2 - PCLTRECD                               
B#SDRREC EQU   5                         SELF DEFINING RECORD                   
                                                                                
B#PRDREC EQU   6                   IO3 - PPRDRECD                               
B#AGYREC EQU   6                         AGENCY RECORD                          
B#STAREC EQU   6                         STATION RECORD                         
B#TPHREC EQU   6                         TRAFFIC PRODUCTION HOUSE REC           
B#TCMREC EQU   6                         TRAFFIC COMMERCIAL REC                 
B#TNRREC EQU   6                         TRAFFIC NET RECAP REC                  
ATCMREC  EQU   AIO3                                                             
ARCPREC  EQU   AIO3                Traffic recap record                         
                                                                                
B#OFC    EQU   6                   Office record                                
AOFCREC  EQU   AIO1                                                             
                                                                                
CODE     NMOD1 0,**NN16**                                                       
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
         LAY   R8,WORKD+(((WORKLOFF+7)/8)*8)                                    
         USING SAVED,R8            R8=A(save w/s)                               
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
INIT04   MVC   GETUID,RGETUID      Set A(GETUID)                                
         MVC   RUNMODE,RUNPMODE    Extract DDLINK calling mode                  
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC       A(MASTER)                                    
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
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)                                                    
         MVC   LP_AUIR1,AROUTS1    Set A(index routines 1)                      
         GOTOR (RF),DMCB,('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)                                                    
         MVC   LP_AUIR2,AROUTS2    Set A(index routines 2)                      
                                                                                
         GOTOR (#WRKINI,AWRKINI)   Initialize working storage                   
                                                                                
RUNSTR02 MVC   LP_BLKS+((B#SECD-1)*L'LP_BLKS),LP_ASECD                          
         MVC   LP_BLKS+((B#LPD-01)*L'LP_BLKS),ALP                               
         MVC   LP_BLKS+((B#AGYREC-01)*L'LP_BLKS),AIO1                           
         MVC   LP_BLKS+((B#CLTREC-01)*L'LP_BLKS),AIO2                           
         MVC   LP_BLKS+((B#PRDREC-01)*L'LP_BLKS)(AIOLAST-AIO3),AIO3             
                                                                                
         CLC   LP_QMAPN,=AL2(I#CFMIDL)                                          
         JNE   RUNSTR04                                                         
         GOTOR (#GETAGY,AGETAGY),LP_AGY                                         
         MVC   LP_BLKS+((B#AGYREC-01)*L'LP_BLKS),AIO1                           
                                                                                
RUNSTR04 DS    0H                                                               
         MVC   WVALUES(WVALUEL),LVALUES                                         
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
         MVI   SYSLET,NETLETQ                                                   
         MVC   QAGY,LP_AGY                                                      
         XC    PRDRSTR,PRDRSTR     Product start range                          
         MVC   PRDREND,=X'FFFFFF'  Product end range                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Run a download request                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    Test 'run request' mode                      
         JNE   EXITY                                                            
         GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* CFM initial download (X'FFF1')                                      *         
***********************************************************************         
                                                                                
REQCFMI  LKREQ *,I#CFMIDL,OUTCFMI,NEXTREQ=REQCFMC                               
                                                                                
OUTCFMI  LKOUT H                                                                
CFMI     LKOUT R,X'0001'                                                        
LimAcs   LKOUT C,001,(D,B#LPD,LP_ACCS),HEXD                                     
UserId   LKOUT C,002,(D,B#LPD,LP_USRID),HEXD                                    
PIDNum   LKOUT C,003,(D,B#SECD,SECOPASS),HEXD                                   
AccGrp   LKOUT C,004,(D,B#SECD,SECOSAGN),HEXD                                   
PerAgy   LKOUT C,005,(D,B#SECD,SECOAGPE),CHAR                                   
SecSys   LKOUT C,006,(D,B#SECD,SECOSYS),HEXD                                    
SecPrg   LKOUT C,007,(D,B#SECD,SECOPRG),HEXD                                    
SysLet   LKOUT C,008,(D,B#SAVED,SYSLET),CHAR                                    
         LKOUT E                                                                
                                                                                
AGYR     LKOUT R,X'0031'                                                        
Array    LKOUT C,10,(A,ARYAGY)                                                  
Array    LKOUT C,40,(A,ARYAMD)                                                  
Array    LKOUT C,60,(A,ARYAAG)                                                  
         LKOUT E                                                                
                                                                                
OFC2     LKOUT R,X'0032'                                                        
Array    LKOUT C,X'0032',(A,ARY2OF)                                             
         LKOUT E                                                                
                                                                                
OFC1     LKOUT R,X'0032'                                                        
Array    LKOUT C,X'0032',(A,ARY1OF),FILTROUT=TST1OF                             
         LKOUT E                                                                
                                                                                
SDREC    LKOUT R,X'0036'                                                        
PRout    LKOUT P,,GETSDR                                                        
Array    LKOUT C,X'0036',(A,ARYSDR)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYAGY   LKOUT A,(D,B#AGYREC,AGYEL),EOT=EOR,ROWID=(AGYEL,AGYELQ),      +        
               ROWWIDTH=(V,AGYELEN)                                             
         LKOUT C,010,AGYPRATS,CHAR                                              
         LKOUT C,011,AGYPCLIR,CHAR                                              
         LKOUT C,012,AGYPBPCT,CHAR                                              
         LKOUT C,013,AGYPEDEM,CHAR                                              
         LKOUT C,014,AGYPBOTO,CHAR                                              
         LKOUT C,015,AGYPSAUT,CHAR                                              
         LKOUT C,016,AGYPCNDA,CHAR                                              
         LKOUT C,017,AGYPOPTS,CHAR                                              
         LKOUT C,018,AGYPBREQ,CHAR                                              
         LKOUT C,019,AGYPCBLM,CHAR                                              
         LKOUT C,020,AGYPBPLR,CHAR                                              
         LKOUT C,021,AGYPMGMM,CHAR                                              
         LKOUT C,022,AGYPOREQ,CHAR                                              
         LKOUT C,023,AGYPBYBR,CHAR                                              
         LKOUT C,024,AGYPSOCD,CHAR                                              
         LKOUT C,025,AGYPBLNG,CHAR                                              
         LKOUT C,026,AGYPALPH,CHAR                                              
         LKOUT C,027,AGYPAHEX,CHAR                                              
         LKOUT C,028,AGYOFC2,CHAR                                               
                                                                                
PRout    LKOUT P,AGYFLAG1,SETBITS                                               
         LKOUT C,030,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,031,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,032,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,033,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,034,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,035,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,036,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,037,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
PRout    LKOUT P,AGYFLAG2,SETBITS                                               
         LKOUT C,050,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,051,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,052,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,053,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,054,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,055,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,056,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,057,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYAMD   LKOUT A,(D,B#AGYREC,AGYEL),EOT=EOR,ROWID=(AGYMEDEL,AGYMEDEQ), +        
               ROWWIDTH=(V,AGYMEDLN)                                            
MedCd    LKOUT C,040,AGYMEDCD,CHAR,FILTROUT=TSTMED_N,SKIPCOLS=AMDSKIPN          
AMDSKIPS DS    0X                  Start of columns to skip                     
MedNm    LKOUT C,041,AGYMEDEX,CHAR                                              
Vendr    LKOUT C,042,AGYVENEX,CHAR                                              
AMDSKIPN EQU   (*-AMDSKIPS)/LX_COLSL                                            
         LKOUT E                                                                
                                                                                
ARYAAG   LKOUT A,(D,B#AGYREC,AGYEL),EOT=EOR,ROWID=(AGYACCEL,AGYACCEQ), +        
               ROWWIDTH=(V,AGYACCLN)                                            
AAgyL    LKOUT C,060,AGYACCAG,CHAR,LEN=V                                        
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
                                                                                
TSTMED_N L     R1,LP_AINP                                                       
         USING AGYMEDEL,R1                                                      
         CLI   AGYMEDCD,NETMEDQ                                                 
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
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
         L     RE,AIO1                                                          
         ST    RE,AOFCREC                                                       
         LA    RE,IOKEY                                                         
         USING MOFKEY,RE                                                        
         MVI   MOFKTYP,MOFKTYPQ                                                 
         MVI   MOFKSUB,MOFKS2Q                                                  
         MVC   MOFKAGY,QAGY                                                     
         DROP  RE                                                               
NXT2OF05 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGENDIR+IO1'                            
         JNE   NOMORE                                                           
NXT2OF07 LA    RE,IOKEY                                                         
         USING MOFKEY,RE                                                        
         CLI   MOFKTYP,MOFKTYPQ    Office limit access record?                  
         JNE   NOMORE                                                           
         CLC   MOFKAGY,QAGY        Same request agency?                         
         JNE   NOMORE                                                           
         CLI   MOFKSUB,MOFKS2Q     Two bytes office code?                       
         JNE   *+12                                                             
         CLI   MOFKSYS,SPTSYSQ     Spot system?                                 
         JE    NXT2OF20            NOTE: Net uses Spot's MOFFICE list           
         DROP  RE                                                               
NXT2OF10 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOGENDIR+IO1'                            
         JE    NXT2OF07                                                         
         J     NOMORE                                                           
NXT2OF20 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGENFIL+IO1'                           
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
                                                                                
         EJECT                                                                  
***********************************************************************         
* GET TOKEN RECORD FOR NIELSEN                                        *         
***********************************************************************         
                                                                                
GETTOKN  XC    TOKEN,TOKEN                                                      
         MVC   TOKEN(11),=C'NSI|ACCESS|'                                        
         J     EXITCC                                                           
                                                                                
***********************************************************************         
* GET TOKEN RECORD                                                    *         
***********************************************************************         
                                                                                
GETTOK   XC    TOKEN,TOKEN                                                      
         MVC   TOKEN(5),=C'COM||'                                               
         L     RF,LP_ASECD                                                      
         USING SECD,RF                                                          
         OC    SECOAGYS,SECOAGYS                                                
         JZ    GETTOK10                                                         
         MVC   HALF,SECOAGYS       SECURITY AGENCY CODE                         
         J     GETTOK20                                                         
*                                                                               
GETTOK10 GOTOR VGETFACT,DMCB,0                                                  
         L     RF,DMCB                                                          
         USING FACTSD,RF                                                        
         MVC   HALF,FATAGYSC       SECURITY AGENCY CODE                         
                                                                                
GETTOK20 XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    RE,IOKEY                                                         
         USING TOKKEY,RE                                                        
         MVI   TOKKMIN,TOKKMINQ      C'K'                                       
         MVI   TOKKTYP,TOKKRTRK      X'01' - RENTRAK RECORD                     
         MVC   TOKKAAGY,LP_AGY       AGENCY ALPHA CODE                          
         MVC   TOKKSAGY,HALF         SECURITY AGENCY CODE                       
         MVI   TOKKSYS,X'03'         NET SYSTEM                                 
         DROP  RE,RF                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGENDIR+IO1'                            
         JNE   GETTOKX                                                          
         CLC   IOKEY(L'TOKKEY),IOKEYSAV                                         
         JNE   GETTOKX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGENFIL+IO1'                           
         JNE   GETTOKX                                                          
                                                                                
         L     R2,AIO1                                                          
         AHI   R2,TOKFIRST             R4=A(1ST ELEMENT)                        
                                                                                
GETTOK30 CLI   0(R2),0                 ANY ELEMENTS?                            
         JE    GETTOKX                                                          
         CLI   0(R2),RTAUTELQ          X'0A' - RENTRAK AUTHOR ELEM?             
         JE    GETTOK40                                                         
         LLC   R0,1(R2)                CHECK THE NEXT ELEMENT                   
         AR    R2,R0                                                            
         J     GETTOK30                                                         
                                                                                
         USING RTAUTHD,R2                                                       
GETTOK40 CLC   RTAUTID,SPACES          LICENSE ID BETTER BE > SPACES            
         JNH   GETTOKX                                                          
         MVC   TOKEN+4(L'RTAUTID),RTAUTID                 LICENSE ID            
         MVI   TOKEN+4+L'RTAUTID,C'|'                                           
         MVC   TOKEN+4+L'RTAUTID+1(L'RTAUTSEC),RTAUTSEC   SECURITY ID           
         DROP  R2                                                               
                                                                                
GETTOKX  J     EXITCC                                                           
                                                                                
***********************************************************************         
* CALL DDLINK TO GET SELF-DEFINING RECORD                             *         
*                                                                               
* NOTE:  AIO3 WILL BE CLOBBERED TO READ THE GENERIC SDR                         
*        AIO5 WILL BE CLOBBERED TO READ THE AGENCY SPECIFIC SDR                 
***********************************************************************         
                                                                                
GETSDR   GOTOR LP_AGSDR,DMCB,LP_D,AIO2,0                                        
*                                                                               
         L     RF,ACOMFACS         LOAD FACILITIES OVERLAYS                     
         L     RF,CRECUP-COMFACSD(RF)                                           
         ST    RF,VRECUP                                                        
*****                                                                           
* AGENCY ALPHA SDR OVERRIDE                                                     
*****                                                                           
         L     R2,AIO5                                                          
         USING SDRRECD,R2                                                       
         XC    0(L'SDRKEY,R2),0(R2)  CLEAR OUT KEY IN IOAREA                    
*                                                                               
         XC    WORK,WORK             READ INTO AIO5                             
         MVC   WORK(L'LP_AGY),LP_AGY                                            
         GOTOR LP_AGSDR,DMCB,LP_D,AIO5,WORK                                     
*                                                                               
         L     RE,AIO2               NEED THIS CHECK AS GETSDR DOES A           
         L     RF,AIO5                 DEFAULT READ BY CLEARING SDRKFFL         
         CLC   0(SDRKKEY-SDRKEY,RE),0(RF)                                       
         JNE   GTSDR10                                                          
*                                                                               
         BRAS  RE,SDROVRW                                                       
*****                                                                           
* ENVIROMENT SDR OVERRIDE                                                       
*****                                                                           
GTSDR10  L     R2,AIO5                                                          
         USING SDRRECD,R2                                                       
         XC    0(L'SDRKEY,R2),0(R2)  CLEAR OUT KEY IN IOAREA                    
*                                                                               
         CLI   ENVIRO,0              DO WE HAVE AN ENVIROMENT OVERRIDE?         
         JE    GTSDRX                NO                                         
         XC    WORK,WORK             READ INTO AIO5                             
         MVC   WORK+2(L'ENVIRO),ENVIRO                                          
         GOTOR LP_AGSDR,DMCB,LP_D,AIO5,WORK                                     
*                                                                               
         L     RE,AIO2               NEED THIS CHECK AS GETSDR DOES A           
         L     RF,AIO5                 DEFAULT READ BY CLEARING SDRKFFL         
         CLC   0(SDRKKEY-SDRKEY,RE),0(RF)                                       
         JNE   GTSDRX                                                           
*                                                                               
         BRAS  RE,SDROVRW                                                       
*                                                                               
GTSDRX   J     EXITCC                                                           
**********************                                                          
* OVERWRITE VALUES IN GENERIC SDR THAT WE FOUND IN OUR SPECIFIC SDR             
**********************                                                          
SDROVRW  NTR1  LABEL=*                                                          
         L     R2,AIO5             LOOK AT THE AGENCY SPECIFIC SDR              
         OC    0(2,R2),0(R2)       MAKE SURE WE HAVE A RECORD                   
         JZ    SDROVX              OTHERWISE JUST USE GENERIC SDR               
         LA    R2,SDRRFRST                                                      
SDROV010 CLI   0(R2),0             ANY MORE ELEMENT IN AGY SPECIFIC?            
         JE    SDROVX                                                           
*                                                                               
         L     R6,AIO2                                                          
         LA    R6,SDRRFRST-SDRKEY(R6)  R6 = 1ST ELEMENT IN GENERIC              
SDROV020 CLI   0(R6),0             CAN'T FIND A MATCHING ELEM?                  
         JE    SDROV040            NO, JUST ADD IT TO GENERIC                   
         CLC   0(1,R6),0(R2)                                                    
         JE    SDROV030                                                         
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         J     SDROV020                                                         
*                                                                               
* DELETE THE ELEM IN GENERIC                                                    
SDROV030 GOTO1 VRECUP,DMCB,(X'FE',AIO2),0(R6),0(R6),=X'002A002007D0'            
*                                                                               
* ADD ELEM IN GENERIC                                                           
SDROV040 GOTO1 VRECUP,DMCB,(X'FE',AIO2),0(R2),0(R6),=X'002A002007D0'            
*                                                                               
         LLC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         J     SDROV010                                                         
*                                                                               
SDROVX   J     EXITCC                                                           
                                                                                
***********************************************************************         
* Send Self-Defining Elements                                         *         
***********************************************************************         
                                                                                
ARYSDR   LKOUT A,(D,B#SDRREC,SDRRFRST),EOT=EOR,ROWWIDTH=(V,SDELEN),    +        
               ROWID=(SDELD,0)                                                  
                                                                                
***********************************************************************         
* FIELDS SENT FROM SELF-DEFINING RECORD ARE AS FOLLOWS:-              *         
*                                                                     *         
*        10    Agency uses Midas Feature (cannot be Pinergy)          *         
*        11    Agency uses Pinergy Feature (cannot be Midas)          *         
*        20    Maximum number of product records allowed              *         
***********************************************************************         
                                                                                
Defvl    LKOUT C,255,SDELD,SDEL                                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* CFM client download (X'FFF2')                                       *         
***********************************************************************         
                                                                                
REQCFMC  LKREQ H,I#CFMCDL,OUTCFMC,NEXTREQ=REQNCLT                               
                                                                                
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
Option03 LKREQ F,42,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option04 LKREQ F,43,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option05 LKREQ F,44,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option06 LKREQ F,45,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option07 LKREQ F,46,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
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
DOPTLIT  DC    C'Download Options'                                              
DUMYLIT  DC    C'Dummy'                                                         
                                                                                
OUTCFMC  LKOUT H                                                                
                                                                                
CFMC     LKOUT R,X'0031'                                                        
Array    LKOUT C,X'0031',(A,ARYCLT)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYCLT   LKOUT A,(R,NXTCLT),MULTIROW=Y,ROWNAME=CLTRECD                          
CMedCod  LKOUT C,001,(D,B#WORKD,QMEDA),CHAR,FILTROUT=TSTCLT,           +        
               SKIPCOLS=CLTSKIPN                                                
CLTSKIPS DS    0X                  Start of columns to skip                     
CCltCod  LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CCltNam  LKOUT C,003,CNAME,CHAR                                                 
COffNum  LKOUT C,004,COFFICE,(R,EDTACS)                                         
CAccOfc  LKOUT C,005,CACCOFC,CHAR                                               
CAccAgy  LKOUT C,006,CACCAGY,CHAR,ND=Y                                          
                                                                                
PRout    LKOUT P,CINDS1,SETBITS                                                 
         LKOUT C,020,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,021,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,022,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,023,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,024,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,025,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,026,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,027,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CTrfOfc  LKOUT C,030,CTRAFOFC,CHAR,ND=Y                                         
CIntfCd  LKOUT C,031,CCLTIFC,CHAR,ND=Y                                          
CCopPrd  LKOUT C,032,CPRPRD,CHAR,ND=Y                                           
                                                                                
PRout    LKOUT P,COPT2,SETBITS                                                  
* * * *  LKOUT C,040,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,041,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,042,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,043,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,044,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,045,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,046,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,047,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
PRout    LKOUT P,COPT3,SETBITS                                                  
* * * *  LKOUT C,050,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,051,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,052,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,053,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,054,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,055,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,056,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,057,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
PRout    LKOUT P,COPT4,SETBITS                                                  
* * * *  LKOUT C,060,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,061,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,062,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,063,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,064,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,065,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,066,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,067,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CCos2Fa  LKOUT C,070,CCOST2,(R,OUTCS2),ND=Y,LEN=7                               
CBilECn  LKOUT C,071,CPROF+05,CHAR,LEN=1,ND=Y                                   
CPrtCCd  LKOUT C,072,CPROF+06,CHAR,LEN=1,ND=Y                                   
CCRatCn  LKOUT C,073,CPROF+14,CHAR,LEN=1,ND=Y                                   
CCRatCv  LKOUT C,074,CEXTRA+14,CHAR,LEN=1,ND=Y                                  
CCRatCv  LKOUT C,075,CEXTRA+03,(R,EDTC0N),ND=Y                                  
CCltLoc  LKOUT C,076,CLOCK,CHAR,ND=Y                                            
CIDTitl  LKOUT C,077,CTITLE,CHAR,ND=Y                                           
                                                                                
CPUser1  LKOUT C,090,CPU1,CHAR,ND=Y                                             
CPUTyp1  LKOUT C,091,CPU1TYPE,CHAR,ND=Y                                         
CPULen1  LKOUT C,092,CPU1LEN,UBIN,ND=Y                                          
PRout    LKOUT P,CPU1FLG1,SETBITS                                               
         LKOUT C,093,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,094,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,095,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,096,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,097,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,098,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,099,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,100,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CPUser2  LKOUT C,110,CPU2,CHAR,ND=Y                                             
CPUTyp2  LKOUT C,111,CPU2TYPE,CHAR,ND=Y                                         
CPULen2  LKOUT C,112,CPU2LEN,UBIN,ND=Y                                          
PRout    LKOUT P,CPU2FLG1,SETBITS                                               
         LKOUT C,113,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,114,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,115,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,116,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,117,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,118,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,119,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,120,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CEUser1  LKOUT C,130,CEU1,CHAR,ND=Y                                             
CEUTyp1  LKOUT C,131,CEU1TYPE,CHAR,ND=Y                                         
CEULen1  LKOUT C,132,CEU1LEN,UBIN,ND=Y                                          
PRout    LKOUT P,CEU1FLG1,SETBITS                                               
         LKOUT C,133,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,134,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,135,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,136,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,137,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,138,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,139,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,140,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
PRout    LKOUT P,CEU1FLG2,SETBITS                                               
         LKOUT C,141,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,142,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,143,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,144,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,145,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,146,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,147,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,148,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CEUser2  LKOUT C,150,CEU2,CHAR,ND=Y                                             
CEUTyp2  LKOUT C,151,CEU2TYPE,CHAR,ND=Y                                         
CEULen2  LKOUT C,152,CEU2LEN,UBIN,ND=Y                                          
PRout    LKOUT P,CEU2FLG1,SETBITS                                               
         LKOUT C,153,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,154,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,155,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,156,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,157,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,158,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,159,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,160,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
PRout    LKOUT P,CEU2FLG2,SETBITS                                               
         LKOUT C,161,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,162,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,163,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,164,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,165,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,166,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,167,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,168,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
Array    LKOUT C,170,(A,ARYCACS)                                                
CFRSubL  LKOUT C,171,CLTSLLMT,LBIN,ND=Y                                         
CFRSubL  LKOUT C,172,CSCJROT,CHAR,ND=Y,PCVERSION=1.0.5.11                       
                                                                                
PRout    LKOUT P,CKEYCLT,SETAAA                                                 
         LKOUT C,173,(D,B#WORKD,WORK+00),CHAR,LEN=30,ND=Y                       
         LKOUT C,174,(D,B#WORKD,WORK+30),CHAR,LEN=30,ND=Y                       
         LKOUT C,175,(D,B#WORKD,WORK2+00),CHAR,LEN=30,ND=Y                      
         LKOUT C,176,(D,B#WORKD,WORK2+30),CHAR,LEN=30,ND=Y                      
                                                                                
CReqTok  LKOUT C,300,(D,B#SAVED,SVREQTOK),CHAR                                  
CLTSKIPN EQU   (*-CLTSKIPS)/LX_COLSL                                            
                                                                                
Array    LKOUT C,X'0032',(A,ARYPRD),FILTROUT=TSTPRD                             
         LKOUT E                                                                
                                                                                
ARYCACS  LKOUT A,(D,B#CLTREC,CACCESS),ROWNAME=CACCESS,ROWWIDTH=1,      +        
               NROWS=L'CACCESS                                                  
CLimAcc  LKOUT C,170,CACCESS,(R,EDTACS),LEN=1,ND=Y                              
         LKOUT E                                                                
                                                                                
TSTCLT   CLI   QCLTOPT,YESQ        Test downloading client details              
         BR    RE                                                               
                                                                                
TSTPRD   CLI   QPRDOPT,YESQ        Test downloading product details             
         BR    RE                                                               
                                                                                
OUTCS2   L     R2,LP_AINP                                                       
         OC    0(4,R2),0(R2)                                                    
         JZ    XCOLEN                                                           
*******                                                                         
* COST2 WITH A VALUE OF 0.000000 IS STORED AS X'80000000'                       
*******                                                                         
         TM    0(R2),X'80'         HIGH ORDER BIT IS ON?                        
         JZ    *+10                                                             
         XC    0(4,R2),0(R2)       YES, CHANGE TO 0.0 SO IT CAN DISPLAY         
         L     R3,LP_AOUT                                                       
         EDIT  (B4,0(R2)),(7,0(R3)),FILL=0                                      
         J     EXITY               DISPLAY THIS FIELD                           
                                                                                
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
                                                                                
         USING OFFICED,RC                                                       
EDTACS   LM    R2,R4,LP_AINP       Edit office access control                   
         CLI   0(R2),0             Don't send null entries                      
         JE    XCOLEN                                                           
         XC    OFFICED(OFCLENQ),OFFICED                                         
         MVI   OFCSYS,NETLETQ      Look-up 2 character office                   
         MVC   OFCAGY,LP_AGY                                                    
         MVC   OFCOFC,0(R2)                                                     
         GOTOR VOFFICER,DMCB,(C'2',OFFICED),(X'01',LP_ACOM)                     
         CLI   0(R1),0                                                          
         JNE   XCOLEN                                                           
         MVC   0(L'OFCOFC2,R4),OFCOFC2                                          
         LHI   R0,L'OFCOFC2                                                     
         J     SETOLENX                                                         
         DROP  RC                                                               
                                                                                
EDTC0N   LM    R2,R4,LP_AINP       Edit client rate coverage                    
         MVC   0(1,R4),0(R2)                                                    
         CLI   0(R2),C'0'                                                       
         JNE   *+8                                                              
         MVI   0(R4),C'N'                                                       
         LHI   R0,1                                                             
         J     SETOLENX                                                         
                                                                                
         USING GCWORKD,RC                                                       
SETAAA   XC    WORK,WORK           Set prd AAA's name & address                 
         XC    WORK2,WORK2         Clear work areas                             
         L     R1,LP_AINP                                                       
         OC    0(L'CKEYCLT,R1),0(R1)                                            
         JZ    XCOLEN                                                           
         MVC   SVIOVALS,IOVALS                                                  
K        USING PKEY,IOKEY                                                       
         XC    K.PKEY,K.PKEY                                                    
         MVI   K.PKEYTYPE,PKEYTYPQ                                              
         MVC   K.PKEYAM,QMEDX                                                   
         MVC   K.PKEYCLT,0(R1)                                                  
         MVC   K.PKEYPRD,=C'AAA'                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JNE   SETAAAN                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO4'                           
         JNE   SETAAAN                                                          
         L     R2,AIO4                                                          
         USING PRDRECD,R2                                                       
         MVC   WORK(60),PADDR1     Bill name & address line 1                   
         MVC   WORK2(60),PADDR3    Bill address lines 2 & 3                     
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITY                                                            
                                                                                
SETAAAN  MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     XCOLEN                                                           
         DROP  R2                                                               
                                                                                
GCWORKD  DSECT                     ** SETAAA s/r local w/s **                   
SVIOVALS DS    XL(IOVALL)          Saved I/O values                             
GCWORKL  EQU   *-GCWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
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
         LA    R0,CA_D+CA_LNQ                                                   
         ST    R0,ANXTCLT                                                       
         CLC   CA_SYS,SYSLET       Test for my system                           
         JNE   NXTCLT02                                                         
         CLC   LAGY,CA_AGYA        Test change of agency                        
         JE    NXTCLT10                                                         
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
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,UNTDIR,(4,0),0                               
         GOTOR (RF),(R1),DMKEY,UNTFIL,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,SPTDIR,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,SPTFIL,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,XSPDIR,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,XSPFIL,(4,0),0                                   
                                                                                
NXTCLT08 GOTOR (#GETAGY,AGETAGY),CA_AGYA                                        
                                                                                
NXTCLT10 GOTOR (#VALMED,AVALMED),DMCB,CA_MEDC,0,QMEDX                           
         JNE   NXTCLT02                                                         
         GOTOR (#VALCLT,AVALCLT),DMCB,CA_CLTC,3,QCLTX                           
         JNE   NXTCLT02                                                         
         L     RE,ACLTREC                                                       
         MVC   SVCLTKEY,0(RE)      Save key of client record                    
         XC    PRDRSTR,PRDRSTR     Product start range                          
         MVC   PRDREND,=X'FFFFFF'  Product end range                            
         OC    CA_PRDC,CA_PRDC                                                  
         CLC   CA_PRDC,SPACES      Filtering on prduct codes?                   
         JNH   *+16                                                             
         MVC   PRDRSTR,CA_PRDC                                                  
         MVC   PRDREND,CA_PRDC                                                  
         MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
ARYPRD   LKOUT A,(R,NXTPRD),MULTIROW=Y,ROWNAME=PRDRECD                          
PMedCod  LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
PCltCod  LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
PPrdCod  LKOUT C,003,(D,,PKEYPRD),CHAR                                          
PPrdAcc  LKOUT C,004,PACCT,(R,EDTPA#)                                           
PPrdNam  LKOUT C,005,PNAME,CHAR                                                 
PPClass  LKOUT C,006,PCLASS,(R,EDTPCL),ND=Y                                     
PPrdLoc  LKOUT C,007,PLOCK,CHAR,ND=Y                                            
PPLAcDt  LKOUT C,008,PLKDAT,CDAT,ND=Y                                           
PPBilNm  LKOUT C,009,PADDR1,CHAR                                                
PPAdrL1  LKOUT C,010,PADDR2,CHAR,ND=Y                                           
PPAdrL2  LKOUT C,011,PADDR3,CHAR,ND=Y                                           
PPAdrL3  LKOUT C,012,PADDR4,CHAR,ND=Y                                           
PBilBas  LKOUT C,013,PBILLBAS,UBIN,FILTROUT=TSVS0810                            
PBilCom  LKOUT C,014,PBILLCOM,CBIN,ND=Y                                         
PAgyFee  LKOUT C,015,PAGYFEE,SPAK,ND=Y                                          
PYrMoSv  LKOUT C,016,PBILLDT,BMON,ND=Y                                          
PUserF1  LKOUT C,017,PUSER1,CHAR,ND=Y                                           
PUserF2  LKOUT C,018,PUSER2,CHAR,ND=Y                                           
                                                                                
PRout    LKOUT P,POPT1,SETBITS                                                  
         LKOUT C,020,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,021,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,022,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,023,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,024,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,025,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,026,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,027,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
PBillBas LKOUT C,080,PBILLBAS,(R,EDTBBS),ND=Y,PCVERSION=0.8.10.0                
PBBComm  LKOUT C,081,PBILLBAS,(R,EDTBBC),ND=Y,PCVERSION=0.8.10.0                
PCommBas LKOUT C,082,PBILLBAS,(R,EDTCBS),ND=Y,PCVERSION=0.8.10.0                
                                                                                
PReqTok  LKOUT C,300,(D,B#SAVED,SVREQTOK),CHAR                                  
         LKOUT E                                                                
                                                                                
EDTPA#   LM    R2,R4,LP_AINP       Edit product account number                  
         CLI   0(R2),X'FF'                                                      
         JNE   *+18                                                             
         UNPK  0(5,R4),1(3,R2)                                                  
         LHI   R0,5                                                             
         J     SETOLENX                                                         
         MVC   0(L'PACCT,R4),0(R2)                                              
         LHI   R0,L'PACCT                                                       
         J     SETOLENX                                                         
                                                                                
EDTPCL   LM    R2,R4,LP_AINP       Edit product class                           
         CLI   0(R2),0                                                          
         JE    XCOLEN                                                           
         LHI   R0,1                                                             
         MVC   0(1,R4),0(R2)                                                    
         CLI   0(R2),X'99'                                                      
         JH    SETOLENX                                                         
         LHI   R0,2                                                             
         PACK  0(1,R4),0(1,R2)     There are 2 product classes                  
         NI    0(R4),X'0F'                                                      
         OI    0(R4),X'C0'                                                      
         MVC   1(1,R4),0(R2)                                                    
         NI    1(R4),X'0F'                                                      
         OI    1(R4),X'C0'                                                      
         J     SETOLENX                                                         
                                                                                
EDTBBS   LM    R2,R4,LP_AINP       Edit Bill Basis                              
         OC    SVPBLDAT,SVPBLDAT                                                
         JZ    XCOLEN                                                           
         LHI   R0,1                                                             
         MVI   0(R4),C'G'                                                       
         TM    SVPBLBAS,X'10'                                                   
         JZ    SETOLENX                                                         
         MVI   0(R4),C'N'                                                       
         J     SETOLENX                                                         
                                                                                
EDTBBC   LM    R2,R4,LP_AINP       Edit   Commission Only Billing               
         OC    SVPBLDAT,SVPBLDAT                                                
         JZ    XCOLEN                                                           
         LHI   R0,1                                                             
         MVI   0(R4),C'N'                                                       
         TM    SVPBLBAS,X'40'                                                   
         JZ    SETOLENX                                                         
         MVI   0(R4),C'Y'                                                       
         J     SETOLENX                                                         
                                                                                
EDTCBS   LM    R2,R4,LP_AINP       Edit Comm Basis                              
         OC    SVPBLDAT,SVPBLDAT                                                
         JZ    XCOLEN                                                           
         OC    SVPBLCOM,SVPBLCOM                                                
         JZ    XCOLEN                                                           
         LHI   R0,1                                                             
         MVI   0(R4),C'G'                                                       
         TM    SVPBLBAS,X'01'                                                   
         JZ    SETOLENX                                                         
         MVI   0(R4),C'N'                                                       
         J     SETOLENX                                                         
                                                                                
TSVS0810 CLC   LP_VRSN,VS081000    Test against version 0.8.10.x                
         JL    EXITY               Lower, we want the field                     
         J     EXITN                                                            
                                                                                
***********************************************************************         
* Read product records for current client                             *         
***********************************************************************         
                                                                                
NXTPRD   XC    SVPBLDAT,SVPBLDAT                                                
         XC    SVPBLBAS,SVPBLBAS                                                
         XC    SVPBLCOM,SVPBLCOM                                                
*                                                                               
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',PRDKEYT),                +        
               ('B#PRDREC',0),('$NXTRSPT',SAVED),0,0                            
         JNE   EXITY               Used to be  "J  EXITY"                       
*                                                                               
         L     R2,IOADDR                                                        
         MVC   SVPBLDAT,PBILLDT-PRDHDR(R2)                                      
         MVC   SVPBLBAS,PBILLBAS-PRDHDR(R2)                                     
         MVC   SVPBLCOM,PBILLCOM-PRDHDR(R2)                                     
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Net client record download                                          *         
***********************************************************************         
                                                                                
REQNCLT  LKREQ H,M#NECLTD,OUTNCL,NEXTREQ=REQSTA                                 
Media    LKREQ F,1,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),             +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=NE#MED,COL=*                   
         LKREQ E                                                                
                                                                                
OUTNCL   LKOUT H                   ** Client download **                        
         LKOUT R,X'0020'                                                        
Array    LKOUT C,X'0020',(A,ARYNCL)                                             
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CLIENT DOWNLOAD                                *         
***********************************************************************         
                                                                                
ARYNCL   LKOUT A,(R,NXTNCL),MULTIROW=Y,ROWNAME=CLTRECD                          
CltCd    LKOUT C,1,(D,B#WORKD,QCLTA),CHAR                                       
CltNm    LKOUT C,2,CNAME,CHAR                                                   
*Daily    LKOUT C,3,CDAILY,CHAR,ND=Y                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET CLIENT RECORDS FOR CLIENT DOWNLOAD                              *         
***********************************************************************         
                                                                                
NXTNCL   DS    0H                                                               
         L     R5,ALP                                                           
         L     RF,LP_ALPXD         LET'S SEE IF OPTICA IS CALLING               
         USING LP_XD,RF                                                         
         LA    RE,LP_XPINF                                                      
         DROP  RF                                                               
*                                                                               
         USING TXPINFO,RE             FROM FAUTL                                
         CLC   TXPNUM,=AL2(XPOPTICQ)  OPTICA CALLING FOR A CLIENT LIST?         
         JNE   NXTNCLNX               NO                                        
         DROP  RE                                                               
*                                  ANYTHING IN TWAACCS? (TWA+6)                 
         L     RF,LP_ATWA                                                       
         OC    TWAACCS-TWAD(L'TWAACCS,RF),TWAACCS-TWAD(RF)                      
         JNZ   NXTNCLNX                                                         
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NOMORE              No, only send all/all first time             
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
         XC    0(255,R2),0(R2)                                                  
         MVC   QCLTA,=C'ALL'          DO WE NEED THE BINARY AGY/MED?            
         MVC   CKEYCLT,=X'816B'       CLPACK for C'ALL'                         
         MVC   CNAME(3),=C'ALL'                                                 
         ST    R2,LP_ADATA                                                      
         J     EXITY                                                            
*                                                                               
NXTNCLNX GOTOR (#NXTREC,ANXTREC),DMCB,CLTKEYT,('B#CLTREC',0),          +        
               ('$NXTRSPT',SAVED),0,('#LIMCLT',ALIMCLT)                         
         JNE   EXITY                                                            
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
         OC    CPLDATA(CPLDATAL),CPLDATA                                        
         JZ    NXTNCL                                                           
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Station record download                                                       
***********************************************************************         
REQSTA   LKREQ H,M#STALST,OUTSTA,NEXTREQ=REQTCML                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=NE#MED                        
         LKREQ E                                                                
*                                                                               
OUTSTA   LKOUT H                                                                
         LKOUT R,X'0050'                                                        
PRout    LKOUT P,,STAINI           Init station arrary                          
Array    LKOUT C,X'0050',(A,ARYSTA)                                             
         LKOUT X                                                                
                                                                                
STAINI   GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Array definition for station record download                        *         
***********************************************************************         
                                                                                
ARYSTA   LKOUT A,(R,NXTSTA),MULTIROW=Y,ROWNAME=STARECD                          
                                                                                
Media    LKOUT C,001,STAKMED,CHAR                                               
Statn    LKOUT C,002,STAKCALL,CHAR,LEN=4                                        
NetMd    LKOUT C,003,STYPE,CHAR,ND=Y                                            
SubMd    LKOUT C,004,SUBMEDIA,CHAR,ND=Y                                         
NetPo    LKOUT C,005,SPTYPE,CHAR,ND=Y                                           
NTISC    LKOUT C,006,SNTISTA,CHAR,ND=Y                                          
TalCN    LKOUT C,007,SLSTCNET,CHAR,ND=Y                                         
TalMd    LKOUT C,008,STALTYP,CHAR,ND=Y                                          
TrfTy    LKOUT C,009,STRTYPE,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Send station values                                                 *         
***********************************************************************         
                                                                                
NXTSTA   GOTOR (#NXTREC,ANXTREC),DMCB,MSTKEYT,('B#STAREC',0),          +        
               ('$NXTRSTA',SAVED),0,0                                           
         JNE   EXITY                                                            
         L     R2,IOADDR                                                        
         USING STARECD,R2                                                       
         CLI   SPTYPE,C'S'         Network post type is syndication?            
         JE    NXTSTA                                                           
         CLI   STYPE,C'S'          Network media type is syndication?           
         JE    NXTSTA                                                           
         CLI   STRTYPE,C'S'        Traffic post type is syndication?            
         JE    NXTSTA                                                           
         J     EXITY               Exit to send station values                  
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Traffic commercial record download                                  *         
***********************************************************************         
                                                                                
REQTCML  LKREQ H,M#TRCMML,OUTTCM,NEXTREQ=REQTHOU                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=NE#MED                        
CltCd    LKREQ F,002,(D,B#WORKD,QCLTA),CHAR,TEXT=NE#CLI                         
SDate    LKREQ F,003,(D,B#SAVED,STRDATEB),BDAT,TEXT=NE#SDATE,COL=*              
EDate    LKREQ F,004,(D,B#SAVED,ENDDATEB),BDAT,TEXT=NE#EDATE,COL=*              
CmlPrefx LKREQ F,005,(D,B#SAVED,CMLPREFX),CHAR,TEXT=NE#CMML,COL=*               
         LKREQ E                                                                
                                                                                
OUTTCM   LKOUT H                   Traffic commercial output maps start         
                                                                                
         LKOUT R,1                                                              
PRout    LKOUT P,,TCMINI           Init traffic commercial arrary               
Array    LKOUT C,1,(A,ARYTCM)                                                   
Array    LKOUT C,1,(A,ARYTHC)                                                   
Array    LKOUT C,2,(A,ARYTCL)                                                   
Array    LKOUT C,3,(A,ARYTPR)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                   Traffic commercial output maps end           
                                                                                
ARYTCM   LKOUT A,(R,NXTTCM),MULTIROW=Y,ROWNAME=CMLRECD                          
Media    LKOUT C,001,(D,B#SAVED,MEDCOD),CHAR                                    
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#WORKD,IOKEY),(R,EDTCMC)                               
Array    LKOUT C,004,(A,ARYCPRD)                                                
CmlT1    LKOUT C,005,(D,B#SAVED,SVCMLDS1),CHAR,ND=Y                             
CmlT2    LKOUT C,006,(D,B#SAVED,SVCMLDS2),CHAR,ND=Y                             
CmlT3    LKOUT C,007,(D,B#SAVED,SVCMLDS3),CHAR,ND=Y                             
Array    LKOUT C,008,(A,ARYCMDS)                                                
         LKOUT E                                                                
                                                                                
ARYTHC   LKOUT A,(R,NXTTHC),MULTIROW=Y,ROWNAME=CMLRECD                          
Media    LKOUT C,001,(D,B#SAVED,MEDCOD),CHAR                                    
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#WORKD,IOKEY),(R,EDTCMC)                               
Array    LKOUT C,004,(A,ARYCPRD)                                                
CmlT1    LKOUT C,005,(D,B#SAVED,SVCMLDS1),CHAR,ND=Y                             
CmlT2    LKOUT C,006,(D,B#SAVED,SVCMLDS2),CHAR,ND=Y                             
CmlT3    LKOUT C,007,(D,B#SAVED,SVCMLDS3),CHAR,ND=Y                             
Array    LKOUT C,008,(A,ARYCMDS)                                                
         LKOUT E                                                                
                                                                                
ARYCPRD  LKOUT A,(D,B#WORKD,ELEM2),NROWS=255,ROWWIDTH=1,ROWNAME=DUMMY_D         
PrdCd    LKOUT C,004,DUM_LIN1,(R,EDTCPR),ND=Y                                   
         LKOUT E                                                                
                                                                                
ARYCMDS  LKOUT A,(D,B#TCMREC,CMLDTAEL),EOT=EOR,ROWID=(CMLDTAEL,X'10'), +        
               ROWWIDTH=(V,CMLDTALN)                                            
         LKOUT C,008,CMLSLN,UBIN                                                
         LKOUT C,009,CMLRLSE,BDAT                                               
         LKOUT C,010,CMLRCL,BDAT,ND=Y                                           
         LKOUT C,011,(D,B#SAVED,SVCMLUFN),CHAR,ND=Y                             
         LKOUT E                                                                
                                                                                
ARYTCL   LKOUT A,(R,NXTTCL),MULTIROW=Y,ROWNAME=CLTRECD                          
Array    LKOUT C,2,(A,ARYTC1)                                                   
         LKOUT E                                                                
                                                                                
ARYTC1   LKOUT A,(D,B#CLTREC,CLTRECD),NEWEL=Y,NROWS=1,ROWWIDTH=0                
CltCd    LKOUT C,001,(D,B#WORKD,QCLTA),CHAR                                     
CltNm    LKOUT C,002,(D,B#WORKD,WORK2),CHAR                                     
         LKOUT E                                                                
                                                                                
ARYTPR   LKOUT A,(R,NXTTPR),MULTIROW=Y,ROWNAME=PRDRECD                          
Array    LKOUT C,3,(A,ARYTP1)                                                   
         LKOUT E                                                                
                                                                                
ARYTP1   LKOUT A,(D,B#PRDREC,PRDRECD),NEWEL=Y,NROWS=1,ROWWIDTH=0                
PrdCd    LKOUT C,001,(D,B#SAVED,QPRDA),CHAR                                     
PrdNm    LKOUT C,002,(D,B#WORKD,WORK2),CHAR                                     
TalAg    LKOUT C,003,(D,B#WORKD,DUB2),CHAR,ND=Y                                 
         LKOUT E                                                                
                                                                                
TCMINI   OC    MEDCOD,SPACES                                                    
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   EXITN                                                            
         OC    QCLTA,SPACES                                                     
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    Get address of TRPACK                  
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         MVI   CMLPRFXL,0                                                       
         CLC   CMLPREFX,SPACES     Any commercial prefix?                       
         JNH   TCMINI20            None                                         
         LA    R1,L'CMLPREFX                                                    
         LA    RE,CMLPREFX+L'CMLPREFX-1                                         
*                                                                               
TCMINI10 CLI   0(RE),C' '          Anything here?                               
         JH    TCMINI15                                                         
         BCTR  RE,0                                                             
         JCT   R1,TCMINI10                                                      
*                                                                               
TCMINI15 STC   R1,CMLPRFXL         We know the length now                       
         CLI   CMLPRFXL,8          Input length >= 8  ?                         
         JNL   TCMINI30                                                         
TCMINI20 LA    RE,1(RE)            Point to the first blank                     
         MVI   0(RE),C'A'          1st 8 chars can't be < C'AAAAAAAA'           
         LA    R1,1(R1)            fill with C'A' until first 8 char            
         CHI   R1,8                   are populated                             
         JL    TCMINI20                                                         
*                                                                               
TCMINI30 GOTOR VTRPACK,DMCB,(C'P',CMLPREFX),CMLPRFXS                            
*                                                                               
         CLI   CMLPRFXL,12         FOR A SPECIFIC 12 CHAR FILM?                 
         JE    TCMINI40                                                         
*                                                                               
         LLC   R1,CMLPRFXL                                                      
         LA    R3,CMLPREFX(R1)                                                  
         LA    RE,12                                                            
         SR    RE,R1               RE=# of C'9's to fill to the end             
         BCTR  RE,0                                                             
         BASR  R4,0                                                             
         MVC   0(0,R3),=12C'9'                                                  
         EX    R3,0(R4)                                                         
*                                                                               
TCMINI40 GOTOR VTRPACK,DMCB,(C'P',CMLPREFX),CMLPRFXE                            
*                                                                               
TCMINIX  J     EXITY                                                            
*                                                                               
EDTCPR   LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             Have binary product code?                    
         JE    XCOLEN                                                           
         XC    0(3,R4),0(R4)       Init output                                  
         CLI   0(R2),X'FF'         'All' product?                               
         JNE   *+14                                                             
         MVC   0(3,R4),=C'ALL'                                                  
         J     EDTCPR42                                                         
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST                                                         
         LA    R1,220              220 entries in CLIST                         
EDTCPR20 CLC   0(1,R2),3(RF)       Match that of product list?                  
         JE    EDTCPR40                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,EDTCPR20                                                      
         LA    RF,CLIST2                                                        
         LA    R1,35               Additional 35 entries in CLIST2              
EDTCPR24 CLC   0(1,R2),3(RF)       Match that of product list?                  
         JE    EDTCPR40                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,EDTCPR24                                                      
         J     XCOLEN              Bad product code                             
EDTCPR40 MVC   0(3,R4),0(RF)       Get character format for prd code            
EDTCPR42 LA    R0,3                Max length for product code                  
         J     SETOLENX                                                         
         DROP  RE                                                               
*                                                                               
EDTCMC   LM    R2,R4,LP_AINP                                                    
         USING CMLKEY,R2                                                        
         OC    CMLKCML,CMLKCML     Have commercial code?                        
         JZ    XCOLEN                                                           
         XC    0(13,R4),0(R4)                                                   
         MVC   0(L'CMLKCML,R4),CMLKCML                                          
*****    TM    CMLKSTAT,CMLKSTA_PCKD   We're going thru the packed              
*****    JZ    EDTCMC10                 films with 0AC1 & 0Ac2 passives         
         XC    0(13,R4),0(R4)                                                   
         GOTOR VTRPACK,DMCB,(C'U',CMLKCML),0(R4)                                
EDTCMC10 CLI   0(R4),C'A'          Lower case letters?                          
         JNL   EDTCMC30                                                         
         OC    SVCMLAID,SVCMLAID   Have saved Ad-ID?                            
         JZ    EDTCMC30                                                         
         XC    0(13,R4),0(R4)                                                   
         MVC   0(L'SVCMLAID,R4),SVCMLAID                                        
EDTCMC30 LA    R0,L'CMLADID        Max length for commerical code               
         J     SETOLENX                                                         
         DROP  R2                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTTCM   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTTCMSQ                                                         
         XC    SVCMLFLS(SVCMLFLQ),SVCMLFLS   Init saved commercial flds         
         XC    ELEM1,ELEM1         Table of prd codes to be downloaded          
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLPIDAD+0,X'0A'    Passive keys for packed Ad-IDs               
         MVI   CMLPIDAD+1,X'C1'                                                 
         MVC   CMLPADAM,QMEDX                                                   
         MVC   CMLPADCL,QCLTX                                                   
         MVC   CMLPADID,CMLPRFXS   Starting point                               
*                                                                               
NXTTCMHI GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
*                                                                               
NXTTCM10 LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         CLC   IOKEY(CMLPADID-CMLKEY),IOKEYSAV  Same upto client?               
         JNE   NOMORE                           No, then we're done             
         OC    CMLPADID,CMLPADID   Have commercial ID?                          
         JZ    NXTTCMSQ            No                                           
         CLC   =X'C5DCC5DCC5B80000',CMLPADID   House for the Client?            
         JE    NXTTCMSQ              Yes, this is TRPACK of C'99999999'         
                                                                                
         CLC   CMLPADID,CMLPRFXE   Past the last?                               
         JH    NOMORE              Then no more commercials to look for         
         DROP  RE                                                               
*                                                                               
NXTTCM16 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
         JNE   NXTTCMSQ                                                         
         XC    SVCMLAID,SVCMLAID   Init saved commercial fields                 
         XC    SVCMLDS1,SVCMLDS1                                                
         XC    SVCMLDS2,SVCMLDS2                                                
         XC    SVCMLDS3,SVCMLDS3                                                
         MVI   SVCMLUFN,0                                                       
         XC    ELEM2,ELEM2         List of prds for this commercial             
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
         USING CMLDTAEL,R1                                                      
         TM    CMLSTAT,X'80'       Deleted cml                                  
         JO    NXTTCMSQ                                                         
         MVC   SVCMLDS1(L'CMLTITLE),CMLTITLE  Save title in case no x30         
         OC    STRDATEB(L'STRDATEB*2),STRDATEB   Any date filter?               
         JZ    NXTTCM18                          None, accept all cmmls         
         CLC   ENDDATEB,CMLRLSE    End date filter before release date?         
         JL    NXTTCMSQ            Yes, next record then                        
         CLC   STRDATEB,CMLRCL     Start date after recall date?                
         JH    NXTTCMSQ            Yes, next record then                        
*                                                                               
NXTTCM18 CLC   CMLRCL,=X'FFFFFF'   Recall date 'until further notice'?          
         JNE   NXTTCM20                                                         
         XC    CMLRCL,CMLRCL                                                    
         MVI   SVCMLUFN,YESQ       Set 'until further notice' flag to Y         
         DROP  R1                                                               
*                                                                               
NXTTCM20 L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
NXTTCM23 CLI   0(R1),EOR           End of record?                               
         JE    NXTTCM80                                                         
         CLI   0(R1),X'20'         Product list element?                        
         JE    NXTTCM50                                                         
         CLI   0(R1),X'30'         Extra description element?                   
         JE    NXTTCM30                                                         
         CLI   0(R1),X'A0'         Ad-ID element?                               
         JE    NXTTCM40                                                         
NXTTCMNX LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     NXTTCM23            Bump to next element                         
*                                                                               
         USING CMLDSCEL,R1                                                      
NXTTCM30 CLI   CMLDSCSQ,0                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS1,CMLDSC                                                  
         CLI   CMLDSCSQ,1                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS2,CMLDSC                                                  
         CLI   CMLDSCSQ,2                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS3,CMLDSC                                                  
         J     NXTTCMNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLADIEL,R1                                                      
NXTTCM40 MVC   SVCMLAID,CMLADID    AD-ID                                        
         J     NXTTCMNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLPRDEL,R1                                                      
NXTTCM50 CLI   CMLPRDS,X'FF'       All products?                                
         JNE   *+8                                                              
         OI    SVCMLFLG,CML_ALLQ   Set to download all products                 
*                                                                               
         LLC   RF,CMLPRDLN                                                      
         AHI   RF,-2               Number of binary product codes               
         CHI   RF,0                                                             
         JNH   NXTTCMNX                                                         
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   ELEM2(0),CMLPRDS   Save list of binary prod codes                
         EX    RF,0(RE)                                                         
*                                                                               
         LA    RF,ELEM2            Current prds for this commercial             
NXTTCM52 CLI   0(RF),X'00'         End of current prds list?                    
         JE    NXTTCMNX                                                         
         CLI   0(RF),X'FF'         All product?                                 
         JE    NXTTCMNX                                                         
*                                                                               
         LA    RE,ELEM1            List of prds to be downloaded later          
NXTTCM54 CLI   0(RE),X'00'         Blank entry?                                 
         JE    NXTTCM56                                                         
         CLC   0(1,RF),0(RE)       Already in table?                            
         JE    NXTTCM58                                                         
         AHI   RE,1                                                             
         J     NXTTCM54                                                         
NXTTCM56 MVC   0(1,RE),0(RF)                                                    
NXTTCM58 AHI   RF,1                Bump to next current prd list                
         J     NXTTCM52                                                         
         DROP  R1                                                               
*                                                                               
NXTTCM80 DS    0H                  To extract more fields if needed             
         LA    R0,3                3 description fields                         
         LA    RE,SVCMLDS1         1st description                              
NXTTCM85 OC    0(L'SVCMLDS1,RE),0(RE)                                           
         JZ    NXTTCM87                                                         
         TR    0(L'SVCMLDS1,RE),TRTAB Translate data                            
NXTTCM87 LA    RE,L'SVCMLDS1(RE)   Bump to next desc field                      
         JCT   R0,NXTTCM85                                                      
*                                                                               
         MVC   LP_ADATA,ATCMREC    Point to traffic commercial record           
         J     EXITY                                                            
*                                                                               
NXTTCMSQ GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOSPTDIR+IO3'                            
         J     NXTTCM10                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTTHC   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTTHCSQ                                                         
         XC    SVCMLFLS(SVCMLFLQ),SVCMLFLS   Init saved commercial flds         
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLHDF+0,X'0A'     Passive for HiDef commercials                 
         MVI   CMLHDF+1,X'C2'                                                   
         MVC   CMLKAM,QMEDX                                                     
         MVC   CMLKCLT,QCLTX                                                    
         MVC   CMLHDFID,CMLPRFXS                                                
*                                                                               
NXTTHCHI GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         DROP  RE                                                               
*                                                                               
NXTTHC10 LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         CLC   IOKEY(CMLHDFID-CMLKEY),IOKEYSAV  Same upto the client?           
         JNE   NOMORE                          No, we're done                   
         OC    CMLHDFID,CMLHDFID   Have commercial ID?                          
         JZ    NXTTHCSQ            No                                           
         CLC   =X'C5DCC5DCC5B80000',CMLHDFID   House for the Client?            
         JE    NXTTCMSQ              Yes, this is TRPACK of C'99999999'         
*                                                                               
         CLC   CMLHDFID,CMLPRFXE   Past the last?                               
         JH    NOMORE              Then no more commercials to look for         
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
         JNE   NXTTHCSQ                                                         
         XC    SVCMLAID,SVCMLAID   Init saved commercial fields                 
         XC    SVCMLDS1,SVCMLDS1                                                
         XC    SVCMLDS2,SVCMLDS2                                                
         XC    SVCMLDS3,SVCMLDS3                                                
         MVI   SVCMLUFN,0                                                       
         XC    ELEM2,ELEM2         List of prds for this commercial             
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
         USING CMLDTAEL,R1                                                      
         TM    CMLSTAT,X'80'       Deleted cml                                  
         JO    NXTTHCSQ            Yes, next record then                        
         MVC   SVCMLDS1(L'CMLTITLE),CMLTITLE  Save title in case no x30         
         OC    STRDATEB(L'STRDATEB*2),STRDATEB   Any date filter?               
         JZ    NXTTHC15                          None, accept all cmmls         
         CLC   ENDDATEB,CMLRLSE    End date filter before release date?         
         JL    NXTTHCSQ            Yes, next record then                        
         CLC   STRDATEB,CMLRCL     Start date after recall date?                
         JH    NXTTHCSQ            Yes, next record then                        
*                                                                               
NXTTHC15 CLC   CMLRCL,=X'FFFFFF'   Recall date 'until further notice'?          
         JNE   NXTTHC20                                                         
         XC    CMLRCL,CMLRCL                                                    
         MVI   SVCMLUFN,YESQ       Set 'until further notice' flag to Y         
         DROP  R1                                                               
*                                                                               
NXTTHC20 L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
NXTTHC23 CLI   0(R1),EOR           End of record?                               
         JE    NXTTHC80                                                         
         CLI   0(R1),X'20'         Product list element?                        
         JE    NXTTHC50                                                         
         CLI   0(R1),X'24'         Extended data element?                       
         JE    NXTTHC40                                                         
         CLI   0(R1),X'30'         Extra description element?                   
         JE    NXTTHC30                                                         
         CLI   0(R1),X'A0'         Ad-ID element?                               
         JE    NXTTHC40                                                         
NXTTHCNX LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     NXTTHC23            Bump to next element                         
*                                                                               
         USING CMLDSCEL,R1                                                      
NXTTHC30 CLI   CMLDSCSQ,0                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS1,CMLDSC                                                  
         CLI   CMLDSCSQ,1                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS2,CMLDSC                                                  
         CLI   CMLDSCSQ,2                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS3,CMLDSC                                                  
         J     NXTTHCNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLADIEL,R1                                                      
NXTTHC40 MVC   SVCMLAID,CMLADID    Ad-Id                                        
         J     NXTTHCNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLPRDEL,R1                                                      
NXTTHC50 CLI   CMLPRDS,X'FF'       All products?                                
         JNE   *+8                                                              
         OI    SVCMLFLG,CML_ALLQ   Set to download all products                 
*                                                                               
         LLC   RF,CMLPRDLN                                                      
         AHI   RF,-2               Number of binary product codes               
         CHI   RF,0                                                             
         JNH   NXTTHCNX                                                         
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   ELEM2(0),CMLPRDS   Save list of binary prod codes                
         EX    RF,0(RE)                                                         
*                                                                               
         LA    RF,ELEM2            Current prds for this commercial             
NXTTHC52 CLI   0(RF),X'00'         End of current prds list?                    
         JE    NXTTHCNX                                                         
         CLI   0(RF),X'FF'         All product?                                 
         JE    NXTTHCNX                                                         
*                                                                               
         LA    RE,ELEM1            List of prds to be downloaded later          
NXTTHC54 CLI   0(RE),X'00'         Blank entry?                                 
         JE    NXTTHC56                                                         
         CLC   0(1,RF),0(RE)       Already in table?                            
         JE    NXTTHC58                                                         
         AHI   RE,1                                                             
         J     NXTTHC54                                                         
NXTTHC56 MVC   0(1,RE),0(RF)                                                    
NXTTHC58 AHI   RF,1                Bump to next current prd list                
         J     NXTTHC52                                                         
         DROP  R1                                                               
*                                                                               
NXTTHC80 DS    0H                  To extract more fields if needed             
*                                                                               
         MVC   LP_ADATA,ATCMREC    Point to traffic commercial record           
         J     EXITY                                                            
*                                                                               
NXTTHCSQ GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOSPTDIR+IO3'                            
         J     NXTTHC10                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTTCL   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTTCL30                                                         
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    WORK2,WORK2         Init client name for output                  
         MVC   WORK2(L'CNAME),CNAME                                             
         J     NXTTCL70                                                         
         DROP  RE                                                               
*                                                                               
NXTTCL30 DS    0H                  To process more client rec if needed         
         J     NOMORE                                                           
*                                                                               
NXTTCL70 DS    0H                  To extract more fields if needed             
*                                                                               
         MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTTPR   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTTPR30                                                         
         TM    SVCMLFLG,CML_ALLQ   Download all products?                       
         JZ    NXTTPR18                                                         
NXTTPR10 L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    ELEM1,ELEM1         Init table of binary product codes           
         LA    R1,CLIST                                                         
         LA    R2,ELEM1                                                         
         LA    RF,220                                                           
NXTTPR12 CLI   3(R1),0             No more binary product code?                 
         JE    NXTTPR18                                                         
         MVC   0(1,R2),3(R1)       Save binary product code to look up          
         AHI   R2,1                                                             
         AHI   R1,4                                                             
         JCT   RF,NXTTPR12                                                      
         LA    R1,CLIST2           2nd product list                             
         LA    RF,35                                                            
NXTTPR14 CLI   3(R1),0             No more binary product code?                 
         JE    NXTTPR18                                                         
         MVC   0(1,R2),3(R1)       Save binary product code to look up          
         AHI   R2,1                                                             
         AHI   R1,4                                                             
         JCT   RF,NXTTPR14                                                      
NXTTPR18 LA    R1,ELEM1                                                         
         MVI   ELEM1+255,X'FF'     Mark end of binary prd codes table           
         ST    R1,FULL1            Address of binary prd codes to proc          
         DROP  RE                                                               
*                                                                               
NXTTPR30 L     R2,FULL1            Point to binary prd table                    
         CLI   0(R2),X'FF'         End of table?                                
         JE    NOMORE                                                           
         CLI   0(R2),X'00'         Blank entry?                                 
         JE    NXTTPR38                                                         
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST            Point to 1st list of products                
         LA    R1,220                                                           
         CLC   0(1,R2),3(RF)       Binary prd code match that of list?          
         JE    NXTTPR36                                                         
         AHI   RF,4                                                             
         JCT   R1,*-14                                                          
         LA    RF,CLIST2           Point to 2nd list of products                
         LA    R1,35                                                            
         CLC   0(1,R2),3(RF)       Binary prd code match that of list?          
         JE    NXTTPR36                                                         
         AHI   RF,4                                                             
         JCT   R1,*-14                                                          
         DROP  RE                                                               
*                                                                               
NXTTPR36 XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING PKEY,RE                                                          
         MVI   PKEYTYPE,PKEYTYPQ                                                
         MVC   PKEYAM,QMEDX                                                     
         MVC   PKEYCLT,QCLTX                                                    
         MVC   PKEYPRD,0(RF)       Character format prd code from table         
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         CLC   IOKEY(L'PKEY),IOKEYSAV                                           
         JNE   NXTTPR38                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
         JNE   NXTTPR38                                                         
         AHI   R2,1                Point to next entry in table                 
         ST    R2,FULL1            For next round                               
         J     NXTTPR50            Now extract prd fields for output            
*                                                                               
NXTTPR38 AHI   R2,1                Point to next entry in table                 
         ST    R2,FULL1                                                         
         J     NXTTPR30            Get next product record                      
*                                                                               
NXTTPR50 L     R1,IOADDR                                                        
         USING PRDHDR,R1                                                        
         XC    QPRDA,QPRDA         Return prd code - character                  
         XC    WORK2,WORK2         Return prd name                              
         XC    DUB2,DUB2           Return Talent agency code                    
         MVC   QPRDA,PKEYPRD                                                    
         MVC   WORK2(L'PNAME),PNAME                                             
         MVC   DUB2(L'PTALAGY),PTALAGY                                          
         DROP  R1                                                               
*                                                                               
NXTTPR70 DS    0H                  To extract more fields if needed             
*                                                                               
         MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Traffic production house record download                            *         
***********************************************************************         
                                                                                
REQTHOU  LKREQ H,M#TRHOUS,OUTTPH,NEXTREQ=REQTRCP                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=NE#MED                        
CltCd    LKREQ F,002,(D,B#WORKD,QCLTA),CHAR,TEXT=NE#CLI                         
         LKREQ E                                                                
                                                                                
OUTTPH   LKOUT H                   Traffic prod house output maps start         
         LKOUT R,1                                                              
PRout    LKOUT P,,TPHINI           Init traffic prod house arrary               
Array    LKOUT C,1,(A,ARYTPH)                                                   
         LKOUT X                   Traffic prod house output maps end           
                                                                                
TPHINI   OC    MEDCOD,SPACES                                                    
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,AGYMEDX                          
         JNE   EXITN                                                            
         OC    QCLTA,SPACES                                                     
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
         MVC   TPHRECQ,=X'0A29'    Traffic prod house record code               
         J     EXITY                                                            
                                                                                
ARYTPH   LKOUT A,(R,NXTTPH),MULTIROW=Y,ROWNAME=PRHRECD                          
Media    LKOUT C,001,(D,B#SAVED,MEDCOD),CHAR                                    
PHoID    LKOUT C,002,PRHKPRH,CHAR                                               
Array    LKOUT C,003,(A,ARYPHAD)                                                
Array    LKOUT C,007,(A,ARYPHFX)                                                
Array    LKOUT C,012,(A,ARYPHOP)                                                
Array    LKOUT C,014,(A,ARYPHCM)                                                
         LKOUT E                                                                
                                                                                
ARYPHAD  LKOUT A,(D,B#TPHREC,PRHDTAEL),EOT=EOR,ROWID=(PRHDTAEL,X'10'), +        
               ROWWIDTH=(V,PRHDTALN)                                            
         LKOUT C,003,PRHLINE1,CHAR,ND=Y                                         
         LKOUT C,004,PRHLINE2,CHAR,ND=Y                                         
         LKOUT C,005,PRHLINE3,CHAR,ND=Y                                         
         LKOUT C,006,PRHLINE4,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYPHFX  LKOUT A,(D,B#TPHREC,PRHDTAEL),EOT=EOR,ROWID=(PRHFAXEL,X'20'), +        
               ROWWIDTH=(V,PRHFAXLN)                                            
         LKOUT C,007,PRHFTEL1,CHAR,ND=Y                                         
         LKOUT C,008,PRHFTELA,CHAR,ND=Y                                         
         LKOUT C,009,PRHFTELE,CHAR,ND=Y                                         
         LKOUT C,010,PRHFTELN,CHAR,ND=Y                                         
         LKOUT C,011,PRHFTELX,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYPHOP  LKOUT A,(D,B#TPHREC,PRHDTAEL),EOT=EOR,ROWID=(PRHOPCEL,X'25'), +        
               ROWWIDTH=(V,PRHOPCLN)                                            
         LKOUT C,012,PRHOPC,CHAR,ND=Y                                           
         LKOUT C,013,PRHDELAY,UBIN,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYPHCM  LKOUT A,(D,B#TPHREC,PRHDTAEL),EOT=EOR,ROWID=(PRHCMTEL,X'40'), +        
               ROWWIDTH=(V,PRHCMTLN)                                            
         LKOUT C,014,PRHCMTNO,UBIN,ND=Y                                         
         LKOUT C,015,PRHCMT,CHAR,LEN=V,ND=Y                                     
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Send production house values                                        *         
***********************************************************************         
NXTTPH   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTTPH50                                                         
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLKID+0,X'0A'                                                   
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,QMEDX                                                     
         MVC   CMLKCLT,QCLTX                                                    
         MVC   CMLKCML,=C'99999999'  99999999 Commercial tells us house         
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         JNE   NOMORE                          No, we're done                   
*                                                                               
         CLC   IOKEY(CMLKSTAT-CMLKEY),IOKEYSAV Exact match?                     
         JNE   NOMORE                          No, we're done                   
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NOMORE                                                           
*                                                                               
         L     RE,IOADDR                                                        
         USING CMLKEY,RE                                                        
         MVC   HOUSECD,CMLTITLE                                                 
         OC    HOUSECD,SPACES                                                   
         DROP  RE                                                               
*                                                                               
NXTTPH50 GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',TPHKEYT),('B#TPHREC',0), +        
               ('$NXTRSPT',SAVED),0,0                                           
         JNE   EXITY                                                            
         L     R2,IOADDR           Put filtering logic here...                  
         USING PRHRECD,R2                                                       
         J     EXITY               Exit to send prod house values               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* Traffic Net Recap record download                                             
***********************************************************************         
*                                                                               
REQTRCP  LKREQ H,M#TRNRCP,OUTRCP,NEXTREQ=REQCLPH                                
MedCd    LKREQ F,001,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,MAXLEN=L'QMEDX,TEXT=NE#MED,COL=*                    
CltCd    LKREQ F,002,(D,B#WORKD,QCLTX),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'QCLTX,MAXLEN=L'QCLTA,TEXT=NE#CLI,COL=*                    
PrdCode  LKREQ F,003,(D,B#SAVED,PRDCODE),CHAR,TEXT=NE#PCOD                      
SDate    LKREQ F,004,(D,B#SAVED,STRDATEB),BDAT,TEXT=NE#SDATE,COL=*              
EDate    LKREQ F,005,(D,B#SAVED,ENDDATEB),BDAT,TEXT=NE#EDATE,COL=*              
         LKREQ E                                                                
                                                                                
OUTRCP   LKOUT H                   Traffic Net Recap output maps                
                                                                                
         LKOUT R,X'0023'                                                        
PRout    LKOUT P,,TRCPINI          Init traffic Net Recap arrary                
Array    LKOUT C,X'0023',(A,ARYTRCP)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                   Traffic net recap output maps end            
                                                                                
ARYTRCP  LKOUT A,(R,NXTRCP),MULTIROW=Y,ROWNAME=NRCPRECD                         
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#WORKD,IOKEY),(R,EDCML)                                
SDate    LKOUT C,004,(D,B#SAVED,STRDATEB),BDAT                                  
EDate    LKOUT C,005,(D,B#SAVED,ENDDATEB),BDAT                                  
Network  LKOUT C,006,(D,B#SAVED,NETWORK),CHAR                                   
SubMed   LKOUT C,007,(D,B#SAVED,SUBMED),CHAR                                    
Progcd   LKOUT C,008,(D,B#SAVED,PROGCDE),CHAR,ND=Y                              
Prognm   LKOUT C,009,(D,B#SAVED,PROGNME),CHAR,ND=Y                              
Progedt  LKOUT C,010,(D,B#SAVED,PROGEDTE),BDAT,ND=Y                             
Array    LKOUT C,X'0023',(A,ARYTRCA),FILTROUT=TSTADID                           
Array    LKOUT C,X'0023',(A,ARYTRCH),FILTROUT=TSTHIDF                           
         LKOUT E                                                                
*                                                                               
ARYTRCA  LKOUT A,(D,B#SAVED,QVALUES),NEWEL=Y,NROWS=1                            
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#SAVED,SVCMLAD),CHAR                                   
SDate    LKOUT C,004,(D,B#SAVED,STRDATEB),BDAT                                  
EDate    LKOUT C,005,(D,B#SAVED,ENDDATEB),BDAT                                  
Network  LKOUT C,006,(D,B#SAVED,NETWORK),CHAR                                   
SubMed   LKOUT C,007,(D,B#SAVED,SUBMED),CHAR                                    
Progcd   LKOUT C,008,(D,B#SAVED,PROGCDE),CHAR,ND=Y                              
Prognm   LKOUT C,009,(D,B#SAVED,PROGNME),CHAR,ND=Y                              
Progedt  LKOUT C,010,(D,B#SAVED,PROGEDTE),BDAT,ND=Y                             
         LKOUT E                                                                
*                                                                               
ARYTRCH  LKOUT A,(D,B#SAVED,QVALUES),NEWEL=Y,NROWS=1                            
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#SAVED,SVCMLHD),CHAR                                   
SDate    LKOUT C,004,(D,B#SAVED,STRDATEB),BDAT                                  
EDate    LKOUT C,005,(D,B#SAVED,ENDDATEB),BDAT                                  
Network  LKOUT C,006,(D,B#SAVED,NETWORK),CHAR                                   
SubMed   LKOUT C,007,(D,B#SAVED,SUBMED),CHAR                                    
Progcd   LKOUT C,008,(D,B#SAVED,PROGCDE),CHAR,ND=Y                              
Prognm   LKOUT C,009,(D,B#SAVED,PROGNME),CHAR,ND=Y                              
Progedt  LKOUT C,010,(D,B#SAVED,PROGEDTE),BDAT,ND=Y                             
         LKOUT E                                                                
*                                                                               
TRCPINI  DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    Get address of TRPACK                  
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VTRPACK,0(R1)                                                    
****                                                                            
*NOP     MVC   TRCRECQ,=X'0AE2'    Traffic passive net recap rec code           
*                                                                               
         MVC   TRCRECQ,=X'0A20'    Traffic Net Recap Rec Code                   
         J     EXITY                                                            
*                                                                               
TSTADID  DS    0H                                                               
         OC    SVCMLAD,SVCMLAD     Do we have an AD-ID?                         
         J     SETCCC                                                           
*                                                                               
TSTHIDF  DS    0H                                                               
         OC    SVCMLHD,SVCMLHD     Do we have a HiDef?                          
         J     SETCCC                                                           
*                                                                               
EDCML    LM    R2,R4,LP_AINP                                                    
         USING NRCPKEY,R2                                                       
*                                                                               
         XC    SVCMLAD,SVCMLAD     Init adid commercial field                   
         XC    SVCMLHD,SVCMLHD     Init hidef commercial field                  
*                                                                               
         XC    0(13,R4),0(R4)                                                   
         MVC   0(L'NRCPCML,R4),NRCPCML                                          
         TM    NRCPSTAT,NRCPSTP        Packed commercial ?                      
         JZ    EDCML01                                                          
         XC    0(13,R4),0(R4)                                                   
         GOTOR VTRPACK,DMCB,(C'U',NRCPCML),0(R4)                                
*****    MVC   SVCMLAD,0(R4)           Don't set this if code is Ad-ID          
*                                                                               
EDCML01  MVC   SVKEY,IOKEY         Save recap key                               
*                                                                               
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLKID+0,X'0A'     Commercial record X'0A21'                     
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,QMEDX                                                     
         MVC   CMLKCLT,QCLTX                                                    
         MVC   CMLKCML,NRCPCML                                                  
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         CLC   IOKEY(L'CMLKEY),IOKEYSAV                                         
         JE    EDCML01C                                                         
*                                                                               
         LA    RE,IOKEY                                                         
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   CMLKID+1,X'C1'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         CLC   IOKEY(L'CMLKEY),IOKEYSAV                                         
         JNE   EDCML10                                                          
*                                                                               
         DROP  RE                                                               
*                                                                               
EDCML01C GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
EDCML02  CLI   0(R1),EOR           End of record?                               
         JE    EDCML10                                                          
         CLI   0(R1),X'24'         Hidef element?                               
         JE    EDCML05                                                          
         CLI   0(R1),X'A0'         Ad-id element?                               
         JE    EDCML04                                                          
EDCMLNXT LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     EDCML02             Bump to next element                         
*                                                                               
         USING CMLADIEL,R1                                                      
EDCML04  CLC   CMLADID,0(R4)       Ad-ID is same as primary film?               
         JNE   EDCML04C             No, go save it                              
         L     R6,IOADDR                                                        
         CLC   CMLADIDP,5(R6)      Compare the packed values                    
         JE    EDCML10             Yes, no need to set SVCMLAD                  
*                                                                               
         MVC   SVCMLAD(8),5(R6)    Save 8 char iscii                            
         XC    SVCMLAD,0(R4)       and swap adid and iscii                      
         XC    0(12,R4),SVCMLAD    in the fields                                
         XC    SVCMLAD,0(R4)                                                    
         J     EDCML10                                                          
*                                                                               
EDCML04C MVC   SVCMLAD,CMLADID     Ad-id                                        
         J     EDCML10                                                          
*                                                                               
         USING CMLXDTEL,R1                                                      
EDCML05  MVC   SVCMLHD,CMLXHDEF    Ad-id                                        
         J     EDCMLNXT            bump to next element                         
*                                                                               
EDCML10  MVC   IOKEY,SVKEY         Restore recap key                            
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOXSPDIR+IO3'                            
*                                                                               
EDCMLX   LA    R0,12               Max length for commerical code               
         J     SETOLENX                                                         
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* Send net recap values                                                         
***********************************************************************         
NXTRCP   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTRCP40                                                         
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING NRCPKEY,RE                                                       
         MVI   NRCPKID+0,X'0A'     Traffic net recap                            
         MVI   NRCPKID+1,X'20'     key X'0A20'                                  
         MVC   NRCPKAM,QMEDX                                                    
         MVC   NRCPKCLT,QCLTX                                                   
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOXSPDIR+IO3'                            
*                                                                               
NXTRCP10 CLC   IOKEY(NRCPCML-NRCPKEY),IOKEYSAV Match on a/m/clt                 
         JNE   NOMORE                          No, we're done                   
*                                                                               
         XC    PROGCDE,PROGCDE                                                  
         XC    PROGNME,PROGNME                                                  
         XC    PROGEDTE,PROGEDTE                                                
*                                                                               
         LA    RE,IOKEY                                                         
         USING NRCPKEY,RE                                                       
         MVC   SVMED,NRCPSMED      Save submedia                                
         MVC   SVNET,NRCPNET       Save network                                 
*                                                                               
         CLI   NRCPSMED,C'S'       Submedia syndication?                        
         JE    NXTRCP20                                                         
         CLI   NRCPSMED,C'C'       Submedia cable?                              
         JE    NXTRCP20                                                         
         CLI   NRCPSMED,C'N'       Or network                                   
         JNE   NXTRCP40                                                         
*                                                                               
NXTRCP20 OC    PRDCODE,PRDCODE     Any prd entered?                             
         JZ    NXTRCP25                                                         
         CLC   NRCPPROD,PRDCODE                                                 
         JE    NXTRCP25            Found match                                  
         CLC   NRCPPRO2,PRDCODE                                                 
         JNE   NXTRCP40                                                         
         DROP  RE                                                               
*                                                                               
NXTRCP25 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOXSPFIL+IO3'                           
         JNE   NXTRCP40                                                         
*                                                                               
         L     R3,IOADDR                                                        
         USING NRCPRECD,R3                                                      
*                                                                               
         MVI   DATADISP+1,42       Displacement to first elem                   
         MVI   ELCODE,X'10'        Commercial Data Elem                         
         BRAS  RE,GETEL                                                         
         JNE   NXTRCP40                                                         
         JE    NXTRCP30                                                         
*                                                                               
NXTRCP27 BRAS  RE,NEXTEL                                                        
         JNE   NXTRCP40                                                         
         DROP  R3                                                               
*                                                                               
* See if within requested dates                                                 
         USING NRCPDTEL,R3                                                      
NXTRCP30 CLC   NRCPFTD,ENDDATEB                FTD to end date                  
         JH    NXTRCP27                                                         
         CLC   NRCPLTD,STRDATEB                LTD to start date                
         JL    NXTRCP27                                                         
         DROP  R3                                                               
*                                                                               
         CLI   SVMED,C'S'          Syndication?                                 
         JNE   NXTRCPEQ                                                         
*                                                                               
         MVC   NETWORK,SVNET       Save network                                 
*NOP     JNE   NXTRCP60                                                         
                                                                                
         L     R3,IOADDR                                                        
         USING NRCPRECD,R3                                                      
*                                                                               
         MVI   DATADISP+1,42       Displacement to first elem                   
         MVI   ELCODE,X'20'        Syndication elem                             
         BRAS  RE,GETEL                                                         
         JNE   NXTRCPEQ                                                         
*NOP     JNE   NXTRCP60                                                         
         DROP  R3                                                               
*                                                                               
         USING NRCPSNEL,R3                                                      
         MVC   PROGCDE,NRCPPRG     Program code                                 
         CLI   NRCPSNLN,NRCPOLN    Old elem len (no end date)                   
         JE    *+10                                                             
         MVC   PROGEDTE,NRCPEDTE   Program end date                             
         DROP  R3                                                               
*                                                                               
         MVC   SVKEY,IOKEY         Save recap key                               
*                                                                               
*Read station record for network market number                                  
         MVI   IOKEY,C'0'                                                       
         MVC   IOKEY+1(16),IOKEY                                                
         LA    R3,IOKEY                                                         
         USING STARECD,R3                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),NETWORK                                              
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,QAGY        Agency alpha                                 
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSTAFIL+IO7'                            
         CLC   IOKEY(STAKEYLN),IOKEYSAV                                         
         JNE   NXTRCP60                                                         
*                                                                               
         L     R3,AIO7                                                          
         PACK  DUB,SMKT(4)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF2          NETWORK MARKET                               
         DROP  R3                                                               
*                                                                               
         GOTO1 VDATCON,DMCB,(3,STRDATEB),(2,HALF)                               
*                                                                               
         LA    RE,IOKEY                                                         
         USING NPGKEY,RE                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,QMEDX                                                     
         MVC   NPGKNET,HALF2       NETWORK MARKET                               
         MVC   NPGKPROG,PROGCDE                                                 
         MVC   NPGKEND,HALF        END DATE                                     
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(11),IOKEYSAV                                               
         JNE   NXTRCP60                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO7'                           
         JNE   NXTRCP60                                                         
                                                                                
         L     R3,IOADDR           Put filtering logic here...                  
         USING NRCPRECD,R3                                                      
*                                                                               
         MVI   DATADISP+1,24       Displacement to first elem                   
         MVI   ELCODE,X'92'                                                     
         BRAS  RE,GETEL                                                         
         JNE   NXTRCP60                                                         
         DROP  R3                                                               
*                                                                               
         USING NPGEL92,R3                                                       
         MVC   PROGNME,NPGNAME     Program name                                 
         J     NXTRCP60                                                         
         DROP  R3                                                               
*                                                                               
NXTRCP40 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOXSPDIR+IO3'                            
         J     NXTRCP10                                                         
*                                                                               
NXTRCP60 MVC   IOKEY,SVKEY         Restore recap key                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOXSPDIR+IO3'                            
*                                                                               
NXTRCPEQ DS    0H                                                               
         MVC   NETWORK,SVNET       Save network                                 
         MVC   SUBMED,SVMED        and submedia                                 
*                                                                               
NXTRCPX  MVC   LP_ADATA,ARCPREC    Point to traffic recap record                
         J     EXITY               Exit to send net recap values                
         EJECT                                                                  
                                                                                
*                                                                               
***********************************************************************         
* Traffic client production house download                                      
***********************************************************************         
                                                                                
REQCLPH  LKREQ H,M#TRCPHS,OUTCPH,NEXTREQ=REQCML                                 
MedCd    LKREQ F,001,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,MAXLEN=L'QMEDX,TEXT=NE#MED,COL=*                    
CltCd    LKREQ F,002,(I,B#SAVED,QCLTIND),CHAR,LIST=F,                  +        
               OLEN=L'QCLTA,MAXLEN=L'QCLTA,TEXT=NE#CLI,COL=*                    
VndCd    LKREQ F,003,(I,B#SAVED,QVNDIND),CHAR,LIST=F,OLEN=3,           +        
               MAXLEN=3,TEXT=(*,TVENLIT),COL=*                                  
         LKREQ E                                                                
                                                                                
TVENLIT  DC    C'Vendor code(s)'                                                
                                                                                
OUTCPH   LKOUT H                   Traffic prod house output maps start         
         LKOUT R,X'0030'                                                        
PRout    LKOUT P,,CPHINI           Init traffic prod house arrary               
Array    LKOUT C,1,(A,ARYCPH),FILTROUT=SHWVLDH                                  
         LKOUT X                   Traffic prod house output maps end           
*                                                                               
ARYCPH   LKOUT A,(R,NXTCPH),MULTIROW=Y,ROWNAME=PRHRECD                          
Client   LKOUT C,2,(D,B#WORKD,QCLTA),CHAR,ND=Y                                  
PHoID    LKOUT C,3,(D,B#SAVED,HOUSECD),CHAR,ND=Y                                
Vendor   LKOUT C,4,(D,B#SAVED,SVVND),CHAR,ND=Y                                  
CltNm    LKOUT C,5,(D,B#WORKD,WORK2),CHAR                                       
         LKOUT E                                                                
*                                                                               
*                                                                               
SHWVLDH  CLI   SHWVLD,C'Y'         Valid request                                
         BR    RE                                                               
*                                                                               
CPHINI   MVI   SHWVLD,C'N'         Init request is invalid                      
         OC    MEDCOD,SPACES                                                    
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         JNE   EXITN                                                            
*                                                                               
         MVC   AGYMEDX,QMEDX       For prod house key driver                    
*                                                                               
         XC    QCLTX,QCLTX         Init if all client request                   
         SR    RE,RE                                                            
         ICM   RE,7,QACLT                                                       
         JZ    CPHINI10            All client request                           
*                                                                               
         MVC   NUMCLT,LW_NUMN-LW_D(RE) Number of clients to process             
         AHI   RE,LW_LN2Q          Point to first client                        
         ST    RE,ANXTCLT          Set A(client request)                        
*                                                                               
         MVC   QCLTA,0(RE)                                                      
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    WORK2,WORK2                                                      
         MVC   WORK2(L'CNAME),CNAME                                             
         DROP  RE                                                               
*                                                                               
CPHINI10 SR    RE,RE                                                            
         ICM   RE,7,AVND           Any vendor                                   
         JZ    NOMORE              This should never happen                     
*                                                                               
         MVC   NUMVND,LW_NUMN-LW_D(RE) Number of vendors                        
         AHI   RE,LW_LN2Q          Point to first vendor                        
         ST    RE,ANXTVND          Set A(vendor)                                
*                                                                               
         MVI   SHWVLD,C'Y'         Valid request                                
*                                                                               
         MVC   TPHRECQ,=X'0A29'    Traffic prod house record code               
         J     EXITY                                                            
***********************************************************************         
* Send production house values                                        *         
***********************************************************************         
NXTCPH   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCPH28                                                         
*                                                                               
         MVC   IOKEY(L'CMLKEY),SVKEY Restore last cml key                       
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLKID+0,X'0A'                                                   
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,QMEDX                                                     
         OC    QCLTX,QCLTX         All client request?                          
         JZ    NXTCPH05            Yes                                          
         MVC   CMLKCLT,QCLTX                                                    
         MVC   CMLKCML,=C'99999999'  99999999 Commercial tells us house         
         DROP  RE                                                               
*                                                                               
NXTCPH05 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         JNE   NOMORE                          No, we're done                   
*                                                                               
         OC    QCLTX,QCLTX         If blank, all client request                 
         JNZ   NXTCPH20                                                         
*                                                                               
NXTCPH10 CLC   IOKEY(3),IOKEYSAV   Same rec type/agency/media                   
         JNE   NOMORE                                                           
*                                 Got the client for all clt request            
         CLC   =C'99999999',IOKEY+(CMLKCML-CMLKEY)                              
         JE    NXTCPH22                                                         
*                                 Got the client for all clt request            
         MVC   IOKEY+(CMLKCML-CMLKEY),=C'99999999'  Chk for house rec           
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         JNE   NXTCPH40                                                         
         J     NXTCPH10                                                         
*                                                                               
NXTCPH20 CLC   IOKEY(CMLKSTAT-CMLKEY),IOKEYSAV Exact match?                     
         JNE   NXTCPH30            Skip to next client                          
*                                                                               
NXTCPH22 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NXTCPH30            Skip to next client                          
*                                                                               
         L     RE,IOADDR                                                        
         USING CMLKEY,RE                                                        
         MVC   HOUSECD,CMLTITLE                                                 
         OC    HOUSECD,SPACES                                                   
*                                                                               
         MVC   SVKEY,IOKEY         Save commercial key                          
         J     NXTCPH50                                                         
         DROP  RE                                                               
*                                                                               
NXTCPH28 MVC   IOKEY(L'CMLKEY),SVKEY Restore last cml key                       
*                                                                               
NXTCPH30 OC    QCLTX,QCLTX                                                      
         JZ    NXTCPH40                                                         
*                                                                               
* Get next client from list                                                     
         L     RE,ANXTCLT                                                       
         LA    RE,L'QCLTA(RE)      Point to next clt                            
         ST    RE,ANXTCLT                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NUMCLT         R0=remaining client count                    
         JZ    NOMORE              Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMCLT                                                      
*                                                                               
         MVC   QCLTA,0(RE)                                                      
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    WORK2,WORK2                                                      
         MVC   WORK2(L'CNAME),CNAME                                             
         DROP  RE                                                               
                                                                                
         MVC   IOKEY+(CMLKCLT-CMLKEY)(L'QCLTX),QCLTX                            
         MVC   IOKEY+(CMLKCML-CMLKEY)(L'CMLKCML),=C'99999999'                   
         J     NXTCPH05                                                         
*                                                                               
* Skip to next client                                                           
NXTCPH40 MVC   IOKEY+(CMLKCML-CMLKEY)(3),=X'FFFFFF'                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         JNE   NOMORE                          No, we're done                   
         J     NXTCPH10                                                         
*                                                                               
* Read production house record for Optica vendors                               
NXTCPH50 XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING PRHKEY,RE                                                        
         MVI   PRHKID+0,X'0A'                                                   
         MVI   PRHKID+1,X'29'                                                   
         MVC   PRHKAM,QMEDX                                                     
         MVC   PRHKPRH,HOUSECD                                                  
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
         JNE   NXTCPH28            Skip to next client                          
*                                                                               
         CLC   IOKEY(13),IOKEYSAV Exact match?                                  
         JNE   NXTCPH28            Skip to next client                          
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO7'                           
         JNE   NXTCPH28            Skip to next client                          
                                                                                
         L     R2,IOADDR           Put filtering logic here...                  
         USING PRHRECD,R2                                                       
*                                                                               
         LR    R3,R2                                                            
         MVI   DATADISP+1,24       Displacement to first elem                   
         MVI   ELCODE,X'25'                                                     
         BRAS  RE,GETEL                                                         
         JNE   NXTCPH28            Skip to next client                          
*                                                                               
         USING PRHOPCEL,R3                                                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NUMVND         Vendor count                                 
*                                                                               
         L     RE,ANXTVND                                                       
NXTCPH55 CLC   PRHOPC,0(RE)        Match on vendor                              
         JE    NXTCPHX                                                          
         LA    RE,L'PRHOPC(RE)     Point to next vendor                         
         JCT   R0,NXTCPH55                                                      
         J     NXTCPH28            Skip to next client                          
*                                                                               
NXTCPHX  MVC   SVVND,PRHOPC                                                     
         OC    QCLTX,QCLTX                                                      
         JNZ   NXTCPHX2                                                         
         DROP  R3                                                               
*                                                                               
         MVC   QCLTX,SVKEY+(CMLKCLT-CMLKEY)                                     
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
         GOTOR VCLUNPK,DMCB,(CPROF+6,QCLTX),QCLTA                               
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
         XC    WORK2,WORK2                                                      
         MVC   WORK2(L'CNAME),CNAME                                             
         DROP  R2                                                               
         XC    QCLTX,QCLTX         All client request                           
*                                                                               
NXTCPHX2 DS    0H                                                               
         MVC   LP_ADATA,AIO3                                                    
         J     EXITY               Exit to send prod house values               
*                                                                               
*                                                                               
***********************************************************************         
* Traffic commercial record download for list and display             *         
***********************************************************************         
*                                                                               
REQCML   LKREQ H,M#TCMLDL,OUTCML,NEXTREQ=REQPROF                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=NE#MED                        
CltCd    LKREQ F,002,(I,B#SAVED,QCLTIND),CHAR,LIST=F,                  +        
               OLEN=L'QCLTA,MAXLEN=L'QCLTA,TEXT=NE#CLI,COL=*                    
CmlPrefx LKREQ F,003,(D,B#SAVED,CMLPREFX),CHAR,TEXT=NE#CMML,COL=*               
SDate    LKREQ F,004,(D,B#SAVED,STRDATEB),BDAT,TEXT=NE#SDATE,COL=*              
EDate    LKREQ F,005,(D,B#SAVED,ENDDATEB),BDAT,TEXT=NE#EDATE,COL=*              
RecAct   LKREQ F,100,(D,B#SAVED,RECACT),CHAR,TEXT=(*,RECALIT),COL=*             
         LKREQ E                                                                
                                                                                
RECALIT  DC    C'ACTION'                                                        
                                                                                
OUTCML   LKOUT H                   Traffic commercial output maps start         
                                                                                
         LKOUT R,X'97'                                                          
PRout    LKOUT P,,CMLINI           Init traffic commercial arrary               
                                                                                
Array    LKOUT C,1,(A,ARYCML),FILTROUT=SHWVLDC                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                   Traffic commercial output maps end           
                                                                                
ARYCML   LKOUT A,(R,NXTCML),MULTIROW=Y,ROWNAME=CMLRECD                          
Array    LKOUT C,X'97',(A,ARYCMLD),FILTROUT=SHWCMLC                             
Array    LKOUT C,2,(A,ARYCCL),FILTROUT=SHWCLTC                                  
         LKOUT E                                                                
                                                                                
ARYCMLD  LKOUT A,(R,SETCMLD),MULTIROW=Y,ROWNAME=CMLRECD                         
Media    LKOUT C,001,(D,B#SAVED,MEDCOD),CHAR                                    
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#WORKD,IOKEY),(R,EDCML1)                               
Array    LKOUT C,004,(A,ARYCPR)                                                 
CmlT1    LKOUT C,005,(D,B#SAVED,SVCMLDS1),CHAR,ND=Y                             
CmlT2    LKOUT C,006,(D,B#SAVED,SVCMLDS2),CHAR,ND=Y                             
CmlT3    LKOUT C,007,(D,B#SAVED,SVCMLDS3),CHAR,ND=Y                             
Cksum    LKOUT C,200,(D,B#SAVED,SVCKSM),HEXD,ND=Y                               
Array    LKOUT C,008,(A,ARYCDS)                                                 
         LKOUT E                                                                
                                                                                
ARYCCL   LKOUT A,(R,NXTCL),MULTIROW=Y,ROWNAME=CLTRECD                           
Array    LKOUT C,2,(A,ARYTC2),FILTROUT=TSTCMLC                                  
Array    LKOUT C,3,(A,ARYTPR1),FILTROUT=SHWPRDC                                 
         LKOUT E                                                                
*                                                                               
SHWVLDC  CLI   SHWVLD,C'Y'         Valid request                                
         BR    RE                                                               
*                                                                               
SHWCMLC  CLI   SHWCML,C'Y'         Show commercial?                             
         BR    RE                                                               
*                                                                               
SHWCLTC  CLI   SHWCLT,C'Y'         Show client?                                 
         BR    RE                                                               
*                                                                               
SHWPRDC  CLI   SHWPRD,C'Y'         Show prods?                                  
         BR    RE                                                               
*                                                                               
TSTCMLC  CLI   CMLDLSW1,YESQ       Commercial record downloaded?                
         BR    RE                                                               
*                                                                               
ARYCPR   LKOUT A,(I,B#SAVED,ACMLPR),NROWS=85,ROWWIDTH=3,               +        
               ROWNAME=DUMMY_D                                                  
PrdCd    LKOUT C,004,DUM_LIN1,(R,EDCPR),ND=Y                                    
         LKOUT E                                                                
*                                                                               
ARYCDS   LKOUT A,(D,B#TCMREC,CMLDTAEL),EOT=EOR,ROWID=(CMLDTAEL,X'10'), +        
               ROWWIDTH=(V,CMLDTALN)                                            
         LKOUT C,008,CMLSLN,UBIN                                                
         LKOUT C,009,CMLRLSE,BDAT                                               
         LKOUT C,010,CMLRCL,BDAT,ND=Y                                           
         LKOUT C,011,(D,B#SAVED,SVCMLUFN),CHAR,ND=Y                             
         LKOUT C,012,CMLSOLO,CHAR,ND=Y                                          
         LKOUT C,013,(D,B#SAVED,SVPARENT),CHAR,ND=Y                             
         LKOUT C,014,(D,B#SAVED,SVMSTIME),CHAR,ND=Y                             
         LKOUT C,015,(D,B#SAVED,SVMETIME),CHAR,ND=Y                             
         LKOUT C,016,(D,B#SAVED,SVDDATE),BDAT,ND=Y                              
         LKOUT C,017,(D,B#SAVED,SVDTIME),CHAR,ND=Y                              
         LKOUT C,018,CMLOVRD1,UBIN,ND=Y                                         
         LKOUT C,019,CMLOVRD2,UBIN,ND=Y                                         
         LKOUT C,020,(D,B#SAVED,SVDAILY),CHAR,ND=Y                              
Array    LKOUT C,021,(A,ARYMDT)                                                 
Array    LKOUT C,023,(A,NETSINC)                                                
Array    LKOUT C,024,(A,NETSEXC)                                                
         LKOUT C,025,(D,B#SAVED,SVFORMAT),CHAR,ND=Y                             
         LKOUT C,026,CMLTALEX,CHAR,ND=Y                                         
         LKOUT C,027,CMLCLTNO,CHAR,ND=Y                                         
Array    LKOUT C,028,(A,ARYACT)                                                 
         LKOUT C,029,CMLCLASS,CHAR,ND=Y                                         
         LKOUT C,050,(D,B#SAVED,CDELETE),CHAR,ND=Y                              
         LKOUT E                                                                
*                                                                               
ARYACT   LKOUT A,(D,B#SAVED,SVACTUL1),NROWS=4,ROWWIDTH=12,             +        
               ROWNAME=DUMMY_D                                                  
ActCml   LKOUT C,028,DUM_LIN1,(R,EDACTCM),CHAR,ND=Y                             
         LKOUT E                                                                
*                                                                               
ARYMDT   LKOUT A,(D,B#SAVED,SVMPERID),NROWS=6,ROWWIDTH=4,              +        
               ROWNAME=DUMMY_D                                                  
MStrDte  LKOUT C,21,DUM_LIN1,(R,EDMSDTE),BDAT,ND=Y                              
MEndDte  LKOUT C,22,DUM_LIN1,(R,EDMEDTE),BDAT,ND=Y                              
         LKOUT E                                                                
*                                                                               
NETSINC  LKOUT A,(D,B#SAVED,SVNETSI),NROWS=4,ROWWIDTH=4,ROWNAME=DUMMY_D         
NetCd    LKOUT C,023,DUM_LIN1,(R,EDCNETI),ND=Y                                  
         LKOUT E                                                                
*                                                                               
NETSEXC  LKOUT A,(D,B#SAVED,SVNETSX),NROWS=4,ROWWIDTH=4,ROWNAME=DUMMY_D         
NetCd    LKOUT C,024,DUM_LIN1,(R,EDCNETX),ND=Y                                  
         LKOUT E                                                                
*                                                                               
ARYTC2   LKOUT A,(D,B#CLTREC,CLTRECD),NEWEL=Y,NROWS=1,ROWWIDTH=0                
CltCd    LKOUT C,001,(D,B#WORKD,QCLTA),CHAR                                     
CltNm    LKOUT C,002,(D,B#WORKD,WORK2),CHAR                                     
         LKOUT E                                                                
*                                                                               
ARYTPR1  LKOUT A,(R,NXTPR),MULTIROW=Y,ROWNAME=PRDRECD                           
Array    LKOUT C,3,(A,ARYTPR2)                                                  
         LKOUT E                                                                
*                                                                               
ARYTPR2  LKOUT A,(D,B#PRDREC,PRDRECD),NEWEL=Y,NROWS=1,ROWWIDTH=0                
PrdCd    LKOUT C,001,(D,B#SAVED,QPRDA),CHAR                                     
PrdNm    LKOUT C,002,(D,B#WORKD,WORK2),CHAR                                     
CltCd    LKOUT C,003,(D,B#WORKD,QCLTA),CHAR                                     
*TalAg    LKOUT C,004,(D,B#WORKD,DUB2),CHAR,ND=Y                                
         LKOUT E                                                                
*                                                                               
SETCMLD  DS    0H                                                               
         CLI   DONESW,C'Y'                                                      
         JE    EXITN                                                            
                                                                                
         MVI   DONESW,C'Y'                                                      
         MVC   LP_ADATA,ATCMREC    Point to traffic commercial record           
         J     EXITY                                                            
*                                                                               
CMLINI   MVI   SHWVLD,C'N'         Init request is invalid                      
         OC    MEDCOD,SPACES                                                    
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   EXITN                                                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,QACLT          Have any client code to look up?             
         JZ    NOMORE                                                           
                                                                                
         MVC   NUMCLT,LW_NUMN-LW_D(RE) Number of clients to process             
         AHI   RE,LW_LN2Q          Point to first client                        
         ST    RE,ANXTCLT          Set A(client request)                        
*                                                                               
         XC    QCLTX,QCLTX         Init binary client code                      
         MVC   QCLTA,0(RE)         Get input client code                        
         OC    QCLTA,SPACES                                                     
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   EXITN                                                            
                                                                                
         MVI   CLTCHG,C'N'         Init client changed flag                     
                                                                                
         L     R0,AIO8                                                          
         L     R1,AIO8                                                          
         AHI   R1,IO8LQ                                                         
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RE,AIO8                                                          
         ST    RE,ABPRLIST         Binary product codes list                    
         ST    RE,ABPRNXT          Current binary product code pointer          
*                                                                               
         AHI   RE,IO8LQ                                                         
         ST    RE,ACPRX            End of 3 char prod list                      
         SHI   RE,CPRLQ            Use last 1500 bytes                          
         ST    RE,ACPRLIST         Start of 3 char prod list                    
         ST    RE,ACPRNXT                                                       
*                                                                               
         SHI   RE,8                                                             
         ST    RE,ACMPRX           End of 3 char prod list for 1 cml            
         SHI   RE,CMLPRQ           255 bytes, 3 char prods for 1 cml            
         ST    RE,ACMLPR           Start of 3 char prod list                    
         ST    RE,ACMPRNXT                                                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    Get address of TRPACK                  
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A11'  Get address of untime                    
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VUNTIME,0(R1)                                                    
*                                                                               
         MVI   CMLPRFXL,0                                                       
         LA    R1,L'CMLPREFX                                                    
         LA    RE,CMLPREFX-1       Preset                                       
         CLC   CMLPREFX,SPACES     Any commercial prefix?                       
         JNH   CMLINI20            None                                         
         LA    RE,CMLPREFX+L'CMLPREFX-1                                         
*                                                                               
CMLINI10 CLI   0(RE),C' '          Anything here?                               
         JH    CMLINI15                                                         
         BCTR  RE,0                                                             
         JCT   R1,CMLINI10                                                      
*                                                                               
CMLINI15 STC   R1,CMLPRFXL         We know the length now                       
         CLI   CMLPRFXL,8          Input length >= 8  ?                         
         JNL   CMLINI30                                                         
*                                                                               
         CLI   RECACT,C'L'         Only for list can have partial cml           
         JNE   NOMORE                                                           
*                                                                               
CMLINI20 LA    RE,1(RE)            Point to the first blank                     
         MVI   0(RE),C'A'          1st 8 chars can't be < C'AAAAAAAA'           
         LA    R1,1(R1)            fill with C'A' until first 8 char            
         CHI   R1,8                   are populated                             
         JL    CMLINI20                                                         
*                                                                               
CMLINI30 GOTOR VTRPACK,DMCB,(C'P',CMLPREFX),CMLPRFXS                            
*                                                                               
         CLI   CMLPRFXL,12         FOR A SPECIFIC 12 CHAR FILM?                 
         JE    CMLINI40                                                         
*                                                                               
         LLC   R1,CMLPRFXL                                                      
         LA    R3,CMLPREFX(R1)                                                  
         LA    RE,12                                                            
         SR    RE,R1               RE=# of C'9's to fill to the end             
         BCTR  RE,0                                                             
         BASR  R4,0                                                             
         MVC   0(0,R3),=12C'9'                                                  
         EX    RE,0(R4)                                                         
*                                                                               
CMLINI40 GOTOR VTRPACK,DMCB,(C'P',CMLPREFX),CMLPRFXE                            
*                                                                               
CMLINIX  MVI   SHWVLD,C'Y'                                                      
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTCML   MVI   DONESW,C'N'         For setcmld routine                          
*                                                                               
         NI    SVCMLFLG,X'FF'-CHARQ  Init 3 char prods found                    
         CLI   CLTCHG,C'Y'                                                      
         JNE   NXTCML02                                                         
         MVI   CLTCHG,C'N'                                                      
         MVI   CMLDLSW1,NOQ        No commerical rec downloaded yet             
*                                                                               
         L     R0,ABPRLIST         Clear prod list area                         
         L     R1,ABPRLIST         in AIO8                                      
         AHI   R1,IO8LQ                                                         
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RE,ABPRLIST                                                      
         ST    RE,ABPRNXT          Re-init current prod code pointer            
*                                                                               
* Get next client from list                                                     
         DS    0H                                                               
         L     RE,ANXTCLT                                                       
         LA    RE,L'QCLTA(RE)      Point to next clt                            
         ST    RE,ANXTCLT                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NUMCLT         R0=remaining client count                    
         JZ    NOMORE              Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMCLT                                                      
*                                                                               
         MVC   QCLTA,0(RE)                                                      
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   NOMORE                                                           
                                                                                
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   NOMORE                                                           
         J     *+12                                                             
NXTCML02 CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCMLSQ                                                         
*                                                                               
         MVI   CMLDLSW1,NOQ        No commerical rec downloaded yet             
         MVI   SHWCML,C'Y'         Preset show commercial                       
         MVI   SHWCLT,C'N'         Preset not to show clts                      
         NI    SVCMLFLG,X'FF'-CML_ALLQ  Reset download all products             
*                                                                               
NXTCML03 XC    SVCMLFLS(SVCMLQ2),SVCMLFLS   Init saved commercial flds          
         XC    SVACTULS,SVACTULS                                                
         XC    ELEM1,ELEM1         Table of prd codes to be downloaded          
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLPIDAD+0,X'0A'    Passive keys for packed Ad-IDs               
         MVI   CMLPIDAD+1,X'C1'                                                 
         MVC   CMLPADAM,QMEDX                                                   
         MVC   CMLPADCL,QCLTX                                                   
         MVC   CMLPADID,CMLPRFXS   Starting point                               
*                                                                               
NXTCMLHI GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         CLI   RECACT,C'D'         Action display                               
         JNE   NXTCML10                                                         
         CLC   IOKEY(13),IOKEYSAV  Yes, make sure exact match                   
         JNE   NOMORE                                                           
         DROP  RE                                                               
*                                                                               
NXTCML10 LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         CLC   IOKEY(CMLPADID-CMLKEY),IOKEYSAV  Same upto client?               
         JE    NXTCML14                                                         
         MVI   SHWCML,C'N'         Do not show comml                            
         MVI   SHWCLT,C'Y'         Show client                                  
         MVI   SHWPRD,C'Y'              product                                 
         MVI   CLTCHG,C'Y'         Turn on client changed indicator             
         MVC   LP_ADATA,ATCMREC    Point to traffic commercial record           
         J     EXITY                                                            
*                                                                               
NXTCML14 OC    CMLPADID,CMLPADID   Have commercial ID?                          
         JZ    NXTCMLSQ            No                                           
         CLC   =X'C5DCC5DCC5B80000',CMLPADID   House for the Client?            
         JE    NXTCMLSQ              Yes, this is TRPACK of C'99999999'         
*                                                                               
         CLC   CMLPADID,CMLPRFXE   Past the last?                               
         JNH   NXTCML15                                                         
         MVI   SHWCML,C'N'         Do not show commercial                       
         MVI   SHWCLT,C'Y'         Show client                                  
         MVI   SHWPRD,C'Y'              product                                 
         MVI   CLTCHG,C'Y'         Turn on client changed indicator             
         MVC   LP_ADATA,ATCMREC    Point to traffic commercial record           
         J     EXITY                                                            
         DROP  RE                                                               
*                                                                               
NXTCML15 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NXTCMLSQ                                                         
         XC    SVCMLUFN(SVPCMLQ),SVCMLUFN Init saved commercial flds            
         XC    SVACTULS,SVACTULS                                                
         XC    ELEM2,ELEM2         List of prds for this commercial             
*                                                                               
         BRAS  RE,GETCSUM          Get record check sum                         
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
         USING CMLDTAEL,R1                                                      
         MVC   SVCMLDS1(L'CMLTITLE),CMLTITLE                                    
*                                                                               
         TM    CMLSTAT,X'80'                                                    
         JZ    NXTCML17                                                         
         MVI   CDELETE,C'Y'        Deleted cml                                  
*                                                                               
NXTCML17 OC    STRDATEB(L'STRDATEB*2),STRDATEB   Any date filter?               
         JZ    NXTCML18                          None, accept all cmmls         
         CLC   ENDDATEB,CMLRLSE    End date filter before release date?         
         JL    NXTCMLSQ            Yes, next record then                        
         CLC   STRDATEB,CMLRCL     Start date after recall date?                
         JH    NXTCMLSQ            Yes, next record then                        
*                                                                               
NXTCML18 CLC   CMLRCL,=X'FFFFFF'   Recall date 'until further notice'?          
         JNE   NXTCML20                                                         
         XC    CMLRCL,CMLRCL                                                    
         MVI   SVCMLUFN,YESQ       Set 'until further notice' flag to Y         
         DROP  R1                                                               
*                                                                               
NXTCML20 L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
NXTCML23 CLI   0(R1),EOR           End of record?                               
         JE    NXTCML90                                                         
         CLI   0(R1),X'20'         Product list element?                        
         JE    NXTCML50                                                         
         CLI   0(R1),X'22'         Network element?                             
         JE    NXTCML60                                                         
         CLI   0(R1),X'24'         Extended data elem                           
         JE    NXTCML70                                                         
         CLI   0(R1),X'29'         3 char product list element?                 
         JE    NXTCML45                                                         
         CLI   0(R1),X'30'         Extra description element?                   
         JE    NXTCML30                                                         
         CLI   0(R1),X'60'         Actual cmls                                  
         JE    NXTCML76                                                         
         CLI   0(R1),X'A0'         Ad-ID element?                               
         JE    NXTCML40                                                         
         CLI   0(R1),X'B0'         Matching data                                
         JE    NXTCML80                                                         
NXTCMLNX LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     NXTCML23            Bump to next element                         
*                                                                               
         USING CMLDSCEL,R1                                                      
NXTCML30 CLC   CMLDSC,SPACES                                                    
         JE    NXTCMLNX                                                         
         CLI   CMLDSCSQ,0                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS1,CMLDSC                                                  
         CLI   CMLDSCSQ,1                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS2,CMLDSC                                                  
         CLI   CMLDSCSQ,2                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS3,CMLDSC                                                  
         J     NXTCMLNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLADIEL,R1                                                      
NXTCML40 MVC   SVCMLAID,CMLADID    Ad-Id                                        
         J     NXTCMLNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLMPREL,R1                                                      
NXTCML45 L     R3,ABPRLIST         Point to binary product table                
         CLI   CMLMPRS,X'FF'       All products?                                
         JNE   *+8                                                              
         OI    SVCMLFLG,CML_ALLQ   Set to download all products                 
*                                                                               
NXTCM45C OI    SVCMLFLG,CHARQ      Set to 3 char prods found                    
         XC    ELEM2,ELEM2                                                      
         L     R3,ACPRLIST         Point to 3 char product table                
*                                                                               
         LLC   RF,CMLMPRLN                                                      
         AHI   RF,-2               Number of binary product codes               
         CHI   RF,0                                                             
         JNH   NXTCMLNX                                                         
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   ELEM2(0),CMLMPRS   Save list of 3 char prod codes                
         EX    RF,0(RE)                                                         
*                                                                               
         LA    RF,ELEM2            Current prds for this commercial             
NXTCML46 CLI   0(RF),X'00'         End of current prds list?                    
         JE    NXTCMLNX                                                         
         CLI   0(RF),X'FF'         All product?                                 
         JE    NXTCMLNX                                                         
*                                                                               
NXTCML47 CLI   0(R3),X'00'         Blank entry?                                 
         JE    NXTCML48                                                         
         CLC   0(3,RF),0(R3)       Already in table?                            
         JE    NXTCML49                                                         
         AHI   R3,3                Position to next 3 char product              
         J     NXTCML47                                                         
NXTCML48 MVC   0(3,R3),0(RF)                                                    
         LA    R3,3(R3)                                                         
         ST    R3,ACPRNXT                                                       
NXTCML49 AHI   RF,3                Bump to next current prd list                
         L     R3,ACPRLIST         Start of 3 char product table                
         J     NXTCML46                                                         
         DROP  R1                                                               
*                                                                               
*                                                                               
         USING CMLPRDEL,R1                                                      
NXTCML50 L     R3,ABPRLIST         Point to binary product table                
         CLI   CMLPRDS,X'FF'       All products?                                
         JNE   *+8                                                              
         OI    SVCMLFLG,CML_ALLQ   Set to download all products                 
*                                                                               
         LLC   RF,CMLPRDLN                                                      
         AHI   RF,-2               Number of binary product codes               
         CHI   RF,0                                                             
         JNH   NXTCMLNX                                                         
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   ELEM2(0),CMLPRDS   Save list of binary prod codes                
         EX    RF,0(RE)                                                         
*                                                                               
         LA    RF,ELEM2            Current prds for this commercial             
NXTCML52 CLI   0(RF),X'00'         End of current prds list?                    
         JE    NXTCMLNX                                                         
         CLI   0(RF),X'FF'         All product?                                 
         JE    NXTCMLNX                                                         
*                                                                               
NXTCML54 CLI   0(R3),X'00'         Blank entry?                                 
         JE    NXTCML56                                                         
         CLC   0(1,RF),0(R3)       Already in table?                            
         JE    NXTCML58                                                         
         AHI   R3,1                Position to next binary product              
         J     NXTCML54                                                         
NXTCML56 MVC   0(1,R3),0(RF)                                                    
         LA    R3,1(R3)                                                         
         ST    R3,ABPRNXT                                                       
NXTCML58 AHI   RF,1                Bump to next current prd list                
         L     R3,ABPRLIST         Start of binary product table                
         J     NXTCML52                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLNETEL,R1                                                      
NXTCML60 LA    RF,SVNETSI          Networks                                     
         TM    CMLFLG,CMLEXNET                                                  
         JZ    NXTCM66C                                                         
         LA    RF,SVNETSX                                                       
                                                                                
NXTCM66C OC    0(L'CMLNET,RF),0(RF)                                             
         JZ    NXTCML67                                                         
         LA    RF,L'CMLNET(RF)                                                  
         J     NXTCM66C                                                         
                                                                                
NXTCML67 MVC   0(L'CMLNET,RF),CMLNET Network                                    
         J     NXTCMLNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLXDTEL,R1                                                      
NXTCML70 DS    0H                                                               
         MVC   SVPARENT,CMLXPRNT                                                
                                                                                
         CLI   CMLXDSDT,0                                                       
         JE    NXTCML72                                                         
                                                                                
         MVC   SVDDATE,CMLXDSDT                                                 
         ST    R1,FULL                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(2),CMLXDSTM                                                 
         GOTOR VUNTIME,DMCB,WORK,SVDTIME                                        
         L     R1,FULL                                                          
                                                                                
NXTCML72 OC    CMLXHDEF,CMLXHDEF                                                
         JZ    *+8                                                              
         MVI   SVFORMAT,C'H'                                                    
         J     NXTCMLNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLACTEL,R1                                                      
NXTCML76 LA    RF,SVACTUL1         Actual cml                                   
                                                                                
NXTCM76C OC    0(L'SVACTUL1,RF),0(RF)                                           
         JZ    NXTCML77                                                         
         LA    RF,L'SVACTUL1(RF)                                                
         J     NXTCM76C                                                         
                                                                                
NXTCML77 MVC   0(L'CMLACTID,RF),CMLACTID  Disp 8 char cml                       
         CLI   CMLACTLN,CMLACTL1          Old elem                              
         JE    *+10                                                             
         MVC   0(L'CMLACTCM,RF),CMLACTCM   No, disp 12 char cml                 
         J     NXTCMLNX                                                         
         DROP  R1                                                               
*                                                                               
NXTCML80 LR    R2,R1                                                            
         USING CMLMATCH,R2                                                      
         MVC   SVMPERID,CMLMPER1   Move all match dates                         
                                                                                
         ST    R1,FULL                                                          
*                                                                               
         OC    CMLMSTIM,CMLMSTIM   Match start time                             
         JZ    NXTCML85                                                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),CMLMSTIM                                                 
         GOTOR VUNTIME,DMCB,WORK,SVMSTIME                                       
*                                                                               
NXTCML85 DS    0H                                                               
         OC    CMLMETIM,CMLMETIM   Match end time                               
         JZ    NXTCML87                                                         
         CLC   CMLMETIM,=X'FFFF'                                                
         JE    NXTCML87                                                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),CMLMETIM                                                 
         GOTOR VUNTIME,DMCB,WORK,SVMETIME                                       
*                                                                               
NXTCML87 DS    0H                  Check times daily                            
         L     R1,FULL             Restore pointer to elem                      
         MVI   SVDAILY,C'N'                                                     
         TM    CMLMFLAG,CMLMFDAY                                                
         JZ    NXTCMLNX                                                         
         MVI   SVDAILY,C'Y'                                                     
         J     NXTCMLNX                                                         
         DROP  R2                                                               
*                                                                               
NXTCML90 DS    0H                  To extract more fields if needed             
         BRAS  RE,CNVPRD           Convert binary prods to 3 char               
         LA    R0,3                3 description fields                         
         LA    RE,SVCMLDS1         1st description                              
NXTCML95 OC    0(L'SVCMLDS1,RE),0(RE)                                           
         JZ    NXTCML97                                                         
         TR    0(L'SVCMLDS1,RE),TRTAB Translate data                            
NXTCML97 LA    RE,L'SVCMLDS1(RE)   Bump to next desc field                      
         JCT   R0,NXTCML95                                                      
*                                                                               
         MVI   CMLDLSW1,YESQ       Commerical rec downloaded                    
         CLI   RECACT,C'D'         Action display                               
         JNE   NXTCML99                                                         
         MVI   SHWCLT,C'Y'         Show client                                  
         MVI   SHWPRD,C'Y'              product                                 
NXTCML99 MVC   LP_ADATA,ATCMREC    Point to traffic commercial record           
         J     EXITY                                                            
*                                                                               
NXTCMLSQ CLI   RECACT,C'D'         Action display                               
         JE    NOMORE              Yes, only one record displayed               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO3'                            
         J     NXTCML10                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDMSDTE  LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             Have matching dates?                         
         JE    XCOLEN                                                           
*                                                                               
         XC    0(8,R4),0(R4)       Init output                                  
         GOTO1 VDATCON,DMCB,(2,0(R2)),(20,0(R4))                                
*                                                                               
         LA    R0,8                Max length for matching end date             
         J     SETOLENX                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDMEDTE  LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             Have matching dates?                         
         JE    XCOLEN                                                           
*                                                                               
         XC    0(8,R4),0(R4)       Init output                                  
         GOTO1 VDATCON,DMCB,(2,2(R2)),(20,0(R4))                                
*                                                                               
         LA    R0,8                Max length for matching end date             
         J     SETOLENX                                                         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDCNETI  LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             Have network code?                           
         JE    XCOLEN                                                           
         XC    0(4,R4),0(R4)       Init output                                  
         MVC   0(4,R4),0(R2)       Network include                              
*                                                                               
         LA    R0,4                Max length for network code                  
         J     SETOLENX                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDCNETX  LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             Have network code?                           
         JE    XCOLEN                                                           
         XC    0(4,R4),0(R4)       Init output                                  
         MVC   0(4,R4),0(R2)       Network exclude                              
*                                                                               
         LA    R0,4                Max length for network code                  
         J     SETOLENX                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDACTCM  LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             Have actual cml?                             
         JE    XCOLEN                                                           
         XC    0(12,R4),0(R4)      Init output                                  
         MVC   0(12,R4),0(R2)      actual cml                                   
*                                                                               
         LA    R0,12               Max length for network code                  
         J     SETOLENX                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTCL    CLI   SHWCLT,C'Y'         Show client?                                 
         JNE   EXITN                                                            
*                                                                               
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCL30                                                          
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    WORK2,WORK2         Init client name for output                  
         MVC   WORK2(L'CNAME),CNAME                                             
         J     NXTTCL70                                                         
         DROP  RE                                                               
*                                                                               
NXTCL30  DS    0H                  To process more client rec if needed         
         J     NOMORE                                                           
*                                                                               
NXTCL70  DS    0H                  To extract more fields if needed             
*                                                                               
         MVI   SHWCLT,C'N'         Do not show clt                              
*                                                                               
         MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDCPR    LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             Have product code?                           
         JE    XCOLEN                                                           
         XC    0(3,R4),0(R4)       Init output                                  
         CLI   0(R2),X'FF'         'All' product?                               
         JNE   *+14                                                             
         MVC   0(3,R4),=C'ALL'                                                  
         J     EDCPR42                                                          
*                                                                               
         LR    RF,R2               Preset to point to 3 char prod               
         L     RE,ACPRLIST         Start of 3 char prod list                    
         CLI   0(RE),0             Any 3 char prods                             
         JNE   EDCPR30             Yes                                          
*                                                                               
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST                                                         
         LA    R1,220              220 entries in CLIST                         
EDCPR20  CLC   0(1,R2),3(RF)       Match that of product list?                  
         JE    EDCPR40                                                          
         LA    RF,4(RF)                                                         
         JCT   R1,EDCPR20                                                       
         LA    RF,CLIST2                                                        
         LA    R1,35               Additional 35 entries in CLIST2              
EDCPR24  CLC   0(1,R2),3(RF)       Match that of product list?                  
         JE    EDCPR40                                                          
         LA    RF,4(RF)                                                         
         JCT   R1,EDCPR24                                                       
         J     XCOLEN              Bad product code                             
* 3 char prod                                                                   
EDCPR30  MVC   0(3,R4),0(RF)       3 char prod for output                       
         L     RE,ACPRLIST         Start of 3 char product codes list           
         L     RF,ACPRNXT          Current 3 char product code pointer          
EDCPR32  CR    RE,RF                                                            
         JE    EDCPR34                                                          
         CLC   0(3,RE),0(R2)       Same prod                                    
         JE    EDCPR42                                                          
         LA    RE,3(RE)                                                         
         J     EDCPR32                                                          
*                                                                               
EDCPR34  MVC   0(3,RF),0(R2)       Save 3 char product in buffer                
         LA    RF,3(RF)                                                         
         ST    RF,ACPRNXT                                                       
         J     EDCPR42                                                          
*                                                                               
EDCPR40  MVC   0(3,R4),0(RF)       3 char prod for output                       
         L     RE,ABPRLIST         Start of binary product codes list           
         L     RF,ABPRNXT          Current binary product code pointer          
EDCPR40C CR    RE,RF                                                            
         JE    EDCPR41                                                          
         CLC   0(1,RE),0(R2)       Same prod                                    
         JE    EDCPR42                                                          
         LA    RE,1(RE)                                                         
         J     EDCPR40C                                                         
*                                                                               
EDCPR41  MVC   0(1,RF),0(R2)       Save binary product in buffer                
         LA    RF,1(RF)                                                         
         ST    RF,ABPRNXT                                                       
                                                                                
EDCPR42  LA    R0,3                Max length for product code                  
         J     SETOLENX                                                         
         DROP  RE                                                               
*                                                                               
EDCML1   LM    R2,R4,LP_AINP                                                    
         USING CMLKEY,R2                                                        
         OC    CMLKCML,CMLKCML     Have commercial code?                        
         JZ    XCOLEN                                                           
         XC    0(13,R4),0(R4)                                                   
         MVC   0(L'CMLKCML,R4),CMLKCML                                          
         XC    0(13,R4),0(R4)                                                   
         GOTOR VTRPACK,DMCB,(C'U',CMLKCML),0(R4)                                
EDCM10   CLI   0(R4),C'A'          Lower case letters?                          
         JNL   EDCM30                                                           
         OC    SVCMLAID,SVCMLAID   Have saved Ad-ID?                            
         JZ    EDCM30                                                           
         XC    0(13,R4),0(R4)                                                   
         MVC   0(L'SVCMLAID,R4),SVCMLAID                                        
EDCM30   LA    R0,L'CMLADID        Max length for commerical code               
         J     SETOLENX                                                         
         DROP  R2                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTPR    CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTPR30                                                          
         TM    SVCMLFLG,CML_ALLQ   Download all products?                       
         JNZ   NXTPR10                                                          
                                                                                
         OI    SVCMLFLG,CHARQ      Preset to 3 char prod                        
         L     RF,ACPRLIST         3 char product codes list                    
         ST    RF,FULL1                                                         
         LR    R2,RF                                                            
         CLI   0(RF),0                                                          
         JNE   NXTPR36                                                          
         NI    SVCMLFLG,X'FF'-CHARQ                                             
                                                                                
         L     RE,ABPRLIST         Binary product codes list                    
         MVC   ELEM1(255),0(RE)    Get set of binary prd to process             
         J     NXTPR18                                                          
                                                                                
NXTPR10  L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    ELEM1,ELEM1         Init table of binary product codes           
         LA    R1,CLIST                                                         
         LA    R2,ELEM1                                                         
         LA    RF,220                                                           
NXTPR12  CLI   3(R1),0             No more binary product code?                 
         JE    NXTPR18                                                          
         MVC   0(1,R2),3(R1)       Save binary product code to look up          
         AHI   R2,1                                                             
         AHI   R1,4                                                             
         JCT   RF,NXTPR12                                                       
         LA    R1,CLIST2           2nd product list                             
         LA    RF,35                                                            
NXTPR14  CLI   3(R1),0             No more binary product code?                 
         JE    NXTPR18                                                          
         MVC   0(1,R2),3(R1)       Save binary product code to look up          
         AHI   R2,1                                                             
         AHI   R1,4                                                             
         JCT   RF,NXTPR14                                                       
                                                                                
NXTPR18  LA    R1,ELEM1                                                         
         ST    R1,FULL1            Address of binary prd codes to proc          
         DROP  RE                                                               
*                                                                               
NXTPR30  L     RF,FULL1            Preset for 3 char prod                       
         L     R2,FULL1            Point to next prod to process                
         CLI   0(R2),X'00'         EOL                                          
         JE    NXTPRX                                                           
         CLI   0(R2),X'FF'         POL?                                         
         JE    NXTPRX                                                           
*                                                                               
         TM    SVCMLFLG,CHARQ                                                   
         JO    NXTPR36                                                          
*                                                                               
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST            Point to 1st list of products                
         LA    R1,220                                                           
         CLC   0(1,R2),3(RF)       Binary prd code match that of list?          
         JE    NXTPR36                                                          
         AHI   RF,4                                                             
         JCT   R1,*-14                                                          
         LA    RF,CLIST2           Point to 2nd list of products                
         LA    R1,35                                                            
         CLC   0(1,R2),3(RF)       Binary prd code match that of list?          
         JE    NXTPR36                                                          
         AHI   RF,4                                                             
         JCT   R1,*-14                                                          
         DROP  RE                                                               
*                                                                               
NXTPR36  XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING PKEY,RE                                                          
         MVI   PKEYTYPE,PKEYTYPQ                                                
         MVC   PKEYAM,QMEDX                                                     
         MVC   PKEYCLT,QCLTX                                                    
         MVC   PKEYPRD,0(RF)       Character format prd code from table         
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         CLC   IOKEY(L'PKEY),IOKEYSAV                                           
         JNE   NXTPR38                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
         JNE   NXTPR38                                                          
         AHI   R2,1                Point to next entry in table                 
         TM    SVCMLFLG,CHARQ                                                   
         JZ    *+8                                                              
         AHI   R2,2                Point to next 3 char prod                    
         ST    R2,FULL1            For next round                               
         J     NXTPR50             Now extract prd fields for output            
*                                                                               
NXTPR38  AHI   R2,1                Point to next entry in table                 
         TM    SVCMLFLG,CHARQ                                                   
         JZ    *+8                                                              
         AHI   R2,2                Point to next 3 char prod                    
         ST    R2,FULL1                                                         
         J     NXTPR30             Get next product record                      
*                                                                               
NXTPR50  L     R1,IOADDR                                                        
         USING PRDHDR,R1                                                        
         XC    QPRDA,QPRDA         Return prd code - character                  
         XC    WORK2,WORK2         Return prd name                              
         XC    DUB2,DUB2           Return Talent agency code                    
         MVC   QPRDA,PKEYPRD                                                    
         MVC   WORK2(L'PNAME),PNAME                                             
         MVC   DUB2(L'PTALAGY),PTALAGY                                          
         J     NXTPR70                                                          
         DROP  R1                                                               
*                                                                               
NXTPRX   MVI   SHWCML,C'Y'         Show commercial                              
         MVI   SHWCLT,C'Y'              client                                  
         MVI   SHWPRD,C'N'         Do not show products                         
         NI    SVCMLFLG,X'FF'-CML_ALLQ Reset all prod switch                    
         J     NOMORE                                                           
*                                                                               
NXTPR70  MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* Convert all binary products to 3 char                                         
CNVPRD   NTR1  BASE=*,LABEL=*      Convert binary prods to 3 char               
         MVI   BYTE,0              Init processing elem flag                    
         L     R5,ABPRLIST         Start of binary product codes list           
*                                                                               
CNV05    CLI   0(R5),0                                                          
         JE    CNV50                                                            
         CLI   0(R5),X'FF'                                                      
         JE    CNV50                                                            
*                                                                               
CNV08    L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST                                                         
         LA    R1,220              220 entries in CLIST                         
CNV10    CLC   0(1,R5),3(RF)       Match that of product list?                  
         JE    CNV20                                                            
         LA    RF,4(RF)                                                         
         JCT   R1,CNV10                                                         
         LA    RF,CLIST2                                                        
         LA    R1,35               Additional 35 entries in CLIST2              
CNV12    CLC   0(1,R5),3(RF)       Match that of product list?                  
         JE    CNV20                                                            
         LA    RF,4(RF)                                                         
         JCT   R1,CNV12                                                         
         J     CNV45               Skip bad product code                        
*                                                                               
CNV20    L     R2,ACMLPR           Preset to process elem                       
         L     R3,ACMPRNXT                                                      
         CLI   BYTE,C'E'           Processing elem?                             
         JE    CNV30                                                            
         L     R2,ACPRLIST         Start of 3 char product codes list           
         L     R3,ACPRNXT          Current 3 char product code pointer          
*                                                                               
CNV30    CLC   0(3,R2),0(RF)       Same prod                                    
         JE    CNV45               Yes, process next binary prod                
         CR    R2,R3                                                            
         JE    CNV40                                                            
         LA    R2,3(R2)                                                         
         CLI   0(R2),0                                                          
         JNE   CNV30                                                            
*                                                                               
CNV40    CLI   0(R2),0                                                          
         JE    *+12                                                             
         LA    R2,3(R2)                                                         
         J     CNV40                                                            
         MVC   0(3,R2),0(RF)       Save 3 char prod in list                     
         CLI   BYTE,C'E'           Processing elem?                             
         JE    *+12                                                             
         ST    R2,ACPRNXT          Current binary product code pointer          
         J     CNV45                                                            
         ST    R2,ACMPRNXT         Current point in elem                        
CNV45    LA    R5,1(R5)            Process next binary prod                     
         J     CNV05                                                            
*                                                                               
CNV50    TM    SVCMLFLG,CHARQ      3 char prod elem?                            
         JZ    CNV55               No, must convert                             
*                                                                               
         L     RE,ACMLPR           List of 3 char prods for this cml            
         MVC   0(L'ELEM2,RE),ELEM2 Move 3 char prods                            
         J     EXITY                                                            
*                                                                               
*Convert binary prods to 3 char prods                                           
CNV55    CLI   BYTE,C'E'           Elem processed already?                      
         JE    EXITY                                                            
         LA    R5,ELEM2                                                         
         MVI   BYTE,C'E'           Processing elem                              
         J     CNV05                                                            
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETCSUM  NTR1  BASE=*,LABEL=*                                                   
         L     RE,IOADDR           RE=A(A(RECORD))                              
         SR    RF,RF                                                            
         ICM   RF,3,CMLRLEN-CMLKEY(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,SVCKSM                                                     
         J     EXITY                                                            
*                                                                               
*                                                                               
         GETEL R3,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
***********************************************************************         
* Traffic profile records download                                              
***********************************************************************         
*                                                                               
*REQPROF  LKREQ H,M#TPRFDL,OUTPRF,NEXTREQ=REQUESTX                              
REQPROF  LKREQ H,M#TPRFDL,OUTPRF,NEXTREQ=REQRTG                                 
MedCd    LKREQ F,001,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,MAXLEN=L'QMEDX,TEXT=NE#MED,COL=*                    
CltCd    LKREQ F,002,(I,B#SAVED,QCLTIND),CHAR,LIST=F,                  +        
               OLEN=L'QCLTA,MAXLEN=L'QCLTA,TEXT=NE#CLI,COL=*                    
SubMed   LKREQ F,003,(D,B#SAVED,SUBMED),CHAR,OLEN=L'SUBMED,            +        
               MAXLEN=L'SUBMED,TEXT=(*,SUBMLIT),COL=*                           
         LKREQ E                                                                
                                                                                
SUBMLIT  DC    C'Sub-media'                                                     
                                                                                
OUTPRF   LKOUT H                   Net Traffic prof output maps                 
                                                                                
         LKOUT R,X'0030'                                                        
PRout    LKOUT P,,TPRINI           Init traffic prof array                      
Array    LKOUT C,X'0030',(A,ARYTNPR)                                            
Array    LKOUT C,X'0030',(A,ARYTN1P)                                            
Array    LKOUT C,X'0030',(A,ARYTN2P)                                            
Array    LKOUT C,X'0030',(A,ARYTN3P)                                            
Array    LKOUT C,X'0030',(A,ARYTW)                                              
Array    LKOUT C,X'0030',(A,ARYN0)                                              
         LKOUT E                                                                
                                                                                
         LKOUT X                  Traffic prof output maps end                  
                                                                                
ARYTNPR  LKOUT A,(R,NXTPRTN),MULTIROW=Y,ROWNAME=DUMMY_D                         
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
         LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT C,011,(D,B#WORKD,WORK+00),LBIN,LEN=1,ND=Y                        
         LKOUT C,012,(D,B#WORKD,WORK+01),CHAR,LEN=1,ND=Y                        
         LKOUT C,013,(D,B#WORKD,WORK+02),CHAR,LEN=1,ND=Y                        
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1,ND=Y                        
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1,ND=Y                        
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1,ND=Y                        
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1,ND=Y                        
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1,ND=Y                        
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1,ND=Y                        
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1,ND=Y                        
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1,ND=Y                        
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1,ND=Y                        
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1,ND=Y                        
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1,ND=Y                        
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1,ND=Y                        
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1,ND=Y                        
         LKOUT E                                                                
                                                                                
ARYTN1P  LKOUT A,(R,NXTPRTN1),MULTIROW=Y,ROWNAME=DUMMY_D                        
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
         LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT C,011,(D,B#WORKD,WORK+00),CHAR,LEN=1,ND=Y                        
         LKOUT C,012,(D,B#WORKD,WORK+01),CHAR,LEN=1,ND=Y                        
         LKOUT C,013,(D,B#WORKD,WORK+02),CHAR,LEN=1,ND=Y                        
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1,ND=Y                        
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1,ND=Y                        
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1,ND=Y                        
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1,ND=Y                        
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1,ND=Y                        
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1,ND=Y                        
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1,ND=Y                        
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1,ND=Y                        
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1,ND=Y                        
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1,ND=Y                        
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1,ND=Y                        
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1,ND=Y                        
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1,ND=Y                        
         LKOUT E                                                                
                                                                                
ARYTN2P  LKOUT A,(R,NXTPRTN2),MULTIROW=Y,ROWNAME=DUMMY_D                        
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
         LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT C,011,(D,B#WORKD,WORK+00),CHAR,LEN=1,ND=Y                        
         LKOUT C,012,(D,B#WORKD,WORK+01),LBIN,LEN=1,ND=Y                        
         LKOUT C,013,(D,B#WORKD,WORK+02),LBIN,LEN=1,ND=Y                        
         LKOUT C,014,(D,B#WORKD,WORK+03),LBIN,LEN=1,ND=Y                        
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1,ND=Y                        
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1,ND=Y                        
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1,ND=Y                        
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1,ND=Y                        
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1,ND=Y                        
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1,ND=Y                        
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1,ND=Y                        
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1,ND=Y                        
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1,ND=Y                        
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1,ND=Y                        
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1,ND=Y                        
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1,ND=Y                        
         LKOUT E                                                                
                                                                                
ARYTN3P  LKOUT A,(R,NXTPRTN3),MULTIROW=Y,ROWNAME=DUMMY_D                        
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
         LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT C,011,(D,B#WORKD,WORK+00),CHAR,LEN=1,ND=Y                        
         LKOUT C,012,(D,B#WORKD,WORK+01),CHAR,LEN=1,ND=Y                        
         LKOUT C,013,(D,B#WORKD,WORK+02),CHAR,LEN=1,ND=Y                        
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1,ND=Y                        
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1,ND=Y                        
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1,ND=Y                        
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1,ND=Y                        
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1,ND=Y                        
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1,ND=Y                        
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1,ND=Y                        
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1,ND=Y                        
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1,ND=Y                        
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1,ND=Y                        
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1,ND=Y                        
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1,ND=Y                        
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1,ND=Y                        
         LKOUT E                                                                
                                                                                
ARYTW    LKOUT A,(R,NXTPRTW),MULTIROW=Y,ROWNAME=DUMMY_D                         
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
         LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT C,011,(D,B#WORKD,WORK+00),CHAR,LEN=1,ND=Y                        
         LKOUT C,012,(D,B#WORKD,WORK+01),CHAR,LEN=1,ND=Y                        
         LKOUT C,013,(D,B#WORKD,WORK+02),CHAR,LEN=1,ND=Y                        
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1,ND=Y                        
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1,ND=Y                        
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1,ND=Y                        
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1,ND=Y                        
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1,ND=Y                        
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1,ND=Y                        
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1,ND=Y                        
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1,ND=Y                        
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1,ND=Y                        
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1,ND=Y                        
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1,ND=Y                        
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1,ND=Y                        
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1,ND=Y                        
         LKOUT E                                                                
                                                                                
ARYN0    LKOUT A,(R,NXTPRN0),MULTIROW=Y,ROWNAME=DUMMY_D                         
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
         LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT C,011,(D,B#WORKD,WORK+00),CHAR,LEN=1,ND=Y                        
         LKOUT C,012,(D,B#WORKD,WORK+01),LBIN,LEN=1,ND=Y                        
         LKOUT C,013,(D,B#WORKD,WORK+02),CHAR,LEN=1,ND=Y                        
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1,ND=Y                        
         LKOUT C,015,(D,B#WORKD,WORK+04),LBIN,LEN=1,ND=Y                        
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1,ND=Y                        
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1,ND=Y                        
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1,ND=Y                        
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1,ND=Y                        
         LKOUT C,020,(D,B#WORKD,WORK+09),LBIN,LEN=1,ND=Y                        
         LKOUT C,021,(D,B#WORKD,WORK+10),LBIN,LEN=1,ND=Y                        
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1,ND=Y                        
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1,ND=Y                        
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1,ND=Y                        
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1,ND=Y                        
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1,ND=Y                        
         LKOUT E                                                                
                                                                                
TPRINI   DS    0H                                                               
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA  Values disappeared          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,QACLT                                                       
         JZ    NOMORE                                                           
*                                                                               
         MVC   NUMCLT,LW_NUMN-LW_D(RE) Number of clients to process             
         MVC   LOOPCT,NUMCLT+1                                                  
         AHI   RE,LW_LN2Q          Point to first client                        
         ST    RE,ANXTCLT          Set A(client request)                        
         ST    RE,AFRSCLT          Set A(1st client)                            
*                                                                               
         MVC   QCLTA,0(RE)                                                      
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
*                                                                               
         L     R2,ANXTCLT          Point to 1st client                          
         BRAS  RE,GCLTOFF          Get  client office                           
         J     EXITY                                                            
*                                                                               
***********************************************************************         
* Get Traffic profile for all clients                                           
***********************************************************************         
                                                                                
                                                                                
NXTPRTN  LA    RF,PRFTABN          Point to Traffic TN profile table            
         J     NXTPRFT                                                          
                                                                                
NXTPRTN1 LA    RF,PRFTABN1         Point to Traffic TN1 profile table           
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRFT                                                          
         J     NXTPRCLT                                                         
                                                                                
NXTPRTN2 LA    RF,PRFTABN2         Point to Traffic TN2 profile table           
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRFT                                                          
         J     NXTPRCLT                                                         
                                                                                
NXTPRTN3 LA    RF,PRFTABN3         Point to Traffic TN3 profile table           
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRFT                                                          
         J     NXTPRCLT                                                         
                                                                                
NXTPRTW  LA    RF,PRFTABW          Point to Traffic TW profile table            
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRFT                                                          
         J     NXTPRCLT                                                         
                                                                                
NXTPRN0  LA    RF,PRFTAB0          Point to User N0 profile table               
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRFT                                                          
                                                                                
NXTPRCLT L     R2,AFRSCLT          1st clt                                      
         MVC   NUMCLT+1(1),LOOPCT  Reset count                                  
         BRAS  RE,GCLTOFF          Get  client office                           
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,NUMCLT         R0=remaining client count                    
         JZ    NOMORE              Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMCLT                                                      
                                                                                
NXTPRFT  XC    FULL,FULL           Init output profile name                     
         XC    WORK,WORK           Init output profile values                   
                                                                                
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JE    NXTPFT12                                                         
                                                                                
         L     R2,ANXTCLT          Point to client                              
         CLC   0(L'QCLTA,R2),SPACES                                             
         JNH   NOMORE                                                           
         BRAS  RE,GCLTOFF          Get  client office                           
                                                                                
NXTPFT12 MVC   FULL(4),0(RF)       Set output profile name                      
         XC    TEMP,TEMP                                                        
         MVC   TEMP+00(2),=C'S0'                                                
         MVC   TEMP+02(2),0(RF)                                                 
         CLI   2(RF),C' '          3 character profile?                         
         JNH   NXTPFT16                                                         
         MVI   TEMP+00,C's'        Yes                                          
         MVC   TEMP+01(3),0(RF)                                                 
                                                                                
NXTPFT16 MVC   TEMP+04(L'QAGY),QAGY                                             
         CLC   =C'TN2',0(RF)                                                    
         JNE   *+18                                                             
         MVC   TEMP+06(L'SUBMED),SUBMED Preset to submedia                      
         CLI   SUBMED,X'40'                                                     
         JH    *+10                                                             
         MVC   TEMP+06(L'QMEDA),QMEDA                                           
         MVC   TEMP+07(L'QCLTA),QCLTA                                           
         MVI   TEMP+10,C'*'                                                     
         MVC   TEMP+11(L'SVCLTOFF),SVCLTOFF                                     
         GOTOR VGETPROF,DMCB,TEMP,WORK,VDATAMGR                                 
         MVC   LP_ADATA,AOFCREC                                                 
         J     EXITY                                                            
*                                                                               
*                                                                               
*----------------------------------------------------                           
* Get client office and save address of next client                             
*----------------------------------------------------                           
*                                                                               
GCLTOFF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   QCLTA,0(R2)                                                      
         OC    QCLTA,SPACES                                                     
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
                                                                                
         LA    R2,L'QCLTA(R2)                                                   
         ST    R2,ANXTCLT                                                       
                                                                                
         XC    SVCLTOFF,SVCLTOFF                                                
                                                                                
         L     RF,ACLTREC                                                       
         MVC   SVCLTOFF,COFFICE-CLTRECD(RF)                                     
*                                                                               
         J     EXITY                                                            
*                                                                               
*----------------------------------------------------                           
* Rating source download                                                        
*----------------------------------------------------                           
                                                                                
RTNGS#   EQU   353                                                              
REQRTG   LKREQ *,RTNGS#,OUTRTG,NEXTREQ=REQUESTX                                 
                                                                                
OUTRTG   LKOUT H                                                                
         LKOUT R,X'0025'                                                        
PRout    LKOUT P,,GETTOKN          Pass default NSI token (blank)               
Token    LKOUT C,001,(D,B#SAVED,TOKEN),CHAR                                     
PRout    LKOUT P,,GETTOK                                                        
Token    LKOUT C,001,(D,B#SAVED,TOKEN),CHAR                                     
PRout    LKOUT P,,GETSDR                                                        
Array    LKOUT C,X'0025',(A,ARYSDR)                                             
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
*                                                                               
REQUESTX LKREQ X                                                                
         EJECT                                                                  
                                                                                
         LKARY T                                                                
         EJECT                                                                  
                                                                                
SETCCC   JE    *+8                 Set converse condition code                  
RTRNYES  CR    RE,RE               CC=Equal if not equal                        
         BR    RE                                                               
RTRNNO   LTR   RE,RE               CC=Not equal if equal                        
         BR    RE                                                               
                                                                                
MORE     MVI   LP_RMODE,LP_RMORE   Tell DDLINK to call again                    
         J     EXITY                                                            
                                                                                
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
*                                                                               
* Traffic profiles                                                              
PRFTABN  DC    C'TN ',X'C0'          TN                                         
PRFTABN1 DC    C'TN1',X'C0'          TN1                                        
PRFTABN2 DC    C'TN2',X'C0'          TN2                                        
PRFTABN3 DC    C'TN3',X'C0'          TN3                                        
PRFTABW  DC    C'TW ',X'C0'          TW                                         
PRFTAB0  DC    C'N0 ',X'C0'          N0                                         
*                                                                               
*                                                                               
TRTAB    DC    X'40404040404040404040404040404040'     00-0F                    
         DC    X'40404040404040404040404040404040'     10-1F                    
         DC    X'40404040404040404040404040404040'     20-2F                    
         DC    X'40404040404040404040404040404040'     30-3F                    
         DC    X'404040404040404040404040404D4E4F'     40-4F                    
         DC    X'4E4040404040404040405A5B5C5D5E40'     50-5F                    
         DC    X'604040404040404040406A6B6C6D406F'     60-6F                    
         DC    X'404040404040404040797A7B7C407E40'     70-7F                    
         DC    X'40818283848586878889404040404040'     80-8F                    
         DC    X'40919293949596979899404040404040'     90-9F                    
         DC    X'4040A2A3A4A5A6A7A8A9404040404040'     A0-AF                    
         DC    X'40404040404040404040404040404040'     B0-BF                    
         DC    X'C0C1C2C3C4C5C6C7C8C9404040404040'     C0-CF                    
         DC    X'D0D1D2D3D4D5D6D7D8D9404040404040'     D0-DF                    
         DC    X'E040E2E3E4E5E6E7E8E9404040404040'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F9404040404040'     F0-FF                    
*                                                                               
         LTORG                                                                  
                                                                                
ONEOFCTB DC    C'ABCDEFGH'                                                      
         DC    C'IJKLMNOP'                                                      
         DC    C'QRSTUVWX'                                                      
         DC    C'YZ123456'         0 is removed                                 
         DC    C'789'                                                           
         DC    C'<>?/`~!@'         = - , not allowed                            
         DC    C'#$%^*()_'                                                      
         DC    C'+{}º|\"'                                                      
         DC    C'.'                                                             
         DC    C'&&'                                                            
         DC    C''''                                                            
         DC    X'FF'                                                            
ONEOFCLN EQU   *-ONEOFCTB                                                       
                                                                                
PZERO    DC    P'0'                                                             
EZEROS   DC    C'00000000'                                                      
                                                                                
CLTKEYT  LKKEY H,CKEY,SAVED        ** CLIENT DRIVER TABLE **                    
         LKKEY LIT,CKEYTYPE,CKEYTYPQ                                            
         LKKEY WMP,CKEYAM,AMED                                                  
         LKKEY NZR,CKEYCLT                                                      
         LKKEY LIT,CKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
PRDKEYT  LKKEY H,PLSTPSSV,SAVED    ** CFM Product key driver table **           
         LKKEY LIT,PLSTTYPE,PLSTTYPQ                                            
         LKKEY LIT,PLSTSUB,PLSTSUBQ                                             
         LKKEY SIN,PLSTAM,SVCLTKEY+(CKEYAM-CKEY)                                
         LKKEY SIN,PLSTCLT,SVCLTKEY+(CKEYCLT-CKEY)                              
         LKKEY ALL,PLSTXFF                                                      
         LKKEY RNG,PLSTPRD,PRDRNGE                                              
         LKKEY ALL,PLSTBPRD                                                     
         LKKEY ALL,PLSTREST                                                     
         LKKEY E                                                                
                                                                                
MSTKEYT  LKKEY H,STAKEY,SAVED      ** Market/station driver table **            
         LKKEY LIT,STAKTYPE,STAKTYPQ                                            
         LKKEY SIN,STAKMED,MEDCOD                                               
         LKKEY ALL,STAKCALL                                                     
         LKKEY SIN,STAKAGY,QAGY                                                 
         LKKEY ALL,STAKCLT                                                      
         LKKEY ALL,STAKFILL                                                     
         LKKEY E                                                                
                                                                                
TPHKEYT  LKKEY H,PRHKEY,SAVED      ** Traffic prod house driver table *         
         LKKEY SIN,PRHKID,TPHRECQ                                               
         LKKEY SIN,PRHKAM,AGYMEDX                                               
         LKKEY SIN,PRHKPRH,HOUSECD                                              
         LKKEY E                                                                
                                                                                
FILES    DS    0X                  ** System/file list **                       
         DC    C'SPOT   '          System for open calls                        
                                                                                
         DC    C'N'                                                             
SPTDIR   DC    C'SPTDIR '                                                       
         DC    C'N'                                                             
SPTFIL   DC    C'SPTFILE'                                                       
         DC    C'N'                                                             
XSPDIR   DC    C'XSPDIR '                                                       
         DC    C'N'                                                             
XSPFIL   DC    C'XSPFIL '                                                       
         DC    C'N'                                                             
UNTDIR   DC    C'UNTDIR '                                                       
         DC    C'N'                                                             
UNTFIL   DC    C'UNTFIL '                                                       
         DC    C'N'                                                             
         DC    C'STAFILE'                                                       
         DC    C'NDEMDIRA'                                                      
         DC    C'NDEMDIRN'                                                      
         DC    C'NDEMDIRR'                                                      
         DC    C'NL=DEMFA'                                                      
         DC    C'NL=DEMFN'                                                      
         DC    C'NL=DEMFR'                                                      
         DC    C'NNTIDIR '                                                      
         DC    C'NL=NTIFL'                                                      
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'N'                                                             
GENDIR   DC    C'GENDIR '                                                       
         DC    C'N'                                                             
GENFIL   DC    C'GENFIL '                                                       
         DC    C'X'                                                             
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
                                                                                
LVALUES  DS    0F                  ** Literals, see WVALUES **                  
         DC    AL1(0,8,10,0)       ASSOCIATED WITH LABEL VS081000               
         DS    0H                                                               
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
SAVED    DSECT ,                   ** Dsect to cover saved storage **           
WVALUES  DS    0X                  ** LITERAL VALUES **                         
VS081000 DS    XL4                 PC VERSION 0.8.10.0                          
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
GETUID   DS    A                   A(GETUID)                                    
VTRPACK  DS    A                   A(TRPACK)                                    
VUNTIME  DS    A                   A(TRPACK)                                    
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK calling mode                          
                                                                                
SYSLET   DS    C                   ** System letter **                          
NETMEDQ  EQU   C'N'                Net media letter                             
                                                                                
SPTSYSQ  EQU   X'02'               SPOT SYSTEM NUMBER                           
                                                                                
QVALUES  DS    0F                  ** Request values **                         
* These tables are built in AIO8                                                
ABPRNXT  DS    A                   Binary product                               
ABPRLIST DS    A                                                                
                                                                                
ACMPRNXT DS    A                   3 char prod list                             
ACMLPR   DS    A                                                                
ACMPRX   DS    A                                                                
CMLPRQ   EQU   L'ELEM2                                                          
                                                                                
ACPRNXT  DS    A                   3 char prod list                             
ACPRLIST DS    A                                                                
ACPRX    DS    A                                                                
CPRLQ    EQU   1500                Max 500 prods                                
* End of tables in AIO8                                                         
                                                                                
QAGY     DS    CL(L'LP_AGY)                                                     
QAGYIND  DS    X                   Agency array                                 
QAAGY    DS    AL3                                                              
                                                                                
MEDIND   DS    X                   Media request array                          
AMED     DS    AL3                                                              
                                                                                
QCLTIND  DS    X                   Client request array                         
QACLT    DS    AL3                                                              
                                                                                
PRDIND   DS    X                   Product request array                        
QAPRD    DS    AL3                                                              
                                                                                
QVNDIND  DS    X                                                                
AVND     DS    AL3                 Array of Optica vendors                      
NUMVND   DS    XL(L'LW_NUMN)       N'vendors to process                         
                                                                                
QONEOFC  DS    C                   Agy using ONE char media office?             
QCLTOPT  DS    C                   Download client? Y/N option                  
QPRDOPT  DS    C                   Download product? Y/N option                 
QOPTION  DS    C                   Download options not used in Net             
                                                                                
QPRDA    DS    CL3                 Product alpha code                           
MEDCOD   DS    CL(L'QMEDA)         Media code (character format)                
AGYMEDX  DS    XL(L'QMEDX)         Binary Agency/media                          
HOUSECD  DS    CL(L'PRHKPRH)       house code for media/client                  
*                                                                               
QDUMMY   DS    CL1                 Dummy field                                  
*                                                                               
AMASTC   DS    A                   A(MASTER)                                    
SVNET    DS    CL4                 Save Network                                 
SVMED    DS    CL1                 Save sub-media                               
SUBMED   DS    CL1                 Sub-Media transmit                           
NETWORK  DS    CL4                 Network transmit                             
PRDCODE  DS    CL3                 Product code                                 
CMLCODE  DS    CL12                Commercial code                              
*                                                                               
RECACT   DS    C                   Record Action (L/D)                          
*                                                                               
STRDATEB DS    XL3                 Request start date (binary)                  
ENDDATEB DS    XL3                 Request end date (binary)                    
CMLPRFXS DS    XL8                 Packed of commercial search start            
CMLPRFXE DS    XL8                 Packed of commercial search end              
CMLPREFX DS    CL12                Prefix for commercial search                 
CMLPRFXL DS    XL1                 L(Prefix for commercial)                     
*                                                                               
PROGCDE  DS    CL6                 Program code from net prog rec               
PROGNME  DS    CL16                Program name                                 
PROGEDTE DS    XL3                 Program end date                             
*                                                                               
DATADISP DS    H                   Displacement to first element                
*                                                                               
SVVND    DS    CL3                 Vendor code                                  
SHWVLD   DS    C                   Valid request                                
SHWCML   DS    C                   Show commercial(s)                           
SHWCLT   DS    C                        client                                  
SHWPRD   DS    C                        product(s)                              
CLTCHG   DS    C                   Client changed indicator                     
CMLDLSW1 DS    C                   Commercial download switch 1                 
*                                                                               
QVALUEL  EQU   *-QVALUES                                                        
                                                                                
ENVIRO   DS    X                   Enviroment to use                            
SVPBLDAT DS    XL(L'PBILLDT)                                                    
SVPBLBAS DS    XL(L'PBILLBAS)                                                   
SVPBLCOM DS    XL(L'PBILLCOM)                                                   
*                                                                               
LAGY     DS    CL(L'CA_AGYA)       Last (previous) agency                       
ANXTCLT  DS    A                   A(next client code in work map pool)         
NUMCLT   DS    XL(L'LW_NUMN)       N'client codes left to process               
*                                                                               
ANXTVND  DS    A                   A(next vendor code in work map pool)         
*                                                                               
AFRSCLT  DS    A                   A(1st client)                                
*                                                                               
LOOPCT   DS    C                                                                
ELCODE   DS    X                                                                
SVCLTOFF DS    CL1                                                              
*                                                                               
SVCLTKEY DS    CL(L'CKEY)          Client record key                            
SVREQTOK DS    CL(L'CA_TOKEN)      Client array token                           
*                                                                               
PRDRNGE  DS    0XL(3*2)            ** Product key reading range **              
PRDRSTR  DS    XL3                 Start of range                               
PRDREND  DS    XL3                 End of range                                 
*                                                                               
*                                                                               
SVCMLFLS DS    0X                  Start of saved commercial rec fields         
SVCMLFLG DS    XL1                 Saved flag                                   
CML_ALLQ EQU   X'80'               Need to download all products                
CHARQ    EQU   X'40'               3 char product element found                 
*                                                                               
SVCMLUFN DS    CL1                 Saved traffic commercial UFN flag            
SVCMLAID DS    CL(L'CMLADID)       Saved traffic commercial Ad-ID               
SVCMLDS1 DS    CL(L'CMLDSC)        Saved traffic commercial title 1             
SVCMLDS2 DS    CL(L'CMLDSC)        Saved traffic commercial title 2             
SVCMLDS3 DS    CL(L'CMLDSC)        Saved traffic commercial title 3             
SVHDCMLC DS    CL(L'CMLXHDEF)      Saved HiDef commercial code                  
SVCMLFLX DS    0X                  End of saved commercial rec fields           
SVCMLFLQ EQU   *-SVCMLFLS          Length of saved commercial rec flds          
*                                                                               
SVPARENT DS    CL12                Parent commercial                            
SVMSTIME DS    CL6                 Match start time                             
SVMETIME DS    CL6                 Match end time                               
SVDDATE  DS    CL8                 Destroy date                                 
SVDTIME  DS    CL5                 Destroy time (2400=12A,0=NONE)               
SVDAILY  DS    C                   Check times daily                            
SVFORMAT DS    C                   Format H for hidef                           
*                                                                               
SVCMLAD  DS    CL(L'CMLADID)       Commercial adid                              
SVCMLHD  DS    CL(L'CMLXHDEF)      Commercial hidef                             
SVKEY    DS    CL(L'NRCPKEY)       Save recap key                               
*                                                                               
SVCKSM   DS    XL4                 Record check sum                             
CDELETE  DS    C                   Cml is deleted flag                          
*                                                                               
SVMPERID DS    XL24                Match period (6 sets of)                     
SVMSDAT1 DS   0XL2                 Match start dates                            
SVMEDAT1 DS   0XL2                 And end dates                                
*                                                                               
SVNETSI  DS    CL16                4 Networks (include)                         
SVNETSX  DS    CL16                4 Networks (exclude)                         
*                                                                               
SVPCMLQ  EQU   *-SVCMLUFN          Cml flds to clear nxt time around            
SVCMLQ2  EQU   *-SVCMLFLS          Length of all saved cml rec flds             
*                                                                               
SVACTULS DS  0CL48                                                              
SVACTUL1 DS   CL12                 8-12 char actual cml (space padded)          
SVACTUL2 DS   CL12                                                              
SVACTUL3 DS   CL12                                                              
SVACTUL4 DS   CL12                                                              
*                                                                               
DONESW   DS    C                                                                
*                                                                               
TPHRECQ  DS    XL(L'PRHKID)        Traffic prod house record code               
TRCRECQ  DS    XL(L'NRCPKID)       Traffic net recap record code                
*                                                                               
TOKEN    DS    CL200               TOKEN INFO                                   
*                                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
* Other included books follow                                                   
       ++INCLUDE NENAVWORKD                                                     
                                                                                
WORKD    DSECT ,                   ** Redefine OVERWORK **                      
         ORG   OVERWORK                                                         
VRECUP   DS    V                   RECUP FROM COMFACS                           
                                                                                
DUMMY_D  DSECT                                                                  
DUM_LIN1 DS    XL1                                                              
                                                                                
       ++INCLUDE GEGENOFF                                                       
       ++INCLUDE GEGENSDR                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FAXPEQUS                                                       
       ++INCLUDE GEMAPEQUS                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE GEGENTOK                                                       
       ++INCLUDE FAFACTS                                                        
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030NENAV16   03/01/18'                                      
         END                                                                    
