*          DATA SET NENAV23    AT LEVEL 034 AS OF 09/15/20                      
*PHASE T31823B                                                                  
NENAV23  TITLE '- NETPAK MATCHMAKER - DOWNLOAD UNITS AND INVOICES'              
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,WORKERKEY=NEMM,APPEND=Y,CODE=CODE,REQUEST=*,     *        
               FILES=FILES,FACS=FACS,ABENDLIST=ABENDS,SLOWLIST=SLOWS,  *        
               SBLOCK=SAVE,SERVERTYPE=TSTNETM,SYSTEM=NETSYSQ,IDF=Y,    *        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#NETIOD,NETIOD,    *        
               B#LP,LP_D,B#UNIT,NURECD,B#CLT,CLTHDR,B#PRD,PRDHDR,      *        
               B#EST,ESTHDR,B#INV,IDTVALS)                                      
                                                                                
CODE     NMOD1 WORKL,**NN23*,CLEAR=YES                                          
         USING WORKD,RC            RC=A(LOCAL W/S)                              
         LR    R6,R1                                                            
         USING LP_D,R6             R6=A(LP_D)                                   
         BASR  R7,0                                                             
         AHI   R7,GLOBALS-*                                                     
         USING GLOBALS,R7          R7=A(GLOBAL LITERALS)                        
         LR    R0,RE                                                            
         L     RF,LP_ARUNP                                                      
         USING RUNPARMD,RF         RF=A(RUNPARMS)                               
         MVC   RUNMODE,RUNPMODE    EXTRACT RUN MODE                             
         SR    RE,RE                                                            
         ICM   RE,7,RUNPARUN                                                    
         USING RUNFACSD,RE         RE=A(RUNFACS)                                
         L     R8,RSVRSAVE                                                      
         USING SAVED,R8            R8=A(SERVER SAVE AREA)                       
         L     R1,RSYSFACS                                                      
         MVC   FACLIST(FACLISTL),0(R1)                                          
                                                                                
         MVC   AMASTC,RMASTC       EXTRACT RUNFACS ADDRESSES                    
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   ACPRINT2,RCPRINT2                                                
         MVC   PRTLOG,RPRTLOG                                                   
         MVC   BUFFERIN,RBUFFRIN                                                
                                                                                
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   GETPROF,CGETPROF    EXTRACT COMFACS ADDRESSES                    
         MVC   DATCON,CDATCON                                                   
         MVC   ADDAY,CADDAY                                                     
         MVC   DATVAL,CDATVAL                                                   
         MVC   GETDAY,CGETDAY                                                   
         MVC   PERVAL,CPERVAL                                                   
         DROP  RE,RF                                                            
                                                                                
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR CALLED ROUTINES           
                                                                                
         USING TSARD,TSARBLK       TSAR BLOCK                                   
         USING TRECD,TSARREC       TSAR RECORD                                  
         USING NDPTKEY,KEY         DAYPART RECORD                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
         MVC   SVALUES(SVALUEL),LVALUES                                         
                                                                                
         LA    R0,WORKD            SET BLOCK ADDRESSES                          
B#WORKD  EQU   1                   WORKD                                        
         ST    R0,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         LA    R0,SAVED                                                         
B#SAVED  EQU   2                   SAVED                                        
         ST    R0,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         LA    R0,NETIOD                                                        
B#NETIOD EQU   3                   NETIOD                                       
         ST    R0,LP_BLKS+((B#NETIOD-1)*L'LP_BLKS)                              
         LA    R0,SAVED                                                         
         AHI   R0,IO1-SAVED                                                     
B#AGY    EQU   4                   AGENCY RECORD                                
         ST    R0,LP_BLKS+((B#AGY-1)*L'LP_BLKS)                                 
         AHI   R0,L'IO1                                                         
B#CLT    EQU   5                   CLIENT RECORD                                
         ST    R0,LP_BLKS+((B#CLT-1)*L'LP_BLKS)                                 
         AHI   R0,L'IO2                                                         
B#PRD    EQU   6                   PRODUCT RECORD AND                           
B#EST    EQU   6                   ESTIMATE RECORD                              
         ST    R0,LP_BLKS+((B#PRD-1)*L'LP_BLKS)                                 
         AHI   R0,L'IO3                                                         
*                                                                               
         LA    R0,WORKD            6K UNTFILE                                   
         AHI   R0,IO4-WORKD                                                     
B#UNIT   EQU   7                   UNIT RECORDS AND                             
B#INV    EQU   7                   INVOICE RECORDS (BIG I/O AREA)               
         ST    R0,LP_BLKS+((B#UNIT-1)*L'LP_BLKS)                                
B#LP     EQU   8                                                                
         LA    R0,LP_D                                                          
         ST    R0,LP_BLKS+((B#LP-1)*L'LP_BLKS)                                  
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
                                                                                
         LA    R0,SAVECLR          CLEAR DOWN SAVE AREA                         
         LHI   R1,SAVECLRL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   ENDDATE,EFFS                                                     
                                                                                
         MVC   NBSELID,LP_USRID                                                 
         MVC   NBSELAGY,LP_AGY                                                  
         MVC   NBACOM,ACOMFACS                                                  
         MVC   NBCLPACK,CLPACK                                                  
         MVC   NBCLUNPK,CLUNPK                                                  
         MVC   NBNETVAL,NETVALUE                                                
         MVC   NBDEMCON,DEMOCON                                                 
         MVC   NBAAGY,LP_BLKS+((B#AGY-1)*L'LP_BLKS)                             
         MVC   NBACLI,LP_BLKS+((B#CLT-1)*L'LP_BLKS)                             
         MVC   NBAPRD,LP_BLKS+((B#PRD-1)*L'LP_BLKS)                             
         MVC   NBAEST,LP_BLKS+((B#EST-1)*L'LP_BLKS)                             
         MVC   NBAIO,LP_BLKS+((B#UNIT-1)*L'LP_BLKS)                             
         LA    R0,NDDEMBLK                                                      
         ST    R0,NBADEM           SET A(DEMO LOOKUP BLOCK)                     
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   NBDM,CDATAMGR                                                    
         MVC   NBCALLOV,CCALLOV                                                 
         MVC   NBDATCON,CDATCON                                                 
         MVC   NBGETDAY,CGETDAY                                                 
         MVC   NBADDAY,CADDAY                                                   
         MVC   NBHEXOUT,CHEXOUT                                                 
         MVC   NBHELLO,CHELLO                                                   
         MVC   NBDEMADR,CDEMADDR                                                
         MVC   NBDEMAIN,CDEMAINT                                                
         MVC   NBDEMAND,CDEMAND                                                 
         MVC   NBDEMEL,CDEMEL                                                   
         MVC   NBDEMMTH,CDEMOMTH                                                
         MVC   NBDEMOUT,CDEMOUT                                                 
         MVC   NBGTPROF,CGETPROF                                                
         DROP  RF                                                               
                                                                                
         LHI   RF,NUDATA-NURECD                                                 
         STCM  RF,3,NBDTADSP                                                    
         MVI   NBESTOPT,NBESTOUQ                                                
         OI    NBVARIND,NBVARUDQ   DON'T READ ESTIMATES                         
                                                                                
         OC    AMASTC,AMASTC       TEST OFFLINE                                 
         BZ    PRCWRK02                                                         
         GOTOR NBDM,DMCB,DMKEY,SPTDIR,(4,0),0                                   
         GOTOR NBDM,DMCB,DMKEY,SPTFIL,(4,0),0                                   
         GOTOR NBDM,DMCB,DMKEY,UNTDIR,(4,0),0                                   
         GOTOR NBDM,DMCB,DMKEY,UNTFIL,(4,0),0                                   
         GOTOR NBDM,DMCB,DMKEY,XSPDIR,(4,0),0                                   
         GOTOR NBDM,DMCB,DMKEY,XSPFIL,(4,0),0                                   
         LA    RE,MMSAVE                                                        
         STCM  RE,15,LP_ABLKF                                                   
                                                                                
PRCWRK02 LA    R3,KEY              BUILD KEY OF AGENCY RECORD                   
         USING AGYKEY,R3                                                        
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,AGYKTYPQ                                                
         MVC   AGYKAGY,NBSELAGY                                                 
         GOTOR DIRREAD,NBFILSPT                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR FILGETR,NBAAGY      READ THE AGENCY RECORD                       
                                                                                
         L     R3,NBAAGY           RESOLVE AGENCY/MEDIA CODE                    
         LA    R3,AGYEL                                                         
         USING AGYMEDEL,R3                                                      
         SR    R0,R0                                                            
PRCWRK04 CLI   AGYMEDEL,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   AGYMEDEL,AGYMEDEQ                                                
         BNE   *+12                                                             
         CLI   AGYMEDCD,NETMEDQ    TEST NETPAK MEDIA CODE                       
         BE    *+14                                                             
         IC    R0,AGYMEDLN                                                      
         AR    R3,R0                                                            
         B     PRCWRK04                                                         
         MVC   NBACTAM,AGYMEDBT    SET AGENCY/MEDIA CODE                        
                                                                                
         XC    NDPTKEY,NDPTKEY                                                  
         MVI   NDPTKTY,NDPTKTYQ                                                 
         MVI   NDPTKST,NDPTKSTQ                                                 
         MVC   NDPTAGM,NBACTAM                                                  
         MVC   KEYSAVE,NDPTKEY                                                  
         GOTOR DIRHIGH,NBFILUNT    DO READ HIGH FOR THE KEY                     
         CLC   NDPTKEY(NDPTCLT-NDPTKEY),KEYSAVE                                 
         BNE   *+8                                                              
         OI    INDS,INDS2CDP       SET USING 2 CHARACTER DAYPART CODES          
         J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
                                                                                
         L     RE,LP_AWMP                                                       
         LA    RF,((L'PRDALPH*500)+LW_LN2Q)(RE)                                 
         ST    RF,LP_AWMP                                                       
         ST    RE,APRDLST          SET PRDLIST WMP                              
                                                                                
         SR    RE,RE                                                            
         ICM   RE,7,ADAT+1                                                      
         BZ    *+10                                                             
         MVC   MOSVALS,LW_DATA1-LW_D(RE)                                        
MOS      USING MOSD,MOSVALS                                                     
                                                                                
         CLC   LP_QMAPN,MAP#UONR                                                
         BNE   RUNREQ02                                                         
         LHI   RE,UC#COST                                                       
         SRDL  RE,3                                                             
         A     RE,LP_AREQM                                                      
         SRL   RF,32-3                                                          
         IC    RF,BITLIST(RF)                                                   
         EX    RF,*+8                                                           
         BZ    RUNREQ02                                                         
         TM    0(RE),0                                                          
         MVI   COSTFILT,YESQ       SET COST FILTER ACTIVE                       
                                                                                
RUNREQ02 L     R1,ACLT                                                          
         MVC   NBACTCLI,LW_DATA1-LW_D(R1)                                       
                                                                                
         LA    R3,KEY                                                           
         USING CLTHDR,R3                                                        
         XC    CKEY,CKEY                                                        
         MVC   CKEYAM,NBACTAM                                                   
         MVC   CKEYCLT,NBACTCLI                                                 
         GOTOR DIRREAD,NBFILSPT                                                 
         JNE   EXITN                                                            
         GOTOR FILGETR,NBACLI      GET CLIENT RECORD                            
         L     R3,NBACLI                                                        
         MVC   NBEFFOFF,COFFICE    SET OFFICE                                   
         GOTOR NBCLUNPK,DMCB,(CPROF+6,NBACTCLI),NBCLICOD                        
                                                                                
         L     R4,LP_AWMP                                                       
         ST    R4,ACLTPRD          SET CLIENT/PRODUCT LIST MAP                  
                                                                                
         LA    R3,KEY                                                           
         USING PRDHDR,R3                                                        
         XC    PLSTPSSV,PLSTPSSV                                                
         MVI   PLSTTYPE,PLSTTYPQ                                                
         MVI   PLSTSUB,PLSTSUBQ                                                 
         MVC   PLSTAM,NBACTAM                                                   
         MVC   PLSTCLT,NBACTCLI                                                 
                                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTOR DIRHIGH,NBFILSPT                                                 
         J     RUNREQ06                                                         
                                                                                
RUNREQ04 GOTOR DIRRSEQ,NBFILSPT      READ PRODUCT PASSIVE KEYS                  
RUNREQ06 CLC   KEY(PLSTXFF-PLSTPSSV),KEYSAVE  FINISHED WITH CLIENT?             
         JNE   RUNREQ08                                                         
         MVC   0(L'PLSTPRD,R4),PLSTPRD                                          
         MVC   L'PLSTPRD(L'NUPRD,R4),PLSTBPRD+1                                 
         AHI   R4,L'PLSTPRD+L'NUPRD                                             
         J     RUNREQ04                                                         
                                                                                
RUNREQ08 XC    0(L'PLSTPRD+L'NUPRD,R4),0(R4)   END OF TABLE                     
         AHI   R4,L'PLSTPRD+L'NUPRD                                             
         ST    R4,LP_AWMP                                                       
                                                                                
         L     R3,NBAIO                                                         
         USING STARECD,R3          READ STATION MASTER RECORD                   
         XC    STAKEY,STAKEY                                                    
         MVI   STAKTYPE,STAKTYPQ                                                
         MVI   STAKMED,NETMEDQ                                                  
         L     RE,ANWK                                                          
         MVC   NBSELNET,LW_DATA1-LW_D(RE)                                       
         MVC   STAKCALL(L'NBSELNET),NBSELNET                                    
         MVI   STAKCALL+L'STAKCALL-1,NETMEDQ                                    
         OC    STAKCALL,BLANKS                                                  
         MVC   STA,LW_DATA1+L'NUKNET-LW_D(RE)                                   
         MVC   STAKAGY,NBSELAGY                                                 
         MVC   STAKCLT(L'STAKCLT+L'STAKFILL),EZEROES                            
         MVC   KEYSAVE,STAKEY                                                   
         GOTOR NBDM,DMCB,DMREAD,STAFIL,STAKEY,STAKEY                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   STAMEDIA,STYPE      EXTRACT STATION MEDIA TYPE                   
         MVC   STAFLAG,SFLAG1      EXTRACT STATION FLAG                         
         DROP  R3                                                               
                                                                                
         CLI   STAMEDIA,C' '                                                    
         BNE   *+8                                                              
         MVI   STAMEDIA,NETMEDQ                                                 
                                                                                
         XC    WORK,WORK           BUILD PROFILE KEY & GET PROFILES             
         MVC   WORK+04(L'NBSELAGY),NBSELAGY                                     
         MVI   WORK+06,NETMEDQ                                                  
         MVC   WORK+07(L'NBCLICOD),NBCLICOD                                     
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF                                              
                                                                                
         MVC   WORK(L'PROGB0),PROGB0                                            
         GOTOR GETPROF,DMCB,WORK,PROFB0,NBDM                                    
                                                                                
         MVC   WORK(L'PROGMK),PROGMK                                            
         GOTOR GETPROF,DMCB,WORK,PROFMK,NBDM                                    
                                                                                
         MVC   WORK(L'PROGN0),PROGN0                                            
         GOTOR GETPROF,DMCB,WORK,PROFN0,NBDM                                    
                                                                                
         MVC   WORK(L'PROGN1),PROGN1                                            
         GOTOR GETPROF,DMCB,WORK,PROFN1,NBDM                                    
         CLI   PROFN1+13,YESQ                                                   
         BE    *+8                                                              
         MVI   PROFN1+13,0                                                      
                                                                                
         MVC   WORK(L'PROGN2),PROGN2                                            
         GOTOR GETPROF,DMCB,WORK,PROFN2,NBDM                                    
         CLI   PROFN2+14,YESQ                                                   
         BE    *+8                                                              
         MVI   PROFN2+14,0                                                      
                                                                                
         XC    WORK,WORK           BUILD PROFILE KEY & GET PROFILES             
         MVC   WORK+04(L'NBSELAGY),NBSELAGY                                     
         MVI   WORK+06,NETMEDQ                                                  
         MVC   WORK+07(L'NBCLICOD),NBCLICOD                                     
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF                                              
                                                                                
         MVC   WORK(L'PROGI2Z),PROGI2Z                                          
         GOTOR GETPROF,DMCB,WORK,PROFI2Z,NBDM                                   
         CLI   PROFI2Z+15,YESQ                                                  
         BNE   RUNREQ10                                                         
         MVC   WORK+6(L'STAMEDIA),STAMEDIA                                      
         GOTOR GETPROF,DMCB,WORK,PROFI2Z,NBDM                                   
                                                                                
RUNREQ10 MVI   WORK+6,NETMEDQ                                                   
         CLI   PROFI2Z+6,YESQ                                                   
         BNE   *+10                                                             
         MVC   WORK+6(L'STAMEDIA),STAMEDIA                                      
                                                                                
         MVC   WORK(L'PROGI2),PROGI2                                            
         GOTOR GETPROF,DMCB,WORK,PROFI2,NBDM                                    
                                                                                
         MVC   WORK(L'PROGI2S),PROGI2S                                          
         GOTOR GETPROF,DMCB,WORK,PROFI2S,NBDM                                   
                                                                                
         MVC   WORK(L'PROGI2X),PROGI2X                                          
         GOTOR GETPROF,DMCB,WORK,PROFI2X,NBDM                                   
                                                                                
         MVC   WORK(L'PROGI2A),PROGI2A                                          
         GOTOR GETPROF,DMCB,WORK,PROFI2A,NBDM                                   
                                                                                
         MVC   WORK(L'PROGI2B),PROGI2B                                          
         GOTOR GETPROF,DMCB,WORK,PROFI2B,NBDM                                   
                                                                                
         MVC   WORK(L'PROGI2C),PROGI2C                                          
         GOTOR GETPROF,DMCB,WORK,PROFI2C,NBDM                                   
                                                                                
         MVC   WORK(L'PROGI2N),PROGI2N                                          
         GOTOR GETPROF,DMCB,WORK,PROFI2N,NBDM                                   
                                                                                
         MVC   WORK(L'PROGTI),PROGTI                                            
         GOTOR GETPROF,DMCB,WORK,PROFTI,NBDM                                    
                                                                                
         MVI   WORK+6,NETMEDQ                                                   
         CLI   PROFI2A+8,YESQ                                                   
         BNE   *+10                                                             
         MVC   WORK+6(L'STAMEDIA),STAMEDIA                                      
                                                                                
         MVC   WORK(L'PROGI2Y),PROGI2Y                                          
         GOTOR GETPROF,DMCB,WORK,PROFI2Y,NBDM                                   
                                                                                
         MVC   WORK(L'PROGI2I),PROGI2I                                          
         GOTOR GETPROF,DMCB,WORK,PROFI2I,NBDM                                   
                                                                                
         XC    MOSI2BCF,MOSI2BCF                                                
         OC    PROFI2B+4(2),PROFI2B+4                                           
         BZ    RUNREQ12                                                         
         MVC   WORK(2),PROFI2B+4                                                
         MVI   WORK+2,1                                                         
         GOTOR DATCON,DMCB,(3,WORK),(0,WORK+3)                                  
         GOTOR (RF),(R1),(0,WORK+3),(3,WORK)                                    
         MVC   MOSI2BCF,WORK                                                    
                                                                                
RUNREQ12 XC    MOSI2BCT,MOSI2BCT                                                
         OC    PROFI2B+7(2),PROFI2B+7                                           
         BZ    RUNREQ14                                                         
         MVC   WORK(2),PROFI2B+7                                                
         MVI   WORK+2,1                                                         
         GOTOR DATCON,DMCB,(3,WORK),(0,WORK+3)                                  
         GOTOR (RF),(R1),(0,WORK+3),(3,WORK)                                    
         MVC   MOSI2BCT,WORK                                                    
                                                                                
RUNREQ14 XC    MOSI2BBF,MOSI2BBF                                                
         OC    PROFI2B+14(2),PROFI2B+14                                         
         BZ    RUNREQ16                                                         
         MVC   WORK(2),PROFI2B+14                                               
         MVI   WORK+2,1                                                         
         GOTOR DATCON,DMCB,(3,WORK),(0,WORK+3)                                  
         GOTOR (RF),(R1),(0,WORK+3),(3,WORK)                                    
         MVC   MOSI2BBF,WORK                                                    
                                                                                
RUNREQ16 XC    MOSI2BCM,MOSI2BCM                                                
         OC    PROFI2B+1(2),PROFI2B+1                                           
         BZ    RUNREQ18                                                         
         MVC   WORK(2),PROFI2B+1                                                
         MVI   WORK+2,1                                                         
         GOTOR DATCON,DMCB,(3,WORK),(0,WORK+3)                                  
         GOTOR (RF),(R1),(0,WORK+3),(3,WORK)                                    
         MVC   MOSI2BCM,WORK                                                    
                                                                                
RUNREQ18 XC    MOSI2ARP,MOSI2ARP                                                
         OC    PROFI2A+6(2),PROFI2A+6                                           
         BZ    RUNREQ20                                                         
         MVC   WORK(2),PROFI2A+6                                                
         MVI   WORK+2,1                                                         
         GOTOR DATCON,DMCB,(3,WORK),(0,WORK+3)                                  
         GOTOR (RF),(R1),(0,WORK+3),(3,WORK)                                    
         MVC   MOSI2ARP,WORK                                                    
                                                                                
RUNREQ20 XC    MOSI2CCL,MOSI2CCL                                                
         OC    PROFI2C+8(2),PROFI2C+8                                           
         BZ    RUNREQ22                                                         
         MVC   WORK(2),PROFI2C+8                                                
         MVI   WORK+2,1                                                         
         GOTOR DATCON,DMCB,(3,WORK),(0,WORK+3)                                  
         GOTOR (RF),(R1),(0,WORK+3),(3,WORK)                                    
         MVC   MOSI2CCL,WORK                                                    
                                                                                
RUNREQ22 XC    MIRRDATE,MIRRDATE   SET NO MIRROR START DATE                     
         OC    PROFI2A+3(2),PROFI2A+3                                           
         BZ    RUNREQ24                                                         
         MVC   DUB(2),PROFI2A+3    SET MIRROR START DATE FROM PROFILE           
         SR    R0,R0                                                            
         IC    R0,DUB                                                           
         CHI   R0,50                                                            
         BH    *+8                                                              
         AHI   R0,100                                                           
         STC   R0,DUB                                                           
         MVI   DUB+2,1                                                          
         GOTOR DATCON,(R1),(3,DUB),(2,MIRRDATE)                                 
                                                                                
         CLI   PROFI2+9,C'B'       BROADCAST CALENDAR?                          
         BNE   RUNREQ24                                                         
                                                                                
         GOTOR DATCON,DMCB,(3,DUB),(0,WORK)                                     
         GOTOR GETBRD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                         
         CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR DATCON,DMCB,(0,WORK+6),(2,MIRSTDTE)                              
         GOTOR DATCON,DMCB,(0,WORK+12),(2,MIRENDTE)                             
         MVC   MIRRDATE,MIRSTDTE                                                
                                                                                
RUNREQ24 XC    MOSTIVAL,MOSTIVAL                                                
         OC    PROFTI+11(2),PROFTI+11                                           
         BZ    RUNREQ26                                                         
         MVC   WORK(2),PROFTI+11                                                
         MVI   WORK+2,1                                                         
         GOTOR DATCON,DMCB,(3,WORK),(0,WORK+3)                                  
         GOTOR (RF),(R1),(0,WORK+3),(2,MOSTI)                                   
         GOTOR (RF),(R1),(0,WORK+3),(3,WORK)                                    
         MVC   MOSTIYM,WORK                                                     
                                                                                
RUNREQ26 OC    PROFI2Z+11(2),PROFI2Z+11                                         
         BZ    RUNREQ28                                                         
         SR    R0,R0                                                            
         IC    R0,PROFI2Z+11                                                    
         CHI   R0,50               IF YEAR > 50                                 
         BH    *+8                                                              
         AHI   R0,100              BUMP TO NEXT CENTURY                         
         STC   R0,PROFI2Z+11                                                    
         CLC   MOS.MOSB,PROFI2Z+11 TEST USE I2Z MATCH ON PACKAGE                
         BNL   RUNREQ28                                                         
         MVI   PROFI2Z+10,0        CLEAR MATCH PROFILE IF NOT EFFECTIVE         
                                                                                
RUNREQ28 CLI   PROFI2Z+10,NOQ      TEST NOT MATCHING ON PACKAGE                 
         BNE   *+8                                                              
         MVI   PROFI2Z+10,0        YES - DON'T SEND VALUE                       
         OC    PROFI2Z+8(2),PROFI2Z+8                                           
         BZ    RUNREQ30                                                         
         SR    R0,R0                                                            
         IC    R0,PROFI2Z+8                                                     
         CHI   R0,50               IF YEAR > 50                                 
         BH    *+8                                                              
         AHI   R0,100              BUMP TO NEXT CENTURY                         
         STC   R0,PROFI2Z+8                                                     
         CLC   MOS.MOSB,PROFI2Z+8  TEST USE I2Z TIME LEEWAY                     
         BL    RUNREQ30                                                         
         MVC   PROFI2+0(1),PROFI2Z+7                                            
         MVC   PROFI2X+13(1),PROFI2Z+14                                         
                                                                                
RUNREQ30 CLI   PROFI2X+13,0        TEST LEEWAY AFTER SET                        
         BNE   *+10                                                             
         MVC   PROFI2X+13(1),PROFI2+0                                           
         CLI   PROFI2X+13,255      TEST SPECIAL LEEWAY AFTER VALUE              
         BNE   *+8                                                              
         MVI   PROFI2X+13,0        YES - SET TO ACTUAL VALUE (ZERO)             
         MVC   WORK(L'PROGA0),PROGA0                                            
         XC    WORK+06(6),WORK+06  GET AGENCY LEVEL A0 PROFILE                  
         GOTOR (RF),(R1),WORK,PROFA0,NBDM                                       
                                                                                
         OC    PGRP,PGRP           TEST PRODUCT GROUP REQUEST                   
         BZ    RUNREQ38                                                         
                                                                                
         LA    R3,KEY                                                           
         USING PRGRECD,R3                                                       
         XC    PRGKEY,PRGKEY       READ PRODUCT GROUP HEADER RECORD             
         MVC   PRGKTYP,PRGKIDQ                                                  
         MVC   PRGPAGMD,NBACTAM                                                 
         MVC   PRGPCLT,NBACTCLI                                                 
         MVC   PRGPID(L'PRGPID+L'PRGPPRD),PGRP                                  
         GOTOR DIRREAD,NBFILSPT                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR FILGETR,NBAPRD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,NBAPRD                                                        
         USING PRGEL10,PRGEL                                                    
         CLI   PRGEL10,X'10'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PRDNAME,PRGNAM1     EXTRACT PRODUCT GROUP NAME                   
                                                                                
         LA    R3,KEY                                                           
         MVI   PRGPTYP+1,X'81'     READ PASSIVES TO BUILD PRODUCT LIST          
                                                                                
         USING LW_D,R2                                                          
         L     R2,APRDLST                                                       
         MVI   LW_TYPE,LW_TLSTQ                                                 
                                                                                
         LA    R4,LW_DATA2                                                      
         SR    R0,R0                                                            
         MVC   KEYSAVE,PRGKEY                                                   
         GOTOR DIRHIGH,NBFILSPT                                                 
         B     RUNREQ34                                                         
                                                                                
RUNREQ32 GOTOR DIRRSEQ,NBFILSPT                                                 
                                                                                
RUNREQ34 CLC   PRGKEY(PRGPPRD-PRGKEY),KEYSAVE                                   
         BNE   RUNREQ36                                                         
         MVC   0(L'PRGPPRD,R4),PRGPPRD                                          
         AHI   R4,L'PRGPPRD                                                     
         AHI   R0,1                                                             
         B     RUNREQ32                                                         
                                                                                
RUNREQ36 STCM  R0,3,LW_NUMN                                                     
         MVI   PRDNUM,LSTNUM       SET F CHARG PRODUCT NUMBER                   
         MVI   PRDLSTN,1                                                        
         MVC   PRDALPH,POLPRD                                                   
         B     RUNREQ40                                                         
         DROP  R2                                                               
                                                                                
RUNREQ38 L     R1,APRD                                                          
         MVC   PRDALPH,LW_DATA1-LW_D(R1)                                        
         MVI   PRDNUM,POLNUM                                                    
         CLC   PRDALPH,POLPRD      TEST 'POL' PRODUCT                           
         BNE   *+14                                                             
         CLC   LP_VRSN,V1100       TEST PC APPLICATION VERSION 1.1.0.0          
         BNL   RUNREQ40                                                         
                                                                                
         GOTOR GETPRD,PRDALPH      GET PRODUCT RECORD                           
         L     R1,NBAPRD                                                        
         USING PRDHDR,R1                                                        
         MVC   PRDNUM,PCODE+1                                                   
         MVC   PRDNAME,PNAME                                                    
                                                                                
         SR    R1,R1                                                            
         ICM   R1,15,APIG                                                       
         BZ    RUNREQ40                                                         
         AHI   R1,LW_DATA1-LW_D                                                 
         GOTOR GETPRD,(R1)         GET PIGGYBACK PRODUCT RECORD                 
         L     R1,NBAPRD                                                        
         MVC   PIGALPH,PKEYPRD     SET PIGGYBACK PRODUCT VALUES                 
         MVC   PIGNUM,PCODE+1                                                   
         MVC   PIGNAME,PNAME                                                    
                                                                                
RUNREQ40 LA    R0,I2COMMNT         CLEAR I2 COMMENT AREA                        
         LHI   R1,I2COMMLN                                                      
         SR    RE,RE                                                            
         LHI   RF,SPACEQ                                                        
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
                                                                                
         CLC   LP_QMAPN,MAP#NEMM   TEST MATCHMAKER UNITS & INVOICES             
         BNE   RUNREQ50                                                         
                                                                                
         LA    R3,KEY                                                           
         USING XCOMKEY,R3          BUILD KEY OF I2 COMMENT RECORD               
         XC    XCOMKEY,XCOMKEY                                                  
         MVI   COMI2KR,COMI2KRQ                                                 
         MVI   COMI2KS,COMI2KSQ                                                 
         MVC   COMI2KAM,NBACTAM                                                 
         MVI   COMI2KTY,COMI2KTQ                                                
         MVC   COMI2KCL,NBACTCLI                                                
         MVC   COMI2KPR,PRDALPH                                                 
         CLI   ESTIND,LQ_TALLQ     TEST NO ESTIMATE INPUT                       
         BE    RUNREQ42                                                         
         L     RE,AEST                                                          
         MVC   COMI2KES,LW_DATA1-LW_D(RE)                                       
         CLI   LW_TYPE-LW_D(RF),LQ_TSINQ                                        
         BE    *+10                                                             
         MVC   COMI2KE2,LW_DATA1+L'COMI2KES-LW_D(RE)                            
         CLC   COMI2KES,COMI2KE2                                                
         BNE   RUNREQ42                                                         
         MVI   COMI2KE2,0                                                       
RUNREQ42 MVC   COMI2KP2,PIGALPH                                                 
         MVC   COMI2KST,STA                                                     
         MVC   COMI2KYM,MOS.MOSB                                                
                                                                                
         OC    AMASTC,AMASTC       TEST OFFLINE                                 
         BZ    RUNREQ44                                                         
         L     RF,LP_ABLKF                                                      
         USING MMSAVED,RF          SAVE REQUEST VALUES                          
         MVC   MMQPRD,COMI2KPR                                                  
         MVC   MMQPRD2,COMI2KP2                                                 
         MVC   MMBEST,COMI2KES                                                  
         MVC   MMBEST2,COMI2KE2                                                 
         L     RE,ANWK                                                          
         MVC   MMQNET,LW_DATA1-LW_D(RE)                                         
         DROP  RF                                                               
                                                                                
RUNREQ44 GOTOR DIRREAD,NBFILXSP                                                 
         JNE   RUNREQ50                                                         
         GOTOR FILGETR,NBAIO       READ THE I2 COMMENT RECORD                   
                                                                                
         L     R3,NBAIO                                                         
         LA    R3,XCOMELS          R3=A(FIRST ELEMENT ON RECORD)                
         USING XCCTD,R3                                                         
         LA    R1,I2COMMNT         R1=A(OUTPUT COMMENT TEXT BLOCK)              
         LHI   R0,I2COMMMX         R0=MAXIMUM N'COMMENTS                        
         SR    RE,RE                                                            
RUNREQ46 CLI   XCCTEL,0            TEST END OF RECORD                           
         JE    RUNREQ50                                                         
         CLI   XCCTEL,XCCTELTQ     TEST COMMENT TEXT ELEMENT                    
         BE    *+12                                                             
         CLI   XCCTEL,XCCTELBQ                                                  
         BNE   RUNREQ48                                                         
         BCT   R0,*+8                                                           
         J     RUNREQ50                                                         
         IC    RE,XCCTLN                                                        
         SHI   RE,XCCTL1Q+1                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),XCCTEXT     MOVE TEXT TO OUTPUT BLOCK                    
         AHI   R1,L'I2COMMNT       BUMP TO NEXT OUTPUT ENTRY                    
RUNREQ48 IC    RE,XCCTLN           BUMP TO NEXT RECORD ELEMENT                  
         AR    R3,RE                                                            
         B     RUNREQ46                                                         
                                                                                
RUNREQ50 MVI   ESTCOUNT,0          INITIALIZE ESTIMATE COUNT                    
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY                                                            
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET PRODUCT RECORDS FOR PC APPLICATION VERSION 1.1.0.0 AND HIGHER   *         
***********************************************************************         
NXTPRD   J     *+12                                                             
         DC    CL8'*NXTPRD*'                                                    
         LR    RB,RF                                                            
         USING NXTPRD,RB                                                        
         MVC   LP_ADATA,NBAPRD                                                  
         CLI   LP_RMODE,LP_RFRST                                                
         BNE   NXTPRD02                                                         
                                                                                
K        USING PLSTPSSV,KEY                                                     
         XC    K.PLSTPSSV,K.PLSTPSSV                                            
         MVI   K.PLSTTYPE,PLSTTYPQ                                              
         MVI   K.PLSTSUB,PLSTSUBQ                                               
         MVC   K.PLSTAM,NBACTAM                                                 
         MVC   K.PLSTCLT,NBACTCLI                                               
         GOTOR DIRHIGH,NBFILSPT                                                 
         J     NXTPRD04                                                         
                                                                                
NXTPRD02 GOTOR DIRRSEQ,NBFILSPT                                                 
                                                                                
NXTPRD04 CLC   K.PLSTPSSV(PLSTXFF-PLSTPSSV),KEYSAVE                             
         JNE   NOMORE                                                           
         GOTOR FILGETR,NBAPRD                                                   
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  RB,K                                                             
         EJECT                                                                  
***********************************************************************         
* GET FILM RECORDS AND PRODUCTS                                       *         
***********************************************************************         
NXTFILM  J     *+12                                                             
         DC    CL8'*NXTFLM*'                                                    
         LR    RB,RF                                                            
         USING NXTFILM,RB                                                       
         MVC   LP_ADATA,NBAPRD                                                  
         CLI   LP_RMODE,LP_RFRST                                                
         BNE   NXTFLM02                                                         
                                                                                
         LA    R3,KEY                                                           
         USING CMLRECD,R3                                                       
         XC    CMLKEY,CMLKEY                                                    
         MVI   CMLKID,X'0A'                                                     
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,NBACTAM                                                   
         MVC   CMLKCLT,NBACTCLI                                                 
         GOTOR DIRHIGH,NBFILSPT                                                 
         J     NXTFLM04                                                         
                                                                                
NXTFLM02 GOTOR DIRRSEQ,NBFILSPT                                                 
                                                                                
NXTFLM04 CLC   KEY(CMLKCML-CMLKEY),KEYSAVE                                      
         JNE   NOMORE                                                           
         CLI   CMLKCML,0                                                        
         JE    NXTFLM02                                                         
                                                                                
         GOTOR FILGETR,NBAIO       READ THE FILM RECORD                         
         JNE   NOMORE                                                           
         XC    FILMVALS(FILMVALX),FILMVALS                                      
                                                                                
         L     R3,NBAIO                                                         
         USING CMLRECD,R3          R3=A(FILM RECORD)                            
                                                                                
         TM    CMLRSTAT,CMLKSTA_PCKD TEST PACKED ADID                           
         JO    *+10                                                             
         MVC   FILMCODE(L'CMLKCML),CMLKCML    FILMCODE                          
         OC    FILMCODE,BLANKS                                                  
                                                                                
         LA    R4,CMLDTAEL                                                      
         USING CMLXDTEL,R4                                                      
NXTFLM06 CLI   0(R4),0                                                          
         JE    NXTFLM12                                                         
         CLI   0(R4),CMLDTAEQ      CMML DATA ELEM                               
         JNE   NXTFLM07                                                         
         MVC   FILMSEQ,CMLSEQ      SEQUENCE #                                   
         MVC   FILMSLN,CMLSLN      CMML LENGTH                                  
         J     NXTFLM10                                                         
NXTFLM07 CLI   0(R4),CMLXDTEQ      EXTENDED DATA ELEM                           
         JNE   NXTFLM08                                                         
         MVC   FILMHDEF(L'CMLXHDEF),CMLXHDEF   HI DEF                           
         MVC   FILMCNTR(L'CMLXCNTR),CMLXCNTR   CENTERCUT                        
         OC    FILMHDEF,BLANKS                                                  
         OC    FILMCNTR,BLANKS                                                  
         J     NXTFLM10                                                         
                                                                                
         USING CMLADIEL,R4                                                      
NXTFLM08 CLI   0(R4),CMLADIDQ      AD-ID ELEM                                   
         JNE   NXTFLM10                                                         
         MVC   FILMADID(L'CMLADID),CMLADID                                      
         CLC   FILMCODE,BLANKS     TEST FILMCODE ALREADY EXISTS                 
         JNE   *+10                                                             
         MVC   FILMCODE(L'CMLADID),CMLADID  OVERWRITE FILM CODE W/ ADID         
         OC    FILMADID,BLANKS                                                  
         OC    FILMCODE,BLANKS                                                  
                                                                                
NXTFLM10 ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         J     NXTFLM06                                                         
                                                                                
NXTFLM12 LA    R0,FILMPRD          CLEAR FILM PRODCUCT AREA                     
         LHI   R1,FILMPRDL                                                      
         SR    RE,RE                                                            
         LHI   RF,SPACEQ                                                        
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
                                                                                
         LA    R1,FILMPRD          R1=A(OUTPUT FILM PRODUCT BLOCK)              
         LHI   R0,FILMPRDX         R0=MAXIMUM N'FILM PRODUCTS                   
         MVC   0(L'FILMPRD,R1),=C'ALL'                                          
                                                                                
         LA    R4,CMLDTAEL                                                      
NXTFLM14 CLI   0(R4),0                                                          
         JE    NXTFLM15                                                         
         CLI   0(R4),CMLPRDAQ      ALPHA PRODUCT LIST                           
         JE    NXTFLM18                                                         
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         J     NXTFLM14                                                         
                                                                                
NXTFLM15 LA    R4,CMLDTAEL                                                      
NXTFLM16 CLI   0(R4),0                                                          
         JE    EXITY                                                            
         CLI   0(R4),CMLPRDQ       NON ALPHA PRODUCT LIST                       
         JE    NXTFLM22                                                         
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         J     NXTFLM16                                                         
                                                                                
         USING CMLMPREL,R4                                                      
NXTFLM18 CLI   CMLMPRS,FF          ALL PRODUCTS?                                
         JE    EXITY                                                            
         CLI   CMLMPRS,0                                                        
         JE    NXTFLM16                                                         
         SR    RE,RE                                                            
         ZIC   RF,1(R4)                                                         
         SHI   RF,2                                                             
         D     RE,=F'3'                                                         
         LA    R4,CMLMPRS                                                       
NXTFLM20 MVC   0(L'FILMPRD,R1),0(R4)                                            
         AHI   R1,L'FILMPRD                                                     
         AHI   R4,L'CMLMPRS                                                     
         BCT   RF,NXTFLM20                                                      
         J     EXITY                                                            
         DROP  R4                                                               
                                                                                
         USING CMLPRDEL,R4                                                      
NXTFLM22 CLI   CMLPRDS,FF          ALL PRODUCTS?                                
         JE    EXITY                                                            
         CLI   CMLPRDS,0                                                        
         JE    NXTFLM16                                                         
         ZIC   RF,1(R4)                                                         
         SHI   RF,2                                                             
         LA    R4,CMLPRDS                                                       
                                                                                
NXTFLM24 L     R5,NBACLI           LOOK UP PRODUCT ALPHA                        
         USING CLTHDR,R5                                                        
         LA    R5,CLIST                                                         
         SR    R6,R6                                                            
         L     R6,=F'880'                                                       
NXTFLM26 CLC   3(L'CMLPRDS,R5),0(R4)                                            
         JE    NXTFLM30                                                         
         AHI   R5,4                                                             
         BCT   R6,NXTFLM26                                                      
                                                                                
         L     R5,NBACLI                                                        
         LA    R5,CLIST2                                                        
         SR    R6,R6                                                            
         L     R6,=F'35'                                                        
NXTFLM28 CLC   3(L'CMLPRDS,R5),0(R4)                                            
         JE    NXTFLM30                                                         
         AHI   R5,4                                                             
         BCT   R6,NXTFLM28                                                      
         J     EXITY                                                            
                                                                                
NXTFLM30 MVC   0(L'FILMPRD,R1),0(R5)                                            
         AHI   R1,L'FILMPRD                                                     
         AHI   R4,L'CMLPRDS                                                     
         BCT   RF,NXTFLM24                                                      
         J     EXITY                                                            
         DROP  RB,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* GET ESTIMATE RECORDS                                                *         
***********************************************************************         
                                                                                
GETEST   J     *+12                                                             
         DC    CL8'*GETEST*'                                                    
         LR    RB,RF                                                            
         USING GETEST,RB                                                        
         MVC   LP_ADATA,NBAEST                                                  
         L     R2,AEST                                                          
         USING LW_D,R2             R2=A(ESTIMATE REQUEST ENTRY)                 
                                                                                
GETEST02 LA    R3,KEY                                                           
         USING ESTHDR,R3           R3=A(ESTIMATE KEY)                           
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,NBACTCLI                                                 
         MVC   EKEYPRD,PRDALPH                                                  
                                                                                
         CLI   ESTIND,LQ_TSINQ     TEST SINGLE ESTIMATE REQUEST                 
         BNE   GETEST04                                                         
         CLI   LASTEST,0           TEST THIS IS THE FIRST TIME                  
         JNE   NOMORE              NO - ALL DONE                                
         MVC   EKEYEST,LW_DATA1                                                 
         B     GETEST08                                                         
                                                                                
GETEST04 TM    ESTIND,LQ_TRNGQ     TEST RANGE OF ESTIMATES REQUEST              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   LASTEST,0           TEST THIS IS THE FIRST TIME                  
         BNE   GETEST06                                                         
         MVC   EKEYEST,LW_DATA1    SET FIRST ESTIMATE IN RANGE                  
         CLI   EKEYEST,0                                                        
         BNE   *+8                                                              
         MVI   EKEYEST,1           SET LOW ESTIMATE NUMBER IF NOT GIVEN         
         B     GETEST08                                                         
                                                                                
GETEST06 SR    R0,R0                                                            
         IC    R0,LASTEST                                                       
         AHI   R0,1                                                             
         CHI   R0,255                                                           
         JH    NOMORE                                                           
         CLI   ESTIND,LW_TALLQ                                                  
         JE    *+12                                                             
         CLM   R0,1,LW_DATA1+L'EKEYEST                                          
         JH    NOMORE                                                           
         STC   R0,EKEYEST                                                       
                                                                                
GETEST08 MVC   LASTEST,EKEYEST                                                  
         GOTOR DIRHIGH,NBFILSPT                                                 
         CLC   EKEY(EKEYEST-EKEY),KEYSAVE                                       
         JNE   NOMORE                                                           
         OC    EKEYZERO,EKEYZERO   TEST THIS IS AN ESTIMATE                     
         BNZ   GETEST02                                                         
         MVC   LASTEST,EKEYEST     SET ESTIMATE NUMBER FOUND                    
         CLI   ESTIND,LQ_TSINQ     TEST SINGLE ESTIMATE REQUEST                 
         BNE   GETEST10                                                         
         CLC   EKEYEST,LW_DATA1    YES - TEST ESTIMATE FOUND                    
         JNE   NOMORE                                                           
         B     GETEST12                                                         
                                                                                
GETEST10 CLI   ESTIND,LW_TALLQ                                                  
         JE    GETEST12                                                         
         CLC   EKEYEST,LW_DATA1+L'EKEYEST                                       
         JH    NOMORE                                                           
                                                                                
GETEST12 GOTOR FILGETR,NBAEST      READ THE ESTIMATE RECORD                     
         L     R3,NBAEST                                                        
                                                                                
         LA    R0,ESTVALS          CLEAR ESTIMATE VALUES                        
         LHI   R1,ESTVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR DATCON,DMCB,(0,ESTART),(2,ESTSTDTB)                              
         GOTOR (RF),(R1),(0,EEND),(2,ESTNDDTB)                                  
         MVI   ESTLOCKD,SPACEQ                                                  
         TM    ECNTRL,X'08'        TEST ESTIMATE IS LOCKED                      
         BZ    *+8                                                              
         MVI   ESTLOCKD,YESQ                                                    
                                                                                
         CLI   ESTIND,LQ_TSINQ     TEST SINGLE ESTIMATE REQUEST                 
         BE    GETEST14                                                         
         CLC   ESTNDDTB,MOS.MOSCSTR                                             
         BL    GETEST02                                                         
         CLC   ESTSTDTB,MOS.MOSCEND                                             
         BH    GETEST02                                                         
                                                                                
GETEST14 GOTOR GETEDN              GET ESTIMATE DEMO NAMES                      
         SR    R1,R1                                                            
         IC    R1,ESTCOUNT         CREATE ENTRY IN ESTIMATE TABLE               
         LA    R0,1(R1)                                                         
         CHI   R0,ESTTABN          TEST MAXIMUM ENTRIES EXCEEDED                
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   R0,ESTCOUNT                                                      
         MHI   R1,ESTTABL                                                       
         AHI   R1,ESTTAB-SAVED                                                  
         LA    R1,SAVED(R1)                                                     
         USING ESTTAB,R1           R1=A(TABLE ENTRY)                            
         MVC   ESTTNUM,EKEYEST                                                  
         MVC   ESTTDEMO,EDEMLST                                                 
         TM    ECNTRL,X'02'        TEST FOR STEWARDSHIP ESTIMATE                
         BZ    *+8                                                              
         OI    ESTTSTAT,X'80'                                                   
         J     EXITY                                                            
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
***********************************************************************         
* GET UNITS ORDERED NOT RUN                                           *         
***********************************************************************         
                                                                                
GETUONR  MVC   LP_ADATA,NBAIO                                                   
         GOTOR NXTUNT,NUKEYT       GET NEXT UNIT RECORD                         
         JE    EXITY                                                            
         J     NOMORE                                                           
         EJECT                                                                  
***********************************************************************         
* GET UNITS AND INVOICES                                              *         
***********************************************************************         
                                                                                
GETUAI   J     *+12                                                             
         DC    CL8'*GETUAI*'                                                    
         LR    RB,RF                                                            
         USING GETUAI,RB                                                        
         LA    R0,INVVALS                                                       
         ST    R0,LP_ADATA                                                      
         CLI   LP_RMODE,LP_RFRST                                                
         BNE   GETUAI20                                                         
                                                                                
         MVI   TSACTN,TSAINI       INITIALIZE TSAR BUFFER                       
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,TKEYL                                                     
         MVI   TSPAGN,8                                                         
         OI    TSINDS,TSINODSK+TSIXTTWA                                         
         LHI   R0,TRECL                                                         
         STCM  R0,3,TSRECL                                                      
         LA    R0,TSARREC                                                       
         STCM  R0,15,TSAREC                                                     
         GOTOR BUFFER                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSAADD       SET TSAR ACTION TO 'ADD'                     
                                                                                
         OC    AMASTC,AMASTC       TEST OFFLINE                                 
         BZ    GETUAI02                                                         
         ICM   RF,15,LP_ABLKF                                                   
         USING MMSAVED,RF                                                       
         XC    MMIVALS(MMIVALSL),MMIVALS                                        
                                                                                
***********************************************************************         
* READ ALL UNIT RECORDS FOR THE PERIOD.  CREATE A BUFFER 'UNIT'       *         
* RECORD (TKTYPE=TKTUNTQ) FOR ALL UNITS THAT QUALIFY                  *         
***********************************************************************         
                                                                                
GETUAI02 GOTOR NXTUNT,NPKEYT       GET NEXT UNIT RECORD                         
         BNE   GETUAI04            ALL DONE - GET INVOICES                      
         L     R3,NBAIO                                                         
         USING NURECD,R3           R3=A(UNIT RECORD)                            
         XC    TRECD(TRECL),TRECD  BUILD TSAR UNIT RECORD                       
         MVI   TKTYPE,TKTUNTQ                                                   
         MVC   TKDATE,NUKDATE                                                   
         MVC   TKTIME,NUKTIME                                                   
         MVC   TKPROG,NUKPROG                                                   
         MVC   TKEST,NUKEST                                                     
         MVC   TKSUB,NUKSUB                                                     
         MVC   TKDP,NUKDP                                                       
         MVC   TDUNTDA,NBKEY+(NUDA-NURECD)                                      
         MVC   TDUNTINT,NBINTEG    SET INTEGRATION COST                         
         GOTOR BUFFER              ADD UNIT RECORD TO TSAR BUFFER               
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   PROFI2A+2,YESQ      EXPAND MIRROR UNITS?                         
         BNE   GETUAI02                                                         
                                                                                
         OC    UNTMIROR,UNTMIROR   TEST 1ST MIRRORED UNIT                       
         BZ    GETUAI02                                                         
         MVI   TKMIRROR,TKMIR1     YES - CREATE 1ST MIRROR UNIT RECORD          
         GOTOR BUFFER                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OC    UNTMIRO2,UNTMIRO2   TEST 2ND MIRRORED UNIT                       
         BZ    GETUAI02                                                         
         MVI   TKMIRROR,TKMIR2     YES - CREATE 2ND MIRROR UNIT RECORD          
         GOTOR BUFFER                                                           
         BE    GETUAI02                                                         
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* READ ALL THE INVOICE RECORDS FOR THE MONTH OF SERVICE. FOR ALL      *         
* INVOICES THAT QUALIFY (IE HAVE SOME DETAILS ATTACHED) SEND AN       *         
* INVOICE HEADER RECORD TO THE PC.  FOR EACH MATCHED DETAIL ELEMENT   *         
* FIND THE ASSOCIATED UNIT RECORD IN THE BUFFER AND UPDATE THE        *         
* 'UNIT' RECORD WITH THE MATCHING INVOICE DETAILS.  FOR EACH          *         
* UNMATCHED INVOICE DETAIL ELEMENT CREATE A BUFFER 'INVOICE' RECORD   *         
* (TKTYPE=TKTRNOQ). NOTE THAT MATCHED INVOICES FOR WHICH WE CANNOT    *         
* LOCATE THE MATCHING UNIT RECORD ARE TREATED AS UNMATCHED.           *         
* THE CODE WILL BE RE-ENTERED AT GETUAI20 TO GET THE NEXT INVOICE     *         
* AFTER EXITING TO DDLINK TO SEND THE PREVIOUS INVOICE HEADER         *         
***********************************************************************         
                                                                                
GETUAI04 LA    R3,KEY              BUILD INITIAL INVOICE KEY                    
         USING SNVKEYD,R3                                                       
         XC    SNVKEY,SNVKEY                                                    
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,NBACTAM                                                   
         MVC   SNVKCLT,NBACTCLI                                                 
         MVC   SNVKSTA,STA                                                      
         MVC   SNVKMOS,MOS.MOSC                                                 
         MVC   SNVKINV,INV#                                                     
                                                                                
         GOTOR DIRHIGH,NBFILXSP    START WITH A READ HIGH                       
         B     GETUAI12                                                         
                                                                                
GETUAI06 LA    R3,KEY              AND FOLLOW WITH READ SEQUENTIALS             
         GOTOR DIRRSEQ,NBFILXSP                                                 
         B     GETUAI12                                                         
                                                                                
GETUAI08 OC    INV#,INV#           COME HERE TO SKIP TO NEXT INVOICE            
         JNZ   NOMORE                                                           
         LA    R3,KEY                                                           
         CLC   SNVKMINK,EFFS                                                    
         BE    GETUAI10                                                         
         MVC   SNVKMINK,EFFS                                                    
         GOTOR DIRHIGH,NBFILXSP                                                 
                                                                                
GETUAI10 GOTOR DIRRSEQ,NBFILXSP                                                 
                                                                                
GETUAI12 CLC   SNVKEY(SNVKINV-SNVKEY),KEYSAVE                                   
         BE    GETUAI14                                                         
         OI    INVINDS,INVIEOFR    SET END OF FILE REACHED                      
         TM    INVINDS,INVIWANT    TEST INVOICE SEND PENDING                    
         JZ    NOMORE                                                           
         NI    INVINDS,FF-(INVIWANT)                                            
         J     EXITY               EXIT TO SEND LAST INVOICE                    
                                                                                
GETUAI14 CLC   SNVKINV,LASTINV     TEST CHANGE OF INVOICE                       
         BE    GETUAI20                                                         
         CLC   SNVKMINK,EFFS       TEST WE HAVE LAST RECORD IN SET              
         BNE   GETUAI16                                                         
         GOTOR FLTINV              YES - APPLY INVOICE FILTERS NOW              
         BNE   GETUAI08                                                         
         B     GETUAI18                                                         
                                                                                
GETUAI16 MVC   SAVEKEY,SNVKEY      READ LAST RECORD IN SET                      
         MVC   SNVKMINK,EFFS                                                    
         GOTOR DIRREAD,NBFILXSP                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR FLTINV              APPLY INVOICE FILTERS                        
         BNE   GETUAI08                                                         
         MVC   SNVKEY,SAVEKEY      RE-READ FIRST RECORD IN SET                  
         GOTOR DIRREAD,NBFILXSP                                                 
         BE    GETUAI18                                                         
         DC    H'0'                                                             
                                                                                
GETUAI18 OC    LASTINV,LASTINV     TEST FIRST INVOICE                           
         MVC   LASTINV,SNVKINV     (SET CURRENT INVOICE NUMBER)                 
         BZ    GETUAI20                                                         
         TM    INVINDS,INVIWANT    TEST LAST INVOICE WAS WANTED                 
         BZ    GETUAI20                                                         
         NI    INVINDS,FF-(INVIWANT)                                            
         J     EXITY               EXIT TO SEND LAST INVOICE                    
                                                                                
GETUAI20 TM    INVINDS,INVIEOFR    TEST END OF FILE REACHED                     
         JNZ   NOMORE                                                           
         GOTOR FILGETR,NBAIO       READ INVOICE INTO LARGE I/O AREA             
         L     R3,NBAIO            R3=A(INVOICE RECORD)                         
         MVC   INVDMDA,DMDA        SAVE OFF INVOICE RECORD D/A                  
                                                                                
         MVI   INVFLAG,0                                                        
         LA    R2,SNVELS           R2=A(FIRST ELEMENT ON RECORD)                
                                                                                
         USING SNVMMELD,R2                                                      
GETUAI22 CLI   0(R2),0             TEST END OF RECORD                           
         BE    GETUAI28                                                         
GETUAI24 CLI   SNVMMEL,SNVMMELQ    TEST CORRECT ELEMENT                         
         BNE   GETUAI26                                                         
         CLI   SNVMMGN,C'N'        TEST NET                                     
         BNE   GETUAI28                                                         
         OI    INVFLAG,INVFLNET                                                 
         B     GETUAI28                                                         
                                                                                
GETUAI26 SR    R0,R0               BUMP TO NEXT ELEMENT ON INVOICE              
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GETUAI22                                                         
                                                                                
GETUAI28 LA    R2,SNVELS           R2=A(FIRST ELEMENT ON RECORD)                
GETUAI30 CLI   0(R2),0             TEST END OF RECORD                           
         BE    GETUAI06                                                         
                                                                                
***********************************************************************         
* PROCESS AN INVOICE HEADER ELEMENT                                   *         
***********************************************************************         
                                                                                
         USING SNVHDELD,R2                                                      
         CLI   SNVHDEL,SNVHDELQ    TEST CORRECT ELEMENT                         
         BNE   GETUAI40                                                         
         TM    SNVHDCTL,SNVHDMCQ   TEST MID-FLIGHT CLEARANCE                    
         BNZ   GETUAI08            YES - IGNORE THIS INVOICE                    
                                                                                
         MVC   INVREP,SNVHDREP                                                  
         XC    PLKUP(PLKUPL),PLKUP                                              
         MVC   PLKPRD,SNVHDPRD                                                  
         MVC   PLKPIG,SNVHDPR2                                                  
         CLI   SNVHDLEN,SNVHDLN2                                                
         JNH   GETUAI32                                                         
         MVC   PLKPRDA,SNVHDAP1                                                 
         MVC   PLKPIGA,SNVHDAP2                                                 
         J     GETUAI34                                                         
                                                                                
GETUAI32 CLI   PLKPRD,0                                                         
         JE    GETUAI34                                                         
         GOTOR GETPRC,PLKPRD                                                    
         JNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   PLKPRDA,PLPMNEM                                                  
         CLI   PLKPIG,0                                                         
         JE    GETUAI34                                                         
         GOTOR GETPRC,PLKPIG                                                    
         JNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   PLKPIGA,PLPMNEM                                                  
                                                                                
GETUAI34 GOTOR TSTPRI              APPLY PRODUCT FILTERS                        
         BNE   GETUAI74                                                         
         L     RF,NBAIO                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SNVKEY),0(RF)                                              
         GOTOR DIRHIGH,NBFILXSP    START WITH A READ HIGH                       
         MVC   DMDA,INVDMDA                                                     
                                                                                
         GOTOR TSTEST,SNVHDEST     APPLY ESTIMATE FILTERS                       
         BNE   GETUAI08                                                         
         OC    APKN,APKN           APPLY PACKAGE FILTER                         
         BZ    GETUAI36                                                         
         GOTOR LP_ASETK,DMCB,(1,PKNFLT),SNVHDPKG,SAVED,('FF',LP_D)              
         BNE   GETUAI08                                                         
                                                                                
GETUAI36 XC    INVVALS(INVVALSL),INVVALS                                        
         MVC   INVDA,DMDA                                                       
         MVC   INVCTL,SNVHDCTL     EXTRACT INVOICE HEADER DETAILS               
         MVC   INVEST,SNVHDEST                                                  
                                                                                
         MVC   INVPRD,SNVHDPRD                                                  
         MVC   INVPIG,SNVHDPR2                                                  
                                                                                
         XC    INVPRDA,INVPRDA                                                  
         XC    INVPIGA,INVPIGA                                                  
         CLI   SNVHDLEN,SNVHDLN2   HAS ALPHA PRODUCTS?                          
         BNH   GETUAI38                                                         
         MVC   INVPRDA,SNVHDAP1                                                 
         MVC   INVPIGA,SNVHDAP2                                                 
         MVI   INVPRD,0                                                         
         MVI   INVPIG,0                                                         
                                                                                
GETUAI38 MVC   INVINVN,SNVKINV                                                  
         OC    SNVHDEZS,SNVHDEZS   TEST EASI SOURCE                             
         BZ    *+14                                                             
         MVI   INVEASI,YESQ        YES - SET FLAG                               
         MVC   INVESRC,SNVHDEZS    EASI SOURCE                                  
         MVC   INVCNUM,SNVHDCON                                                 
         MVC   INVSDT,SNVHDSDT                                                  
         MVC   INVPKGNO,SNVHDPKG                                                
         GOTOR DATCON,DMCB,(2,INVSDT),(0,INVESDT)                               
                                                                                
         LA    R0,SAVED            CLEAR FILM SAVE AREA                         
         AHI   R0,FLMSAVE-SAVED                                                 
         LHI   R1,FLMSAVEL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     GETUAI72                                                         
                                                                                
***********************************************************************         
* PROCESS A COMMERCIAL CODE TRANSLATION ELEMENT                       *         
***********************************************************************         
                                                                                
         USING SNVCMELD,R2                                                      
GETUAI40 CLI   SNVCMEL,SNVCMELQ    TEST CORRECT ELEMENT                         
         BNE   GETUAI42                                                         
         SR    RF,RF                                                            
         IC    RF,SNVCMICD                                                      
         CLM   RF,3,INVFLMS                                                     
         BNH   *+8                                                              
         STCM  RF,3,INVFLMS        SAVE HIGHEST FILM SEQUENCE NUMBER            
         BCTR  RF,0                                                             
         MHI   RF,FLMNTRYL         RF=INDEX INTO FILM TABLE                     
         AHI   RF,FLMSAVE-SAVED    RF=DISPLACEMENT INTO SAVED                   
         LA    RF,SAVED(RF)        RF=A(FILM ENTRY)                             
         USING FLMNTRY,RF          BUILD A FILM TABLE ENTRY                     
         MVC   FLMCODE,SNVCMAID                                                 
         OC    FLMCODE,BLANKS                                                   
         MVI   FLMINV,SPACEQ                                                    
         MVC   WORK(L'SNVKMOS),SNVKMOS                                          
         XC    WORK(L'SNVKMOS),EFFS                                             
         CLC   WORK(L'SNVKMOS),MOSTI                                            
         BL    GETUAI72                                                         
                                                                                
         OC    SNVCMSEQ,SNVCMSEQ   TEST WE HAVE TRAFFIC SEQUENCE#               
         BNZ   *+8                                                              
         MVI   FLMINV,YESQ         NO - SET FILM IS INVALID                     
         MVC   FLMSEQN,SNVCMSEQ    SET TRAFFIC SEQUENCE NUMBER                  
         B     GETUAI72                                                         
         DROP  RF                                                               
                                                                                
***********************************************************************         
* PROCESS AN INVOICE DETAIL ELEMENT                                   *         
***********************************************************************         
                                                                                
         USING SNVIDELD,R2                                                      
GETUAI42 CLI   SNVIDEL,SNVIDELQ    TEST CORRECT ELEMENT                         
         BNE   GETUAI64                                                         
                                                                                
         XC    PLKUP(PLKUPL),PLKUP                                              
         MVC   PLKPRD,SNVIDPRD                                                  
         MVC   PLKPIG,SNVIDPR2                                                  
         CLI   SNVIDLEN,SNVIDL2Q                                                
         JNH   GETUAI44                                                         
         MVC   PLKPRDA,SNVIDAP1                                                 
         MVC   PLKPIGA,SNVIDAP2                                                 
         J     GETUAI46                                                         
                                                                                
GETUAI44 CLI   PLKPRD,0                                                         
         JE    GETUAI46                                                         
         GOTOR GETPRC,PLKPRD                                                    
         JNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   PLKPRDA,PLPMNEM                                                  
         CLI   PLKPIG,0                                                         
         JE    GETUAI46                                                         
         GOTOR GETPRC,PLKPIG                                                    
         JNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   PLKPIGA,PLPMNEM                                                  
                                                                                
GETUAI46 GOTOR TSTPRI              APPLY PRODUCT FILTERS                        
         BNE   GETUAI72                                                         
         L     RF,NBAIO                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SNVKEY),0(RF)                                              
         GOTOR DIRHIGH,NBFILXSP    START WITH A READ HIGH                       
         MVC   DMDA,INVDMDA                                                     
                                                                                
         GOTOR TSTEST,SNVIDEST     APPLY ESTIMATE FILTERS                       
         BNE   GETUAI72                                                         
         MVC   WORK(L'INVESDT),INVESDT                                          
         SR    R0,R0               TEST INVOICE DETAIL FALLS IN PERIOD          
         ICM   R0,1,SNVIDDAY                                                    
         GOTOR ADDAY,DMCB,INVESDT,WORK,(R0)                                     
         ORG   *-2                                                              
         LTR   R0,R0               TEST RELATIVE DAY IS ZERO                    
         BZ    *+6                 YES - DON'T DO ADDAY CALL                    
         BASR  RE,RF               ELSE CALCULATE ACTUAL DATE                   
         CLC   MOS.MOSESTR,WORK                                                 
         BH    GETUAI72                                                         
         CLC   MOS.MOSEEND,WORK                                                 
         BL    GETUAI72                                                         
                                                                                
         TM    INVINDS,INVIWANT    TEST FIRST TIME FOR THIS INVOICE             
         BNZ   GETUAI48                                                         
         SR    R0,R0                                                            
         ICM   R0,3,LASTISEQ       YES - BUMP INVOICE SEQUENCE NUMBER           
         AHI   R0,1                                                             
         STCM  R0,3,LASTISEQ                                                    
                                                                                
GETUAI48 CLI   SNVIDLEN,SNVIDL2Q   TEST NEW STYLE DETAIL ELEMENT                
         BL    GETUAI60                                                         
         CLI   SNVIDUES,0          TEST INVOICE IS MATCHED                      
         BE    GETUAI60                                                         
         XC    TKEY(TKEYL),TKEY    YES - LOCATE MATCHED UNIT                    
         MVI   TKTYPE,TKTUNTQ                                                   
         MVC   TKEST,SNVIDUES                                                   
         MVC   TKDATE,SNVIDUDT                                                  
         MVC   TKTIME,SNVIDUTM                                                  
         MVC   TKPROG,SNVIDUPG                                                  
         MVC   TKDP,SNVIDUDP                                                    
         MVC   TKSUB,SNVIDUSB                                                   
                                                                                
         TM    SNVIDFLG,SNVIDMIR   TEST MIRROR INVOICE                          
         BO    GETUAI50                                                         
         MVI   TSACTN,TSARDH                                                    
         GOTOR BUFFER                                                           
         BE    GETUAI54                                                         
         B     GETUAI60            NOT FOUND - TREAT AS UNMATCHED               
                                                                                
GETUAI50 MVI   TKMIRROR,TKMIR1     YES - SET TO READ 1ST MIRROR UNIT            
         MVI   TSACTN,TSARDH                                                    
         GOTOR BUFFER                                                           
         BNE   GETUAI60                                                         
         OC    TDINVDA,TDINVDA                                                  
         BZ    GETUAI54                                                         
                                                                                
GETUAI52 XC    TKEY(TKEYL),TKEY                                                 
         MVI   TKTYPE,TKTUNTQ                                                   
         MVC   TKEST,SNVIDUES                                                   
         MVC   TKDATE,SNVIDUDT                                                  
         MVC   TKTIME,SNVIDUTM                                                  
         MVC   TKPROG,SNVIDUPG                                                  
         MVC   TKDP,SNVIDUDP                                                    
         MVC   TKSUB,SNVIDUSB                                                   
         MVI   TKMIRROR,TKMIR2     YES - SET TO READ 2ND MIRROR UNIT            
         MVI   TSACTN,TSARDH                                                    
         GOTOR BUFFER                                                           
         BNE   GETUAI60            NOT FOUND - TREAT AS UNMATCHED               
                                                                                
GETUAI54 CLI   INTOPT,INTOTIMQ     TEST TIME ONLY                               
         BE    GETUAI58                                                         
         TM    INVFLAG,INVFLNET    TEST NET                                     
         BZ    GETUAI56                                                         
                                                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,TDUNTINT      CALCULATE NET INTEGRATION                    
         MHI   RF,85                                                            
         AHI   RF,5                                                             
         D     RE,=F'100'                                                       
         STCM  RF,15,TDUNTINT                                                   
                                                                                
GETUAI56 CLC   TDUNTINT,SNVIDINT   NO - INTEGRATION COST MUST MATCH             
         BNE   GETUAI60                                                         
                                                                                
GETUAI58 OC    TDINVDA,TDINVDA     TEST UNIT IS ALREADY MATCHED                 
         BZ    *+16                                                             
         GOTOR LOGERR,DUPERR       YES - PRINT ERROR LOG AND TREAT AS           
         B     GETUAI60            AN RNO                                       
                                                                                
         GOTOR SETINV,SNVIDELD     SEED TSAR RECORD WITH INVOICE VALUES         
         MVI   TSACTN,TSAWRT       WRITE BACK TSAR UNIT RECORD                  
         GOTOR BUFFER                                                           
         BE    GETUAI62                                                         
         DC    H'0'                                                             
                                                                                
GETUAI60 XC    TRECD(TRECL),TRECD  CREATE AN UNMATCHED DETAIL RECORD            
         MVI   TKTYPE,TKTRNOQ                                                   
         MVC   TKINVSEQ,LASTISEQ   INVOICE SEQUENCE NUMBER                      
         SR    R0,R0                                                            
         ICM   R0,3,LASTDSEQ                                                    
         AHI   R0,1                                                             
         STCM  R0,3,LASTDSEQ                                                    
         MVC   TKDETSEQ,LASTDSEQ   DETAIL SEQUENCE NUMBER                       
         GOTOR SETINV,SNVIDELD     SEED TSAR RECORD WITH INVOICE VALUES         
         MVI   TSACTN,TSAADD                                                    
         GOTOR BUFFER              ADD UNMATCHED INVOICE DETAIL RECORD          
         BE    GETUAI62                                                         
         DC    H'0'                                                             
                                                                                
GETUAI62 OI    INVINDS,INVIWANT    SET THAT WE WANT THIS INVOICE SENT           
         OC    AMASTC,AMASTC       TEST OFFLINE                                 
         BZ    GETUAI72                                                         
         ICM   RF,15,LP_ABLKF                                                   
         USING MMSAVED,RF          POINT TO SAVE AREA                           
         OC    MMIKEY,MMIKEY       TEST INVOICE KEY ALREADY SAVED               
         BNZ   GETUAI72                                                         
         MVC   MMIKEY,SNVKEY       NO - SAVE THIS ONE                           
         B     GETUAI72                                                         
         DROP  RF                                                               
                                                                                
***********************************************************************         
* PROCESS A MATCHMAKER STATUS ELEMENT                                 *         
***********************************************************************         
                                                                                
         USING SNVMMELD,R2                                                      
GETUAI64 CLI   SNVMMEL,SNVMMELQ    TEST CORRECT ELEMENT                         
         BNE   GETUAI70                                                         
         TM    INVINDS,INVIWANT    TEST WE WANT THIS INVOICE                    
         BZ    GETUAI72                                                         
                                                                                
         MVC   INVI2DAT,SNVMMDAT   EXTRACT I2 RUN DATE                          
         MVC   INVI2TIM,SNVMMTIM   .......... RUN TIME                          
                                                                                
         OC    AMASTC,AMASTC       TEST OFFLINE                                 
         BZ    GETUAI66                                                         
         ICM   RF,15,LP_ABLKF                                                   
         USING MMSAVED,RF                                                       
         OC    MMI2DATE,MMI2DATE   TEST FIRST WANTED INVOICE                    
         BNZ   GETUAI66                                                         
         MVC   MMIKEY,SNVKEY       YES - SAVE KEY & I2 DATE AND TIME            
         MVC   MMI2DATE,SNVMMDAT                                                
         MVC   MMI2TIME,SNVMMTIM                                                
         DROP  RF                                                               
                                                                                
GETUAI66 MVC   INVI2RBK,BLANKS     SET BOOK                                     
         MVC   INVI2RBK(L'SNVMMBK),SNVMMBK                                      
         CLC   BOOKLAT,SNVMMBK     TEST 'LAT'                                   
         BE    GETUAI72                                                         
         CLC   BOOKACT,SNVMMBK     TEST 'ACT'                                   
         BNE   GETUAI68                                                         
         CLI   SNVMMBK+3,C'-'      TEST 'ACT-'                                  
         BNE   GETUAI72                                                         
         MVC   INVI2RBK+L'SNVMMBK(L'BOOKNO),BOOKNO                              
         B     GETUAI72                                                         
                                                                                
GETUAI68 TM    SNVMMBK,X'F0'       TEST BOOK DATE PRESENT                       
         BNO   GETUAI72                                                         
         MVC   WORK(L'SNVMMBK),SNVMMBK                                          
         MVC   WORK+L'SNVMMBK(L'EONE),EONE                                      
         GOTOR DATCON,DMCB,(0,WORK),(6,INVI2RBK)                                
         B     GETUAI72                                                         
                                                                                
***********************************************************************         
* PROCESS I2 REQUEST DETAILS ELEMENT                                  *         
***********************************************************************         
                                                                                
         USING SNVMTELD,R2                                                      
GETUAI70 CLI   SNVMTEL,SNVMTELQ    TEST CORRECT ELEMENT                         
         BNE   GETUAI72                                                         
         MVC   INVI2INT,SNVMTINT   SET I2 REQUEST INTEGRATION OPTION            
                                                                                
GETUAI72 SR    R0,R0               BUMP TO NEXT ELEMENT ON INVOICE              
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GETUAI30                                                         
                                                                                
GETUAI74 L     RF,NBAIO                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SNVKEY),0(RF)                                              
         GOTOR DIRHIGH,NBFILXSP    START WITH A READ HIGH                       
         MVC   DMDA,INVDMDA                                                     
         B     GETUAI08                                                         
         DROP  R2,R3,RB                                                         
         EJECT                                                                  
***********************************************************************         
* GET MATCHED UNITS AND INVOICES                                      *         
***********************************************************************         
                                                                                
GETMCH   LA    R0,INVVALS                                                       
         ST    R0,LP_ADATA                                                      
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   GETMCH02                                                         
         LHI   RF,FLMDATES-SAVED   CLEAR FILM DATE TABLE                        
         LA    RF,SAVED(RF)                                                     
         XC    0(FLMDATEL,RF),0(RF)                                             
         XC    TKEY(TKEYL),TKEY    YES - INITIALIZE KEY                         
         MVI   TKTYPE,TKTUNTQ                                                   
         MVI   TSACTN,TSARDH       AND START WITH A READ HIGH                   
         J     GETMCH04                                                         
                                                                                
GETMCH02 MVI   TSACTN,TSANXT       FOLLOW WITH READ SEQUENTIALS                 
                                                                                
GETMCH04 GOTOR BUFFER              GET FIRST/NEXT TSAR RECORD                   
         TM    TSERRS,TSEEOF       TEST END OF FILE REACHED                     
         JNZ   NOMORE                                                           
         CLI   TKTYPE,TKTUNTQ      TEST WE HAVE A UNIT RECORD                   
         JNE   NOMORE                                                           
         OC    TDINVDA,TDINVDA     TEST MATCHED UNIT/INVOICE PAIR               
         JZ    GETMCH02                                                         
         GOTOR GETINV              READ AND FORMAT INVOICE DETAILS              
         JNE   EXITY                                                            
         GOTOR GETUNT,0            READ AND FORMAT UNIT RECORD                  
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET UNMATCHED UNIT RECORDS (ORDERED NOT RUN)                        *         
***********************************************************************         
                                                                                
GETONR   MVC   LP_ADATA,NBAIO                                                   
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   GETONR02                                                         
         XC    TKEY(TKEYL),TKEY    YES - INITIALIZE KEY                         
         MVI   TKTYPE,TKTUNTQ                                                   
         MVI   TSACTN,TSARDH       AND START WITH A READ HIGH                   
         J     GETONR04                                                         
                                                                                
GETONR02 MVI   TSACTN,TSANXT       FOLLOW WITH READ SEQUENTIALS                 
                                                                                
GETONR04 GOTOR BUFFER              GET FIRST/NEXT TSAR RECORD                   
         TM    TSERRS,TSEEOF       TEST END OF FILE REACHED                     
         JNZ   NOMORE                                                           
         CLI   TKTYPE,TKTUNTQ      TEST WE HAVE A UNIT RECORD                   
         JNE   NOMORE                                                           
         CLI   TKMIRROR,TKMNO      IGNORE MIRRORS                               
         JNE   GETONR02                                                         
         OC    TDINVDA,TDINVDA     TEST UNIT IS MATCHED                         
         JNZ   GETONR02                                                         
                                                                                
         GOTOR GETUNT,0            GET UNIT RECORD                              
         L     R3,NBAIO                                                         
         USING NURECD,R3           R3=A(UNIT RECORD)                            
                                                                                
         OC    NUAFFTIM,NUAFFTIM   TEST AFFID. SEEDED TO UNIT                   
         JZ    EXITY               NO - KEEP THIS ONE                           
         CLI   INTOPT,INTOYESQ     TEST TIME+INTEGRATION                        
         JE    GETONR02            YES - DON'T WANT THIS ONE                    
                                                                                
         LA    R1,NUDATA                                                        
         SR    R0,R0                                                            
         USING NUDTAD,R1                                                        
GETONR06 CLI   NUDTAEL,0           TEST END OF RECORD                           
         JE    GETONR02            YES - DON'T WANT THIS ONE                    
         CLI   NUDTAEL,NUDTAELQ    TEST ADDITIONAL DATA ELEMENT                 
         JE    *+14                                                             
         IC    R0,NUDTALEN                                                      
         AR    R1,R0                                                            
         J     GETONR06                                                         
                                                                                
         CLI   INTOPT,INTOTIMQ     TEST TIME ONLY                               
         JNE   GETONR08            NO - DON'T WANT THIS ONE                     
         CLC   NUDTINVN,BLANKS     TEST TIME MATCHED                            
         JH    GETONR02            YES - DON'T WANT THIS ONE                    
         J     EXITY                                                            
                                                                                
GETONR08 CLI   INTOPT,INTOINTQ     TEST INTEGRATION ONLY                        
         JNE   GETONR02                                                         
         CLC   NUDTIIVN,BLANKS     TEST INTEGRATION MATCHED                     
         JH    GETONR02            YES - DON'T WANT THIS ONE                    
         J     EXITY                                                            
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET UNMATCHED INVOICES (RUN NOT ORDERED)                            *         
***********************************************************************         
                                                                                
GETRNO   LA    R0,INVVALS                                                       
         ST    R0,LP_ADATA                                                      
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   GETRNO02                                                         
         XC    TKEY(TKEYL),TKEY    YES - INITIALIZE KEY                         
         MVI   TKTYPE,TKTRNOQ                                                   
         MVI   TSACTN,TSARDH       AND START WITH A READ HIGH                   
         J     GETRNO04                                                         
                                                                                
GETRNO02 MVI   TSACTN,TSANXT       FOLLOW WITH READ SEQUENTIALS                 
                                                                                
GETRNO04 GOTOR BUFFER              GET FIRST/NEXT TSAR RECORD                   
         TM    TSERRS,TSEEOF       TEST END OF FILE REACHED                     
         JNZ   NOMORE                                                           
         CLI   TKTYPE,TKTRNOQ      TEST WE HAVE AN INVOICE RECORD               
         JNE   NOMORE                                                           
         GOTOR GETINV              NO - READ & FORMAT INVOICE                   
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATE OPTION                                                *         
***********************************************************************         
                                                                                
VALDOP   LM    R2,R4,LP_AINP                                                    
         MVC   0(1,R4),0(R2)                                                    
         MVC   DATOPT,0(R2)                                                     
         J     EXITY                                                            
                                                                                
***********************************************************************         
* VALIDATE DAYS                                                       *         
***********************************************************************         
                                                                                
VALDAY   LM    R2,R4,LP_AINP                                                    
         GOTOR DAYVAL,DMCB,((R3),(R2)),(R4),DUB                                 
         CLI   0(R1),0                                                          
         JNE   EXITY                                                            
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE TIMES                                                      *         
***********************************************************************         
                                                                                
VALTIM   LM    R2,R4,LP_AINP                                                    
         GOTOR TIMVAL,DMCB,((R3),(R2)),DUB                                      
         CLI   0(R1),FF                                                         
         JE    EXITN                                                            
         SR    R1,R1                                                            
         ICM   R1,3,DUB                                                         
         GOTOR MILTQH              CONVERT START TIME TO QUARTER HOUR           
         STC   R1,0(R4)                                                         
         ICM   R1,3,DUB+2                                                       
         GOTOR MILTQH              CONVERT END TIME TO QUARTER HOUR             
         STC   R1,1(R4)                                                         
         J     EXITY                                                            
         DROP  MOS                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE MONTH OF SERVICE                                           *         
***********************************************************************         
                                                                                
VALMOS   J     *+12                                                             
         DC    CL8'*VALMOS*'                                                    
         LR    RB,RF                                                            
         USING VALMOS,RB                                                        
         LM    R2,R4,LP_AINP                                                    
         USING MOSD,R4                                                          
         CLI   DATOPT,DATOCALQ     TEST USING CALENDAR MONTHS                   
         BE    VALMOS02                                                         
         GOTOR DATVAL,DMCB,(2,(R2)),WORK                                        
         CLI   3(R1),0                                                          
         JE    EXITN                                                            
         GOTOR DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         MVC   MOSB,WORK+6         MOSB=YYMM BINARY                             
         GOTOR GETBRD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                         
         CLI   0(R1),FF                                                         
         JE    EXITN                                                            
         GOTOR DATCON,DMCB,(0,WORK+6),(2,MOSCSTR)                               
         GOTOR (RF),(R1),(0,WORK+12),(2,MOSCEND)                                
         GOTOR ADDAY,(R1),WORK+6,WORK+12,7                                      
         MVC   WORK+16(L'EONE),EONE                                             
         GOTOR DATCON,DMCB,(0,WORK+12),(2,MOSC)                                 
         XC    MOSC,EFFS           MOSC=YYMMDD COMPRESSED & INVERTED            
         J     VALMOS04                                                         
                                                                                
         USING PERVALD,WORK                                                     
VALMOS02 GOTOR PERVAL,DMCB,((R3),(R2)),PERVALD                                  
         MVC   MOSB,PVALBSTA       MOSB=YYMM BINARY                             
         MVC   MOSCSTR(L'MOSCSTR+L'MOSCEND),PVALCSTA                            
         MVC   MOSC,PVALCSTA                                                    
         XC    MOSC,EFFS           MOSC=YYMMDD COMPRESSED & INVERTED            
                                                                                
VALMOS04 GOTOR DATCON,DMCB,(2,MOSCSTR),(0,MOSESTR)                              
         GOTOR (RF),(R1),(2,MOSCEND),(0,MOSEEND)                                
         J     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE NETWORK - CONVERT EBCDIC NETWORK INTO 3 BYTE PACKED (STA)  *         
***********************************************************************         
                                                                                
VALNWK   J     *+12                                                             
         DC    CL8'*VALNWK*'                                                    
         LR    RB,RF                                                            
         USING VALNWK,RB                                                        
         LM    R2,R4,LP_AINP                                                    
         XC    WORK,WORK                                                        
         USING STAPACKD,WORK                                                    
         MVI   STAPACT,C'P'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,NBSELAGY                                                 
         MVI   STAPMED,NETMEDQ                                                  
         L     RE,NBAAGY                                                        
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RE)                                     
         MVC   STAPQMKT,EZEROES                                                 
         MVC   STAPQSTA,BLANKS                                                  
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   STAPQSTA(0),0(R2)                                                
         GOTOR STAPACK,STAPACKD                                                 
         CLI   STAPERR,0                                                        
         JNE   EXITN                                                            
         MVC   0(L'NUKNET,R4),BLANKS                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
         MVC   L'NUKNET(L'STAPSTA,R4),STAPSTA                                   
         J     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT CODE - CONVERT EBCDIC CLIENT INTO 2 BYTE INTERNAL   *         
***********************************************************************         
                                                                                
VALCLT   J     *+12                                                             
         DC    CL8'*VALCLT*'                                                    
         LR    RB,RF                                                            
         USING VALCLT,RB                                                        
         LM    R2,R4,LP_AINP                                                    
         MVC   WORK(3),0(R2)                                                    
         CHI   R3,2                                                             
         JL    EXITN                                                            
         BH    *+8                                                              
         MVI   WORK+2,SPACEQ                                                    
         GOTOR NBCLPACK,DMCB,WORK,(R4)                                          
         CLI   0(R1),FF                                                         
         JE    EXITN                                                            
         MVC   NBSELCL2,0(R4)      SET CLIENT CODE                              
                                                                                
         LA    R3,KEY                                                           
         USING CLTHDR,R3           BUILD CLIENT KEY                             
         XC    CKEY,CKEY                                                        
         MVC   CKEYAM,NBACTAM                                                   
         MVC   CKEYCLT,NBSELCL2                                                 
         GOTOR DIRREAD,NBFILSPT                                                 
         JNE   EXITN                                                            
         GOTOR FILGETR,NBACLI      GET CLIENT RECORD                            
         J     EXITY                                                            
         DROP  RB,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE A PRODUCT CODE - LOOK UP PRODUCT IN CLIENT RECORD          *         
***********************************************************************         
                                                                                
VALPRD   J     *+12                                                             
         DC    CL8'*VALPRD*'                                                    
         LR    RB,RF                                                            
         USING VALPRD,RB                                                        
         LM    R2,R4,LP_AINP                                                    
         OC    NBSELCL2,NBSELCL2                                                
         JZ    EXITN                                                            
                                                                                
         LA    RF,KEY                                                           
         USING PRDHDR,RF                                                        
         XC    PKEY,PKEY                                                        
         MVC   PKEYAM,NBACTAM                                                   
         L     RE,NBACLI                                                        
         MVC   PKEYCLT,2(RE)                                                    
         MVC   PKEYPRD,0(R2)                                                    
         CHI   R3,2                ONE CHARACTER PRODUCT CODE IS BAD            
         JL    EXITN                                                            
         JH    *+8                                                              
         MVI   PKEYPRD+2,SPACEQ    PAD 2 CHARACTER PRODUCT WITH A SPACE         
                                                                                
         GOTOR DIRREAD,NBFILSPT                                                 
         JNE   EXITN                                                            
         LA    RF,KEY                                                           
         MVC   0(L'NBSELPRD,R4),PKEYPRD                                         
         J     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DAYPART CODE                                               *         
***********************************************************************         
                                                                                
VALDPT   J     *+12                                                             
         DC    CL8'*VALDPT*'                                                    
         LR    RB,RF                                                            
         USING VALDPT,RB                                                        
         LM    R2,R4,LP_AINP                                                    
         TM    INDS,INDS2CDP       TEST 2 CHARACTER DAYPARTS IN USE             
         BNZ   VALDPT02            YES                                          
         CHI   R3,1                TEST ONE CHARACTER INPUT                     
         JNE   EXITN                                                            
         MVC   0(L'NBSELDP,R4),0(R2)                                            
         J     EXITY                                                            
                                                                                
VALDPT02 MVC   WORK(L'NDPTDPTA),0(R2)                                           
         CHI   R3,L'NDPTDPTA                                                    
         BE    *+8                                                              
         MVI   WORK+1,SPACEQ                                                    
                                                                                
         XC    NDPTKEY,NDPTKEY                                                  
         MVI   NDPTKTY,NDPTKTYQ                                                 
         MVI   NDPTKST,NDPTKSTQ                                                 
         MVC   NDPTAGM,NBACTAM                                                  
         MVC   KEYSAVE,NDPTKEY                                                  
         GOTOR DIRHIGH,NBFILUNT                                                 
                                                                                
VALDPT04 CLC   NDPTKEY(NDPTDPTE-NDPTKEY),KEYSAVE                                
         BNE   VALDPT06                                                         
         CLC   NDPTDPTA,WORK       MATCH INPUT TO RECORD                        
         BNE   *+14                                                             
         MVC   0(L'NBSELDP,R4),NDPTDPTE                                         
         J     EXITY                                                            
         GOTOR DIRRSEQ,NBFILUNT                                                 
         B     VALDPT04                                                         
                                                                                
VALDPT06 MVC   NDPTKEY,KEYSAVE                                                  
         OC    NDPTCLT,NDPTCLT                                                  
         JNZ   EXITN                                                            
         OC    NBSELCL2,NBSELCL2                                                
         JZ    EXITN                                                            
         CLC   NBSELCL2,EFFS                                                    
         JE    EXITN                                                            
         MVC   NDPTCLT,NBSELCL2                                                 
         MVC   KEYSAVE,NDPTKEY                                                  
         GOTOR DIRHIGH,NBFILUNT                                                 
         B     VALDPT04                                                         
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A PRODUCT GROUP CODE                                       *         
***********************************************************************         
                                                                                
VALPGP   J     *+12                                                             
         DC    CL8'*VALPGP*'                                                    
         LR    RB,RF                                                            
         USING VALPGP,RB                                                        
         LM    R2,R4,LP_AINP                                                    
         OC    NBSELCL2,NBSELCL2   CLIENT MUST BE SPECIFIED                     
         JZ    EXITN                                                            
         SHI   R3,1                                                             
         JZ    EXITN                                                            
         CHI   R3,3                                                             
         JH    EXITN                                                            
         MVI   WORK,C'0'                                                        
         MVC   WORK+1(4),WORK                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),1(R2)                                                    
         PACK  DUB(3),WORK(5)                                                   
         MVC   0(1,R4),0(R2)                                                    
         OI    0(R4),C' '          FORCE GROUP CODE TO UPPER CASE               
         MVC   1(2,R4),DUB                                                      
                                                                                
         LA    R3,KEY                                                           
         USING PRGRECD,R3                                                       
         XC    PRGKEY,PRGKEY       READ PRODUCT GROUP HEADER RECORD             
         MVI   PRGPTYP+0,X'0D'                                                  
         MVI   PRGPTYP+1,X'01'                                                  
         MVC   PRGPAGMD,NBACTAM                                                 
         MVC   PRGPCLT,NBSELCL2                                                 
         MVC   PRGPID(L'PRGPID+L'PRGPPRD),0(R4)                                 
         GOTOR DIRREAD,NBFILSPT                                                 
         JNE   EXITN                                                            
         GOTOR FILGETR,NBAPRD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,NBAPRD                                                        
         USING PRGEL10,PRGEL                                                    
         CLI   PRGEL10,X'10'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,KEY                                                           
         MVI   PRGPTYP+1,X'81'     READ PASSIVES TO BUILD PRODUCT LIST          
                                                                                
         SR    R0,R0                                                            
         MVC   KEYSAVE,PRGKEY                                                   
         GOTOR DIRHIGH,NBFILSPT                                                 
         B     VALPGP06                                                         
                                                                                
VALPGP04 GOTOR DIRRSEQ,NBFILSPT                                                 
                                                                                
VALPGP06 CLC   PRGKEY(PRGPPRD-PRGKEY),KEYSAVE                                   
         BNE   VALPGP08                                                         
                                                                                
         MVC   SAVEKEY(L'PRGKEY),KEY                                            
                                                                                
         LA    RF,KEY                                                           
         USING PRDHDR,RF                                                        
         XC    PKEY,PKEY                                                        
         MVC   PKEYAM,NBACTAM                                                   
         L     RE,NBACLI                                                        
         MVC   PKEYCLT,2(RE)                                                    
         MVC   PKEYPRD,SAVEKEY+PRGPPRD-PRGPTYP                                  
                                                                                
         GOTOR DIRREAD,NBFILSPT                                                 
         JNE   EXITN                                                            
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'PRGKEY),SAVEKEY                                            
         GOTOR DIRREAD,NBFILSPT                                                 
         JE    *+6                                                              
         DC    H'00'                                                            
         AHI   R0,1                                                             
         B     VALPGP04                                                         
         DROP  R4                                                               
                                                                                
VALPGP08 LTR   R0,R0                                                            
         JZ    EXITN                                                            
         J     EXITY                                                            
         DROP  RB,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SEED BUFFER RECORD WITH INVOICE VALUES                   *         
***********************************************************************         
                                                                                
         USING SNVIDELD,R1                                                      
SETINV   ST    RE,SAVERE                                                        
         MVC   TDINVDA,INVDMDA     SET DISK ADDRESS OF INVOICE                  
         MVC   TDINVCTL,INVCTL     SET INVOICE HEADER CONTROL                   
         MVC   TDINVSDT,INVSDT     SET INVOICE START DATE                       
         MVC   TDINVPRD,INVPRD     SET PRODUCT NUMBER                           
         MVC   TDINVPRA,INVPRDA    SET PRODUCT ALPHA                            
         MVC   TDINVPIG,INVPIG     SET PIGGYBACK PRODUCT NUMBER                 
         MVC   TDINVPIA,INVPIGA    SET PIGGY PRODUCT ALPHA                      
         MVC   TDINVEST,INVEST     SET ESTIMATE NUMBER                          
         MVC   TDINVREP,INVREP     SET INVOICE REP CODE                         
         SR    RF,RF                                                            
         ICM   RF,1,SNVIDCML       SET FILM#1 TRAFFIC SEQUENCE#                 
         JZ    SETINV02                                                         
         BCTR  RF,0                                                             
         MHI   RF,FLMNTRYL                                                      
         AHI   RF,FLMSAVE-SAVED                                                 
         LA    RF,SAVED(RF)                                                     
         MVC   TDINVFT1,FLMSEQN-FLMNTRY(RF)                                     
SETINV02 SR    RF,RF                                                            
         ICM   RF,1,SNVIDCM2       SET FILM#2 TRAFFIC SEQUENCE#                 
         JZ    SETINV04                                                         
         BCTR  RF,0                                                             
         MHI   RF,FLMNTRYL                                                      
         AHI   RF,FLMSAVE-SAVED                                                 
         LA    RF,SAVED(RF)                                                     
         MVC   TDINVFT2,FLMSEQN-FLMNTRY(RF)                                     
SETINV04 LA    RF,SNVIDELD                                                      
         S     RF,NBAIO                                                         
         STCM  RF,3,TDINVDSP       SET DISPLACEMENT TO DETAIL ELEMENT           
         L     RE,NBAIO            COMPUTE INVOICE RECORD CHECK SUM             
         SR    RF,RF                                                            
         ICM   RF,3,SNVRLEN-SNVKEYD(RE)                                         
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,TDINVCSM                                                   
         MVC   TDINVSEQ,LASTISEQ   SET INVOICE SEQUENCE NUMBER                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ PRODUCT RECORD                                      *         
***********************************************************************         
                                                                                
GETPRD   ST    RE,SAVERE2                                                       
         LA    RF,KEY                                                           
         USING PRDHDR,RF                                                        
         XC    PKEY,PKEY                                                        
         MVC   PKEYAM,NBACTAM                                                   
         MVC   PKEYCLT,NBACTCLI                                                 
         MVC   PKEYPRD,0(R1)                                                    
         GOTOR DIRREAD,NBFILSPT                                                 
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR FILGETR,NBAPRD      GET PRODUCT RECORD                           
         L     RE,SAVERE2                                                       
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* GET ESTIMATE DEMO NAMES                                             *         
***********************************************************************         
                                                                                
GETEDN   ST    RE,SAVERE                                                        
         L     R1,NBAEST                                                        
         USING ESTHDR,R1           R1=A(ESTIMATE RECORD)                        
         XC    WORK,WORK                                                        
         MVC   WORK(L'EDEMLST),EDEMLST                                          
         MVC   WORK+L'EDEMLST(L'EDEM21),EDEM21                                  
         LA    RE,WORK                                                          
         LA    RF,WORK2                                                         
         SR    R0,R0                                                            
GETEDN02 OC    0(L'EDEMLIST,RE),0(RE)                                           
         JZ    GETEDN06                                                         
         CLI   0(RE),X'FF'         END OF LIST                                  
         JE    GETEDN06                                                         
         CLI   1(RE),33            REMOVE USER DEMOS                            
         JE    GETEDN04                                                         
         CLI   1(RE),63            AND WEIGHTED DEMOS                           
         JE    GETEDN04                                                         
         MVC   0(L'EDEMLIST,RF),0(RE)                                           
         AHI   RF,L'EDEMLIST                                                    
         AHI   R0,1                                                             
GETEDN04 AHI   RE,L'EDEMLIST                                                    
         J     GETEDN02                                                         
                                                                                
GETEDN06 STCM  R0,3,ESTNDEMS                                                    
         LTR   R0,R0                                                            
         JZ    GETEDNX                                                          
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,DBFNTI                                                    
         MVI   DBSELMED,NETMEDQ                                                 
         MVI   DBSELSRC,C'N'                                                    
         LA    RF,EUSRNMS                                                       
         DROP  R1                                                               
                                                                                
         L     R2,NBAEST                                                        
         USING ESTHDR,R2           R1=A(ESTIMATE RECORD)                        
         GOTOR DEMOCON,DMCB,((R0),WORK2),(13,ESTDEMSN),(C'S',DBLOCK),  *        
               EUSRNMS,ENONTDMS                                                 
                                                                                
GETEDNX  L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET UNIT RECORD AND FORMAT FIELDS FOR DOWNLOADING                   *         
***********************************************************************         
                                                                                
GETUNT   NTR1  BASE=*,LABEL=*                                                   
         MVI   FLAG,1              SET TO READ AND APPLY FILTERS                
         LTR   R0,R1                                                            
         BNZ   *+12                                                             
         LA    R0,TDUNTDA          SET TO READ AND FORMAT                       
         MVI   FLAG,0                                                           
         GOTOR NBDM,DMCB,(0,DMGETR),UNTFIL,(R0),NBAIO,DMWK                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,NBAIO                                                         
         USING NURECD,R3           R3=A(UNIT RECORD)                            
                                                                                
         CLI   FLAG,1              TEST FILTERING                               
         BNE   GETUNT10                                                         
         TM    NUPACKST,NUPANSDQ+NUPALOKQ                                       
         JNZ   EXITN               DISCARD NO-SHOW/LOCKED UNITS                 
         TM    NUUNITST,NUUNPREQ+NUUNMISQ                                       
         JNZ   EXITN               DISCARD MISSED/PRE-EMPT UNITS                
                                                                                
         CLI   PRDNUM,POLNUM                                                    
         JE    GETUNT03            IF POL, KEEP IT                              
                                                                                
         XC    PLKUP(PLKUPL),PLKUP                                              
         MVC   PLKPRD,NUPRD        APPLY PRODUCT FILTERS                        
         MVC   PLKPIG,NUPRD2                                                    
         CLI   PLKPRD,0            ANY PRODUCT?                                 
         JNE   GETUNT01                                                         
                                                                                
         L     R4,NBAIO                                                         
         USING NUPDED,R4                                                        
         GOTOR NBHELLO,DMCB,(C'G',UNTFILE),(X'19',(R4)),0                       
         CLI   12(R1),0                                                         
         JNE   EXITN               UNALLOC UNIT - SKIP IT                       
         L     R4,12(R1)                                                        
         MVC   PLKPRDA,NUPDEPR                                                  
         XC    PLKPIGA,PLKPIGA                                                  
         J     GETUNT02                                                         
         DROP  R4                                                               
                                                                                
GETUNT01 L     R3,NBAIO                                                         
         GOTOR GETPRC,PLKPRD                                                    
         JNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   PLKPRDA,PLPMNEM                                                  
         CLI   PLKPIG,0                                                         
         JE    GETUNT02                                                         
         GOTOR GETPRC,PLKPIG                                                    
         JNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   PLKPIGA,PLPMNEM                                                  
                                                                                
GETUNT02 GOTOR TSTPRU                                                           
         JNE   EXITN                                                            
                                                                                
GETUNT03 L     R3,NBAIO                                                         
         CLI   COSTFILT,YESQ       APPLY COST FILTER IF REQUIRED                
         BNE   *+14                                                             
         CLC   NUACTUAL,COST       MATCH TO ACTUAL UNIT COST                    
         JNE   EXITN                                                            
                                                                                
         CLC   LP_QMAPN,MAP#UONR                                                
         BNE   *+14                                                             
         OC    NUAFFTIM,NUAFFTIM   RNO - CHECK AFFID TIME IS SET                
         JNZ   EXITN               YES - DON'T WANT MATCHED ONES                
                                                                                
         L     RF,LP_ASETK                                                      
         LA    R1,DMCB                                                          
         OC    APKN,APKN           APPLY PACKAGE FILTER                         
         BZ    GETUNT04                                                         
         GOTOR (RF),(R1),(1,PKNFLT),NUPACK,SAVED,('FF',LP_D)                    
         JNE   EXITN                                                            
                                                                                
GETUNT04 OC    ATIM,ATIM           APPLY TIME FILTER                            
         BZ    GETUNT06                                                         
         GOTOR (RF),(R1),(1,TIMFLT),NUKTIME,SAVED,('FF',LP_D)                   
         JNE   EXITN                                                            
                                                                                
GETUNT06 OC    ADPT,ADPT           APPLY DAYPART FILTER                         
         BZ    GETUNT08                                                         
         GOTOR (RF),(R1),(1,DPTFLT),NUKDP,SAVED,('FF',LP_D)                     
         JNE   EXITN                                                            
                                                                                
GETUNT08 OC    ALEN,ALEN           APPLY LENGTH FILTER                          
         BZ    GETUNT10                                                         
         GOTOR (RF),(R1),(1,LENFLT),NULEN,SAVED,('FF',LP_D)                     
         JNE   EXITN                                                            
                                                                                
GETUNT10 SR    R0,R0                                                            
         ICM   R0,1,ESTCOUNT       TEST HAVE ESTIMATE TABLE BUILT               
         BZ    GETUNT16                                                         
         LHI   R1,ESTTAB-SAVED                                                  
         LA    R1,SAVED(R1)                                                     
         USING ESTTAB,R1                                                        
GETUNT12 CLC   ESTTNUM,NUKEST      YES - LOOK UP ESTIMATE IN TABLE              
         BE    GETUNT14                                                         
         AHI   R1,ESTTABL                                                       
         BCT   R0,GETUNT12                                                      
         CLI   FLAG,1              TEST APPLYING FILTERS                        
         JE    EXITN               YES - ESTIMATE MAY NOT BE FOUND              
         DC    H'0'                NO  - ESTIMATE MUST BE FOUND                 
*                                                                               
GETUNT14 CLI   PROFI2N+11,C'Y'     KEEP STEWARDSHIP ESIMATES?                   
         BE    *+12                                                             
         TM    ESTTSTAT,X'80'      TEST STEWARDSHIP ESTIMATE                    
         JO    EXITN               IF ON BYPASS                                 
         LA    R1,ESTTDEMO                                                      
         B     GETUNT18                                                         
         DROP  R1                                                               
                                                                                
GETUNT16 LA    R2,KEY                                                           
         USING ESTHDR,R2           BUILD KEY OF ESTIMATE RECORD                 
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,NBACTCLI                                                 
         MVC   EKEYPRD,PRDALPH                                                  
         MVC   EKEYEST,NUKEST                                                   
         L     R2,NBAEST                                                        
         LA    R1,EDEMLST                                                       
         CLC   EKEY,KEY            TEST ESTIMATE IN I/O AREA                    
         BE    GETUNT18                                                         
         GOTOR DIRREAD,NBFILSPT    NO - READ IT                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR FILGETR,NBAEST                                                   
         LA    R1,EDEMLST                                                       
                                                                                
GETUNT18 MVC   NDDEMOS(3),0(R1)    SET DEMO CODE                                
         LHI   R0,1                SET SINGLE DEMO LOOKUP                       
         STCM  R0,3,NDEMOS                                                      
         DROP  R2                                                               
                                                                                
GETUNT20 MVI   NBSURVEY,0          SET UNKNOWN TYPE                             
         XC    LP_RMASK,LP_RMASK                                                
         XC    UNTVALS(UNTVALL),UNTVALS                                         
         MVI   UNTCHARD,SPACEQ                                                  
         MVC   UNTCHARD+1(UNTCHARL-1),UNTCHARD                                  
         XC    UNTPRDS,UNTPRDS     CLEAR PRODUCT LIST                           
         MVC   UNTPRDS(L'NUPRD),NUPRD                                           
         MVC   UNTPRDS+L'NUPRD(L'NUPRD2),NUPRD2                                 
         OC    NUPROGNM,BLANKS     ENSURE TRAILING SPACES IN NUPROGNM           
         XC    UNTPRDSA,UNTPRDSA                                                
                                                                                
         OC    TDINVDA,TDINVDA     TEST WE HAVE INVOICE DISK ADDRESS            
         BZ    *+8                                                              
         MVI   UNTMATCH,YESQ       YES - SET THIS UNIT IS MATCHED               
                                                                                
         TM    NUUNITST,NUUNPFBQ                                                
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#PFBS      SET PFB                                      
         TM    NUUNITST,NUUNPREQ                                                
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#PMPT      SET PRE-EMPTED                               
         TM    NUUNITST,NUUNMISQ                                                
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#MISS      SET MISSED                                   
         TM    NUUNITST,NUUNMKGQ                                                
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#MKGD      SET MAKEGOOD                                 
         TM    NUUNITST,NUUNACTQ                                                
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#ACTI      SET ACTUAL COST INPUT                        
         TM    NUUNITST,NUUNACIQ                                                
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#ASSI      SET ASSIGNED COST INPUT                      
         TM    NUPACKST,NUPALOKQ                                                
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#LOCK      SET LOCKED                                   
         TM    NUUNST2,NUUNMGDQ                                                 
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#MGDR      SET MAKE-GOOD DEMO RETRIEVAL                 
         TM    NUUNST2,NUUNPAFQ                                                 
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#PRAF      SET PRODUCT ALLOCATION FROZEN                
         TM    NUPACKST,NUPAFRZQ                                                
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#FRZN      SET FROZEN                                   
         TM    NUPACKST,NUPAAIOQ                                                
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#AUDO      SET AUDIT ON                                 
                                                                                
         LA    R4,NUDATA           PROCESS UNIT RECORD ELEMENTS                 
         SR    R0,R0                                                            
GETUNT22 CLI   0(R4),0             TEST END OF RECORD                           
         BE    GETUNT40                                                         
                                                                                
         USING NUSDRD,R4                                                        
         CLI   NUSDREL,NUSDRELQ                                                 
         BNE   GETUNT24                                                         
         GOTOR SETMIR,NUMIRTYP     SET MIRROR ADJUSTMENT (UNTMIROR)             
         MVC   NBSURVEY,NUPOSTYP   SET SURVEY TYPE                              
         MVI   NBHUNOPT,0          SET WANT IMPS IN THOUSANDS                   
         MVI   NBPREOPT,0                                                       
         CLI   NBSURVEY,C'S'                                                    
         BE    *+8                                                              
         CLI   NBSURVEY,C'H'                                                    
         BE    *+8                                                              
         CLI   NBSURVEY,C'N'                                                    
         BE    *+12                                                             
         MVI   NBHUNOPT,YESQ       SET WANT IMPS IN HUNDREDS                    
         MVI   NBPREOPT,YESQ       AND CABLE RATING TO 2 DECIMALS               
         TM    NUSDST3,NUSDWUBQ                                                 
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#WDOW      SET WINDOW                                   
         TM    NUSDST3,NUSDADUQ                                                 
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#ADUS      SET ADU                                      
         TM    NUSDST3,NUSDACOQ                                                 
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#ACOV      SET ASSIGNED COST OVERRIDE                   
                                                                                
         CLI   DAYIND,0            TEST NO DAY FILTER                           
         BE    GETUNT38                                                         
         CLI   DAYIND,LQ_TALLQ     APPLY ROTATION FILTER                        
         BE    GETUNT38                                                         
         SR    RF,RF                                                            
         ICM   RF,7,ADAY+1                                                      
         MVC   WORK(L'NUSDROT),NUSDROT                                          
         NC    WORK(L'NUSDROT),LW_DATA1-LW_D(RF)                                
         BNZ   GETUNT38                                                         
         J     EXITN                                                            
                                                                                
         USING NUCMLEL,R4                                                       
GETUNT24 CLI   NUCMLEID,NUCMLEQ                                                 
         BNE   GETUNT26                                                         
         TM    NUCMLFLG,NUCULCIQ+NUCUPCIQ+NUCDCHAQ                              
         BNZ   GETUNT38                                                         
******** MVC   UNTPREF,NUCMLREF    SET PATTERN REFERENCE #                      
         MVC   UNTPREF,NUCMLR3F    SET PATTERN REFERENCE #                      
         TM    NUCMLFLG,NUCBREQQ                                                
         BZ    *+16                                                             
         GOTOR SETMSK,MQ#BBRD      SET BILLBOARD                                
         MVI   UNTBBD,YESQ                                                      
         OC    TDINVDA,TDINVDA     TEST MATCHED INVOICE                         
         BNZ   GETUNT38            YES                                          
         CLI   NUCMLELN,NUCMADFL-NUCMLEL                                        
         BNH   GETUNT38                                                         
         MVC   UNTFILM1(L'NUCML1),NUCML1                                        
         TM    NUCMADFL,NUCMADF1   TEST AD-ID                                   
         BZ    *+12                                                             
         GOTOR EDTAID,UNTFILM1                                                  
         MVC   UNTFILM2(L'NUCML2),NUCML2                                        
         TM    NUCMADFL,NUCMADF2   TEST AD-ID                                   
         BZ    *+12                                                             
         GOTOR EDTAID,UNTFILM2                                                  
         B     GETUNT38                                                         
                                                                                
         USING NUBILD,R4                                                        
GETUNT26 CLI   NUBILEL,NUBILELQ                                                 
         BNE   GETUNT28                                                         
         GOTOR SETMSK,MQ#BILL      SET BILLED                                   
         CLI   NUBILTYP,C'T'       TEST TIME CHARGE                             
         BNE   *+12                                                             
         OI    UNTBSTAT,UNTBSTQ    YES - SET TIME CHARGE BILLED                 
         B     GETUNT38                                                         
         CLI   NUBILTYP,C'I'       TEST INTEGRATION CHARGE                      
         BNE   *+12                                                             
         OI    UNTBSTAT,UNTBSIQ    YES - SET INTEGRATION CHARGE BILLED          
         B     GETUNT38                                                         
         OI    UNTBSTAT,UNTBSSQ    YES - SET SPECIAL CHARGE BILLED              
         B     GETUNT38                                                         
                                                                                
         USING NUPAYD,R4                                                        
GETUNT28 CLI   NUPAYEL,NUPAYELQ                                                 
         BNE   GETUNT29                                                         
         GOTOR SETMSK,MQ#PAID      SET PAID                                     
         CLI   NUPAYTYP,C'T'       TEST TIME CHARGE                             
         BNE   *+12                                                             
         OI    UNTPSTAT,UNTPSTQ    YES - SET TIME CHARGE PAID                   
         B     GETUNT38                                                         
         CLI   NUPAYTYP,C'I'       TEST INTEGRATION CHARGE                      
         BNE   *+12                                                             
         OI    UNTPSTAT,UNTPSIQ    YES - SET INTEGRATION CHARGE PAID            
         B     GETUNT38                                                         
         CLI   NUPAYTYP,C'O'       TEST OTHER CHARGE PAID                       
         BNE   *+8                                                              
         OI    UNTPSTAT,UNTPSOQ    YES - SET OTHER CHARGE PAID                  
         B     GETUNT38                                                         
                                                                                
         USING NUPDED,R4                                                        
GETUNT29 CLI   NUPDEEL,NUPDELQ                                                  
         BNE   GETUNT30                                                         
         XC    UNTPRDS,UNTPRDS                                                  
         SR    R0,R0               SET PRODUCTS                                 
         IC    R0,NUPDELEN                                                      
         SHI   R0,NUPDEPR-NUPDED                                                
         SRDA  R0,32                                                            
         LHI   RE,NUPDTLEN                                                      
         DR    R0,RE                                                            
         LR    R0,R1               R0=N'PRODUCTS IN LIST                        
         CHI   R0,MAXUPRD                                                       
         BNH   *+8                                                              
         LHI   R0,MAXUPRD                                                       
         LA    R5,UNTPRDSA                                                      
         LA    R9,NUPDEPR          RF=A(PRODUCT LIST IN ELEMENT)                
         USING NUPDEPR,R9                                                       
                                                                                
         MVC   0(L'NUPDEPR,R5),NUPDEPR                                          
         AHI   R5,L'NUPDEPR                                                     
         AHI   R9,NUPDTLEN                                                      
         BCT   R0,*-14                                                          
         B     GETUNT38                                                         
         DROP  R9                                                               
                                                                                
         USING NUPRDD,R4                                                        
GETUNT30 CLI   NUPRDEL,NUPRDELQ                                                 
         BNE   GETUNT32                                                         
         SR    R0,R0               SET PRODUCTS                                 
         IC    R0,NUPRDLEN                                                      
         SHI   R0,3                                                             
         SRDA  R0,32                                                            
         LHI   RE,6                                                             
         DR    R0,RE                                                            
         LR    R0,R1               R0=N'PRODUCTS IN LIST                        
         CHI   R0,MAXUPRD                                                       
         BNH   *+8                                                              
         LHI   R0,MAXUPRD                                                       
         LA    RE,UNTPRDS                                                       
         LA    RF,NUPRDPR          RF=A(PRODUCT LIST IN ELEMENT)                
         USING NUPRDPR,RF                                                       
         MVC   0(L'NUPRD,RE),NUPRDPR                                            
         AHI   RE,L'NUPRDPR                                                     
         AHI   RF,6                                                             
         BCT   R0,*-14                                                          
         B     GETUNT38                                                         
         DROP  RF                                                               
                                                                                
         USING NUCOMD,R4                                                        
GETUNT32 CLI   NUCOMEL,NUCOMELQ    TEST COMMENT ELEMENT                         
         BNE   GETUNT34                                                         
         CLI   NUCOMTYP,NUCOMTCQ   TEST CLIENT COMMENT                          
         BNE   GETUNT34                                                         
         CLI   COMIND,YESQ         TEST PC WANTS COMMENTS                       
         BNE   GETUNT34                                                         
         LA    RF,UNTCCOM1                                                      
         CLI   NUCOMLIN,1          TEST LINE 1                                  
         BE    *+16                                                             
         LA    RF,UNTCCOM2                                                      
         CLI   NUCOMLIN,2          TEST LINE 2                                  
         BNE   GETUNT34                                                         
         SR    RE,RE                                                            
         IC    RE,NUCOMLEN                                                      
         SHI   RE,NUCOMMNT-NUCOMD+1                                             
         EX    RE,*+8                                                           
         B     GETUNT34                                                         
         MVC   0(0,RF),NUCOMMNT                                                 
         B     GETUNT38                                                         
                                                                                
         USING NUOTH,R4                                                         
GETUNT34 CLI   NUOTEL,NUOTELQ      TEST OTHER ELEMENT                           
         BNE   GETUNT38                                                         
         CLI   NUOTTYP,NUOTUSDQ    TEST USER DEFINED SUBDAYPART                 
         BE    *+12                                                             
         CLI   NUOTTYP,NUOTSDPQ    TEST SUBDAYPART                              
         BNE   GETUNT38                                                         
         MVC   UNTSUBDP,NUOTHER                                                 
                                                                                
GETUNT38 IC    R0,1(R4)            BUMP TO NEXT ELEMENT ON RECORD               
         AR    R4,R0                                                            
         B     GETUNT22                                                         
                                                                                
GETUNT40 GOTOR TSTMSK,MQ#ADUS      TEST ADU UNIT                                
         BNZ   GETUNT42                                                         
         GOTOR TSTMSK,MQ#PFBS      OR PFB UNIT                                  
         BNZ   GETUNT42                                                         
         GOTOR SETMSK,MQ#ESTS      NOT ADU OR PFB - SET EST                     
                                                                                
GETUNT42 CLI   FLAG,1              TEST FILTERING                               
         BNE   GETUNT44                                                         
         GOTOR LP_ATSTM,LP_D       CALL TSTMSK TO SEE IF UNIT QUALIFIES         
         JNE   EXITN                                                            
                                                                                
GETUNT44 MVI   NBWHERE,NBWNETIO    CALL NETVALUE TO GET UNIT VALUES             
         OI    NBSBKEND,NBNODPT2   SET DON'T READ DAYPART RECORDS               
         GOTOR NBNETVAL,DMCB,NETIOD                                             
         MVI   NBWHERE,0                                                        
                                                                                
GETUNTX  J     EXITY                                                            
                                                                                
EDTAID   NTR1  ,                   ** EDIT AD-ID **                             
         LR    RF,R1                                                            
         MVC   WORK(L'NUCML1),0(RF)                                             
         GOTOR TRPACK,DMCB,(C'U',WORK),(RF)                                     
         J     EXIT                                                             
                                                                                
UNTFILE  DC    CL8'UNTFILE'                                                     
         EJECT                                                                  
***********************************************************************         
* SET RECORD MASK BIT ON                                              *         
***********************************************************************         
                                                                                
SETMSK   STM   RE,R1,12(RD)                                                     
         LR    RE,R1                                                            
         BCTR  RE,0                                                             
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RF,BITLIST(RF)                                                   
         LA    RE,LP_RMASK(RE)                                                  
         OC    0(1,RE),0(RF)                                                    
SETMSKX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* TEST RECORD MASK BIT IS ON                                          *         
***********************************************************************         
                                                                                
TSTMSK   STM   RE,R1,12(RD)                                                     
         LR    RE,R1                                                            
         BCTR  RE,0                                                             
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         IC    RF,BITLIST(RF)                                                   
         LA    RE,LP_RMASK(RE)                                                  
         EX    RF,*+8                                                           
         B     TSTMSKX                                                          
         TM    0(RE),0                                                          
TSTMSKX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  R3,R4,RB                                                         
         EJECT                                                                  
***********************************************************************         
* GET INVOICE RECORD AND FORMAT DETAILS FOR DOWNLOADING               *         
***********************************************************************         
                                                                                
GETINV   NTR1  BASE=*,LABEL=*                                                   
         GOTOR NBDM,DMCB,(0,DMGETR),XSPFIL,TDINVDA,NBAIO,DMWK                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,NBAIO            COMPUTE INVOICE RECORD CHECK SUM             
         SR    RF,RF                                                            
         ICM   RF,3,SNVRLEN-SNVKEYD(RE)                                         
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         BO    *-4                                                              
         CLM   R0,15,TDINVCSM      TEST INVOICE RECORD CHANGED                  
         BNE   GETINVN                                                          
         SR    R3,R3                                                            
         ICM   R3,3,TDINVDSP       R3=DISPLACEMENT TO DETAIL ELEMENT            
         BZ    GETINVN                                                          
         A     R3,NBAIO            R3=A(DETAIL ELEMENT)                         
         USING SNVIDELD,R3                                                      
         CLI   SNVIDEL,SNVIDELQ    ENSURE WE ARE POINTING TO ELEMENT            
         BNE   GETINVN                                                          
                                                                                
         XC    IDTVALS(IDTVALSL),IDTVALS                                        
         MVC   IDTKEY,TDINVDA                                                   
         MVC   IDTREP,TDINVREP     SET REP CODE                                 
         OC    TDUNTDA,TDUNTDA     TEST WE HAVE A UNIT RECORD ADDRESS           
         BZ    *+8                                                              
         MVI   IDTMATCH,YESQ       YES - SET THIS IS MATCHED                    
         CLI   TKMIRROR,TKMNO      TEST MIRROR                                  
         BE    *+8                                                              
         MVI   IDTMIROR,YESQ       YES - SET THIS IS A MIRROR                   
                                                                                
         GOTOR DATCON,DMCB,(2,TDINVSDT),(0,WORK+6)                              
         SR    R0,R0               CALCULATE INVOICE DATE IF NECESSARY          
         ICM   R0,1,SNVIDDAY                                                    
         BZ    GETINV02                                                         
         MVC   WORK(6),WORK+6                                                   
         GOTOR ADDAY,DMCB,WORK,WORK+6,(R0)                                      
                                                                                
GETINV02 GOTOR DATCON,DMCB,(0,WORK+6),(3,IDTAFDT)                               
         GOTOR DATCON,DMCB,(0,WORK+6),(2,IDTMOS)                                
         SR    R0,R0                                                            
         ICM   R0,3,SNVIDTIM                                                    
         SRDL  R0,32                                                            
         LHI   RE,60                                                            
         DR    R0,RE                                                            
         MHI   R1,100                                                           
         AR    R0,R1                                                            
         AHI   R0,600                                                           
         CHI   R0,2400                                                          
         BL    *+8                                                              
         SHI   R0,2400                                                          
         STCM  R0,3,IDTAFTM        SET MILITARY TIME                            
                                                                                
         MVI   IDTFLMRN,NOQ        FLIGHT DATE RANGE                            
         MVI   IDTFLMTM,NOQ        FLIGHT TIME RANGE                            
                                                                                
         MVI   IDTICOST,NOQ                                                     
         TM    SNVIDCTL,SNVIDICQ   IGNORE COST                                  
         BZ    *+8                                                              
         MVI   IDTICOST,YESQ                                                    
                                                                                
         MVI   IDTITIME,NOQ                                                     
         TM    SNVIDCTL,SNVIDITQ   IGNORE TIME                                  
         BZ    *+8                                                              
         MVI   IDTITIME,YESQ                                                    
                                                                                
         MVI   IDTIFILM,NOQ                                                     
         TM    SNVIDCTL,SNVIDIFQ   IGNORE FILM                                  
         BZ    *+8                                                              
         MVI   IDTIFILM,YESQ                                                    
                                                                                
         MVI   IDTILEN,NOQ                                                      
         TM    SNVIDCT2,SNVIDISQ   IGNORE LENGTH                                
         BZ    *+8                                                              
         MVI   IDTILEN,YESQ                                                     
                                                                                
         MVC   IDTSLN,SNVIDSLN     SET SECONDS LENGTH                           
         MVC   IDTPRD,SNVIDPRD     SET PRODUCT                                  
         CLI   SNVIDLEN,SNVIDL2Q                                                
         BNH   *+14                                                             
         MVC   IDTPRDA,SNVIDAP1    SET PRODUCT ALPHA                            
         MVI   IDTPRD,0                                                         
                                                                                
         CLC   IDTPRD,TDINVPRD     IF PRODUCT SAME AS HEADER                    
         BNE   *+8                                                              
         MVI   IDTPRD,0            DON'T SEND IT                                
         CLC   IDTPRDA,TDINVPRA                                                 
         BNE   *+10                                                             
         XC    IDTPRDA,IDTPRDA                                                  
                                                                                
         MVC   IDTPIG,SNVIDPR2     SET PIGGYBACK PRODUCT                        
         CLI   SNVIDLEN,SNVIDL2Q                                                
         BNH   *+14                                                             
         MVC   IDTPIGA,SNVIDAP2    SET PIGGYBACK PRODUCT ALPHA                  
         MVI   IDTPIG,0                                                         
                                                                                
         CLC   IDTPIG,TDINVPIG     IF PIGGBYBACK SAME AS HEADER                 
         BNE   *+8                                                              
         MVI   IDTPIG,0            DON'T SEND IT                                
         CLC   IDTPIGA,TDINVPIA                                                 
         BNE   *+10                                                             
         XC    IDTPIGA,IDTPIGA                                                  
                                                                                
         MVC   IDTEST,SNVIDEST     SET ESTIMATE NUMBER                          
         MVC   IDTPKG,SNVIDPKG     SET PACKAGE NUMBER                           
         CLC   IDTEST,TDINVEST     IF ESTIMATE SAME AS HEADER                   
         BNE   *+8                                                              
         MVI   IDTEST,0            DON'T SEND IT                                
                                                                                
         MVC   IDTCST,SNVIDCST     SET GROSS COST                               
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE ITEM?                               
         JZ    GETINV03                                                         
         ICM   RE,15,SNVIDCST                                                   
         MHI   RE,-1                                                            
         STCM  RE,15,IDTCST                                                     
                                                                                
GETINV03 MVC   IDTINT,SNVIDINT     SET INTEGRATION COST                         
         TM    TDINVCTL,SNVHDNTQ+SNVHDDNQ                                       
         BZ    GETINV04                                                         
         ICM   R1,15,IDTCST        COST IS NET - CALCULATE GROSS                
         LHI   RE,200                                                           
         MR    R0,RE                                                            
         LHI   RE,100-COMMPCT      ASSUME NET IS 15% OF GROSS                   
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         STCM  R1,15,IDTCST                                                     
                                                                                
GETINV04 SR    R0,R0               SET DEMO VALUE                               
         CLI   SNVIDLEN,SNVIDL2Q                                                
         BL    *+8                                                              
         ICM   R0,3,SNVIDNDM                                                    
         STCM  R0,15,IDTDEMO                                                    
         MVC   IDTPROG,SNVIDUPG    PROGRAM CODE                                 
         MVC   IDTFSEQ1,SNVIDCML   SET FILM SEQUENCE NUMBERS                    
         MVC   IDTFSEQ2,SNVIDCM2                                                
         XC    IDTKSEQ,IDTKSEQ                                                  
         MVC   IDTKSEQ+(L'IDTKSEQ-L'IDTFSEQ1)(L'IDTFSEQ1),IDTFSEQ1              
         TM    SNVIDCTL,SNVIDMGQ   TEST AFFIDAVIT IS A MAKEGOOD                 
         BZ    *+8                                                              
         MVI   IDTMKGD,YESQ        YES - SET MAKEGOOD INDICATOR                 
         MVC   IDTISEQ,TDINVSEQ    SET INVOICE SEQUENCE NUMBER                  
         TM    SNVIDCT2,SNVIDEMS   TEST E-MAIL SENT                             
         BZ    *+8                                                              
         MVI   IDTEMAIL,YESQ       YES - SET E-MAIL SENT                        
         TM    SNVIDCTL,SNVIDSIQ   TEST SKIP INTERVAL CHECKING                  
         BZ    *+8                                                              
         MVI   IDTSKIP,YESQ        YES - SET SKIP INTERVAL CHECKING             
         TM    SNVIDCTL,SNVIDBLQ   TEST BILLBOARD                               
         BZ    *+8                                                              
         MVI   IDTBBD,YESQ         YES - SET BILLBOARD                          
                                                                                
         L     R3,NBAIO                                                         
         AHI   R3,42                                                            
         USING SNVHDELD,R3                                                      
         CLI   0(R3),SNVHDELQ      X'10' ELEMENT                                
         BNE   GETINV06                                                         
         CLI   SNVHDLEN,SNVHDLNQ                                                
         BNH   *+10                                                             
         MVC   IDTREP,SNVHDREP                                                  
                                                                                
GETINV06 CLI   IDTFSEQ1,0          FOUND INTERNAL CMML SEQ?                     
         BE    GETINV36                                                         
                                                                                
         L     R3,NBAIO                                                         
         AHI   R3,42                                                            
         USING SNVHDELD,R3                                                      
         CLI   0(R3),0             MAKE SURE THIS IS THE FIRST INVOICE          
         BNE   GETINV08            HEADER RECORD                                
         CLI   0(R3),SNVHDELQ      IT MUST HAVE A X'10' ELEMENT                 
         BE    GETINV10                                                         
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GETINV12                                                         
                                                                                
GETINV08 L     R3,NBAIO            GET FIRST INVOICE RECORD IN SEQ.             
         USING SNVKEYD,R3                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(SNVKMINK-SNVKEY),SNVKEY                                      
         GOTOR DIRREAD,NBFILXSP                                                 
         CLC   KEY(SNVKMINK-SNVKEY),KEYSAVE                                     
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTOR FILGETR,NBAIO                                                    
                                                                                
GETINV10 L     R3,NBAIO                                                         
         AHI   R3,42                                                            
         USING SNVCMELD,R3                                                      
GETINV12 CLI   0(R3),0                                                          
         BE    GETINV16                                                         
         CLI   0(R3),SNVCMELQ                                                   
         BNE   GETINV14                                                         
         CLC   SNVCMICD,IDTFSEQ1    FOUND INTERNAL CMML SEQ MATCH?              
         BNE   GETINV14                                                         
         CLI   SNVCMSEQ+1,0                                                     
         BE    *+10                                                             
         MVC   IDTKSEQ,SNVCMSEQ  SAVE ACTUAL CMML SEQ #                         
         B     GETINV16                                                         
                                                                                
GETINV14 ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GETINV12                                                         
                                                                                
GETINV16 LA    R3,KEY              READ TRAFFIC COMMERCIAL RECORD               
         USING CMLKEY,R3                                                        
         XC    CMLKEY,CMLKEY                                                    
         MVC   CMLPID,CMLPIDL                                                   
         MVC   CMLPAM,NBACTAM                                                   
         MVC   CMLPCLT,NBACTCLI                                                 
         MVC   CMLPSEQ+(L'CMLPSEQ-L'IDTKSEQ)(L'IDTKSEQ),IDTKSEQ                 
         GOTOR DIRREAD,NBFILSPT                                                 
         BNE   GETINV36                                                         
         GOTOR FILGETR,NBAIO                                                    
*                                                                               
         L     R3,NBAIO                                                         
         LA    R3,CMLDTAEL                                                      
GETINV18 CLI   0(R3),0                                                          
         BE    GETINV36                                                         
         CLI   0(R3),X'10'                                                      
         BNE   *+10                                                             
         USING CMLDTAEL,R3                                                      
         MVC   IDTFLMRD,CMLRCL                                                  
         CLI   0(R3),X'B0'                                                      
         BE    GETINV20                                                         
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GETINV18                                                         
         USING CMLMATCH,R3                                                      
                                                                                
GETINV20 CLI   PROFI2B+6,YESQ          CHECK FILM TIME?                         
         BNE   GETINV28                                                         
         OC    PROFI2B+7(2),PROFI2B+7  CHECK FILM TIME MOS?                     
         BZ    *+14                                                             
         CLC   MOSI2BCT,IDTAFDT                                                 
         BH    GETINV28                                                         
                                                                                
         MVI   IDTFLMTM,NOQ                                                     
         MVC   HALF,IDTAFTM                                                     
         CLI   PROFI2C+6,C'Y'      CHECK BROADCAST DAY?                         
         BNE   GETINV26                                                         
                                                                                
         CLC   HALF,=H'600'        BEFORE 6AM?                                  
         BH    GETINV22                                                         
         CLC   HALF,=H'0000'       AFTER MIDNIGHT                               
         BNH   GETINV22                                                         
         SR    RF,RF                                                            
         ICM   RF,3,HALF                                                        
         AH    RF,=H'2400'                                                      
         STCM  RF,3,HALF                                                        
                                                                                
GETINV22 CLC   CMLMSTIM,=H'600'    BEFORE 6AM?                                  
         BH    GETINV24                                                         
         CLC   CMLMSTIM,=H'0000'   AFTER MIDNIGHT                               
         BNH   GETINV24                                                         
         SR    RF,RF                                                            
         ICM   RF,3,CMLMSTIM                                                    
         AH    RF,=H'2400'                                                      
         STCM  RF,3,CMLMSTIM                                                    
                                                                                
GETINV24 CLC   CMLMETIM,=H'600'    BEFORE 6AM?                                  
         BH    GETINV26                                                         
         CLC   CMLMETIM,=H'0000'   AFTER MIDNIGHT                               
         BNH   GETINV26                                                         
         SR    RF,RF                                                            
         ICM   RF,3,CMLMETIM                                                    
         AH    RF,=H'2400'                                                      
         STCM  RF,3,CMLMETIM                                                    
                                                                                
GETINV26 TM    CMLMFLAG,CMLMFDAY       CHECK TIMES DAILY?                       
         BO    *+14                                                             
         CLC   IDTAFDT,IDTFLMRD                                                 
         BNE   GETINV28                                                         
                                                                                
         MVI   IDTFLMTM,YESQ                                                    
         CLC   HALF,CMLMSTIM           WITHIN TIME RANGE?                       
         BL    *+18                                                             
         CLC   HALF,CMLMETIM                                                    
         BH    *+8                                                              
         MVI   IDTFLMTM,NOQ                                                     
                                                                                
GETINV28 CLI   PROFI2B+3,YESQ          CHECK FILM RANGE?                        
         BNE   GETINV36                                                         
         OC    PROFI2B+4(2),PROFI2B+4  CHECK FILM RANGE MOS?                    
         BZ    *+14                                                             
         CLC   MOSI2BCF,IDTAFDT                                                 
         BH    GETINV36                                                         
                                                                                
         LA    R4,CMLMSTD1                                                      
         MVI   IDTFLMRN,NOQ                                                     
         OC    0(L'CMLMSTD1,R4),0(R4)  ANY PERIODS ON COMMERCIAL?               
         JZ    GETINV36                                                         
                                                                                
         LA    R0,6                                                             
GETINV30 OC    0(L'CMLMSTD1,R4),0(R4)                                           
         JZ    GETINV32                                                         
         CLC   IDTMOS,2(R4)                                                     
         BH    GETINV34                                                         
         CLC   IDTMOS,0(R4)                                                     
         BNL   GETINV36                                                         
GETINV32 MVI   IDTFLMRN,YESQ                                                    
         B     GETINV36                                                         
GETINV34 AHI   R4,4                                                             
         BCT   R0,GETINV30                                                      
                                                                                
GETINV36 CLI   PROFTI+9,YESQ       TEST CHECK RECALL/RELEASE DATES              
         BNE   GETINVX                                                          
         LA    R4,TDINVFT1         R4=A(FILM SEQUENCE#S)                        
         LHI   R0,TDINVFTN         R0=N'FILMS                                   
GETINV38 OC    0(L'TDINVFT1,R4),0(R4)                                           
         BZ    GETINV46                                                         
                                                                                
         LHI   R2,FLMDATES-SAVED   LOOK UP FILM IN TABLE                        
         LA    R2,SAVED(R2)                                                     
         USING FLMDATES,R2                                                      
GETINV40 OC    FLMDATES(FLMDATEL),FLMDATES                                      
         BNZ   GETINV42                                                         
         LA    R3,KEY              READ TRAFFIC COMMERCIAL RECORD               
         USING CMLKEY,R3                                                        
         XC    CMLKEY,CMLKEY                                                    
         MVC   CMLPID,CMLPIDL                                                   
         MVC   CMLPAM,NBACTAM                                                   
         MVC   CMLPCLT,NBACTCLI                                                 
         MVC   CMLPSEQ+(L'CMLPSEQ-L'TDINVFT1)(L'TDINVFT1),0(R4)                 
         GOTOR DIRREAD,NBFILSPT                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR FILGETR,NBAIO                                                    
         L     R3,NBAIO                                                         
         MVC   FLMDSEQ,CMLSEQ+(L'CMLSEQ-L'FLMDSEQ)                              
         MVC   FLMDREL,CMLRLSE                                                  
         MVC   FLMDRCL,CMLRCL                                                   
         XC    FLMDATES+FLMDATEL(FLMDATEL),FLMDATES+FLMDATEL                    
         B     GETINV44                                                         
                                                                                
GETINV42 CLC   FLMDSEQ,0(R4)       MATCH FILM SEQUENCE# TO TABLE                
         BE    GETINV44                                                         
         AHI   R2,FLMDATEL         NO - BUMP TO NEXT ENTRY                      
         B     GETINV40                                                         
                                                                                
GETINV44 CLC   IDTAFDT,FLMDREL     TEST BEFORE RELEASE DATE                     
         BL    *+14                                                             
         CLC   IDTAFDT,FLMDRCL     OR AFTER RECALL DATE                         
         BNH   GETINV46                                                         
         MVI   IDTRCRL,YESQ        YES - SET OUTSIDE RECALL/RELEASE             
         B     GETINVX                                                          
                                                                                
GETINV46 AHI   R4,L'TDINVFT1       BUMP TO NEXT FILM SEQUENCE#                  
         BCT   R0,GETINV38         DO FOR N'FILMS                               
                                                                                
GETINVX  J     EXITY                                                            
                                                                                
GETINVN  LHI   R0,846              RETURN 'INVOICE CHANGED' ERROR               
         STCM  R0,3,LP_ERROR                                                    
         MVI   LP_EMSYS,NETSYSQ                                                 
         MVI   LP_RMODE,LP_RERRR                                                
         J     EXITN                                                            
         DROP  R2,R3,RB                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ERROR                                              *         
***********************************************************************         
                                                                                
         USING SNVKEYD,R3                                                       
MOS      USING MOSD,MOSVALS                                                     
LOGERR   OC    AMASTC,AMASTC                                                    
         BZR   RE                                                               
LOGERRN  NTR1  BASE=*,LABEL=N                                                   
         STC   R1,WORK                                                          
         L     R4,ACPRINT2                                                      
         USING DPRINT,R4                                                        
         LA    R5,ERRMESS                                                       
*                                                                               
         MVC   0(8,R5),=C'*DupErr*'                                             
         CLI   WORK,DUPERR                                                      
         BE    *+10                                                             
         MVC   0(8,R5),=C'*MisErr*'                                             
         AHI   R5,9                                                             
         MVC   0(12,R5),=C'Cli/Prd/Est='                                        
         MVC   12(L'NBCLICOD,R5),NBCLICOD                                       
         AHI   R5,12+L'NBCLICOD-1                                               
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C'/'                                                       
         MVC   2(L'PRDALPH,R5),PRDALPH                                          
         AHI   R5,2+L'PRDALPH-1                                                 
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(4,R5),=C'/ALL'                                                 
         CLI   ESTIND,LQ_TALLQ                                                  
         BNE   *+12                                                             
         AHI   R5,5                                                             
         B     LOGERR02                                                         
         L     RE,AEST                                                          
         SR    R0,R0                                                            
         IC    R0,LW_DATA1-LW_D(RE)                                             
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  2(3,R5),DUB                                                      
         CLI   LW_TYPE-LW_D(RE),LQ_TSINQ                                        
         BNE   *+12                                                             
         AHI   R5,5                                                             
         B     LOGERR02                                                         
         IC    R0,LW_DATA1+L'EKEYEST-LW_D(RE)                                   
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVI   5(R5),C'-'                                                       
         UNPK  6(3,R5),DUB                                                      
         AHI   R5,9                                                             
LOGERR02 MVC   0(5,R5),=C',Net='                                                
         MVC   5(L'NBSELNET,R5),NBSELNET                                        
         AHI   R5,5+L'NBSELNET-1                                                
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(5,R5),=C',MoS='                                                
         MVC   6(4,R5),MOS.MOSESTR                                              
         AHI   R5,10                                                            
         CLI   DATOPT,C' '                                                      
         BNH   LOGERR04                                                         
         MVC   0(8,R5),=C',DATOPT='                                             
         MVC   8(L'DATOPT,R5),DATOPT                                            
         AHI   R5,8+L'DATOPT                                                    
LOGERR04 CLI   INTOPT,C' '                                                      
         BNH   LOGERR06                                                         
         MVC   0(8,R5),=C',INTOPT='                                             
         MVC   8(L'INTOPT,R5),INTOPT                                            
         AHI   R5,8+L'INTOPT                                                    
LOGERR06 MVC   0(6,R5),=C',Inv#='                                               
         MVC   6(L'SNVKINV,R5),SNVKINV                                          
         AHI   R5,6+L'SNVKINV-1                                                 
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(6,R5),=C',DMDA='                                               
         LA    R0,DMDA                                                          
         CLI   WORK,MMMERR                                                      
         BNE   *+12                                                             
         AHI   R5,7                                                             
         B     LOGERR08                                                         
         LA    R0,TDINVDA                                                       
         MVC   1(9,R5),=C',TDINVDA='                                            
         AHI   R5,10                                                            
LOGERR08 GOTOR NBHEXOUT,DMCB,(R0),0(R5),L'DMDA,=C'TOG'                          
         CLI   WORK,DUPERR                                                      
         BNE   LOGERR10                                                         
         AHI   R5,L'TDINVDA*2                                                   
         MVC   0(10,R5),=C',TDINVDSP='                                          
         SR    R0,R0                                                            
         ICM   R0,3,TDINVDSP                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  10(4,R5),DUB                                                     
         AHI   R5,14                                                            
         MVC   0(7,R5),=C',INVDA='                                              
         GOTOR NBHEXOUT,DMCB,DMDA,7(R5),L'DMDA,=C'TOG'                          
         AHI   R5,7+(L'DMDA*2)                                                  
         MVC   0(8,R5),=C',INVDSP='                                             
         SR    R2,R3                                                            
         CVD   R2,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  8(4,R5),DUB                                                      
                                                                                
LOGERR10 GOTOR PRTLOG              PRINT ERROR ON RUNNER LOG                    
         J     EXIT                                                             
         DROP  R3,R4,RB,MOS                                                     
         EJECT                                                                  
***********************************************************************         
* APPLY CALENDAR AND TYPE FILTERS TO INVOICE STATUS                   *         
* R3=A(INVOICE HEADER DIRECTORY POINTER)                              *         
***********************************************************************         
                                                                                
         USING SNVKEYD,R3                                                       
FLTINV   CLI   DATOPT,DATOCALQ     TEST CALENDAR MONTH OPTION SET               
         JNE   FLTINV02                                                         
         TM    SNVDSTAT+1,SNVMMCMQ YES - MUST BE SET ON INVOICE TOO             
         JZ    FLTINVN                                                          
         J     FLTINV04                                                         
                                                                                
FLTINV02 TM    SNVDSTAT+1,SNVMMCMQ BROADCAST - CALENDAR MUST NOT BE SET         
         JNZ   FLTINVN                                                          
                                                                                
FLTINV04 MVC   FLAG,SNVDSTAT+1     EXTRACT INVOICE INTEGRATION FLAGS            
         NI    FLAG,SNVMMITQ+SNVMMIOQ                                           
                                                                                
         CLI   INTOPT,INTOTIMQ     TEST 'TIME ONLY' REQUEST                     
         JNE   *+12                                                             
         TM    FLAG,SNVMMITQ+SNVMMIOQ                                           
         JNZ   FLTINVN                                                          
                                                                                
         CLI   INTOPT,INTOYESQ     TEST 'INCLUDE INTEGRATION' REQUEST           
         JNE   *+12                                                             
         CLI   FLAG,SNVMMITQ       TEST INCLUDE INTEGRATION INVOICE             
         JNE   FLTINVN                                                          
                                                                                
         CLI   INTOPT,INTOINTQ     TEST 'INTEGRATION ONLY' REQUEST              
         JNE   *+12                                                             
         CLI   FLAG,SNVMMIOQ       TEST INTEGRATION ONLY INVOICE                
         JNE   FLTINVN                                                          
                                                                                
         LR    R0,RE                                                            
         GOTOR FILGETR,NBAIO       READ INVOICE HEADER RECORD                   
         LR    RE,R0                                                            
                                                                                
         L     R1,NBAIO            AND FIND THE MATCHMAKER ELEMENT              
         AHI   R1,SNVELS-SNVKEYD                                                
         SR    R0,R0                                                            
         USING SNVMMELD,R1                                                      
FLTINV06 CLI   SNVMMEL,0           TEST END OF RECORD                           
         JNE   FLTINV08                                                         
**NOOP** LR    R0,RE                                                            
**NOOP** GOTOR LOGERR,MMMERR       YES - PRINT ERROR LOG AND IGNORE             
**NOOP** LR    RE,R0                                                            
         J     FLTINVN                                                          
                                                                                
FLTINV08 CLI   SNVMMEL,SNVMMELQ    TEST MATCHMAKER ELEMENT                      
         JE    *+14                YES - INCLUDE THIS INVOICE                   
         IC    R0,SNVMMLEN         BUMP TO NEXT ELEMENT ON RECORD               
         AR    R1,R0                                                            
         J     FLTINV06                                                         
                                                                                
         ICM   RF,7,AEST+1                                                      
         CLI   LW_TYPE-LW_D(RF),LQ_TALLQ                                        
         JNE   FLTINV10                                                         
         CLI   SNVMMRE1,0          INCLUDE IF REQUESTED FOR EST=NO              
         JE    FLTINVY                                                          
         J     FLTINVN                                                          
                                                                                
FLTINV10 CLI   SNVMMRE1,0          EXCLUDE IF REQUESTED FOR EST=NO              
         JE    FLTINVN                                                          
                                                                                
FLTINVY  CR    RE,RE                                                            
         BR    RE                                                               
FLTINVN  LTR   RE,RE                                                            
         BR    RE                                                               
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* APPLY PRODUCT FILTER TO INVOICE HEADER & DETAIL                     *         
***********************************************************************         
                                                                                
TSTPRI   CLI   PRDNUM,POLNUM       TEST 'POL' PRODUCT                           
         BER   RE                                                               
         CLI   PLKPRDA,0           TEST PRODUCT SET IN HEADER                   
         BER   RE                                                               
         CLI   PRDNUM,LSTNUM       TEST PRODUCT LIST BUILT                      
         JE    TSTPRL                                                           
         CLC   PRDALPH,PLKPRDA     NO - MATCH PRODUCT                           
         BNER  RE                                                               
         CLI   PIGALPH,0           ANY PIGGYBACK?                               
         BER   RE                                                               
         CLC   PIGALPH,PLKPIGA                                                  
         BR    RE                                                               
                                                                                
***********************************************************************         
* APPLY ESTIMATE FILTER                                               *         
***********************************************************************         
                                                                                
TSTEST   CLI   0(R1),0             TEST ESTIMATE IN ELEMENT                     
         BER   RE                                                               
         L     RF,AEST                                                          
         CLI   LW_TYPE-LW_D(RF),LQ_TALLQ                                        
         BER   RE                                                               
         CLI   LW_TYPE-LW_D(RF),LQ_TSINQ                                        
         JNE   TSTEST02                                                         
         CLC   0(1,R1),LW_DATA1-LW_D(RF)                                        
         BR    RE                                                               
                                                                                
TSTEST02 CLI   LW_TYPE-LW_D(RF),LQ_TRNGQ                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),LW_DATA1-LW_D(RF)                                        
         BLR   RE                                                               
         CLC   0(1,R1),LW_DATA1+1-LW_D(RF)                                      
         BHR   RE                                                               
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* APPLY PRODUCT FILTER TO UNIT RECORD                                 *         
***********************************************************************         
                                                                                
TSTPRU   CLI   PRDNUM,POLNUM       TEST 'POL' PRODUCT                           
         BER   RE                                                               
         CLI   PRDNUM,LSTNUM       TEST PRODUCT LIST BUILT                      
         JE    TSTPRL                                                           
         CLC   PRDALPH,PLKPRDA     NO - MATCH ALPHA PRODUCT                     
         BNER  RE                                                               
         CLI   PIGALPH,0           ANY PIGGYBACK?                               
         BER   RE                                                               
         CLC   PIGALPH,PLKPIGA                                                  
         BR    RE                                                               
                                                                                
***********************************************************************         
* FILTER PRODUCT AGAINST PRODUCT LIST (FOR PRODUCT GROUP REQUEST)     *         
***********************************************************************         
                                                                                
TSTPRL   STM   RE,R1,12(RD)                                                     
         SR    R0,R0                                                            
         USING LW_D,R1                                                          
         L     R1,APRDLST                                                       
         ICM   R0,3,LW_NUMN                                                     
         LA    RF,LW_DATA2                                                      
TSTPRL02 CLC   PLKPRDA,0(RF)                                                    
         JE    TSTPRL04                                                         
         AHI   RF,L'PRDALPH                                                     
         BRCT  R0,TSTPRL02                                                      
         CLC   LW_NUMN,=X'0000'     SET CC=NOT EQUAL                            
         DROP  R1                                                               
                                                                                
TSTPRL04 LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET NEXT UNIT RECORD                                     *         
***********************************************************************         
                                                                                
NXTUNT   ST    RE,SAVERE                                                        
         LR    R0,R1               R0=A(KEY DRIVER TABLE)                       
NXTUNT02 GOTOR LP_ASETK,DMCB,(0,(R0)),NBKEY,SAVED,('FF',LP_D)                   
         JH    NXTUNTN                                                          
         MVC   KEY,NBKEY                                                        
         GOTOR DIRHIGH,NBFILUNT    DO READ HIGH FOR THE KEY                     
         MVC   NBKEY,KEY                                                        
         GOTOR LP_ASETK,DMCB,(1,(R0)),NBKEY,SAVED,('FF',LP_D)                   
         JNE   NXTUNT02                                                         
         GOTOR GETUNT,NBKEY+(NUDA-NURECD)                                       
         JNE   NXTUNT02                                                         
         L     RE,SAVERE           SET CC=EQUAL IF UNIT READ                    
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
NXTUNTN  L     RE,SAVERE           SET CC=NOT EQUAL IF ALL DONE                 
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* EDIT PRODUCT NUMBER (CONVERT FROM 1 BYTE NUMBER TO 3 BYTE MNEMONIC) *         
***********************************************************************         
                                                                                
EDTPRD   J     *+12                                                             
         DC    CL8'*EDTPRD*'                                                    
         MVI   LP_ODELM,C'*'       SET ASTERISK AS DELIMITER                    
         SR    R3,R3                                                            
         STCM  R3,15,LP_OLEN                                                    
         L     R2,LP_AINP                                                       
         CLI   0(R2),0                                                          
         JE    EXITY                                                            
         LHI   R3,L'NBSELPRD                                                    
         STCM  R3,15,LP_OLEN                                                    
         L     R4,LP_AOUT                                                       
         GOTOR GETPRC,(R2)                                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'PKEYPRD,R4),PLPMNEM                                          
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SET MIRROR ADJUSTMENT VALUE - R1=A(MIRROR CODE IN UNIT)             *         
***********************************************************************         
                                                                                
         USING NURECD,R3                                                        
SETMIR   CLI   0(R1),C' '          TEST MIRROR CODE PRESENT                     
         BNHR  RE                                                               
         TM    STAFLAG,SMIRRORS    TEST STATION USES MIRRORS                    
         BZR   RE                                                               
         CLC   NUKDATE,MIRRDATE    TEST BEFORE I2A PROFILE DATE                 
         BLR   RE                                                               
                                                                                
         BASR  RF,0                LOOK UP MIRROR CODE IN TABLE                 
         AHI   RF,MIRTAB-*         RF=A(MIRROR CONVERSION TABLE)                
SETMIR02 CLI   0(RF),0             TEST END OF TABLE                            
         BER   RE                                                               
         CLC   0(1,RF),0(R1)       MATCH INPUT TO TABLE                         
         JE    *+12                                                             
         AHI   RF,L'MIRTAB                                                      
         J     SETMIR02                                                         
                                                                                
         MVC   UNTMIROR,1(RF)      SET ADJUSTMENT VALUE                         
         MVC   UNTMIRO2,4(RF)                                                   
         BR    RE                                                               
         DROP  R3                                                               
                                                                                
MIRTAB   DS    0XL7                ** MIRROR ADJUSTMENT TABLE **                
         DC    C'A',PL3'400',PL3'000'                                           
         DC    C'B',PL3'-500',PL3'000'                                          
         DC    C'C',PL3'-1100',PL3'000'                                         
         DC    C'D',PL3'-600',PL3'000'                                          
         DC    C'E',PL3'-900',PL3'000'                                          
         DC    C'F',PL3'300',PL3'000'                                           
         DC    C'G',PL3'700',PL3'000'                                           
         DC    C'H',PL3'-400',PL3'000'                                          
         DC    C'I',PL3'630',PL3'000'                                           
         DC    C'J',PL3'730',PL3'000'                                           
         DC    C'K',PL3'500',PL3'000'                                           
         DC    C'M',PL3'300',PL3'500'                                           
         DC    C'N',PL3'200',PL3'400'                                           
         DC    C'P',PL3'300',PL3'600'                                           
         DC    C'Q',PL3'200',PL3'000'                                           
         DC    C'R',PL3'600',PL3'000'                                           
MIRTABX  DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
* CONVERT 2 BYTE MILITARY TIME (IN R1) TO QUARTER HOUR                *         
***********************************************************************         
                                                                                
MILTQH   STM   RE,R0,12(RD)                                                     
         SR    R0,R0                                                            
         LHI   RE,100                                                           
         DR    R0,RE                                                            
         MHI   R1,MINSHOUR                                                      
         AR    R1,R0                                                            
         CHI   R1,MINSHOUR*6       TEST EARLIER THAN 6AM                        
         JNL   *+8                                                              
         AHI   R1,MINSHOUR*HOURSDAY                                             
         SHI   R1,MINSHOUR*6       SET BASE TO 6AM                              
         SR    R0,R0                                                            
         LHI   RE,MINSHOUR/4                                                    
         DR    R0,RE               R1=QUARTER HOUR VALUE                        
MILTQHX  LM    RE,R0,12(RD)                                                     
         BR    RE                                                               
                                                                                
MINSHOUR EQU   60                                                               
HOURSDAY EQU   24                                                               
         EJECT                                                                  
***********************************************************************         
* DATAMGR INTERFACE - DIRECTORIES                                     *         
***********************************************************************         
                                                                                
DIRHIGH  STC   R1,NBFILE                                                        
         LA    RF,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         J     DIRALL                                                           
                                                                                
DIRREAD  STC   R1,NBFILE                                                        
         LA    RF,DMREAD                                                        
         MVC   KEYSAVE,KEY                                                      
         J     DIRALL                                                           
                                                                                
DIRRSEQ  STC   R1,NBFILE                                                        
         LA    RF,DMRSEQ                                                        
                                                                                
DIRALL   NTR1  LABEL=NO                                                         
         GOTOR SETIOV                                                           
         GOTOR NBDM,DMCB,(DMCI,(RF)),DMDIR,KEY,KEY,0                            
         JE    EXITY                                                            
         TM    8(R1),IOERNF+IOEEOF+IOEDEL                                       
         JNZ   EXITN                                                            
         DC    H'0'                ANYTHING ELSE IS DEAD BAD                    
                                                                                
***********************************************************************         
* DATAMGR INTERFACE - FILES                                           *         
***********************************************************************         
                                                                                
FILGETR  NTR1  LABEL=NO                                                         
         L     R0,0(R1)            R0=A(I/O AREA)                               
         GOTOR SETIOV                                                           
         GOTOR NBDM,DMCB,(DMCI,DMGETR),DMFIL,DMDA,(R0),DMWK                     
         JE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* ROUTINE TO SET VALUES FOR I/O CALL                                  *         
***********************************************************************         
                                                                                
SETIOV   MVC   DMDIR,UNTDIR                                                     
         MVC   DMFIL,UNTFIL                                                     
         MVC   DMDA,KEY+(NUDA-NURECD)                                           
         MVI   DMCI,0                                                           
         CLI   NBFILE,NBFILUNT                                                  
         BER   RE                                                               
         MVC   DMDIR,SPTDIR                                                     
         MVC   DMFIL,SPTFIL                                                     
         MVC   DMDA,KEY+(AGYKDA-AGYHDR)                                         
         CLI   NBFILE,NBFILSPT                                                  
         BER   RE                                                               
         MVC   DMDIR,XSPDIR                                                     
         MVC   DMFIL,XSPFIL                                                     
         MVC   DMDA,KEY+(SNVDDA-SNVKEYD)                                        
         MVI   DMCI,X'80'          SET READ FOR UPDATE                          
         CLI   NBFILE,NBFILXSP                                                  
         BER   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* INTERFACE TO TSAR (ONLINE) OR BUFFERIN (OFFLINE)                    *         
***********************************************************************         
                                                                                
BUFFER   NTR1  LABEL=NO                                                         
         OC    AMASTC,AMASTC       TEST ONLINE                                  
         JNZ   BUFFER02                                                         
         GOTOR TSAR,TSARD          YES - USE TSAR                               
         J     EXIT                                                             
                                                                                
BUFFER02 LHI   R0,BUFFAINI         CONVERT TSAR ACTION CODE TO BUFFERIN         
         CLI   TSACTN,TSAINI                                                    
         JE    BUFFER04                                                         
         LHI   R0,BUFFAPUT                                                      
         CLI   TSACTN,TSAADD                                                    
         JE    BUFFER04                                                         
         CLI   TSACTN,TSAWRT                                                    
         JE    BUFFER04                                                         
         LHI   R0,BUFFASEQ                                                      
         CLI   TSACTN,TSANXT                                                    
         JE    BUFFER04                                                         
         MVC   TKEYSAVE,TKEY                                                    
         LHI   R0,BUFFARDH                                                      
         CLI   TSACTN,TSARDH                                                    
         JE    BUFFER04                                                         
         DC    H'0'                                                             
                                                                                
BUFFER04 GOTOR BUFFERIN,DMCB,((R0),UAIBUF),TRECD,NBACOM                         
         CLI   TSACTN,TSARDH                                                    
         JNE   BUFFER06                                                         
         CLC   TKEYSAVE,TKEY       EMULATE TSAR READ HIGH                       
         JE    BUFFER06                                                         
         OI    BUFFERRS-BUFFPARM(R1),BUFFERNF                                   
                                                                                
BUFFER06 MVC   TSERRS,BUFFERRS-BUFFPARM(R1)                                     
         CLI   TSERRS,0                                                         
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOOK UP PRODUCT CODE/NUMBER IN CLIENT RECORD                        *         
***********************************************************************         
                                                                                
GETPRC   L     RF,ACLTPRD          CLIENT/PRODUCT MAP                           
GETPRC02 OC    0(PLDATAL,RF),0(RF)                                              
         JZ    GETPRAN             DID NOT FIND IT                              
         CLC   L'PLPMNEM(L'PLPNUMB,RF),0(R1)                                    
         JE    *+12                                                             
         AHI   RF,PLDATAL                                                       
         J     GETPRC02                                                         
                                                                                
         MVC   PLPMNEM,0(RF)                                                    
         J     GETPRAY                                                          
                                                                                
GETPRN   L     RF,ACLTPRD          CLIENT/PRODUCT MAP                           
GETPRN02 OC    0(PLDATAL,RF),0(RF)                                              
         JZ    GETPRAN             DID NOT FIND IT                              
         CLC   0(L'PLPMNEM,RF),0(R1)                                            
         JE    *+12                                                             
         AHI   RF,PLDATAL                                                       
         J     GETPRN02                                                         
                                                                                
         MVC   PLPNUMB,L'PLPMNEM(RF)                                            
         J     GETPRAY                                                          
                                                                                
GETPRAN  SR    RF,RF               PRODUCT NOT FOUND                            
                                                                                
GETPRAY  LTR   RF,RF                                                            
         BR    RE                                                               
                                                                                
         EJECT                                                                  
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS TO COME                  
                                                                                
EXITY    LHI   RE,0                CC=EQUAL FOR YES                             
         J     EXITCC                                                           
EXITN    LHI   RE,1                CC=NOT EQUAL FOR NO                          
EXITCC   CHI   RE,0                                                             
                                                                                
EXIT     XIT1  ,                                                                
                                                                                
DUPERR   EQU   0                   DUPLICATE INVOICE/UNIT POINTER               
MMMERR   EQU   1                   MISSING MM ELEMENT ON INVOICE                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP(S)                                                      *         
***********************************************************************         
                                                                                
REQUAI#  EQU   306                                                              
REQUAI   LKREQ H,REQUAI#,OUTUAI    ** UNITS AND INVOICES **                     
DatOp    LKREQ F,1,(D,B#SAVED,DATOPT),(R,VALDOP),COL=*,TEXT=NE#DOPT             
IntOp    LKREQ F,2,(D,B#SAVED,INTOPT),CHAR,COL=*,TEXT=NE#IOPT                   
NetWk    LKREQ F,3,(I,B#SAVED,NWKIND),(R,VALNWK),MAXLEN=L'NBSELNET,    *        
               OLEN=L'NUKNET+L'STAPSTA,COL=*,TEXT=NE#NTWK                       
CltCd    LKREQ F,4,(I,B#SAVED,CLTIND),(R,VALCLT),MAXLEN=L'NBSELCLI,    *        
               OLEN=L'NBSELCL2,COL=*,TEXT=NE#CCOD                               
PrdCd    LKREQ F,5,(I,B#SAVED,PRDIND),(R,VALPRD),MAXLEN=L'NBSELPRD,    *        
               OLEN=L'NBSELPRD,COL=*,TEXT=NE#PCOD                               
PigCd    LKREQ F,6,(I,B#SAVED,PIGIND),(R,VALPRD),MAXLEN=L'NBSELPRD,    *        
               OLEN=L'NBSELPRD,COL=*,TEXT=NE#PIGCD                              
PrdGp    LKREQ F,10,(D,B#SAVED,PGRP),(R,VALPGP),MAXLEN=4,COL=*,        *        
               TEXT=NE#PRDGP                                                    
MOSrv    LKREQ F,7,(I,B#SAVED,DATIND),(R,VALMOS),MAXLEN=8,OLEN=MOSL,   *        
               COL=*,TEXT=NE#MOSV                                               
EstNo    LKREQ F,8,(I,B#SAVED,ESTIND),UBIN,OLEN=L'NBSELEST,RANGE=Y,    *        
               DEFAULT=Y,COL=*,TEXT=NE#ERNG                                     
Comnt    LKREQ F,9,(D,B#SAVED,COMIND),CHAR,COL=*,TEXT=NE#ICOM                   
PkgNo    LKREQ F,11,(I,B#SAVED,PKNIND),UBIN,OLEN=L'NUPACK,LIST=Y,      *        
               RANGE=Y,DEFAULT=Y,COL=*,TEXT=NE#PKGFT                            
InvNo    LKREQ F,12,(D,B#SAVED,INV#),CHAR,COL=*,TEXT=NE#INV#                    
         LKREQ E                                                                
                                                                                
REQONR#  EQU   307                                                              
REQONR   LKREQ H,REQONR#,OUTONR    ** UNITS ORDERED NOT RUN **                  
SDate    LKREQ F,24,(D,B#SAVED,STRDATE),CDAT,COL=*,TEXT=NE#SDATE                
EDate    LKREQ F,25,(D,B#SAVED,ENDDATE),CDAT,COL=*,TEXT=NE#EDATE                
CltCd    LKREQ F,2,(I,B#SAVED,CLTIND),(R,VALCLT),MAXLEN=L'NBSELCLI,    *        
               OLEN=L'NBSELCL2,COL=*,TEXT=NE#CCOD                               
PrdCd    LKREQ F,3,(I,B#SAVED,PRDIND),(R,VALPRD),MAXLEN=L'NBSELPRD,    *        
               DEFAULT=Y,OLEN=L'NBSELPRD,COL=*,TEXT=NE#PCOD                     
PigCd    LKREQ F,11,(I,B#SAVED,PIGIND),(R,VALPRD),MAXLEN=L'NBSELPRD,   *        
               OLEN=L'NBSELPRD,COL=*,TEXT=NE#PIGCD                              
EstNo    LKREQ F,4,(I,B#SAVED,ESTIND),UBIN,OLEN=L'NBSELEST,LIST=NOD,   *        
               RANGE=Y,DEFAULT=Y,COL=*,TEXT=NE#ENRNG                            
NetWk    LKREQ F,5,(I,B#SAVED,NWKIND),CHAR,OLEN=L'NBSELNET,LIST=NOD,   *        
               DEFAULT=Y,COL=*,TEXT=NE#NTWKS                                    
DayFt    LKREQ F,6,(I,B#SAVED,DAYIND),(R,VALDAY),MAXLEN=11,            *        
               OLEN=L'NUKDDAY,DEFAULT=Y,COL=*,TEXT=NE#ROTTN                     
TimFt    LKREQ F,7,(I,B#SAVED,TIMIND),(R,VALTIM),MAXLEN=12,            *        
               OLEN=L'NUKDTIME,DEFAULT=Y,COL=*,TEXT=NE#TIMES                    
PkgFt    LKREQ F,8,(I,B#SAVED,PKNIND),UBIN,OLEN=L'NUPACK,LIST=NOD,     *        
               RANGE=NOT,DEFAULT=Y,COL=*,TEXT=NE#PKGNS                          
DptFt    LKREQ F,9,(I,B#SAVED,DPTIND),(R,VALDPT),MAXLEN=L'NDPTDPTA,    *        
               OLEN=L'NBSELDP,LIST=NOD,DEFAULT=Y,COL=*,TEXT=NE#DPRTS            
PrgFt    LKREQ F,10,(I,B#SAVED,PRGIND),CHAR,OLEN=L'NBSELPRG,LIST=NOD,  *        
               DEFAULT=Y,COL=*,TEXT=NE#PRGMS                                    
SecFt    LKREQ F,13,(I,B#SAVED,LENIND),UBIN,OLEN=L'NULEN,LIST=Y,       *        
               DEFAULT=NOT,COL=*,TEXT=NE#LENFT                                  
CosFt    LKREQ F,23,(D,B#SAVED,COST),CBIN,COL=(*,UC#COST),TEXT=NE#COSTF         
Comnt    LKREQ F,22,(D,B#SAVED,COMIND),CHAR,COL=*,TEXT=NE#ICOM                  
Bill?    LKREQ F,14,,MASK,MASKNOSET=Y,MASKFLIP=Y,MASKBIT=MQ#BILL,      *        
               COL=*,TEXT=NE#NOTBL                                              
Paid?    LKREQ F,15,,MASK,MASKNOSET=Y,MASKFLIP=Y,MASKBIT=MQ#PAID,      *        
               COL=*,TEXT=NE#NOTPD                                              
ADUs?    LKREQ F,16,,MASK,MASKNOSET=Y,MASKFLIP=Y,MASKBIT=MQ#ADUS,      *        
               COL=*,TEXT=NE#NOTAD                                              
PFBs?    LKREQ F,17,,MASK,MASKNOSET=Y,MASKFLIP=Y,MASKBIT=MQ#PFBS,      *        
               COL=*,TEXT=NE#NOTPF                                              
Pmpt?    LKREQ F,18,,MASK,MASKNOSET=Y,MASKFLIP=Y,MASKBIT=MQ#PMPT,      *        
               COL=*,TEXT=NE#PMPTD                                              
Miss?    LKREQ F,19,,MASK,MASKNOSET=Y,MASKFLIP=Y,MASKBIT=MQ#MISS,      *        
               COL=*,TEXT=NE#MISSD                                              
Lock?    LKREQ F,20,,MASK,MASKNOSET=Y,MASKFLIP=Y,MASKBIT=MQ#LOCK,      *        
               COL=*,TEXT=NE#LOCKD                                              
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS                                                         *         
***********************************************************************         
                                                                                
OUTUAI   LKOUT H                   ** UNITS AND INVOICES **                     
                                                                                
PROF     LKOUT R,1                 ** PROFILE VALUES **                         
GR/NT    LKOUT C,1,(D,B#SAVED,PROFA0+0),CHAR,LEN=1,ND=Y                         
MGBIO    LKOUT C,5,(D,B#SAVED,PROFI2X+6),CHAR,LEN=1,ND=Y                        
SSSEQ    LKOUT C,6,(D,B#SAVED,PROFI2X+12),CHAR,LEN=1,ND=Y                       
PSREP    LKOUT C,7,(D,B#SAVED,PROFI2Y+4),CHAR,LEN=1,ND=Y                        
MAPPL    LKOUT C,8,(D,B#SAVED,PROFMK+4),CHAR,LEN=1,ND=Y                         
TLWAY    LKOUT C,11,(D,B#SAVED,PROFI2+0),UBIN,LEN=1,ND=Y                        
TLWYA    LKOUT C,12,(D,B#SAVED,PROFI2X+13),UBIN,LEN=1,ND=Y                      
SEPCK    LKOUT C,15,(D,B#SAVED,PROFI2S+0),CHAR,LEN=1,ND=Y                       
SCPRI    LKOUT C,16,(D,B#SAVED,PROFI2S+1),UBIN,LEN=1,ND=Y                       
SCSEC    LKOUT C,17,(D,B#SAVED,PROFI2S+2),UBIN,LEN=1,ND=Y                       
IFLMD    LKOUT C,18,(D,B#SAVED,PROFTI+10),CHAR,LEN=1,ND=Y                       
ACCFP    LKOUT C,19,(D,B#SAVED,PROFN2+13),CHAR,LEN=1,ND=Y                       
MOIAC    LKOUT C,20,(D,B#SAVED,PROFI2I+0),CHAR,LEN=1,ND=Y                       
MOION    LKOUT C,21,(D,B#SAVED,PROFI2I+1),CHAR,LEN=1,ND=Y                       
CRFMG    LKOUT C,22,(D,B#SAVED,PROFN2+14),CHAR,LEN=1,ND=Y                       
PBSMG    LKOUT C,23,(D,B#SAVED,PROFN1+13),CHAR,LEN=1,ND=Y                       
MGDOE    LKOUT C,24,(D,B#SAVED,PROFN1+14),UBIN,LEN=1,ND=Y                       
MOPKG    LKOUT C,25,(D,B#SAVED,PROFI2Z+10),CHAR,LEN=1,ND=Y,            *        
               PCVERSION=1.0.0.37                                               
ACMCH    LKOUT C,26,(D,B#SAVED,PROFTI+8),CHAR,LEN=1,ND=Y,              *        
               PCVERSION=1.1.0.0                                                
MOSTI    LKOUT C,27,(D,B#SAVED,MOSTIYM),BMON,ND=Y,PCVERSION=1.1.0.0             
MonBB    LKOUT C,28,(D,B#SAVED,PROFI2Z+0),CHAR,LEN=1,ND=Y                       
USEBD    LKOUT C,29,(D,B#SAVED,PROFI2Y+7),CHAR,LEN=1,ND=Y,             *        
               PCVERSION=1.2.0.4                                                
ILTot    LKOUT C,30,(D,B#SAVED,PROFI2Z+5),CHAR,LEN=1,ND=Y,             *        
               PCVERSION=1.4.0.18                                               
AUTI2    LKOUT C,31,(D,B#SAVED,PROFMK+10),CHAR,LEN=1,ND=Y                       
MOREP    LKOUT C,32,(D,B#SAVED,PROFI2A+5),CHAR,LEN=1,ND=Y                       
MOSREP   LKOUT C,33,(D,B#SAVED,MOSI2ARP),BMON,ND=Y                              
EstNO    LKOUT C,34,(D,B#SAVED,PROFI2N+3),CHAR,LEN=1,ND=Y                       
I2Pol    LKOUT C,35,(D,B#SAVED,PROFI2N+6),CHAR,LEN=1,ND=Y                       
I2Est    LKOUT C,36,(D,B#SAVED,PROFI2N+7),CHAR,LEN=1,ND=Y                       
ValAd    LKOUT C,37,(D,B#SAVED,PROFI2A+15),CHAR,LEN=1,ND=Y                      
CkFlght  LKOUT C,38,(D,B#SAVED,PROFI2B+3),CHAR,LEN=1,ND=Y                       
MtchPrd  LKOUT C,39,(D,B#SAVED,PROFI2B+13),CHAR,LEN=1,ND=Y                      
MOSMPrd  LKOUT C,40,(D,B#SAVED,MOSI2BBF),BMON,ND=Y                              
CkCMML   LKOUT C,41,(D,B#SAVED,PROFI2B),CHAR,LEN=1,ND=Y                         
MOSCMML  LKOUT C,42,(D,B#SAVED,MOSI2BCM),BMON,ND=Y                              
CkCMLTim LKOUT C,43,(D,B#SAVED,PROFI2B+6),CHAR,LEN=1,ND=Y                       
MOSCMLTm LKOUT C,44,(D,B#SAVED,MOSI2BCT),BMON,ND=Y                              
CkCMLBrd LKOUT C,45,(D,B#SAVED,PROFI2C+6),CHAR,LEN=1,ND=Y                       
CkCMLLen LKOUT C,46,(D,B#SAVED,PROFI2C+7),CHAR,LEN=1,ND=Y                       
MOSCMLLn LKOUT C,47,(D,B#SAVED,MOSI2CCL),BMON,ND=Y                              
FlmDscr  LKOUT C,48,(D,B#SAVED,PROFI2C+5),CHAR,LEN=1,ND=Y,             *        
               PCVERSION=2.0.0.25                                               
                                                                                
         LKOUT E                                                                
                                                                                
ADDL     LKOUT R,3                 ** ADDITIONAL VALUES **                      
CltNm    LKOUT C,10,(D,B#CLT,CNAME),CHAR                                        
PrdNm    LKOUT C,11,(D,B#SAVED,PRDNAME),CHAR,ND=Y                               
Array    LKOUT C,17,(A,ARYCOM)                                                  
PigNm    LKOUT C,18,(D,B#SAVED,PIGNAME),CHAR,ND=Y                               
Array    LKOUT C,63,(A,ARYFILM),SETEL=Y                                         
Array    LKOUT C,4,(A,ARYPRD),SETEL=Y,PCVERSION=1.1.0.0                         
Array    LKOUT C,5,(A,ARYEST),SETEL=Y                                           
Array    LKOUT C,6,(A,ARYIHD),SETEL=Y                                           
Array    LKOUT C,7,(A,ARYMCH),SETEL=Y                                           
Array    LKOUT C,8,(A,ARYONR),SETEL=Y                                           
Array    LKOUT C,9,(A,ARYRNO),SETEL=Y                                           
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
OUTONR   LKOUT H                   ** UNITS ORDERED NOT RUN **                  
                                                                                
UONR     LKOUT R,8                                                              
Array    LKOUT C,8,(A,ARYUONR),SETEL=Y                                          
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR FILM PRODUCT CODES                             *         
***********************************************************************         
                                                                                
ARYFILM  LKOUT A,(R,NXTFILM),MULTIROW=Y,ROWNAME=FILMVALS                        
FlmCd    LKOUT C,1,(D,B#SAVED,FILMCODE),CHAR                                    
FlmCdAd  LKOUT C,3,(D,B#SAVED,FILMADID),CHAR                                    
FlmCdHD  LKOUT C,4,(D,B#SAVED,FILMHDEF),CHAR                                    
FlmCdCC  LKOUT C,5,(D,B#SAVED,FILMCNTR),CHAR                                    
FlmSeq   LKOUT C,6,(D,B#SAVED,FILMSEQ),UBIN,PCVERSION=2.0.0.0                   
FlmLen   LKOUT C,7,(D,B#SAVED,FILMSLN),UBIN,PCVERSION=2.0.0.0                   
Array    LKOUT C,2,(A,ARYFPRD)                                                  
         LKOUT E                                                                
                                                                                
ARYFPRD  LKOUT A,(D,B#SAVED,FILMPRD),NROWS=FILMPRDX                             
FlmPrd   LKOUT C,2,(D,,FILMPRD),CHAR,ND=Y                                       
         LKOUT E                                                                
                                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR I2 COMMENTS                                    *         
***********************************************************************         
                                                                                
ARYCOM   LKOUT A,(D,B#SAVED,I2COMMNT),NROWS=I2COMMMX                            
I2Com    LKOUT C,17,(D,,I2COMMNT),CHAR,ND=Y                                     
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PRODUCT RECORDS                                *         
***********************************************************************         
                                                                                
ARYPRD   LKOUT A,(R,NXTPRD),MULTIROW=Y,ROWNAME=PRDHDR                           
PCode    LKOUT C,1,(D,,PKEYPRD),CHAR                                            
PName    LKOUT C,2,(D,,PNAME),CHAR                                              
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR ESTIMATE RECORDS                               *         
***********************************************************************         
                                                                                
ARYEST   LKOUT A,(R,GETEST),MULTIROW=Y,ROWNAME=ESTHDR                           
EstNo    LKOUT C,1,(D,,EKEYEST),UBIN                                            
EDesc    LKOUT C,2,(D,,EDESC),CHAR                                              
ESDat    LKOUT C,3,(D,,ESTART),EDAT                                             
EEDat    LKOUT C,4,(D,,EEND),EDAT                                               
Array    LKOUT C,99,(A,ARYEDC)                                                  
OOWSD    LKOUT C,6,(D,,EOWSDAY),UBIN,ND=Y                                       
ELock    LKOUT C,7,(D,B#SAVED,ESTLOCKD),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR ESTIMATE DEMO CATEGORIES                       *         
***********************************************************************         
                                                                                
ARYEDC   LKOUT A,(D,B#SAVED,ESTDEMSN),NROWS=(B#SAVED,ESTNDEMS)                  
DemCd    LKOUT C,5,(D,,ESTDEMSN),CHAR                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE HEADER RECORDS                         *         
***********************************************************************         
                                                                                
ARYIHD   LKOUT A,(R,GETUAI),MULTIROW=Y,ROWNAME=INVVALS                          
InvNo    LKOUT C,1,(D,,INVINVN),CHAR                                            
IBook    LKOUT C,2,(D,,INVI2RBK),CHAR,ND=Y                                      
Elect    LKOUT C,4,(D,,INVEASI),CHAR,ND=Y                                       
ConNo    LKOUT C,5,(D,,INVCNUM),CHAR,ND=Y                                       
Array    LKOUT C,99,(A,ARYFLM)                                                  
PrdCd    LKOUT C,8,(D,,INVPRD),(R,EDTPRD),ND=Y                                  
PrdCd    LKOUT C,8,(D,,INVPRDA),CHAR,ND=Y                                       
PigCd    LKOUT C,9,(D,,INVPIG),(R,EDTPRD),ND=Y                                  
PigCd    LKOUT C,9,(D,,INVPIGA),CHAR,ND=Y                                       
EstNo    LKOUT C,10,(D,,INVEST),UBIN,ND=Y                                       
DskAd    LKOUT C,11,(D,,INVDA),HEXD                                             
I2IOp    LKOUT C,12,(D,,INVI2INT),CHAR,ND=Y                                     
I2Dat    LKOUT C,13,(D,,INVI2DAT),CDAT,ND=Y,PCVERSION=1.0.0.26                  
I2Tim    LKOUT C,14,(D,,INVI2TIM),HEXD,ND=Y,PCVERSION=1.0.0.26                  
PkgNo    LKOUT C,15,(D,,INVPKGNO),LBIN,ND=Y,PCVERSION=1.0.0.37                  
EasiSrc  LKOUT C,16,(D,,INVESRC),CHAR,ND=Y,PCVERSION=2.0.0.1                    
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR FILM CODES                                     *         
***********************************************************************         
                                                                                
ARYFLM   LKOUT A,(D,B#SAVED,FLMNTRY),NROWS=(B#SAVED,INVFLMS),          *        
               ROWWIDTH=FLMNTRYL                                                
FlmCd    LKOUT C,6,(D,,FLMCODE),CHAR                                            
FlmIn    LKOUT C,7,(D,,FLMINV),CHAR,ND=Y                                        
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MATCHED UNITS AND INVOICES                     *         
***********************************************************************         
                                                                                
ARYMCH   LKOUT A,(R,GETMCH),MULTIROW=Y                                          
Array    LKOUT C,8,(A,ARYUNT),FILTROUT=TSTMIRN                                  
Array    LKOUT C,9,(A,ARYINV)                                                   
         LKOUT E                                                                
                                                                                
TSTMIRN  CLI   TKMIRROR,TKMNO      SET CC=EQUAL IF NOT A MIRROR UNIT            
         BR    RE                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR UNMATCHED UNITS (ORDERED NOT RUN)              *         
***********************************************************************         
                                                                                
ARYONR   LKOUT A,(R,GETONR),MULTIROW=Y                                          
Array    LKOUT C,8,(A,ARYUNT)                                                   
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR UNMATCHED INVOICES (RUN NOT ORDERED)           *         
***********************************************************************         
                                                                                
ARYRNO   LKOUT A,(R,GETRNO),MULTIROW=Y                                          
Array    LKOUT C,9,(A,ARYINV)                                                   
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR UNITS ORDERED NOT RUN                          *         
***********************************************************************         
                                                                                
ARYUONR  LKOUT A,(R,GETUONR),MULTIROW=Y                                         
Array    LKOUT C,8,(A,ARYUNT)                                                   
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR MATCHED/UNMATCHED UNITS                        *         
***********************************************************************         
                                                                                
ARYUNT   LKOUT A,(D,B#UNIT,NURECD),NEWEL=Y,NROWS=1                              
EstNo    LKOUT C,1,(D,B#UNIT,NUKEST),UBIN                                       
PkgNo    LKOUT C,2,(D,B#UNIT,NUPACK),UBIN                                       
PrgCd    LKOUT C,3,(D,B#UNIT,NUKPROG),CHAR                                      
PrgNm    LKOUT C,4,(D,B#UNIT,NUPROGNM),CHAR                                     
ADate    LKOUT C,5,(D,B#UNIT,NUKDATE),CDAT                                      
SubNo    LKOUT C,6,(D,B#UNIT,NUKSUB),UBIN,ND=Y                                  
UStat    LKOUT C,7,(D,B#LP,LP_RMASK),HEXD,ND=Y                                  
Array    LKOUT C,99,(A,ARYALC)                                                  
Array    LKOUT C,99,(A,ARYALCA)                                                 
Mtch?    LKOUT C,9,(D,B#SAVED,UNTMATCH),CHAR,ND=Y                               
ACost    LKOUT C,10,(D,B#UNIT,NUACTUAL),CBIN,ND=Y                               
ADays    LKOUT C,11,(D,B#UNIT,NUDAY),UBIN                                       
RDays    LKOUT C,12,(D,B#NETIOD,NBSDROT),UBIN                                   
STime    LKOUT C,13,(D,B#UNIT,NUTIME+0),UBIN,LEN=2                              
ETime    LKOUT C,14,(D,B#UNIT,NUTIME+2),UBIN,LEN=2,ND=Y                         
SecLn    LKOUT C,15,(D,B#UNIT,NULEN),UBIN                                       
Comm1    LKOUT C,16,(D,B#SAVED,UNTCCOM1),CHAR,ND=Y                              
Comm2    LKOUT C,17,(D,B#SAVED,UNTCCOM2),CHAR,ND=Y                              
Array    LKOUT C,99,(A,ARYDEM)                                                  
RepCd    LKOUT C,19,(D,B#UNIT,NUSREP),UBIN,ND=Y                                 
PType    LKOUT C,20,(D,B#NETIOD,NBSURVEY),CHAR,ND=Y                             
Incst    LKOUT C,21,(D,B#NETIOD,NBINTEG),CBIN,ND=Y                              
PStat    LKOUT C,22,(D,B#SAVED,UNTPSTAT),HEXD                                   
Array    LKOUT C,99,(A,ARYTRA),PCVERSION=1.0.0.25                               
Assgn    LKOUT C,24,(D,B#NETIOD,NBASSIGN),CBIN,ND=Y,PCVERSION=1.0.0.26          
SubDP    LKOUT C,26,(D,B#SAVED,UNTSUBDP),CHAR,ND=Y,PCVERSION=1.4.0.18           
Miror    LKOUT C,27,(D,B#SAVED,UNTMIROR),SPAK,ND=Y,PCVERSION=1.4.0.18           
BilBd    LKOUT C,28,(D,B#SAVED,UNTBBD),CHAR,ND=Y                                
BStat    LKOUT C,29,(D,B#SAVED,UNTBSTAT),HEXD                                   
PatRef   LKOUT C,30,(D,B#SAVED,UNTPREF),UBIN,ND=Y,PCVERSION=2.0.0.0             
Miror2   LKOUT C,31,(D,B#SAVED,UNTMIRO2),SPAK,ND=Y,PCVERSION=2.0.0.6            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR UNIT RECORD PRODUCT ARRAY                      *         
***********************************************************************         
                                                                                
ARYALC   LKOUT A,(D,B#SAVED,UNTPRDS),ROWWIDTH=L'NUPRD,EOT=0                     
PrdCd    LKOUT C,8,(D,,UNTPRDS),(R,EDTPRD),LEN=1,OLIST=C,ND=Y                   
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR UNIT RECORD PRODUCT ARRAY                      *         
***********************************************************************         
                                                                                
ARYALCA  LKOUT A,(D,B#SAVED,UNTPRDSA),ROWWIDTH=L'PKEYPRD,EOT=0                  
PRout    LKOUT P,,SETDLM                                                        
PrdCd    LKOUT C,8,(D,,UNTPRDSA),CHAR,OLIST=C,LEN=3,ND=Y                        
         LKOUT E                                                                
SETDLM   LR    R1,R6                                                            
         MVI   LP_ODELM-LP_D(R1),C'*'                                           
         BR    RE                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DEMO VALUES                                    *         
***********************************************************************         
                                                                                
ARYDEM   LKOUT A,(D,B#NETIOD,NDESTDEM),NROWS=(B#SAVED,NDEMOS),         *        
               ROWWIDTH=8,SEQUENCE=C                                            
RGRPs    LKOUT C,18,(D,,NDESTDEM+2),UBIN,LEN=2                                  
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR TRAFFIC FILM CODES                             *         
***********************************************************************         
                                                                                
ARYTRA   LKOUT A,(D,B#SAVED,UNTFILMS),NROWS=UNTFILMN                            
         LKOUT C,23,(D,,UNTFILMS),CHAR,ND=Y                                     
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MATCHED/UNMATCHED INVOICE DETAILS              *         
***********************************************************************         
                                                                                
ARYINV   LKOUT A,(D,B#SAVED,IDTVALS),NROWS=1,NEWEL=Y,ROWWIDTH=IDTVALSL          
Mtch?    LKOUT C,1,(D,,IDTMATCH),CHAR,ND=Y                                      
AffDt    LKOUT C,2,(D,,IDTAFDT),BDAT                                            
AffTm    LKOUT C,3,(D,,IDTAFTM),UBIN                                            
SecLn    LKOUT C,4,(D,,IDTSLN),UBIN                                             
PrdCd    LKOUT C,5,(D,,IDTPRD),(R,EDTPRD),ND=Y                                  
PrdCd    LKOUT C,5,(D,,IDTPRDA),CHAR,ND=Y                                       
PigCd    LKOUT C,6,(D,,IDTPIG),(R,EDTPRD),ND=Y                                  
PigCd    LKOUT C,6,(D,,IDTPIGA),CHAR,ND=Y                                       
GCost    LKOUT C,7,(D,,IDTCST),CBIN,ND=Y                                        
DemVl    LKOUT C,8,(D,,IDTDEMO),UBIN,ND=Y                                       
F1Seq    LKOUT C,9,(D,,IDTFSEQ1),UBIN,ND=Y                                      
F2Seq    LKOUT C,10,(D,,IDTFSEQ2),UBIN,ND=Y                                     
AMkgd    LKOUT C,11,(D,,IDTMKGD),CHAR,ND=Y                                      
InvSq    LKOUT C,12,(D,,IDTISEQ),UBIN                                           
EstNo    LKOUT C,13,(D,,IDTEST),UBIN,ND=Y                                       
EMail    LKOUT C,15,(D,,IDTEMAIL),CHAR,ND=Y                                     
SkpIC    LKOUT C,16,(D,,IDTSKIP),CHAR,ND=Y                                      
RecRl    LKOUT C,17,(D,,IDTRCRL),CHAR,ND=Y                                      
AfKey    LKOUT C,18,(D,,IDTKEY),HEXD                                            
ICost    LKOUT C,19,(D,,IDTINT),CBIN,ND=Y                                       
BilBd    LKOUT C,20,(D,,IDTBBD),CHAR,ND=Y,PCVERSION=1.2.0.7                     
PkgNo    LKOUT C,21,(D,,IDTPKG),UBIN,ND=Y,PCVERSION=1.4.0.15                    
Miror    LKOUT C,22,(D,B#SAVED,IDTMIROR),HDRO,ND=Y,PCVERSION=1.4.0.18           
FlmEr    LKOUT C,23,(D,,IDTFLMER),CHAR,ND=Y                                     
RepCd    LKOUT C,24,(D,,IDTREP),CHAR,ND=Y                                       
PrgNm    LKOUT C,25,(D,,IDTPROG),CHAR,ND=Y                                      
FlmRnge  LKOUT C,26,(D,,IDTFLMRN),CHAR,ND=Y                                     
FlmTime  LKOUT C,27,(D,,IDTFLMTM),CHAR,ND=Y                                     
IgCost   LKOUT C,28,(D,,IDTICOST),CHAR,ND=Y                                     
IgTime   LKOUT C,29,(D,,IDTITIME),CHAR,ND=Y                                     
IgFilm   LKOUT C,30,(D,,IDTIFILM),CHAR,ND=Y                                     
IgLen    LKOUT C,31,(D,,IDTILEN),CHAR,ND=Y                                      
         LKOUT E                                                                
         EJECT                                                                  
         LKARY T                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERAL VALUES **                  
         LTORG                                                                  
                                                                                
NUKEYT   DS    0X                  ** UNIT RECORD KEY DRIVER TABLE **           
         DC    AL2(L'NUKPKEY)                                                   
                                                                                
         DC    AL1(NUKPTYPE-NURECD,L'NUKPTYPE-1)                                
         DC    AL1(NUKPTYPQ,0)                                                  
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(NUKPAM-NURECD,L'NUKPAM-1)                                    
         DC    AL2(NBACTAM-SAVED)                                               
         DC    AL1(LK_ISINQ)                                                    
                                                                                
         DC    AL1(NUKPCLT-NURECD,L'NUKPCLT-1)                                  
         DC    AL2(ACLT+1-SAVED)                                                
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(NUKPNET-NURECD,L'NUKPNET-1)                                  
         DC    AL2(ANWK+1-SAVED)                                                
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(NUKPPROG-NURECD,L'NUKPPROG-1)                                
         DC    AL2(APRG+1-SAVED)                                                
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(NUKPDATE-NURECD,L'NUKPDATE-1)                                
         DC    AL2(STRDATE-SAVED)                                               
         DC    AL1(LK_IRNGQ)                                                    
                                                                                
         DC    AL1(NUKPEST-NURECD,L'NUKPEST-1)                                  
         DC    AL2(AEST+1-SAVED)                                                
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(NUKPSUB-NURECD,L'NUKPSUB-1)                                  
         DC    AL2(SUB-SAVED)                                                   
         DC    AL1(LK_IRNGQ)                                                    
                                                                                
         DC    AL1(NUKPDP-NURECD,L'NUKPDP-1)                                    
         DC    AL2(ADPT+1-SAVED)                                                
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
NUKEYTX  DC    AL1(LK_EOTQ)                                                     
                                                                                
NPKEYT   DS    0X                  ** UNIT RECORD KEY DRIVER TABLE **           
         DC    AL2(L'NUKPKEY)                                                   
                                                                                
         DC    AL1(NUKPTYPE-NURECD,L'NUKPTYPE-1)                                
         DC    AL1(NUKPTYPQ,0)                                                  
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(NUKPAM-NURECD,L'NUKPAM-1)                                    
         DC    AL2(NBACTAM-SAVED)                                               
         DC    AL1(LK_ISINQ)                                                    
                                                                                
         DC    AL1(NUKPCLT-NURECD,L'NUKPCLT-1)                                  
         DC    AL2(ACLT+1-SAVED)                                                
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(NUKPNET-NURECD,L'NUKPNET-1)                                  
         DC    AL2(ANWK+1-SAVED)                                                
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(NUKPPROG-NURECD,L'NUKPPROG-1)                                
         DC    AL2(PRG-SAVED)                                                   
         DC    AL1(LK_IRNGQ)                                                    
                                                                                
         DC    AL1(NUKPDATE-NURECD,L'NUKPDATE-1)                                
         DC    AL2(MOSVALS+(MOSCSTR-MOSD)-SAVED)                                
         DC    AL1(LK_IRNGQ)                                                    
                                                                                
         DC    AL1(NUKPEST-NURECD,L'NUKPEST-1)                                  
         DC    AL2(AEST+1-SAVED)                                                
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(NUKPSUB-NURECD,L'NUKPSUB-1)                                  
         DC    AL2(SUB-SAVED)                                                   
         DC    AL1(LK_IRNGQ)                                                    
                                                                                
         DC    AL1(NUKPDP-NURECD,L'NUKPDP-1)                                    
         DC    AL2(DPT-SAVED)                                                   
         DC    AL1(LK_IRNGQ)                                                    
                                                                                
NPKEYTX  DC    AL1(LK_EOTQ)                                                     
         EJECT                                                                  
PKNFLT   DS    0X                  ** PACKAGE NUMBER FILTER **                  
         DC    AL2(L'NUPACK)                                                    
         DC    AL1(0,L'NUPACK-1)                                                
         DC    AL2(APKN+1-SAVED)                                                
         DC    AL1(LK_IWMPQ)                                                    
         DC    AL1(LK_EOTQ)                                                     
                                                                                
TIMFLT   DS    0X                  ** TIME FILTER **                            
         DC    AL2(L'NUKTIME)                                                   
         DC    AL1(0,L'NUKTIME-1)                                               
         DC    AL2(ATIM+1-SAVED)                                                
         DC    AL1(LK_IWMPQ)                                                    
         DC    AL1(LK_EOTQ)                                                     
                                                                                
DPTFLT   DS    0X                  ** DAYPART FILTER **                         
         DC    AL2(L'NUKDP)                                                     
         DC    AL1(0,L'NUKDP-1)                                                 
         DC    AL2(ADPT+1-SAVED)                                                
         DC    AL1(LK_IWMPQ)                                                    
         DC    AL1(LK_EOTQ)                                                     
                                                                                
LENFLT   DS    0X                  ** SPOT LENGTH FILTER **                     
         DC    AL2(L'NULEN)                                                     
         DC    AL1(0,L'NULEN-1)                                                 
         DC    AL2(ALEN+1-SAVED)                                                
         DC    AL1(LK_IWMPQ)                                                    
         DC    AL1(LK_EOTQ)                                                     
                                                                                
MAP#NEMM DC    AL2(REQUAI#)                                                     
MAP#UONR DC    AL2(REQONR#)                                                     
                                                                                
CMLPIDL  DC    X'0AA1'                                                          
CMLKIDL  DC    X'0A21'                                                          
CMLPRELQ DC    X'20'                                                            
CMLMATQ  DC    X'B0'                                                            
PRGKIDQ  DC    X'0D01'                                                          
DBFNTI   DC    C'NTI'                                                           
POLPRD   DC    C'POL'                                                           
V1100    DC    AL1(1,1,0,0)                                                     
                                                                                
BITLIST  DC    X'8040201008040201'                                              
BLANKS   DC    CL80' '                                                          
EFFS     DC    X'FFFFFFFFFFFFFFFFFFFF'                                          
EZEROES  DC    C'000000'                                                        
EONE     DC    C'01'                                                            
                                                                                
BOOKLAT  DC    C'LAT'                                                           
BOOKACT  DC    C'ACT'                                                           
BOOKNO   DC    C'NO'                                                            
                                                                                
PROGN0   DC    C'S0N0'                                                          
PROGN1   DC    C'S0N1'                                                          
PROGN2   DC    C'S0N2'                                                          
PROGA0   DC    C'S0A0'                                                          
PROGMK   DC    C'S0MK'                                                          
PROGB0   DC    C'S0B0'                                                          
PROGTI   DC    C'S0TI'                                                          
PROGI2   DC    C'S0I2'                                                          
PROGI2A  DC    C'sI2A'                                                          
PROGI2B  DC    C'sI2B'                                                          
PROGI2C  DC    C'sI2C'                                                          
PROGI2I  DC    C'sI2I'                                                          
PROGI2N  DC    C'sI2N'                                                          
PROGI2S  DC    C'sI2S'                                                          
PROGI2X  DC    C'sI2X'                                                          
PROGI2Y  DC    C'sI2Y'                                                          
PROGI2Z  DC    C'sI2Z'                                                          
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
DMRDHI   DC    C'DMRDHI  '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMGETR   DC    C'GETREC  '                                                      
                                                                                
STAFIL   DC    C'STATION'                                                       
                                                                                
FILES    DS    0X                                                               
         DC    C'SPOT   '          SYSTEM/FILE LIST                             
                                                                                
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
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'X'                                                             
                                                                                
LVALUES  DS    0X                  ** LITERAL VALUES (SEE SVALUES) **           
         DC    X'000000000000',X'FFFFFFFFFFFF'                                  
         DC    X'00',X'C0'                                                      
         DC    X'00',X'FF'                                                      
                                                                                
UAIBUF   BUFFD TYPE=D,KEYLEN=TKEYL,COMLEN=TDATAL,BUFFERS=20                     
         EJECT                                                                  
***********************************************************************         
* SYSTEM FACILITIES LIST                                              *         
***********************************************************************         
                                                                                
FACS     DS    0XL(RFACTABL)       ** SYSTEM FACILITIES **                      
         DC    AL1(QNETVALU),AL2(0,FNETVALU-FACD)                               
         DC    AL1(QDEMOCON),AL2(0,FDEMOCON-FACD)                               
         DC    AL1(QDEMADDR),AL2(CDEMADDR-COMFACSD,0)                           
         DC    AL1(QDEMOUT),AL2(CDEMOUT-COMFACSD,0)                             
         DC    AL1(QDEMEL),AL2(CDEMEL-COMFACSD,0)                               
         DC    AL1(QDEMAINT),AL2(CDEMAINT-COMFACSD,0)                           
         DC    AL1(QDEMAND),AL2(CDEMAND-COMFACSD,0)                             
         DC    AL1(QDEMOMTH),AL2(CDEMOMTH-COMFACSD,0)                           
         DC    AL1(QDEMOVAL),AL2(CDEMOVAL-COMFACSD,0)                           
         DC    AL1(QDDISP),AL2(CT00AD0-COMFACSD,0)                              
         DC    AL1(QCLPACK),AL2(0,FCLPACK-FACD)                                 
         DC    AL1(QCLUNPK),AL2(0,FCLUNPK-FACD)                                 
         DC    AL1(QGETBRD),AL2(0,FGETBRD-FACD)                                 
         DC    AL1(QTSAR),AL2(0,FTSAR-FACD)                                     
         DC    AL1(QSTAPACK),AL2(0,FSTAPACK-FACD)                               
         DC    AL1(QTIMVAL),AL2(0,FTIMVAL-FACD)                                 
         DC    AL1(QDAYVAL),AL2(0,FDAYVAL-FACD)                                 
         DC    AL1(QTRPACK),AL2(0,FTRPACK-FACD)                                 
         DC    AL1(RFACEOTQ)                                                    
                                                                                
ABENDS   DC    C'SCHT:'                                                         
SLOWS    DC    C'SCHT:'                                                         
                                                                                
SAVE     DS    0X                  ** TWA SAVE DATA DEFINITION **               
         DC    AL1(B#SAVED),AL2(MMSAVE-SAVED,L'MMSAVE)                          
         EJECT                                                                  
MQ#ACOV  EQU   47                  ASSIGNED COST OVERRIDE                       
MQ#PRAF  EQU   48                  PRODUCT ALLOCATION FROZEN                    
MQ#BILL  EQU   49                  BILLED                                       
MQ#PAID  EQU   50                  PAID                                         
MQ#ADUS  EQU   51                  ADU UNIT                                     
MQ#PFBS  EQU   52                  PFB UNIT                                     
MQ#PMPT  EQU   53                  PRE-EMPTED                                   
MQ#MISS  EQU   54                  MISSED                                       
MQ#LOCK  EQU   55                  LOCKED                                       
MQ#WDOW  EQU   56                  WINDOW                                       
MQ#BBRD  EQU   57                  BILLBOARD                                    
MQ#FRZN  EQU   58                  FROZEN                                       
MQ#MKGD  EQU   59                  MAKE-GOOD                                    
MQ#MGDR  EQU   60                  MAKE-GOOD DEMO RETREIVAL                     
MQ#ESTS  EQU   61                  ESTIMATE (NOT ADU OR PFB)                    
MQ#AUDO  EQU   62                  AUDIT ON                                     
MQ#ACTI  EQU   63                  ACTUAL COST INPUT                            
MQ#ASSI  EQU   64                  ASSIGNED COST INPUT                          
                                                                                
FF       EQU   X'FF'                                                            
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
SPACEQ   EQU   C' '                                                             
ALL      EQU   C'ALL'                                                           
                                                                                
NETSYSQ  EQU   3                   NETPAK SYSTEM NUMBER                         
NETMEDQ  EQU   C'N'                NETWORK MEDIA LETTER                         
                                                                                
COMMPCT  EQU   15                  COMMISSION PERCENTAGE                        
LSTNUM   EQU   254                 PRODUCT LIST (SPECIAL)                       
POLNUM   EQU   255                 'POL' PRODUCT NUMBER                         
MAXDEMS  EQU   21                  MAXIMUM N'DEMOS IN ESTIMATE HEADER           
MAXUPRD  EQU   6                   MAXIMUM N'PRODUCTS PER UNIT                  
ONETHOU  EQU   1000                MAXIMUM RECORD LENGTH SUPPORTED              
                                                                                
IOEDEL   EQU   X'02'               RECORD IS DELETED                            
IOERNF   EQU   X'10'               RECORD NOT FOUND                             
IOEEOF   EQU   X'80'               END OF FILE                                  
                                                                                
FACD     DSECT                     ** SERVER FACILITIES **                      
FNETVALU DS    A                   A(NETVALUE)                                  
FDEMOCON DS    A                   A(DEMOCON)                                   
FCLPACK  DS    A                   A(CLPACK)                                    
FCLUNPK  DS    A                   A(CLUNPK)                                    
FGETBRD  DS    A                   A(GETBROAD)                                  
FTSAR    DS    A                   A(TSAR)                                      
FSTAPACK DS    A                   A(STAPACK)                                   
FTIMVAL  DS    A                   A(TIMVAL)                                    
FDAYVAL  DS    A                   A(DAYVAL)                                    
FTRPACK  DS    A                   A(TRPACK)                                    
         EJECT                                                                  
SAVED    DSECT                     ** SERVER SAVED W/S **                       
                                                                                
ANXTPRD  DS    A                   A(NEXT PRODUCT NUMBER/CODE)                  
                                                                                
MMSAVE   DS    XL(MMSAVEL)         MMSAVE AREA IF OFFLINE                       
MOSVALS  DS    XL(MOSL)            MOS VALUES                                   
                                                                                
SVALUES  DS    0D                  ** LITERAL VALUES (SEE LVALUES) **           
PRG      DS    XL(L'NUKPPROG*2)    X'00...00',X'FF...FF'                        
SUB      DS    XL(L'NUKPSUB*2)     X'00',X'C0'                                  
DPT      DS    XL(L'NUKPDP*2)      X'00',X'FF'                                  
SVALUEL  EQU   *-SVALUES                                                        
                                                                                
FACLIST  DS    0A                                                               
NETVALUE DS    A                   A(NETVALUE)                                  
DEMOCON  DS    A                   A(DEMOCON)                                   
CLPACK   DS    A                   A(CLPACK)                                    
CLUNPK   DS    A                   A(CLUNPK)                                    
GETBRD   DS    A                   A(GETBROAD)                                  
TSAR     DS    A                   A(TSAR)                                      
STAPACK  DS    A                   A(STAPACK)                                   
TIMVAL   DS    A                   A(TIMVAL)                                    
DAYVAL   DS    A                   A(DAYVAL)                                    
TRPACK   DS    A                   A(TRPACK)                                    
FACLISTL EQU   *-FACLIST                                                        
                                                                                
BUFFERIN DS    A                   A(BUFFERIN)                                  
PRTLOG   DS    A                   A(PRTLOG)                                    
GETPROF  DS    A                   A(GETPROF)                                   
DATCON   DS    A                   A(DATCON)                                    
ADDAY    DS    A                   A(ADDAY)                                     
DATVAL   DS    A                   A(DATVAL)                                    
GETDAY   DS    A                   A(GETDAY)                                    
PERVAL   DS    A                   A(PERVAL)                                    
                                                                                
ACPRINT2 DS    A                   A(CPRINT2)                                   
ACOMFACS DS    A                   A(COMFACS)                                   
AMASTC   DS    A                   A(MASTC)                                     
                                                                                
SAVECLR  DS    0X                  ** START OF CLEARED AREA **                  
                                                                                
KEY      DS    XL64                                                             
KEYSAVE  DS    XL64                                                             
                                                                                
TSARBLK  DS    XL(TSARDL)          TSAR BLOCK                                   
TSARREC  DS    XL(TRECL)           TSAR RECORD                                  
TKEYSAVE DS    XL(TKEYL)           TSAR RECORD KEY SAVE AREA                    
                                                                                
LASTS    DS    0X                  ** LAST TIME VALUES **                       
LASTEST  DS    XL(L'EKEYEST)       LAST ESTIMATE NUMBER READ                    
LASTINV  DS    CL(L'SNVKINV)       LAST INVOICE SENT                            
LASTISEQ DS    XL2                 LAST INVOICE HEADER SEQUENCE NUMBER          
LASTDSEQ DS    XL2                 LAST INVOICE DETAIL SEQUENCE NUMBER          
LASTSL   EQU   *-LASTS                                                          
                                                                                
MOSTIVAL DS    0XL(L'MOSTI+L'MOSTIYM)                                           
MOSTI    DS    XL2                 MOS FROM TI PROFILE OR ZEROES                
MOSTIYM  DS    XL2                 MOS (Y/M) FROM TI PROFILE                    
                                                                                
MOSI2ARP DS    XL2                 MOS ON REP CODE                              
MOSI2BBF DS    XL2                 MOS FOR MATCH ON CMML PRD                    
MOSI2BCM DS    XL2                 MOS FOR CMML PRD                             
                                                                                
MIRRDATE DS    XL2                 MIRROR UNIT START DATE                       
MIRSTDTE DS    XL2                 MIRROR UNIT START DATE                       
MIRENDTE DS    XL2                 MIRROR UNIT END DATE                         
MOSI2BCF DS    XL2                 MOS COMML FLIGHT                             
MOSI2BCT DS    XL2                 MOS COMML TIME                               
MOSI2CCL DS    XL2                 MOS COMML LENGTH                             
                                                                                
STA      DS    XL(L'SNVKSTA)       STATION (STAPACK VERSION)                    
NDEMOS   DS    XL2                 N'DEMOS IN ESTIMATE HEADER                   
STAMEDIA DS    CL(L'STYPE)         NETWORK MEDIA FROM STATION MASTER            
STAFLAG  DS    XL(L'SFLAG1)        STATION FLAG                                 
                                                                                
PROFA0   DS    CL16                A0 PROFILE                                   
PROFMK   DS    CL16                MK PROFILE                                   
PROFB0   DS    CL16                B0 PROFILE                                   
PROFTI   DS    CL16                TI PROFILE                                   
PROFI2   DS    CL16                I2 PROFILE                                   
PROFI2A  DS    CL16                I2A PROFILE                                  
PROFI2B  DS    CL16                I2B PROFILE                                  
PROFI2C  DS    CL16                I2C PROFILE                                  
PROFI2I  DS    CL16                I2I PROFILE                                  
PROFI2N  DS    CL16                I2N PROFILE                                  
PROFI2S  DS    CL16                I2S PROFILE                                  
PROFI2X  DS    CL16                I2X PROFILE                                  
PROFI2Y  DS    CL16                I2Y PROFILE                                  
PROFI2Z  DS    CL16                I2Z PROFILE                                  
PROFN0   EQU   NBUSER              N0 PROFILE                                   
PROFN1   EQU   NBUSER1             N1 PROFILE                                   
PROFN2   EQU   NBUSER2             N2 PROFILE                                   
                                                                                
PRDALPH  DS    CL(L'PKEYPRD)       PRODUCT ALPHA                                
PRDNUM   DS    X                   PRODUCT NUMBER                               
PRDNAME  DS    CL(L'PNAME)         PRODUCT NAME                                 
                                                                                
PIGALPH  DS    CL(L'PKEYPRD)       PIGGYBACK PRODUCT ALPHA                      
PIGNUM   DS    X                   PIGGYBACK PRODUCT NUMBER                     
PIGNAME  DS    CL(L'PNAME)         PIGGYBACK PRODUCT NAME                       
                                                                                
ESTCOUNT DS    X                   ESTIMATE COUNTER                             
ESTVALS  DS    0X                  ** ESTIMATE VALUES **                        
ESTSTDTB DS    XL2                 ESTIMATE START DATE (BINARY)                 
ESTNDDTB DS    XL2                 ESTIMATE END DATE (BINARY)                   
ESTNDEMS DS    XL2                 ACTUAL NUMBER OF DEMOS                       
ESTLOCKD DS    C                   C'Y' IF LOCKED                               
ESTDEMSN DS    (MAXDEMS)CL11       DEMO NAME LIST                               
ESTVALSL EQU   *-ESTVALS                                                        
                                                                                
INVVALS  DS    0X                  ** INVOICE HEADER VALUES **                  
INVINDS  DS    X                   ** INDICATOR BYTE **                         
INVIWANT EQU   X'80'               INVOICE HEADER WANTED                        
INVIEOFR EQU   X'40'               END OF FILE REACHED                          
INVDA    DS    XL(L'DMDA)          DISK ADDRESS OF INVOICE HEADER               
INVCTL   DS    XL(L'SNVHDCTL)      INVOICE HEADER CONTROL                       
INVEST   DS    XL(L'SNVHDEST)      ESTIMATE NUMBER OR ZERO                      
INVSDT   DS    XL(L'SNVHDSDT)      INVOICE HEADER START DATE                    
INVESDT  DS    CL6                 INVOICE HEADER START DATE (EBCDIC)           
INVPRD   DS    XL(L'SNVHDPRD)      PRODUCT CODE                                 
INVPRDA  DS    XL(L'SNVHDAP1)      PRODUCT CODE                                 
INVPIG   DS    XL(L'SNVHDPR2)      PIGGYBACK PRODUCT CODE                       
INVPIGA  DS    XL(L'SNVHDAP2)      PIGGYBACK PRODUCT CODE                       
INVINVN  DS    CL(L'SNVKINV)       INVOICE NUMBER                               
INVEASI  DS    C                   C'Y'=EASI SOURCE                             
INVCNUM  DS    CL(L'SNVHDCON)      CONTRACT NUMBER                              
INVFLMS  DS    XL2                 N'FILM TRANSLATION ENTRIES                   
INVI2DAT DS    XL(L'SNVMMDAT)      I2 RUN DATE                                  
INVI2TIM DS    XL(L'SNVMMTIM)      I2 RUN TIME                                  
INVI2RBK DS    CL6                 I2 REQUEST BOOK                              
INVI2INT DS    C                   I2 REQUEST INTEGRATION OPTION                
INVPKGNO DS    XL(L'SNVHDPKG)      PACKAGE NUMBER                               
INVESRC  DS    CL(L'SNVHDEZS)      EASI SOURCE                                  
INVVALSL EQU   *-INVVALS                                                        
                                                                                
FILMVALS DS    0X                                                               
FILMCODE DS    XL12                FILM CODE                                    
FILMADID DS    XL12                FILM CODE AD-ID                              
FILMHDEF DS    XL12                FILM CODE HI-DEF                             
FILMCNTR DS    XL12                FILM CODE CENTER CUT                         
FILMSEQ  DS    XL(L'CMLSEQ)        FILM SEQUENCE NUMBER                         
FILMSLN  DS    XL(L'CMLSLN)        FILM CMML LENGTH                             
FILMPRDX EQU   28                  MAXIMUM N'FILM PRODUCTS                      
FILMPRD  DS    (FILMPRDX)CL3       PRODUCT CODE                                 
FILMPRDL EQU   *-FILMPRD                                                        
FILMVALX EQU   *-FILMVALS                                                       
                                                                                
UNTVALS  DS    0X                  ** EXTRACTED BINARY UNIT VALUES **           
UNTPSTAT DS    X                   ** PAID STATUS **                            
UNTPSTQ  EQU   X'01'               TIME PAID                                    
UNTPSIQ  EQU   X'02'               INTEGRATION PAID                             
UNTPSOQ  EQU   X'04'               OTHER CHARGES PAID                           
UNTBSTAT DS    X                   ** BILL STATUS **                            
UNTBSTQ  EQU   X'01'               TIME BILLED                                  
UNTBSIQ  EQU   X'02'               INTEGRATION BILLED                           
UNTBSSQ  EQU   X'08'               SPECIAL CHARGES PAID                         
UNTMIROR DS    PL3                 MIRROR ADJUSTMENT VALUE                      
UNTMIRO2 DS    PL3                 MIRROR ADJUSTMENT VALUE 2                    
UNTBBD   DS    C                   C'Y'=BILLBOARD                               
************UNTPREF  DS    XL(L'NUCMLREF)      PATTERN REF #                    
UNTPREF  DS    XL(L'NUCMLR3F)      PATTERN REF #                                
UNTVALL  EQU   *-UNTVALS                                                        
                                                                                
UNTCHARD DS    0C                  ** EXTRACTED ALPHA UNIT VALUES **            
UNTMATCH DS    C                   C'Y'=MATCHED UNIT                            
UNTCCOM1 DS    CL60                CLIENT COMMENT LINE 1                        
UNTCCOM2 DS    CL60                CLIENT COMMENT LINE 2                        
                                                                                
UNTFILMS DS    0CL12               ** TRAFFIC FILM CODES **                     
UNTFILM1 DS    CL12                TRAFFIC FILM CODE 1                          
UNTFILM2 DS    CL12                TRAFFIC FILM CODE 2 (PIGGYBACK)              
UNTFILMN EQU   (*-UNTFILMS)/L'UNTFILMS                                          
                                                                                
UNTSUBDP DS    CL2                 SUB-DAYPART FROM X'60' ELEM                  
                                                                                
UNTCHARL EQU   *-UNTCHARD                                                       
                                                                                
UNTPRDS  DS    XL(MAXUPRD+1)       PRODUCT LIST FROM UNIT RECORD                
UNTPRDSA DS    XL((MAXUPRD*L'NUPDEPR)+1)  PRODUCT LIST FROM UNIT RECORD         
                                                                                
IDTVALS  DS    0X                  ** INVOICE DETAIL VALUES **                  
IDTMATCH DS    C                   C'Y'=MATCHED INVOICE                         
IDTKEY   DS    XL(L'TDINVDA+L'TDINVDSP+L'TDINVCSM)                              
IDTAFDT  DS    XL3                 AFFIDAVIT DATE (YYMMDD) BINARY               
IDTAFTM  DS    XL2                 AFFIDAVIT TIME (MILITARY)                    
IDTSLN   DS    XL(L'SNVIDSLN)      SECONDS LENGTH                               
IDTPRD   DS    XL(L'SNVIDPRD)      PRODUCT CODE                                 
IDTPRDA  DS    XL(L'SNVIDAP1)      PRODUCT CODE                                 
IDTPIG   DS    XL(L'SNVIDPR2)      PIGGYBACK PRODUCT CODE                       
IDTPIGA  DS    XL(L'SNVIDAP2)      PIGGYBACK PRODUCT CODE                       
IDTCST   DS    XL(L'SNVIDCST)      GROSS COST                                   
IDTINT   DS    XL(L'SNVIDINT)      INTEGRATION COST                             
IDTDEMO  DS    XL4                 DEMO VALUE                                   
IDTFSEQ1 DS    XL(L'SNVIDCML)      FILM#1 SEQUENCE NUMBER                       
IDTFSEQ2 DS    XL(L'SNVIDCM2)      FILM#2 SEQUENCE NUMBER                       
IDTISEQ  DS    XL(L'TDINVSEQ)      INVOICE SEQUENCE NUMBER                      
IDTEST   DS    XL(L'SNVIDEST)      ESTIMATE NUMBER                              
IDTPKG   DS    XL(L'SNVIDPKG)      PACKAGE NUMBER                               
IDTMKGD  DS    C                   C'Y'=AFFIDAVIT IS A MAKEGOOD                 
IDTEMAIL DS    C                   C'Y'=E-MAIL HAS BEEN SENT                    
IDTSKIP  DS    C                   C'Y'=SKIP INTERVAL CHECKING                  
IDTRCRL  DS    C                   C'Y'=DATE OUTSIDE RECALL/RELEASE             
IDTBBD   DS    C                   C'Y'=BILLBOARD                               
IDTMIROR DS    C                   C'Y'=MIRROR                                  
IDTFLMER DS    C                   C'Y'=NO MATCH ON FILM PRODUCT                
IDTFLMRN DS    C                   C'Y'=OUT OF FLIGHT DATE RANGE                
IDTFLMTM DS    C                   C'Y'=OUT OF FLIGHT TIME RANGE                
IDTMOS   DS    XL(L'SNVHDSDT)      INVOICE MOS                                  
IDTKSEQ  DS    XL(L'SNVCMSEQ)      FILM#1 TRAFFIC SEQUENCE NUMBER               
IDTREP   DS    XL(L'SNVHDREP)      REP CODE                                     
IDTPROG  DS    XL(L'SNVIDUPG)      PROGRAM CODE                                 
IDTICOST DS    C                   C'Y'=IGNORE COST                             
IDTITIME DS    C                   C'Y'=IGNORE TIME                             
IDTIFILM DS    C                   C'Y'=IGNORE FILM                             
IDTILEN  DS    C                   C'Y'=IGNORE LENGTH                           
IDTFLMRD DS    XL(L'CMLRCL)        FILM RECALL DATE                             
IDTVALSL EQU   *-IDTVALS                                                        
                                                                                
REQVALS  DS    0F                  ** REQUEST VALUE POINTERS **                 
CLTIND   DS    0X                  CLIENT INDICATOR                             
ACLT     DS    A                   A(CLIENT)                                    
PRDIND   DS    0X                  PRODUCT INDICATOR                            
APRD     DS    A                   A(PRODUCT)                                   
PIGIND   DS    0X                  PIGGYBACK PRODUCT INDICATOR                  
APIG     DS    A                   A(PIGGYBACK PRODUCT)                         
ESTIND   DS    0X                  ESTIMATE INDICATOR                           
AEST     DS    A                   A(ESTIMATE(S))                               
NWKIND   DS    0X                  NETWORK INDICATOR                            
ANWK     DS    A                   A(NETWORK)                                   
DATIND   DS    0X                  DATE INDICATOR                               
ADAT     DS    A                   A(DATE(S))                                   
DAYIND   DS    0X                  DAY INDICATOR                                
ADAY     DS    A                   A(DAY(S))                                    
PRGIND   DS    0X                  PROGRAM INDICATOR                            
APRG     DS    A                   A(PROGRAM(S))                                
TIMIND   DS    0X                  TIME INDICATOR                               
ATIM     DS    A                   A(TIMES)                                     
PKNIND   DS    0X                  PACKAGE NUMBER INDICATOR                     
APKN     DS    A                   A(PACKAGE NUMBER(S))                         
LENIND   DS    0X                  LENGTH INDICATOR                             
ALEN     DS    A                   A(LENGTH(S))                                 
DPTIND   DS    0X                  DAYPART INDICATOR                            
ADPT     DS    A                   A(DAYPART(S))                                
                                                                                
INDS     DS    X                   ** INDICATOR BYTE **                         
INDS2CDP EQU   X'80'               2 CHARACTER DAYPARTS IN USE                  
                                                                                
STRDATE  DS    XL2                 START DATE                                   
ENDDATE  DS    XL2                 END DATE                                     
                                                                                
INV#     DS    CL(L'SNVKINV)       INVOICE NUMBER                               
                                                                                
COMIND   DS    C                   INCLUDE COMMENTS INDICATOR                   
                                                                                
PGRP     DS    XL(L'PRGPID+L'PRGPGRP) PRODUCT GROUP CODE                        
                                                                                
COSTFILT DS    C                   C'Y' IF COST FILTER ENTERED                  
COST     DS    XL4                 COST FILTER                                  
REQVALSL EQU   *-REQVALS                                                        
                                                                                
DATOPT   DS    C                   DATE OPTION                                  
DATOCALQ EQU   C'C'                CALENDAR DATES                               
                                                                                
INTOPT   DS    C                   INTEGRATION OPTION                           
INTOTIMQ EQU   0                   TIME ONLY (DEFAULT)                          
INTOINTQ EQU   C'O'                INTEGRATION ONLY                             
INTOYESQ EQU   C'I'                INCLUDE INTEGRATION                          
                                                                                
PRDLST   DS    (CPLDMAXN+1)X       LIST OF PRODUCTS                             
PRDLSTN  DS    AL1                 NUMBER OF PRODUCTS IN LIST                   
                                                                                
APRDLST  DS    A                   PRODUCT WMP ENTRY                            
ACLTPRD  DS    A                   TABLE FOR ALL CLIENT PRODUCTS                
INVDMDA  DS    XL4                                                              
INVREP   DS    XL(L'SNVHDREP)      INVOICE REP CODE                             
                                                                                
PLKUP    DS    0X                  ** PRODUCT CODE LOOKUP **                    
PLKPRDA  DS    CL3                 PRODUCT MNEMONIC LOOKUP                      
PLKPRD   DS    XL1                 PRODUCT NUMBER LOOKUP                        
PLKPIGA  DS    CL3                 PRODUCT MNEMONIC LOOKUP                      
PLKPIG   DS    XL1                 PRODUCT NUMBER LOOKUP                        
PLKUPL   EQU   *-PLKUP             L'DATA ENTRY                                 
                                                                                
*NENETIOD                                                                       
NETIOD   DS    0D                                                               
       ++INCLUDE NENETIOD                                                       
NBFILXSP EQU   C'X'                                                             
                                                                                
*NETDEMOT                                                                       
       ++INCLUDE NETDEMOT                                                       
                                                                                
*DEDBLOCK                                                                       
       ++INCLUDE DEDBLOCK                                                       
                                                                                
I2COMMMX EQU   15                  MAXIMUM N'I2 COMMENTS                        
I2COMMNT DS    (I2COMMMX)CL80      I2 COMMENTS                                  
I2COMMLN EQU   *-I2COMMNT                                                       
                                                                                
ESTTAB   DS    0X                  ** ESTIMATE TABLE **                         
ESTTNUM  DS    XL(L'EKEYEST)       ESTIMATE NUMBER                              
ESTTDEMO DS    XL(L'EDEMLIST)      PRIMARY DEMO                                 
ESTTSTAT DS    XL1                 ESTIMATE STATUS                              
*                                  X'80'-STEWARDSHIP ESTIMATE                   
ESTTABL  EQU   *-ESTTAB                                                         
ESTTABN  EQU   255                 MAXIMUM N'ESTIMATE TABLE ENTRIES             
         DS    (ESTTABN)XL(ESTTABL)                                             
                                                                                
IO1      DS    XL(ONETHOU)                                                      
IO2      DS    XL(ONETHOU*2)                                                    
IO3      DS    XL(ONETHOU*2)                                                    
*IO4      DS    XL(ONETHOU*4)                                                   
                                                                                
NETBLKL  EQU   *-NETBLOCK                                                       
                                                                                
FLMSAVE  DS    0X                  ** FILM SAVE AREA **                         
                                                                                
FLMNTRY  DS    0X                  ** TABLE ENTRY **                            
FLMCODE  DS    XL(L'SNVCMAID)      FILM CODE                                    
FLMINV   DS    C                   C'Y'=FILM CODE INVALID                       
FLMSEQN  DS    XL(L'SNVCMSEQ)      TRAFFIC SYSTEM SEQUENCE NUMBER               
FLMNTRYL EQU   *-FLMNTRY           LENGTH OF AN ENTRY                           
                                                                                
         ORG   FLMNTRY                                                          
FLMMAXN  EQU   255                 MAXIMUM N'FILM ENTRIES                       
         DS    (FLMMAXN)XL(FLMNTRYL)                                            
FLMSAVEL EQU   *-FLMSAVE                                                        
                                                                                
         ORG   FLMSAVE                                                          
FLMDATES DS    0X                  ** FILM DATES **                             
FLMDSEQ  DS    XL(L'SNVCMSEQ)      TRAFFIC SYSTEM FILM SEQUENCE NUMBER          
FLMDREL  DS    XL(L'CMLRLSE)       FILM RELEASE DATE                            
FLMDRCL  DS    XL(L'CMLRCL)        FILM RECALL DATE                             
FLMDATEL EQU   *-FLMDATES          LENGTH OF AN ENTRY                           
                                                                                
SAVECLRL EQU   *-SAVECLR                                                        
                                                                                
MOSD     DSECT                     ** MONTH OF SERVICE VALUES **                
MOSCSTR  DS    XL2                 MONTH OF SERVICE COMPRESSED START            
MOSCEND  DS    XL2                 MONTH OF SERVICE COMPRESSED END              
MOSC     DS    XL2                 MONTH OF SERVICE (COMPRESSED YYMMDD)         
MOSB     DS    XL2                 MONTH OF SERVICE (BINARY YYMM)               
MOSESTR  DS    CL6                 MONTH OF SERVICE (EBCDIC YYMMDD)             
MOSEEND  DS    CL6                 MONTH OF SERVICE (EBCDIC YYMMDD)             
MOSL     EQU   *-MOSD                                                           
                                                                                
WORKD    DSECT                     ** SERVER LOCAL W/S **                       
DUB      DS    D                                                                
DMCB     DS    6F                                                               
HALF     DS    H                                                                
SAVER2R3 DS    2F                                                               
SAVERE   DS    A                                                                
SAVERE2  DS    A                                                                
SAVEKEY  DS    XL(L'SNVKEY)                                                     
WORK     DS    XL256                                                            
WORK2    DS    XL256                                                            
FLAG     DS    X                                                                
INVFLAG  DS    X                                                                
INVFLNET EQU   X'01'               INVOICE ENTERED WITH NET AMOUNT              
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      RUNNER/DDLINK MODE                           
                                                                                
DMDIR    DS    CL8                                                              
DMFIL    DS    CL8                                                              
DMDA     DS    XL4                                                              
DMWK     DS    XL48                                                             
DMCI     DS    XL1                                                              
                                                                                
PLDATA   DS    0X                  ** PRODUCT CODE LIST **                      
PLPMNEM  DS    CL3                 PRODUCT MNEMONIC                             
PLPNUMB  DS    XL1                 PRODUCT NUMBER                               
PLDATAL  EQU   *-PLDATA            L'DATA ENTRY                                 
                                                                                
IO4      DS    XL(ONETHOU*6)       EXPAND UNIT FILE TO HANDLE COMSCORE          
                                                                                
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
TRECD    DSECT                     ** TSAR RECORD LAYOUT **                     
                                                                                
TKEY     DS    0X                  ** RECORD KEY **                             
                                                                                
TKTYPE   DS    XL1                 RECORD TYPE                                  
TKTUNTQ  EQU   1                   MATCHED/UNMATCHED UNIT RECORD                
TKTRNOQ  EQU   2                   UNMATCHED INVOICE DETAIL RECORD              
                                                                                
*                                  ** FOR UNMATCHED INVOICE DETAILS **          
TKINVSEQ DS    XL2                 UNMATCHED INVOICE HEADER SEQUENCE#           
TKDETSEQ DS    XL2                 UNMATCHED INVOICE DETAIL SEQUENCE#           
                                                                                
*                                  ** FOR MATCHED/UNMATCHED UNITS **            
         ORG   TKINVSEQ                                                         
TKEST    DS    XL(L'NUKPEST)       ESTIMATE NUMBER                              
TKDATE   DS    CL(L'NUKDATE)       AIR DATE                                     
TKTIME   DS    CL(L'NUKTIME)       AIR TIME                                     
TKPROG   DS    CL(L'NUKPPROG)      PROGRAM CODE                                 
TKDP     DS    XL(L'NUKPDP)        DAYPART CODE                                 
TKSUB    DS    XL(L'NUKPSUB)       SUB-LINE NUMBER                              
TKMIRROR DS    X                   MIRROR FLAG                                  
TKMNO    EQU   0                   NOT A MIRROR UNIT                            
TKMIR1   EQU   1                   MIRROR UNIT (PHANTOM)                        
TKMIR2   EQU   2                   MIRROR UNIT (PHANTOM)                        
TKEYL    EQU   *-TKEY              KEY LENGTH                                   
                                                                                
TDATA    DS    0X                  ** RECORD DATA **                            
                                                                                
TDINVDA  DS    XL(L'DMDA)          INVOICE DISK ADDRESS OR ZERO                 
TDINVDSP DS    XL2                 DISPLACEMENT TO ELEMENT OR ZERO              
TDINVCSM DS    XL4                 INVOICE CHECK SUM                            
TDINVSEQ DS    XL2                 INVOICE SEQUENCE NUMBER                      
TDINVCTL DS    XL(L'INVCTL)        INVOICE HEADER CONTROL                       
TDINVSDT DS    XL(L'INVSDT)        INVOICE HEADER START DATE OR ZERO            
TDINVPRD DS    XL(L'INVPRD)        PRODUCT NUMBER                               
TDINVPRA DS    XL(L'INVPRDA)       PRODUCT NUMBER                               
TDINVPIG DS    XL(L'INVPIG)        PIGGYBACK PRODUCT NUMBER                     
TDINVPIA DS    XL(L'INVPIGA)       PIGGYBACK PRODUCT NUMBER                     
TDINVEST DS    XL(L'INVEST)        ESTIMATE NUMBER                              
TDINVFT1 DS    XL(L'FLMSEQN)       FILM#1 TRAFFIC SEQUENCE NUMBER               
TDINVFT2 DS    XL(L'FLMSEQN)       FILM#2 TRAFFIC SEQUENCE NUMBER               
TDINVFTN EQU   (*-TDINVFT1)/L'TDINVFT1                                          
TDUNTDA  DS    XL(L'DMDA)          UNIT DISK ADDRESS OR ZERO                    
TDUNTINT DS    XL(L'NBINTEG)       UNIT INTEGRATION COST                        
TDINVREP DS    XL(L'SNVHDREP)      INVOICE REP CODE                             
                                                                                
TDATAL   EQU   *-TDATA             DATA LENGTH                                  
                                                                                
TRECL    EQU   *-TRECD             TOTAL RECORD LENGTH                          
                                                                                
* INCLUDED DSECTS FOLLOW                                                        
       ++INCLUDE NAVDSECTS                                                      
         PRINT ON                                                               
       ++INCLUDE NENAVMMD                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDDPRINTL                                                      
         ORG   P+13                                                             
ERRMESS  DS    0C                                                               
       ++INCLUDE DDLINKD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDRUNNERD                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDWRKIOD                                                       
       ++INCLUDE FATABSRUN                                                      
       ++INCLUDE NEDDEQUS                                                       
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE NEGENUNIT                                                      
                                                                                
NUKSTRAQ EQU   X'C1'               FIRST TRAFFIC SUB-LINE NUMBER                
                                                                                
NUCOMELQ EQU   X'04'                                                            
NUPDELQ  EQU   X'19'                                                            
NUCMLEQ  EQU   X'21'                                                            
NUBKELEQ EQU   X'5D'                                                            
NUOTELQ  EQU   X'60'                                                            
*                                  ** PACKAGE STATUS (NUPACKST) **              
NUPAFRZQ EQU   X'80'               FROZEN                                       
NUPALOKQ EQU   X'20'               LOCKED                                       
NUPADNPQ EQU   X'10'               DO NOT PRINT                                 
NUPANCIQ EQU   X'04'               NON-COM INTEG                                
NUPAAIOQ EQU   X'02'               AUDIT IS ON                                  
NUPANSDQ EQU   X'01'               NO-SHOW DELETE                               
*                                  ** UNIT STATUS (NUUNITST) **                 
NUUNMINQ EQU   X'80'               MINUS  UNIT                                  
NUUNPREQ EQU   X'40'               PRE-EMPT                                     
NUUNACTQ EQU   X'20'               ACTUAL COST INPUT                            
NUUNASNQ EQU   X'10'               AUTO SEED NTI                                
NUUNACIQ EQU   X'08'               ASSIGNED COST INPUT                          
NUUNPFBQ EQU   X'04'               PFB                                          
NUUNMISQ EQU   X'02'               MISSED                                       
NUUNMKGQ EQU   X'01'               MAKE-GOOD                                    
*                                  ** UNIT STATUS 2 (NUUNST2) **                
NUUNGHDQ EQU   X'80'               GET HUTS FROM DEMO FILE                      
NUUN52WQ EQU   X'40'               52 WEEK HUT CALENDAR                         
NUUNPAFQ EQU   X'20'               PRODUCT ALLOCATION FROZEN                    
NUUNLFAQ EQU   X'10'               LENGTH FROZEN FOR ALLOCATION                 
NUUNUCUQ EQU   X'08'               UNIT HAS CABLE UNIVERSES                     
NUUNP1ZQ EQU   X'04'               1ST PRD HAS ZERO ALLOCATION                  
NUUNPUPQ EQU   X'02'               PUP UNIT                                     
NUUNMGDQ EQU   X'01'               MAKE-GOOD DEMO RETRIEVAL                     
*                                  ** UNIT STATUS (NUSDST3) **                  
NUSDACOQ EQU   X'80'               ASSIGNED COST OVERRIDE                       
NUSDUCSQ EQU   X'40'               UNIT IS A COPY SPLIT                         
NUSDSSNQ EQU   X'20'               SEED SEEDED NTI                              
NUSDPUPQ EQU   X'10'               UNIT WAS CREATED IN PUP                      
NUSDWUBQ EQU   X'08'               WINDOW BUY USE ALT BOOK                      
NUSDACUQ EQU   X'04'               UNIT ADD BY CABLE UPLOAD                     
NUSDADUQ EQU   X'02'               ADU UNIT (COST NOT ALLOWED)                  
NUSDFPDQ EQU   X'01'               FEED PCT. HITS IMPS AND RTGS                 
*                                  ** FLAGS (NUCMLFLG) **                       
NUCULCIQ EQU   X'80'               UNIT LEN CHANGED, ELEM INVALID               
NUCUPCIQ EQU   X'40'               UNIT PRD CHANGED, ELEM INVALID               
NUCDCHAQ EQU   X'20'               UNIT DATE CHANGED                            
NUCUPOIQ EQU   X'10'               THIS UNIT PRINTED ON INSTR                   
NUCUDTDQ EQU   X'08'               UNIT DELETED BY TRAFFIC DEPT                 
NUCBREQQ EQU   X'04'               BILLBOARD REQUIRED (SET IN BUY)              
NUCTDPIQ EQU   X'02'               TRAFFIC DEL/MISSED PRINTED ON INSTR          
NUCIPPIQ EQU   X'01'               INVERT PRINTING PRODUCTS ON INSTR            
*                                  ** OTHER TYPE (NUOTTYP) **                   
NUOTAUTQ EQU   C'A'                AUTHORIZATION CODE                           
NUOTCAGQ EQU   C'B'                COMMISSION AGENCY                            
NUOTRCDQ EQU   C'R'                REASON CODE                                  
NUOTSOFQ EQU   C'S'                SOURCE OF FUNDS                              
NUOTTAGQ EQU   C'T'                TRAFFIC AGENCY                               
NUOTPCDQ EQU   C'P'                PROMOTION CODE                               
NUOTTMZQ EQU   C'Z'                TIME ZONE                                    
NUOTPOSQ EQU   C'O'                POSITION CODE                                
NUOTECSQ EQU   C'E'                ESTIMATED COST                               
NUOTSDPQ EQU   C'D'                SUB-DAYPART                                  
NUOTPACQ EQU   C'G'                PIGGYBACK ALLOWANCE CODE                     
NUOTWABQ EQU   C'W'                WINDOWS ALTERNATE BOOK                       
NUOTCOMQ EQU   C'C'                COMMENT                                      
NUOTMCDQ EQU   C'M'                MIRROR CODE                                  
NUOTBTCQ EQU   C'F'                BUY TYPE CODE                                
NUOTTSPQ EQU   C'H'                TRAFFIC SUPPLIER                             
NUOTMLTQ EQU   C'L'                MULTIRUN                                     
NUOTUSDQ EQU   C'U'                USER DEFINED SUB-DAYPART                     
NUOTCBOQ EQU   C'I'                COMBO CODE (AAA999)                          
NUOTAIRQ EQU   C'V'                AIRING                                       
*                                  ** COMMENT TYPE (NUCOMTYP) **                
NUCOMTCQ EQU   C'C'                CLIENT                                       
NUCOMTIQ EQU   C'I'                INTERNAL                                     
*                                                                               
CMLPRDQ  EQU   X'20'               NON ALPHA PRODUCT LIST                       
CMLPRDAQ EQU   X'29'               ALPHA PRODUCT LIST                           
CMLDTAEQ EQU   X'10'               CMML DATA ELEMENT                            
CMLADIDQ EQU   X'A0'               AD-ID                                        
CMLXDTEQ EQU   X'24'               EXTENDED ELEMENT                             
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE SPGENEST                                                       
         ORG   EKEYEST+L'EKEYEST                                                
EKEYZERO DS    XL5                                                              
         ORG                                                                    
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENXCOM                                                      
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPTRCMML                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034NENAV23   09/15/20'                                      
         END                                                                    
