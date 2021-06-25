*          DATA SET NENAV13    AT LEVEL 044 AS OF 11/20/19                      
*PHASE T31813B                                                                  
*INCLUDE NETBLRDR                                                               
*INCLUDE NETNET                                                                 
NENAV13  TITLE '- NETPAK STEWARD - UNITS && MFM DOWNLOADS'                      
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,FACS=FACS,        *        
               ABENDLIST=ABENDS,SLOWLIST=SLOWS,WORKERKEY=NEUD,         *        
               SERVERTYPE=TSTSTEW,SEGMENT=Y,SYSTEM=3,APPEND=Y,         *        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#NETIOD,NETIOD,    *        
               B#UNIT,NURECD,B#LP,LP_D,B#MEDTAB,MEDTABD),TYPE=D,       *        
               SYSPHASE=X'0318'                                                 
                                                                                
***********************************************************************         
* NETWORK SERVER CODE                                                 *         
***********************************************************************         
                                                                                
CODE     NMOD1 WORKL,**NN13*,CLEAR=YES,RR=RE                                    
         USING WORKD,RC                                                         
         BASR  RA,0                                                             
         AHI   RA,GLOBALS-*                                                     
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         USING LP_D,R1                                                          
         L     RF,LP_ARUNP                                                      
         USING RUNPARMD,RF         RF=A(RUNPARMS)                               
         SR    R9,R9                                                            
         ICM   R9,7,RUNPARUN                                                    
         USING RUNFACSD,R9         R9=A(RUNFACS)                                
         L     R8,RSVRSAVE                                                      
         USING SAVED,R8            R8=A(SERVER SAVE AREA)                       
         MVC   CALLMODE,RUNPMODE   SAVE CALLING MODE                            
         ST    RB,ABASE            SAVE SERVER ENTRY POINT                      
         ST    R1,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,RELO             SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   CALLMODE,RRUNSTRQ   TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
         GOTOR INIWRK              INITIALIZE RUN VALUES                        
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   CALLMODE,RPRCWRKQ   TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         GOTOR ININET              INITIALIZE NETBLOCK VALUES                   
         TM    LP_FLAG,LP_FOFFL                                                 
         JNZ   EXITY                                                            
         GOTOR INIAGY              INITIALIZE AGENCY IF ONLINE                  
         GOTOR GETLIC              GET COMSCORE LICENSE                         
         J     EXITY                                                            
                                                                                
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   CALLMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
         TM    LP_FLAG,LP_FOFFL    ISSUE FLUSH FOR ALL FILES OFFLINE            
         BZ    RUNREQ01                                                         
         GOTOR NBDM,DMCB,DMKEY,UNTDIR,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,UNTFIL,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,SPTDIR,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,SPTFIL,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,XSPDIR,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,XSPFIL,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,GENDIR,(4,0),0                                   
                                                                                
         GOTOR INIAGY              AND INITIALIZE AGENCY VALUES                 
         GOTOR GETLIC              GET COMSCORE LICENSE                         
                                                                                
RUNREQ01 SR    RF,RF                                                            
         ICM   RF,7,ACLT                                                        
         JZ    RUNREQ02                                                         
         AHI   RF,LW_LN1Q                                                       
         CLI   0(RF),0                                                          
         JE    RUNREQ02                                                         
         MVC   REQCLT,0(RF)        SAVE OFF REQUEST CLIENT                      
                                                                                
         L     R1,ALP                                                           
         MVC   PRDINDA,PRDIND                                                   
                                                                                
         L     R7,LP_AWMP          GET SPACE FOR DUMMY ENTRY                    
         STCM  R7,7,APRDA                                                       
         CLI   PRDINDA,LW_TSINQ    SINGLE ENTRY                                 
         JNE   RUNSCR10                                                         
         SR    R2,R2                                                            
         ICM   R2,7,APRD                                                        
         CLI   LW_LN1Q(R2),POLPRDN     POL REQUEST?                             
         JE    RUNSCR15                TREAT THIS AS ALL                        
         SR    RF,RF                                                            
         ICM   RF,7,APRD           COPY APRD TO APRDA                           
         SR    RE,RE                                                            
         ICM   RE,7,APRDA                                                       
         LA    R1,LW_LN1Q+L'PRDALPH-1                                           
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         LA    R7,1(R1,R7)         BUMP TO NEXT WMP ENTRY                       
         J     RUNSCR30                                                         
                                                                                
         USING LW_D,R7                                                          
RUNSCR10 CLI   PRDINDA,LW_TALLQ    ALL ENTRY                                    
         JNE   RUNSCR20                                                         
RUNSCR15 SR    RF,RF                                                            
         ICM   RF,7,APRD           COPY APRD TO APRDA                           
         SR    RE,RE                                                            
         ICM   RE,7,APRDA                                                       
         MVC   0(LW_LN1Q,RE),0(RF)                                              
         MVI   PRDINDA,LW_TALLQ    TRUE ALL                                     
         MVI   LW_TYPE,LW_TALLQ    TRUE ALL                                     
         MVI   LW_NUMN+1,2                                                      
         MVC   LW_DATA2(L'PRDALPH*2),=X'000000FFFFFF'                           
         AHI   R7,LW_LN2Q+(L'PRDALPH*2)                                         
         J     RUNSCR30                                                         
                                                                                
RUNSCR20 CLI   PRDINDA,LW_TLSTQ    LIST?                                        
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         SR    RF,RF                                                            
         ICM   RF,7,APRD           COPY APRD TO APRDA                           
         SR    R1,R1                                                            
         ICM   R1,3,LW_NUMN-LW_D(RF)  # OF ENTRIES                              
         MHI   R1,L'PRDALPH                                                     
         AHI   R1,LW_LN2Q-1                                                     
                                                                                
         SR    RE,RE                                                            
         ICM   RE,7,APRDA                                                       
                                                                                
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         LA    R7,1(R1,R7)         BUMP TO NEXT WMP ENTRY                       
RUNSCR30 L     R1,ALP                                                           
         ST    R7,LP_AWMP                                                       
                                                                                
RUNSCR40 SR    R7,R7                                                            
         ICM   R7,7,APRD                                                        
         CLI   PRDINDA,LW_TALLQ    ALL REQUEST?                                 
         JE    RUNREQ02                                                         
                                                                                
         SR    R3,R3                                                            
         AHI   R3,1                DEFAULT TO SINGLE ENTRY                      
         TM    LW_LN1Q-L'LW_TYPE(R7),LW_TSINQ    SINGLE ENTRY?                  
         JZ    *+12                                                             
         AHI   R7,LW_LN1Q                                                       
         J     RUNSCR50                                                         
         ICM   R3,3,LW_NUMN        # OF ENTRIES                                 
         AHI   R7,LW_LN2Q                                                       
                                                                                
RUNSCR50 CLI   1(R7),0             BINARY PRODUCT EQUATE?                       
         JE    *+10                                                             
         XC    0(L'PRDALPH,R7),0(R7)    CLEAR OUT ALPHA                         
         AHI   R7,L'PRDALPH                                                     
         JCT   R3,RUNSCR50                                                      
                                                                                
         SR    R7,R7                                                            
         ICM   R7,7,APRDA                                                       
                                                                                
         SR    R3,R3                                                            
         AHI   R3,1                DEFAULT TO SINGLE ENTRY                      
         TM    LW_LN1Q-L'LW_TYPE(R7),LW_TSINQ      SINGLE ENTRY?                
         JZ    *+12                                                             
         AHI   R7,LW_LN1Q                                                       
         J     RUNSCR60                                                         
         ICM   R3,3,LW_NUMN        # OF ENTRIES                                 
         AHI   R7,LW_LN2Q                                                       
                                                                                
RUNSCR60 CLI   1(R7),0             BINARY PRODUCT EQUATE?                       
         JNE   RUNSCR70                                                         
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(L'NUPRD),0(R7)                                              
         GOTOR GETPRC,WORK                                                      
         MVC   0(L'PRDALPH,R7),PRDALPH                                          
                                                                                
RUNSCR70 AHI   R7,L'PRDALPH                                                     
         JCT   R3,RUNSCR60                                                      
         DROP  R7                                                               
                                                                                
RUNREQ02 L     R1,ALP                                                           
         MVC   MAP,LP_QMAPN        SET INPUT MAP NUMBER                         
         MVC   VERSION,LP_VRSN1    SET VERSION NUMBER                           
         GOTOR INIREQ              INITIALIZE REQUEST VALUES                    
         JNE   EXITY                                                            
                                                                                
         CLC   MAP,MAPUNIT         TEST UNIT DOWNLOAD                           
         BE    RUNREQ10                                                         
         CLC   MAP,MAPMFMM         TEST MFM MEDIA DOWNLOAD                      
         BE    RUNREQ10                                                         
                                                                                
         CLC   MAP,MAPMFMC         TEST MFM CLIENT DOWNLOAD                     
         BE    *+14                                                             
         CLC   MAP,MAPMFMP         TEST MFM PRODUCT DOWNLOAD                    
         BNE   RUNREQ06                                                         
                                                                                
         L     R2,NBAAGY           SET BINARY AGENCY/MEDIA VALUE                
         AHI   R2,AGYEL-AGYHDR                                                  
         SR    R0,R0                                                            
         USING AGYMEDEL,R2                                                      
RUNREQ04 IC    R0,AGYMEDLN                                                      
         AR    R2,R0                                                            
         CLI   AGYMEDEL,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   AGYMEDEL,AGYMEDEQ                                                
         BNE   RUNREQ04                                                         
         CLI   AGYMEDCD,NETMEDQ                                                 
         BNE   RUNREQ04                                                         
         MVC   AGYMED,AGYMEDBT                                                  
         DROP  R2                                                               
                                                                                
         CLC   MAP,MAPMFMP         TEST MFM PRODUCT DOWNLOAD                    
         BNE   RUNREQ10                                                         
         GOTOR BLDCLT              BUILD CLIENT TABLE                           
         JNE   EXITY                                                            
         B     RUNREQ10                                                         
                                                                                
RUNREQ06 CLC   MAP,MAPMFMV         TEST MFM VENDOR DOWNLOAD                     
         BNE   RUNREQ08                                                         
         GOTOR BLDMED              BUILD MEDIA TABLE                            
         JNE   EXITY                                                            
         B     RUNREQ10                                                         
                                                                                
RUNREQ08 CLC   MAP,MAPSTAT         TEST STATS DOWNLOAD                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING CFMIOD,CFMIOC       BUILD CFMIO CONTROL BLOCK                    
         LA    R0,CFMIOD                                                        
         LHI   R1,CFMIOL                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   CFMSYS,NETMEDQ                                                   
         MVC   CFMABFIN,RBUFFRIN                                                
         L     RF,RSYSFACS                                                      
         MVC   CFMATSAR,FTSAR-FACD(RF)                                          
         MVC   CFMAGBRD,FGETBRD-FACD(RF)                                        
         MVC   CFMAGRAT,VNETNET                                                 
         MVC   CFMACOM,RCOMFACS                                                 
         MVC   CFMAIO,NBAIO                                                     
         MVC   CFMSUMOP,SUMOPTN                                                 
         MVC   CFMCALOP,CALOPTN                                                 
         MVC   CFMSPLOP,SPLOPTN                                                 
         MVC   CFMPNTOP,PNTOPTN                                                 
         MVC   CFMESTOP,ESTOPTN                                                 
         MVC   CFMEDTOP,ESNOPTN                                                 
         BASR  RE,0                                                             
         AHI   RE,CNVMED-*                                                      
         STCM  RE,15,CFMACNVM                                                   
         BASR  RE,0                                                             
         AHI   RE,VALCFC-*                                                      
         STCM  RE,15,CFMAVALC                                                   
         BASR  RE,0                                                             
         AHI   RE,VALCFV-*                                                      
         STCM  RE,15,CFMAVALV                                                   
         MVC   CFMALP,ALP                                                       
         MVC   CFMAMED,MEDIND                                                   
         MVC   CFMADVN,ADVIND                                                   
         MVC   CFMVENN,SUPNODE                                                  
         MVC   CFMSTDTC,STRDATE                                                 
         MVC   CFMENDTC,ENDDATE                                                 
         MVC   CFMACBA,BRDIND                                                   
         MVI   CFMACTN,CFMAINI                                                  
         GOTOR VCFMIO,CFMIOD                                                    
         JNE   EXITY                                                            
                                                                                
RUNREQ10 L     R1,ALP                                                           
         GOTOR LP_APUTO            CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* GET UNIT RECORDS                                                    *         
***********************************************************************         
                                                                                
GETUNT   LR    R6,R1                                                            
         USING LP_D,R6                                                          
         MVC   LP_ADATA,NBAIO                                                   
*                                                                               
         XC    CSINFO(CSINFOLQ),CSINFO                                          
*                                                                               
         BRAS  RE,VALDEMN                                                       
         BRAS  RE,GETUCOD          GET UNIVERSE CUT OFF DATE                    
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   GETUNT02                                                         
         CLI   RECORD,RECGOAL      TEST REQUEST FOR GOALS ONLY                  
         BE    GETUNTX                                                          
         XC    NBKEY,NBKEY         CLEAR KEY                                    
                                                                                
         USING NBLBILLD,BILBLOCK   FOR NETBLRDR                                 
GETUNT02 GOTOR LP_ASETK,DMCB,(0,AKEYTAB),NBKEY,SAVED,('FF',ALP)                 
         BH    GETUNTX                                                          
         GOTOR HIGH,NBFILUNT       DO READ HIGH FOR THE KEY                     
         GOTOR LP_ASETK,DMCB,(1,AKEYTAB),NBKEY,SAVED,('FF',ALP)                 
         BNE   GETUNT02                                                         
         LA    R3,NBKEY                                                         
         USING NURECD,R3                                                        
         CLC   LGETCLT,NUKCLT      TEST CHANGE OF CLIENT                        
         BE    GETUNT04                                                         
                                                                                
         L     R0,AESTTAB          YES - CLEAR ESTIMATE TABLE                   
         LHI   R1,ESTTABL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR RESCLT,NUKCLT       RESOLVE CLIENT VALUES                        
         BE    GETUNT04                                                         
         SR    R1,R1               BAD CLIENT - SKIP TO NEXT                    
         ICM   R1,B'0011',NUKCLT                                                
         AHI   R1,1                                                             
         ICM   R1,B'1100',NUKTYPE                                               
         XC    NUKEY,NUKEY                                                      
         STCM  R1,B'1111',NUKEY                                                 
         B     GETUNT02                                                         
         DROP  R3                                                               
                                                                                
GETUNT04 GOTOR FLTUNT              APPLY FILTERS TO UNIT RECORD                 
         BNE   GETUNT02                                                         
         GOTOR GETNTI              RESOLVE NBNTISTA                             
*                                                                               
         MVI   NBWHERE,NBWNETIO    CALL NETVALUE TO GET UNIT VALUES             
         OI    NBSBKEND,NBNODPT2   SET DON'T READ DAYPART RECORDS               
         TM    DEMOFLAG,DEMOFUSR   TEST ANY USER DEMOS REQUESTED                
         BZ    *+8                                                              
         GOTOR SETUSR              SET USER DEMO NAMES FOR NETVALUE             
                                                                                
         LA    R3,RFLAVOR2                                                      
         ST    R3,ARFLVOR2                                                      
         OC    RFLAVOR2,SPACES                                                  
                                                                                
         XC    TMPDVALS(TMPDVALQ),TMPDVALS                                      
         MVI   IOAREA,FF                                                        
                                                                                
         CLI   RFLAVOR2,SPACEQ     TEST SIDE BY SIDE DEMO REQUEST               
         BNE   GETUNT06            YES                                          
                                                                                
         MVI   RFLAVOR2,RFL2EST    DEFAULT TO ESTIMATES                         
         MVI   RFLAVOR2+1,YESQ     W/ GUARANTEES                                
                                                                                
         CLI   RFLAVOR,RFLAACT     ACTUALS W/ GUARANTEE?                        
         BNE   *+12                                                             
         MVI   RFLAVOR2,RFL2ACT    ACTUALS                                      
         MVI   RFLAVOR2+1,YESQ     W/ GUARANTEES                                
                                                                                
         CLI   RFLAVOR,RFLANOG     ESTIMATES W/ NO GUARANTEE?                   
         BNE   *+12                                                             
         MVI   RFLAVOR2,RFL2EST    ESTIMATES                                    
         MVI   RFLAVOR2+1,NOQ      NO GUARANTEES                                
                                                                                
GETUNT06 L     R3,ARFLVOR2                                                      
         CLI   0(R3),SPACEQ                                                     
         BE    GETUNT20                                                         
                                                                                
         CLI   0(R3),RFL2EST       ESTIMATE?                                    
         BNE   *+12                                                             
         MVI   NBESTOPT,NBESTOUQ                                                
         MVI   NBACTOPT,0                                                       
                                                                                
         CLI   0(R3),RFL2ACT       ACTUALS?                                     
         BNE   *+12                                                             
         MVI   NBESTOPT,0                                                       
         MVI   NBACTOPT,YESQ                                                    
                                                                                
         CLI   0(R3),RFL2EAC       ESTIMATE & ACTUALS?                          
         BNE   *+12                                                             
         MVI   NBESTOPT,NBESTOUQ                                                
         MVI   NBACTOPT,YESQ                                                    
                                                                                
         MVI   NBDEMRAW,0          W/ GUARANTEES (DEFAULT)                      
         OI    NBINDS9,NBI9GDM                                                  
         CLI   1(R3),YESQ          W/ GUARANTEES?                               
         BE    *+12                                                             
         MVI   NBDEMRAW,YESQ       NO - ONLY GET RAW VALUES W/O GUAR            
         NI    NBINDS9,X'FF'-NBI9GDM                                            
                                                                                
         NI    NBINDS9,X'FF'-NBI9SLDV  SHOW LOCKED DEMO VALUES?                 
         CLI   TMPUSER3,C'Y'                                                    
         JNE   *+8                                                              
         OI    NBINDS9,NBI9SLDV                                                 
                                                                                
GETUNT08 LA    R1,NDNTDMS                                                       
         L     RF,NBEXTEND                                                      
         USING NBXTNDD,RF                                                       
         ST    R1,NBXCDNL          A(COMSCORE DEMO NAME LIST)                   
         DROP  RF                                                               
                                                                                
         BRAS  RE,UNIVLKUP         UNIVERSE ONLY LOOKUP?                        
         JE    EXITY                                                            
                                                                                
         GOTOR NBNETVAL,DMCB,NETIOD                                             
                                                                                
         CLI   0(R3),RFL2EAC       ESTIMATE & ACTUALS?                          
         BE    GETUNT20                                                         
                                                                                
         CLI   IOAREA,FF           1ST PASS?                                    
         BE    GETUNT12                                                         
                                                                                
* IF REQUESTED TWO DEMOS SIDE BY SIDE THEN THE EST DEMOS AREA WILL              
* HAVE THE 1ST DEMO REQUEST AND THE ACT DEMOS AREA WILL HAVE THE 2ND            
                                                                                
         CLI   0(R3),RFL2EST                                                    
         BNE   GETUNT10                                                         
         MVC   NBACTSHR,NBESTSHR   2ND PASS, MOVE VALUES TO 2ND SPOT            
         MVC   NBACTHUT,NBESTHUT                                                
         MVC   NDACTHOM(L'NDACTDEM),NDESTHOM                                    
         MVC   NBESTSHR,TMPSHR                                                  
         MVC   NBESTHUT,TMPHUT                                                  
         MVC   NDESTHOM(L'NDESTDEM),IOAREA                                      
         B     GETUNT18                                                         
                                                                                
GETUNT10 CLI   0(R3),RFL2ACT                                                    
         BNE   GETUNT18                                                         
         MVC   NBESTSHR,TMPSHR     2ND PASS, SINCE ACTS ALREADY IN 2ND          
         MVC   NBESTHUT,TMPHUT     SPOT, JUST RESTORE 1ST SPOT                  
         MVC   NDESTHOM(L'NDESTDEM),IOAREA                                      
         B     GETUNT16                                                         
                                                                                
GETUNT12 CLI   0(R3),RFL2EST       1ST DEMO, SAVE VALUES                        
         BNE   GETUNT14                                                         
         MVC   TMPSHR,NBESTSHR                                                  
         MVC   TMPHUT,NBESTHUT                                                  
         MVC   IOAREA(L'NDESTDEM),NDESTHOM                                      
                                                                                
GETUNT14 CLI   0(R3),RFL2ACT       1ST DEMO, SAVE VALUES                        
         BNE   GETUNT18                                                         
         MVC   NBESTSHR,NBACTSHR   AND MOVE DEMOS TO 1ST SPOT                   
         MVC   NBESTHUT,NBACTHUT                                                
         MVC   NDESTHOM(L'NDESTDEM),NDACTHOM                                    
                                                                                
         MVC   TMPSHR,NBACTSHR                                                  
         MVC   TMPHUT,NBACTHUT                                                  
         MVC   IOAREA(L'NDACTDEM),NDACTHOM                                      
                                                                                
         XC    NBACTSHR,NBACTSHR                                                
         XC    NBACTHUT,NBACTHUT                                                
         XC    NDACTDEM(L'NDACTDEM),NDACTHOM                                    
                                                                                
GETUNT16 MVC   RESULT(1),NBRESULT  BUILD POSTING RESULT FIELD                   
         MVI   RESULT+1,C' '                                                    
         CLI   NBPOSTYP,MEDTMCBL   EXTRA FOR CABLE FROM PROFILE                 
         BNE   GETUNT18                                                         
         CLI   NBUSER2+5,C' '      IF NOT BLANK, Y OR N                         
         BE    GETUNT18                                                         
         CLI   NBUSER2+5,C'N'                                                   
         BE    GETUNT18                                                         
         CLI   NBUSER2+5,C'Y'                                                   
         BE    GETUNT18                                                         
         MVC   RESULT+1(1),NBUSER2+5                                            
                                                                                
GETUNT18 L     R3,ARFLVOR2                                                      
         AHI   R3,2                                                             
         ST    R3,ARFLVOR2                                                      
         B     GETUNT06                                                         
                                                                                
GETUNT20 XC    DOVIND1,DOVIND1                                                  
         XC    DOVIND2,DOVIND2                                                  
         XC    DOIND1HG(DOINDHQ),DOIND1HG                                       
                                                                                
         OC    NDEMOS,NDEMOS                                                    
         JZ    GETUNT21                                                         
         OC    NDESTDEM,NDESTDEM                                                
         JZ    GETUN20B                                                         
                                                                                
         MVC   DOIND1HG(4),=C'NNNN'                                             
         TM    NBESTSHR,X'80'      HOMES SHARE OVERRIDE?                        
         JZ    *+12                                                             
         MVI   DOIND1HS,C'Y'                                                    
         NI    NBESTSHR,X'FF'-X'80'                                             
         TM    NBACTHUT,X'80'      HOMES HUT OVERRIDE?                          
         JZ    *+12                                                             
         MVI   DOIND1HH,C'Y'                                                    
         NI    NBACTHUT,X'FF'-X'80'                                             
         TM    NDESTDEM+2,X'80'    HOMES GRP OVERRIDE?                          
         JZ    *+12                                                             
         MVI   DOIND1HG,C'Y'                                                    
         NI    NDESTDEM+2,X'FF'-X'80'                                           
         TM    NDESTDEM+4,X'80'    HOMES IMP OVERRIDE?                          
         JZ    *+12                                                             
         MVI   DOIND1HI,C'Y'                                                    
         NI    NDESTDEM+4,X'FF'-X'80'                                           
                                                                                
         SR    RF,RF                                                            
         LH    RF,NDEMOS                                                        
         LA    RE,DOVIND1                                                       
         LA    R1,NDESTDEM+8       SKIP HOMES                                   
GETUN20A MVC   0(3,RE),=C'NNN'                                                  
         TM    0(R1),X'80'         VPH OVERRIDE?                                
         JZ    *+12                                                             
         MVI   0(RE),C'Y'                                                       
         NI    0(R1),X'FF'-X'80'   UNMARK IT                                    
         TM    2(R1),X'80'         GRP OVERRIDE?                                
         JZ    *+12                                                             
         MVI   1(RE),C'Y'                                                       
         NI    2(R1),X'FF'-X'80'   UNMARK IT                                    
         TM    4(R1),X'80'         IMP OVERRIDE?                                
         JZ    *+12                                                             
         MVI   2(RE),C'Y'                                                       
         NI    4(R1),X'FF'-X'80'   UNMARK IT                                    
         AHI   RE,3                NEXT DEMO FOR INDICATORS                     
         AHI   R1,8                NEXT DEMO FOR VALUES                         
         BCT   RF,GETUN20A                                                      
                                                                                
GETUN20B OC    NDACTDEM,NDACTDEM                                                
         JZ    GETUNT21                                                         
                                                                                
         MVC   DOIND2HG(4),=C'NNNN'                                             
         TM    NBACTSHR,X'80'      HOMES SHARE OVERRIDE?                        
         JZ    *+12                                                             
         MVI   DOIND2HS,C'Y'                                                    
         NI    NBACTSHR,X'FF'-X'80'                                             
         TM    NBACTHUT,X'80'      HOMES HUT OVERRIDE?                          
         JZ    *+12                                                             
         MVI   DOIND2HH,C'Y'                                                    
         NI    NBACTHUT,X'FF'-X'80'                                             
         TM    NDACTDEM+2,X'80'    HOMES GRP OVERRIDE?                          
         JZ    *+12                                                             
         MVI   DOIND2HG,C'Y'                                                    
         NI    NDACTDEM+2,X'FF'-X'80'                                           
         TM    NDACTDEM+4,X'80'    HOMES IMP OVERRIDE?                          
         JZ    *+12                                                             
         MVI   DOIND2HI,C'Y'                                                    
         NI    NDACTDEM+4,X'FF'-X'80'                                           
                                                                                
         SR    RF,RF                                                            
         LH    RF,NDEMOS                                                        
         LA    RE,DOVIND2                                                       
         LA    R1,NDACTDEM+8       SKIP HOMES                                   
GETUN20C MVC   0(3,RE),=C'NNN'                                                  
         TM    0(R1),X'80'         VPH OVERRIDE?                                
         JZ    *+12                                                             
         MVI   0(RE),C'Y'                                                       
         NI    0(R1),X'FF'-X'80'   UNMARK IT                                    
         TM    2(R1),X'80'         GRP OVERRIDE?                                
         JZ    *+12                                                             
         MVI   1(RE),C'Y'                                                       
         NI    2(R1),X'FF'-X'80'   UNMARK IT                                    
         TM    4(R1),X'80'         IMP OVERRIDE?                                
         JZ    *+12                                                             
         MVI   2(RE),C'Y'                                                       
         NI    4(R1),X'FF'-X'80'   UNMARK IT                                    
         AHI   RE,3                NEXT DEMO FOR INDICATORS                     
         AHI   R1,8                NEXT DEMO FOR VALUES                         
         BCT   RF,GETUN20C                                                      
                                                                                
GETUNT21 MVC   NTICODE+(L'NTICODE-L'NBNTI)(L'NBNTI),NBNTI                       
         CLI   VERSION,2           DO OPTIMIZATION FOR PC APPLICATION           
         JL    EXITY               VERSION 2 AND HIGHER                         
                                                                                
         CLC   SUBLINE,LSUBLINE    TEST SUB-LINE SAME AS PREVIOUS               
         BNE   *+12                                                             
         MVI   SUBLINE,0           YES - CLEAR THIS SUB-LINE                    
         B     GETUNT22                                                         
                                                                                
         SR    R1,R1                                                            
         IC    R1,LSUBLINE                                                      
         AHI   R1,1                                                             
         CLM   R1,1,SUBLINE        TEST SUB-LINE NUMBER IN SEQUENCE             
         BE    *+14                                                             
         MVC   LSUBLINE,SUBLINE    NO - SAVE/SEND THIS ONE                      
         B     GETUNT22                                                         
                                                                                
         GOTOR SETMSK,MO#SLSEQ                                                  
         MVC   LSUBLINE,SUBLINE    YES - SAVE THIS SUB-LINE NUMBER              
         MVI   SUBLINE,0           AND CLEAR CURRENT VALUE                      
                                                                                
GETUNT22 MVI   BYTE2,0                                                          
         CLC   UNPOSTYP,LPOSTYPE   TEST POSTING TYPE CHANGED                    
         BE    GETUNT23                                                         
         MVI   BYTE2,1                                                          
         MVC   LPOSTYPE,UNPOSTYP                                                
         B     GETUNT30            YES - SEND ALL HOME VALUES                   
                                                                                
GETUNT23 CLC   DHIMP,LHIMP                                                      
         BNE   GETUNT24                                                         
         GOTOR SETMSK,MO#HIMP                                                   
GETUNT24 CLC   DHGRP,LHGRP                                                      
         BNE   GETUNT26                                                         
         GOTOR SETMSK,MO#HGRP                                                   
GETUNT26 CLC   DHSHR,LHSHR                                                      
         BNE   GETUNT28                                                         
         GOTOR SETMSK,MO#HSHR                                                   
GETUNT28 CLC   DHHUT,LHHUT                                                      
         BNE   GETUNT30                                                         
         GOTOR SETMSK,MO#HHUT                                                   
                                                                                
GETUNT30 LA    R2,OPTTAB                                                        
         USING OPTTABD,R2          R2=A(OPTIMIZATION TABLE)                     
GETUNT32 CLI   OPTTABD,OPTTEOTQ    TEST END OF TABLE                            
         BE    GETUNT38                                                         
                                                                                
         SR    R3,R3                                                            
         ICM   R3,3,OPTTDDSP                                                    
         A     R3,ABASE                                                         
         USING LX_COLS,R3          R3=A(OUTPUT COLUMN DEFINITION)               
         MVC   WORK(1),LX_CIND1                                                 
         NI    WORK,LX_CIBLK                                                    
         SR    R4,R4                                                            
         ICM   R4,1,WORK           R4=DATA BLOCK NUMBER                         
         BNZ   *+8                                                              
         LHI   R4,B#UNIT           ZERO MEANS UNIT RECORD                       
         MHI   R4,L'LP_BLKS                                                     
         L     R4,LP_BLKS-L'LP_BLKS(R4)                                         
         SR    RF,RF                                                            
         ICM   RF,3,LX_CDDSP                                                    
         AR    R4,RF               R4=A(THIS DATA VALUE)                        
         SR    R5,R5                                                            
         ICM   R5,3,OPTTLDSP                                                    
         LA    R5,SAVED(R5)        R5=A(LAST SENT VALUE)                        
         SR    RE,RE                                                            
         ICM   RE,1,OPTTDLEN                                                    
         BNZ   *+8                                                              
         IC    RE,LX_CDLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    GETUNT34                                                         
         CLC   0(0,R5),0(R4)       TEST THIS VALUE SAME AS LAST                 
         EX    RE,*+8                                                           
         B     GETUNT36                                                         
         MVC   0(0,R5),0(R4)       SET LAST SENT VALUE FROM THIS                
                                                                                
GETUNT34 EX    RE,*+8              CLEAR THIS DATA                              
         B     *+10                                                             
         XC    0(0,R4),0(R4)                                                    
         SR    R1,R1                                                            
         ICM   R1,1,OPTTMBIT                                                    
         BZ    GETUNT36                                                         
         GOTOR SETMSK,(R1)         AND SEND MASK BIT IF REQUIRED                
                                                                                
GETUNT36 AHI   R2,OPTTABL          BUMP TO NEXT TABLE ENTRY                     
         B     GETUNT32                                                         
                                                                                
GETUNT38 XC    #DEMOS,#DEMOS                                                    
         SR    R1,R1               TEST ANY DEMOS REQUESTED                     
         ICM   R1,3,NDEMOS                                                      
         BZ    GETUNT50                                                         
         STCM  R1,3,#DEMOS         SET N'DEMOS TO SEND                          
         MHI   R1,8                                                             
         BCTR  R1,0                R1=L'DEMO VALUES-1                           
                                                                                
         MVI   BYTE,1                                                           
         CLI   BYTE2,1             TEST POSTING TYPE CHANGED                    
         BE    GETUNT40                                                         
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BE    GETUNT40                                                         
         EX    R1,*+8              TEST CURRENT DEMOS SAME AS LAST              
         BE    GETUNT42                                                         
         CLC   NDESTOTH(0),LDEMOS                                               
         MVI   BYTE,0              YES - SET SAME DEMOS FLAG                    
                                                                                
GETUNT40 EX    R1,*+8              SAVE CURRENT DEMO VALUES                     
         B     *+10                                                             
         MVC   LDEMOS(0),NDESTOTH                                               
         EX    R1,*+12             TEST CURRENT DEMOS ARE ZERO                  
         BZ    GETUNT42            YES - DON'T SEND                             
         B     GETUNT44            NO - SEND CURRENT                            
         OC    NDESTOTH(0),NDESTOTH                                             
                                                                                
GETUNT42 CLI   BYTE2,1             TEST POSTING TYPE CHANGED                    
         BE    *+10                YES - ALWAYS SEND DEMOS                      
         XC    #DEMOS,#DEMOS       CLEAR N'DEMO VALUES TO SEND                  
                                                                                
GETUNT44 LA    RF,RFLAVOR2                                                      
         CLI   2(RF),SPACEQ        TEST SIDE BY SIDE                            
         JE    GETUNT50                                                         
                                                                                
         XC    #DEMOS2,#DEMOS2                                                  
         SR    R1,R1               TEST ANY DEMOS REQUESTED                     
         ICM   R1,3,NDEMOS                                                      
         BZ    GETUNT50                                                         
         STCM  R1,3,#DEMOS2        SET N'DEMOS TO SEND                          
         MHI   R1,8                                                             
         BCTR  R1,0                R1=L'DEMO VALUES-1                           
                                                                                
         CLI   BYTE2,1             TEST POSTING TYPE CHANGED                    
         BE    GETUNT46                                                         
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BE    GETUNT46                                                         
         EX    R1,*+8              TEST CURRENT DEMOS SAME AS LAST              
         BE    GETUNT48                                                         
         CLC   NDACTOTH(0),LDEMOS2                                              
         MVI   BYTE,0              YES - SET SAME DEMOS FLAG                    
                                                                                
GETUNT46 EX    R1,*+8              SAVE CURRENT DEMO VALUES                     
         B     *+10                                                             
         MVC   LDEMOS2(0),NDACTOTH                                              
         EX    R1,*+12             TEST CURRENT DEMOS ARE ZERO                  
         BZ    GETUNT48            YES - DON'T SEND                             
         B     GETUNT50            NO - SEND CURRENT                            
         OC    NDACTOTH(0),NDACTOTH                                             
                                                                                
GETUNT48 CLI   BYTE2,1             TEST POSTING TYPE CHANGED                    
         BE    *+10                YES - ALWAYS SEND DEMOS                      
         XC    #DEMOS2,#DEMOS2     CLEAR N'DEMO VALUES TO SEND                  
                                                                                
GETUNT50 CLI   BYTE,0              SET SAME DEMOS FLAG?                         
         JE    GETUNT52                                                         
         GOTOR SETMSK,MO#DEMOS     YES                                          
*        J     GETUNT54                                                         
                                                                                
GETUNT52 MVC   #DEMOS,NDEMOS                                                    
         MVC   #DEMOS2,NDEMOS                                                   
                                                                                
GETUNT54 CLC   LMASK,LP_RMASK      TEST CURRENT MASK SAME AS LAST               
         JNE   GETUNT56                                                         
         XC    LP_RMASK,LP_RMASK   YES - CLEAR MASK                             
         MVI   SAMEMASK,YESQ       AND SET SAME MASK FLAG                       
         J     EXITY                                                            
                                                                                
GETUNT56 MVC   LMASK,LP_RMASK                                                   
         J     EXITY                                                            
                                                                                
GETUNTX  MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
         DROP  R2,R3,R6,RB                                                      
         EJECT                                                                  
***********************************************************************         
* SET NBNTISTA - ON CHANGE OF NETWORK READ STATION LIST RECORD        *         
***********************************************************************         
                                                                                
GETNTI   MVC   NBNTISTA,LNTISTA                                                 
         L     RF,NBAIO                                                         
         CLC   LNETWORK,NUKNET-NURECD(RF)                                       
         BER   RE                                                               
                                                                                
         NTR1  LABEL=*                                                          
         ICM   R0,B'1100',NBDTADSP                                              
         ICM   R0,B'0010',NBFILE                                                
         XC    LNTISTA,LNTISTA                                                  
         MVC   KEYSAVE,NBKEY       SAVE UNIT KEY                                
         USING SLSRECD,NBKEY                                                    
         XC    SLSKEY,SLSKEY                                                    
         MVI   SLSKTYP,SLSKTYPQ                                                 
         MVI   SLSKSUB,SLSKSUBQ                                                 
         MVC   SLSKAGMD,NBACTAM                                                 
         MVC   SLSKSTA(L'NUKNET),NUKNET-NURECD(RF)                              
         MVI   SLSKSTA+L'SLSKSTA-1,C'N'                                         
         GOTOR READ,NBFILSPT                                                    
         JNE   GETNTIX                                                          
         GOTOR GETREC,AIO3                                                      
         L     RF,AIO3                                                          
         CLI   SLSNTI-SLSRECD(RF),C' '                                          
         JNH   GETNTIX                                                          
         MVC   LNTISTA,SLSNTI-SLSRECD(RF)                                       
                                                                                
GETNTIX  MVC   NBNTISTA,LNTISTA                                                 
         MVC   NBKEY,KEYSAVE                                                    
         STCM  R0,B'1100',NBDTADSP                                              
         STCM  R0,B'0010',NBFILE                                                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CONVERT MEDIA CODE FOR STATS                                        *         
***********************************************************************         
                                                                                
CNVMED   L     R1,4(R1)                                                         
         MVC   0(L'NBACTAM,R1),NBACTAM                                          
         J     EXITY                                                            
                                                                                
***********************************************************************         
* VALIDATE CLIENT CODE AND APPLY LIMIT ACCESS FOR STATS               *         
***********************************************************************         
                                                                                
VALCFC   L     R1,0(R1)                                                         
         GOTOR RESCLT,L'NBACTAM(R1)                                             
         JNE   EXIT                EXIT IF CAN'T RESOLVE CLIENT                 
         GOTOR FLTACS              APPLY LIMIT ACCESS                           
         J     EXIT                EXIT WITH CONDITION CODE SET                 
                                                                                
***********************************************************************         
* VALIDATE STATION CODE FOR STATS                                     *         
***********************************************************************         
                                                                                
VALCFV   LM    RE,RF,0(R1)                                                      
         MVC   0(L'NBACTNET,RF),0(RE)                                           
         J     EXIT                                                             
                                                                                
***********************************************************************         
* CALL CFMIO TO DO STATS DOWNLOADS                                    *         
***********************************************************************         
                                                                                
         USING LP_D,R1                                                          
NXTSTAT  LA    R0,NPVALUES                                                      
         ST    R0,LP_ADATA                                                      
         GOTOR VCFMIO,CFMIOD       CALL CFMIO TO BUILD NEXT RECORD              
         J     EXITY                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* GET GOAL RECORDS                                                    *         
***********************************************************************         
                                                                                
NXTGOL   LR    R6,R1                                                            
         USING LP_D,R6                                                          
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTGOL02                                                         
         CLI   RECORD,RECUNIT      TEST DOWNLOADING UNITS ONLY                  
         JE    NXTGOLX                                                          
         CLI   RECORD,RECDFLT      DEFAULT IS UNITS ONLY TOO                    
         JE    NXTGOLX                                                          
         LA    R0,LASTS                                                         
         LHI   R1,LASTSL+LNETSL                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
NXTGOL02 L     RF,NBAAGY                                                        
         OI    AGYFLAG2,AGYFLAG2_XPR   TURN ON FOR EVERYONE                     
         TM    AGYFLAG2,AGYFLAG2_XPR   READ XSPFILE FOR GOAL RECORDS?           
         JZ    NXTGOL03                                                         
         GOTOR NXTREC,DMCB,('YESQ',GOLXKEYT),('B#GOAL',0),             *        
               ('NBFILXSP',SAVED),0,0                                           
         JNE   EXITY                                                            
         L     R2,IOADDR                                                        
         USING GOALREC,R2          R2=A(GOAL RECORD)                            
         TM    GXKEYAGY,GXKEYTAR   PLANNED GOAL?                                
         JO    EXITY                                                            
         J     NXTGOL04                                                         
NXTGOL03 GOTOR NXTREC,DMCB,('YESQ',GOLKEYT),('B#GOAL',0),              *        
               ('NBFILSPT',SAVED),0,0                                           
         JNE   EXITY                                                            
NXTGOL04 L     R2,IOADDR                                                        
         USING GOALREC,R2          R2=A(GOAL RECORD)                            
         CLC   LGETCLT,GKEYCLT                                                  
         JE    NXTGOL06                                                         
         GOTOR RESCLT,GKEYCLT      RESOLVE CLIENT                               
         JE    NXTGOL06                                                         
         SR    R1,R1               BAD CLIENT - SKIP TO NEXT                    
         ICM   R1,B'0011',GKEYCLT                                               
         AHI   R1,1                                                             
         ICM   R1,B'1100',GKEYTYPE                                              
         XC    NBKEY,NBKEY                                                      
         STCM  R1,B'1111',NBKEY                                                 
         J     NXTGOL02                                                         
                                                                                
NXTGOL06 L     RF,NBAAGY                                                        
         TM    AGYFLAG2,AGYFLAG2_XPR   READ XSPFILE FOR GOAL RECORDS?           
         JZ    *+12                                                             
         AHI   R2,GXDATA-GXKEY                                                  
         J     *+8                                                              
         AHI   R2,GDELEM-GOALREC                                                
         USING GLEMENT,R2          R2=A(GOAL RECORD ELEMENT)                    
         SR    R0,R0                                                            
NXTGOL08 CLI   GLCODE,0            TEST END OF RECORD                           
         JE    NXTGOL02                                                         
         CLI   GLCODE,GLCODEQ      TEST GOAL WEEKLY ELEMENT                     
         JNE   NXTGOL09                                                         
         CLC   GLWEEK,STRDMON      TEST START BEFORE REQUEST START              
         JL    NXTGOL09                                                         
         CLC   GLWEEK,ENDDATE      TEST WEEK START AFTER REQUEST END            
         JNH   NXTGOL10                                                         
NXTGOL09 IC    R0,GLEN             BUMP TO NEXT RECORD ELEMENT                  
         AR    R2,R0                                                            
         J     NXTGOL08                                                         
                                                                                
NXTGOL10 XC    GOLVALS(GOLVALL),GOLVALS                                         
         LA    R0,GOLVALS                                                       
         ST    R0,LP_ADATA                                                      
         L     R2,IOADDR           R2=A(GOAL RECORD)                            
         USING GOALREC,R2                                                       
                                                                                
         LA    RF,MEDTAB           LOCATE MEDIA                                 
         USING MEDTABD,RF          RF=A(MEDIA TABLE)                            
NXTGOL12 CLI   MEDTMED,MEDTMEOT    TEST END OF TABLE                            
         JE    NXTGOL02            YES - BAD GOAL RECORD                        
         CLC   GKEYMKT,MEDTGMKT    MATCH MARKET TO TABLE                        
         JE    *+12                                                             
         AHI   RF,MEDTABL                                                       
         J     NXTGOL12                                                         
         MVC   GOLMED,MEDTMED      SET MEDIA CODE                               
         DROP  RF                                                               
                                                                                
         L     RF,NBAAGY                                                        
         TM    AGYFLAG2,AGYFLAG2_XPR   READ XSPFILE FOR GOAL RECORDS?           
         JZ    *+12                                                             
         LA    R1,(GXDATA-GXKEY)(R2)                                            
         J     *+8                                                              
         LA    R1,GDELEM                                                        
         USING GDELEM,R1                                                        
         SR    R0,R0                                                            
NXTGOL14 CLI   GOCODE,0                                                         
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   GOCODE,GDCODEQ                                                   
         JE    *+14                                                             
         IC    R0,GOLEN                                                         
         AR    R1,R0                                                            
         J     NXTGOL14                                                         
         MVC   GOLNET,GDNETWK      SET NETWORK CODE                             
         DROP  R1                                                               
                                                                                
         MVC   GOLCLT,GKEYCLT      SET CLIENT CODE                              
         MVC   GOLBRD,GKEYPRD      SET BRAND1 (PRODUCT) NUMBER                  
         MVC   GOLEST,GKEYEST      SET ESTIMATE NUMBER                          
         MVC   GOLDPT,GKEYDPT      SET DAYPART CODE                             
         MVC   GOLSEC,GKEYSLN      SET SECONDS LENGTH                           
         MVC   GOLPRDA,GXKPRDA-GXKEY(R2)                                        
         CLC   LCLT,GOLCLT         DO GOAL RECORD OPTIMIZATION                  
         MVC   LCLT,GOLCLT                                                      
         JNE   *+10                                                             
         XC    GOLCLT,GOLCLT                                                    
                                                                                
         CLC   LGOLBRD,GOLBRD                                                   
         MVC   LGOLBRD,GOLBRD                                                   
         JNE   *+10                                                             
         XC    GOLBRD,GOLBRD                                                    
                                                                                
         OC    GOLPRDA,GOLPRDA                                                  
         JZ    *+10                                                             
         XC    GOLBRD,GOLBRD       DON'T SEND IT BACK TWICE                     
                                                                                
         CLC   LGOLPRDA,GOLPRDA                                                 
         MVC   LGOLPRDA,GOLPRDA                                                 
         JNE   *+10                                                             
         XC    GOLPRDA,GOLPRDA                                                  
                                                                                
         CLC   LSECONDS,GOLSEC                                                  
         MVC   LSECONDS,GOLSEC                                                  
         JNE   *+10                                                             
         XC    GOLSEC,GOLSEC                                                    
                                                                                
         CLC   LEST,GOLEST                                                      
         MVC   LEST,GOLEST                                                      
         JNE   *+10                                                             
         XC    GOLEST,GOLEST                                                    
                                                                                
         CLC   LGOLMED,GOLMED                                                   
         MVC   LGOLMED,GOLMED                                                   
         JNE   *+10                                                             
         XC    GOLMED,GOLMED                                                    
                                                                                
         CLC   LDAYPART,GOLDPT                                                  
         MVC   LDAYPART,GOLDPT                                                  
         JNE   *+10                                                             
         XC    GOLDPT,GOLDPT                                                    
                                                                                
         CLC   LNETWORK,GOLNET                                                  
         MVC   LNETWORK,GOLNET                                                  
         JNE   *+10                                                             
         XC    GOLNET,GOLNET                                                    
         J     EXITY                                                            
                                                                                
NXTGOLX  MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD GOAL WEEKLY RECORDS                                *         
***********************************************************************         
                                                                                
GETGWK   LR    R6,R1                                                            
         USING LP_D,R6                                                          
         L     R2,NBAIO                                                         
         USING GOALREC,R2          R2=A(GOAL RECORD)                            
         XC    GWKTAB#,GWKTAB#     INITIALIZE N'ENTRIES IN GWKTAB               
         XC    GOALDATE,GOALDATE   CLEAR LAST WEEK START DATE                   
                                                                                
         LA    R2,(GXDATA-GXKEY)(R2)                                            
         USING GLEMENT,R2          R2=A(FIRST GOAL RECORD ELEMENT)              
         L     R3,AIO3                                                          
         ST    R3,LP_ADATA         SET A(ARRAY FOR DDLINK)                      
TAB      USING GWKTABD,R3          R3=A(CURRENT TABLE ENTRY)                    
         SR    R4,R4                                                            
PRV      USING GWKTABD,R4          R4=A(PREVIOUS TABLE ENTRY)                   
                                                                                
GETGWK02 CLI   GLCODE,0            TEST END OF RECORD                           
         JE    GETGWK10                                                         
         CLI   GLCODE,GLCODEQ      TEST GOAL WEEKLY ELEMENT                     
         JNE   GETGWK08                                                         
         CLC   GLWEEK,STRDMON      TEST START BEFORE REQUEST START              
         JL    GETGWK08                                                         
         CLC   GLWEEK,ENDDATE      TEST WEEK START AFTER REQUEST END            
         JH    GETGWK08                                                         
                                                                                
         XC    TAB.GWKTABD(GWKTABL),TAB.GWKTABD                                 
         MVI   TAB.GWKREP#,1                                                    
         GOTOR NBDATCON,DMCB,(2,GLWEEK),TAB.GWKSTR                              
                                                                                
         ICM   R0,15,GLBUDGET                                                   
         CVD   R0,DUB                                                           
         ZAP   TAB.GWKDOL,DUB      SET DOLLARS                                  
                                                                                
         ICM   R0,15,GLGRP                                                      
         GOTOR SETGRP                                                           
         ZAP   TAB.GWKGRP1,DUB     SET GRPS 1                                   
                                                                                
         CLI   GLEN,GLEN2Q                                                      
         JL    GETGWK04                                                         
         ICM   R0,15,GLGRP2                                                     
         GOTOR SETGRP                                                           
         ZAP   TAB.GWKGRP2,DUB     SET GRPS 2                                   
                                                                                
         CLI   GLEN,GLEN4Q                                                      
         JL    GETGWK04                                                         
         ICM   R0,15,GLGRP3                                                     
         GOTOR SETGRP                                                           
         ZAP   TAB.GWKGRP3,DUB     SET GRPS 3                                   
                                                                                
GETGWK04 LTR   R4,R4               TEST FIRST TIME                              
         JZ    GETGWK06                                                         
         SR    R0,R0               NO - SET 'SAME AS PREVIOUS' FLAGS            
         IC    R0,PRV.GWKREP#                                                   
         MHI   R0,7                                                             
         GOTOR NBADDAY,DMCB,PRV.GWKSTR,WORK,(R0)                                
         CLC   TAB.GWKSTR,WORK                                                  
         JNE   *+8                                                              
         OI    TAB.GWKINDS,GWKISEQ SET WEEK IN SEQUENCE                         
         CLC   TAB.GWKGRPS(GWKGRPL),PRV.GWKGRPS                                 
         JNE   *+8                                                              
         OI    TAB.GWKINDS,GWKIGRP                                              
         CP    TAB.GWKDOL,PRV.GWKDOL                                            
         JNE   *+8                                                              
         OI    TAB.GWKINDS,GWKIDOL SET DOLLARS SAME AS PREVIOUS                 
         TM    TAB.GWKINDS,GWKISEQ+GWKIGRP+GWKIDOL                              
         JNO   GETGWK06                                                         
         SR    R0,R0               BUMP REPLICATION FACTOR IF WEEK IN           
         IC    R0,PRV.GWKREP#      SEQUENCE AND ALL DATA THE SAME               
         AHI   R0,1                                                             
         STC   R0,PRV.GWKREP#                                                   
         J     GETGWK08                                                         
                                                                                
GETGWK06 LA    R4,TAB.GWKTABD      POINT TO CURRENT (PREVIOUS)                  
         AHI   R3,GWKTABL          POINT TO NEXT (CURRENT)                      
         LH    R0,GWKTAB#          BUMP GOAL WEEK ENTRY COUNT                   
         AHI   R0,1                                                             
         STH   R0,GWKTAB#                                                       
                                                                                
GETGWK08 SR    R0,R0               BUMP TO NEXT RECORD ELEMENT                  
         IC    R0,GLEN                                                          
         AR    R2,R0                                                            
         J     GETGWK02                                                         
                                                                                
GETGWK10 SR    R0,R0               DO GOAL WEEK DOWNLOAD OPTIMIZATION           
         ICM   R0,3,GWKTAB#                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3             POINT TO START OF TABLE                      
                                                                                
GETGWK12 CLI   TAB.GWKREP#,1       TEST REPLICATION FACTOR IS ONE               
         JNE   *+8                                                              
         MVI   TAB.GWKREP#,0       YES - CLEAR REPLICATION FACTOR               
                                                                                
         TM    TAB.GWKINDS,GWKISEQ TEST DATE IN SEQUENCE                        
         JZ    *+10                                                             
         XC    TAB.GWKSTR,TAB.GWKSTR                                            
                                                                                
         TM    TAB.GWKINDS,GWKIGRP TEST GRPS1 SAME AS PREVIOUS                  
         JZ    *+10                                                             
         XC    TAB.GWKGRPS(GWKGRPL),TAB.GWKGRPS                                 
                                                                                
         TM    TAB.GWKINDS,GWKIDOL TEST DOLLARS SAME AS PREVIOUS                
         JZ    *+10                                                             
         XC    TAB.GWKDOL,TAB.GWKDOL                                            
                                                                                
         AHI   R3,GWKTABL          BUMP TO NEXT TABLE ENTRY                     
         JCT   R0,GETGWK12         DO FOR NUMBER OF ENTRIES                     
         J     EXITY                                                            
                                                                                
SETGRP   CLC   VERSION,V41028      TEST VERSION 4.1.0.28                        
         JL    *+8                                                              
         MHI   R0,10               EQUAL OR HIGH - ADJUST GRP VALUE             
         CVD   R0,DUB                                                           
         BR    RE                                                               
         DROP  TAB,PRV,R2,R6                                                    
         EJECT                                                                  
***********************************************************************         
* GET CLIENT RECORDS FOR MFM CLIENT DOWNLOAD                          *         
***********************************************************************         
                                                                                
NXTCLT   GOTOR NXTREC,DMCB,('YESQ',CLTKEYT),('B#CLT',0),               *        
               ('NBFILSPT',SAVED),0,AFLTACS                                     
         JNE   EXITY                                                            
         L     R2,NBACLI                                                        
         USING CLTHDR,R2                                                        
         OC    CPLDATA(CPLDATAL),CPLDATA                                        
         JZ    NXTCLT                                                           
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET PRODUCT RECORDS FOR MFM PRODUCT DOWNLOAD                        *         
***********************************************************************         
                                                                                
NXTPRD   GOTOR NXTREC,DMCB,('YESQ',PRDKEYT),('B#PRD',0),               *        
               ('NBFILSPT',SAVED),0,0                                           
         JNE   EXITY                                                            
         L     R2,IOADDR                                                        
         USING PRDHDR,R2                                                        
         L     R1,AIO4             LOOK UP CLIENT CODE IN TABLE                 
NXTPRD02 CLC   PKEYCLT,0(R1)                                                    
         JE    NXTPRD04                                                         
         OC    0(L'PKEYCLT,R1),0(R1)                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AHI   R1,5                                                             
         J     NXTPRD02                                                         
NXTPRD04 MVC   CLTCODE,L'PKEYCLT(R1)                                            
         CLC   LCLTCODE,CLTCODE                                                 
         MVC   LCLTCODE,CLTCODE                                                 
         JNE   *+10                                                             
         XC    CLTCODE,CLTCODE     ONLY SEND CLIENT ONCE                        
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET VENDOR RECORDS FOR MFM VENDOR DOWNLOAD                          *         
***********************************************************************         
                                                                                
NXTVEN   GOTOR NXTREC,DMCB,('YESQ',STAKEYT),('B#STA',0),               *        
               ('NBFILSTA',SAVED),0,0                                           
         JNE   EXITY                                                            
         L     R2,IOADDR                                                        
         USING STAREC,R2           R2=A(STATION RECORD)                         
         LA    R3,MEDFILTS         R3=A(MEDIA FILTERS)                          
         LHI   R0,MEDTABN                                                       
NXTVEN02 CLI   0(R3),0             TEST END OF MEDIA FILTERS                    
         JE    NXTVEN                                                           
         CLC   STYPE,0(R3)         MATCH MEDIA FILTER TO STATION TYPE           
         JE    NXTVEN04                                                         
         AHI   R3,L'MEDFILTS                                                    
         JCT   R0,NXTVEN02                                                      
         J     NXTVEN              NO MATCH - GET NEXT STATION                  
                                                                                
NXTVEN04 MVC   KEYSAVE,NBKEY       SAVE STATION KEY                             
         LA    RE,SMKT                                                          
         XC    NETNAME,NETNAME                                                  
                                                                                
         LA    R0,IOAREA                                                        
         STCM  R0,15,IOADDR                                                     
         LA    R2,NBKEY                                                         
         USING MKTREC,R2           READ MARKET RECORD FOR NETWORK NAME          
         XC    MKTKEY,MKTKEY                                                    
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVI   MKTKMED,NETMEDQ                                                  
         MVC   MKTKMKT,0(RE)                                                    
         MVC   MKTKAGY,AGENCY                                                   
         MVI   MKTKFILL,C'0'                                                    
         MVC   MKTKFILL+1(L'MKTKFILL-1),MKTKFILL                                
         GOTOR READ,NBFILSTA                                                    
         MVC   NBKEY,KEYSAVE                                                    
         JNE   EXITY                                                            
         L     R2,IOADDR                                                        
         MVC   NETNAME,MKTNAME                                                  
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A DEMO EXPRESSION                                          *         
***********************************************************************         
                                                                                
         USING LP_D,R1                                                          
VALDEM   J     *+12                                                             
         DC    CL8'*VALDEM*'                                                    
         LM    R1,R3,LP_AINP                                                    
*                                                                               
         CLC   0(2,R1),=C'U1'      TEST FOR USER DEMOS                          
         JL    VALDEM02                                                         
         CLC   0(2,R1),=C'U4'                                                   
         JH    VALDEM02                                                         
         MVI   0(R3),0             BUILD USER DEMO ENTRY                        
         MVI   1(R3),USERDEMO                                                   
         MVC   2(1,R3),1(R1)                                                    
         NI    2(R3),X'0F'                                                      
         J     EXITY                                                            
                                                                                
VALDEM02 XC    FLDH,FLDH                                                        
         BCTR  R2,0                R2=INPUT LENGTH-1                            
         BASR  RE,0                                                             
         EX    R2,8(RE)                                                         
         J     *+10                                                             
         MVC   FLD(0),0(R1)        MOVE INPUT TO DUMMY TWA FIELD                
         AHI   R2,1                                                             
         STC   R2,FLDH+5           SET L'INPUT DATA                             
         AHI   R2,L'FLDH                                                        
         STC   R2,FLDH                                                          
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,RCOMFACS                                                
         MVC   DBFILE,DBFTPT                                                    
         MVI   DBSELMED,C'T'                                                    
         L     RF,RCOMFACS                                                      
         L     RF,CDEMOVAL-COMFACSD(RF)                                         
*                                                                               
* PARAM 6 PASSED FOR COMSCORE                                                   
*                                                                               
         GOTOR (RF),DMCB,(1,FLDH),(1,(R3)),(C'S',DBLOCK),0,0,DUB                
         CLI   4(R1),1             SHOULD BE ONE VALID DEMO                     
         JNE   EXITN                                                            
*                                                                               
         CLI   2(R3),0             COMSCORE DEMO?                               
         JNE   EXITY                                                            
         ZIC   RF,CDEMINDX                                                      
         AHI   RF,1                                                             
         STC   RF,CDEMINDX                                                      
         MVC   1(1,R3),CDEMINDX    SET CORRECT INDEX                            
*                                                                               
         LA    RF,NDNTDMS                                                       
VALDEM04 CLI   0(RF),0             FIND FIRST AVAILABLE SLOT                    
         JE    *+12                                                             
         AHI   RF,8                                                             
         J     VALDEM04                                                         
         MVC   0(8,RF),DUB         SAVE IT                                      
*                                                                               
         J     EXITY                                                            
***********************************************************************         
* VALIDATE A DEMO EXPRESSION                                          *         
***********************************************************************         
                                                                                
         USING LP_D,R1                                                          
VALDEMO  J     *+12                                                             
         DC    CL8'*VALDEMO*'                                                   
         LM    R2,R4,LP_AINP                                                    
                                                                                
         CLC   0(2,R1),=C'U1'      TEST FOR USER DEMOS                          
         JL    *+14                                                             
         CLC   0(2,R1),=C'U4'                                                   
         JNH   VDEMO02                                                          
                                                                                
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         BCTR  R3,0                R3=INPUT LENGTH-1                            
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         MVC   FLD(0),0(R2)        MOVE INPUT TO DUMMY TWA FIELD                
*                                                                               
         AHI   R3,1                RESTORE INPUT LENGTH                         
         STC   R3,FLDH+5           SET L'INPUT DATA                             
         AHI   R3,L'FLDH                                                        
         STC   R3,FLDH                                                          
         SHI   R3,L'FLDH           RESTORE INPUT LENGTH                         
*                                                                               
         L     RF,AIO4                                                          
         USING P5XD,RF                                                          
         XC    0(P5XDLNQ,RF),0(RF)                                              
         MVC   P5XID,=C'P5X '                                                   
         LA    RE,COMLIC                                                        
         ST    RE,P5XLICNS          A(32 BYTE COMSCORE LICENSE)                 
         ST    RF,DMCB+16           EXTENDED PARAM5                             
         OI    DMCB+16,X'80'        SO DEMOVAL KNOWS EXTENDED BLOCK             
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,RCOMFACS                                                
         MVC   DBFILE,DBFTPT                                                    
         MVI   DBSELMED,C'T'                                                    
         L     RF,RCOMFACS                                                      
         L     RF,CDEMOVAL-COMFACSD(RF)                                         
*                                                                               
* PARAM 6 PASSED FOR COMSCORE                                                   
*                                                                               
         GOTOR (RF),DMCB,(1,FLDH),(1,FULL),(C'S',DBLOCK),0,,DUB                 
         CLI   4(R1),1             SHOULD BE ONE VALID DEMO                     
         JNE   EXITN                                                            
                                                                                
VDEMO02  MVC   0(11,R4),FLD                                                     
         OC    0(11,R4),SPACES                                                  
         J     EXITY                                                            
***********************************************************************         
* VALIDATE A CLIENT CODE                                              *         
***********************************************************************         
                                                                                
VALCLT   J     *+12                                                             
         DC    CL8'*VALCLT*'                                                    
         LM    R2,R4,LP_AINP                                                    
         MVC   WORK(3),0(R2)                                                    
         CHI   R3,2                                                             
         JL    EXITN                                                            
         JH    *+8                                                              
         MVI   WORK+2,SPACEQ                                                    
         GOTOR NBCLPACK,DMCB,WORK,(R4)                                          
         CLI   0(R1),FF                                                         
         JE    EXITN                                                            
         MVC   REQCLT,0(R4)        SAVE AWAY REQUEST CLIENT                     
                                                                                
         GOTOR RESCLT,(R4)         RESOLVE CLIENT                               
         JNE   EXITN                                                            
         CLC   NBSELCL2,EFFS       TEST MULTIPLE CLIENTS                        
         JE    EXITY                                                            
         OC    NBSELCL2,NBSELCL2   TEST ALREADY SET                             
         JZ    *+14                                                             
         MVC   NBSELCL2,EFFS       YES - SET MULTIPLE CLIENTS                   
         J     EXITY                                                            
         MVC   NBSELCL2,0(R4)      ELSE SET CLIENT CODE                         
         J     EXITY                                                            
                                                                                
***********************************************************************         
* VALIDATE A PRODUCT CODE                                             *         
***********************************************************************         
                                                                                
VALPRD   J     *+12                                                             
         DC    CL8'*VALPRD*'                                                    
         OC    NBSELCL2,NBSELCL2   TEST ANY CLIENT SELECTED                     
         JZ    EXITN                                                            
         CLC   NBSELCL2,EFFS       TEST MULTIPLE CLIENTS SELECTED               
         JE    EXITN                                                            
         LM    R2,R4,LP_AINP                                                    
         MVC   WORK(3),0(R2)                                                    
         CHI   R3,2                ONE CHARACTER PRODUCT CODE IS BAD            
         JL    EXITN                                                            
         JH    *+8                                                              
         MVI   WORK+2,SPACEQ       PAD 2 CHARACTER PRODUCT WITH A SPACE         
                                                                                
         LA    R3,NBKEY                                                         
         USING PRDHDR,R3                                                        
         XC    NBKEY,NBKEY                                                      
         MVI   PLSTTYPE,PLSTTYPQ                                                
         MVI   PLSTSUB,PLSTSUBQ                                                 
         MVC   PLSTAM,NBACTAM                                                   
         MVC   PLSTCLT,NBSELCL2                                                 
         MVC   PLSTPRD,WORK        ALPHA PRODUCT CODE                           
         CLC   WORK(3),POL                                                      
         JNE   *+8                                                              
         MVI   PLSTXFF,X'FF'                                                    
                                                                                
         MVC   KEYSAVE(13),NBKEY                                                
         GOTOR HIGH,NBFILSPT                                                    
         CLC   NBKEY(9),KEYSAVE    FOUND THE PRODUCT PASSIVE?                   
         JNE   EXITN                                                            
                                                                                
VALPRD04 MVC   0(L'NUPRD,R4),PLSTBPRD+1     MOVE IN PRODUCT EQUATE              
         CLI   PLSTBPRD+1,0                                                     
         JNE   *+10                                                             
         MVC   0(L'NBSELPRD,R4),WORK                                            
                                                                                
         CLC   POL,WORK            TEST PRODUCT IS 'POL'                        
         JNE   VALPRD08                                                         
         MVI   0(R4),POLPRDN                                                    
         OC    NBSELPRD,NBSELPRD   CAN'T HAVE 'POL' AND ANY OTHER               
         JNZ   EXITN                                                            
         MVC   NBSELPRD,EFFS       SET 'POL' PRODUCT INPUT                      
         J     EXIT                                                             
                                                                                
VALPRD08 CLC   NBSELPRD,EFFS       CAN'T HAVE 'POL' AND ANY OTHER               
         JE    EXITN                                                            
         MVC   NBSELPRD,WORK       SET REGULAR PRODUCT INPUT                    
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* VALIDATE DAYS                                                       *         
***********************************************************************         
                                                                                
VALDAY   J     *+12                                                             
         DC    CL8'*VALDAY*'                                                    
         LM    R2,R4,LP_AINP                                                    
         L     RF,RSYSFACS                                                      
         L     RF,FDAYVAL-FACD(RF)                                              
         GOTOR (RF),DMCB,((R3),(R2)),(R4),DUB                                   
         CLI   0(R1),0                                                          
         JNE   EXITY                                                            
         J     EXITN                                                            
                                                                                
***********************************************************************         
* VALIDATE TIMES                                                      *         
***********************************************************************         
                                                                                
VALTIM   J     *+12                                                             
         DC    CL8'*VALTIM*'                                                    
         LM    R2,R4,LP_AINP                                                    
         L     RF,RSYSFACS                                                      
         L     RF,FTIMVAL-FACD(RF)                                              
         GOTOR (RF),DMCB,((R3),(R2)),DUB                                        
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
                                                                                
***********************************************************************         
* VALIDATE DAYPART CODE                                               *         
***********************************************************************         
                                                                                
VALDPT   J     *+12                                                             
         DC    CL8'*VALDPT*'                                                    
         LM    R2,R4,LP_AINP                                                    
         TM    INDS,INDS2CDP       TEST 2 CHARACTER DAYPARTS IN USE             
         JNZ   VALDPT02            YES                                          
         CHI   R3,1                TEST ONE CHARACTER INPUT                     
         JNE   EXITN                                                            
         MVC   0(L'NBSELDP,R4),0(R2)                                            
         J     EXITY                                                            
                                                                                
VALDPT02 MVC   WORK(L'NDPTDPTA),0(R2)                                           
         CHI   R3,L'NDPTDPTA                                                    
         JE    *+8                                                              
         MVI   WORK+1,SPACEQ                                                    
                                                                                
         USING NDPTKEY,NBKEY                                                    
         XC    NDPTKEY,NDPTKEY                                                  
         MVI   NDPTKTY,NDPTKTYQ                                                 
         MVI   NDPTKST,NDPTKSTQ                                                 
         MVC   NDPTAGM,NBACTAM                                                  
         MVC   KEYSAVE,NDPTKEY                                                  
         GOTOR HIGH,NBFILUNT                                                    
                                                                                
VALDPT04 CLC   NDPTKEY(NDPTDPTE-NDPTKEY),KEYSAVE                                
         JNE   VALDPT06                                                         
         CLC   NDPTDPTA,WORK       MATCH INPUT TO RECORD                        
         JNE   *+14                                                             
         MVC   0(L'NBSELDP,R4),NDPTDPTE                                         
         J     EXITY                                                            
         GOTOR SEQ,NBFILUNT                                                     
         J     VALDPT04                                                         
                                                                                
VALDPT06 MVC   NDPTKEY,KEYSAVE                                                  
         OC    NDPTCLT,NDPTCLT                                                  
         JNZ   EXITN                                                            
         OC    NBSELCL2,NBSELCL2                                                
         JZ    EXITN                                                            
         CLC   NBSELCL2,EFFS                                                    
         JE    EXITN                                                            
         MVC   NDPTCLT,NBSELCL2                                                 
         MVC   KEYSAVE,NDPTKEY                                                  
         GOTOR HIGH,NBFILUNT                                                    
         J     VALDPT04                                                         
         EJECT                                                                  
***********************************************************************         
* EDIT CLIENT CODE                                                    *         
***********************************************************************         
                                                                                
EDTCLT   J     *+12                                                             
         DC    CL8'*EDTCLT*'                                                    
         L     R2,LP_AINP                                                       
         OC    0(L'NUKCLT,R2),0(R2) TEST DATA GIVEN                             
         JZ    EXITN               NO - DON'T SEND                              
         L     R4,LP_AOUT                                                       
         L     R3,NBACLI                                                        
         USING CLTHDR,R3                                                        
         GOTOR NBCLUNPK,DMCB,(CPROF+6,(R2)),(R4)                                
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* EDIT PRODUCT NUMBER (CONVERT FROM 1 FILETYPE NUMBER TO 3 BYTE CODE) *         
***********************************************************************         
                                                                                
EDTPRD   J     *+12                                                             
         DC    CL8'*EDTPRD*'                                                    
         MVI   LP_ODELM,C'*'       SET ASTERISK AS DELIMITER                    
         LHI   R3,L'NBSELPRD                                                    
         STCM  R3,15,LP_OLEN                                                    
         L     R2,LP_AINP                                                       
         L     R4,LP_AOUT                                                       
         L     R3,NBACLI                                                        
         AHI   R3,CLIST-CLTHDR     R3=A(PRODUCT LIST)                           
         LHI   R0,CPLDMAXN                                                      
         CLC   L'PKEYPRD(L'NUPRDPR,R3),0(R2)                                    
         JE    EDTPRD04                                                         
         AHI   R3,CPLDATAL                                                      
         JCT   R0,*-14                                                          
                                                                                
         L     R3,NBACLI                                                        
         AHI   R3,CLIST2-CLTHDR    R3=A(PRODUCT LIST EXTENSION)                 
         LHI   R0,255-CPLDMAXN                                                  
         CLC   L'PKEYPRD(L'NUPRDPR,R3),0(R2)                                    
         JE    EDTPRD04                                                         
         AHI   R3,CPLDATAL                                                      
         JCT   R0,*-14                                                          
                                                                                
         MVC   0(L'PKEYPRD,R4),UNKNOWN                                          
         J     EXITY                                                            
                                                                                
EDTPRD04 MVC   0(L'PKEYPRD,R4),0(R3)                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* EDIT TIME(S) FOR STEWARD                                            *         
***********************************************************************         
                                                                                
EDTTIM   J     *+12                                                             
         DC    CL8'*EDTTIM*'                                                    
         L     R2,LP_AINP                                                       
         OC    0(L'NBTIME,R2),0(R2)                                             
         JZ    EXITN                                                            
         L     R4,LP_AOUT                                                       
         L     RF,RSYSFACS                                                      
         L     RF,FUNTIME-FACD(RF)                                              
         GOTOR (RF),DMCB,(R2),(R4)                                              
         J     EXITY                                                            
                                                                                
BLDTIME  L     R2,LP_AINP                                                       
         OC    0(L'NBTIME,R2),0(R2)                                             
         JZ    EXITN                                                            
         MVC   UNTSTIM,0(R2)                                                    
         MVC   UNTETIM,2(R2)                                                    
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
                                                                                
**********************************************************************          
* Edit special charge Market/Station                                            
**********************************************************************          
                                                                                
         USING LP_D,R1                                                          
SCMKTSTA L     R2,LP_AINP                                                       
         USING NUSPRD,R2                                                        
         XC    CISTATIN,CISTATIN        clear output                            
         XC    DUB1,DUB1                                                        
         OC    NUSPRCIS,NUSPRCIS   is market inputted                           
         JZ    EXITY                                                            
                                                                                
         USING STAPACKD,WORK                                                    
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,NBAAGY                                                        
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVI   STAPMED,C'N'                                                     
         MVC   STAPACOM,RCOMFACS                                                
         MVC   STAPMKST,NUSPRCIS   Set dummy market                             
         GOTOR NBCALLOV,DMCB,0,X'D9000A7A'                                      
         L     R3,DMCB                                                          
         GOTOR (R3),STAPACKD                                                    
         MVC   CISTATIN,STAPQSTA        CI STATION                              
         J     EXITY                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* EDIT DAYPART CODE                                                   *         
***********************************************************************         
                                                                                
EDTDPT   J     *+12                                                             
         DC    CL8'*EDTDPT*'                                                    
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R2,LP_AINP                                                       
         OC    0(L'NUKDP,R2),0(R2)                                              
         JZ    EXITN                                                            
         L     R4,LP_AOUT                                                       
         TM    INDS,INDS2CDP       TEST 2 CHARACTER DAYPARTS IN USE             
         JNZ   EDTDPT02            YES                                          
         MVC   0(L'NBACTDP,R4),0(R2)                                            
         LHI   R3,L'NBACTDP                                                     
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTDPT02 MVC   WORK(L'NBKEY),NBKEY                                              
         XC    NDPTKEY,NDPTKEY                                                  
         MVI   NDPTKTY,NDPTKTYQ                                                 
         MVI   NDPTKST,NDPTKSTQ                                                 
         MVC   NDPTAGM,NBACTAM                                                  
         MVC   NDPTDPTE,0(R2)                                                   
         TM    NDPTDPTE,X'80'                                                   
         JNZ   *+10                                                             
         MVC   NDPTCLT,NBACTCLI                                                 
         MVC   KEYSAVE,NDPTKEY                                                  
         GOTOR HIGH,NBFILUNT                                                    
         MVI   0(R4),C'?'                                                       
         LHI   R3,1                                                             
         CLC   NDPTKEY(NDPTDES-NDPTKEY),KEYSAVE                                 
         JNE   *+14                                                             
         MVC   0(L'NDPTDPTA,R4),NDPTDPTA                                        
         LHI   R3,L'NDPTDPTA                                                    
         STCM  R3,15,LP_OLEN                                                    
         MVC   NBKEY,WORK                                                       
         J     EXITY                                                            
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RESOLVE CLIENT                                           *         
***********************************************************************         
                                                                                
RESCLT   NTR1  LABEL=N                                                          
         MVC   KEYSAVE,NBKEY                                                    
         MVC   NBACTCLI,0(R1)                                                   
                                                                                
         LA    R3,NBKEY                                                         
         USING CLTHDR,R3                                                        
         XC    CKEY,CKEY                                                        
         MVC   CKEYAM,NBACTAM                                                   
         MVC   CKEYCLT,NBACTCLI                                                 
         GOTOR READ,NBFILSPT                                                    
         JNE   RESCLTN                                                          
         GOTOR GETREC,NBACLI       GET CLIENT RECORD                            
         L     R1,ALP                                                           
         CLC   MAPSTAT,LP_QMAPN-LP_D(R1)                                        
         JE    EXITY                                                            
                                                                                
         L     R3,NBACLI                                                        
         MVC   NBEFFOFF,COFFICE    SET OFFICE                                   
                                                                                
         GOTOR NBCLUNPK,DMCB,(CPROF+6,NBACTCLI),NBCLICOD                        
                                                                                
         XC    WORK,WORK           GET USER PROFILE INTO NBUSER                 
         MVI   WORK+00,SPTLETQ                                                  
         MVI   WORK+01,C'0'        GET N0 PROFILE INTO NBUSER                   
         MVI   WORK+02,C'N'                                                     
         MVI   WORK+03,C'0'                                                     
         MVC   WORK+04(L'NBSELAGY),NBSELAGY                                     
         MVI   WORK+06,NETMEDQ                                                  
         MVC   WORK+07(L'NBCLICOD),NBCLICOD                                     
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF                                              
         GOTOR NBGTPROF,DMCB,WORK,NBUSER,NBDM                                   
         MVI   WORK+3,C'1'         GET N1 PROFILE INTO NBUSER1                  
         GOTOR NBGTPROF,DMCB,WORK,NBUSER1,NBDM                                  
         MVI   WORK+3,C'2'         GET N2 PROFILE INTO NBUSER2                  
         GOTOR NBGTPROF,DMCB,WORK,NBUSER2,NBDM                                  
         MVI   WORK+3,C'3'         GET N3 PROFILE INTO NBUSER2                  
         GOTOR NBGTPROF,DMCB,WORK,TMPUSER3,NBDM                                 
                                                                                
         CLI   ERTG,ERTGPRF        SET EQUIV. RATINGS OVERRIDE                  
         JE    RESCLT02                                                         
         MVI   NBUSER2+0,30        SET EQUIVALENCED VALUE REQUIRED              
         CLI   ERTG,ERTGEQV                                                     
         JE    RESCLT02                                                         
         MVI   NBUSER2+0,0         SET RAW VALUE REQUIRED                       
                                                                                
RESCLT02 CLI   EIMP,EIMPPRF        SET EQUIV. IMPRESSIONS OVERRIDE              
         JE    RESCLTY                                                          
         MVI   NBUSER+1,30         SET EQUIVALENCED VALUE REQUIRED              
         CLI   EIMP,EIMPEQV                                                     
         JE    RESCLTY                                                          
         MVI   NBUSER+1,0          SET RAW VALUE REQUIRED                       
                                                                                
RESCLTY  LA    R0,LASTS            SET LAST TIMES TO HIGH VALUES                
         LHI   R1,LASTSL                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,8,EFFS                                                        
         MVCL  R0,RE                                                            
         MVI   LEST,0              ESTIMATE CAN BE 255                          
         MVI   LPACKAGE,0          PACKAGE CAN BE 255                           
         MVC   LGETCLT,NBACTCLI    SET CURRENT CLIENT CODE                      
         MVC   NBKEY,KEYSAVE                                                    
         J     EXITY                                                            
                                                                                
RESCLTN  MVC   NBKEY,KEYSAVE                                                    
         J     EXITN                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE SAVED WORKING STORAGE VALUES                             *         
***********************************************************************         
                                                                                
INIWRK   STM   RE,R1,12(RD)                                                     
                                                                                
         LA    R0,SAVECLR          CLEAR DOWN SAVE AREA                         
         LHI   R1,SAVECLRL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         BASR  RE,0                                                             
         AHI   RE,LVALUES-*                                                     
         LA    R0,SVALUES          MOVE LITERALS TO SAVED STORAGE               
         LHI   R1,SVALUESL                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    RF,RELOLST          RELOCATE ADDRESS CONSTANTS                   
         LHI   R0,RELOLSTN                                                      
         BASR  RE,0                                                             
         L     R1,0(RF)                                                         
         A     R1,RELO                                                          
         ST    R1,0(RF)                                                         
         AHI   RF,L'RELOLST                                                     
         BCTR  R0,RE                                                            
                                                                                
         LA    R0,SAVED                                                         
         AHI   R0,ESTTAB-SAVED                                                  
         ST    R0,AESTTAB          SET A(ESTIMATE TABLE)                        
                                                                                
         L     R1,ALP              GET A(DDLINK PARAMETER LIST)                 
         USING LP_D,R1                                                          
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
         ST    R0,AIO1                                                          
B#AGY    EQU   4                   AGENCY RECORD                                
         ST    R0,LP_BLKS+((B#AGY-1)*L'LP_BLKS)                                 
         AHI   R0,L'IO1                                                         
         ST    R0,AIO2                                                          
B#CLT    EQU   5                   CLIENT RECORD                                
B#PRD    EQU   5                   PRODUCT RECORD (MFM)                         
B#STA    EQU   5                   STATION RECORD (MFM)                         
         ST    R0,LP_BLKS+((B#CLT-1)*L'LP_BLKS)                                 
         AHI   R0,L'IO2                                                         
         ST    R0,AIO3             (ESTIMATE RECORD)                            
         AHI   R0,L'IO3                                                         
         ST    R0,AIO4                                                          
*                                                                               
         LA    R0,WORKD            6K UNTFILE                                   
         AHI   R0,IO4-WORKD                                                     
         ST    R0,AIO4                                                          
B#UNIT   EQU   6                   UNIT RECORD                                  
B#GOAL   EQU   6                   GOAL RECORD                                  
         ST    R0,LP_BLKS+((B#UNIT-1)*L'LP_BLKS)                                
B#LP     EQU   7                   LP_D (FOR MASK DOWNLOAD)                     
         MVC   LP_BLKS+((B#LP-1)*L'LP_BLKS)(L'LP_BLKS),ALP                      
B#MEDTAB EQU   8                                                                
         BASR  RE,0                                                             
         AHI   RE,MEDTAB-*                                                      
         STCM  RE,15,LP_BLKS+((B#MEDTAB-1)*L'LP_BLKS)                           
                                                                                
INIWRKX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* UNIVSERSE ONLY LOOKUP                                               *         
***********************************************************************         
                                                                                
UNIVLKUP NTR1  BASE=*,LABEL=*                                                   
         CLI   REQUNIV,YESQ        GET UNIVERSES?                               
         JE    *+12                                                             
         CLI   REQUNIV,C'H'        GET HISPANIC UNIVERSES?                      
         JNE   EXITN                                                            
                                                                                
         LA    RF,UNIVBLK                                                       
         ST    RF,NDAUBLOK                                                      
         MVI   NBESTOPT,0                                                       
         MVI   NBACTOPT,YESQ       ALWAYS REQUEST ACTUALS                       
         SR    R1,R1                                                            
         ICM   R1,3,NDEMOS                                                      
         AHI   R1,1                ALWAYS HAVE TO PASS HOMES                    
         STCM  R1,3,NDEMOS                                                      
                                                                                
         CLI   REQUNIV,C'H'        GET HISPANIC UNIVERSES?                      
         JNE   *+12                                                             
         MVI   NBSURVEY,C'H'                                                    
         MVI   NBPOSTYP,C'H'                                                    
                                                                                
         GOTOR NBNETVAL,DMCB,NETIOD                                             
         J     EXITY                                                            
                                                                                
***********************************************************************         
* INITIALIZE NETBLOCK VALUES                                          *         
***********************************************************************         
                                                                                
ININET   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   INDS,0              INITIALIZE INDICATOR FILETYPE                
         XC    REQVALS(REQVALL),REQVALS                                         
         MVC   ENDDATE,EFFS                                                     
                                                                                
         LA    R0,NETBLOCK         INITIALIZE NETBLOCK FOR PROCESSING           
         LHI   R1,NETBLKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,ALP              GET A(DDLINK PARAMETER LIST)                 
         MVC   AGENCY,LP_AGY-LP_D(RF)                                           
         MVC   NBSELAGY,AGENCY                                                  
         MVC   NBAAGY,LP_BLKS-LP_D+((B#AGY-1)*L'LP_BLKS)(RF)                    
         MVC   NBACLI,LP_BLKS-LP_D+((B#CLT-1)*L'LP_BLKS)(RF)                    
         MVC   NBAIO,LP_BLKS-LP_D+((B#UNIT-1)*L'LP_BLKS)(RF)                    
                                                                                
         MVC   NBACOM,RCOMFACS                                                  
         MVI   NBTRCOPT,NOQ        SET TRACE=N (ONLINE DEFAULT)                 
         MVC   NDDEMOS(DEMHOMEL),DEMHOMES                                       
         OI    NBVARIND,NBVARUDQ   DON'T READ ESTIMATES                         
                                                                                
         L     RF,RSYSFACS                                                      
         MVC   VCLPACK,FCLPACK-FACD(RF)                                         
         MVC   NBCLPACK,FCLPACK-FACD(RF)                                        
         MVC   NBCLUNPK,FCLUNPK-FACD(RF)                                        
         MVC   NBNETVAL,FNETVALU-FACD(RF)                                       
         MVC   NBDEMCON,FDEMOCON-FACD(RF)                                       
         MVC   VOFFICER,FOFFICER-FACD(RF)                                       
         MVC   VTRPACK,FTRPACK-FACD(RF)                                         
         MVC   VCFMIO,FCFMIO-FACD(RF)                                           
                                                                                
         LA    R0,NDDEMBLK                                                      
         ST    R0,NBADEM           SET A(DEMO LOOKUP BLOCK)                     
                                                                                
         L     R0,AIO4                                                          
         AHI   R0,L'IO4                                                         
         ST    R0,NBEXTEND         A(EXTEND BLOCK)                              
                                                                                
         L     RF,RCOMFACS                                                      
         USING COMFACSD,RF         RF=A(COMFACS)                                
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
         MVC   VXSORT,CXSORT                                                    
         DROP  RF                                                               
                                                                                
         GOTOR NBCALLOV,DMCB,0,X'D9000A26'                                      
         MVC   NBDEFINE,DMCB                                                    
                                                                                
         LA    RF,L'DOVIND1        DEMO OVERRIDE INDICATOR                      
         LA    RE,DOVIND1                                                       
ININET02 MVI   0(RE),C'N'          INITIALIZE TO NO                             
         AHI   RE,1                                                             
         BCT   RF,ININET02                                                      
                                                                                
         LA    RF,L'DOVIND2        DEMO OVERRIDE INDICATOR                      
         LA    RE,DOVIND2                                                       
ININET04 MVI   0(RE),C'N'          INITIALIZE TO NO                             
         AHI   RE,1                                                             
         BCT   RF,ININET04                                                      
                                                                                
ININETX  J     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE AGENCY VALUES                                            *         
***********************************************************************         
                                                                                
INIAGY   NTR1  LABEL=*                                                          
         L     RF,ALP              GET A(DDLINK PARAMETER LIST)                 
         MVC   NBSELAGY,LP_AGY-LP_D(RF)                                         
         LA    R3,NBKEY            RESOLVE AGENCY/MEDIA CODE                    
         USING AGYKEY,R3                                                        
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,AGYKTYPQ                                                
         MVC   AGYKAGY,NBSELAGY                                                 
         GOTOR READ,NBFILSPT                                                    
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GETREC,NBAAGY                                                    
         L     R3,NBAAGY                                                        
         TM    AGYFLAG2,AGYFLAG2_2DP+AGYFLAG2_BDP                               
         JZ    *+8                                                              
         OI    NBINDS3,NBI3A2DC                                                 
                                                                                
         LA    R2,AGYEL                                                         
         USING AGYMEDEL,R2                                                      
         SR    R0,R0                                                            
INIAGY02 CLI   AGYMEDEL,0                                                       
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   AGYMEDEL,AGYMEDEQ                                                
         JNE   *+12                                                             
         CLI   AGYMEDCD,NETMEDQ    TEST NETPAK MEDIA CODE                       
         JE    *+14                                                             
         IC    R0,AGYMEDLN                                                      
         AR    R2,R0                                                            
         J     INIAGY02                                                         
         MVC   NBACTAM,AGYMEDBT    SET AGENCY/MEDIA CODE                        
                                                                                
         XC    NDPTKEY,NDPTKEY                                                  
         MVI   NDPTKTY,NDPTKTYQ                                                 
         MVI   NDPTKST,NDPTKSTQ                                                 
         MVC   NDPTAGM,NBACTAM                                                  
         MVC   KEYSAVE,NDPTKEY                                                  
         GOTOR HIGH,NBFILUNT       DO READ HIGH FOR THE KEY                     
         CLC   NDPTKEY(NDPTCLT-NDPTKEY),KEYSAVE                                 
         JNE   INIAGYX                                                          
         OI    INDS,INDS2CDP                                                    
                                                                                
INIAGYX  J     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET COMSCORE LICENSE                                                *         
***********************************************************************         
                                                                                
GETLIC   NTR1  LABEL=*                                                          
         LA    R3,NBKEY            RESOLVE AGENCY/MEDIA CODE                    
         USING CT5KEY,R3                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,NBSELAGY                                                
         GOTOR READ,NBFILCTL                                                    
         JNE   *+2                                                              
*                                                                               
         MVC   NBSECAGY,NBSELAGY                                                
*                                                                               
         LA    R3,IOAREA                                                        
         LA    R5,CT5DATA          POINT TO THE FIRST ELEMENT IN REC            
         USING CTSEAD,R5                                                        
GLIC02   CLI   0(R5),0                                                          
         JE    GLIC04                                                           
         CLI   0(R5),CTSEAELQ      ALREADY ON SECURITY ALPHA ELEM?              
         JE    *+16                YES                                          
         ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         J     GLIC02                                                           
         MVC   NBSECAGY,CTSEAAID                                                
         DROP  R5                                                               
*                                                                               
GLIC04   LA    R3,NBKEY                                                         
         USING TOKRECD,R3                                                       
         XC    TOKKEY,TOKKEY                                                    
         MVI   TOKKMIN,TOKKMINQ                                                 
         MVI   TOKKTYP,TOKKRTRK                                                 
         MVC   TOKKSAGY,NBSECAGY   SECURITY AGENCY ALPHA                        
         MVC   TOKKAAGY,NBSELAGY   AND AGENCY ALPHA                             
         MVI   TOKKSYS,X'03'       NET SYSTEM                                   
         GOTOR READ,NBFILGEN                                                    
         JNE   GETLICX                                                          
         CLC   NBKEY(L'TOKKEY),IOAREA                                           
         JNE   GETLICX                                                          
         GOTOR GETREC,AIO3                                                      
*                                                                               
         L     R3,AIO3                                                          
         AHI   R3,TOKFIRST                                                      
         USING RTAUTHD,R3                                                       
         CLI   0(R3),RTAUTELQ      X'0A' ELEMENT                                
         JNE   GETLICX                                                          
         OC    RTAUTID(L'RTAUTID+L'RTAUTSEC),RTAUTID                            
         JZ    GETLICX                                                          
         MVC   COMLIC,RTAUTID      SAVE USER TOKEN FOR DEMOVAL                  
*                                                                               
GETLICX J      EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALIZE REQUEST                                       *         
***********************************************************************         
                                                                                
INIREQ   NTR1  LABEL=*                                                          
                                                                                
         ICM   RF,15,RMASTC                                                     
         JZ    *+16                                                             
         MVC   NBTRCOPT,MCTRACE-MASTD(RF)                                       
         MVC   NBPRINT,MCVPRINT-MASTD(RF)                                       
                                                                                
         L     R1,ALP              SET LIMIT ACCESS BYTES                       
         MVC   NBTWAACC,LP_ACCS-LP_D(R1)                                        
         LA    R0,LASTS                                                         
         LHI   R1,LASTSL+LNETSL                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLC   MAP,MAPUNIT         TEST UNIT DOWNLOAD                           
         JNE   INIREQ40            NO - ALL DONE                                
                                                                                
         OI    NBINDS,X'80'        FORCE EQUIVALENCE OVERRIDES                  
                                                                                
         CLI   CLTIND,LW_TALLQ     TEST 'ALL' CLIENT REQUEST                    
         JNE   INIREQ08                                                         
         OC    NBTWAACC,NBTWAACC   TEST FOR LIMIT ACCESS                        
         JZ    INIREQ08                                                         
                                                                                
         SR    RF,RF                                                            
         ICM   RF,7,ACLT           SAVE A(MAP ENTRY)                            
         L     R7,ALP              POINT TO END OF WORK POOL                    
         L     R7,LP_AWMP-LP_D(R7)                                              
         USING LW_D,R7                                                          
         STCM  R7,7,ACLT                                                        
         MVC   LW_D(LW_LN1Q),0(RF)                                              
         MVI   LW_TYPE,LW_TLSTQ    SET THIS IS A LIST                           
                                                                                
         AHI   R7,LW_LN2Q          R7=A(LIST OF CLIENTS)                        
         SR    R0,R0               R0=COUNT OF CLIENTS                          
         LA    R3,NBKEY                                                         
         USING CLTHDR,R3                                                        
         XC    CKEY,CKEY                                                        
         MVC   CKEYAM,NBACTAM                                                   
                                                                                
INIREQ04 ICM   R1,3,CKEYCLT        SKIP TO NEXT CLIENT                          
         AHI   R1,1                                                             
         STCM  R1,3,CKEYCLT                                                     
         GOTOR HIGH,NBFILSPT       GET NEXT CLIENT RECORD                       
         CLC   CKEY(CKEYCLT-CKEY),NBKEYLST                                      
         JNE   INIREQ06                                                         
         GOTOR GETREC,NBACLI       GET CLIENT RECORD                            
                                                                                
         OC    NBTWAACC,NBTWAACC                                                
         JZ    *+12                                                             
         GOTOR FLTACS              LIMIT ACCESS FILTER                          
         JNE   INIREQ04                                                         
                                                                                
         MVC   0(L'CKEYCLT,R7),CKEYCLT                                          
         AHI   R7,L'CKEYCLT                                                     
         AHI   R0,1                BUMP N'CLIENTS                               
         J     INIREQ04                                                         
         DROP  R3                                                               
                                                                                
INIREQ06 LTR   R0,R0               TEST ANY CLIENTS FOUND                       
         JNZ   *+12                                                             
         MVI   NBERROR,NBINVCLI    NO - RETURN ERROR                            
         J     EXITN                                                            
                                                                                
         L     RF,ALP                                                           
         ST    R7,LP_AWMP-LP_D(RF) SET A(NEXT WMP ENTRY)                        
         SR    RF,RF                                                            
         ICM   RF,7,ACLT                                                        
         STCM  R0,3,LW_NUMN-LW_D(RF)                                            
         MVI   CLTIND,LW_TLSTQ                                                  
                                                                                
INIREQ08 CLI   PRDIND,LW_TALLQ     TEST 'ALL' PRODUCTS                          
         JE    INIREQ10                                                         
         SR    RF,RF               TEST 'POL' PRODUCT REQUEST                   
         ICM   RF,7,APRD                                                        
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   LW_DATA1-LW_D(RF),POLPRDN                                        
         JNE   INIREQ10                                                         
         L     R1,ALP              YES - DUMMY UP 'ALL' ENTRY FOR POL           
         L     R7,LP_AWMP          GET SPACE FOR DUMMY ENTRY                    
         STCM  R7,7,APRD                                                        
         MVC   LW_D(LW_LN1Q),0(RF) COPY ENTRY TO NEW LOCATION                   
         MVI   LW_TYPE,LW_TALLQ                                                 
         MVI   LW_DATA1,0                                                       
         MVI   LW_DATA1+L'NUPRD,FF                                              
         AHI   R7,LW_LN1Q+(L'NUPRD*2)                                           
         ST    R7,LP_AWMP                                                       
         MVI   PRDIND,LW_TALLQ                                                  
                                                                                
INIREQ10 CLI   SUBIND,0            TEST SPECIFIC SUBLINE REQUEST                
         JNE   INIREQ12                                                         
         L     R1,ALP                                                           
         L     R7,LP_AWMP                                                       
         STCM  R7,7,ASUB                                                        
         MVI   LW_TYPE,LW_TRNGQ                                                 
         MVI   LW_DATA1,0                                                       
         MVI   LW_DATA1+L'NUKSUB,NUKSTRAQ-1                                     
         AHI   R7,LW_LN1Q+(L'NUKSUB*2)                                          
         ST    R7,LP_AWMP                                                       
         DROP  R7                                                               
                                                                                
INIREQ12 XC    NBKEY,NBKEY         SET INITIAL KEY VALUE FOR SETKEY             
                                                                                
         LA    R0,NDKEYT                                                        
         CLI   ESTIND,LW_TALLQ                                                  
         JE    *+12                ONLY USE IF NOT ALL ESTIMATES                
         CLI   PRGIND,LW_TALLQ     AND THERE IS NO PROGRAM FILTER               
         JNZ   INIREQ14                                                         
         LA    R0,NPKEYT                                                        
         TM    NWKIND,LW_TLSTQ+LW_TSINQ                                         
         JNZ   INIREQ14                                                         
         LA    R0,NUKEYT                                                        
                                                                                
INIREQ14 ST    R0,AKEYTAB                                                       
                                                                                
         OC    DEMIND,DEMIND       ANY DEMOS PASSED?                            
         JZ    INIREQ19                                                         
         ICM   RF,7,ADEM                                                        
         USING LW_D,RF                                                          
         CLI   LW_TYPE,LW_TSINQ    SINGLE ENTRY?                                
         JNE   *+14                                                             
         MVC   NDEMOS,=H'1'                                                     
         J     *+10                                                             
         MVC   NDEMOS,LW_NUMN      NO - MULTIPLE ENTRIES                        
         J     INIREQ19                                                         
         DROP  RF                                                               
*&&DO                                                                           
*                                                                               
* OLD WAY OF VALIDATING DEMOS - REMOVED 11/17 SCHT                              
*                                                                               
INIREQ15 LA    R1,NDDEMOS+3                                                     
         SR    R0,R0                                                            
         LHI   RE,MAXDEMS                                                       
         MVI   DEMOFLAG,0          SET NO USER DEMO(S) REQUESTED                
INIREQ16 OC    0(3,R1),0(R1)                                                    
         JZ    INIREQ18                                                         
         CLI   1(R1),USERDEMO                                                   
         JNE   *+8                                                              
         OI    DEMOFLAG,DEMOFUSR   SET USER DEMO(S) REQUESTED                   
         AHI   R0,1                                                             
         AHI   R1,3                                                             
         BRCT  RE,INIREQ16                                                      
INIREQ18 STH   R0,NDEMOS           SET N'DEMOS FOR THIS REQUEST                 
*&&                                                                             
                                                                                
INIREQ19 SR    R2,R2                                                            
         ICM   R2,7,AMED           TEST/SET MEDIA FILTER POINTER                
         JZ    INIREQ28                                                         
         USING LW_D,R2             R2=A(MEDIA WMP ENTRY)                        
         LHI   R1,L'STYPE                                                       
         LA    RF,LW_DATA1                                                      
         TM    LW_TYPE,LW_TSINQ    TEST SINGLE MEDIA FILTER                     
         JNZ   *+12                                                             
         ICM   R1,3,LW_NUMN        NO - MUST BE A LIST OF VALUES                
         LA    RF,LW_DATA2                                                      
         BASR  RE,0                                                             
         EX    R1,8(RE)            BUILD LIST OF MEDIA FILTERS IN WORK          
         J     *+10                                                             
         XC    WORK(0),WORK                                                     
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   WORK(0),0(RF)       EXTRACT MEDIA LIST INTO WORK                 
                                                                                
         L     R1,ALP              BUILD A LIST OF NETWORKS                     
         L     R2,LP_AWMP          R2=A(NEXT SLOT IN WMP)                       
         STCM  R2,7,ANWK                                                        
         XC    LW_D(LW_LN2Q),LW_D                                               
         SR    R0,R0               R0=N'NETWORK LIST ENTRIES                    
         LA    R2,LW_DATA2         R2=NETWORK LIST                              
         MVI   LP_RMODE-LP_D(R1),LP_RFRST                                       
                                                                                
INIREQ20 GOTOR NXTREC,DMCB,('YESQ',STAKEYT),('B#STA',0),               *        
               ('NBFILSTA',SAVED),0,0                                           
         JNE   INIREQ26                                                         
         L     R1,IOADDR                                                        
         USING STAREC,R1           R1=A(STATION RECORD)                         
         LA    RF,WORK             RF=A(LIST OF MEDIA FILTERS)                  
INIREQ22 CLI   0(RF),0             TEST END OF MEDIA FILTERS                    
         JE    INIREQ20                                                         
         CLC   STYPE,0(RF)         MATCH MEDIA FILTER TO STATION TYPE           
         JE    INIREQ24                                                         
         AHI   RF,L'STYPE                                                       
         J     INIREQ22            NO MATCH - GET NEXT STATION                  
                                                                                
INIREQ24 MVC   0(L'NUKNET,R2),STAKCALL                                          
         AHI   R2,L'NUKNET         BUMP LIST POINTER                            
         AHI   R0,1                AND NUMBER OF ENTRIES                        
         J     INIREQ20                                                         
         DROP  R1                                                               
                                                                                
INIREQ26 ICM   R2,7,ANWK                                                        
         STCM  R0,3,LW_NUMN        SET N'NETWORKS IN THE LIST                   
         MHI   R0,L'NUKNET         CALCULATE ELEMENT LENGTH                     
         AHI   R0,LW_DATA2-LW_D                                                 
         STCM  R0,3,LW_LN                                                       
         MVI   LW_TYPE,LW_TLSTQ    SET THIS IS A LIST                           
         MVC   NWKIND,LW_TYPE                                                   
         DROP  R2                                                               
                                                                                
         AR    R2,R0               R2=A(NEXT WMP SLOT)                          
         L     R1,ALP                                                           
         ST    R2,LP_AWMP-LP_D(R1) SET ADDRESS IN LP_AWMP                       
                                                                                
INIREQ28 CLI   RECORD,RECGOAL      TEST GOAL ONLY DOWNLOAD                      
         JE    *+12                                                             
         CLI   RECORD,RECBOTH      OR GOALS AND UNITS DOWNLOAD                  
         JNE   INIREQ38                                                         
                                                                                
         OC    STRDATE,STRDATE     TEST START DATE GIVEN                        
         JZ    INIREQ30                                                         
         GOTOR NBDATCON,DMCB,(2,STRDATE),(0,WORK)                               
         GOTOR NBGETDAY,DMCB,WORK,WORK+6                                        
         MVC   STRDMON,STRDATE                                                  
         CLI   0(R1),1             TEST START DATE IS A MONDAY                  
         JE    INIREQ30                                                         
         SR    R0,R0               NO - GET DATE OF PREVIOUS MONDAY             
         ICM   R0,1,0(R1)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SHI   R0,1                                                             
         LNR   R0,R0                                                            
         GOTOR NBADDAY,DMCB,WORK,WORK+6,(R0)                                    
         GOTOR NBDATCON,DMCB,WORK+6,(2,STRDMON)                                 
                                                                                
INIREQ30 BASR  R2,0                                                             
         AHI   R2,MEDTAB-*                                                      
         USING MEDTABD,R2          R2=A(MEDIA TABLE)                            
         LA    R3,MKTLST           R3=A(MARKET LIST)                            
         SR    R4,R4               R4=N'MARKETS IN MARKET LIST                  
INIREQ32 CLC   MEDTGMKT,BZEROES    TEST GOAL MARKET SET                         
         JE    INIREQ36                                                         
         CLI   MEDTMED,MEDTMALL    ALWAYS SEND ALL MARKET GOALS                 
         JE    INIREQ34                                                         
         ICM   RF,7,AMED           RF=A(SUB-MEDIA FILTERS)                      
         JZ    INIREQ34                                                         
         USING LW_D,RF                                                          
         LHI   R0,1                                                             
         LA    R1,LW_DATA1                                                      
         TM    LW_TYPE,LW_TSINQ    TEST SINGLE VALUE                            
         JNZ   *+12                                                             
         ICM   R0,3,LW_NUMN                                                     
         LA    R1,LW_DATA2         ELSE IS A LIST                               
         BASR  RE,0                                                             
         CLC   MEDTMED,0(R1)       MATCH TO SUB-MEDIA FILTER                    
         JE    INIREQ34                                                         
         AHI   R1,L'MEDTMED                                                     
         BCTR  R0,RE                                                            
         J     INIREQ36                                                         
                                                                                
INIREQ34 MVC   0(L'MKTLST,R3),MEDTGMKT                                          
         AHI   R3,L'MKTLST                                                      
         AHI   R4,1                                                             
                                                                                
INIREQ36 AHI   R2,MEDTABL          BUMP TO NEXT MEDIA TABLE ENTRY               
         CLI   MEDTMED,MEDTMEOT                                                 
         JNE   INIREQ32                                                         
         STCM  R4,3,MKTNUM         SET NUMBER OF GOAL MARKETS IN LIST           
         DROP  R2                                                               
                                                                                
INIREQ38 CLI   RECORD,RECGOAL      TEST GOAL ONLY DOWNLOAD                      
         JE    INIREQX                                                          
         MVI   NBESTOPT,NBESTOUQ   SET ESTIMATED DEMOS REQUIRED                 
         OC    NDEMOS,NDEMOS       TEST ANY DEMOS REQUESTED                     
         JZ    INIREQX                                                          
         MVI   NBESTOPT,0                                                       
         CLI   RFLAVOR,RFLAACT     TEST ACTUAL DEMOS REQUIRED                   
         JNE   *+12                                                             
         MVI   NBACTOPT,YESQ       SET ACTUAL DEMOS REQUIRED                    
         J     INIREQX                                                          
         MVI   NBESTOPT,NBESTOUQ   SET ESTIMATED DEMOS REQUIRED                 
         J     INIREQX                                                          
                                                                                
INIREQ40 CLC   MAP,MAPSTAT         TEST STATS DOWNLOAD                          
         JNE   INIREQX                                                          
         OC    STRDATEE,STRDATEE   TEST START DATE GIVEN                        
         JZ    INIREQ42                                                         
         GOTOR NBDATCON,DMCB,STRDATEE,(2,STRDATE)                               
                                                                                
INIREQ42 OC    ENDDATEE,ENDDATEE                                                
         JZ    INIREQX                                                          
         GOTOR NBDATCON,DMCB,ENDDATEE,(2,ENDDATE)                               
                                                                                
INIREQX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* APPLY LIMIT ACCESS FILTERS                                          *         
***********************************************************************         
                                                                                
FLTACS   NTR1  LABEL=*                                                          
         L     R3,NBACLI                                                        
         USING CLTHDR,R3           R3=A(CLIENT RECORD)                          
         GOTOR NBCLUNPK,DMCB,(CPROF+6,CKEYCLT),CLTCODE                          
         USING OFFICED,OFFCWORK    OFFICER CONTROL BLOCK                        
         XC    OFFICED(OFCLENQ),OFFICED                                         
         L     R1,ALP                                                           
         USING LP_D,R1             R1=A(LP)                                     
         MVI   OFCSYS,NETMEDQ                                                   
         MVC   OFCAGY,LP_AGY                                                    
         MVC   OFCAUTH,LP_ACCS                                                  
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,CLTCODE                                                   
         MVC   OFCSAGMD,CKEYAM                                                  
         MVC   OFCLMT,LP_ACCS                                                   
         MVC   OFCSECD,LP_ASECD                                                 
         MVC   OFCACCSC(L'CACCESS),CACCESS                                      
         MVI   OFCACCSM,FF                                                      
         MVI   OFCINDS,OFCI2CSC                                                 
         MVC   OFCCLT2,CKEYCLT                                                  
         GOTOR VOFFICER,DMCB,(C'N',OFFICED),NBACOM,OFFCSAVE                     
         J     EXIT                NOTE: CONDITION CODE SET BY OFFICER          
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* APPLY FILTERS TO UNIT RECORD                                        *         
***********************************************************************         
                                                                                
FLTUNT   NTR1  BASE=*,LABEL=*                                                   
         MVI   NBFILE,NBFILUNT                                                  
         MVI   NBSURVEY,0          SET UNKNOWN TYPE                             
         GOTOR GETREC,NBAIO                                                     
         L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
                                                                                
         OC    DTETIM,DTETIM       TEST DATE/TIME STAMP                         
         BZ    *+14                                                             
         CLC   NURLINK,DTETIM                                                   
         BNH   FLTUNTN                                                          
                                                                                
         TM    NUPACKST,NUPANSDQ   TEST NO-SHOW PACKAGE                         
         BNZ   FLTUNTN                                                          
                                                                                
         SR    RF,RF                                                            
         ICM   RF,7,ADAY                                                        
         CLI   DAYIND,LW_TALLQ     APPLY DAY FILTER (IF NOT ALL)                
         BE    FLTUNT02                                                         
         MVC   WORK(L'NUDAY),NUDAY                                              
         NC    WORK(L'NUDAY),LW_DATA1-LW_D(RF)                                  
         CLC   WORK(L'NUDAY),LW_DATA1-LW_D(RF)                                  
         BNE   FLTUNTN                                                          
                                                                                
FLTUNT02 L     RF,ALP                                                           
         L     RF,LP_ASETK-LP_D(RF)                                             
         LA    R1,DMCB             APPLY PKG/DPT/TIME/LENGTH FILTERS            
         GOTOR (RF),(R1),(1,PKNFLT),NUPACK,SAVED,('FF',ALP)                     
         BNE   FLTUNTN                                                          
         GOTOR (RF),(R1),(1,TIMFLT),NUKTIME,SAVED,('FF',ALP)                    
         BNE   FLTUNTN                                                          
         GOTOR (RF),(R1),(1,DPTFLT),NUKDP,SAVED,('FF',ALP)                      
         BNE   FLTUNTN                                                          
         GOTOR (RF),(R1),(1,LENFLT),NULEN,SAVED,('FF',ALP)                      
         BNE   FLTUNTN                                                          
                                                                                
         L     RF,ALP              CLEAR THE RECORD MASK                        
         XC    LP_RMASK-LP_D(,RF),LP_RMASK-LP_D(RF)                             
                                                                                
         LA    RF,UNITEBCD                                                      
         LA    RE,UNITEBCX-UNITEBCD                                             
         MVI   0(RF),SPACEQ                                                     
         AHI   RF,1                                                             
         BCT   RE,*-8                                                           
                                                                                
         XC    UNITBNRY(UNITBNRL),UNITBNRY                                      
         XC    PRDL(PRDLL),PRDL                                                 
                                                                                
         MVC   PRDLIST(L'NUPRD),NUPRD                                           
         MVC   PRDLIST+PRDLPCT(L'NUP1SHR),NUP1SHR                               
         MVC   PRDLIST+PRDLFDPT(L'NUFEED),NUFEED                                
         CLI   NUPRD2,0                                                         
         BE    FLTUNT3                                                          
         MVC   PRDLIST+PRDLLEN(L'NUPRD2),NUPRD2                                 
         LHI   RE,10000                                                         
         MVC   FULL(2),NUP1SHR                                                  
         SH    RE,FULL                                                          
         STCM  RE,3,PRDLIST+PRDLLEN+PRDLPCT    2ND PRD PCT                      
         LHI   RE,10000                                                         
         MVC   FULL(2),NUFEED                                                   
         SH    RE,FULL                                                          
         STCM  RE,3,PRDLIST+PRDLLEN+PRDLFDPT   2ND PRD FEED PCT                 
FLTUNT3  BRAS  RE,CONVPRD          CONVERT PRODUCTS TO ALPHA                    
         MVC   SUBLINE,NUKSUB      SET SUB-LINE NUMBER                          
         XC    OVAFFTIM,OVAFFTIM                                                
                                                                                
         LA    R3,NBKEY                                                         
         MVC   UNTDA,NUDA          UNIT DISK ADDRESS                            
         L     R3,NBAIO                                                         
                                                                                
         CLC   UNTDTM,NURLINK                                                   
         BH    *+10                                                             
         MVC   UNTDTM,NURLINK      SET LATEST UNIT DATE/TIME STAMP              
                                                                                
         TM    NBINDS2,NBNOBLRD    TEST NOT NEW BILLING                         
         BNZ   FLTUNT06                                                         
         TM    NBINDS2,NBAGYB1S    TEST BLOCK INITIALIZED                       
         BNZ   FLTUNT04                                                         
         XC    NBLBILLD(NBLLENQ),NBLBILLD                                       
         LA    R0,BILBLOCK                                                      
         STCM  R0,15,NBABILRD                                                   
                                                                                
FLTUNT04 MVI   NBLFUNC,NBLBLD      CALL NETBLRDR TO GET BILLING STATUS          
         GOTOR VNBLRDR,DMCB,NETIOD                                              
         TM    NBLFUNC,NBLBILD                                                  
         BZ    FLTUNT06                                                         
         GOTOR SETMSK,MQ#BILL      SET BILLED                                   
                                                                                
FLTUNT06 OC    NUPROGNM,SPACES     ENSURE TRAILING SPACES IN NUPROGNM           
         MVC   NSISRCE,=C'NSI'                                                  
         MVC   COMSRCE,=C'COM'                                                  
                                                                                
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
                                                                                
         MVC   FULL,NUACTUAL                                                    
                                                                                
         LA    R4,NUDATA           PROCESS UNIT RECORD ELEMENTS                 
         SR    R0,R0                                                            
FLTUNT08 CLI   0(R4),0             TEST END OF RECORD                           
         BE    FLTUNT36                                                         
                                                                                
         USING NUSDRD,R4                                                        
         CLI   NUSDREL,NUSDRELQ                                                 
         BNE   FLTUNT10                                                         
                                                                                
         MVC   UNPOSTYP,NUPOSTYP   SET POST TYPE                                
         MVC   NBSURVEY,NUPOSTYP   SET SURVEY TYPE                              
         CLI   NUBKTYP,X'40'                                                    
         BNH   *+10                                                             
         MVC   NBSURVEY,NUBKTYP          BOOK TYPE                              
         MVI   NBHUNOPT,0          SET WANT IMPS IN THOUSANDS                   
         MVI   NBPREOPT,0                                                       
         CLI   NBSURVEY,C'S'                                                    
         BE    FLTUNT09                                                         
         CLI   NBSURVEY,C'H'                                                    
         BE    FLTUNT09                                                         
         CLI   NBSURVEY,C'N'                                                    
         BE    FLTUNT09                                                         
         CLI   NUPOSTYP,C'H'       HISPANIC(NETWORK PRECISION)?                 
         BE    FLTUNT09            YES-NO IMPS IN HUNDREDS                      
                                                                                
         MVI   NBHUNOPT,C'Y'       SET WANT IMPS IN HUNDREDS                    
         MVI   NBPREOPT,C'Y'       AND CABLE RATING TO 2 DECIMALS               
                                                                                
FLTUNT09 MVC   SPRCODE,NUSDSRT     SET SPECIAL RATE CODE                        
         OI    SPRCODE,SPACEQ                                                   
         MVC   STATYPE,NUSTATYP    SET STATION TYPE                             
         OI    STATYPE,SPACEQ                                                   
         MVC   SUBMEDT,NUSDSBMD    SET SUB-MEDIA TYPE                           
         OI    SUBMEDT,SPACEQ                                                   
         MVC   MIRRORC,NUMIRTYP    SET MIRROR CODE                              
         OI    MIRRORC,SPACEQ                                                   
         MVC   TCARLEV,NUSTCAR     SET TCAR LEVEL                               
         OI    TCARLEV,SPACEQ                                                   
         MVC   AFIDATE,NUSDAFDT    SET AFFIDAVIT DATE                           
         TM    NUSDST3,NUSDWUBQ                                                 
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#WDOW      SET WINDOW                                   
         TM    NUSDST3,NUSDADUQ                                                 
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#ADUS      SET ADU                                      
         TM    NUSDST3,NUSDACOQ                                                 
         BZ    *+12                                                             
         GOTOR SETMSK,MQ#ACOV      SET ASSIGNED COST OVERRIDE                   
         B     FLTUNT34                                                         
                                                                                
         USING NUCMLEL,R4                                                       
FLTUNT10 CLI   NUCMLEID,NUCMLEQ                                                 
         BNE   FLTUNT13                                                         
                                                                                
         BRAS  RE,SETNATNL         SET NATIONAL FEED IN PROD TABLE              
                                                                                
         TM    NUCMLFL2,NUCMLFFD                                                
         BZ    *+8                                                              
         OI    PRDSTAT,X'40'       SET NO NATIONAL FLAG                         
         XC    COMCOD1,COMCOD1     COMMERCIAL CODE 1                            
         OC    NUCML1,NUCML1                                                    
         BZ    *+10                                                             
         MVC   COMCOD1(L'NUCML1),=C'REASSIGN   '                                
         TM    NUCMLFLG,NUCULCIQ+NUCUPCIQ                                       
         BNZ   FLTUNT11                                                         
         MVC   COMCOD1(L'NUCML1),NUCML1                                         
         TM    NUCMADFL,NUCMADF1   TEST AD-ID                                   
         BZ    *+12                                                             
         GOTOR EDTAID,COMCOD1                                                   
                                                                                
FLTUNT11 XC    COMCOD2,COMCOD2     COMMERCIAL CODE 2                            
         OC    NUCML2,NUCML2                                                    
         BZ    *+10                                                             
         MVC   COMCOD2(L'NUCML1),=C'REASSIGN   '                                
         TM    NUCMLFLG,NUCULCIQ+NUCUPCIQ                                       
         BNZ   FLTUNT12                                                         
         MVC   COMCOD2(L'NUCML2),NUCML2                                         
         TM    NUCMADFL,NUCMADF2   TEST AD-ID                                   
         BZ    *+12                                                             
         GOTOR EDTAID,COMCOD2                                                   
                                                                                
FLTUNT12 TM    NUCMLFLG,NUCBREQQ                                                
         BZ    FLTUNT34                                                         
         GOTOR SETMSK,MQ#BBRD      SET BILLBOARD                                
         B     FLTUNT34                                                         
                                                                                
         USING NUBILD,R4                                                        
FLTUNT13 CLI   NUBILEL,NUBILELQ                                                 
         BNE   FLTUNT14                                                         
         TM    NBINDS2,NBAGYB1S    TEST NEW BILLING                             
         BNZ   FLTUNT34                                                         
         GOTOR SETMSK,MQ#BILL      SET BILLED                                   
         B     FLTUNT34                                                         
                                                                                
         USING NUPAYD,R4                                                        
FLTUNT14 CLI   NUPAYEL,NUPAYELQ                                                 
         BNE   FLTUNT16                                                         
         GOTOR SETMSK,MQ#PAID      SET PAID                                     
         CLC   VERSION,V2101       TEST VERSION 2.1.0.1 OR HIGHER               
         BL    FLTUNT34                                                         
         CLI   NUPAYTYP,C'T'       TEST TIME CHARGE                             
         BNE   *+16                                                             
         GOTOR SETMSK,MQ#TPAID     YES - SET TIME CHARGE PAID                   
         B     FLTUNT34                                                         
         CLI   NUPAYTYP,C'I'       TEST INTEGRATION CHARGE                      
         BNE   *+16                                                             
         GOTOR SETMSK,MQ#IPAID     YES - SET INTEGRATION CHARGE PAID            
         B     FLTUNT34                                                         
         CLI   NUPAYTYP,C'O'       TEST OTHER CHARGE PAID                       
         BNE   *+12                                                             
         GOTOR SETMSK,MQ#OPAID     YES - SET OTHER CHARGE PAID                  
         B     FLTUNT34                                                         
                                                                                
         USING NUOTH,R4                                                         
FLTUNT16 CLI   NUOTEL,NUOTELQ                                                   
         BNE   FLTUNT18                                                         
         CLI   NUOTTYP,NUOTBTCQ                                                 
         BNE   *+14                                                             
         MVC   BUYTYPE,NUOTHER     SET BUY TYPE                                 
         B     FLTUNT34                                                         
         CLI   NUOTTYP,NUOTSDPQ                                                 
         BNE   *+14                                                             
         MVC   SUBDPRT(1),NUOTHER  SET SUB-DAYPART                              
         B     FLTUNT34                                                         
         CLI   NUOTTYP,NUOTUSDQ                                                 
         BNE   *+14                                                             
         MVC   SUBDPRT,NUOTHER     SET (USER DEFINED) SUB-DAYPART               
         B     FLTUNT34                                                         
         CLI   NUOTTYP,NUOTMLTQ                                                 
         BNE   *+14                                                             
         MVC   MLTIRUN,NUOTHER     SET MULTI-RUN                                
         B     FLTUNT34                                                         
         CLI   NUOTTYP,NUOTRCDQ                                                 
         BNE   *+14                                                             
         MVC   REASONC,NUOTHER     SET REASON CODE                              
         B     FLTUNT34                                                         
         CLI   NUOTTYP,NUOTCBOQ                                                 
         BNE   *+14                                                             
         MVC   COMBOCD,NUOTHER     SET COMBO CODE                               
         B     FLTUNT34                                                         
         CLI   NUOTTYP,NUOTFLTQ                                                 
         BNE   *+14                                                             
         MVC   OVFLTID,NUOTHER     OVERRIDE FLIGHT ID                           
         B     FLTUNT34                                                         
         CLI   NUOTTYP,NUOTPODQ                                                 
         BNE   *+14                                                             
         MVC   POD,NUOTHER         SET POD                                      
         B     FLTUNT34                                                         
         CLI   NUOTTYP,NUOTWABQ                                                 
         BNE   *+14                                                             
         MVC   WINDOW,NUOTHER      WINDOWS ALTERNATE BOOK                       
         B     FLTUNT34                                                         
         CLI   NUOTTYP,NUOTAIRQ                                                 
         BNE   FLTUNT34                                                         
         MVC   AIR,NUOTHER         AIR NETWORK                                  
         B     FLTUNT34                                                         
                                                                                
         USING NUPDED,R4                                                        
FLTUNT18 CLI   NUPDEEL,NUPDEELQ                                                 
         BNE   FLTUNT20                                                         
         TM    NUPDEIND,NUPDCSP    IS UNIT A COPYSPLIT                          
         BZ    *+8                                                              
         OI    PRDSTAT,X'80'       SET UNIT AS COPYSPLIT                        
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
         LA    RE,PRDLIST                                                       
         LA    RF,NUPDEPR          RF=A(PRODUCT LIST IN ELEMENT)                
         USING NUPDEPR,RF                                                       
         MVC   0(L'NUPDEPR,RE),NUPDEPR                                          
         OC    NUPDEPCT(L'NUPDEPCT+L'NUPDEFD),NUPDEPCT                          
         BZ    *+16                                                             
         MVC   PRDLPCT(L'NUPDEPCT,RE),NUPDEPCT                                  
         MVC   PRDLFDPT(L'NUPDEFD,RE),NUPDEFD                                   
         AHI   RE,PRDLLEN                                                       
         AHI   RF,NUPDTLEN                                                      
         BCT   R0,*-36                                                          
         B     FLTUNT34                                                         
         DROP  RF                                                               
                                                                                
         USING NUPRDD,R4                                                        
FLTUNT20 CLI   NUPRDEL,NUPRDELQ                                                 
         BNE   FLTUNT22                                                         
         OI    PRDSTAT,X'80'       SET UNIT AS COPYSPLIT                        
         XC    PRDLIST,PRDLIST                                                  
         SR    R0,R0               SET PRODUCTS                                 
         IC    R0,NUPRDLEN                                                      
         SHI   R0,NUPRDPR-NUPRDD                                                
         SRDA  R0,32                                                            
         LHI   RE,6                                                             
         DR    R0,RE                                                            
         LR    R0,R1               R0=N'PRODUCTS IN LIST                        
         CHI   R0,MAXUPRD                                                       
         BNH   *+8                                                              
         LHI   R0,MAXUPRD                                                       
         LA    RE,PRDLIST                                                       
         LA    RF,NUPRDPR          RF=A(PRODUCT LIST IN ELEMENT)                
         USING NUPRDPR,RF                                                       
         MVC   0(L'NUPRDPR,RE),NUPRDPR                                          
         MVC   PRDLPCT(L'NUPRDPCT,RE),NUPRDPCT                                  
         MVC   PRDLFDPT(L'NUPRDFD,RE),NUPRDFD                                   
         AHI   RE,PRDLLEN                                                       
         AHI   RF,6                                                             
         BCT   R0,*-26                                                          
         B     FLTUNT34                                                         
         DROP  RF                                                               
                                                                                
         USING NUBKD,R4                                                         
FLTUNT22 CLI   NUBKEL,NUBKELEQ                                                  
         BNE   FLTUNT24                                                         
         MVI   BKBASIS,C'N'        SET 'N' FOR EVN                              
         CLI   NUBKFMS+1,C'V'                                                   
         BE    *+8                                                              
         MVI   BKBASIS,C'Y'        SET 'Y' FOR EIN                              
         CLC   NUBKBOOK,NEWBOOK    TEST NEW BOOK FORMAT                         
         BL    *+12                                                             
         GOTOR SETMSK,MQ#NEWRB     YES - SET STATUS BIT FOR IT                  
         B     FLTUNT34                                                         
                                                                                
         USING NUCOMD,R4                                                        
FLTUNT24 CLI   NUCOMEL,NUCOMELQ    TEST COMMENT ELEMENT                         
         BNE   FLTUNT26                                                         
         CLI   NUCOMTYP,NUCOMTCQ   TEST CLIENT COMMENT                          
         BNE   FLTUNT26                                                         
         LA    RF,CLICOM1                                                       
         CLI   NUCOMLIN,1          TEST LINE 1                                  
         BE    *+16                                                             
         LA    RF,CLICOM2                                                       
         CLI   NUCOMLIN,2          TEST LINE 2                                  
         BNE   FLTUNT34                                                         
         SR    RE,RE                                                            
         IC    RE,NUCOMLEN                                                      
         SHI   RE,NUCOMMNT-NUCOMD+1                                             
         CHI   RE,L'CLICOM1-1      ENSURE COMMENT WILL FIT                      
         BNH   *+8                                                              
         LHI   RE,L'CLICOM1-1                                                   
         EX    RE,*+8                                                           
         B     FLTUNT34                                                         
         MVC   0(0,RF),NUCOMMNT                                                 
                                                                                
         USING NUDTAD,R4                                                        
FLTUNT26 CLI   NUDTAEL,NUDTAELQ                                                 
         BNE   FLTUNT28                                                         
         MVC   PROGTPRG,NUDTATYP   PROGRAM TYPE                                 
         MVC   PROGTSUB,NUDTASPT   SUB-PROGRAM TYPE                             
         MVC   NEWORET,NUDTANEW    NEW OR RETURNING PROGRAM                     
         MVC   TIERNUM,NUDTATIR    TIER NUMBER                                  
         MVC   CONTENT,NUDTARAT    CONTENT                                      
         MVC   OVAFFTIM,NUDTGAFD   OVERRIDE AFFID TIME                          
         MVC   INVTIME,NUDTINVN    TIME INVOICE                                 
         MVC   INVINTG,NUDTIIVN    INTEGRATION INVOICE                          
                                                                                
         MVC   VTYPE(L'NUDVTYPE),NUDVTYPE      VTYPE                            
         TM    NUUNST5,NST5PMBM    TEST POD LOOKUP                              
         BZ    *+8                                                              
         MVI   VTYPE+L'NUDVTYPE,C'P'                                            
         B     FLTUNT34                                                         
                                                                                
         USING NUSQD,R4                                                         
FLTUNT28 CLI   NUSQEL,NUSQELQ                                                   
         BNE   FLTUNT29                                                         
         TM    OTHERS,OTHSERQ                                                   
         BZ    *+10                                                             
         MVC   SERIAL#,NUSQSER     SERIAL NUMBER                                
         TM    OTHERS,OTHDEALQ                                                  
         BZ    *+10                                                             
         MVC   DEAL#,NUSQDEAL      DEAL NUMBER                                  
         TM    OTHERS,OTHCONQ                                                   
         BZ    *+10                                                             
         MVC   CON#,NUSQCON        CONTRACT NUMBER                              
         B     FLTUNT34                                                         
                                                                                
         USING NUPCELD,R4                                                       
FLTUNT29 CLI   NUPCEID,NUPCELQ                                                  
         BNE   FLTUNT30                                                         
                                                                                
         CLC   NUPCCML,SPACES      TEST SPACES                                  
         BE    FLTUNT30                                                         
                                                                                
         XC    COMSCOD,COMSCOD     COMMERCIAL CODE                              
         MVC   COMSCOD(L'NUPCCML),NUPCCML                                       
         OC    NUPCCML,NUPCCML                                                  
         BNZ   *+10                                                             
         MVC   COMSCOD(L'NUPCCML),=C'REASSIGN   '                               
         TM    NUPCMLFL,NUPCADF1   TEST AD-ID                                   
         BZ    *+12                                                             
         GOTOR EDTAID,COMSCOD                                                   
                                                                                
         USING NUFDCEL,R4                                                       
FLTUNT30 CLI   NUFDCEID,NUFDCELQ                                                
         BNE   FLTUNT32                                                         
                                                                                
         BRAS  RE,SETFEED          GET FEED INFO                                
                                                                                
         USING NUCSELD,R4                                                       
FLTUNT32 CLI   NUCSEL,NUCSELQ                                                   
         BNE   FLTUNT34                                                         
         MVC   CSSER#,NUCSSN       COMSCORE SERIES #                            
         MVC   COMSRCE,=C'COM'                                                  
         MVC   CSVTYPE,=C'RL'                                                   
         CLI   NUCSVTYP,NPKVTRLQ   LIVE?                                        
         JNE   *+10                                                             
         MVC   CSVTYPE,=C'RL'                                                   
         CLI   NUCSVTYP,NPKVTRCQ   LIVE COMMERCIAL?                             
         JNE   *+10                                                             
         MVC   CSVTYPE,=C'RC'                                                   
         CLI   NUCSVTYP,NPKVTR3Q   LIVE + 3?                                    
         JNE   *+10                                                             
         MVC   CSVTYPE,=C'R3'                                                   
         CLI   NUCSVTYP,NPKVTR7Q   LIVE + 7?                                    
         JNE   *+10                                                             
         MVC   CSVTYPE,=C'R7'                                                   
                                                                                
FLTUNT34 IC    R0,1(R4)            BUMP TO NEXT ELEMENT ON RECORD               
         AR    R4,R0                                                            
         B     FLTUNT08                                                         
*                                                                               
FLTUNT36 BRAS  RE,CONVPRD          CONVERT PRODUCTS TO ALPHA                    
*                                                                               
         CLI   PRDIND,LW_TALLQ     TEST ALL PRODUCT REQUEST                     
         BE    FLTUNT48                                                         
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,7,APRDA                                                       
         TM    LW_LN1Q-L'LW_TYPE(R5),LW_TSINQ   SINGLE ENTRY?                   
         JZ    *+12                                                             
         AHI   R5,LW_LN1Q                                                       
         J     *+8                                                              
         AHI   R5,LW_LN2Q                                                       
         CLI   0(R5),0             ANY PRODUCT FILTERS?                         
         BE    FLTUNTY                                                          
         LA    R2,PRDLIST          YES- APPLY PRODUCT FILTERS                   
         LHI   R0,MAXUPRD                                                       
                                                                                
FLTUNT38 CLI   0(R2),0             TEST END OF PRODUCT LIST                     
         BE    FLTUNTN                                                          
                                                                                
         SR    R5,R5                                                            
         ICM   R5,7,APRDA                                                       
         TM    LW_LN1Q-L'LW_TYPE(R5),LW_TSINQ   SINGLE ENTRY?                   
         JZ    *+12                                                             
         AHI   R5,LW_LN1Q                                                       
         J     *+8                                                              
         AHI   R5,LW_LN2Q                                                       
FLTUNT40 CLI   0(R5),0             END OF FILTER LIST                           
         BE    FLTUNT42                                                         
         CLC   0(L'PRDALPH,R2),0(R5)                                            
         BE    FLTUNT48                                                         
         AHI   R5,L'PRDALPH        BUMP TO NEXT PRODUCT IN FILTER LIST          
         B     FLTUNT40                                                         
                                                                                
FLTUNT42 AHI   R2,PRDLLEN          BUMP TO NEXT PRODUCT ON UNIT                 
         BCT   R0,FLTUNT38                                                      
         B     FLTUNTN                                                          
                                                                                
FLTUNT48 GOTOR TSTMSK,MQ#ADUS      TEST ADU UNIT                                
         BNZ   FLTUNT50                                                         
         GOTOR TSTMSK,MQ#PFBS      OR PFB UNIT                                  
         BNZ   FLTUNT50                                                         
         GOTOR SETMSK,MQ#ESTS      NOT ADU OR PFB - SET EST                     
                                                                                
FLTUNT50 L     R1,ALP                                                           
         L     RF,LP_ATSTM-LP_D(R1)                                             
         GOTOR (RF),(R1)           CALL TSTMSK TO SEE IF UNIT QUALIFIES         
         BNE   FLTUNTN                                                          
                                                                                
FLTUNTY  J     EXITY                                                            
                                                                                
FLTUNTN  J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* LOOK UP PRODUCT CODE/NUMBER IN CLIENT RECORD                        *         
***********************************************************************         
                                                                                
GETPRC   MVC   WORK+50(64),KEYSAVE                                              
         MVI   WORK2,0             SET TO LOOK UP PRODUCT CODE                  
         MVC   PRDNUM,0(R1)                                                     
                                                                                
         STM   RE,RF,12(RD)                                                     
         ST    RE,SAVERE2                                                       
                                                                                
         LA    R5,NBKEY                                                         
         USING PRDHDR,R5                                                        
         XC    PLSTPSSV,PLSTPSSV                                                
         MVI   PLSTTYPE,X'0D'                                                   
         MVI   PLSTSUB,X'F1'                                                    
         MVC   PLSTAM,NBACTAM                                                   
         MVC   PLSTCLT,REQCLT                                                   
         CLC   PRDNUM,EFFS                                                      
         JNE   *+10                                                             
         MVC   PLSTXFF,EFFS                                                     
                                                                                
         GOTOR HIGH,NBFILSPT                                                    
         J     GETPRC04                                                         
                                                                                
GETPRC02 GOTOR SEQ,NBFILSPT          READ PRODUCT PASSIVE KEYS                  
GETPRC04 CLI   PLSTTYPE,X'0D'        TO FIND MATCHING EQUATE #                  
         JNE   GETPRAN                                                          
         CLI   PLSTSUB,X'F1'                                                    
         JNE   GETPRAN                                                          
         CLC   PLSTAM,NBACTAM                                                   
         JNE   GETPRAN                                                          
         CLC   PLSTCLT,REQCLT                                                   
         JNE   GETPRAN                                                          
         CLC   PLSTBPRD+1(1),PRDNUM                                             
         JNE   GETPRC02                                                         
                                                                                
         MVC   PRDALPH,PLSTPRD                                                  
         J     GETPRAY                                                          
                                                                                
GETPRN   MVI   WORK2,1             SET TO LOOK UP PRODUCT NUMBER                
         MVC   PRDALPH,0(R1)                                                    
         STM   RE,RF,12(RD)                                                     
         ST    RE,SAVERE2                                                       
                                                                                
         LA    R5,NBKEY                                                         
         USING PRDHDR,R5                                                        
         XC    PLSTPSSV,PLSTPSSV                                                
         MVI   PLSTTYPE,X'0D'                                                   
         MVI   PLSTSUB,X'F1'                                                    
         MVC   PLSTAM,NBACTAM                                                   
         MVC   PLSTCLT,REQCLT                                                   
         CLC   PRDALPH,POL                                                      
         JNE   *+10                                                             
         MVC   PLSTXFF,EFFS                                                     
         MVC   PLSTPRD,PRDALPH                                                  
                                                                                
         GOTOR HIGH,NBFILSPT                                                    
                                                                                
         CLI   PLSTTYPE,X'0D'                                                   
         JNE   GETPRAN                                                          
         CLI   PLSTSUB,X'F1'                                                    
         JNE   GETPRAN                                                          
         CLC   PLSTAM,NBACTAM                                                   
         JNE   GETPRAN                                                          
         CLC   PLSTCLT,REQCLT                                                   
         JNE   GETPRAN                                                          
         CLC   PLSTPRD,PRDALPH                                                  
         JNE   GETPRAN                                                          
                                                                                
         MVC   PRDNUM,PLSTBPRD+1                                                
         J     GETPRAY                                                          
                                                                                
GETPRAN  SR    R1,R1               PRODUCT NOT FOUND                            
         J     GETPRAX                                                          
                                                                                
GETPRAY  LA    R1,PRDALPH          PRODUCT FOUND                                
                                                                                
GETPRAX  LM    RE,RF,12(RD)                                                     
         L     RE,SAVERE2                                                       
         MVC   KEYSAVE,WORK+50                                                  
         LTR   R1,R1               SET CONDITION CODE                           
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* EDIT AD-ID                                                          *         
***********************************************************************         
                                                                                
EDTAID   NTR1  ,                                                                
         LR    RF,R1                                                            
         MVC   WORK(L'NUCML1),0(RF)                                             
         GOTOR VTRPACK,DMCB,(C'U',WORK),(RF)                                    
         J     EXIT                                                             
                                                                                
***********************************************************************         
* TEST RECORD MASK BIT IS ON                                          *         
***********************************************************************         
                                                                                
TSTMSK   STM   RE,R1,12(RD)                                                     
         LR    RE,R1                                                            
         BCTR  RE,0                                                             
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         IC    RF,BITLIST(RF)                                                   
         LA    RE,LP_RMASK-LP_D(RE)                                             
         A     RE,ALP                                                           
         EX    RF,*+8                                                           
         B     TSTMSKX                                                          
         TM    0(RE),0                                                          
TSTMSKX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SET ESTIMATE USER DEMO NAMES                                        *         
***********************************************************************         
                                                                                
SETUSR   NTR1  LABEL=*                                                          
         L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
         SR    R4,R4                                                            
         ICM   R4,1,NUKEST                                                      
         BCTR  R4,0                                                             
         MHI   R4,L'ESTTAB                                                      
         A     R4,AESTTAB                                                       
         OC    0(L'ESTTAB,R4),0(R4)                                             
         JNZ   SETUSR04                                                         
                                                                                
         MVC   KEYSAVE,NBKEY                                                    
         LA    R2,NBKEY            READ POOL ESTIMATE HEADER                    
         USING ESTHDR,R2                                                        
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,NBACTCLI                                                 
         MVC   EKEYPRD,POL                                                      
         MVC   EKEYEST,NUKEST                                                   
         GOTOR READ,NBFILSPT                                                    
         JNE   SETUSR02                                                         
         GOTOR GETREC,AIO3                                                      
         L     R2,AIO3                                                          
         MVC   0(L'ESTTAB,R4),EUSRNMS                                           
         DROP  R2                                                               
                                                                                
SETUSR02 OC    0(L'ESTTAB,R4),SPACES                                            
         MVC   NBKEY,KEYSAVE                                                    
                                                                                
SETUSR04 MVC   NDUSRNMS,0(R4)      SET USER NAMES IN DEMO BLOCK                 
         J     EXIT                                                             
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
         LA    RE,LP_RMASK-LP_D(RE)                                             
         A     RE,ALP                                                           
         OC    0(1,RE),0(RF)                                                    
SETMSKX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET UNIVERSE CUT OFF DATE                                           *         
***********************************************************************         
                                                                                
GETUCOD  NTR1  BASE=*,LABEL=*                                                   
         CLI   REQUNIV,YESQ        REQUESTED UNIVERSES ONLY?                    
         JE    *+12                                                             
         CLI   REQUNIV,C'H'        REQUESTED HISP UNIVERSES ONLY?               
         JNE   GETUCODX                                                         
*                                                                               
         XC    UNIVCOD,UNIVCOD     UNIVERSE CUT OFF DATE                        
         XC    UNIVBLK,UNIVBLK     UNIVERSE BLOCK                               
*                                                                               
         XC    FULL,FULL                                                        
         GOTOR NBDATCON,DMCB,(2,STRDATE),(22,WORK)                              
         LA    RF,WORK+6           YEAR                                         
         LA    RE,4                L'YEAR                                       
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   RE,DUB                                                           
         AHI   RE,1                GET FOLLOWING YEAR IN DEMTABS                
         STCM  RE,3,FULL           FOR CURRENT YEAR'S CUTOFF                    
*                                                                               
         L     RF,RCOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,UNIVYRS   GET A(UNIVERSE YEARS)                        
         ICM   R6,15,0(R1)         A(TABLE) RETURNED IN P1                      
         JZ    GETUCODX                                                         
         L     R3,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
GUCOD10  CLI   0(R6),X'FF'                                                      
         BE    GETUCODX                                                         
         CLC   FULL(2),0(R6)       MATCH ON YEAR?                               
         JNE   GUCOD20                                                          
         GOTOR NBDATCON,DMCB,(4,2(R6)),(2,UNIVCOD)                              
         J     GETUCODX                                                         
*                                                                               
GUCOD20  AR    R6,R3                                                            
         B     GUCOD10                                                          
*                                                                               
GETUCODX J     EXIT                                                             
*                                                                               
***********************************************************************         
* VALIDATE A DEMO EXPRESSION                                          *         
***********************************************************************         
                                                                                
VALDEMN  NTR1  BASE=*,LABEL=*                                                   
         OC    DEMIND,DEMIND       ANY DEMOS PASSED?                            
         JZ    VALDEMNX                                                         
*                                                                               
         L     RF,AIO4                                                          
         USING P5XD,RF                                                          
         XC    0(P5XDLNQ,RF),0(RF)                                              
         MVC   P5XID,=C'P5X '                                                   
         LA    RE,COMLIC                                                        
         ST    RE,P5XLICNS          A(32 BYTE COMSCORE LICENSE)                 
         ST    RF,DMCB+16           EXTENDED PARAM5                             
         OI    DMCB+16,X'80'        SO DEMOVAL KNOWS EXTENDED BLOCK             
         DROP  RF                                                               
*                                                                               
         LA    RF,NDNTDMS                                                       
         CLI   0(RF),0             FIRST TIME?                                  
         JNE   VALDEMNX                                                         
         MVI   0(RF),X'FF'         DEFAULT TO NO COMSCORE                       
*                                                                               
         MVI   CDEMINDX,0          COMSCORE DEMO INDEX                          
*                                                                               
         ICM   RF,7,ADEM                                                        
         USING LW_D,RF                                                          
*                                                                               
         SR    R4,R4                                                            
         CLI   LW_TYPE,LW_TSINQ    SINGLE ENTRY?                                
         JNE   VDEMN00                                                          
         MVC   NDEMOS,=H'1'                                                     
         L     R4,=F'1'                                                         
         LA    R1,LW_DATA1         A(INPUT AREA)                                
         J     VDEMN02                                                          
*                                                                               
VDEMN00  ICM   R4,3,LW_NUMN        # OF DEMOS TO PROCESS                        
         MVC   NDEMOS,LW_NUMN                                                   
         LA    R1,LW_DATA2         A(INPUT AREA)                                
*                                                                               
VDEMN02  ST    R1,FULL                                                          
         LA    R3,NDDEMOS+3        A(OUTPUT AREA)                               
         DROP  RF                                                               
*                                                                               
VDEMN04  L     R1,FULL                                                          
         CLC   0(2,R1),=C'U1'      TEST FOR USER DEMOS                          
         JL    VDEMN06                                                          
         CLC   0(2,R1),=C'U4'                                                   
         JH    VDEMN06                                                          
         MVI   0(R3),0             BUILD USER DEMO ENTRY                        
         MVI   1(R3),USERDEMO                                                   
         MVC   2(1,R3),1(R1)                                                    
         NI    2(R3),X'0F'                                                      
         J     VDEMN14                                                          
                                                                                
VDEMN06  XC    FLDH,FLDH           BUILD FLD FOR DEMOVAL CALL                   
         LA    RF,FLD              A(OUTPUT)                                    
         LR    RE,R1               A(INPUT)                                     
         LA    R0,0                FIELD LENGTH                                 
*                                                                               
VDEMN08  CLI   0(RE),C' '                                                       
         JE    VDEMN10                                                          
         CLI   0(RE),0                                                          
         JE    VDEMN10                                                          
         MVC   0(1,RF),0(RE)                                                    
         AHI   R0,1                                                             
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         J     VDEMN08                                                          
*                                                                               
VDEMN10  STC   R0,FLDH+5                                                        
         AHI   R0,L'FLDH                                                        
         STC   R0,FLDH                                                          
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,RCOMFACS                                                
         MVC   DBFILE,DBFTPT                                                    
         MVI   DBSELMED,C'T'                                                    
         L     RF,RCOMFACS                                                      
         L     RF,CDEMOVAL-COMFACSD(RF)                                         
*                                                                               
* PARAM 6 PASSED FOR COMSCORE                                                   
*                                                                               
         GOTOR (RF),DMCB,(1,FLDH),(1,(R3)),(C'S',DBLOCK),0,,DUB                 
         CLI   4(R1),1             SHOULD BE ONE VALID DEMO                     
         JNE   VDEMN14                                                          
*                                                                               
         CLI   2(R3),0             COMSCORE DEMO?                               
         JNE   VDEMN14                                                          
         ZIC   RF,CDEMINDX                                                      
         AHI   RF,1                                                             
         STC   RF,CDEMINDX                                                      
         MVC   1(1,R3),CDEMINDX    SET CORRECT INDEX                            
*                                                                               
         LA    RF,NDNTDMS                                                       
VDEMN12  CLI   0(RF),X'FF'                                                      
         JE    *+8                                                              
         CLI   0(RF),0             FIND FIRST AVAILABLE SLOT                    
         JE    *+12                                                             
         AHI   RF,8                                                             
         J     VDEMN12                                                          
         MVC   0(8,RF),DUB         SAVE IT                                      
         MVI   8(RF),X'FF'                                                      
*                                                                               
VDEMN14  L     R1,FULL                                                          
         AHI   R1,11               NEXT INPUT DEMO                              
         ST    R1,FULL                                                          
         AHI   R3,3                NEXT DEMO SLOT IN NDDEMOS                    
         JCT   R4,VDEMN04                                                       
*                                                                               
VALDEMNX J     EXITY                                                            
***********************************************************************         
* GET NEXT RECORD                                                     *         
***********************************************************************         
                                                                                
NXTREC   NTR1  BASE=*,LABEL=*                                                   
         L     R0,0(R1)            R0=A(KEY TABLE)                              
         LM    R3,R6,4(R1)         R3=A(KEY SAVE AREA), R4=A(WORK SAVE)         
         LA    R3,0(R3)            R5=A(KEY FILTER ROUTINE)                     
*                                  R6=A(RECORD FILTER ROUTINE)                  
         MVC   FILETYPE,8(R1)      SAVE FILE TYPE                               
         L     R2,ALP                                                           
         USING LP_D,R2             R2=A(LP_D)                                   
         SR    RE,RE                                                            
         ICM   RE,1,4(R1)                                                       
         SLL   RE,2                                                             
         L     RE,LP_BLKS-L'LP_BLKS(RE)                                         
         ST    RE,LP_ADATA                                                      
         ST    RE,IOADDR                                                        
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME CALL                         
         BNE   NXTREC02                                                         
         MVI   LP_RMODE,LP_RNEXT   RESET FIRST TIME CALL                        
         CLI   0(R1),YESQ          TEST RECORD SET IS WANTED                    
         BNE   NXTREC06                                                         
         LTR   R3,R3               TEST RECORD KEY SAVE AREA PROVIDED           
         BZ    *+10                                                             
         MVC   0(L'NBKEY,R3),NBKEY YES - SAVE CURRENT RECORD KEY                
         XC    NBKEY,NBKEY                                                      
NXTREC02 GOTOR LP_ASETK,DMCB,(0,(R0)),NBKEY,(R4),('FF',ALP)                     
         BH    NXTREC04                                                         
         SR    R1,R1                                                            
         IC    R1,FILETYPE                                                      
         GOTOR HIGH                                                             
         GOTOR LP_ASETK,DMCB,(1,(R0)),NBKEY,(R4),('FF',ALP)                     
         BNE   NXTREC02                                                         
         LTR   RF,R5               TEST/SET DIRECTORY FILTER ROUTINE            
         BZ    *+12                                                             
         GOTOR GOFILT                                                           
         BNE   NXTREC02                                                         
         OC    DMFIL,DMFIL         TEST D/A FILE FOR THIS DIRECTORY             
         JZ    EXITY                                                            
         GOTOR GETREC,IOADDR                                                    
         LTR   RF,R6               SET/TEST FILE FILTER ROUTINE                 
         JZ    EXITY                                                            
         L     R1,IOADDR           PASS A(RECORD) IN R1                         
         GOTOR GOFILT                                                           
         BNE   NXTREC02            DIDN'T PASS FILTERS - GET NEXT               
         J     EXITY                                                            
                                                                                
NXTREC04 LTR   R3,R3               TEST ANY KEY SAVED                           
         BZ    NXTREC06                                                         
         MVC   NBKEY,0(R3)         YES - RESTORE IT                             
NXTREC06 MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS TO COME                  
         J     EXITN               EXIT WITH CC=NOT EQUAL TO CALLER             
                                                                                
GOFILT   NTR1  LABEL=*             CALL RECORD FILTER ROUTINE                   
         L     RE,ALP                                                           
         LM    R2,RB,LP_R2RB-LP_D(RE)                                           
         BASR  RE,RF                                                            
         J     EXIT                                                             
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD CLIENT TABLE FOR MFM PRODUCT DOWNLOAD                         *         
***********************************************************************         
                                                                                
BLDCLT   NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         ICM   R2,7,ASMC                                                        
         JZ    EXITN                                                            
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R2)                                            
         LA    R2,LW_DATA2-LW_D(R2)                                             
         USING SMCTABD,R2          R2=A(SYSTEM/MEDIA/CLIENT LIST)               
         L     R7,ALP                                                           
         L     R7,LP_AWMP-LP_D(R7)                                              
         USING LW_D,R7                                                          
         LA    R3,LW_DATA2                                                      
         USING CLTTABD,R3          R3=A(AGENCY/MEDIA/CLIENT LIST)               
         L     R4,AIO4             R4=A(CLIENT CONVERSION TABLE)                
         SR    R5,R5               R5=N'ENTRIES IN OUTPUT TABLE                 
                                                                                
BLDCLT02 CLI   SMCTSYS,NETMEDQ     TEST ENTRY FOR ME                            
         BNE   BLDCLT04                                                         
         GOTOR VCLPACK,DMCB,SMCTCLT,CLTTCLT                                     
         MVC   0(L'CLTTCLT,R4),CLTTCLT                                          
         MVC   L'CLTTCLT(L'SMCTCLT,R4),SMCTCLT                                  
         AHI   R4,5                BUMP CLIENT TABLE POINTER                    
         XC    0(5,R4),0(R4)                                                    
         AHI   R5,1                BUMP N'OUTPUT ENTRIES                        
         AHI   R3,CLTTABL          BUMP OUTPUT TABLE POINTER                    
                                                                                
BLDCLT04 AHI   R2,SMCTABL          BUMP INPUT TABLE POINTER                     
         BCT   R0,BLDCLT02         DO FOR NUMBER OF ENTRIES                     
         LTR   R5,R5               TEST ANY OUTPUT ENTRIES CREATED              
         BZ    BLDCLT06                                                         
         GOTOR VXSORT,DMCB,LW_DATA2,(R5),CLTTABL,CLTTABL,0                      
                                                                                
BLDCLT06 STCM  R7,7,ACLT           SET A(AGENCY/MEDIA/CLIENT TABLE)             
         STCM  R5,3,LW_NUMN                                                     
         MVI   LW_TYPE,LW_TLSTQ                                                 
         SR    R3,R7                                                            
         STCM  R3,3,LW_LN                                                       
         AR    R3,R7                                                            
         L     R7,ALP                                                           
         STCM  R3,15,LP_AWMP-LP_D(R7)                                           
         J     EXITY                                                            
         DROP  R2,R3,R7,RB                                                      
                                                                                
SMCTABD  DSECT                     ** SYSTEM/MEDIA/CLIENT TABLE **              
SMCTSYS  DS    C                   SYSTEM CODE                                  
SMCTMED  DS    C                   MEDIA CODE                                   
SMCTCLT  DS    CL3                 CLIENT CODE                                  
SMCTABL  EQU   *-SMCTABD                                                        
                                                                                
CLTTABD  DSECT                     ** CLIENT TABLE **                           
CLTTCLT  DS    XL(L'CKEYCLT)       CLIENT CODE                                  
CLTTABL  EQU   *-CLTTABD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* CONVERT PRODUC IN PRDLIST TABLE TO ALPHA (IF NEEDED)                          
***********************************************************************         
                                                                                
CONVPRD  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,PRDLIST                                                       
         CLI   1(R4),0            IS TABLE ALREADY IN ALPHA FORMAT              
         BNE   CNVPRD60                                                         
                                                                                
         LHI   R2,MAXUPRD                                                       
                                                                                
*                                                                               
CNVPRD20 CLI   0(R4),0             END OF TABLE                                 
         BE    CNVPRD60                                                         
                                                                                
         L     R3,NBACLI                                                        
         AHI   R3,CLIST-CLTHDR     R3=A(PRODUCT LIST)                           
         LHI   R0,CPLDMAXN                                                      
         CLC   L'PKEYPRD(L'NUPRDPR,R3),0(R4)                                    
         JE    CNVPRD40                                                         
         AHI   R3,CPLDATAL                                                      
         JCT   R0,*-14                                                          
                                                                                
         L     R3,NBACLI                                                        
         AHI   R3,CLIST2-CLTHDR    R3=A(PRODUCT LIST EXTENSION)                 
         LHI   R0,255-CPLDMAXN                                                  
         CLC   L'PKEYPRD(L'NUPRDPR,R3),0(R4)                                    
         JE    CNVPRD40                                                         
         AHI   R3,CPLDATAL                                                      
         JCT   R0,*-14                                                          
                                                                                
         MVC   0(L'PKEYPRD,R4),UNKNOWN                                          
         J     EXITY                                                            
                                                                                
CNVPRD40 MVC   0(L'PKEYPRD,R4),0(R3)                                            
         LA    R4,PRDLLEN(R4)                                                   
         BCT   R2,CNVPRD20                                                      
                                                                                
*  SET BLANKS TO SPACES FOR FEED                                                
CNVPRD60 LHI   R2,MAXUPRD                                                       
         LA    R4,PRDFEED                                                       
                                                                                
CNVPRD80 OC    PRDFFEED(4,R4),SPACES                                            
         LA    R4,PRDFLEN(R4)                                                   
         BCT   R2,CNVPRD80                                                      
                                                                                
CNVPRDEX J     EXITY                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* SET NATIONAL FEED IN PRODUCT TABLE                                            
* R4 = ADDRESS OFF X'21' ELEMENT FROM UNIT                                      
***********************************************************************         
                                                                                
SETNATNL NTR1  BASE=*,LABEL=*                                                   
         USING NUCMLEL,R4                                                       
*                                                                               
         TM    NUCMLFL2,X'01'      CHECK FOR NO NATIONAL                        
         JO    SETNATEX                                                         
*                                                                               
         CLI   PRDLIST,0           CHECK IF UNIT IS ALLOCATED                   
         JE    SETNATEX                                                         
                                                                                
         LA    RE,PRDFEED                                                       
         LHI   RF,MAXUPRD                                                       
*                                                                               
                                                                                
SETNAT20 CLI   0(RE),0             END OF TABLE                                 
         JE    SETNAT30                                                         
         LA    RE,PRDFLEN(RE)                                                   
         JCT   RF,SETNAT20                                                      
         J     SETNATEX                                                         
                                                                                
SETNAT30 OC    NUCMPPOS,NUCMPPOS   IS PROD SET IN UNIT                          
         JNZ   SETNAT40            NO DEFAULT FIRST PROD                        
*                                                                               
         MVI   PRDFPRD(RE),X'01'   DEFAULT TO FIRST PRODUCT                     
         J     *+10                                                             
SETNAT40 MVC   PRDFPRD(L'NUCMPPOS,RE),NUCMPPOS                                  
         MVC   PRDFFEED(4,RE),=CL4'*N  '                                        
SETNATEX J     EXITY                                                            
         DROP  R4                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
* SET FEEDS IN PRODUCT TABLE                                                    
* R4 = ADDRESS OFF X'23' ELEMENT FROM UNIT                                      
***********************************************************************         
                                                                                
SETFEED  NTR1  BASE=*,LABEL=*                                                   
         USING NUFDCEL,R4                                                       
                                                                                
         LA    RE,PRDFEED                                                       
         LHI   RF,MAXUPRD                                                       
                                                                                
SETFD20  CLI   0(RE),0              END OF TABLE                                
         JE    SETFD30                                                          
         LA    RE,PRDFLEN(RE)                                                   
         JCT   RF,SETFD20                                                       
         J     SETFDEX                                                          
                                                                                
SETFD30  LA    R1,1                                                             
         LA    R5,PRDLIST                                                       
         LHI   RF,MAXUPRD                                                       
SETFD40  CLC   NUFDPROD,0(R5)      IS PRODUCT IN PRDLIST                        
         BE    SETFD60                                                          
         LA    R5,PRDLLEN(R5)                                                   
         LA    R1,1(R1)                                                         
         JCT   RF,SETFD40                                                       
*                                                                               
         MVI   PRDFPRD(RE),X'01'   DEFAULT TO FIRST PRODUCT                     
         J     *+8                                                              
SETFD60  STCM  R1,1,0(RE)          MOVE PRDUCT NUMBER TO PRDFEED                
         MVC   PRDFFEED(L'NUFDCFED,RE),NUFDCFED                                 
         TM    NUFDCFL2,X'40'                                                   
         BZ    *+8                                                              
         OI    PRDFSTAT(RE),X'80'   INSTRUCTION SET CANT DELETE FEED            
                                                                                
SETFDEX  J     EXITY                                                            
         DROP  R4                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
* BUILD MEDIA TABLE FOR MFM VENDOR DOWNLOAD                           *         
***********************************************************************         
                                                                                
BLDMED   NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         ICM   R2,7,ASMC                                                        
         JZ    EXITN                                                            
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R2)                                            
         LA    R2,LW_DATA2-LW_D(R2)                                             
         USING SMDTABD,R2          R2=A(SYSTEM/MEDIA LIST)                      
         LA    R4,MEDFILTS         R4=A(MEDIA TABLE)                            
BLDMED02 CLI   SMDTSYS,NETMEDQ     TEST ENTRY FOR ME                            
         BNE   *+14                                                             
         MVC   0(L'SMDTMED,R4),SMDTMED                                          
         AHI   R4,L'SMDTMED                                                     
         AHI   R2,SMDTABL                                                       
         BCT   R0,BLDMED02                                                      
         OC    MEDFILTS,MEDFILTS   TEST ANY MEDIA FILTERS FOUND                 
         JZ    EXITN                                                            
         J     EXITY                                                            
         DROP  R2,RB                                                            
                                                                                
SMDTABD  DSECT                     ** SYSTEM/MEDIA TABLE **                     
SMDTSYS  DS    C                   SYSTEM CODE                                  
SMDTMED  DS    C                   MEDIA CODE                                   
SMDTABL  EQU   *-SMDTABD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT 2 FILETYPE MILITARY TIME (IN R1) TO QUARTER HOUR            *         
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
                                                                                
HIGH     MVC   DMCMND,DMRDHI                                                    
         STC   R1,NBFILE                                                        
         J     DIRALL                                                           
                                                                                
READ     MVC   DMCMND,DMREAD                                                    
         STC   R1,NBFILE                                                        
         J     DIRALL                                                           
                                                                                
SEQ      MVC   DMCMND,DMRSEQ                                                    
                                                                                
DIRALL   NTR1  LABEL=*                                                          
         MVC   NBKEYLST,NBKEY                                                   
         GOTOR SETFIL                                                           
         MVC   DMFILE,DMDIR                                                     
         GOTOR DMTRACE,0                                                        
         LA    R0,IOAREA                                                        
         OC    DMFIL,DMFIL         TEST DIRECTORY ONLY (STAFIL)                 
         JNZ   *+8                                                              
         L     R0,IOADDR                                                        
         GOTOR NBDM,DMCB,(0,DMCMND),DMFILE,NBKEY,(R0),0                         
         LR    RF,R0                                                            
         MVC   NBKEY(40),0(RF)                                                  
         GOTOR DMTRACE,1                                                        
         GOTOR DMCHECK                                                          
DIRALLX  J     EXITY                                                            
                                                                                
***********************************************************************         
* DATAMGR INTERFACE - FILES                                           *         
***********************************************************************         
                                                                                
GETREC   NTR1  LABEL=*                                                          
         L     R0,0(R1)            R0=A(I/O AREA)                               
         GOTOR SETFIL                                                           
         MVC   DMFILE,DMFIL                                                     
         MVC   DMCMND,DMGETR                                                    
         GOTOR NBDM,DMCB,(X'22',DMCMND),DMFILE,DMDA,(R0),DMWK                   
         GOTOR DMTRACE,1                                                        
         GOTOR DMCHECK                                                          
         J     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO SET VALUES FOR I/O CALL                                  *         
***********************************************************************         
                                                                                
SETFIL   MVC   DMDIR,UNTDIR                                                     
         MVC   DMFIL,UNTFIL                                                     
         MVI   DMKEYLEN,L'NUKEY                                                 
         LHI   RF,NUDATA-NURECD                                                 
         STCM  RF,3,NBDTADSP                                                    
         MVC   DMDA,NBKEY+(NUDA-NURECD)                                         
         CLI   NBFILE,NBFILUNT                                                  
         BER   RE                                                               
*                                                                               
         MVC   DMDIR,CTFILE                                                     
         MVC   DMFIL,CTFILE                                                     
         MVI   DMKEYLEN,L'CT5KEY                                                
         LHI   RF,CT5DATA-CT5KEY                                                
         STCM  RF,3,NBDTADSP                                                    
         CLI   NBFILE,NBFILCTL                                                  
         BER   RE                                                               
*                                                                               
         MVC   DMDIR,GENDIR                                                     
         MVC   DMFIL,GENFIL                                                     
         MVI   DMKEYLEN,L'TOKKEY                                                
         LHI   RF,TOKFIRST                                                      
         STCM  RF,3,NBDTADSP                                                    
         MVC   DMDA,NBKEY+(TOKKDA-TOKRECD)                                      
         CLI   NBFILE,NBFILGEN                                                  
         BER   RE                                                               
*                                                                               
         MVC   DMDIR,XSPDIR                                                     
         MVC   DMFIL,XSPFIL                                                     
         MVI   DMKEYLEN,L'GXKEY                                                 
         LHI   RF,GXDATA-GXKEY                                                  
         STCM  RF,3,NBDTADSP                                                    
         MVC   DMDA,NBKEY+(GXKDA-GXKEY)                                         
         CLI   NBFILE,NBFILXSP                                                  
         BER   RE                                                               
*                                                                               
         MVC   DMDIR,SPTDIR                                                     
         MVC   DMFIL,SPTFIL                                                     
         MVI   DMKEYLEN,L'AGYKEY                                                
         LHI   RF,AGYEL-AGYHDR                                                  
         STCM  RF,3,NBDTADSP                                                    
         MVC   DMDA,NBKEY+(AGYKDA-AGYHDR)                                       
         CLI   NBFILE,NBFILSPT                                                  
         BER   RE                                                               
*                                                                               
         MVC   DMDIR,STAFIL                                                     
         XC    DMFIL,DMFIL                                                      
         MVI   DMKEYLEN,STAKLEN-STAREC                                          
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* DATAMGR CHECKING AND TRACING ROUTINES                               *         
***********************************************************************         
                                                                                
DMCHECK  CLI   DMCB+8,0            OK                                           
         BER   RE                                                               
         CLI   DMCB+8,IOEDEL       RECORD IS DELETED OK TOO                     
         BER   RE                                                               
         TM    DMCB+8,IOERNF+IOEEOF                                             
         JNZ   EXITN                                                            
         DC    H'0'                ANYTHING ELSE IS DEAD BAD                    
                                                                                
DMTRACE  CLI   NBTRCOPT,NOQ        TEST TRACE OPTION ACTIVE                     
         BER   RE                                                               
DMTRACEN NTR1  LABEL=*                                                          
         MVI   P,SPACEQ                                                         
         MVC   P+1(L'P-1),P                                                     
         MVC   P(L'DMCMND),DMCMND                                               
         MVC   P+8(L'DMFILE),DMFILE                                             
         LTR   R1,R1               TEST 'BEFORE I/O' CALL                       
         JNZ   DMTRACE2                                                         
         MVC   P+18(6),BEFLIT                                                   
         GOTOR TRCDIR                                                           
         J     DMTRACEX                                                         
                                                                                
DMTRACE2 MVC   P+18(5),AFTLIT                                                   
         CLC   DMFILE,UNTFIL                                                    
         JE    DMTRACE4                                                         
         CLC   DMFILE,SPTFIL                                                    
         JE    DMTRACE4                                                         
         GOTOR TRCDIR                                                           
         J     DMTRACEX                                                         
                                                                                
DMTRACE4 MVI   DMKEYLEN,L'NUKEY                                                 
         CLC   DMFILE,UNTFIL                                                    
         JE    *+8                                                              
         MVI   DMKEYLEN,L'AGYKEY                                                
         GOTOR TRCFIL                                                           
                                                                                
DMTRACEX J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TRACE DIRECTORY CALLS                                               *         
***********************************************************************         
                                                                                
TRCDIR   LR    R0,RE                                                            
         MVC   P+45(4),KEYLIT                                                   
         SR    R2,R2                                                            
         IC    R2,DMKEYLEN                                                      
         BCTR  R2,0                                                             
         BASR  RE,0                                                             
         EX    R2,4(RE)                                                         
         MVC   P+49(0),NBKEY                                                    
         BASR  RE,0                                                             
         EX    R2,4(RE)                                                         
         TR    P+49(0),TRTTAB                                                   
         GOTOR NBPRINT,PARA,P-1,PRTBL01                                         
         MVI   P,SPACEQ                                                         
         MVC   P+1(L'P-1),P                                                     
         GOTOR NBHEXOUT,PARA,NBKEY,HEXWORK,1(R2),HEXSEP                         
         BASR  RE,0                                                             
         EX    R2,4(RE)                                                         
         MVC   P+49(0),HEXWORK                                                  
         GOTOR NBPRINT,PARA,P-1,PRTBL01                                         
         MVI   P,SPACEQ                                                         
         MVC   P+1(L'P-1),P                                                     
         LA    R1,HEXWORK+1(R2)                                                 
         BASR  RE,0                                                             
         EX    R2,4(RE)                                                         
         MVC   P+49(0),0(R1)                                                    
         GOTOR NBPRINT,PARA,P-1,PRTBL01                                         
         MVI   P,SPACEQ                                                         
         MVC   P+1(L'P-1),P                                                     
TRCDIRX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* TRACE FILE CALLS                                                    *         
***********************************************************************         
                                                                                
TRCFIL   LR    R0,RE                                                            
         MVC   P+25(4),DALIT                                                    
         L     RF,DMCB+8                                                        
         GOTOR NBHEXOUT,PARA,(RF),P+29,L'DMDA,HEXTOG                            
         MVC   P+38(4),ERRLIT                                                   
         GOTOR (RF),(R1),DMCB+8,P+42,1,HEXTOG                                   
         MVC   P+45(4),KEYLIT                                                   
         L     RF,DMCB+12                                                       
         SR    R2,R2                                                            
         IC    R2,DMKEYLEN                                                      
         BCTR  R2,0                                                             
         BASR  RE,0                                                             
         EX    R2,4(RE)                                                         
         MVC   P+49(0),0(RF)                                                    
         BASR  RE,0                                                             
         EX    R2,4(RE)                                                         
         TR    P+49(0),TRTTAB                                                   
         GOTOR NBPRINT,PARA,P-1,PRTBL01                                         
         MVI   P,SPACEQ                                                         
         MVC   P+1(L'P-1),P                                                     
         L     RF,DMCB+12                                                       
         GOTOR NBHEXOUT,PARA,(RF),HEXWORK,1(R2),HEXSEP                          
         BASR  RE,0                                                             
         EX    R2,4(RE)                                                         
         MVC   P+49(0),HEXWORK                                                  
         GOTOR NBPRINT,PARA,P-1,PRTBL01                                         
         MVI   P,SPACEQ                                                         
         MVC   P+1(L'P-1),P                                                     
         LA    R1,HEXWORK+1(R2)                                                 
         BASR  RE,0                                                             
         EX    R2,4(RE)                                                         
         MVC   P+49(0),0(R1)                                                    
         GOTOR NBPRINT,PARA,P-1,PRTBL01                                         
         MVI   P,SPACEQ                                                         
         MVC   P+1(L'P-1),P                                                     
TRCFILX  LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
EXITY    LHI   RE,1                CC=EQUAL FOR YES                             
         J     EXITCC                                                           
                                                                                
EXITN    LHI   RE,0                CC=NOT EQUAL FOR NO                          
                                                                                
EXITCC   CHI   RE,1                SET CONDITION CODE                           
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
GLOBALS  DS    0D                                                               
         LTORG                                                                  
                                                                                
BZEROES  DC    AL4(0)                                                           
KEYLIT   DC    C'Key='                                                          
DALIT    DC    C'D/A='                                                          
ERRLIT   DC    C'Err='                                                          
BEFLIT   DC    C'Before'                                                        
AFTLIT   DC    C'After'                                                         
PRTBL01  DC    C'BL01'                                                          
PRTBL02  DC    C'BL02'                                                          
HEXTOG   DC    C'TOG'                                                           
HEXSEP   DC    C'SEP'                                                           
EFFS     DC    32AL1(FF)                                                        
POL      DC    C'POL'                                                           
DMRDHI   DC    C'DMRDHI  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMKEY    DC    C'DMKEY   '                                                      
DMGETR   DC    C'GETREC  '                                                      
DMPUTR   DC    C'PUTREC  '                                                      
DBFTPT   DC    C'TPT'                                                           
UNKNOWN  DC    C'???'                                                           
DEMHOMES DC    X'00',C'R',AL1(1)                                                
DEMHOMEL EQU   *-DEMHOMES                                                       
SPACES   DC    CL80' '                                                          
PZERO    DC    P'0'                                                             
BITLIST  DC    X'8040201008040201'                                              
ICMLIST  DC    AL1(1,3,7,15)                                                    
TRTTAB   DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
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
V2101    DC    AL1(2,1,0,1)                                                     
V41028   DC    AL1(4,1,0,28)                                                    
NEWBOOK  DC    AL1(89,01)                                                       
MAPUNIT  DC    AL2(32)                                                          
MAPMFMM  DC    X'FFFD'                                                          
MAPMFMC  DC    X'FFFC'                                                          
MAPMFMP  DC    X'FFFB'                                                          
MAPMFMV  DC    X'FFFA'                                                          
MAPSTAT  DC    X'FFF8'                                                          
                                                                                
STAFIL   DC    C'STATION'                                                       
                                                                                
FILES    DS    0X                  ** FILE INFO **                              
         DC    C'SPOT   '          SYSTEM NAME FOR OPEN                         
                                                                                
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
                                                                                
PKNFLT   DS    0X                  ** PACKAGE NUMBER FILTER **                  
         DC    AL2(L'NUPACK)                                                    
         DC    AL1(0,L'NUPACK-1)                                                
         DC    AL2(APKN-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
         DC    AL1(LK_EOTQ)                                                     
                                                                                
TIMFLT   DS    0X                  ** TIME FILTER **                            
         DC    AL2(L'NUKTIME)                                                   
         DC    AL1(0,L'NUKTIME-1)                                               
         DC    AL2(ATIM-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
         DC    AL1(LK_EOTQ)                                                     
                                                                                
DPTFLT   DS    0X                  ** DAYPART FILTER **                         
         DC    AL2(L'NUKDP)                                                     
         DC    AL1(0,L'NUKDP-1)                                                 
         DC    AL2(ADPT-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
         DC    AL1(LK_EOTQ)                                                     
                                                                                
LENFLT   DS    0X                  ** SPOT LENGTH FILTER **                     
         DC    AL2(L'NULEN)                                                     
         DC    AL1(0,L'NULEN-1)                                                 
         DC    AL2(ALEN-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
         DC    AL1(LK_EOTQ)                                                     
                                                                                
PRDFLT   DS    0X                  ** PRODUCT FILTER **                         
         DC    AL2(L'NUPRD)                                                     
         DC    AL1(0,L'PRDALPH-1)                                               
         DC    AL2(APRD-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
         DC    AL1(LK_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* KEY DRIVER TABLES                                                   *         
***********************************************************************         
                                                                                
NUKEYT   LKKEY H,NUKEY,SAVED                                                    
         LKKEY LIT,NUKTYPE,NUKTYPEQ                                             
         LKKEY SIN,NUKAM,NBACTAM                                                
         LKKEY WMP,NUKCLT,ACLT                                                  
         LKKEY RNG,NUKDATE,STRDATE                                              
         LKKEY WMP,NUKTIME,ATIM                                                 
         LKKEY WMP,NUKNET,ANWK                                                  
         LKKEY WMP,NUKPROG,APRG                                                 
         LKKEY WMP,NUKEST,AEST                                                  
         LKKEY WMP,NUKSUB,ASUB                                                  
         LKKEY WMP,NUKDP,ADPT                                                   
         LKKEY E                                                                
                                                                                
NPKEYT   LKKEY H,NUKPKEY                                                        
         LKKEY LIT,NUKPTYPE,NUKPTYPQ                                            
         LKKEY SIN,NUKPAM,NBACTAM                                               
         LKKEY WMP,NUKPCLT,ACLT                                                 
         LKKEY WMP,NUKPNET,ANWK                                                 
         LKKEY WMP,NUKPPROG,APRG                                                
         LKKEY RNG,NUKPDATE,STRDATE                                             
         LKKEY WMP,NUKPEST,AEST                                                 
         LKKEY WMP,NUKPSUB,ASUB                                                 
         LKKEY WMP,NUKPDP,ADPT                                                  
         LKKEY E                                                                
                                                                                
NDKEYT   LKKEY H,NUKDKEY                                                        
         LKKEY LIT,NUKDTYPE,NUKDTYPQ                                            
         LKKEY SIN,NUKDAM,NBACTAM                                               
         LKKEY WMP,NUKDCLT,ACLT                                                 
         LKKEY WMP,NUKDEST,AEST                                                 
         LKKEY WMP,NUKDNET,ANWK                                                 
         LKKEY WMP,NUKDDAY,ADAY                                                 
         LKKEY WMP,NUKDTIME,ATIM                                                
         LKKEY WMP,NUKDPROG,APRG                                                
         LKKEY RNG,NUKDDATE,STRDATE                                             
         LKKEY WMP,NUKDSUB,ASUB                                                 
         LKKEY E                                                                
                                                                                
GOLKEYT  LKKEY H,GKEY              ** GOAL KEY DRIVER TABLE **                  
         LKKEY LIT,GKEYTYPE,GKEYTYPQ                                            
         LKKEY SIN,GKEYAM,NBACTAM                                               
         LKKEY WMP,GKEYCLT,ACLT                                                 
         LKKEY WMP,GKEYPRD,APRD                                                 
         LKKEY LST,GKEYMKT,MKTNUM                                               
         LKKEY WMP,GKEYEST,AEST                                                 
         LKKEY WMP,GKEYDPT,ADPT                                                 
         LKKEY WMP,GKEYSLN,ALEN                                                 
         LKKEY RNG,GKEYSEC,SLNRNG                                               
         LKKEY LIT,GKEYAGY,0                                                    
         LKKEY LIT,GKEYPKGE,0                                                   
         LKKEY E                                                                
                                                                                
GOLXKEYT LKKEY H,GXKEY             ** GOAL KEY DRIVER TABLE **                  
         LKKEY LIT,GXKEYTYP,GKEYTYPQ                                            
         LKKEY SIN,GXKEYAM,NBACTAM                                              
         LKKEY WMP,GXKEYCLT,ACLT                                                
         LKKEY WMP,GXKEYPRD,APRD                                                
         LKKEY LST,GXKEYMKT,MKTNUM                                              
         LKKEY WMP,GXKEYEST,AEST                                                
         LKKEY WMP,GXKEYDPT,ADPT                                                
         LKKEY WMP,GXKEYSLN,ALEN                                                
         LKKEY RNG,GXKEYSEC,SLNRNG                                              
         LKKEY LIT,GXKEYAGY,0                                                   
         LKKEY LIT,GXKPKGE,0                                                    
         LKKEY WMP,GXKPRDA,APRDA                                                
         LKKEY E                                                                
                                                                                
CLTKEYT  LKKEY H,CKEY              ** CLIENT KEY DRIVER TABLE **                
         LKKEY LIT,CKEYTYPE,0                                                   
         LKKEY SIN,CKEYAM,AGYMED                                                
         LKKEY RNG,CKEYCLT,CLTRNG                                               
         LKKEY LIT,CKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
PRDKEYT  LKKEY H,PKEY              ** PRODUCT KEY DRIVER TABLE **               
         LKKEY LIT,PKEYTYPE,0                                                   
         LKKEY SIN,PKEYAM,AGYMED                                                
         LKKEY WMP,PKEYCLT,ACLT                                                 
         LKKEY RNG,PKEYPRD,PRDRNG                                               
         LKKEY LIT,PKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
STAKEYT  LKKEY H,STAKEY            ** STATION KEY DRIVER TABLE **               
         LKKEY LIT,STAKTYPE,STAKTYPQ                                            
         LKKEY LIT,STAKMED,NETMEDQ                                              
         LKKEY RNG,STAKCALL,STARNG                                              
         LKKEY SIN,STAKAGY,AGENCY                                               
         LKKEY LIT,STAKCLT,C'0',L'STAKCLT+L'STAKFILL                            
         LKKEY E                                                                
         EJECT                                                                  
OPTTAB   DS    0XL(OPTTABL)        ** OPTIMIZATION TABLE **                     
         DC    AL2(DCLT-CODE,LCLT-SAVED),AL1(L'NUKCLT,0)                        
         DC    AL2(DEST-CODE,LEST-SAVED),AL1(0,0)                               
         DC    AL2(DPROG-CODE,LPROG-SAVED),AL1(0,0)                             
         DC    AL2(DPRGNAM-CODE,LPRGNAM-SAVED),AL1(0,0)                         
         DC    AL2(DDATE-CODE,LDATE-SAVED),AL1(0,0)                             
         DC    AL2(DTIME-CODE,LTIME-SAVED),AL1(L'NBTIME,0)                      
         DC    AL2(DSECONDS-CODE,LSECONDS-SAVED),AL1(0,0)                       
         DC    AL2(DDAYPART-CODE,LDAYPART-SAVED),AL1(L'NUKDP,0)                 
         DC    AL2(DPACKAGE-CODE,LPACKAGE-SAVED),AL1(0,0)                       
         DC    AL2(DNETWORK-CODE,LNETWORK-SAVED),AL1(0,0)                       
         DC    AL2(DROT-CODE,LROT-SAVED),AL1(0,0)                               
         DC    AL2(DNTICODE-CODE,LNTICODE-SAVED),AL1(0,MO#NTICD)                
         DC    AL2(DBUYTYPE-CODE,LBUYTYPE-SAVED),AL1(0,MO#BTYPE)                
         DC    AL2(DSTATYPE-CODE,LSTATYPE-SAVED),AL1(0,MO#STYPE)                
         DC    AL2(DPSTTYPE-CODE,LPSTTYPE-SAVED),AL1(0,MO#PTYPE)                
         DC    AL2(DACTCOST-CODE,LACTCOST-SAVED),AL1(0,MO#ACCST)                
         DC    AL2(DUNVCODE-CODE,LUNVCODE-SAVED),AL1(0,MO#UCODE)                
         DC    AL2(DBOOKBAS-CODE,LBOOKBAS-SAVED),AL1(0,MO#BKBAS)                
*        DC    AL2(DHIMP-CODE,LHIMP-SAVED),AL1(0,MO#HIMP)                       
*        DC    AL2(DHGRP-CODE,LHGRP-SAVED),AL1(0,MO#HGRP)                       
*        DC    AL2(DHSHR-CODE,LHSHR-SAVED),AL1(0,MO#HSHR)                       
*        DC    AL2(DHHUT-CODE,LHHUT-SAVED),AL1(0,MO#HHUT)                       
         DC    AL2(DPROGTYP-CODE,LPROGTYP-SAVED),AL1(0,MO#PSPTY)                
         DC    AL2(DNEWORET-CODE,LNEWORET-SAVED),AL1(0,MO#NORET)                
         DC    AL2(DCONTENT-CODE,LCONTENT-SAVED),AL1(0,MO#CNTNT)                
         DC    AL2(DINTCOST-CODE,LINTCOST-SAVED),AL1(0,MO#INCST)                
         DC    AL2(DTIERNUM-CODE,LTIERNUM-SAVED),AL1(0,MO#TIERN)                
         DC    AL2(DASSIGN-CODE,LASSIGN-SAVED),AL1(0,MO#ASCST)                  
         DC    AL2(DPRDLIST-CODE,LPRDLIST-SAVED),AL1(0,MO#ALLOC)                
         DC    AL2(DSUBMEDT-CODE,LSUBMEDT-SAVED),AL1(0,MO#SMTYP)                
         DC    AL2(DSPRCODE-CODE,LSPRCODE-SAVED),AL1(0,MO#SRCOD)                
         DC    AL2(DMIRRORC-CODE,LMIRRORC-SAVED),AL1(0,MO#MCODE)                
         DC    AL2(DREASONC-CODE,LREASONC-SAVED),AL1(0,MO#RCODE)                
         DC    AL2(DAFIDATE-CODE,LAFIDATE-SAVED),AL1(0,MO#AFDAT)                
         DC    AL2(DAFITIME-CODE,LAFITIME-SAVED),AL1(0,MO#AFTIM)                
         DC    AL2(DSUBDPRT-CODE,LSUBDPRT-SAVED),AL1(0,MO#SUBDP)                
         DC    AL2(DTCARLEV-CODE,LTCARLEV-SAVED),AL1(0,MO#TCARL)                
         DC    AL2(DCLICOM1-CODE,LCLICOM1-SAVED),AL1(0,MO#CCOM1)                
         DC    AL2(DCLICOM2-CODE,LCLICOM2-SAVED),AL1(0,MO#CCOM2)                
         DC    AL2(DMLTIRUN-CODE,LMLTIRUN-SAVED),AL1(0,MO#MULTI)                
         DC    AL2(DCOMBOCD-CODE,LCOMBOCD-SAVED),AL1(0,MO#COMBO)                
         DC    AL2(DCOMCOD1-CODE,LCOMCOD1-SAVED),AL1(0,MO#COMM1)                
         DC    AL2(DCOMCOD2-CODE,LCOMCOD2-SAVED),AL1(0,MO#COMM2)                
         DC    AL2(DPOD-CODE,LPOD-SAVED),AL1(0,MO#POD)                          
         DC    AL2(DRESULT-CODE,LRESULT-SAVED),AL1(0,MO#PRSLT)                  
         DC    AL2(DSERIAL#-CODE,LSERIAL#-SAVED),AL1(0,MO#SER)                  
         DC    AL2(DDEAL#-CODE,LDEAL#-SAVED),AL1(0,MO#DEAL)                     
         DC    AL2(DCON#-CODE,LCON#-SAVED),AL1(0,MO#CON)                        
OPTTABX  DC    AL1(OPTTEOTQ)                                                    
                                                                                
OPTTABD  DSECT                     ** OPTIMIZATION TABLE **                     
OPTTEOTQ EQU   FF                  END OF TABLE INDICATOR                       
OPTTDDSP DS    AL2                 DISPLACEMENT OF OUTPUT DEFINITION            
OPTTLDSP DS    AL2                 DISPLACEMENT TO LAST SENT VALUE              
OPTTDLEN DS    AL1                 OVERRIDE COMPARE LENGTH                      
OPTTMBIT DS    X                   MASK BIT OR ZERO                             
OPTTABL  EQU   *-OPTTABD                                                        
SVRDEF   CSECT                                                                  
                                                                                
MEDTAB   DS    0XL(MEDTABL)        ** MEDIA TABLE **                            
         DC    AL1(MEDTMRAD),AL2(0000),CL18'Radio'                              
         DC    AL1(MEDTMHIS),AL2(0000),CL18'Hispanic'                           
         DC    AL1(MEDTMOTH),AL2(0000),CL18'Other'                              
         DC    AL1(MEDTMSYN),AL2(0774),CL18'Syndication'                        
         DC    AL1(MEDTMCBL),AL2(0775),CL18'Cable'                              
         DC    AL1(MEDTMNET),AL2(0777),CL18'Network'                            
         DC    AL1(MEDTMALL),AL2(7777),CL18'National Network'                   
MEDTABN  EQU   (*-MEDTAB)/MEDTABL                                               
MEDTABX  DC    AL1(MEDTMEOT)                                                    
                                                                                
MEDTABD  DSECT                     ** MEDIA TABLE LAYOUT **                     
MEDTMED  DS    CL(L'AGYMEDCD)      MEDIA CODE                                   
MEDTMCBL EQU   C'C'                CABLE                                        
MEDTMNET EQU   C'N'                NETWORK                                      
MEDTMRAD EQU   C'D'                NETWORK RADIO                                
MEDTMSYN EQU   C'S'                SYNDICATION                                  
MEDTMHIS EQU   C'H'                HISPANIC                                     
MEDTMOTH EQU   C'O'                OTHER                                        
MEDTMALL EQU   C'*'                ALL MEDIA                                    
MEDTMEOT EQU   0                   END OF TABLE INDICATOR                       
MEDTGMKT DS    AL2                 GOAL MARKET NUMBER                           
MEDTNAME DS    CL18                MEDIA NAME                                   
MEDTABL  EQU   *-MEDTABD                                                        
SVRDEF   CSECT                                                                  
                                                                                
DPRDLIST DC    AL2(0),C'Dummy'                                                  
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(PRDL-SAVED),AL1(PRDLL)                                       
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    XL4'00'                                                          
         EJECT                                                                  
NETSYSQ  EQU   3                                                                
                                                                                
***********************************************************************         
* OPTIMIZATION MASK BITS                                              *         
***********************************************************************         
                                                                                
MO#CON   EQU   02                  CONTRACT NUMBER SAME AS LAST                 
MO#DEAL  EQU   03                  DEAL NUMBER SAME AS LAST                     
MO#SER   EQU   04                  SERIAL NUMBER SAME AS LAST                   
MO#PRSLT EQU   05                  POSTING RESULT SAME AS LAST                  
MO#POD   EQU   06                  POD SAME AS PREVIOUS                         
MO#CNTNT EQU   07                  CONTENT SAME AS PREVIOUS                     
MO#TIERN EQU   08                  TIER NUMBER SAME AS PREVIOUS                 
MO#NORET EQU   09                  NEW OR RETURNING SAME AS PREVIOUS            
MO#COMBO EQU   10                  COMBO-CODE SAME AS PREVIOUS                  
MO#MULTI EQU   11                  MULTI-RUN SAME AS PREVIOUS                   
MO#TCARL EQU   12                  TCAR LEVEL SAME AS PREVIOUS                  
MO#SUBDP EQU   13                  SUB-DAYPART SAME AS PREVIOUS                 
MO#MCODE EQU   14                  MIRROR CODE SAME AS PREVIOUS                 
MO#PSPTY EQU   15                  PROGRAM/SUB TYPE SAME AS PREVIOUS            
MO#SRCOD EQU   16                  SPECIAL RATE CODE SAME AS PREVIOUS           
MO#SMTYP EQU   17                  SUB-MEDIA TYPE SAME AS PREVIOUS              
MO#CCOM2 EQU   18                  CLIENT COMMENT 2 SAME AS PREVIOUS            
MO#CCOM1 EQU   19                  CLIENT COMMENT 1 SAME AS PREVIOUS            
MO#COMM2 EQU   20                  COMMERCIAL 2 SAME AS PREVIOUS                
MO#COMM1 EQU   21                  COMMERCIAL 1 SAME AS PREVIOUS                
MO#AFTIM EQU   22                  AFFIDAVIT TIME SAME AS PREVIOUS              
MO#AFDAT EQU   23                  AFFIDAVIT DATE SAME AS PREVIOUS              
MO#BTYPE EQU   24                  BUY TYPE SAME AS PREVIOUS                    
MO#NTICD EQU   25                  NTI CODE SAME AS PREVIOUS                    
MO#ALLOC EQU   26                  ALLOCATION SAME AS PREVIOUS                  
MO#ASCST EQU   27                  ASSIGNED COST SAME AS PREVIOUS               
MO#INCST EQU   28                  INTEGRATION COST SAME AS PREVIOUS            
MO#ACCST EQU   29                  ACTUAL COST SAME AS PREVIOUS                 
MO#RCODE EQU   30                  REASON CODE SAME AS PREVIOUS                 
MO#UCODE EQU   31                  UNIVERSE CODE SAME AS PREVIOUS               
MO#DEMOS EQU   32                  DEMOS SAME AS PREVIOUS                       
MO#HHUT  EQU   33                  HOMES HUT SAME AS PREVIOUS                   
MO#HSHR  EQU   34                  HOMES SHR SAME AS PREVIOUS                   
MO#HIMP  EQU   35                  HOMES IMP SAME AS PREVIOUS                   
MO#HGRP  EQU   36                  HOMES GRP SAME AS PREVIOUS                   
MO#PTYPE EQU   37                  POSTING TYPE SAME AS PREVIOUS                
MO#STYPE EQU   38                  STATION TYPE SAME AS PREVIOUS                
MO#BKBAS EQU   39                  BOOK BASIS SAME AS PREVIOUS                  
MO#SLSEQ EQU   40                  SUB-LINE IS IN SEQUENCE                      
                                                                                
***********************************************************************         
* UNIT STATUS MASK BITS                                               *         
***********************************************************************         
                                                                                
MQ#NEWRB EQU   43                  NEW RATING BOOK (>= JAN'89)                  
MQ#TPAID EQU   44                  TIME CHARGE PAID                             
MQ#IPAID EQU   45                  INTEGRATION CHARGE PAID                      
MQ#OPAID EQU   46                  OTHER CHARGE PAID                            
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
         EJECT                                                                  
***********************************************************************         
* REQUEST MAPS                                                        *         
***********************************************************************         
                                                                                
REQUNIT  LKREQ H,32,OUTUNIT        ** STEWARD UNIT/GOAL DOWNLOAD **             
Media    LKREQ F,22,(I,B#SAVED,MEDIND),CHAR,LIST=NOD,OLEN=L'STYPE,     *        
               COL=*,TEXT=NE#MED                                                
RecRd    LKREQ F,21,(D,B#SAVED,RECORD),CHAR,COL=*,TEXT=NE#RFILT                 
Dates    LKREQ F,1,(D,B#SAVED,STRDATE),CDAT,RANGE=Y,COL=*,TEXT=NE#SEDAT         
SDate    LKREQ F,24,(D,B#SAVED,STRDATE),CDAT,COL=*,TEXT=NE#SDATE                
EDate    LKREQ F,25,(D,B#SAVED,ENDDATE),CDAT,COL=*,TEXT=NE#EDATE                
CltCd    LKREQ F,2,(I,B#SAVED,CLTIND),(R,VALCLT),OLEN=L'NBSELCL2,      *        
               MAXLEN=L'NBSELCLI,LIST=NOD,DEFAULT=Y,COL=*,TEXT=NE#CCODS         
PrdCd    LKREQ F,3,(I,B#SAVED,PRDIND),(R,VALPRD),OLEN=L'NBSELPRD,      *        
               MAXLEN=L'NBSELPRD,LIST=NOD,DEFAULT=Y,COL=*,TEXT=NE#PCODS         
EstNo    LKREQ F,4,(I,B#SAVED,ESTIND),UBIN,OLEN=L'NBSELEST,LIST=NOD,   *        
               RANGE=Y,DEFAULT=Y,COL=*,TEXT=NE#ENRNG                            
Ntwks    LKREQ F,5,(I,B#SAVED,NWKIND),CHAR,OLEN=L'NBSELNET,LIST=NOD,   *        
               DEFAULT=Y,COL=*,TEXT=NE#NTWKS                                    
Days     LKREQ F,6,(I,B#SAVED,DAYIND),(R,VALDAY),OLEN=L'NUKDDAY,       *        
               MAXLEN=11,DEFAULT=Y,COL=*,TEXT=NE#ROTTN                          
Times    LKREQ F,7,(I,B#SAVED,TIMIND),(R,VALTIM),OLEN=L'NUKDTIME,      *        
               MAXLEN=12,RANGE=Y,DEFAULT=Y,COL=*,TEXT=NE#TIMES                  
PkgNo    LKREQ F,8,(I,B#SAVED,PKNIND),UBIN,OLEN=L'NUPACK,LIST=Y,       *        
               RANGE=Y,DEFAULT=Y,COL=*,TEXT=NE#PKGNS                            
DayPt    LKREQ F,9,(I,B#SAVED,DPTIND),(R,VALDPT),OLEN=L'NBSELDP,       *        
               MAXLEN=L'NDPTDPTA,LIST=NOD,DEFAULT=Y,COL=*,TEXT=NE#DPRTS         
ProgN    LKREQ F,10,(I,B#SAVED,PRGIND),CHAR,OLEN=L'NBSELPRG,           *        
               LIST=NOD,DEFAULT=Y,COL=*,TEXT=NE#PRGMS                           
Demos    LKREQ F,11,(I,B#SAVED,DEMIND),(R,VALDEMO),OLEN=11,MAXLEN=11,  *        
               LIST=NOD,COL=*,TEXT=NE#DEMOS,SORT=NO                             
*&&DO                                                                           
*                                                                               
* OLD WAY OF VALIDATING DEMOS - REMOVED 7/18 SCHT                               
*                                                                               
Demos    LKREQ F,11,(I,B#SAVED,DEMIND),CHAR,OLEN=11,MAXLEN=11,         *        
               LIST=NOD,COL=*,TEXT=NE#DEMOS,SORT=NO                             
*                                                                               
* OLD WAY OF VALIDATING DEMOS - REMOVED 11/17 SCHT                              
*                                                                               
Demos    LKREQ F,11,(D,B#NETIOD,NDDEMOS+3),(R,VALDEM),OLEN=3,MAXLEN=11,*        
               LIST=NOD,COL=*,TEXT=NE#DEMOS                                     
*&&                                                                             
RtgFl    LKREQ F,26,(D,B#SAVED,RFLAVOR),CHAR,COL=*,TEXT=NE#RTGFL                
RtgFl2   LKREQ F,30,(D,B#SAVED,RFLAVOR2),CHAR,COL=*,TEXT=NE#RTGFL,     *        
               MAXLEN=12                                                        
ERtgO    LKREQ F,27,(D,B#SAVED,ERTG),CHAR,COL=*,TEXT=NE#EQVRO                   
EImpO    LKREQ F,28,(D,B#SAVED,EIMP),CHAR,COL=*,TEXT=NE#EQVIO                   
Line#    LKREQ F,12,(I,B#SAVED,SUBIND),UBIN,OLEN=L'NUKSUB,LIST=NOD,    *        
               RANGE=Y,COL=*,TEXT=NE#SUBLS                                      
SptLn    LKREQ F,13,(I,B#SAVED,LENIND),UBIN,OLEN=L'NULEN,LIST=NOD,     *        
               DEFAULT=Y,COL=*,TEXT=NE#LENFT                                    
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
Other    LKREQ F,29,(D,B#SAVED,OTHERS),LBIN,COL=*,TEXT=NE#OTHER                 
DateTime LKREQ F,31,(D,B#SAVED,DTETIM),HEXD,COL=*,TEXT=NE#FDTE                  
Univs?   LKREQ F,32,(D,B#SAVED,REQUNIV),CHAR,COL=*,TEXT=NE#UNIVS                
         LKREQ E                                                                
                                                                                
REQMFMM  LKREQ *,X'FFFD',OUTMFMM   ** MFM MEDIA DOWNLOAD **                     
                                                                                
REQMFMC  LKREQ *,X'FFFC',OUTMFMC   ** MFM CLIENT DOWNLOAD **                    
                                                                                
REQMFMP  LKREQ H,X'FFFB',OUTMFMP   ** MFM PRODUCT DOWNLOAD **                   
SysCd    LKREQ F,4,(I,B#SAVED,SMCIND),CHAR,ARRAY=S,OLEN=1,COL=*,       *        
               TEXT=NE#SCODE                                                    
MedCd    LKREQ F,5,(I,B#SAVED,SMCIND),CHAR,OLEN=1,COL=*,TEXT=NE#MED             
CliCd    LKREQ F,6,(I,B#SAVED,SMCIND),CHAR,ARRAY=E,OLEN=3,COL=*,       *        
               TEXT=NE#CCOD                                                     
         LKREQ E                                                                
                                                                                
REQMFMV  LKREQ H,X'FFFA',OUTMFMV   ** MFM VENDOR DOWNLOAD **                    
SysCd    LKREQ F,1,(I,B#SAVED,SMCIND),CHAR,ARRAY=S,OLEN=1,COL=*,       *        
               TEXT=NE#SCODE                                                    
MedCd    LKREQ F,2,(I,B#SAVED,SMCIND),CHAR,ARRAY=E,OLEN=1,COL=*,       *        
               TEXT=NE#MED                                                      
         LKREQ E                                                                
                                                                                
REQMVAN  LKREQ H,X'FFF8',OUTSTAT   ** MEDIAVANTAGE DOWNLOAD **                  
                                                                                
MNode    LKREQ F,21,(I,B#SAVED,MEDIND),LBIN,OLEN=4,LIST=F,             *        
               TEXT=(2,SP#MCONT),COL=*                                          
ANode    LKREQ F,22,(I,B#SAVED,ADVIND),LBIN,OLEN=4,LIST=F,             *        
               TEXT=(2,SP#ACONT),COL=*                                          
SNode    LKREQ F,23,(I,B#SAVED,SUPNODE),LBIN,TEXT=(2,SP#SCONT),COL=*,  *        
               LIST=F,OLEN=4                                                    
SDate    LKREQ F,24,(D,B#SAVED,STRDATEE),EDAT,TEXT=(2,SP#STDT),COL=*            
EDate    LKREQ F,25,(D,B#SAVED,ENDDATEE),EDAT,TEXT=(2,SP#ENDT),COL=*            
CalOp    LKREQ F,26,(D,B#SAVED,CALOPTN),CHAR,TEXT=(2,SP#CALOP),COL=*            
Split    LKREQ F,27,(D,B#SAVED,SPLOPTN),CHAR,TEXT=(2,SP#SPLOP),COL=*            
Sumry    LKREQ F,28,(D,B#SAVED,SUMOPTN),CHAR,TEXT=(2,SP#SUMRZ),COL=*            
PrgNm    LKREQ F,29,(D,B#SAVED,PNTOPTN),CHAR,TEXT=(2,SP#PRGNM),COL=*            
BrdNd    LKREQ F,30,(I,B#SAVED,BRDIND),LBIN,OLEN=4,ARRAY=S,            *        
               TEXT=(2,SP#BCONT),COL=*                                          
CliCd    LKREQ F,31,(I,B#SAVED,BRDIND),CHAR,OLEN=L'NBSELCL2,           *        
               TEXT=(2,SP#CLI),COL=*                                            
BrdCd    LKREQ F,32,(I,B#SAVED,BRDIND),CHAR,OLEN=L'CPLPMNEM,ARRAY=E,   *        
               TEXT=(2,SP#PRO),COL=*                                            
EstOp    LKREQ F,33,(D,B#SAVED,ESTOPTN),CHAR,TEXT=(2,SP#ESTOP),COL=*            
EstNm    LKREQ F,34,(D,B#SAVED,ESNOPTN),CHAR,TEXT=(*,ESNLIT),COL=*              
                                                                                
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
                                                                                
ESNLIT   DC    C'Estimate names required?'                                      
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS                                                         *         
***********************************************************************         
                                                                                
OUTUNIT  LKOUT H                   ** UNIT AND GOAL DOWNLOAD **                 
OUTUNT   LKOUT R,34                                                             
Array    LKOUT C,34,(A,ARYUNT),FILTROUT=UNIVNO                                  
Array    LKOUT C,34,(A,ARYUNIV),FILTROUT=UNIVYES                                
         LKOUT E                                                                
OUTGOL   LKOUT R,35                                                             
Array    LKOUT C,35,(A,ARYGOL)                                                  
         LKOUT E                                                                
OUTDTM   LKOUT R,37                                                             
Array    LKOUT C,37,(A,ARYDTM)                                                  
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
OUTMFMM  LKOUT H                   ** MFM MEDIA DOWNLOAD **                     
         LKOUT R,1                                                              
LimAc    LKOUT C,1,(D,B#NETIOD,NBTWAACC),CHAR,ND=Y                              
Array    LKOUT C,1,(A,ARYMED)                                                   
         LKOUT X                                                                
                                                                                
OUTMFMC  LKOUT H                   ** MFM CLIENT DOWNLOAD **                    
         LKOUT R,2                                                              
Array    LKOUT C,1,(A,ARYCLT)                                                   
         LKOUT X                                                                
                                                                                
OUTMFMP  LKOUT H                   ** MFM PRODUCT DOWNLOAD **                   
         LKOUT R,2                                                              
Array    LKOUT C,1,(A,ARYPRD)                                                   
         LKOUT X                                                                
                                                                                
OUTMFMV  LKOUT H                   ** MFM VENDOR DOWNLOAD **                    
         LKOUT R,2                                                              
Array    LKOUT C,1,(A,ARYVEN)                                                   
         LKOUT X                                                                
                                                                                
OUTSTAT  LKOUT H                   ** STATS DOWNLOAD **                         
         LKOUT R,2                                                              
Array    LKOUT C,1,(A,ARYSTAT)                                                  
         LKOUT X                                                                
                                                                                
UNIVNO   CLI   REQUNIV,YESQ        REQUEST UNIVERSES ONLY?                      
         JE    EXITN                                                            
         CLI   REQUNIV,C'H'        REQUEST HISPANIC UNIVERSES ONLY?             
         JE    EXITN                                                            
         J     EXITY                                                            
                                                                                
UNIVYES  CLI   REQUNIV,YESQ        REQUEST UNIVERSES ONLY?                      
         JE    EXITY                                                            
         CLI   REQUNIV,C'H'        REQUEST HISPANIC UNIVERSES ONLY?             
         JE    EXITY                                                            
         J     EXITN                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR UNIT DOWNLOAD                                  *         
***********************************************************************         
                                                                                
ARYUNT   LKOUT A,(R,GETUNT),MULTIROW=Y,ROWNAME=NURECD                           
SMask    LKOUT C,68,(D,B#SAVED,SAMEMASK),HDRO,ND=Y,PCVERSION=2.0.0.0            
UStat    LKOUT C,52,(D,B#LP,LP_RMASK),HEXD,ND=Y                                 
DBUYTYPE DS    0X                                                               
BType    LKOUT C,1,(D,B#SAVED,BUYTYPE),CHAR,ND=Y                                
DCLT     DS    0X                                                               
CltCd    LKOUT C,3,(D,,NUKCLT),(R,EDTCLT),LEN=L'NBCLICOD,ND=Y                   
DACTCOST DS    0X                                                               
ACost    LKOUT C,4,(D,B#NETIOD,NBACTUAL),CBIN,ND=Y                              
DDATE    DS    0X                                                               
ADate    LKOUT C,5,(D,,NUKDATE),CDAT,ND=Y                                       
DROT     DS    0X                                                               
RDays    LKOUT C,6,(D,B#NETIOD,NBSDROT),UBIN,ND=Y                               
DDAYPART DS    0X                                                               
DptCd    LKOUT C,7,(D,,NUKDP),(R,EDTDPT),LEN=L'NUKDP,ND=Y                       
DEST     DS    0X                                                               
EstNo    LKOUT C,8,(D,,NUKEST),UBIN,ND=Y                                        
DASSIGN  DS    0X                                                               
AsCst    LKOUT C,9,(D,B#NETIOD,NBASSIGN),CBIN,ND=Y                              
DNETWORK DS    0X                                                               
NetWk    LKOUT C,10,(D,,NUKNET),CHAR,ND=Y                                       
DNTICODE DS    0X                                                               
NTICd    LKOUT C,11,(D,B#SAVED,NTICODE),UBIN,ND=Y                               
DPACKAGE DS    0X                                                               
PkgNo    LKOUT C,12,(D,,NUPACK),UBIN,ND=Y                                       
DINTCOST DS    0X                                                               
IntCs    LKOUT C,13,(D,B#NETIOD,NBINTEG),CBIN,ND=Y                              
Array    LKOUT C,99,(A,ARYBRD)                                                  
DPROG    DS    0X                                                               
PrgCd    LKOUT C,15,(D,,NUKPROG),CHAR,ND=Y                                      
DPRGNAM  DS    0X                                                               
PrgNm    LKOUT C,16,(D,,NUPROGNM),CHAR,ND=Y                                     
DSTATYPE DS    0X                                                               
SType    LKOUT C,17,(D,B#SAVED,STATYPE),CHAR,ND=Y                               
DTIME    DS    0X                                                               
ATime    LKOUT C,18,(D,B#NETIOD,NBTIME),(R,EDTTIM),LEN=11,ND=Y                  
DSECONDS DS    0X                                                               
SecLn    LKOUT C,19,(D,,NULEN),UBIN,ND=Y                                        
DPSTTYPE DS    0X                                                               
PType    LKOUT C,60,(D,B#SAVED,UNPOSTYP),CHAR,ND=Y                              
DHGRP    DS    0X                                                               
HGrps    LKOUT C,21,(D,B#NETIOD,NDESTHOM+2),UBIN,LEN=2                          
DHIMP    DS    0X                                                               
HImps    LKOUT C,22,(D,B#NETIOD,NDESTHOM+4),UBIN,LEN=4                          
DHSHR    DS    0X                                                               
HShar    LKOUT C,26,(D,B#NETIOD,NBESTSHR),UBIN                                  
DHHUT    DS    0X                                                               
HHuts    LKOUT C,27,(D,B#NETIOD,NBESTHUT),UBIN                                  
Array    LKOUT C,99,(A,ARYDEM)                                                  
DHGRPA   DS    0X                                                               
HGrps    LKOUT C,108,(D,B#NETIOD,NDACTHOM+2),UBIN,LEN=2,               *        
               PCVERSION=5.2.0.3                                                
DHIMPA   DS    0X                                                               
HImps    LKOUT C,109,(D,B#NETIOD,NDACTHOM+4),UBIN,LEN=4,               *        
               PCVERSION=5.2.0.3                                                
DHSHRA   DS    0X                                                               
HShar    LKOUT C,110,(D,B#NETIOD,NBACTSHR),UBIN,                       *        
               PCVERSION=5.2.0.3                                                
DHHUTA   DS    0X                                                               
HHuts    LKOUT C,111,(D,B#NETIOD,NBACTHUT),UBIN,                       *        
               PCVERSION=5.2.0.3                                                
Array    LKOUT C,99,(A,ARYDEMA)                                                 
DSUBMEDT DS    0X                                                               
SMTyp    LKOUT C,28,(D,B#SAVED,SUBMEDT),CHAR,ND=Y                               
DSPRCODE DS    0X                                                               
SRCod    LKOUT C,29,(D,B#SAVED,SPRCODE),CHAR,ND=Y                               
DMIRRORC DS    0X                                                               
MCode    LKOUT C,30,(D,B#SAVED,MIRRORC),CHAR,ND=Y                               
DREASONC DS    0X                                                               
RsnCd    LKOUT C,43,(D,B#SAVED,REASONC),CHAR,ND=Y                               
DAFIDATE DS    0X                                                               
AFDat    LKOUT C,44,(D,B#SAVED,AFIDATE),CDAT,ND=Y                               
DAFITIME DS    0X                                                               
AFTim    LKOUT C,45,(D,,NUAFFTIM),UBIN,ND=Y                                     
DOVAFTIM DS    0X                                                               
AFTim    LKOUT C,103,(D,B#SAVED,OVAFFTIM),UBIN,ND=Y,PCVERSION=5.2.0.3           
SpecRep  LKOUT C,112,(D,B#NETIOD,NBSREP),UBIN,ND=Y,PCVERSION=5.2.0.3            
DSUBDPRT DS    0X                                                               
SubDp    LKOUT C,46,(D,B#SAVED,SUBDPRT),CHAR,ND=Y                               
SubLn    LKOUT C,47,(D,B#SAVED,SUBLINE),UBIN,ND=Y                               
DBOOKBAS DS    0X                                                               
BkBas    LKOUT C,49,(D,B#SAVED,BKBASIS),CHAR,ND=Y                               
DTCARLEV DS    0X                                                               
TCARL    LKOUT C,50,(D,B#SAVED,TCARLEV),CHAR,ND=Y                               
DUNVCODE DS    0X                                                               
UniCd    LKOUT C,51,(D,,NUUNCODE),HEXD,ND=Y                                     
DCLICOM1 DS    0X                                                               
Com#1    LKOUT C,56,(D,B#SAVED,CLICOM1),CHAR,ND=Y                               
DCLICOM2 DS    0X                                                               
Com#2    LKOUT C,57,(D,B#SAVED,CLICOM2),CHAR,ND=Y                               
DMLTIRUN DS    0X                                                               
Multi    LKOUT C,59,(D,B#SAVED,MLTIRUN),UBIN,ND=Y                               
DCOMBOCD DS    0X                                                               
Combo    LKOUT C,61,(D,B#SAVED,COMBOCD),CHAR,ND=Y                               
DPROGTYP DS    0X                                                               
PSTyp    LKOUT C,62,(D,B#SAVED,PROGTYP),CHAR,LEN=PROGTYPL,ND=Y,        *        
               PCVERSION=2.0.0.0                                                
DNEWORET DS    0X                                                               
NwORt    LKOUT C,63,(D,B#SAVED,NEWORET),CHAR,ND=Y,PCVERSION=2.0.0.0             
DTIERNUM DS    0X                                                               
Tier#    LKOUT C,64,(D,B#SAVED,TIERNUM),CHAR,ND=Y,PCVERSION=2.0.0.0             
DCONTENT DS    0X                                                               
Cntnt    LKOUT C,65,(D,B#SAVED,CONTENT),CHAR,ND=Y,PCVERSION=2.0.0.0             
DCOMCOD1 DS    0X                                                               
Film1    LKOUT C,66,(D,B#SAVED,COMCOD1),CHAR,ND=Y,PCVERSION=2.0.0.0             
DCOMCOD2 DS    0X                                                               
Film2    LKOUT C,67,(D,B#SAVED,COMCOD2),CHAR,ND=Y,PCVERSION=2.0.0.0             
DCOMSCOD DS    0X                                                               
Film1    LKOUT C,104,(D,B#SAVED,COMSCOD),CHAR,ND=Y,PCVERSION=5.2.0.3            
DPOD     DS    0X                                                               
P.O.D    LKOUT C,71,(D,B#SAVED,POD),CHAR,ND=Y,PCVERSION=3.0.0.6                 
DRESULT  DS    0X                                                               
PRslt    LKOUT C,72,(D,B#SAVED,RESULT),CHAR,ND=Y,PCVERSION=4.0.0.0              
DSERIAL# DS    0X                                                               
SerNo    LKOUT C,73,(D,B#SAVED,SERIAL#),CHAR,ND=Y                               
DDEAL#   DS    0X                                                               
Deal#    LKOUT C,74,(D,B#SAVED,DEAL#),CHAR,ND=Y                                 
DCON#    DS    0X                                                               
ConNo    LKOUT C,75,(D,B#SAVED,CON#),CHAR,ND=Y                                  
DVTYPE   DS    0X                                                               
Vtype    LKOUT C,77,(D,B#SAVED,VTYPE),CHAR,ND=Y                                 
DSTYPE   DS    0X                                                               
Stype    LKOUT C,78,(D,B#NETIOD,NBSTATYP),CHAR,ND=Y                             
DSMEDIA  DS    0X                                                               
SubMed   LKOUT C,79,(D,B#NETIOD,NBSDSBMD),CHAR,ND=Y                             
DFLIGHT  DS    0X                                                               
FltId    LKOUT C,80,(D,B#SAVED,OVFLTID),CHAR,ND=Y                               
NETCOST  DS    0X                                                               
TimeInv  LKOUT C,114,(D,B#SAVED,INVTIME),CHAR,ND=Y,PCVERSION=5.2.0.31           
INTGINV  DS    0X                                                               
IntgInv  LKOUT C,115,(D,B#SAVED,INVINTG),CHAR,ND=Y,PCVERSION=5.2.0.31           
UNITDA   DS    0X                                                               
UnitDA   LKOUT C,116,(D,B#SAVED,UNTDA),HEXD,ND=Y,PCVERSION=5.2.0.32             
PRout    LKOUT P,(B#NETIOD,NBTIME),BLDTIME                                      
StartTim LKOUT C,117,(D,B#SAVED,UNTSTIM),HEXD,ND=Y,PCVERSION=6.2.0.1            
EndTime  LKOUT C,118,(D,B#SAVED,UNTETIM),HEXD,ND=Y,PCVERSION=6.2.0.1            
Array    LKOUT C,255,(A,ARYUNTBA)                                               
Array    LKOUT C,255,(A,ARYUNTSC)                                               
DPRDSTA  DS    0X                                                               
Prdsta   LKOUT C,133,(D,B#SAVED,PRDSTAT),UBIN                                   
Array    LKOUT C,255,(A,ARYUNTPT)                                               
Array    LKOUT C,255,(A,ARYUNTFD)                                               
*Array    LKOUT C,255,(A,ARYCDEMA)                                              
*Array    LKOUT C,255,(A,ARYCDEME)                                              
Array    LKOUT C,255,(A,ARYPRSMA)                                               
NSISrce  LKOUT C,148,(D,B#SAVED,NSISRCE),CHAR                                   
Vtype    LKOUT C,149,(D,B#SAVED,VTYPE),CHAR,ND=Y                                
COMSrce  LKOUT C,148,(D,B#SAVED,COMSRCE),CHAR,ND=Y                              
CSVtype  LKOUT C,149,(D,B#SAVED,CSVTYPE),CHAR,ND=Y                              
Window   LKOUT C,150,(D,B#SAVED,WINDOW),CDAT,ND=Y                               
COMSer   LKOUT C,151,(D,B#SAVED,CSSER#),CHAR,ND=Y                               
Air      LKOUT C,152,(D,B#SAVED,AIR),CHAR,ND=Y                                  
                                                                                
*&&DO                                                                           
*                                                                               
* REMOVED SPEC-30773 (SCHT)                                                     
*                                                                               
HGrps    LKOUT C,211,(D,B#SAVED,DOIND1HG),CHAR,LEN=1,ND=Y                       
HImps    LKOUT C,212,(D,B#SAVED,DOIND1HI),CHAR,LEN=1,ND=Y                       
HShar    LKOUT C,213,(D,B#SAVED,DOIND1HS),CHAR,LEN=1,ND=Y                       
HHuts    LKOUT C,214,(D,B#SAVED,DOIND1HH),CHAR,LEN=1,ND=Y                       
HGrps    LKOUT C,215,(D,B#SAVED,DOIND2HG),CHAR,LEN=1,ND=Y                       
HImps    LKOUT C,216,(D,B#SAVED,DOIND2HI),CHAR,LEN=1,ND=Y                       
HShar    LKOUT C,217,(D,B#SAVED,DOIND2HS),CHAR,LEN=1,ND=Y                       
HHuts    LKOUT C,218,(D,B#SAVED,DOIND2HH),CHAR,LEN=1,ND=Y                       
Array    LKOUT C,99,(A,ARYDEMO1)                                                
Array    LKOUT C,99,(A,ARYDEMO2)                                                
*&&                                                                             
         LKOUT E                                                                
                                                                                
ARYUNIV  LKOUT A,(R,GETUNT)                                                     
Cutoff   LKOUT C,200,(D,B#SAVED,UNIVCOD),CDAT,ND=Y                              
Univs    LKOUT C,255,(A,ARYUNIVS)                                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR UNIT DATE/TIME STAMP                           *         
***********************************************************************         
                                                                                
ARYDTM   LKOUT A,(D,B#SAVED,UNTDTM),ROWWIDTH=L'UNTDTM,NROWS=1                   
DateTime LKOUT C,01,(D,B#SAVED,UNTDTM),HEXD,PCVERSION=5.2.0.32                  
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR UNIT PRODUCT ALPHA ARRAY                       *         
***********************************************************************         
                                                                                
ARYBRD   LKOUT A,(D,B#SAVED,PRDLIST),ROWWIDTH=PRDLLEN,EOT=0                     
PRout    LKOUT P,,SETDLM           SET DELIMETER                                
PrdCd    LKOUT C,14,(D,,PRDLIST),CHAR,OLIST=C,LEN=L'PKEYPRD,ND=Y                
         LKOUT E                                                                
SETDLM   L     R1,ALP                                                           
         MVI   LP_ODELM-LP_D(R1),C'*'      SET DELIMETER FOR PRODUCT            
         BR    RE                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DEMO VALUES                                    *         
***********************************************************************         
                                                                                
ARYDEM   LKOUT A,(D,B#NETIOD,NDESTOTH),SEQUENCE=C,ROWWIDTH=8,          *        
               NROWS=(B#SAVED,#DEMOS)                                           
RGrps    LKOUT C,23,(D,,NDESTOTH+2),UBIN,LEN=2                                  
RImps    LKOUT C,24,(D,,NDESTOTH+4),UBIN,LEN=4                                  
RVphs    LKOUT C,25,(D,,NDESTOTH+0),UBIN,LEN=2                                  
         LKOUT E                                                                
                                                                                
ARYDEMA  LKOUT A,(D,B#NETIOD,NDACTOTH),SEQUENCE=C,ROWWIDTH=8,          *        
               NROWS=(B#SAVED,#DEMOS2)                                          
RGrps    LKOUT C,105,(D,,NDACTOTH+2),UBIN,LEN=2,PCVERSION=5.2.0.3               
RImps    LKOUT C,106,(D,,NDACTOTH+4),UBIN,LEN=4,PCVERSION=5.2.0.3               
RVphs    LKOUT C,107,(D,,NDACTOTH+0),UBIN,LEN=2,PCVERSION=5.2.0.3               
         LKOUT E                                                                
                                                                                
ARYDEMO1 LKOUT A,(D,B#SAVED,DOVIND1),SEQUENCE=C,ROWWIDTH=3,            *        
               NROWS=(B#SAVED,#DEMOS)                                           
GrpsInd  LKOUT C,221,(D,,DOVIND1+1),CHAR,LEN=1,PCVERSION=7.0.0.0                
ImpsInd  LKOUT C,222,(D,,DOVIND1+2),CHAR,LEN=1,PCVERSION=7.0.0.0                
VphsInd  LKOUT C,223,(D,,DOVIND1),CHAR,LEN=1,PCVERSION=7.0.0.0                  
         LKOUT E                                                                
                                                                                
ARYDEMO2 LKOUT A,(D,B#SAVED,DOVIND2),SEQUENCE=C,ROWWIDTH=3,            *        
               NROWS=(B#SAVED,#DEMOS2)                                          
GrpsInd  LKOUT C,224,(D,,DOVIND2+1),CHAR,LEN=1,PCVERSION=7.0.0.0                
ImpsInd  LKOUT C,225,(D,,DOVIND2+2),CHAR,LEN=1,PCVERSION=7.0.0.0                
VphsInd  LKOUT C,226,(D,,DOVIND2),CHAR,LEN=1,PCVERSION=7.0.0.0                  
         LKOUT E                                                                
                                                                                
ARYUNIVS LKOUT A,(D,B#SAVED,ACTUNIVV),SEQUENCE=C,ROWWIDTH=4,           *        
               NROWS=(B#SAVED,NDEMOS)                                           
Univs    LKOUT C,201,(D,,ACTUNIVV),UBIN,LEN=4                                   
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR COMSCORE DEMO VALUES                           *         
***********************************************************************         
                                                                                
ARYCDEMA LKOUT A,(D,B#UNIT,NUDATA),ROWID=(NUCOVD,NUCOV2LQ),            *        
               ROWWIDTH=(V,NUCOVLEN),EOT=0                                      
Demo     LKOUT C,140,NUCOVCAT,CHAR                                              
Type     LKOUT C,141,NUCOVMOD,CHAR                                              
Value    LKOUT C,142,(D,,NUCOVVAL),UBIN,LEN=4                                   
         LKOUT E                                                                
                                                                                
ARYCDEME LKOUT A,(D,B#UNIT,NUDATA),ROWID=(NUCOVD,NUCOV1LQ),            *        
               ROWWIDTH=(V,NUCOVLEN),EOT=0                                      
Demo     LKOUT C,143,NUCOVCAT,CHAR                                              
Type     LKOUT C,144,NUCOVMOD,CHAR                                              
Value    LKOUT C,145,(D,,NUCOVVAL),UBIN,LEN=4                                   
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PRISMA INFO                                    *         
***********************************************************************         
                                                                                
ARYPRSMA LKOUT A,(D,B#UNIT,NUDATA),ROWID=(NUPSELD,NUPSELQ),            *        
               ROWWIDTH=(V,NUPSLEN),EOT=0                                       
PrsmaID  LKOUT C,146,NUPSID,CHAR                                                
NetWk    LKOUT C,147,NUPSCDT,CDAT                                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR SPECIAL CHARGE INFORMATION                     *         
***********************************************************************         
                                                                                
ARYUNTBA LKOUT A,(D,B#UNIT,NUDATA),ROWID=(NUASCD,NUASCELQ),            *        
               ROWWIDTH=(V,NUASCLEN),EOT=0                                      
Fratamt  LKOUT C,119,NUASCAMT,CBIN,PCVERSION=6.3.0.53                           
Fratprd  LKOUT C,120,NUASCBPC,CHAR,ND=Y,PCVERSION=6.3.0.53                      
         LKOUT E                                                                
                                                                                
ARYUNTSC LKOUT A,(D,B#UNIT,NUDATA),ROWID=(NUSPREL,NUSPRELQ),           *        
               ROWWIDTH=(V,NUSPRLEN),EOT=0                                      
Rateseq  LKOUT C,121,NUSPRSEQ,UBIN,PCVERSION=6.3.0.53                           
Ratetyp  LKOUT C,122,NUSPRTYP,CHAR,ND=Y,PCVERSION=6.3.0.53                      
Comm     LKOUT C,123,NUSPRCOM,CHAR,ND=Y,PCVERSION=6.3.0.53                      
Scamount LKOUT C,124,NUSPRAMT,CBIN,ND=Y,PCVERSION=6.3.0.53                      
Scsrep   LKOUT C,125,NUSPRREP,CHAR,ND=Y,PCVERSION=6.3.0.53                      
Scstatus LKOUT C,126,NUSPRSTA,HEXD,ND=Y,PCVERSION=6.3.0.53                      
Prout    LKOUT P,NUSPREL,SCMKTSTA    Edit market station in in DUB              
Sccista  LKOUT C,127,(D,B#SAVED,CISTATIN),CHAR,ND=Y,PCVERSION=6.3.0.53          
*******  LKOUT C,127,(D,B#WORKD,DUB1),CHAR,ND=Y,LEN=L'STAPQSTA                  
Screvisn LKOUT C,128,NUSPRREV,UBIN,ND=Y,PCVERSION=6.3.0.53                      
Sccomind LKOUT C,129,NUSPRCMI,UBIN,ND=Y,PCVERSION=6.3.0.53                      
Scfeed   LKOUT C,130,NUSPRFED,CHAR,ND=Y,PCVERSION=6.3.0.53                      
Scbilprd LKOUT C,131,NUSPRBPC,CHAR,ND=Y,PCVERSION=6.3.0.53                      
Sctraprd LKOUT C,132,NUSPRTPC,CHAR,ND=Y,PCVERSION=6.3.0.53                      
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR UNIT PRODUCT FEED AND PERCENT INFORMATION      *         
***********************************************************************         
                                                                                
ARYUNTPT LKOUT A,(D,B#SAVED,PRDLIST),ROWWIDTH=PRDLLEN,EOT=0                     
Prdprod  LKOUT C,134,(D,,PRDLIST),CHAR,LEN=3,PCVERSION=6.5.0.0                  
PrdPct   LKOUT C,135,(D,,PRDLIST+3),UBIN,LEN=2,PCVERSION=6.5.0.0                
PrdFpct  LKOUT C,136,(D,,PRDLIST+5),UBIN,LEN=2,PCVERSION=6.5.0.0                
         LKOUT E                                                                
*                                                                               
ARYUNTFD LKOUT A,(D,B#SAVED,PRDFEED),ROWWIDTH=PRDFLEN,EOT=0                     
Prdfprd  LKOUT C,137,(D,,PRDFEED),UBIN,LEN=1,PCVERSION=6.5.0.0                  
Prdfeed  LKOUT C,138,(D,,PRDFEED+1),CHAR,LEN=4,ND=Y,PCVERSION=6.5.0.0           
Prdfsta  LKOUT C,139,(D,,PRDFEED+5),UBIN,LEN=1,PCVERSION=6.5.0.0                
         LKOUT E                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD GOAL DOWNLOAD                          *         
***********************************************************************         
                                                                                
ARYGOL   LKOUT A,(R,NXTGOL),MULTIROW=Y,ROWNAME=GOLVALS                          
CltCd    LKOUT C,1,(D,,GOLCLT),(R,EDTCLT),LEN=L'NBCLICOD,ND=Y                   
EstNo    LKOUT C,2,(D,,GOLEST),LBIN,ND=Y                                        
PrdCd    LKOUT C,3,(D,,GOLBRD),(R,EDTPRD),ND=Y                                  
PrdCd    LKOUT C,3,(D,,GOLPRDA),CHAR,ND=Y                                       
DptCd    LKOUT C,5,(D,,GOLDPT),(R,EDTDPT),ND=Y                                  
SecLn    LKOUT C,6,(D,,GOLSEC),LBIN,ND=Y                                        
Media    LKOUT C,7,(D,,GOLMED),CHAR,ND=Y                                        
NetWk    LKOUT C,8,(D,,GOLNET),CHAR,ND=Y                                        
Array    LKOUT C,36,(A,ARYGWK)                                                  
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD GOAL WEEKLY DOWNLOAD                   *         
***********************************************************************         
                                                                                
ARYGWK   LKOUT A,(R,GETGWK),NROWS=(B#SAVED,GWKTAB#),ROWNAME=GWKTABD,   *        
               ROWWIDTH=GWKTABL,NEWEL=B                                         
WkStD    LKOUT C,1,(D,,GWKSTR),EDAT,ND=Y                                        
Goal$    LKOUT C,2,(D,,GWKDOL),SPAK,ND=Y                                        
GRPs1    LKOUT C,3,(D,,GWKGRP1),SPAK,ND=Y                                       
GRPs2    LKOUT C,4,(D,,GWKGRP2),SPAK,ND=Y                                       
GRPs3    LKOUT C,5,(D,,GWKGRP3),SPAK,ND=Y                                       
RFact    LKOUT C,6,(D,,GWKREP#),LBIN,ND=Y                                       
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MFM MEDIA DOWNLOAD                             *         
***********************************************************************         
                                                                                
ARYMED   LKOUT A,(D,B#MEDTAB,MEDTABD),ROWWIDTH=MEDTABL,EOT=0                    
MedCd    LKOUT C,2,(D,,MEDTMED),CHAR                                            
MedNm    LKOUT C,3,(D,,MEDTNAME),CHAR                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MFM CLIENT DOWNLOAD                            *         
***********************************************************************         
                                                                                
ARYCLT   LKOUT A,(R,NXTCLT),MULTIROW=Y,ROWNAME=CLTHDR                           
CltCd    LKOUT C,2,(D,B#SAVED,CLTCODE),CHAR                                     
CltNm    LKOUT C,3,(D,,CNAME),CHAR                                              
CltHX    LKOUT C,4,(D,,CKEYCLT),HEXD                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MFM PRODUCT DOWNLOAD                           *         
***********************************************************************         
                                                                                
ARYPRD   LKOUT A,(R,NXTPRD),MULTIROW=Y,ROWNAME=PRDHDR                           
CltCd    LKOUT C,2,(D,B#SAVED,CLTCODE),CHAR,ND=Y                                
PrdCd    LKOUT C,3,(D,,PKEYPRD),CHAR                                            
PrdNm    LKOUT C,4,(D,,PNAME),CHAR                                              
PrdNo    LKOUT C,5,(D,,PCODE+1),HEXD,LEN=1                                      
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MFM VENDOR DOWNLOAD                            *         
***********************************************************************         
                                                                                
ARYVEN   LKOUT A,(R,NXTVEN),MULTIROW=Y,ROWNAME=STAREC                           
SType    LKOUT C,1,(D,,STYPE),CHAR                                              
NetWk    LKOUT C,2,(D,,STAKCALL),CHAR,LEN=L'STAKCALL-1                          
MktNM    LKOUT C,3,(D,B#SAVED,NETNAME),CHAR,ND=Y                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MEDIAVANTAGE DOWNLOAD                          *         
***********************************************************************         
                                                                                
ARYSTAT  LKOUT A,(R,NXTSTAT),MULTIROW=Y,ROWNAME=NPVALUES                        
                                                                                
MedCd    LKOUT C,1,(D,,NPMEDCDE),CHAR,ND=Y                                      
CltCd    LKOUT C,2,(D,,NPCLTCDE),CHAR,ND=Y                                      
CltNd    LKOUT C,3,(D,,NPCLTNDE),UBIN,ND=Y                                      
MktSt    LKOUT C,4,(D,,NPSTANET),CHAR,ND=Y                                      
VenNd    LKOUT C,5,(D,,NPVENNDE),UBIN,ND=Y                                      
PrdCd    LKOUT C,6,(D,,NPPRDCDE),CHAR,ND=Y                                      
PrdNd    LKOUT C,7,(D,,NPPRDNDE),UBIN,ND=Y                                      
SptDt    LKOUT C,8,(D,,NPDATPER),CDAT,ND=Y                                      
WklRt    LKOUT C,9,(D,,NPWKYROT),UBIN,ND=Y                                      
SptLn    LKOUT C,10,(D,,NPUNTLEN),UBIN,ND=Y                                     
PrgNm    LKOUT C,11,(D,,NPPRGNAM),CHAR,ND=Y                                     
PrgTm    LKOUT C,12,(D,,NPPRGTIM),(R,EDTTIM),LEN=11,ND=Y                        
EstNo    LKOUT C,17,(D,,NPESTNUM),UBIN,ND=Y                                     
EstNm    LKOUT C,18,(D,,NPEDESC),CHAR,ND=Y                                      
Units    LKOUT C,13,(D,,NPUNITS),UBIN,ND=Y                                      
ZUnit    LKOUT C,14,(D,,NPZUNITS),UBIN,ND=Y                                     
NetAm    LKOUT C,15,(D,,NPNET),SPAK                                             
GrsAm    LKOUT C,16,(D,,NPGROSS),SPAK                                           
                                                                                
         LKOUT E                                                                
         LKARY T                                                                
         EJECT                                                                  
***********************************************************************         
* SYSTEM FACILITIES LIST                                              *         
***********************************************************************         
                                                                                
FACS     DS    0XL(RFACTABL)       ** SYSTEM FACILITIES **                      
         DC    AL1(QSOFDAT),AL2(CSOFDAT-COMFACSD,0)                             
         DC    AL1(QUNDAY),AL2(0,FUNDAY-FACD)                                   
         DC    AL1(QUNTIME),AL2(0,FUNTIME-FACD)                                 
         DC    AL1(QNETVALU),AL2(0,FNETVALU-FACD)                               
         DC    AL1(QDEMOCON),AL2(0,FDEMOCON-FACD)                               
         DC    AL1(QOFFICER),AL2(0,FOFFICER-FACD)                               
         DC    AL1(QDEMTABS),AL2(CDEMTABS-COMFACSD,0)                           
         DC    AL1(QDEMADDR),AL2(CDEMADDR-COMFACSD,0)                           
         DC    AL1(QDEMOUT),AL2(CDEMOUT-COMFACSD,0)                             
         DC    AL1(QDEMEL),AL2(CDEMEL-COMFACSD,0)                               
         DC    AL1(QDEMAINT),AL2(CDEMAINT-COMFACSD,0)                           
         DC    AL1(QDEMAND),AL2(CDEMAND-COMFACSD,0)                             
         DC    AL1(QDEMOMTH),AL2(CDEMOMTH-COMFACSD,0)                           
         DC    AL1(QDEMOVAL),AL2(CDEMOVAL-COMFACSD,0)                           
         DC    AL1(QDDISP),AL2(CT00AD0-COMFACSD,0)                              
         DC    AL1(QDAYVAL),AL2(0,FDAYVAL-FACD)                                 
         DC    AL1(QTIMVAL),AL2(0,FTIMVAL-FACD)                                 
         DC    AL1(QCLPACK),AL2(0,FCLPACK-FACD)                                 
         DC    AL1(QCLUNPK),AL2(0,FCLUNPK-FACD)                                 
         DC    AL1(QTSAR),AL2(0,FTSAR-FACD)                                     
         DC    AL1(QGETBRD),AL2(0,FGETBRD-FACD)                                 
         DC    AL1(QTRPACK),AL2(0,FTRPACK-FACD)                                 
         DC    AL1(QCFMIO),AL2(0,FCFMIO-FACD)                                   
         DC    AL1(RFACEOTQ)                                                    
                                                                                
ABENDS   DC    C'SCHT:'                                                         
SLOWS    DC    C'SCHT:'                                                         
         EJECT                                                                  
LVALUES  DS    0D                                                               
         DC    A(FLTACS)                                                        
         DC    V(NETBLRDR)                                                      
         DC    V(NETNET)                                                        
         DC    X'00FF'                                                          
         DC    X'0001FFFF'                                                      
         DC    X'000001FFFFFF'                                                  
         DC    X'0000000001FFFFFFFFFF'                                          
LVALUESX DS    0X                                                               
         EJECT                                                                  
FF       EQU   X'FF'                                                            
NOQ      EQU   C'N'                                                             
YESQ     EQU   C'Y'                                                             
SPACEQ   EQU   C' '                                                             
WILD     EQU   C'*'                                                             
                                                                                
NETMEDQ  EQU   C'N'                NETWORK MEDIA LETTER                         
SPTLETQ  EQU   C'S'                SPOTPAK SYSTEM LETTER                        
                                                                                
MAXDEMS  EQU   25                  MAXIMUM N'DEMO TO LOOP UP                    
MAXUPRD  EQU   6                   MAXIMUM N'PRODUCTS PER UNIT                  
MAXESTS  EQU   255                 MAXIMUM N'ESTIMATES PER CLIENT               
MAXRECLN EQU   1000                MAXIMUM NORMAL RECORD LENGTH                 
MAXCLTLN EQU   1600                MAXIMUM LENGTH OF CLIENT RECORD              
MAXUNTLN EQU   6000                MAXIMUM UNIT RECORD LENGTH                   
POLPRDN  EQU   255                 'POL' PRODUCT NUMBER                         
USERDEMO EQU   33                  USER DEMO NUMBER                             
                                                                                
IOEDEL   EQU   X'02'               RECORD IS DELETED                            
IOERNF   EQU   X'10'               RECORD NOT FOUND                             
IOEEOF   EQU   X'80'               END OF FILE                                  
         EJECT                                                                  
FACD     DSECT                     ** SERVER FACILITIES **                      
FUNDAY   DS    A                   A(UNDAY)                                     
FUNTIME  DS    A                   A(UNTIME)                                    
FNETVALU DS    A                   A(NETVALUE)                                  
FDEMOCON DS    A                   A(DEMOCON)                                   
FOFFICER DS    A                   A(OFFICER)                                   
FDAYVAL  DS    A                   A(DAYVAL)                                    
FTIMVAL  DS    A                   A(TIMVAL)                                    
FCLPACK  DS    A                   A(CLPACK)                                    
FCLUNPK  DS    A                   A(CLUNPK)                                    
FTSAR    DS    A                   A(TSAR)                                      
FGETBRD  DS    A                   A(GETBROAD)                                  
FTRPACK  DS    A                   A(TRPACK)                                    
FCFMIO   DS    A                   A(CFMIO)                                     
                                                                                
SAVED    DSECT                     ** SERVER SAVED W/S **                       
                                                                                
VOFFICER DS    A                                                                
VTRPACK  DS    A                                                                
VXSORT   DS    A                                                                
VCLPACK  DS    A                                                                
VCFMIO   DS    A                                                                
                                                                                
RELO     DS    A                   RELOCATION FACTOR                            
ABASE    DS    A                   SERVER ENTRY POINT                           
ALP      DS    A                   A(DDLINK PARAMETER BLOCK)                    
VERSION  DS    XL(L'LP_VRSN1)      PC APPLICATION VERSION NUMBER                
                                                                                
KEYSAVE  DS    XL(L'NBKEY)         KEY SAVE AREA                                
                                                                                
SAVECLR  DS    0X                  ** START OF CLEARED AREA **                  
                                                                                
AKEYTAB  DS    A                   A(KEY TABLE)                                 
                                                                                
AESTTAB  DS    A                   A(ESTIMATE TABLE)                            
AIO1     DS    A                   A(I/O AREA 1)                                
AIO2     DS    A                   A(I/O AREA 2)                                
AIO3     DS    A                   A(I/O AREA 3)                                
AIO4     DS    A                   A(I/O AREA 4)                                
IOADDR   DS    A                   I/O AREA ADDRESS FOR NXTREC                  
FILETYPE DS    X                   FILE TYPE FOR NXTREC ROUTINE                 
MAP      DS    XL(L'LP_CMAPN)      REQUEST MAP NUMBER                           
CALLMODE DS    XL(L'RUNPMODE)      DDLINK/RUNNER CALLING MODE                   
                                                                                
DEMOFLAG DS    X                   DEMO LOOKUP FLAG                             
DEMOFUSR EQU   X'01'               USER DEMO(S) REQUESTED                       
                                                                                
CSINFO   DS    0X                                                               
CSSER#   DS    CL10                COMSCORE SERIES #                            
CSVTYPE  DS    CL2                 COMSCORE VIEWING TYPE                        
CSINFOLQ EQU   *-CSINFO                                                         
                                                                                
NDEMOS   DS    H                   N'DEMOS REQUESTED                            
#DEMOS   DS    H                   N'DEMOS TO SEND                              
#DEMOS2  DS    H                   N'DEMOS TO SEND                              
CLTCODE  DS    CL3                 CLIENT CODE                                  
NETNAME  DS    CL(L'MKTNAME)       MARKET (NETWORK) NAME                        
UNPOSTYP DS    CL(L'NUPOSTYP)      POST TYPE                                    
UNTDTM   DS    XL(L'NURLINK)       UNIT DATE/TIME STAMP                         
                                                                                
UNITEBCD DS    0C                  ** EXTRACTED ALPHA UNIT VALUES **            
BUYTYPE  DS    C                   BUY TYPE                                     
SUBDPRT  DS    CL2                 SUB-DAYPART                                  
SPRCODE  DS    CL(L'NUSDSRT)       SPECIAL RATE CODE                            
STATYPE  DS    CL(L'NUSTATYP)      STATION TYPE                                 
SUBMEDT  DS    CL(L'NUSDSBMD)      SUB-MEDIA TYPE                               
MIRRORC  DS    CL(L'NUMIRTYP)      MIRROR TYPE                                  
TCARLEV  DS    CL(L'NUSTCAR)       TCAR LEVEL                                   
REASONC  DS    CL5                 REASON CODE                                  
POD      DS    CL3                 POD                                          
BKBASIS  DS    C                   BOOK BASIS                                   
COMBOCD  DS    CL6                 COMBO CODE (AAA999)                          
CLICOM1  DS    CL60                CLIENT COMMENT LINE 1                        
CLICOM2  DS    CL60                CLIENT COMMENT LINE 2                        
PROGTYP  DS    0C                  **PROGRAM/SUB-PROGRAM TYPE **                
PROGTPRG DS    CL(L'NUDTATYP)      PROGRAM TYPE                                 
PROGTSUB DS    CL(L'NUDTASPT)      SUB-PROGRAM TYPE                             
PROGTYPL EQU   *-PROGTYP                                                        
NEWORET  DS    CL(L'NUDTANEW)      NEW OR RETURNING PROGRAM                     
TIERNUM  DS    CL(L'NUDTATIR)      TIER NUMBER                                  
CONTENT  DS    CL(L'NUDTARAT)      CONTENT                                      
OVAFFTIM DS    CL(L'NUAFFTIM)      OVERRIDE AFFID TIME                          
COMCOD1  DS    CL12                COMMERCIAL 1 CODE                            
COMCOD2  DS    CL12                COMMERCIAL 2 CODE                            
COMSCOD  DS    CL12                SCHEDULED COMMERCIAL CODE                    
RESULT   DS    CL2                 POSTING RESULT                               
SERIAL#  DS    CL(L'NUSQSER)       SERIAL NUMBER                                
DEAL#    DS    CL(L'NUSQDEAL)      DEAL NUMBER                                  
CON#     DS    CL(L'NUSQCON)       CONTRACT NUMBER                              
VTYPE    DS    CL3                 VTYPE                                        
NSISRCE  DS    CL3                                                              
COMSRCE  DS    CL3                                                              
OVFLTID  DS    CL10                OVERRIDE FLIGHT ID                           
INVTIME  DS    CL(L'NUDTINVN)      TIME INVOICE                                 
INVINTG  DS    CL(L'NUDTIIVN)      INTEGRATION INVOICE                          
CISTATIN DS    CL5                 CUT-IN STATION                               
AIR      DS    CL10                AIR NETWORK                                  
UNITEBCL EQU   *-UNITEBCD                                                       
UNITEBCX DS    0X                                                               
                                                                                
UNITBNRY DS    0X                  ** EXTRACTED BINARY UNIT VALUES **           
SUBLINE  DS    XL(L'NUKSUB)        SUB-LINE NUMBER                              
MLTIRUN  DS    AL1                 MULTI-RUN                                    
*                                                                               
PRDL     DS    0X                  ** PRODUCT LISTS **                          
PRDSTAT  DS    XL1                 product status's                             
*                                  x'80' - unit is a copy split                 
*                                  x'40' - unit has no national prod            
*                                                                               
PRDLPRD  EQU   0 3 BYTES           3 BYTE PRODUCT                               
PRDLPCT  EQU   3 2 BYTES           PRODUCT PCT                                  
PRDLFDPT EQU   5 2 BYTES           PRODUCT FEED PCT                             
PRDLLEN  EQU   7                                                                
PRDLIST  DS    XL((MAXUPRD*PRDLLEN)+1) PRODUCT APHA LIST                        
*                                                                               
PRDFPRD  EQU   0 1 BYTES           1 BYTE PRODUCT DISPLACEMENT                  
PRDFFEED EQU   1 4 BYTES           FEED CODE                                    
PRDFSTAT EQU   5 1 BYTES           STATUS                                       
*                                  X'80' - INSTRUCTIONS MEDIA CANT DEL          
PRDFLEN  EQU   6                                                                
PRDFEED  DS    XL((MAXUPRD*PRDFLEN)+1) PRODUCT APHA LIST                        
PRDLL    EQU   *-PRDL                                                           
***PRDL     DS    0X                  ** PRODUCT LISTS **                       
***PRDLIST  DS    XL((MAXUPRD*L'NUPRDPR)+1) PRODUCT CODE LIST                   
***PRDLISTA DS    XL((MAXUPRD*L'NUPDEPR)+1) PRODUCT APHA LIST                   
***PRDLL    EQU   *-PRDL                                                        
AFIDATE  DS    XL(L'NUSDAFDT)      AFFIDAVIT DATE                               
HOMEHUT  DS    XL4                 ESTIMATED HOMES HUT                          
HOMESHR  DS    XL4                 ESTIMATED HOMES SHARE                        
NTICODE  DS    XL4                 NTI CODE                                     
SAMEMASK DS    C                   RECORD MASK SAME AS PREVIOUS                 
UNTDA    DS    XL(L'NUDA)          UNIT DISK ADDRESS                            
UNTSTIM  DS    H                                                                
UNTETIM  DS    H                                                                
WINDOW   DS    XL2                 WINDOW AIRING DATE                           
UNITBNRL EQU   *-UNITBNRY                                                       
                                                                                
         ORG   UNITEBCD                                                         
GOLVALS  DS    0X                  ** GOAL HEADER VALUES **                     
GOLCLT   DS    CL(L'GKEYCLT)       CLIENT CODE                                  
GOLBRD   DS    CL(L'GKEYPRD)       BRAND CODE (EQUATE)                          
GOLPRDA  DS    CL(L'GXKPRDA)       BRAND CODE (ALPHA)                           
GOLSEC   DS    XL(L'GKEYSEC)       SECONDS LENGTH                               
GOLEST   DS    XL(L'GKEYEST)       ESTIMATE NUMBER                              
GOLMED   DS    CL(L'MEDTMED)       SUB-MEDIA CODE                               
GOLDPT   DS    CL(L'GKEYDPT)       DAYPART CODE                                 
GOLNET   DS    CL(L'GDNETWK)       NETWORK CODE                                 
GOLVALL  EQU   *-GOLVALS                                                        
                                                                                
GOALDATE DS    XL(L'GLWEEK)        GOAL WEEK START DATE                         
GWKTAB#  DS    H                   N'ENTRIES IN GWKTAB                          
         ORG                                                                    
                                                                                
LASTS    DS    0X                  ** LAST TIME VALUES **                       
LGETCLT  DS    XL(L'NUKCLT)        LAST TIME CLIENT CODE                        
LCLTCODE DS    CL(L'CLTCODE)       LAST CLIENT CODE SENT                        
LCLT     DS    XL(L'NUKCLT)        LAST CLIENT CODE SENT                        
LEST     DS    XL(L'NUKEST)        LAST ESTIMATE CODE SENT                      
LPROG    DS    CL(L'NUKPROG)       LAST PROGRAM CODE SENT                       
LPRGNAM  DS    CL(L'NUPROGNM)      LAST PROGRAM NAME SENT                       
LDAYPART DS    XL(L'NUKDP)         LAST DAYPART CODE SENT                       
LPACKAGE DS    XL(L'NUPACK)        LAST PACKAGE NUMBER SENT                     
LROT     DS    XL(L'NBSDROT)       LAST ROTATION DAYS SENT                      
LDATE    DS    XL(L'NUKDATE)       LAST AIR DATE SENT                           
LTIME    DS    XL(L'NBTIME)        LAST AIR TIME SENT                           
LSECONDS DS    XL(L'NULEN)         LAST SECONDS LENGTH SENT                     
LSUBLINE DS    XL(L'NUKSUB)        LAST SUB-LINE NUMBER SENT                    
LNTICODE DS    CL(L'NTICODE)       LAST NTI CODE SENT                           
LBUYTYPE DS    CL(L'BUYTYPE)       LAST BUY TYPE SENT                           
LSTATYPE DS    CL(L'STATYPE)       LAST STATION TYPE SENT                       
LPSTTYPE DS    CL(L'NBSURVEY)      LAST POSTING TYPE SENT                       
LACTCOST DS    XL(L'NBACTUAL)      LAST ACTUAL COST SENT                        
LUNVCODE DS    XL(L'NUUNCODE)      LAST UNIVERSE CODE SENT                      
LBOOKBAS DS    CL(L'BKBASIS)       LAST BOOK BASIS SENT                         
LHIMP    DS    XL4                 LAST HOMES IMP SENT                          
LHGRP    DS    XL2                 LAST HOMES GRP SENT                          
LHSHR    DS    XL(L'HOMESHR)       LAST HOMES SHR SENT                          
LHHUT    DS    XL(L'HOMEHUT)       LAST HOMES HUT SENT                          
LPROGTYP DS    CL(PROGTYPL)        LAST PROGRAM TYPE SENT                       
LNEWORET DS    CL(L'NEWORET)       LAST NEW/RETURNING PROGRAM SENT              
LCONTENT DS    CL(L'CONTENT)       LAST CONTENT SENT                            
LINTCOST DS    XL(L'NBINTEG)       LAST INTEGRATION COST SENT                   
LTIERNUM DS    CL(L'TIERNUM)       LAST TIER NUMBER SENT                        
LASSIGN  DS    XL(L'NBASSIGN)      LAST ASSIGNED COST SENT                      
LPRDLIST DS    XL(PRDLL)           LAST PRODUCT ALLOCATIONS SENT                
LSUBMEDT DS    CL(L'SUBMEDT)       LAST SUB-MEDIA TYPE SENT                     
LSPRCODE DS    CL(L'SPRCODE)       LAST SPECIAL RATE CODE SENT                  
LMIRRORC DS    CL(L'MIRRORC)       LAST MIRROR CODE SENT                        
LREASONC DS    CL(L'REASONC)       LAST REASON CODE SENT                        
LAFIDATE DS    XL(L'AFIDATE)       LAST AFFIDAVIT DATE SENT                     
LAFITIME DS    XL(L'NUAFFTIM)      LAST AFFIDAVIT TIME SENT                     
LSUBDPRT DS    CL(L'SUBDPRT)       LAST SUB-DAYPART CODE SENT                   
LTCARLEV DS    CL(L'TCARLEV)       LAST TCAR LEVEL SENT                         
LCLICOM1 DS    CL(L'CLICOM1)       LAST CLIENT COMMENT 1 SENT                   
LCLICOM2 DS    CL(L'CLICOM2)       LAST CLIENT COMMENT 2 SENT                   
LMLTIRUN DS    CL(L'MLTIRUN)       LAST MULTI-RUN VALUE SENT                    
LCOMBOCD DS    CL(L'COMBOCD)       LAST COMBO CODE SENT                         
LCOMCOD1 DS    CL(L'COMCOD1)       LAST COMMERCIAL CODE 1 SENT                  
LCOMCOD2 DS    CL(L'COMCOD2)       LAST COMMERCIAL CODE 2 SENT                  
LDEMOS   DS    XL(NDMAXDEM*8)      LAST DEMO VALUES SENT                        
LDEMOS2  DS    XL(NDMAXDEM*8)      LAST DEMO VALUES SENT                        
LPOD     DS    CL(L'POD)           LAST POD VALUE SENT                          
LRESULT  DS    CL(L'RESULT)        LAST POSTING RESULT SENT                     
LSERIAL# DS    CL(L'SERIAL#)       LAST SERIAL NUMBER SENT                      
LDEAL#   DS    CL(L'DEAL#)         LAST DEAL NUMBER SENT                        
LCON#    DS    CL(L'CON#)          LAST CONTRACT NUMBER SENT                    
LMASK    DS    XL(L'LP_RMASK)      LAST RECORD MASK                             
LGOLBRD  DS    XL(L'GOLBRD)        LAST GOAL BRANDS SENT                        
LGOLPRDA DS    XL(L'GOLPRDA)       LAST GOAL BRANDS SENT (ALPHA)                
LGOLMED  DS    XL(L'GOLMED)        LAST GOAL MEDIA SENT                         
LPOSTYPE DS    XL(L'NUPOSTYP)      LAST POSTING TYPE                            
LASTSL   EQU   *-LASTS                                                          
                                                                                
LNETS    DS    0X                  LAST NETWORK STATION VALUES                  
LNETWORK DS    CL(L'NUKNET)        LAST NETWORK CODE SENT                       
LNTISTA  DS    CL(L'NBNTISTA)      LAST NTI STATION CODE FOR LNETWORK           
LNETSL   EQU   *-LNETS                                                          
                                                                                
         ORG   UNITEBCD            ** CFMIO STORAGE **                          
         DS    0F                                                               
CFMIOC   DS    XL(CFMIOL)          CFMIO CONTROL BLOCK                          
       ++INCLUDE GESTATNET                                                      
         ORG                                                                    
                                                                                
SVALUES  DS    0D                  ** LITERAL VALUES (SEE LVALUES) **           
                                                                                
RELOLST  DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
AFLTACS  DS    A                   A(FLTACS)                                    
VNBLRDR  DS    A                   V(NETBLRDR)                                  
VNETNET  DS    A                   V(NETNET)                                    
RELOLSTN EQU   (*-RELOLST)/L'RELOLST                                            
                                                                                
SLNRNG   DS    XL2                 X'00'-X'FF'                                  
CLTRNG   DS    XL4                 X'0001'-X'FFFF'                              
PRDRNG   DS    XL6                 X'000001'-X'FFFFFF'                          
STARNG   DS    XL10                X'0000000001'-X'FFFFFFFFFF'                  
SVALUESL EQU   *-SVALUES                                                        
                                                                                
INDS     DS    X                   ** INDICATORS **                             
INDS2CDP EQU   X'80'               2 CHARACTER DAYPARTS IN USE                  
                                                                                
AGENCY   DS    CL(L'AGYKAGY)       AGENCY CODE                                  
AGYMED   DS    X                   AGENCY/MEDIA CODE                            
                                                                                
REQVALS  DS    0X                  ** REQUEST VALUES **                         
MEDIND   DS    X                   MEDIA INDICATOR                              
AMED     DS    AL3                 A(MEDIA(S))                                  
CLTIND   DS    X                   CLIENT INDICATOR                             
ACLT     DS    AL3                 A(CLIENT(S))                                 
BRDIND   DS    X                   BRAND INDICATOR                              
ABRD     DS    AL3                 A(BRAND(S))                                  
PRDIND   DS    X                   PRODUCT INDICATOR                            
APRD     DS    AL3                 A(PRODUCT(S))                                
ESTIND   DS    X                   ESTIMATE INDICATOR                           
AEST     DS    AL3                 A(ESTIMATE(S))                               
NWKIND   DS    X                   NETWORK INDICATOR                            
ANWK     DS    AL3                 A(NETWORK(S))                                
DPTIND   DS    X                   DAYPART INDICATOR                            
ADPT     DS    AL3                 A(DAYPART(S))                                
PRGIND   DS    X                   PROGRAM INDICATOR                            
APRG     DS    AL3                 A(PROGRAM(S))                                
DAYIND   DS    X                   DAY INDICATOR                                
ADAY     DS    AL3                 A(DAY(S))                                    
TIMIND   DS    X                   TIME INDICATOR                               
ATIM     DS    AL3                 A(START/END TIMES)                           
SUBIND   DS    X                   SUBLINE INDICATOR                            
ASUB     DS    AL3                 A(SUBLINE RANGE)                             
PKNIND   DS    X                   PACKAGE INDICATOR                            
APKN     DS    AL3                 A(PACKAGE LIST/RANGE)                        
LENIND   DS    X                   LENGTH INDICATOR                             
ALEN     DS    AL3                 A(LENGTH LIST/RANGE)                         
SMCIND   DS    X                   LENGTH INDICATOR                             
ASMC     DS    AL3                 A(SYSTEM/MEDIA(/CLIENT) LIST)                
DEMIND   DS    X                   DEMOS INDICATOR                              
ADEM     DS    AL3                 A(DEMOS LIST)                                
                                                                                
STRDMON  DS    XL2                 MONDAY ADJUSTED START DATE                   
STRDATE  DS    XL2                 REQUEST START DATE                           
ENDDATE  DS    XL2                 REQUEST END DATE                             
DTETIM   DS    XL4                 FILTER DATE/TIME STAMP                       
                                                                                
RECORD   DS    C                   ** RECORD TYPE FILTER **                     
RECUNIT  EQU   C'U'                UNITS ONLY                                   
RECGOAL  EQU   C'G'                GOALS ONLY                                   
RECBOTH  EQU   C'B'                UNITS AND GOALS                              
RECDFLT  EQU   0                   DEFAULT (UNITS ONLY)                         
                                                                                
BYTE     DS    XL1                                                              
BYTE2    DS    XL1                                                              
                                                                                
RFLAVOR  DS    C                   ** RATING FLAVOR **                          
RFLAGUA  EQU   0                   ESTIMATES W/ GUARANTEE                       
RFLANOG  EQU   C'N'                ESTIMATES W/ NO GUARANTEES                   
RFLAACT  EQU   C'A'                ACTUALS W/ GUARANTEES                        
                                                                                
ARFLVOR2 DS    F                                                                
RFLAVOR2 DS    CL12                ** RATING FLAVOR **                          
*                                  BYTE1 = TYPE                                 
*                                  BYTE2 = Y/N APPLY GUARANTEE                  
RFL2EST  EQU   C'E'                ESTIMATE                                     
RFL2ACT  EQU   C'A'                ACTUALS                                      
RFL2EAC  EQU   C'Z'                ESTIMATE & ACTUALS                           
                                                                                
TMPUSER3 DS    CL16                                                             
                                                                                
ERTG     DS    C                   ** EQUIV. RATINGS OPTION **                  
ERTGEQV  EQU   C'E'                EQUIVALENCE                                  
ERTGRAW  EQU   C'R'                RAW VALUE                                    
ERTGPRF  EQU   0                   USE PROFILE VALUE                            
                                                                                
EIMP     DS    C                   ** EQUIV. IMPRESSIONS OPTION **              
EIMPEQV  EQU   C'E'                EQUIVALENCE                                  
EIMPRAW  EQU   C'R'                RAW VALUE                                    
EIMPPRF  EQU   0                   USE PROFILE VALUE                            
                                                                                
OTHERS   DS    X                   ** OTHER OPTIONAL FIELDS **                  
OTHSERQ  EQU   X'01'               SERIAL NUMBER                                
OTHDEALQ EQU   X'02'               DEAL NUMBER                                  
OTHCONQ  EQU   X'04'               CONTRACT NUMBER                              
                                                                                
ADVIND   DS    XL4                 ADVERTISER NODE                              
SUPNODE  DS    XL(L'CFMVENN)       SUPPLIER NODE                                
CALOPTN  DS    C                   CALENDAR OPTION                              
SPLOPTN  DS    C                   SPLIT OPTION                                 
SUMOPTN  DS    C                   SUMMARIZE BY... OPTION                       
PNTOPTN  DS    C                   PROGRAM NAME/TIME OPTION                     
ESTOPTN  DS    C                   ESTIMATE OPTION                              
ESNOPTN  DS    C                   ESTIMATE NAME OPTION                         
STRDATEE DS    CL6                 START DATE (EBCDIC)                          
ENDDATEE DS    CL6                 END DATE (EBCDIC)                            
REQUNIV  DS    CL1                 REQUEST UNIVERSES? (Y/N/BLANK = N)           
REQVALL  EQU   *-REQVALS                                                        
                                                                                
MEDFILTS DS    (MEDTABN)CL(L'SMDTMED)                                           
                                                                                
MKTNUM   DS    AL2                 N'MARKETS IN MARKET LIST                     
MKTLST   DS    10AL(L'MEDTGMKT)    GOAL MARKET LIST                             
                                                                                
PRDINDA  DS    XL1                                                              
APRDA    DS    AL3                                                              
                                                                                
UNIVCOD  DS    XL2                 UNIVERSE CUT OFF DATE                        
COMLIC   DS    CL32                COMSCORE LICENSE                             
                                                                                
         DS    0F                                                               
UNIVBLK  DS    XL184               UNIVERSE BLOCK                               
         ORG   UNIVBLK                                                          
ESTUNIVH DS    F                   ESTIMATED - FIRST 4 BYTES IS HOMES           
ESTUNIVV DS    20F                 HOMES + REST OF REQUESTED DEMOS              
ACTUNIVH DS    F                   ACTUALS - FIRST 4 BYTES IS HOMES             
ACTUNIVV DS    20F                 HOMES + REST OF REQUESTED DEMOS              
         DS    CL16                SPARE                                        
*                                                                               
* DEMO OVERRIDE INDICATORS - 3 BYTES FOR EACH DEMOS (VPH,GRP,IMP)               
*                                                                               
DOIND1HG DS    CL1                 HOMES DEMO OVERRIDE - GRP                    
DOIND1HI DS    CL1                 HOMES DEMO OVERRIDE - IMP                    
DOIND1HS DS    CL1                 HOMES DEMO OVERRIDE - SHARE                  
DOIND1HH DS    CL1                 HOMES DEMO OVERRIDE - HUT                    
DOIND2HG DS    CL1                 HOMES DEMO OVERRIDE - GRP                    
DOIND2HI DS    CL1                 HOMES DEMO OVERRIDE - IMP                    
DOIND2HS DS    CL1                 HOMES DEMO OVERRIDE - SHARE                  
DOIND2HH DS    CL1                 HOMES DEMO OVERRIDE - HUT                    
DOINDHQ  EQU   *-DOIND1HG                                                       
*                                                                               
* DEMO OVERRIDE INDICATORS - 3 BYTES FOR EACH DEMOS (VPH,GRP,IMP)               
*                                                                               
DOVIND1  DS    CL75                DEMOS OVERRIDE INDICATOR                     
DOVIND2  DS    CL75                DEMOS OVERRIDE INDICATOR                     
                                                                                
NETIOD   DS    0D                                                               
                                                                                
       ++INCLUDE NETBLOCKD                                                      
NBWNETIO EQU   C'I'                CALLED FROM NETIO                            
NBFILCTL EQU   C'C'                CONTROL FILE                                 
NBFILGEN EQU   C'G'                GENFILE                                      
NBFILSPT EQU   C'S'                SPOT FILE(S)                                 
NBFILSTA EQU   C'A'                STATION FILE                                 
NBFILUNT EQU   C'U'                UNIT FILE(S)                                 
NBFILXSP EQU   C'X'                XSPOT FILE(S)                                
NBESTOUQ EQU   C'M'                RETURN EST DEMOS UNCONDITIONALLY             
NBVARUDQ EQU   X'40'               FOR NAD USE NDDEMOS                          
NBSELTTQ EQU   C'T'                TRAFFIC ONLY                                 
NBSELTBQ EQU   C'B'                BOTH                                         
         PRINT ON                                                               
                                                                                
*NETDEMODT                                                                      
       ++INCLUDE NETDEMOT                                                       
                                                                                
         ORG   NDESTDEM                                                         
NDESTHOM DS    XL8                                                              
NDESTOTH DS    0X                                                               
         ORG   NDACTDEM                                                         
NDACTHOM DS    XL8                                                              
NDACTOTH DS    0X                                                               
         ORG                                                                    
         PRINT ON                                                               
                                                                                
*DEDBLOCK                                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
                                                                                
         ORG   DBLOCK                                                           
BILBLOCK DS    XL(NBLLENQ)         NBABILRD AREA                                
         ORG                                                                    
                                                                                
ESTTAB   DS    (MAXESTS)XL(L'EUSRNMS)                                           
ESTTABL  EQU   *-ESTTAB                                                         
                                                                                
IO1      DS    XL(MAXRECLN)        AGENCY RECORD                                
IO2      DS    XL(MAXCLTLN)        CLIENT RECORD                                
IO3      DS    XL(MAXRECLN)        ESTIMATE RECORD                              
*IO4      DS    XL(MAXUNTLN)        UNIT RECORD                                 
                                                                                
                                                                                
NETBLKL  EQU   *-NETBLOCK                                                       
SAVECLRL EQU   *-SAVECLR                                                        
SAVELEFT DS    XL((18*1024)-(*-SAVED))                                          
         EJECT                                                                  
WORKD    DSECT                     ** SERVER LOCAL W/S **                       
DUB      DS    D                                                                
DUB1     DS    D                                                                
DMCB     DS    6F                                                               
                                                                                
PARA     DS    6F                                                               
WORK     DS    XL64                                                             
WORK2    DS    XL64                                                             
HEXWORK  DS    XL220                                                            
P        DS    CL132               PRINT LINE (FOR TRACE)                       
FULL     DS    F                                                                
                                                                                
DMCMND   DS    CL8                                                              
DMDIR    DS    CL8                                                              
DMFIL    DS    CL8                                                              
DMFILE   DS    CL8                                                              
DMKEYLEN DS    X                                                                
DMDA     DS    XL4                                                              
DMWK     DS    XL48                                                             
                                                                                
FLDH     DS    XL8                 DUMMY TWA FIELD HEADER                       
FLD      DS    CL80                DUMMY TWA FIELD                              
                                                                                
PRDNUM   DS    XL(L'NUPRD)                                                      
PRDALPH  DS    CL(L'NBSELPRD)                                                   
                                                                                
SAVERE2  DS    A                                                                
REQCLT   DS    XL(L'NUKCLT)                                                     
                                                                                
OFFCWORK DS    (OFCLENQ)X          OFFICER CONTROL BLOCK                        
OFFCSAVE DS    XL32                OFFICER SAVE AREA                            
                                                                                
TMPDVALS DS    0X                                                               
TMPSHR   DS    XL2                                                              
TMPHUT   DS    XL2                                                              
TMPDVALQ EQU   *-TMPDVALS                                                       
                                                                                
CDEMINDX DS    XL1                 COMSCORE DEMO INDEX                          
                                                                                
IOAREA   DS    XL2000                                                           
                                                                                
IO4      DS    XL(MAXUNTLN)        UNIT RECORD                                  
NBXBLK   DS    XL1600              NETBLKXTND BLOCK                             
                                                                                
WORKL    EQU   *-WORKD                                                          
                                                                                
GWKTABD  DSECT                     ** GOAL WEEKLY VALUES **                     
GWKREP#  DS    X                   GOAL REPLICATION FACTOR                      
GWKSTR   DS    CL6                 GOAL WEEK START DATE                         
GWKGRPS  DS    0PL6                ** GRPS **                                   
GWKGRP1  DS    PL6                 GOAL WEEK POINTS 1                           
GWKGRP2  DS    PL6                 GOAL WEEK POINTS 2                           
GWKGRP3  DS    PL6                 GOAL WEEK POINTS 3                           
GWKGRPL  EQU   *-GWKGRPS                                                        
GWKDOL   DS    PL6                 GOAL WEEK DOLLARS                            
GWKINDS  DS    X                   ** INDICATORS **                             
GWKISEQ  EQU   X'80'               GOAL WEEK START DATE IN SEQUENCE             
GWKIGRP  EQU   X'40'               GOAL GRPS SAME AS PREVIOUS                   
GWKIDOL  EQU   X'20'               GOAL DOLLARS SAME AS PREVIOUS                
GWKTABL  EQU   *-GWKTABD                                                        
         EJECT                                                                  
* OTHER INCLUDED DSECTS                                                         
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDLINKD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDRUNNERD                                                      
       ++INCLUDE DDWRKIOD                                                       
       ++INCLUDE FATABSRUN                                                      
       ++INCLUDE NEDDEQUS                                                       
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEGENUNIT                                                      
NUKSTRAQ EQU   X'C1'               FIRST TRAFFIC SUB-LINE NUMBER                
                                                                                
NUCOMELQ EQU   X'04'                                                            
NUCMLEQ  EQU   X'21'                                                            
NUPCELQ  EQU   X'24'                                                            
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
NUOTPODQ EQU   C'Y'                POD                                          
NUOTFLTQ EQU   C'1'                OVERRIDE FLIGHT ID                           
*                                  ** COMMENT TYPE (NUCOMTYP) **                
NUCOMTCQ EQU   C'C'                CLIENT                                       
NUCOMTIQ EQU   C'I'                INTERNAL                                     
*                                                                               
         PRINT ON                                                               
         PRINT GEN                                                              
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE NETBILLRD                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
         ORG   CKEYCLT+L'CKEYCLT                                                
CKEYREST DS    XL(L'CKEY-(*-CKEY))                                              
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENPRD                                                       
         ORG   PKEYPRD+L'PKEYPRD                                                
PKEYREST DS    XL(L'PKEY-(*-PKEY))                                              
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSLST                                                      
       ++INCLUDE GECFMIOD                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE NETBLKXTND                                                     
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DEDEMOVALD                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE GEGENTOK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044NENAV13   11/20/19'                                      
         END                                                                    
