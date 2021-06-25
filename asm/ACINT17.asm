*          DATA SET ACINT17    AT LEVEL 012 AS OF 02/12/04                      
*PHASE T61917A,*                                                                
*INCLUDE CONVMOS                                                                
*INCLUDE COVAIL                                                                 
*INCLUDE DLFLD                                                                  
         TITLE 'T61917 - INTERAGENCY TRACKING REPORT'                           
T61917   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61917,RA,RR=R2                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         ST    R2,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
*                                                                               
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
*                                                                               
         L     RE,RELO                                                          
         LA    R0,NRELOS           R0=LOOP COUNTER                              
         LA    R1,RELOTAB          R1=A(ADCONS TO RELOCATE)                     
         LA    R2,VTYPES           R2=A(OUTPUT)                                 
*                                                                               
MODE4    L     RF,0(R1)            GET ADCON                                    
         AR    RF,RE               RELOCATE IT                                  
         ST    RF,0(R2)                                                         
         LA    R1,L'RELOTAB(R1)                                                 
         LA    R2,4(R2)                                                         
         BCT   R0,MODE4                                                         
*                                                                               
         BAS   RE,PEST             READ ESTIMATE RECORDS                        
         BAS   RE,PDET             READ TRANSACTIONS                            
         BAS   RE,REPORT           DO REPORT                                    
         TM    QREPOPTS,DOWNLOAD   DOWNLOADING?                                 
         BZ    MODE6               NO                                           
         OC    ALSORT,ALSORT       ANY SORT RECORDS                             
         BZ    MODE6               NO                                           
         MVC   DWNFLD,SPACES       YES, DOWNLOAD EOR MARKER                     
         GOTO1 ADWNL,DMCB,(RC),DWNEOR                                           
*                                                                               
MODE6    BAS   RE,WRAP             WRAP-UP AFTER REPORT                         
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE KEY FIELDS                                            
*                                                                               
VKEY     NTR1                                                                   
*                                  CLEAR CHARS FIELDS TO SPACES                 
         LA    RE,LOCAL            RECEIVING FIELD                              
         LA    RF,LOCALCLN         RECEIVING FIELD LENGTH                       
         SR    R1,R1               BITS 0-7 PAD FACTOR,8-32 LENGTH SEND         
         ICM   R1,8,=X'40'         INSERT 40 AS PADDING FACTOR                  
         MVCL  RE,R0                                                            
*                                                                               
*                                  VALIDATE RECEIVABLE ACCOUNT                  
         MVI   OPTION,YES          PASS NAME(FOR REC,CLI,PROD)                  
         MVC   CUL+1(L'RECVLEDG),RECVLEDG                                       
         MVC   QACCOUNT(L'CUL),CUL                                              
         LA    R2,REPRCVH                                                       
         GOTO1 SETHEIR                                                          
         MVC   SRLEVELA,LLEVA                                                   
         MVC   SRLEVELB,LLEVAB                                                  
         MVC   SRLEVELC,LLEVABC                                                 
         MVC   SRLEVELD,LLEVABCD                                                
*                                                                               
         CLI   REPRCVH+5,0         WAS ONE INPUT?                               
         BNE   *+12                YES - GO CHECK IT                            
         TM    WHEN,SOON           RUNNING SOON                                 
         BZ    VKEY01              NO  - THEN INPUT IS OPTIONAL                 
*                                                                               
*                                  YES - THENINPUT IS REQUIRED                  
         MVI   OPTION2,YES         ALLOW INPUT OF HIGH LEVEL ACCOUNT            
         LA    R2,REPRCVH                                                       
         GOTO1 VALACCT                                                          
         MVC   QACCOUNT+L'CUL(L'QACCOUNT-L'CUL),RECCODE                         
*                                                                               
*                                  VALIDATE SJ CLIENT ACCOUNT                   
*                                                                               
VKEY01   CLI   REPCLTH+5,0         WAS ONE INPUT?                               
         BE    VKEY02              NO - SKIP EDIT                               
         LA    R2,REPCLTH                                                       
         MVI   ERROR,INVALID                                                    
         CLI   REPCLTH+5,1         CLIENT MUST BE AT LEAST 2 CHARS              
         BNH   ERREND                                                           
         CLI   8(R2),C' '          DOES CLIENT START WITH A BLANK ?             
         BE    ERREND              YES, THIS IS AN ERROR                        
*                                                                               
         MVC   CUL+1(L'PRODLEDG),PRODLEDG                                       
         GOTO1 VALCLI              VALIDATE CLIENT                              
         MVC   QCLI,CLICODE        SAVE CLIENT CODE                             
*                                                                               
*                                  VALIDATE SJ CLIENT PRODUCT ACCOUNT           
*                                                                               
VKEY02   CLI   REPPRDH+5,0         WAS ONE INPUT?                               
         BE    VKEY03              NO - SKIP EDIT                               
         MVI   ERROR,MISSING                                                    
         CLI   REPCLTH+5,0         YES - THEN A CLIENT MUST ALSO BE             
         BNE   *+12                                                             
         LA    R2,REPCLTH                                                       
         B     ERREND                                                           
         LA    R2,REPPRDH          PRODUCT                                      
         MVI   ERROR,INVALID                                                    
         CLI   REPPRDH+5,1         MUST BE AT LEAST 2 CHARS                     
         BNH   ERREND                                                           
         CLI   8(R2),C' '          DOES PRODUCT START WITH A BLANK ?            
         BE    ERREND              YES, THIS IS AN ERROR                        
*                                                                               
         GOTO1 VALPROD             VALIDATE PRODUCT                             
         MVC   QPROD,PRODCODE      SAVE PRODUCT CODE                            
         MVC   QOFFICE,CLIOFFC     SAVE CLI/PRD OFFICE CODE                     
*                                                                               
*                                  VALIDATE MEDIA RECORD/INCOME ACCT            
*                                                                               
VKEY03   CLI   REPMEDH+5,0         WAS ONE INPUT?                               
         BE    VKEY04              NO - SKIP EDIT                               
         LA    R2,REPMEDH          MEDIA                                        
         GOTO1 VALMED              VALIDATE MEDIA                               
         CLC   =C'MI=',REPMED      MI=?                                         
         BNE   *+10                                                             
         MVC   QMEDMI,REPMED                                                    
         MVC   QMEDCOD,MEDIA       SAVE THE TWO DIGIT MEDIA                     
         MVC   QMEDIA,MEDIACOD     SAVE INCOME ACCOUNT                          
         LA    R2,REPMEDNH         MEDIA DESCRIPTION FIELD                      
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         MVC   REPMEDN,MEDNAME     SAVE INCOME ACCOUNT                          
*                                                                               
*                                  VALIDATE ESTIMATE NUMBER                     
*                                                                               
VKEY04   CLI   REPESTH+5,0         WAS ONE INPUT?                               
         BE    VKEY05              NO - SKIP EDIT                               
         LA    R2,REPESTH          ESTIMATE NUMBER                              
         GOTO1 VALEST                                                           
         MVC   QESTIM,ESTIMATE     SAVE ESTIMATE NUMBER                         
*                                                                               
*                                  * ESTIMATE PERIOD NOT REQUIRED IF *          
*                                  *   AN ESTIMATE NUMBER WAS INPUT  *          
*                                  * EXCEPT IF REQUESTING BY G/L MTH *          
*                                                                               
         CLI   REPPERH+5,0         WAS ESTIMATE PERIOD INPUT?                   
         BNE   VKEY05              YES- GO VALIDATE IT                          
         CLI   REPOP7H+5,0         REQUESTED FOR G/L MONTHS?                    
         BNE   VKEY05              YES- THEN EST PERIOD IS REQUIRED             
         XC    QESTART,QESTART     SET START TO LOWEST VALUE                    
         MVC   QEEND,FOXES         SET END TO HIGHEST VALUE                     
         B     VKEY07                                                           
*                                                                               
*                                  VALIDATE ESTIMATE PERIOD                     
*                                                                               
VKEY05   LA    R2,REPPERH          ESTIMATE NUMBER                              
         GOTO1 VALPERI                                                          
         MVC   QESTART,PERIODS     SAVE START AND END OF EST PERIOD             
         MVC   QEEND,PERIODE                                                    
*                                                                               
*                                  VALIDATE REPORT TYPE OPTIONS                 
VKEY07   XC    QREPOPTS,QREPOPTS                                                
         CLI   REPOP1H+5,0         REPORT BY A.O.R. OPTION INPUT?               
         BE    *+8                 NO - DON'T SET THE BIT                       
         OI    QREPOPTS,BYAOR                                                   
         CLI   REPOP2H+5,0         REPORT BY CLT/PRD OPTION INPUT?              
         BE    *+8                 NO - DON'T SET THE BIT                       
         OI    QREPOPTS,BYCLIENT                                                
         CLI   REPOP3H+5,0         REPORT BY MEDIA OPTION INPUT?                
         BE    *+8                 NO - DON'T SET THE BIT                       
         OI    QREPOPTS,BYMEDIA                                                 
         CLI   REPOP4H+5,0         REPORT BY ESTIMATE SUMMARY?                  
         BE    *+8                 NO - DON'T SET THE BIT                       
         OI    QREPOPTS,ESTSUMRE                                                
         OC    QREPOPTS,QREPOPTS                                                
         BNZ   *+8                                                              
         OI    QREPOPTS,BYAOR      IF NO OPTIONS SET BY A.O.R                   
*                                                                               
*                                  VALIDATE REPORT OPTIONS                      
         XC    QOPTIONS,QOPTIONS   REPORT OPTIONS                               
         OI    QOPTIONS,ADPERIOD   DEFAULT IS REQUEST IS BY ADV MTHS            
         CLI   REPOP7H+5,0         REQUEST IS BY G/L MONTH?                     
         BE    VKEY08              NO - DON'T SET THE BIT                       
*                                                                               
         OI    QOPTIONS,GLPERIOD   SET G/L MTH REQUESTED BIT                    
         CLI   REPOP4H+5,0         ESTIMATE SUMMARY REQUESTED?                  
         BE    VKEY08              NO - YOUR DONE                               
         LA    R2,REPOP4H          SET CURSOR POSITION TO EST SUM OPT           
         MVI   ERROR,X'FE'         I SET THE ERROR MESSAGE                      
         MVC   CONHEAD(L'BADREQST),BADREQST                                     
         B     ERREND                                                           
*                                                                               
VKEY08   CLI   REPOP8H+5,0         DOWNLOAD REQUESTED?                          
         BE    VKEYX               NO                                           
         CLI   QREPOPTS,X'80'                                                   
         BE    VKEY10                                                           
         CLI   QREPOPTS,X'40'                                                   
         BE    VKEY10                                                           
         CLI   QREPOPTS,X'20'                                                   
         BE    VKEY10                                                           
         CLI   QREPOPTS,X'10'                                                   
         BE    VKEY10                                                           
         MVI   ERROR,X'FE'         SET THE ERROR MESSAGE                        
         MVC   CONHEAD(L'MIXDOWN),MIXDOWN                                       
         B     ERREND                                                           
*                                                                               
VKEY10   OI    QREPOPTS,DOWNLOAD   ONLY ONE REPORT AT A TIME                    
         B     VKEYX                                                            
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              PUT ESTIMATES RECORDS TO SORT                        *           
*********************************************************************           
*                                                                               
PEST     NTR1                                                                   
         BAS   RE,INITREP              INITIALIZE OUR BUFFER                    
         OC    QESTART,QESTART         IS THERE A START DATE                    
         BNZ   PEST02                  NO-CONTINUE                              
         MVC   PERIODS,FOXES                                                    
         XC    PERIODE,PERIODE                                                  
*                                                                               
PEST02   MVC   KKEY,SPACES             SET KEY FOR X'2D03' EST RECS             
         LA    R6,KKEY                                                          
         USING ACINKEY,R6                                                       
         MVI   ACINCOD,ACINEQU                                                  
         MVI   ACINSREC,ACINSEQU                                                
         MVC   ACINCOMP(L'COMPANY),COMPANY                                      
         MVC   ACINUL,RECVLEDG                                                  
*                                                                               
         CLC   QACCOUNT,SPACES         IS THERE AN ACCOUNT                      
         BE    PEST04                  NO - GO READ HIGH                        
*                                                                               
*                                               FIGURE OUT THE LENGTH           
*                                               OF ACCOUNT INPUT                
*                                                                               
         LA    R1,QACCOUNT+(L'QACCOUNT-1)       R1 POINT TO LAST BYTE           
         LA    R0,(L'QACCOUNT-1)                R0 COUNT CHARS (-1)             
         CLI   0(R1),C' '                       SIGNIFICANT CHAR?               
         BNE   *+12                             IF YES R0 HAS LEN OF AC         
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         DC    H'0'                                                             
         STC   R0,LEN                           SAVE LEN INPUT ACCOUNT          
*                                                                               
*                                               FILL IN REST OF KEY             
*                                               BASED ON REQUEST DETAIL         
*                                                                               
         MVC   ACINCUL(L'QACCOUNT),QACCOUNT                                     
         CLC   QCLIPROD,SPACES                  CLT OR CLT/PROD INPUT?          
         BE    PEST04                           NO-INITIAL KEY FINISHED         
         MVC   ACINCLT(L'QCLIPROD),QCLIPROD                                     
         CLC   QMEDMIE,SPACES                   MEDIA INPUT?                    
         BE    PEST04                           NO-INITIAL KEY FINISHED         
*                                                                               
         MVC   ACINMED,QMEDCOD                                                  
         MVC   ACINEST,QESTIM                   MOVE EST# OR BLANKS             
*                                                                               
PEST04   DS    0H                                                               
         BAS   RE,RDHIGH                                                        
         B     PEST08                                                           
*                                                                               
PEST06   BAS   RE,RDSEQL                                                        
*                                                                               
PEST08   CLC   SAVKKEY(L'ACINCOD+L'ACINSREC+L'ACINCUL),KKEY                     
         BNE   PEST28                                                           
         L     R6,AIO                                                           
         CLC   QREQUEST,SPACES      WERE THERE KEY FILTERS?                     
         BE    PEST16               NO- GO TEST FOR EST PERIOD                  
*                                                                               
         CLC   QACCOUNT,SPACES      IS ACCOUNT REQUESTED?                       
         BE    PEST10               NO-CHECK FOR CLIENT FILTERING               
         ZIC   R1,LEN                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACINCUL(0),QACCOUNT  DO WE WANT THIS ACCOUNT?                    
         BNE   PEST28               NO - WERE DONE WITH ESTIMATES               
*                                                                               
PEST10   CLC   QCLIPROD,SPACES      FILTERING BY CLIENT/PRODUCT?                
         BE    PEST12               NO - GO CHECK FOR MEDIA                     
         CLC   ACINCLT,QCLI         CLIENT MATCH?                               
         BNE   PEST06               NO - READ NEXT RECORD                       
         CLC   QPROD,SPACES         FILTERING BY PRODUCT?                       
         BE    PEST12               NO - GO CHECK FOR MEDIA                     
         CLC   ACINPRD,QPROD        PRODUCT MATCH?                              
         BNE   PEST06               NO - READ NEXT RECORD                       
*                                                                               
PEST12   CLC   QMEDCOD,SPACES       FILTERING BY MEDIA?                         
         BE    PEST14               NO - GO CHECK EST NUMBER                    
         CLC   ACINMED,QMEDCOD                                                  
         BNE   PEST06               NO - READ NEXT RECORD                       
*                                                                               
PEST14   CLC   QESTIM,SPACES        FILTERING BY ESTIMATE NUMBER?               
         BE    PEST16               NO - GO CHECK EST DATES                     
         CLC   ACINEST,QESTIM                                                   
         BNE   PEST06               NO - READ NEXT RECORD                       
*                                                                               
PEST16   L     R5,ASORTREC          R5 COVERS SORT RECORD                       
         L     R4,AIO               SAVE ADDR OF ESTIMATE RECORD                
         USING SRTD,R5                                                          
         BAS   RE,CLEARSRT                                                      
         MVI   SRTBUFTP,ESTORDER                                                
         MVC   SRTRCUL,ACINCUL      C/U/L                                       
         MVC   SRTACCT,ACINACC      AOR AGENCY                                  
*        MVC   SRTOFFIC,SPACES                                                  
         MVC   SRTCLT,ACINCLT       CLIENT                                      
         MVC   SRTPRD,ACINPRD       PRODUCT                                     
         MVC   SRTMDUL,=C'SI'       DEFAULT IS INCOME ACCOUNT                   
         MVC   SRTMACC(L'ACINMED),ACINMED               MEDIA                   
         TM    ACSTATUS-ACKEYD(R4),X'02'                                        
         BZ    *+10                                                             
         MVC   SRTMDUL,=C'**'       TAKEN FROM MEDIA RECORD                     
         MVC   SRTESTIM,ACINEST     ESTIMATE NUMBER                             
         MVI   SRTTYPE,SRTEST                                                   
*                                                                               
         MVI   ELCODE,ACIPFEQU                                                  
         BAS   RE,GETELIO           GET PROFILE ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACINPRFD,R6                                                      
         TM    QOPTIONS,GLPERIOD   IF REQUESTED BY G/L MTHS SKIP                
         BNZ   PEST18              THIS TEST                                    
         CLC   ACIPFPRE,QESTART    EST PERIOD END LOWER THAN REQ START?         
         BL    PEST06              YES - THEN GET NEXT RECORD                   
         CLC   ACIPFPRS,QEEND      EST START HIGHER THAN REQ END?               
         BH    PEST06              YES - THEN GET NEXT RECORD                   
*                                                                               
*                                  ** IF START DATE NOT INPUT TAKE **           
*                                  ** IT FROM THE OLDEST ESTIMATE  **           
*                                  ** THAT FITS THE REQUEST DETAILS**           
*                                  ** THIS IS FOR HEADLINES ONLY   **           
*                                                                               
         OC    QESTART,QESTART     IS START DATE SET TO LOW VALUES              
         BNZ   PEST18              NO - CONTINUE                                
         CLC   PERIODS,ACIPFPRS    COMPARE THIS ESTIMATES START TO OURS         
         BL    *+10                TAKE THE LOWER OF THE TWO                    
         MVC   PERIODS,ACIPFPRS                                                 
         CLC   PERIODE,ACIPFPRE    TAKE THE HIGEST END DATE FOR HEADS           
         BH    PEST18                                                           
         MVC   PERIODE,ACIPFPRE                                                 
*                                                                               
PEST18   MVC   SRTDESC,ACIPFDES    ESTIMATE DESCRIPTION                         
         MVC   SRTPRS,ACIPFPRS     YYMM START EST PERIOD                        
         MVC   SRTPRE,ACIPFPRE     YYMM END EST PERIOD                          
*                                                                               
         MVI   ELCODE,ACIESEQU                                                  
         BAS   RE,GETELIO          GET PROFILE ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACINESTD,R6                                                      
PEST20   MVC   SRTADMTH,ACIESMTH   YYMM ADVERTISING MONTH                       
         TM    QOPTIONS,GLPERIOD   IF G/L MTHS SKIP TEST                        
         BO    PEST22                                                           
         CLC   SRTADMTH,QESTART    CHECK IF IN RANGE                            
         BL    PEST26              NO - GET NEXT X'C7'                          
         CLC   SRTADMTH,QEEND                                                   
         BH    PEST26                                                           
*                                                                               
PEST22   DS    0H                  YYMMDD POSTING DATE                          
         GOTO1 DATCON,DMCB,(2,ACIESDTO),(1,SRTDATE)                             
         MVC   SRTRDATE,SRTDATE                                                 
         MVC   SRTGLMTH,SRTDATE    G/L MONTH (ACCOUNTING MOS)                   
         ZAP   SRTAPCT,ACIESFEE    AOR FEE PCT                                  
         ZAP   SRTRPCT,ACIESRCV    CREATIVE FEE PCT                             
         ZAP   SRTGROSS,ACIESGRS   GROSS BILLING EST                            
         ZAP   SRTRCV,ACIESREC     RECEIVABLE AMOUNT EST                        
         ZAP   SRTPOST,=P'0'       CLEAR POSTED                                 
         TM    ACIESTAT,X'80'      ESTIMATE POSTED?                             
         BZ    *+10                NO SKIP                                      
         ZAP   SRTPOST,ACIESREC    POSTED                                       
         ZAP   SRTPAID,ACIESPD     PAID SO FAR                                  
*                                                                               
         BAS   RE,ACCADD           ADD ACCOUNT/LEVELS TO TABLE                  
         BAS   RE,CLIADD           ADD CLIENT/PRODUCT TO TABLE                  
         BAS   RE,MEDADD           ADD MEDIA/NAMES TO TABLE                     
         BAS   RE,ESTADD           ADD DATE TO ESTIMATE TABLE                   
*                                                                               
         TM    QREPOPTS,ESTSUMRE   ESTIMATE SUMMARY REP WANTED?                 
         BNO   PEST24              NO - SKIP PUT TO BUFFALO                     
         L     R1,ASORTSAV                                                      
         MVC   0(SRTLNQ,R1),0(R5)  COPY SORT REC TO SAVED AREA                  
         L     R5,ASORTSAV                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFA,(R5)                                 
         MVC   SRTRDATE(L'SRTRDATE+L'SRTCK+L'SRTSUBRF),FOXES                    
         MVC   SRTADMTH,FOXES                                                   
         BASR  RE,RF                EST TOTAL                                   
         MVC   SRTESTIM,FOXES                                                   
         BASR  RE,RF                MEDIA TOTAL                                 
         MVC   SRTMED,FOXES                                                     
         BASR  RE,RF                PROD TOTAL                                  
         MVC   SRTPRD,FOXES                                                     
         BASR  RE,RF                CLT TOTAL                                   
         MVC   SRTCLT,FOXES                                                     
         BASR  RE,RF                RECV TOTAL                                  
         MVC   SRTRECV,FOXES                                                    
         BASR  RE,RF                REPT TOTAL                                  
         L     R5,ASORTREC                                                      
*                                                                               
PEST24   MVI   ALSORT,1                  ACTIVITY SWITCH                        
*                                                                               
PEST26   ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),ACIESEQU                                                   
         BE    PEST20                    GET NEXT X'C7' EL                      
         B     PEST06                    GET NEXT RECORD                        
*                                                                               
*                                        *LOOKUP CLIENT/PRODUCT NAME*           
PEST28   L     R5,ACLTLIST               ADDR OF CLIENT TABLE                   
         USING BIND,R5                                                          
         OC    BININ,BININ               IS TABLE EMPTY?                        
         BZ    PEST34                                                           
         MVC   AIO,AIO2                  USE AIO2                               
         LA    R2,BINTABLE               ADDR TABLE IN R2                       
         L     R4,BININ                  NUMBER IN TABLE IN R4                  
*                                                                               
         USING CLTD,R2                                                          
PEST30   MVC   KEY,SPACES                                                       
         MVC   KEY(L'CLTKEY),CLTKEY                                             
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(42),KEY                                                  
         BE    *+14                                                             
         MVC   CLTNAME(23),=C'***ACCOUNT NOT FOUND***'                          
         B     PEST32                                                           
*                                                                               
         LA    R3,CLTNAME                NAMEIT PUT NAME IN ADDR IN R3          
         MVC   0(L'CLTNAME,R3),SPACES                                           
         GOTO1 NAMEIT                                                           
         MVI   ELCODE,ACPRELQ            GET PROFILE ELEMENT                    
         BAS   RE,GETELIO                FOR THE OFFICE CODE                    
         BNE   PEST32                                                           
         USING ACPROFD,R6                                                       
         MVC   CLTOFF,SPACES                                                    
         CLC   ACPROFFC,SPACES                                                  
         BNH   *+10                                                             
         MVC   CLTOFF,ACPROFFC                                                  
*                                                                               
PEST32   LA    R2,CLTLEN(R2)             NEXT TABLE ENTRY                       
         BCT   R4,PEST30                                                        
         BAS   RE,MEDLOOK                LOOKUP MEDIA NAMES                     
         MVC   AIO,AIO1                  RESET AIO                              
*                                                                               
PEST34   XC    WORK,WORK                 SETUP HEADLINE DATES                   
         MVC   WORK(L'PERIODS),PERIODS                                          
         MVC   WORK+3(L'PERIODE),PERIODE                                        
         GOTO1 DATCON,DMCB,(X'11',WORK),(6,PERRANGE)                            
         B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
*********************************************************************           
*              READ SR TRANS AND PUT INTERAGENCY TRANS TO SORT      *           
*********************************************************************           
*                                                                               
PDET     NTR1                                                                   
         MVC   QESTART,PERIODS                                                  
         MVC   QEEND,PERIODE                                                    
         L     R5,ACODLIST          TABLE OF SR ACCOUNTS TO READ                
         USING BIND,R5                                                          
         OC    BININ,BININ          ANY ACCOUNTS IN TABLE?                      
         BZ    PDET42               NO - NOTHING FOR REQUEST                    
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   WORK,SPACES          YES - MARK END OF TABLE                     
         MVC   WORK(L'CODKEY),FOXES                                             
         GOTO1 BINADD,DMCB,WORK,ACODLIST                                        
*                                                                               
         MVC   WORK,SPACES          MARK END OF ESTIMATE TABLE ALSO             
         MVC   WORK(L'ESTKEY),FOXES                                             
         GOTO1 BINADD,DMCB,WORK,AESTLIST                                        
         SPACE 3                                                                
*                                   ****READING ACCOUNTS FROM TABLE****         
*                                                                               
         LA    R1,BINTABLE          TABLE OFF OF R1                             
*                                                                               
PDET02   ST    R1,AMYPLACE           SAVE ADDR OF MY PLACE IN THE TABLE         
         MVC   SRACCONT,SPACES                                                  
         MVC   SRACCONT,0(R1)        SR ACCOUNT FROM TABLE FOR SR READ          
         CLI   SRACCONT,FOX                                                     
         BE    PDET42                GO DO REPORT                               
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'SRACCONT),SRACCONT                                         
         GOTO1 HIGH                                                             
         B     PDET06                                                           
*                                                                               
PDET04   GOTO1 SEQ                   READ NEXT REC FOR THIS SR ACCT             
*                                                                               
PDET06   L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         CLC   ACKEYACC,SRACCONT                                                
         BE    PDET08                                                           
         L     R1,AMYPLACE           ADDR OF MY PLACE IN THE TABLE              
         LA    R1,CODLEN(R1)         NEXT ACCOUNT IN THE TABLE                  
         B     PDET02                                                           
*                                                                               
PDET08   DS    0H                                                               
         BAS   RE,GETADDR            GET ADDRESSES OF NEEDED ELEMENTS           
         OC    ANAMEL,ANAMEL         DO WE HAVE A NAME ELEMENT                  
         BZ    PDET10                                                           
         L     R6,ANAMEL             NAME ELEMENT                               
         USING ACNAMED,R6                                                       
         L     R1,AMYPLACE                                                      
         ZIC   R2,ACNMLEN                                                       
         SH    R2,=H'3'                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   CODNAME-CODD(0,R1),ACNMNAME                                      
         B     PDET04                READ NEXT RECORD                           
*                                                                               
PDET10   CLI   ACRECORD,TRNSELQ      IS IT A TRANSACTION RECORD                 
         BNE   PDET04                NO - READ NEXT RECORD                      
*                                                                               
         USING TRANSD,R6                                                        
         L     R6,ATRANSEL           TRANS ELEMENT                              
         CLI   TRNSEL,TRNSELQ        MAKE SURE YOU HAVE ONE                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TRNSTYPE,58           INTERAGENCY DEBIT                          
         BE    PDET12                                                           
         CLI   TRNSTYPE,30           CASH APPLICATION                           
         BNE   PDET04                NO - READ NEXT RECORD                      
*                                                                               
PDET12   CLC   QESTIM,SPACES         REQUESTED FOR SPECIFIC EST NUMBER?         
         BE    *+14                  NO - CONTINUE                              
         CLC   TRNSREF,QESTIM        MATCH ON EST NUMBER?                       
         BNE   PDET04                NO - READ NEXT RECORD                      
         CLC   QOFFICE,SPACES        REQUESTED BY OFFICE?                       
         BE    *+14                  NO - CONTINUE                              
         CLC   QOFFICE,TRNSOFFC      MATCH ON OFFICE?                           
         BNE   PDET04                NO - READ NEXT RECORD                      
         MVC   SVEST,TRNSREF       SAVE THE REFERENCE/ESTIMATE                  
*                                                                               
         L     R6,AMEDIAEL           MEDIA TRANSFER ELEMENT                     
         USING ACMTD,R6                                                         
         CLI   ACMTEL,ACMTELQ        MAKE SURE YOU HAVE ONE                     
         BNE   PDET04                ****JUST GET NEXT FOR NOW*****             
         CLC   QCLIPROD,SPACES       FILTERING BY CLI/PROD?                     
         BE    PDET14                NO - GO CHECK FOR MEDIA                    
         CLC   ACMTCLI,QCLI          MATCH ON CLIENT                            
         BNE   PDET04                NO - READ NEXT RECORD                      
         CLC   QPROD,SPACES          FILTERING BY PRODUCT?                      
         BE    PDET14                NO - GO CHECK MEDIA                        
         CLC   ACMTPRD,QPROD         MATCH ON PRODUCT                           
         BNE   PDET04                NO - READ NEXT RECORD                      
*                                                                               
PDET14   CLC   QMEDCOD,SPACES        FILTERING BY MEDIA?                        
         BE    *+14                  NO - GO CHECK REQ PERIOD TYPE              
         CLC   ACMTMED2,QMEDCOD                                                 
         BNE   PDET04                NO - READ NEXT RECORD                      
         TM    QOPTIONS,GLPERIOD     IF REQUESTED BY G/L MTHS SKIP              
         BNZ   PDET16                THIS TEST                                  
         CLC   ACMTMOS,QESTART       ADV MTH LOWER THAN REQUEST START?          
         BL    PDET04                YES - GET NEXT RECORD                      
         CLC   ACMTMOS,QEEND         ADV MTH HIGHER THAN REQUEST END?           
         BH    PDET04                YES - GET NEXT RECORD                      
*                                                                               
PDET16   CLI   ACMTSYS,C'I'          SHOULD BE MARKED AS INTERAGENCY            
         BNE   PDET04                NOT - GET NEXT RECORD                      
*                                                                               
         L     R5,AESTLIST         MAKE SURE ESTIMATE IS VALID                  
         USING BIND,R5                                                          
         LA    R5,BINTABLE                                                      
         OC    BININ,BININ                                                      
         BZ    PDET04                                                           
*                                                                               
         USING ESTD,R5                                                          
PDET18   CLI   ESTCUL,X'FF'                                                     
         BE    PDET04              NO MATCH, SKIP IT                            
         CLC   SRACCONT,ESTCUL                                                  
         BNE   PDET20                                                           
         CLC   ACMTCLI,ESTCLT                                                   
         BNE   PDET20                                                           
         CLC   ACMTPRD,ESTPRD                                                   
         BNE   PDET20                                                           
         CLC   SVEST,ESTEST                                                     
         BE    PDET22                                                           
*                                                                               
PDET20   LA    R5,ESTLEN(R5)                                                    
         B     PDET18                                                           
*                                                                               
         USING SRTD,R5                                                          
         USING ACKEYD,R4                                                        
PDET22   L     R5,ASORTREC           R5 COVERS SORT RECORD                      
         L     R4,AIO                                                           
         BAS   RE,CLEARSRT                                                      
         MVI   SRTBUFTP,AORORDER                                                
         MVC   SRTRECV,ACKEYACC      C/U/L AOR AGENCY ACCOUNT (SR)              
         MVC   SRTSUBRF,ACKEYSBR     SAVE SUB REF TO PREVENT COMBO              
         MVC   SRTESTIM,ACKEYREF     ESTIMATE NUMBER                            
*                                                                               
         L     R6,ATRANSEL           TRANSACTION  ELEMENT                       
         USING TRANSD,R6                                                        
         MVC   SRTDATE,TRNSDATE      TRANSACTION DATE                           
*                                    G/L MONTH                                  
         GOTO1 =V(CONVMOS),DMCB,(X'FD',(R6)),SRTGLMTH                           
*                                                                               
         TM    QOPTIONS,GLPERIOD     REQUESTED BY G/L MTH?                      
         BZ    PDET24                NO - SKIP TEST                             
         CLC   SRTGLMTH,QESTART      CHECK IF IN RANGE                          
         BL    PDET04                NO - GET NEXT                              
         CLC   SRTGLMTH,QEEND                                                   
         BH    PDET04                                                           
*                                                                               
PDET24   CLI   TRNSTYPE,30                                                      
         BNE   PDET30                                                           
*                                    ***CASH APPLICATION***                     
*                                                                               
         ZAP   SRTPAID,TRNSAMNT      PAID AMOUNT                                
         MVC   SRTCASH,ACKEYCON      CONTRA IS CASH ACCOUNT                     
         MVI   SRTTYPE,SRTCHEK       MARK SORT REC AS CASH APPLI ITEM           
*                                                                               
*                                    LOOK FOR CHECK NUMBER,DATE AND             
*                                    DEPOSIT DATE IN NARRATIVE                  
         ZIC   R0,TRNSLEN            LENGTH OF TRANS EL                         
         LA    R1,TRNSLNQ            LENGTH OF TRANS EL W/O NARRA               
         SR    R0,R1                                                            
         STC   R0,NARRALEN           SAVE  THE LENGTH OF NARRATIVE              
*                                                                               
         USING NARRD,R2                                                         
         LA    R2,TRNSNARR           TRANSACTION NARRATIVE                      
         ZIC   R0,NARRALEN           LENGTH OF NARRATIVE                        
         CLC   NARRCK,=C'CHECK NUMBER'                                          
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-14                                                          
         B     PDET26                                                           
*                                                                               
*                                    CHECK NUMBER FOUND                         
         SR    R3,R3                 USE R3 TO COUNT                            
         LA    R0,L'NARCKNUM         MAX LENGTH OF CK NUMBER                    
         LA    R1,NARCKNUM           1ST BYTE                                   
         CLI   0(R1),C' '            IS IT A SPACE                              
         BE    *+16                  YOU FOUND THE END                          
         LA    R3,1(R3)              BUMP CK NUMBER DIGIT COUNT                 
         LA    R1,1(R1)              NEXT BYTE OF CHECK NUMBER                  
         BCT   R0,*-16                                                          
         LTR   R3,R3                                                            
         BZ    PDET26                                                           
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SRTCK(0),NARCKNUM     MOVE CHECK NUMBER TO SORT REC              
*                                                                               
PDET26   LA    R2,TRNSNARR           TRANSACTION NARRATIVE                      
         ZIC   R0,NARRALEN           LENGTH OF NARRATIVE                        
         CLC   NARDATED,=C'DATED'                                               
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-14                                                          
         B     PDET28                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,(0,NARCKDAT),WORK                                    
         GOTO1 DATCON,DMCB,(0,WORK),(1,SRTDATCK)                                
*                                                                               
PDET28   LA    R2,TRNSNARR           TRANSACTION NARRATIVE                      
         ZIC   R0,NARRALEN           LENGTH OF NARRATIVE                        
         CLC   NARDEPON,=C'DEPOSITED ON'                                        
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-14                                                          
         B     PDET32                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,(0,NARDPDAT),WORK                                    
         GOTO1 DATCON,DMCB,(0,WORK),(1,SRTDATDP)                                
         B     PDET32                                                           
*                                    ***INTER DEBIT***                          
*                                                                               
PDET30   TM    TRNSSTAT,X'80'        INTERAGENCY DEBIT?                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ZAP   SRTPOST,TRNSAMNT      RECEIVABLE AMOUNT                          
         MVC   SRTINCOM,ACKEYCON     CONTRA IS INCOME ACCOUNT                   
         MVI   SRTTYPE,SRTAUTO                                                  
         CLC   =C'AU',TRNSBTCH+2     DAILY JOURNAL AUTO POSTING?                
         BE    PDET32                                                           
         CLI   TRNSBTCH+2,C'U'       DAILY JOURNAL AUTO POSTING?                
         BE    PDET32                                                           
         MVI   SRTTYPE,SRTADJUS                                                 
         CLC   =C'AJ',TRNSBTCH+2     INTERAGENCY ADJUSTMENT?                    
         BE    PDET32                                                           
         MVI   SRTTYPE,SRTUNKNO      UNKNOWN                                    
*                                                                               
PDET32   L     R6,AMEDIAEL           MEDIA TRANSFER ELEMENT                     
         USING ACMTD,R6                                                         
         CLI   ACMTEL,ACMTELQ        MAKE SURE YOU HAVE ONE                     
         BNE   PDET34                ****JUST GET NEXT FOR NOW*****             
         MVC   SRTCLT,ACMTCLI        CLIENT                                     
         MVC   SRTPRD,ACMTPRD        PRODUCT                                    
         MVC   SRTMDUL,=C'SI'        SAVE MEDIA (SI ACCT OR MI RECORD)          
         TM    ACMTSTAT,X'02'        IS IT MI=                                  
         BZ    *+10                                                             
         MVC   SRTMDUL,=C'**'                                                   
         MVC   SRTMACC(L'ACMTMED2),ACMTMED2                                     
         MVC   SRTADMTH,ACMTMOS      ADVERTISING MONTH                          
         MVC   SRTDESC,ACMTDSCP      ESTIMATE DESCRIPTION                       
         MVC   FULL,ACMTGRS          GROSS AMNT                                 
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         ZAP   SRTGROSS,DUB                                                     
*                                                                               
PDET34   L     R6,ATCASHEL           SUB CASH ELEMENT                           
         USING TRCASHD,R6                                                       
         CLI   TRCSEL,TRCSELQ        MAKE SURE YOU HAVE ONE                     
         BNE   PDET36                                                           
         CLI   TRCSTYPE,C'G'         LOOK FOR GROSS                             
         BNE   PDET36                                                           
         ZAP   SRTGROSS,TRCSAMNT     SAVE GROSS AMOUNT                          
*                                    (GETTING THE GROSS FROM THE X'50'          
*                                    IS MORE RELIABLE THAN THE X'1A')           
*                                                                               
PDET36   L     R6,ASTATSEL           TRANS STATUS ELEMENT                       
         USING TRSTATD,R6                                                       
         CLI   TRSTEL,X'60'          MAKE SURE YOU HAVE ONE                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(1,SRTRDATE)                            
*                                                                               
         L     R1,ASORTSAV                                                      
         MVC   0(SRTLNQ,R1),SRTKEY   COPY SORTREC INTO SORTSAV                  
*                                                                               
         TM    QREPOPTS,BYAOR         BY A.O.R. REPORT                          
         BZ    PDET38                 NO - SKIP PUT TO BUFFALO                  
*                                                                               
         L     R5,ASORTSAV            ALWAYS USE SORTSAV                        
*                                     LEAVE SORTREC INTACT                      
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFA,(R5)                                 
         MVC   SRTTYPE(SRTVARLN),FOXES                                          
         BASR  RE,RF                  MTH TOTAL                                 
         MVC   SRTADMTH,FOXES                                                   
         BASR  RE,RF                  EST TOTAL                                 
         MVC   SRTESTIM,FOXES                                                   
         BASR  RE,RF                  MEDIA TOTAL                               
         MVC   SRTMED,FOXES                                                     
         BASR  RE,RF                  PROD TOTAL                                
         MVC   SRTPRD,FOXES                                                     
         BASR  RE,RF                  CLT TOTAL                                 
         MVC   SRTCLT,FOXES                                                     
         BASR  RE,RF                  RECV TOTAL                                
         MVC   SRTRECV,FOXES                                                    
         BASR  RE,RF                  REPT TOTAL                                
*                                                                               
PDET38   TM    QREPOPTS,BYCLIENT      BY CLIENT REPORT                          
         BZ    PDET40                 NO - SKIP PUT TO BUFFALO                  
*                                                                               
         L     R1,ASORTSAV                                                      
         L     R5,ASORTREC                                                      
         MVC   0(SRTLNQ,R1),SRTKEY    RESTORE SORTSAV TO SORTREC                
         MVC   0(SRTFLNQU,R1),SPACES  CLEAR KEY                                 
*                                                                               
         MVC   SRTRECV2-SRTD(L'SRTRECV2,R1),SRTRECV                             
         MVC   SRTCLPD2-SRTD(L'SRTCLPD2,R1),SRTCLPD                             
         MVC   SRTMED2-SRTD(L'SRTMED2,R1),SRTMED                                
*                                                                               
         L     R5,ASORTSAV             USE SORTSAV-LEAVE SORTREC INTACT         
         MVI   SRTBUFTP,CLTORDER                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFA,(R5)                                 
         MVC   SRTTYPE(SRTVARLN),FOXES                                          
         BASR  RE,RF                  MTH TOTAL                                 
         MVC   SRTADMTH,FOXES                                                   
         BASR  RE,RF                  EST TOTAL                                 
         MVC   SRTESTIM,FOXES                                                   
         BASR  RE,RF                  MEDIA TOTAL                               
         MVC   SRTMED,FOXES                                                     
         BASR  RE,RF                  PROD TOTAL                                
         MVC   SRTRECV2,FOXES                                                   
         BASR  RE,RF                  RECV TOTAL                                
         MVC   SRTPRD2,FOXES                                                    
         BASR  RE,RF                  CLT TOTAL                                 
         MVC   SRTCLT2,FOXES                                                    
         BASR  RE,RF                  REPT TOTAL                                
*                                                                               
PDET40   TM    QREPOPTS,BYMEDIA       BY MEDIA REPORT                           
         BZ    PDET04                 NO - SKIP PUT TO BUFFALO                  
*                                                                               
         L     R1,ASORTSAV                                                      
         L     R5,ASORTREC                                                      
         MVC   0(SRTLNQ,R1),0(R5)     RESTORE SORTREC                           
         MVC   0(SRTFLNQU,R1),SPACES       CLEAR KEY                            
*                                                                               
         MVC   SRTRECV3-SRTD(L'SRTRECV3,R1),SRTRECV                             
         MVC   SRTCLPD3-SRTD(L'SRTCLPD3,R1),SRTCLPD                             
         MVC   SRTMED3-SRTD(L'SRTMED3,R1),SRTMED                                
*                                                                               
         L     R5,ASORTSAV                                                      
         MVI   SRTBUFTP,MEDORDER                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFA,(R5)                                 
*                                                                               
         MVC   SRTTYPE(SRTVARLN),FOXES                                          
         BASR  RE,RF                  MTH TOTAL                                 
         MVC   SRTADMTH,FOXES                                                   
         BASR  RE,RF                  EST TOTAL                                 
         MVC   SRTESTIM,FOXES                                                   
         BASR  RE,RF                  PROD TOTAL                                
         MVC   SRTPRD3,FOXES                                                    
         BASR  RE,RF                  CLT TOTAL                                 
         MVC   SRTCLT3,FOXES                                                    
         BASR  RE,RF                  RECV TOTAL                                
         MVC   SRTRECV3,FOXES                                                   
         BASR  RE,RF                  MEDIA TOTAL                               
         MVC   SRTMED3,FOXES                                                    
         BASR  RE,RF                  REPT TOTAL                                
         B     PDET04                 YES - GET NEXT RECORD                     
*                                                                               
PDET42   MVC   AIO,AIO1               RESTORE AIO                               
         B     XIT                                                              
         EJECT                                                                  
********************************************************                        
*        DO THE REPORT                                 *                        
********************************************************                        
*                                                                               
REPORT   NTR1                                                                   
         OC    ALSORT,ALSORT          IS THERE A LAST SORT ADDR                 
         BZ    XIT                    NO DATA                                   
*                                                                               
         TM    QREPOPTS,DOWNLOAD          DOWNLOADING?                          
         BZ    REPT02                                                           
         GOTO1 ADWNL,DMCB,(RC),DWNINIT    YES, INITIALIZE                       
         B     REPT04                                                           
*                                                                               
REPT02   LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
*                                                                               
         USING SRTD,R5                                                          
REPT04   L     R5,ASORTREC            ADDR OF SORT WORK AREA                    
         USING WIDED,R6                                                         
         L     R6,AWIDE               ADDR FOR WIDE PRINT                       
*                                                                               
         MVI   FORCEHED,YES                                                     
         MVI   FIRSTIME,YES                                                     
         LA    RE,SAVEALL             RECEIVING FIELD                           
         LA    RF,SAVELNQ             RECEIVING FIELD LENGTH                    
         SR    R1,R1                  BITS 0-7 PAD FACTOR,8-32 LEN SEND         
         ICM   R1,8,=X'40'            INSERT 40 AS PADDING FACTOR               
         MVCL  RE,R0                                                            
*                                                                               
         BAS   RE,CLEARSRT            CLEAR SORT AREA                           
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ABUFFA),(R5),0                          
         MVC   SAVETYPE,SRTBUFTP                                                
         B     REPT08                                                           
*                                                                               
REPT06   DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',(0,ABUFFA),(R5),0                           
*                                                                               
REPT08   TM    DMCB+8,X'80'                                                     
         BO    XIT                    END OF SORTED RECORDS                     
         L     R1,DMCB+8                                                        
         ST    R1,ALSORT              SAVE ADDR OF LAST SORT                    
*                                                                               
         MVC   XP,XSPACES             CLEAR PRINT LINES                         
         MVC   XP2,XSPACES                                                      
*                                                                               
         MVI   RCSUBPRG,AORORDER      DEFAULT IS A.O.R. REPORT                  
         MVC   CURRRACT,SRTRECV                                                 
         MVC   CURCLPRD,SRTCLPD                                                 
         MVC   CURRMED,SRTMED                                                   
         CLC   SAVETYPE,SRTBUFTP      HAS BUFFALO REPORT TYPE CHANGED?          
         BE    *+14                                                             
         MVI   FORCEHED,YES           IF YES, FORCE NEW PAGE                    
         MVC   SAVETYPE,SRTBUFTP         SAVE NEW TYPE                          
*                                                                               
REPT09   CLI   SRTBUFTP,AORORDER                                                
         BE    REPT14                                                           
         CLI   SRTBUFTP,ESTORDER      EST SUMMARY REPORT?                       
         BNE   REPT10                                                           
         MVI   RCSUBPRG,ESTORDER                                                
         B     REPT14                                                           
*                                                                               
REPT10   CLI   SRTBUFTP,CLTORDER      CLIENT ORDER REPORT?                      
         BNE   REPT12                                                           
         MVC   CURRRACT,SRTRECV2                                                
         MVC   CURCLPRD,SRTCLPD2                                                
         MVC   CURRMED,SRTMED2                                                  
         MVI   RCSUBPRG,CLTORDER                                                
         B     REPT14                                                           
*                                                                               
REPT12   MVC   CURRRACT,SRTRECV3      MUST BE MEDIA ORDER REPORT                
         MVC   CURCLPRD,SRTCLPD3                                                
         MVC   CURRMED,SRTMED3                                                  
         MVI   RCSUBPRG,MEDORDER                                                
*                                                                               
REPT14   MVI   TOTALSW,NO             SET TOTAL RECORD SWITCH TO NO             
         BAS   RE,TOTAL               CHECK FOR BREAKS                          
         TM    QREPOPTS,DOWNLOAD      ARE WE DOWNLOADING?                       
         BZ    REPT15                 NO                                        
         CLI   TOTALSW,YES         YES,IS THIS A TOTAL?                         
         BE    REPT06              YES, SKIP IT                                 
*                                                                               
REPT15   CLI   TOTALSW,YES            IF TOTAL SKIP REFRESHING NAMES            
         BE    *+8                                                              
         BAS   RE,REFRESH             REFRESH NAMES                             
         MVI   FIRSTIME,NO                                                      
         USING PRTD,R4                                                          
         LA    R4,XP                                                            
*                                                                               
         CLC   TRKTOTL,SPACES         TOTAL? GO PRINT IT NOW                    
         BE    *+12                                                             
         MVI   SPACING,2                                                        
         B     REPT22                                                           
*                                                                               
         MVC   TRKENUM,SRTESTIM                                                 
         MVC   TRKDESC,SRTDESC                                                  
*                                                                               
         CLI   SRTBUFTP,ESTORDER      IF ITS EST SUMMARY SKIP TYPE              
         BE    REPT20                 LOOKUP                                    
         LA    R1,TYPTABLE                                                      
*                                                                               
REPT16   CLI   0(R1),FOX              THE TYPE OF TRANSACTION                   
         BE    REPT20                                                           
         CLC   SRTTYPE,0(R1)                                                    
         BE    REPT18                                                           
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     REPT16                                                           
*                                                                               
REPT18   MVC   TRKTYPE,2(R1)                                                    
*                                                                               
REPT20   MVC   TRKMEDIA,CURRMED       MEDIA TO PRINT LINE                       
         XC    WORK,WORK                                                        
         MVC   WORK(L'SRTADMTH),SRTADMTH            ADV MTH                     
         MVC   WORK+3(L'SRTGLMTH),SRTGLMTH          G/L MTH                     
         GOTO1 DATCON,DMCB,(1,WORK),(6,TRKADVM)                                 
         GOTO1 (RF),(R1),(1,WORK+3),(6,TRKGLMTH)                                
         GOTO1 (RF),(R1),(1,SRTRDATE),(5,TRKDAT1)                               
*                                                                               
         CLC   SRTDATCK,SPACES         IS THERE A CHECK DATE?                   
         BE    REPT22                  NO- SKIP DATCON                          
         MVI   TRKDATYP,CHEKDATE                                                
         GOTO1 DATCON,DMCB,(1,SRTDATCK),(5,TRKDAT2)                             
         MVC   TRKCKNUM,SRTCK          CHECK NUMBER                             
         CLC   SRTDATDP,SPACES         IS THERE A CHECK DEPOSIT DATE?           
         BE    REPT22                  NO- SKIP DATCON                          
         LA    R3,XP2                                                           
         MVI   TRKDATYP-PRTD(R3),DEPOSDTE                                       
         GOTO1 DATCON,DMCB,(1,SRTDATDP),(5,TRKDAT2-PRTD(R3))                    
*                                                                               
REPT22   CLI   SRTTYPE,SRTCHEK         IF IT'S A CHECK SKIP                     
         BE    REPT30                                                           
         CURED SRTGROSS,(L'TRKGROS,TRKGROS),2,MINUS=YES                         
         CURED SRTPOST,(L'TRKRECV,TRKRECV),2,MINUS=YES                          
*                                                                               
         CLI   SRTBUFTP,ESTORDER       IF EST SUMMARY                           
         BNE   REPT26                  PRINT AOR AND CREAT PCTS                 
         CP    SRTRCV,SRTPOST          IF ESTIMATED RCV AND POSTED              
         BE    REPT24                  ARE THE SAME,ONLY PRINT ONE              
*                                                                               
         MVI   TRKRVTYP,POSTRCV                  POSTED (P)                     
         LA    R3,XP2                                                           
         MVI   TRKRVTYP-PRTD(R3),ESTIMRCV        ESTIMATED (E)                  
         CURED SRTRCV,(L'TRKRECV,TRKRECV-PRTD(R3)),2,MINUS=YES                  
*                                                                               
REPT24   CLI   TOTALSW,YES                                                      
         BE    REPT28                  PCT TOTALS DONT MAKE SENSE               
         CURED SRTAPCT,(L'TRKAORPC,TRKAORPC),2,MINUS=YES                        
         CURED SRTRPCT,(L'TRKCREPC,TRKCREPC),2,MINUS=YES                        
         B     REPT28                                                           
*                                                                               
REPT26   CLI   TOTALSW,YES                                                      
         BNE   REPT32                  NOT A TOT LINE - NO VARIANCE             
*                                                                               
REPT28   ZAP   MYDUBL,SRTPOST                                                   
         SP    MYDUBL,SRTPAID                                                   
         CURED MYDUBL,(L'TRKVARI,TRKVARI),2,MINUS=YES                           
*                                                                               
REPT30   CURED SRTPAID,(L'TRKPAID,TRKPAID),2,MINUS=YES                          
*                                                                               
REPT32   TM    QREPOPTS,DOWNLOAD       DOWNLOADING?                             
         BZ    REPT34                  NO                                       
         GOTO1 ADWNDATA,DMCB,(RC),(R4),(R3) PUT OUT THE DATA                    
         B     REPT06                                                           
*                                                                               
REPT34   GOTO1 SPOOL,DMCB,(R8)                                                  
         B     REPT06                                                           
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
********************************************************                        
*        REFRESH REPORT NAMES                          *                        
********************************************************                        
*                                                                               
REFRESH  NTR1                                                                   
*                                                                               
         USING SRTD,R5                                                          
         L     R5,ASORTREC                                                      
         CLC   CURRRACT,SAVERACT                                                
         BE    REFR04                                                           
         CLC   SAVERACT,SPACES                                                  
         BE    *+8                                                              
         MVI   FORCEHED,YES                                                     
*                                                                               
         LA    R4,SRLEVNUM            NUMBER OF POSSIBLE SR LEVELS              
         LA    R3,SRLEVEL             SR LEVELS                                 
         LA    R2,SAVE1NAM            SAVE NAME OF FIRST LEVEL                  
         CLI   0(R3),0                IF LENGTH OF 1ST LEVEL IS 0 DIE           
         BNE   *+6                                                              
         DC    H'0'                                                             
REFR02   CLI   0(R3),0                IF LENGTH IS 0 WE'RE THROUGH              
         BE    REFR03A                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'SRTRCUL),CURRRCOM                                          
         ZIC   R1,0(R3)               LENGTH OF LEVEL IN R1                     
         BCTR  R1,0                   REDUCE FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SAVERACC(0),CURRRACC   DO WE HAVE THIS LEVEL SAVED?              
         BE    REFR03                 YES SKIP TO NEXT LEVEL                    
         EX    R1,*+8                 NO - GO GET NAME FROM TABLE               
         B     *+10                                                             
         MVC   KEY+L'SRTRCUL(0),CURRRACC                                        
         GOTO1 NAMLOOK,DMCB,KEY,ACODLIST,(R2)                                   
         MVC   SAVE4NAM,0(R2)         SAVE4NAM WILL HAVE POST LEVEL NAM         
*                                                                               
REFR03   LA    R3,SRLEVLEN(R3)        NEXT LEDGER LEVEL LENGTH                  
         LA    R2,SAVNAMLN(R2)        NEXT SAVE NAME AREA                       
         BCT   R4,REFR02                                                        
REFR03A  MVC   SAVERACT,CURRRACT      SAVE THE NEW RECEIVABLE ACCOUNT           
*                                                                               
REFR04   DS    0H                                                               
         CLC   CURCLPRD,SAVCLPRD      SAME CLIENT AND PRODUCT CODE              
         BE    REFR06                 YES - WE MUST HAVE CURRENT NAME           
         CLC   SAVCLPRD,SPACES                                                  
         BE    *+8                                                              
         MVI   FORCEHED,YES                                                     
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'COMPANY),COMPANY                                           
         MVC   KEY+L'COMPANY(L'PRODLEDG),PRODLEDG                               
         CLC   CURRCLT,SAVECLT         SAME CLIENT ?                            
         BE    REFR05                  YES - LOOKUP PRODUCT NAME                
*                                                                               
         MVC   KEY+L'COMPANY+L'PRODLEDG(L'CURRCLT),CURRCLT                      
         GOTO1 NAMLOOK,DMCB,KEY,ACLTLIST,SAVECNAM                               
*                                                                               
REFR05   MVC   KEY+L'COMPANY+L'PRODLEDG(L'CURCLPRD),CURCLPRD                    
         GOTO1 NAMLOOK,DMCB,KEY,ACLTLIST,SAVEPNAM                               
         MVC   SAVCLPRD,CURCLPRD       SAVE THE NEW CLIENT PRODUCT              
*                                                                               
REFR06   DS    0H                                                               
         CLC   CURRMED,SAVEMED         SAME MEDIA CODE                          
         BE    REFR08                  YES - WE MUST HAVE CURRENT NAME          
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'COMPANY),COMPANY                                           
         MVC   KEY+L'COMPANY(L'CURRMED),CURRMED                                 
         GOTO1 NAMLOOK,DMCB,KEY,AMEDLIST,SAVEMNAM                               
         MVC   SAVEMED,CURRMED         SAVE MEDIA CODE                          
*                                                                               
REFR08   DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'SRTADMTH),SRTADMTH                                        
         GOTO1 DATCON,DMCB,(1,WORK),(6,SAVEMTH)                                 
         MVC   SAVEEST,SRTESTIM        SAVE EST NUMBER                          
         B     XIT                                                              
         EJECT                                                                  
*****************************************************                           
*        SEARCH FOR NAME FROM BIN TABLES            *                           
*               PARM 1   ADDR OF KEY FOR LOOKUP     *                           
*               PARM 2   ADDR OF BIND PARMS         *                           
*               PARM 3   ADDR OF SAVE AREA FOR NAME *                           
*****************************************************                           
*                                                                               
NAMLOOK  NTR1                                                                   
         USING BIND,R5                                                          
*                                                                               
         L     R5,4(R1)             ADDR OF BIND PARMS                          
         L     R3,8(R1)             ADDR OF WHERE TO PUT THE NAME               
         MVC   DMCB+8(16),BININ     NUMBER,LENGTH,KEY MAX                       
         LA    R6,BINTABLE          A(TABLE)                                    
         L     R4,0(R1)             A(ITEM)                                     
         GOTO1 BINSRCH,DMCB,(X'00',(R4)),(R6)                                   
         CLI   DMCB,0                                                           
         BNE   XIT                  NOT FOUND                                   
*        DC    H'0'                                                             
         L     R2,DMCB                                                          
         MVC   0(SAVNAMLN,R3),L'CODKEY(R2)                                      
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
********************************************************                        
*        CHECK FOR TOTAL BREAKS                        *                        
********************************************************                        
*                                                                               
TOTAL    NTR1                                                                   
*                                                                               
         CLI   FIRSTIME,YES                FIRST TIME IN DON'T BOTHER           
         BE    XIT                                                              
*                                                                               
         L     R6,AWIDE                                                         
         USING WIDED,R6                                                         
         L     R5,ASORTREC                                                      
         USING SRTD,R5                                                          
         LA    R4,XP                                                            
         MVC   WORK,SPACES                                                      
         USING PRTD,R4                                                          
*                                                                               
         CLI   SRTBUFTP,AORORDER           A.O.R. ORDER REPORT?                 
         BE    *+12                                                             
         CLI   SRTBUFTP,ESTORDER           EST SUMMARY REPORT?                  
         BNE   TOT02                                                            
         CLI   CURRRACT,FOX                SIGNFIES REP TOT                     
         BE    TOT04                                                            
         CLI   CURRCLT,FOX                 SIGNIFIES AOR TOT                    
         BE    TOT06                                                            
         CLI   CURRPRD,FOX                 SIGNIFIES CLIENT TOT                 
         BE    TOT08                                                            
         CLI   CURRMDAC,FOX                SIGNIFIES PRODUCT TOT                
         BE    TOT10                                                            
         CLI   SRTESTIM,FOX                SIGNIFIES MEDIA TOT                  
         BE    TOT12                                                            
         B     TOT16                                                            
*                                                                               
TOT02    CLI   SRTBUFTP,CLTORDER           CLIENT ORDER REPORT?                 
         BNE   TOT03                                                            
         CLI   CURRCLT,FOX                 SIGNIFIES REP TOT                    
         BE    TOT04                                                            
         CLI   CURRPRD,FOX                 CLT TOT                              
         BE    TOT08                                                            
         CLI   CURRRACT,FOX                SIGNFIES PRD TOT                     
         BE    TOT10                                                            
         CLI   CURRMDAC,FOX                AOR TOT                              
         BE    TOT06                                                            
         CLI   SRTESTIM,FOX                SIGNIFIES MEDIA TOT                  
         BE    TOT12                                                            
         B     TOT16                                                            
*                                                                               
TOT03    CLI   SRTBUFTP,MEDORDER           MEDIA ORDER REPORT?                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   CURRMDAC,FOX                SIGNFIES REP TOT                     
         BE    TOT04                                                            
         CLI   CURRRACT,FOX                SIGNIFIES MED TOT                    
         BE    TOT12                                                            
         CLI   CURRCLT,FOX                 SIGNIFIES A.O.R. TOT                 
         BE    TOT06                                                            
         CLI   CURRPRD,FOX                 SIGNIFIES CLIENT TOT                 
         BE    TOT08                                                            
         CLI   SRTESTIM,FOX                SIGNIFIES PRODUCT TOT                
         BE    TOT10                                                            
         B     TOT16                                                            
*                                                                               
*                                          ***REQ TOTAL***                      
TOT04    MVC   WORK(L'REQUESDC),REQUESDC                                        
         B     TOT98                                                            
*                                                                               
*                                          ***AOR TOTAL***                      
TOT06    MVC   WORK(L'AORDC),AORDC                                              
         MVC   WORK+L'AORDC+1(L'SAVERACC),SAVERACC                              
         MVC   WORK+L'AORDC+1+L'SAVERACC+1(L'SAVE4NAM),SAVE4NAM                 
         B     TOT98                                                            
*                                           ***CLT TOTAL***                     
TOT08    MVC   WORK(L'CLIENTDC),CLIENTDC                                        
         MVC   WORK+L'CLIENTDC+1(L'SAVECLT),SAVECLT                             
         MVC   WORK+L'CLIENTDC+1+L'SAVECLT+1(L'SAVECNAM),SAVECNAM               
         B     TOT98                                                            
*                                           ***PRD TOTAL***                     
TOT10    MVC   WORK(L'PRODDC),PRODDC                                            
         MVC   WORK+L'PRODDC+1(L'SAVEPRD),SAVEPRD                               
         MVC   WORK+L'PRODDC+1+L'SAVEPRD+1(L'SAVEPNAM),SAVEPNAM                 
         B     TOT98                                                            
*                                                                               
*                                           ***MED TOTAL***                     
TOT12    MVC   WORK(L'MEDIADC),MEDIADC                                          
         MVC   WORK+L'MEDIADC+1(L'SAVEMED),SAVEMED                              
         MVC   WORK+L'MEDIADC+1+L'SAVEMED+1(L'SAVEMNAM),SAVEMNAM                
         B     TOT98                                                            
*                                                                               
TOT16    CLI   SRTADMTH,FOX                 ** EST NUMBER TOTAL **              
         BNE   TOT20                                                            
         MVC   WORK(L'ESTIMDC),ESTIMDC                                          
         MVC   WORK+L'ESTIMDC+1(L'SAVEEST),SAVEEST                              
         B     TOT98                                                            
*                                                                               
TOT20    CLI   SRTTYPE,FOX                  ** ADV MTH TOTAL **                 
         BNE   TOT99                                                            
         MVC   WORK(L'ADVERTDC),ADVERTDC                                        
         MVC   WORK+L'ADVERTDC+1(L'SAVEMTH),SAVEMTH                             
*                                                                               
TOT98    MVI   TOTALSW,YES                                                      
         TM    QREPOPTS,DOWNLOAD            DOWNLOADING?                        
         BO    TOT99                        YES, DON'T PRINT TOTALS             
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)              SKIP A LINE                         
*                                                                               
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         GOTO1 CHOPPER,DMCB,(L'WORK,WORK),(L'TRKTOTL,TRKTOTL),(L'XP,2)          
*                                                                               
TOT99    B     XIT                                                              
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
********************************************************                        
*        LOOKUP SR ACCOUNT ALL LEVEL AND ADD TO TABLE  *                        
********************************************************                        
*                                                                               
ACCADD   NTR1                                                                   
*                                                                               
         USING SRTD,R5                                                          
         L     R5,ASORTREC          ADDR OF SORT RECORD                         
         LA    R4,SRLEVNUM          NUMBER OF POSSIBLE LEVELS                   
         LA    R3,SRLEVEL           SR ACCOUNT LEVEL LENGTHS                    
         CLI   0(R3),0              IS LENGTH OF LEVEL 0                        
         BNE   ACCADD02             SOMETHINGS WRONG                            
         DC    H'0'                                                             
*                                                                               
ACCADD02 CLI   0(R3),0              IS LENGTH OF LEVEL 0                        
         BE    ACCADD09             YES - LEVEL NOT USED --WE'RE DONE           
         MVC   WORK,SPACES          CLEAR KEY WORK AREA                         
         MVC   WORK(L'SRTRCUL),SRTRCUL                                          
         ZIC   R1,0(R3)             LENGTH OF LEVEL(COMBINED)                   
         BCTR  R1,0                 REDUCE BY ONE FOR EX                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+L'SRTRCUL(0),SRTACCT        MOVE INTO WORK AREA             
         LA    R2,WORK                                                          
         GOTO1 BINADD,DMCB,(R2),ACODLIST                                        
*                                                                               
         LA    R3,SRLEVLEN(R3)      NEXT LEDGER LEVEL LENGTH                    
         BCT   R4,ACCADD02                                                      
*                                                                               
ACCADD09 B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
********************************************************                        
*        LOOKUP SR ACCOUNT ALL LEVEL AND ADD TO TABLE  *                        
********************************************************                        
*                                                                               
RDHIGH   NTR1                                                                   
         MVC   COMMAND,DMRDHI                                                   
         B     GTREC                                                            
RDSEQL   NTR1                                                                   
         MVC   COMMAND,DMRSEQ                                                   
GTREC    DS    0H                                                               
         L     R6,AIO                                                           
         USING INTRECD,R6                                                       
         MVC   SAVKKEY,KKEY                                                     
         GOTO1 DATAMGR,DMCB,COMMAND,DIR,KKEY,(R6)                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KKEY,0(R6)          SAVE DIRECTORY KEY                           
*                                                                               
         CLI   EMULATE,C'Y'                                                     
         BNE   XIT                                                              
         MVC   DA,INTKDA           SAVE DISK ADDRESS                            
         MVC   RDFIL,ACCMST        REGULAR FILE                                 
         TM    INTKSTA,X'04'                                                    
         BZ    *+10                                                             
         MVC   RDFIL,ACCARC        ARCHIVE FILE                                 
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,GETRECD,RDFIL,DA,(R6),DMWORK                        
         LA    R2,DMCB             R2=A(DATAMGR PARMS)                          
         ICM   R2,8,=X'FF'         FF IN HOB                                    
         GOTO1 ACCEMU,DMCB,NEWO,,(R6),(R6)                                      
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
********************************************************                        
*        ADD MEDIA TO TABLE                            *                        
********************************************************                        
*                                                                               
MEDADD   NTR1                                                                   
         USING SRTD,R5                                                          
         L     R5,ASORTREC          ADDR OF SORT RECORD                         
         MVC   WORK,SPACES          CLEAR KEY WORK AREA                         
         MVC   WORK(L'COMPANY),COMPANY                                          
         MVC   WORK+L'COMPANY(L'SRTMED),SRTMED                                  
         LA    R2,WORK                                                          
         GOTO1 BINADD,DMCB,(R2),AMEDLIST                                        
         B     XIT                                                              
         EJECT                                                                  
********************************************************                        
*        ADD DATA TO ESTIMATE TABLE                    *                        
********************************************************                        
*                                                                               
ESTADD   NTR1                                                                   
         USING SRTD,R5                                                          
         L     R5,ASORTREC          ADDR OF SORT RECORD                         
         USING ESTD,R2                                                          
         LA    R2,WORK                                                          
         MVC   WORK,SPACES          CLEAR KEY WORK AREA                         
         MVC   ESTCUL,SRTRCUL                                                   
         MVC   ESTACC,SRTACCT                                                   
         MVC   ESTCLT,SRTCLT                                                    
         MVC   ESTPRD,SRTPRD                                                    
         MVC   ESTEST,SRTESTIM                                                  
         GOTO1 BINADD,DMCB,(R2),AESTLIST                                        
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
********************************************************                        
*        LOOKUP MEDIA NAMES AND ADD TO TABLE           *                        
********************************************************                        
*                                                                               
MEDLOOK  NTR1                                                                   
         L     R5,AMEDLIST               ADDR OF MEDIA TABLE                    
         USING BIND,R5                                                          
         OC    BININ,BININ               IS TABLE EMPTY?                        
         BZ    XIT                                                              
         MVC   AIO,AIO2                  USE AIO2                               
         LA    R2,BINTABLE               ADDR TABLE IN R2                       
         L     R4,BININ                  NUMBER IN TABLE IN R4                  
         USING MEDD,R2                                                          
MEDLOOK1 MVC   KEY,SPACES                                                       
         MVC   MEDNAM,SPACES                                                    
         CLC   =C'SI',MEDKEY+L'COMPANY                                          
         BNE   MEDLOOK3                                                         
         MVC   KEY(L'MEDKEY),MEDKEY                                             
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(42),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MEDNAM                 NAMEIT PUT NAME IN ADDR IN R3          
         GOTO1 NAMEIT                                                           
         B     MEDLOOK9                                                         
*                                                                               
MEDLOOK3 MVI   KEY,X'08'                 MI RECORD                              
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(L'MEDKEY-3),MEDKEY+3                                       
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   MEDLOOK9                                                         
         MVI   ELCODE,ACMIELQ                                                   
         BAS   RE,GETELIO                GET MEDIA ELEMENT                      
         BNE   MEDLOOK9                                                         
         USING ACMID,R6                                                         
         MVC   MEDNAM(L'ACMIDESC),ACMIDESC                                      
MEDLOOK9 LA    R2,MEDLEN(R2)             NEXT TABLE ENTRY                       
         BCT   R4,MEDLOOK1                                                      
         MVC   AIO,AIO1                  RESET AIO                              
         B     XIT                                                              
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
*****************************************************                           
*        ADD CLI AND CLI/PROD CODE TO TABLE         *                           
*****************************************************                           
*                                                                               
CLIADD   NTR1                                                                   
*                                                                               
         L     R5,ASORTREC                  ADDR SORTREC                        
         USING SRTD,R5                                                          
*                                           GO LOOK FOR/ADD CLIENT CODE         
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'COMPANY),COMPANY                                          
         MVC   WORK+L'COMPANY(L'PRODLEDG),PRODLEDG                              
         MVC   WORK+L'COMPANY+L'PRODLEDG(L'SRTCLT),SRTCLT                       
         BAS   RE,CLIADD05                                                      
*                                                                               
         MVC   WORK,SPACES                  THEN CLIENT PRODUCT                 
         MVC   WORK(L'COMPANY),COMPANY                                          
         MVC   WORK+L'COMPANY(L'PRODLEDG),PRODLEDG                              
         MVC   WORK+L'COMPANY+L'PRODLEDG(L'SRTCLPD),SRTCLPD                     
         BAS   RE,CLIADD05                                                      
         B     XIT                                                              
         SPACE 3                                                                
CLIADD05 ST    RE,SAVERE                                                        
         LA    R3,WORK                                                          
         GOTO1 BINADD,DMCB,(R3),ACLTLIST                                        
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
*****************************************************                           
*        ADD AN ITEM TO BINSRCH TABLE               *                           
*****************************************************                           
*                                                                               
BINADD   NTR1                                                                   
         USING BIND,R5                                                          
*                                                                               
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ     NUMBER,LENGTH,KEY MAX                       
         LA    R6,BINTABLE          A(TABLE)                                    
         L     R4,0(R1)             A(ITEM)                                     
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                 TABLE FULL                                  
         MVC   BININ,DMCB+8         UPDATE COUNT                                
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*****************************************************                           
*        CLEAR SORT RECORD AREA                     *                           
*****************************************************                           
         SPACE 3                                                                
CLEARSRT NTR1                                                                   
         USING SRTD,R2                                                          
         L     R2,ASORTREC                                                      
         L     R1,AWIDE                                                         
         USING WIDED,R1                                                         
         MVC   SRTKEY(SRTLNQ),XSPACES                                           
         MVI   SRTBUFTP,0                                                       
         LA    R1,SBUKLOC(R2)           POINT R1 TO BUCKETS                     
         LA    R0,SBUKCONT              NUMBER OF BUCKETS                       
*                                                                               
         ZAP   0(SRTBUKLN,R1),=P'0'     CLEAR TO PACKED ZEROS                   
         LA    R1,SRTBUKLN(R1)                                                  
         BCT   R0,*-10                                                          
         B     XIT                                                              
         DROP  R1,R2                                                            
         EJECT                                                                  
*****************************************************                           
*        HEAD HOOK                                  *                           
*****************************************************                           
*                                                                               
HOOK     NTR1                                                                   
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    HOOKX                                                            
         L     R2,AWIDE                                                         
         USING WIDED,R2                                                         
*                                                                               
HOOK1    MVI   BOXYORN,YES                                                      
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXWIDTH,=A(L'XP)                                                
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,TOP                                                    
         MVI   BOXROWS+9,MIDLINE                                                
         MVI   BOXROWS+57,BOTTOM                                                
*                                                                               
         MVC   BOXCOLS,XSPACES                                                  
         LA    R1,BOXCOLS                                                       
         MVI   TRKLBOX-PRTD(R1),LEFTMAR                                         
         MVI   TRKBOX1-PRTD(R1),COLUMN                                          
         MVI   TRKBOX2-PRTD(R1),COLUMN                                          
         MVI   TRKBOX3-PRTD(R1),COLUMN                                          
         MVI   TRKBOX4-PRTD(R1),COLUMN                                          
         MVI   TRKBOX5-PRTD(R1),COLUMN                                          
         MVI   TRKBOX6-PRTD(R1),COLUMN                                          
         MVI   TRKBOX7-PRTD(R1),COLUMN                                          
         MVI   TRKBOX8-PRTD(R1),COLUMN                                          
         MVI   TRKBOX9-PRTD(R1),COLUMN                                          
         MVI   TRKBOX10-PRTD(R1),COLUMN                                         
         MVI   TRKRBOX-PRTD(R1),RIGHTMAR                                        
*                                                                               
         CLI   RCSUBPRG,CLTORDER                    CLT REPORT?                 
         BE    HOOK2                                                            
         CLI   RCSUBPRG,MEDORDER                    MEDIA REPORT?               
         BE    HOOK4                                                            
         MVC   XHEAD3+12(L'SAVERACC),SAVERACC       MUST BE AOR OR              
         MVC   XHEAD3+12+L'SAVERACC(30),SAVE4NAM    ESTIMATE REPORT             
         MVC   XHEAD4+12(L'SAVECLT),SAVECLT                                     
         MVC   XHEAD4+12+L'SAVERACC(30),SAVECNAM                                
         MVC   XHEAD5+12(L'SAVEPRD),SAVEPRD                                     
         MVC   XHEAD5+12+L'SAVERACC(30),SAVEPNAM                                
         B     HOOK9                                                            
HOOK2    MVC   XHEAD3+12(L'SAVECLT),SAVECLT                                     
         MVC   XHEAD3+12+L'SAVERACC(30),SAVECNAM                                
         MVC   XHEAD4+12(L'SAVEPRD),SAVEPRD                                     
         MVC   XHEAD4+12+L'SAVERACC(30),SAVEPNAM                                
         MVC   XHEAD5+12(L'SAVERACC),SAVERACC                                   
         MVC   XHEAD5+12+L'SAVERACC(30),SAVE4NAM                                
         B     HOOK9                                                            
*                                                                               
HOOK4    MVC   XHEAD3+12(L'SAVERACC),SAVEMED                                    
         MVC   XHEAD3+12+L'SAVERACC(30),SAVEMNAM                                
         MVC   XHEAD4+12(L'SAVERACC),SAVERACC                                   
         MVC   XHEAD4+12+L'SAVERACC(30),SAVE4NAM                                
         MVC   XHEAD5+12(L'SAVECLT),SAVECLT                                     
         MVC   XHEAD5+12+L'SAVERACC(30),SAVECNAM                                
         MVC   XHEAD6+12(L'SAVEPRD),SAVEPRD                                     
         MVC   XHEAD6+12+L'SAVERACC(30),SAVEPNAM                                
*                                                                               
HOOK9    MVC   XHEAD4+138(L'PERRANGE),PERRANGE  ADV. PERIOD                     
         TM    QOPTIONS,GLPERIOD      REQUESTED BY G/L MTH?                     
         BZ    *+10                                                             
         MVC   XHEAD4+125(4),=C'G/L '                                           
HOOKX    B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************                     
*        ACQUIRE CORE FOR BINARY TABLES,SORTR AND BUFFALO *                     
***********************************************************                     
*                                                                               
INITREP  NTR1                                                                   
         L     R0,=A(LENBUFF)            ACQUIRE AN ADDITIONAL BUFFER           
         GETMAIN R,LV=(0)                                                       
         ST    R1,ABUFF                  SAVE A(BUFFER)                         
         LR    R2,R1                     R2=BUFFER POINTER                      
         LR    RE,R2                                                            
         L     RF,=A(LENBUFF)            AND CLEAR IT                           
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,TWAMASTC               RE=A(MASTER CONTROL BLOCK)             
         USING MASTD,RE                                                         
         STCM  R2,15,MCUSRDMP            PRINT THE BUFFER IN A DUMP             
         LR    RF,R2                                                            
         A     RF,=A(LENBUFF)                                                   
         STCM  RF,15,MCUSRDMP+4          END OF DUMP AREA                       
         DROP  RE                                                               
*                                                                               
*                                        INIT BINARY TABLES                     
         USING BIND,R2                                                          
         ST    R2,ACODLIST                                                      
         MVC   BININ,=F'0'                                                      
         MVC   BINLEN,=A(CODLEN)                                                
         MVC   BINDISPK,=A(L'CODKEY)                                            
         MVC   BINMAX,=A(CODMAX)                                                
         A     R2,=A(CODSIZE)                                                   
*                                                                               
         ST    R2,ACLTLIST                                                      
         MVC   BININ,=F'0'                                                      
         MVC   BINLEN,=A(CLTLEN)                                                
         MVC   BINDISPK,=A(L'CLTKEY)                                            
         MVC   BINMAX,=A(CLTMAX)                                                
         A     R2,=A(CLTSIZE)                                                   
*                                                                               
         ST    R2,AMEDLIST                                                      
         MVC   BININ,=F'0'                                                      
         MVC   BINLEN,=A(MEDLEN)                                                
         MVC   BINDISPK,=A(L'MEDKEY)                                            
         MVC   BINMAX,=A(MEDMAX)                                                
         A     R2,=A(MEDSIZE)                                                   
*                                                                               
         ST    R2,AESTLIST                                                      
         MVC   BININ,=F'0'                                                      
         MVC   BINLEN,=A(ESTLEN)                                                
         MVC   BINDISPK,=A(L'ESTKEY)                                            
         MVC   BINMAX,=A(ESTMAX)                                                
         A     R2,=A(ESTSIZE)                                                   
*                                                                               
         ST    R2,ASORTREC               SORT RECORD                            
         A     R2,=A(SRTLNQ)                                                    
         ST    R2,ASORTSAV               SORT SAVE AREA                         
*                                                                               
         L     R1,ABOX                                                          
         MVC   AWIDE,BOXAWIDE-BOXD(R1)   ADDR OF WIDE PRINT                     
*                                                                               
         XC    COVPARM3,COVPARM3                                                
         L     R2,=A(BUFFALOC)                                                  
         A     R2,RELO                                                          
         ST    R2,ABUFFA                                                        
         GOTO1 =V(COVAIL),DMCB,C'SETB',20000,300000,(R2)                        
         MVC   COVPARM3,8(R1)          AND LEN OF BUFFER FOR "FREE"             
         MVC   ABUFFA,12(R1)           "NEW ABUFFA ADDR"                        
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFFA                                      
*                                                                               
         MVC   DIR,ACCFIL                                                       
         CLI   EMULATE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   DIR,ACCDIR          FOR EMULATED FILE READ DIRECTORY             
         B     XIT                                                              
         SPACE 3                                                                
***********************************************************                     
*        SUB-ROUTINE TO WRAP UP REPORT                    *                     
*        FREE UP ACQUIRED CORE                            *                     
***********************************************************                     
*                                                                               
WRAP     NTR1                                                                   
         L     R1,ABUFF                                                         
         L     R0,=A(LENBUFF)                                                   
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         GOTO1 =V(COVAIL),DMCB,C'FREE',ABUFFA,COVPARM3                          
*                                                                               
         L     RE,TWAMASTC                                                      
         USING MASTD,RE                                                         
         XC    MCUSRDMP(8),MCUSRDMP CLEAR OUT EXTRA DUMP AREA                   
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*****************************************************************               
* SUB-ROUTINE TO SET ELEMENT ADDRESSES FROM RECORD              *               
*****************************************************************               
*                                                                               
GETADDR  NTR1                                                                   
         L     R6,AIO                                                           
         AH    R6,DATADISP                                                      
         SR    R0,R0               CLEAR WORK REGISTER                          
         XC    ADELEMS,ADELEMS     CLEAR ALL ELEMENT ADDRESSES                  
*                                                                               
GETADDR2 CLI   0(R6),0             TEST FOR EOR                                 
         BE    GETADDRX                                                         
*                                                                               
         CLI   0(R6),ACNMELQ       X'20'                                        
         BNE   *+12                                                             
         ST    R6,ANAMEL                                                        
         B     GETADDR4                                                         
*                                                                               
         CLI   0(R6),TRNSELQ       X'44'                                        
         BNE   *+12                                                             
         ST    R6,ATRANSEL                                                      
         B     GETADDR4                                                         
*                                                                               
         CLI   0(R6),ACMTELQ       X'1A'                                        
         BNE   *+12                                                             
         ST    R6,AMEDIAEL                                                      
         B     GETADDR4                                                         
*                                                                               
         CLI   0(R6),TRCSELQ       X'50'                                        
         BNE   *+12                                                             
         ST    R6,ATCASHEL                                                      
         B     GETADDR4                                                         
*                                                                               
         CLI   0(R6),X'60'         X'60'                                        
         BNE   *+12                                                             
         ST    R6,ASTATSEL                                                      
         B     GETADDR4                                                         
*                                                                               
GETADDR4 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GETADDR2                                                         
*                                                                               
GETADDRX B     XIT                                                              
         EJECT                                                                  
*****************************************************************               
*        MISCELLANEOUS THINGS                                   *               
*****************************************************************               
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*        DECLARITIVES AND TABLES                                *               
*****************************************************************               
*                                                                               
FOXES    DC    30X'FF'                                                          
AORDC    DC    CL17'TOTALS FOR A.O.R.'                                          
CLIENTDC DC    CL17'TOTALS FOR CLIENT'                                          
PRODDC   DC    CL18'TOTALS FOR PRODUCT'                                         
ESTIMDC  DC    CL19'TOTALS FOR ESTIMATE'                                        
MEDIADC  DC    CL16'TOTALS FOR MEDIA'                                           
ADVERTDC DC    CL18'TOTALS FOR ADV MTH'                                         
REQUESDC DC    CL23'***TOTAL FOR REQUEST***'                                    
BADREQST DC    CL47'*ERROR* - EST. SUM INVALID WITH G/L MTH REQUEST'            
MIXDOWN  DC    CL42'*ERROR* - CAN''T DOWNLOAD MULTIPLE REPORTS'                 
DMRDHI   DC    C'DMRDHI  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
GETRECD  DC    C'GETREC  '                                                      
NEWO     DC    C'NEWO    '                                                      
ACCFIL   DC    C'ACCOUNT '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ACCARC   DC    C'ACCARC  '                                                      
         SPACE 3                                                                
TYPTABLE DS    0H                                                               
         DC    AL1(SRTEST),AL1(6),C'*ES*'                                       
         DC    AL1(SRTAUTO),AL1(6),C'*AU*'                                      
         DC    AL1(SRTADJUS),AL1(6),C'*AJ*'                                     
         DC    AL1(SRTCHEK),AL1(6),C'*CK*'                                      
         DC    AL1(SRTUNKNO),AL1(6),C'*UN*'                                     
         DC    X'FF'                                                            
*                                                                               
RELOTAB  DS    0F                                                               
         DC    A(DWNL)             DOWNLOAD ROUTINES                            
         DC    A(DWNDATA)          DOWNLOAD DETAILS                             
         DC    V(DLFLD)            DOWNLOAD MODULE                              
NRELOS   EQU   (*-RELOTAB)/L'RELOTAB                                            
         EJECT                                                                  
*              SPECS FOR HEADINGS ETC                                           
         SPACE 3                                                                
MYSPECS  DS    0D                                                               
         RSPEC MAXLINES,51                                                      
*        LEFT HEADLINES                                                         
*                                                                               
         SPROG 1,2,3,4                                                          
         ASPEC H1,2,RUN,WIDE=198                                                
         SPROG 1,4                                                              
         ASPEC H3,2,C'A.O.R.:',WIDE=198                                         
         ASPEC H4,2,C'CLIENT:',WIDE=198                                         
         ASPEC H5,2,C'PRODUCT:',WIDE=198                                        
         SPROG 2                                                                
         ASPEC H3,2,C'CLIENT:',WIDE=198                                         
         ASPEC H4,2,C'PRODUCT:',WIDE=198                                        
         ASPEC H5,2,C'A.O.R.:',WIDE=198                                         
         SPROG 3                                                                
         ASPEC H3,2,C'MEDIA:',WIDE=198                                          
         ASPEC H4,2,C'A.O.R.:',WIDE=198                                         
         ASPEC H5,2,C'CLIENT:',WIDE=198                                         
         ASPEC H6,2,C'PRODUCT:',WIDE=198                                        
*                                                                               
*        RIGHT HEADLINES                                                        
*                                                                               
         SPROG 1,2,3,4                                                          
         ASPEC H1,126,C'REPORT ACE2',WIDE=198                                   
         ASPEC H1,138,PAGE,WIDE=198                                             
         ASPEC H3,126,REQUESTOR,WIDE=198                                        
         ASPEC H4,126,C'ADV. PERIOD',WIDE=198                                   
         SPROG 1                                                                
         ASPEC H6,126,C'***A.O.R. ORDER***',WIDE=198                            
         SPROG 2                                                                
         ASPEC H6,126,C'***CLIENT ORDER***',WIDE=198                            
         SPROG 3                                                                
         ASPEC H6,126,C'***MEDIA ORDER***',WIDE=198                             
         SPROG 4                                                                
         ASPEC H6,126,C'***ESTIMATE SUMMARY***',WIDE=198                        
*                                                                               
*        REPORT TITLE                                                           
*                                                                               
         SPROG 1,2,3,4                                                          
         ASPEC H1,60,C'INTERAGENCY TRACKING STATEMENT',WIDE=198                 
         ASPEC H2,60,30C'_',WIDE=198                                            
*                                                                               
*        COLUMN HEADINGS                                                        
*                                                                               
         ASPEC H8,2,C'-------- ESTIMATE -----------',WIDE=198                   
         ASPEC H9,2,C'NUMBER            DESCRIPTION',WIDE=198                   
*                                                                               
         ASPEC H8,46,C'MEDIA',WIDE=198                                          
*                                                                               
         ASPEC H8,52,C' ADV ',WIDE=198                                          
         ASPEC H9,52,C'MONTH',WIDE=198                                          
*                                                                               
         ASPEC H8,70,C' G/L ',WIDE=198                                          
         ASPEC H9,70,C'MONTH',WIDE=198                                          
*                                                                               
         SPROG 1,2,3                                                            
         ASPEC H8,60,C'ACTIVITY',WIDE=198                                       
         ASPEC H9,60,C'  DATE  ',WIDE=198                                       
*                                                                               
         ASPEC H8,77,C'CHECK/DEP.',WIDE=198                                     
         ASPEC H9,77,C'  DATES   ',WIDE=198                                     
*                                                                               
         ASPEC H8,88,C'CHECK ',WIDE=198                                         
         ASPEC H9,88,C'NUMBER',WIDE=198                                         
*                                                                               
         SPROG 4                                                                
         ASPEC H8,60,C'POSTING',WIDE=198                                        
         ASPEC H9,60,C' DATE  ',WIDE=198                                        
*                                                                               
         ASPEC H8,77,C'  A.O.R.  ',WIDE=198                                     
         ASPEC H9,77,C'  FEE %   ',WIDE=198                                     
*                                                                               
         ASPEC H8,88,C' RECV   ',WIDE=198                                       
         ASPEC H9,88,C'   %    ',WIDE=198                                       
*                                                                               
         SPROG 1,2,3,4                                                          
         ASPEC H8,97,C' GROSS ',WIDE=198                                        
         ASPEC H9,97,C'BILLING',WIDE=198                                        
*                                                                               
         ASPEC H8,111,C'RECEIVABLE',WIDE=198                                    
         ASPEC H9,111,C'  AMOUNT  ',WIDE=198                                    
*                                                                               
         ASPEC H8,128,C' PAID ',WIDE=198                                        
         ASPEC H9,128,C'AMOUNT',WIDE=198                                        
*                                                                               
         ASPEC H8,142,C' VARIANCE',WIDE=198                                     
*                                                                               
         DC    X'00'                                                            
*                                                                               
         BUFF  LINES=1,                                                X        
               ROWS=1,                                                 X        
               COLUMNS=6,                                              X        
               FLAVOR=PACKED,                                          X        
               COMMENT=81,                                             X        
               KEYLIST=(65,A)                                                   
         EJECT                                                                  
********************************************************************            
* DOWNLOAD DATA                                                    *            
********************************************************************            
*                                                                               
         USING PRTD,DWNLINE1                                                    
LINE2    USING PRTD,DWNLINE2                                                    
DWNDATA  DS    0D                                                               
         NMOD1 0,**DWND**                                                       
         L     RC,0(R1)                                                         
         L     R4,4(R1)                                                         
         L     R3,8(R1)                                                         
         MVC   DWNLINE1,0(R4)               MOVE PRINT LINE TO SAVE             
         MVI   0(R4),C' '                   CLEAR XP                            
         MVC   1(L'XP-1,R4),0(R4)                                               
*                                                                               
         MVC   DWNLINE2,0(R3)               MOVE PRINT LINE TO SAVE             
         MVI   0(R3),C' '                   CLEAR XP2                           
         MVC   1(L'XP-1,R3),0(R3)                                               
         MVI   PRTSIZE,0                    SET COLUMN LEN=0-NO PADDING         
         MVI   FORCEHED,NO                                                      
*                                                                               
*        DOWNLOAD HEADER INFORMATION                                            
*                                                                               
         CLI   RCSUBPRG,CLTORDER            CLT REPORT?                         
         BE    DWND02                       YES                                 
         CLI   RCSUBPRG,MEDORDER            MEDIA REPORT?                       
         BE    DWND04                       YES                                 
*                                           NO, EST OR AOR                      
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SAVERACC),SAVERACC  SR ACCOUNT                          
         LA    R1,L'SAVERACC                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(30),SAVE4NAM          SR ACCOUNT NAME                     
         LA    R1,30                                                            
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SAVECLT),SAVECLT    CLIENT                              
         LA    R1,L'SAVECLT                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(30),SAVECNAM          CLIENT NAME                         
         LA    R1,30                                                            
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SAVEPRD),SAVEPRD    PRODUCT                             
         LA    R1,L'SAVEPRD                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(30),SAVEPNAM          PRODUCT NAME                        
         LA    R1,30                                                            
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
         B     DWND06                                                           
*                                                                               
DWND02   MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SAVECLT),SAVECLT    CLIENT                              
         LA    R1,L'SAVECLT                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(30),SAVECNAM          CLIENT NAME                         
         LA    R1,30                                                            
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SAVEPRD),SAVEPRD    PRODUCT                             
         LA    R1,L'SAVEPRD                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(30),SAVEPNAM          PRODUCT NAME                        
         LA    R1,30                                                            
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SAVERACC),SAVERACC  SR ACCOUNT                          
         LA    R1,L'SAVERACC                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(30),SAVE4NAM          SR ACCOUNT NAME                     
         LA    R1,30                                                            
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
         B     DWND06                                                           
*                                                                               
DWND04   MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SAVERACC),SAVEMED   MEDIA                               
         LA    R1,L'SAVERACC                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(30),SAVEMNAM          MEDIA NAME                          
         LA    R1,30                                                            
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SAVERACC),SAVERACC  SR ACCOUNT                          
         LA    R1,L'SAVERACC                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(30),SAVE4NAM          SR ACCOUNT NAME                     
         LA    R1,30                                                            
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SAVECLT),SAVECLT    CLIENT                              
         LA    R1,L'SAVECLT                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(30),SAVECNAM          CLIENT NAME                         
         LA    R1,30                                                            
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SAVEPRD),SAVEPRD    PRODUCT                             
         LA    R1,L'SAVEPRD                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(30),SAVEPNAM          PRODUCT NAME                        
         LA    R1,30                                                            
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
DWND06   MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'PERRANGE),PERRANGE  ADV. PERIOD                         
         LA    R1,L'PERRANGE                                                    
         TM    QOPTIONS,GLPERIOD            REQUESTED BY G/L MTH?               
         BZ    *+14                                                             
         MVC   DWNFLD(4),=C'G/L '                                               
         LA    R1,4                                                             
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
*        DOWNLOAD DETAILS                                                       
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKENUM),TRKENUM    ESTIMATE NUMBER                     
         LA    R1,L'TRKENUM                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKDESC),TRKDESC    ESTIMATE DESCRIPTION                
         LA    R1,L'TRKDESC                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         CLI   RCSUBPRG,ESTORDER                                                
         BE    DWND08                                                           
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKTYPE),TRKTYPE    TRANSACTION TYPE                    
         LA    R1,L'TRKTYPE                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
DWND08   MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKMEDIA),TRKMEDIA  MEDIA CODE                          
         LA    R1,L'TRKMEDIA                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKADVM),TRKADVM    MEDIA ADV MONTH                     
         LA    R1,L'TRKADVM                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKDAT1),TRKDAT1    POSTING DATE                        
         LA    R1,L'TRKDAT1                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKGLMTH),TRKGLMTH  G/L MONTH                           
         LA    R1,L'TRKGLMTH                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         CLI   RCSUBPRG,ESTORDER                                                
         BE    DWND10                                                           
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKDATYP),TRKDATYP  CHECK DATE TYPE                     
         LA    R1,L'TRKDATYP                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKDAT2),TRKDAT2    CHECK DATE                          
         LA    R1,L'TRKDAT2                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKDATYP),LINE2.TRKDATYP                                
         LA    R1,L'TRKDATYP                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKDAT2),LINE2.TRKDAT2                                  
         LA    R1,L'TRKDAT2                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
DWND10   CLI   RCSUBPRG,ESTORDER                                                
         BNE   DWND12                                                           
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKAORPC),TRKAORPC  A.O.R. PCT                          
         LA    R1,L'TRKAORPC                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
DWND12   MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKCKNUM),TRKCKNUM  CHECK NUMBER/CREATIVE PCT           
         LA    R1,L'TRKCKNUM                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKGROS),TRKGROS    GROSS                               
         LA    R1,L'TRKGROS                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM                                           
*                                                                               
         CLI   RCSUBPRG,ESTORDER                                                
         BNE   DWND14                                                           
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKRVTYP),TRKRVTYP  RECEIVEABLE TYPE                    
         LA    R1,L'TRKRVTYP                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
DWND14   MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKRECV),TRKRECV    RECEIVEABLE AMOUNT                  
         LA    R1,L'TRKRECV                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM                                           
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKPAID),TRKPAID    PAID SO FAR                         
         LA    R1,L'TRKPAID                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM                                           
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'TRKVARI),TRKVARI    VARIANCE                            
         LA    R1,L'TRKVARI                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM                                           
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         GOTO1 ADWNL,DMCB,(RC),DWNEOL       DOWNLOAD EOL MARKER                 
*                                                                               
         MVC   0(L'XP,R4),DWNLINE1          RESTORE PRINT LINE2                 
         MVC   0(L'XP,R3),DWNLINE2                                              
*                                                                               
         XMOD1                                                                  
         EJECT                                                                  
*********************************************************************           
* DOWNLOAD MODULE                                                   *           
*          PARM1 - RC                                               *           
*          PARM2 - ACTION                                           *           
*********************************************************************           
*                                                                               
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         USING DLCBD,R5                                                         
         L     R5,AIO3                                                          
         USING WIDED,R6                                                         
         L     R6,AWIDE               ADDR FOR WIDE PRINT                       
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWLN20                                                           
         CLI   DWNMODE,DWNNUM      DOWN-LOAD NUMBER                             
         BE    DWLN30                                                           
         CLI   DWNMODE,DWNPACK     DOWN-LOAD NUMBER (PACKED)                    
         BE    DWLN40                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWLN50                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWLN50                                                           
         DC    H'0'                                                             
*                                                                               
* INITIALIZATION                                                                
*                                                                               
DWNL10   MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,XP               PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'P)                                                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 DLFLD,(R5)                                                       
         B     DWNLX               EXIT                                         
*                                                                               
* DOWNLOAD A RECORD - TEXT                                                      
*                                                                               
DWLN20   MVC   DLCBLEN,PRTSIZE     LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWLN50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER                                                    
*                                                                               
DWLN30   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE   IS NUMBER                             
         MVC   DLCBLEN,PRTSIZE     LEN OF FIELD                                 
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWLN50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER (PACKED)                                           
*                                                                               
DWLN40   MVI   DLCBTYP,DLCBPACF    PACKED DATA                                  
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBLEN,L'TRKGROS   YES, USE MAXIMUM LENGTH OF NUMERICS          
         XC    DLCBFLD,DLCBFLD     CLEAN DWNLOAD FIELD TO 0'S                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         NC    DLCBFLD,DLCBFLD    YES, MAKE SURE NUMERIC FLD NOT ZEROS          
         BNZ   DWLN50              NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD A 0          
*                                                                               
* END OF LINE/END OF RECORD                                                     
*                                                                               
DWLN50   GOTO1 DLFLD,(R5)          DOWN-LOAD FIELD                              
*                                                                               
DWNLX    XMOD1                                                                  
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HOOK                                                      *          
**********************************************************************          
*                                                                               
DWNHOOK  NTR1  ,                                                                
         GOTO1 SPOOL,PARAS,ASPOOLD                                              
         MVI   LINE,1              MAKE SURE THERE IS NO PAGE BREAK             
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*ACINTWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACINTWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDWIDED                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
*DDBUFFALOD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
         EJECT                                                                  
T619FFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACINTF7D                                                       
         EJECT                                                                  
*REMAINING SPACE IN TWA*                                                        
         SPACE 3                                                                
MYDUBL   DS    PL8                                                              
COVPARM3 DS    A                                                                
         EJECT                                                                  
*****************************************************************               
* DSECT TO COVER LOCAL WORKING STORAGE                          *               
*****************************************************************               
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   LOCAL                                                            
QREQUEST DS    0CL(QREQLEN)                                                     
QRQSTART EQU   *                                                                
QOFFICE  DS    CL2                                                              
QACCOUNT DS    CL15                                                             
QCLIPROD DS    0CL(L'QCLI+L'QPROD)                                              
QCLI     DS    CL3                                                              
QPROD    DS    CL3                                                              
QMEDIA   DS    CL15                                                             
QMEDMIE  DS    CL05                                                             
QMEDMI   DS    CL3                                                              
QMEDCOD  DS    CL2                                                              
QESTIM   DS    CL(L'ESTIMATE)                                                   
QCHECK   DS    CL6                                                              
QESTART  DS    CL2                                                              
QEEND    DS    CL2                                                              
QREPTYPE DS    C                                                                
QREPOPTS DS    X                        REPORT TYPE OPTIONS                     
*                                       X'80' BY A.O.R. ACCOUNT                 
*                                       X'40' BY CLIENT PRODUCT                 
*                                       X'20' BY MEDIA                          
*                                       X'10' ESTIMATE SUMMARY REPORT           
*                                       X'08' DOWNLOAD REPORT                   
QOPTIONS DS    X                        REPORT OPTIONS                          
*                                       X'80' EST PERIOD IS ADV PERIOD          
*                                       X'40' EST PERIOD IS G/L PERIOD          
QREQLEN  EQU   *-QRQSTART                                                       
*                                                                               
PERRANGE DS    CL15                PERIOD RANGE FOR HEADLINES                   
*                                                                               
SAVEALL  DS    0H                                                               
SAVETYPE DS    CL1                 TYPE OF BUFFALO RECORD                       
SAVERACT DS    0CL15               SR ACCOUNT                                   
SAVERCOM DS    CL1                 COMPANY                                      
SAVERUL  DS    CL2                 UNIT/LEDGER                                  
SAVERACC DS    CL12                SR ACCOUNT                                   
SAVE1NAM DS    CL36                ACCOUNT LEVEL 1 NAME                         
SAVNAMLN EQU   *-SAVE1NAM                                                       
SAVE2NAM DS    CL(SAVNAMLN)        ACCOUNT LEVEL 2 NAME                         
SAVE3NAM DS    CL(SAVNAMLN)        ACCOUNT LEVEL 3 NAME                         
SAVE4NAM DS    CL(SAVNAMLN)        ACCOUNT LEVEL 4 NAME                         
SAVCLPRD DS    0CL6                CLIENT PRODUCT                               
SAVECLT  DS    CL3                 CLIENT                                       
SAVEPRD  DS    CL3                 PRODUCT                                      
SAVECNAM DS    CL(SAVNAMLN)        CLIENT NAME                                  
SAVEPNAM DS    CL(SAVNAMLN)        PRODUCT NAME                                 
SAVEMED  DS    CL14                MEDIA ACCOUNT                                
SAVEMNAM DS    CL(SAVNAMLN)        MEDIA NAME OR DESCRIPTION                    
SAVEEST  DS    CL6                 ESTIMATE NUMBER                              
SAVEMTH  DS    CL6                 ADV MTH                                      
SAVEENAM DS    CL(SAVNAMLN)        ESTIMATE DESCRIPTION                         
CURRRACT DS    0CL15               CURRENT SR ACCOUNT                           
CURRRCOM DS    CL1                         COMPANY                              
CURRRUL  DS    CL2                         UNIT/LEDGER                          
CURRRACC DS    CL12                        SR ACCOUNT                           
CURCLPRD DS    0CL6                        CLIENT PRODUCT                       
CURRCLT  DS    CL3                         CLIENT                               
CURRPRD  DS    CL3                         PRODUCT                              
CURRMED  DS    0CL14                       U/L MEDIA CODE                       
CURRMDUL DS    CL2                         U/L                                  
CURRMDAC DS    CL12                        MEDIA CODE                           
SAVELNQ  EQU   *-SAVEALL                                                        
*                                                                               
SRACCONT DS    CL(L'CODKEY)        SR ACCOUNT WE ARE READING CURRENTLY          
*                                                                               
SRLEVEL  DS    0CL(SRLEVLEN*SRLEVNUM)                                           
SRLEVELA DS    CL1                 HIERARCHY OF SR LENGTH OF LEVEL A            
SRLEVLEN EQU   *-SRLEVELA                                                       
SRLEVELB DS    CL(SRLEVLEN)                                  LEVEL B            
SRLEVELC DS    CL(SRLEVLEN)                                  LEVEL C            
SRLVLAST EQU   *                                                                
SRLEVELD DS    CL(SRLEVLEN)                                  LEVEL D            
SRLEVNUM EQU   (*-SRLEVELA)/SRLEVLEN                                            
SRNUMLEV DS    CL1                 ACTUAL NUMBER OF LEVELS                      
         SPACE 3                                                                
LEN      DS    CL1                 LENGTH OF SR ACCOUNT REQUESTED               
DA       DS    XL4                 DISK ADDRESS                                 
DIR      DS    CL(L'ACCFIL)        DIR NAME (ACCFIL/ACCDIR)                     
RDFIL    DS    CL(L'ACCMST)        FIL NAME (ACCMST/ACCARC)                     
KKEY     DS    CL(ACCKLEN)                                                      
SAVKKEY  DS    CL(ACCKLEN)                                                      
NARRALEN DS    CL1                 LENGTH OF TRANS NARRATIVE                    
FIRSTIME DS    C                   FIRST RECORD FROM SORT                       
TOTALSW  DS    C                   THIS IS A TOTAL RECORD                       
SVEST    DS    CL(L'TRNSREF)       SAVE TRANSACTION REFERENCE/ESTIMATE          
*                                                                               
VTYPES   DS    0A                                                               
ADWNL    DS    A                   DOWNLOAD ROUTINE                             
ADWNDATA DS    A                   DOWNLOAD DATA ROUTINE                        
DLFLD    DS    V                   DOWNLOAD MODULE                              
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
DWNFLD   DS    CL36                SAVED AREA FOR FIELD TO BE DWNLOADED         
PRTSIZE  DS    CL1                 DOWNLOAD FLD PRINT SIZE FOR PADDING          
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                    DOWN-LOAD INITIALIZATION                    
DWNEOL   EQU   2                    MARK END OF LINE                            
DWNEOR   EQU   3                    MARK END OF REPORT                          
DWNTEXT  EQU   4                    DOWN-LOAD TEXT                              
DWNNUM   EQU   5                    DOWN-LOAD NUMBER                            
DWNPACK  EQU   6                    DOWN-LOAD NUMBER (PACKED)                   
*                                                                               
DWNLINE1 DS    CL(L'XP)                                                         
DWNLINE2 DS    CL(L'XP2)                                                        
LOCALCLN EQU   *-LOCAL                                                          
*                                                                               
*        ADDRESSES OF BUFFERS                                                   
*                                                                               
ABUFF    DS    A                   A(ACQUIRED BUFFER)                           
ACODLIST DS    A                   A(A.O.R. SR ACCOUNTS)                        
ACLTLIST DS    A                   A(CLIENT/PRODUCT AND NAMES)                  
AMEDLIST DS    A                                                                
AESTLIST DS    A                                                                
ASORTREC DS    A                                                                
ASORTSAV DS    A                                                                
*                                                                               
*        ADDRESSES OF NEEDED ELEMENTS                                           
*                                                                               
ADELEMS  DS    0XL(ADELNUM*4)                                                   
ANAMEL   DS    A                   A(NAME)                                      
ATRANSEL DS    A                   A(TRANSACTION)                               
AMEDIAEL DS    A                   A(MEDIA TRANSFER)                            
ATCASHEL DS    A                   A(SUBSIDIARY CASH)                           
ASTATSEL DS    A                   A(TRANS STATUS)                              
         DS    6A                                                               
ADELNUM  EQU   (*-ANAMEL)/4                                                     
*                                                                               
*        ADDRESSES IN GENERAL                                                   
*                                                                               
SAVERE   DS    A                                                                
APRTLIN  DS    A                   A(CURRENT PRINT LINE)                        
AMYPLACE DS    A                   A(OF WHERE I'M UP TO IN A TABLE)             
ALSORT   DS    A                   A(OF LAST SORTED REC)                        
AWIDE    DS    A                                                                
ABUFFA   DS    A                   A(BUFFALO CSECT)                             
RELO     DS    A                                                                
         SPACE 3                                                                
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
*****************************************************************               
* DSECT TO COVER PRINT LINE                                     *               
*****************************************************************               
*                                                                               
PRTD     DSECT                                                                  
PRTLIN   EQU   *                                                                
         ORG   PRTLIN              A.O.R. TRACKING REPORT                       
TRKLBOX  DS    C                                                                
TRKENUM  DS    CL6                 ESTIMATE NUMBER                              
         DS    CL1                                                              
         ORG   TRKENUM+2                                                        
TRKTOTL DS     0CL40               TOTAL LINE DESCRIPTION                       
         DS    CL5                                                              
TRKDESC  DS    CL30                ESTIMATE DESCRIPTION                         
         DS    CL1                                                              
TRKTYPE  DS    CL4                 TYPE OF TRANSACTIONN                         
         DS    CL1                                                              
TRKBOX1  DS    C                                                                
TRKMEDIA DS    CL4                 MEDIA                                        
         DS    CL1                                                              
TRKBOX2  DS    C                                                                
TRKADVM  DS    CL6                 MEDIA ADV MTH                                
TRKBOX3  DS    C                                                                
         DS    C                                                                
TRKDAT1  DS    CL8                 POSTING DATE                                 
         DS    C                                                                
TRKBOX4  DS    C                                                                
TRKGLMTH DS    CL6                 G/L MTH                                      
TRKBOX5  DS    C                                                                
TRKDATYP DS    C                   MARK AS C- CHECK DATE D-DEP DATE             
         DS    C                                                                
TRKDAT2  DS    CL8                 CHECK DATE                                   
         ORG   TRKDATYP            (REDFINE BOX 4 FOR EST SUMMARY REP)          
         DS    CL3                                                              
TRKAORPC DS    CL6                 A.O.R. PCT                                   
         DS    C                                                                
TRKBOX6  DS    C                                                                
TRKCKNUM DS    CL6                 CHECK NUMBER                                 
         ORG   TRKCKNUM            (REDEFINE BOX 5 FOR EST REP)                 
TRKCREPC DS    CL6                 CREATIVE PCT                                 
TRKBOX7  DS    C                                                                
         DS    C                                                                
TRKGROS  DS    CL12                GROSS                                        
         DS    C                                                                
TRKBOX8  DS    C                                                                
TRKRVTYP DS    C                   MARK AS E- ESTIMATE P-POSTED                 
TRKRECV  DS    CL12                RECEIVABLE AMT                               
         DS    C                                                                
TRKBOX9  DS    C                                                                
         DS    C                                                                
TRKPAID  DS    CL12                PAID SO FAR                                  
         DS    C                                                                
TRKBOX10 DS    C                                                                
         DS    C                                                                
TRKVARI  DS    CL12                VARIANCE                                     
         DS    C                                                                
TRKRBOX  DS    C                                                                
TRKLNQ   EQU   *-PRTD                                                           
*                                                                               
         EJECT                                                                  
*****************************************************************               
* DSECT TO COVER BUFFALO SORT RECORD                            *               
*****************************************************************               
*                                                                               
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTBUFTP DS    CL1                 TYPE OF BUFFALO RECORD                       
*                                                                               
SRTRECV  DS    0CL(L'SRTRCUL+L'SRTACCT)                                         
SRTRCUL  DS    CL3                                                              
SRTACCT  DS    CL12                A.O.R. RECV ACCOUNT                          
*RTOFFIC DS    CL2                 OFFICE OF CLIENT                             
*                                                                               
         DS    CL3                 SPARE                                        
SRTCLPD  DS    0CL(L'SRTCLT+L'SRTPRD)                                           
SRTCLT   DS    CL3                 CLIENT CODE                                  
SRTPRD   DS    CL3                 PRODUCT CODE                                 
         DS    CL6                 SPARE                                        
*                                                                               
         DS    CL1                 SPARE                                        
SRTMED   DS    0CL(L'SRTMDUL+L'SRTMACC)                                         
SRTMDUL  DS    CL2                 UNIT/LEDGER FOR INCOME OR MI RECORD          
SRTMACC  DS    CL12                INCOME ACCOUNT                               
*                                                                               
         ORG   SRTRECV                                                          
         DS    CL3                 SPARE                                        
SRTCLPD2 DS    0CL(L'SRTCLT+L'SRTPRD)                                           
SRTCLT2  DS    CL3                 CLIENT CODE                                  
SRTPRD2  DS    CL3                 PRODUCT CODE                                 
         DS    CL6                 SPARE                                        
*                                                                               
SRTRECV2 DS    0CL(L'SRTRCUL+L'SRTACCT)                                         
SRTRCUL2 DS    CL3                                                              
SRTACCT2 DS    CL12                A.O.R. RECV ACCOUNT                          
*RTOFFIC DS    CL2                 OFFICE OF CLIENT                             
*                                                                               
         DS    CL1                 SPARE                                        
SRTMED2  DS    0CL(L'SRTMDUL+L'SRTMACC)                                         
SRTMDUL2 DS    CL2                 UNIT/LEDGER FOR INCOME OR MI RECORD          
SRTMACC2 DS    CL12                INCOME ACCOUNT                               
         ORG   SRTRECV                                                          
*                                                                               
         DS    CL1                 SPARE                                        
SRTMED3  DS    0CL(L'SRTMDUL+L'SRTMACC)                                         
SRTMDUL3 DS    CL2                 UNIT/LEDGER FOR INCOME OR MI RECORD          
SRTMACC3 DS    CL12                INCOME ACCOUNT                               
*                                                                               
SRTRECV3 DS    0CL(L'SRTRCUL+L'SRTACCT)                                         
SRTRCUL3 DS    CL3                                                              
SRTACCT3 DS    CL12                A.O.R. RECV ACCOUNT                          
*                                                                               
         DS    CL3                 SPARE                                        
SRTCLPD3 DS    0CL(L'SRTCLT+L'SRTPRD)                                           
SRTCLT3  DS    CL3                 CLIENT CODE                                  
SRTPRD3  DS    CL3                 PRODUCT CODE                                 
         DS    CL6                 SPARE                                        
*                                                                               
SRTFLNQU EQU   *-SRTKEY            LENGTH OF BASIC SORT FIELDS                  
*                                                                               
SRTESTIM DS    CL6                 ESTIMATE NUMBER                              
SRTADMTH DS    XL2                 ADVERTISER MONTH                             
SRTTYPE  DS    CL1                 SORT TYPE                                    
SRTEST   EQU   1                   1 = ESTIMATE                                 
SRTAUTO  EQU   2                   2 = AUTO POSTING                             
SRTADJUS EQU   3                   3 = ADJUSTMENT                               
SRTCHEK  EQU   4                   4 = CHECK                                    
SRTUNKNO EQU   5                   5 = UNKNOWN                                  
SRTRDATE DS    CL3                 DATE TRANSACTION HIT THE FILE                
SRTCK    DS    CL6                 CHECK NUMBER FOR SORTING                     
SRTSUBRF DS    CL1                 SUB REF(STOPS BUFFALO COMBINING)             
SRTVARLN EQU   *-SRTTYPE           LENGTH OF VARIOUS FOR BUFFALO FOXES          
SRTKLNQU EQU   *-SRTKEY            LENGTH OF KEY                                
SRTCOMST DS    0C                                                               
SRTDATE  DS    CL3                 TRANS DATE (BLANK FOR EST)                   
SRTDATCK DS    CL3                 CHECK DATE (BLANK FOR EST)                   
SRTDATDP DS    CL3                 CHECK DEPOSIT DATE (BLANK FOR EST)           
SRTGLMTH DS    XL2                 GL MONTH                                     
SRTCASH  DS    0CL(L'SRTCACUL+L'SRTCAACC)                                       
SRTCACUL DS    CL3                 CO/UNIT/LEDGER FOR CASH                      
SRTCAACC DS    CL12                CASH ACCOUNT                                 
SRTINCOM DS    CL15                INCOME ACCOUNT                               
SRTDESC  DS    CL36                ESTIMATE DESCRIPTION                         
SRTPRS   DS    CL2                 YYMM START EST PERIOD                        
SRTPRE   DS    CL2                 YYMM END EST PERIOD                          
SRTDLNQ  EQU   *-SRTCOMST                                                       
SBUKLOC  EQU   *-SRTD                                                           
SRTBUKS  DS    0C                                                               
SRTGROSS DS    PL8                 GROSS AMNT                                   
SRTBUKLN EQU   *-SRTGROSS                                                       
SRTRCV   DS    PL8                 RECEIVABLE AMOUNT EST                        
SRTPOST  DS    PL8                 POSTED RECV AMOUNT                           
SRTPAID  DS    PL8                 PAID AMOUNT                                  
SRTAPCT  DS    PL8                 A.O.R. FEE PCT                               
SRTRPCT  DS    PL8                 CREATIVE FEE RECEIVABLE PCT                  
SBUKCONT EQU   (*-SRTBUKS)/SRTBUKLN NUMBER OF BUCKETS                           
SRTLNQ   EQU   *-SRTKEY            RECORD LENGTH                                
*                                                                               
         EJECT                                                                  
*****************************************************************               
* DSECTS TO COVER BINARY TABLES                                 *               
*****************************************************************               
*                                                                               
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISPK DS    0F                                                               
BINDISP  DS    CL1                 DISPLACEMENT                                 
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAX NUMBER IN TABLE                          
BINLENQ  EQU   *-BIND                                                           
BINTABLE DS    0CL1                                                             
*                                                                               
CODD     DSECT                     * DSECT FOR SR A.O.R. ACCOUNT/NAME*          
CODKEY   DS    CL15                SR ACCOUNT CODE (AT EACH LEVEL)              
CODNAME  DS    CL36                NAME (AT EACH LEVEL)                         
CODLEN   EQU   *-CODKEY                                                         
*                                                                               
CLTD     DSECT                     * DSECT FOR CLIENT TABLE *                   
CLTKEY   DS    CL(L'CODKEY)        CLIENT OR CLIPROD CODE                       
CLTNAME  DS    CL(L'CODNAME)       NAME                                         
CLTOFF   DS    CL2                 OFFICE CODE                                  
CLTLEN   EQU   *-CLTKEY                                                         
*                                                                               
MEDD     DSECT                     * DSECT FOR MEDIA ACCOUNT/NAME*              
MEDKEY   DS    CL(L'CODKEY)        SI ACCOUNT CODE (AT EACH LEVEL)              
MEDNAM   DS    CL(L'CODNAME)       NAME (AT EACH LEVEL)                         
MEDLEN   EQU   *-MEDKEY                                                         
*                                                                               
ESTD     DSECT                     * DSECT FOR VALID ESTIMATES*                 
ESTKEY   DS    0CL(ESTLEN)                                                      
ESTCUL   DS    CL(L'ACINCUL)       COMPANY/UNIT/LEDGER                          
ESTACC   DS    CL(L'ACINACC)       SR ACCOUNT CODE                              
ESTCLT   DS    CL(L'ACINCLT)       MEDIA                                        
ESTPRD   DS    CL(L'ACINPRD)       MEDIA                                        
ESTEST   DS    CL(L'ACINEST)       ESTIMATE                                     
ESTLEN   EQU   *-ESTCUL                                                         
*                                                                               
NARRD    DSECT                     TRANSACTION NARRATIVE CHECK DSECT            
NARRCK   DS    CL12                "CHECK NUMBER" (TITLE)                       
         DS    CL1                 SPACE                                        
NARCKNUM DS    CL6                 ACTUAL CHECK NUMBER (MAX OF 6)               
         ORG   NARRCK                                                           
NARDATED DS    CL5                 "DATED" (TITLE)                              
         DS    CL1                 SPACE                                        
NARCKDAT DS    CL8                 CHECK DATE MMMDD/YY                          
         ORG   NARRCK                                                           
NARDEPON DS    CL12                "DEPOSITED ON" (TITLE)                       
         DS    CL1                 SPACE                                        
NARDPDAT DS    CL8                 DEPOSIT DATE MMMDD/YY                        
         EJECT                                                                  
* DSECT TO COVER DOWNLOAD BLOCK                                                 
*                                                                               
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*********************************************************                       
*        EQUATES                                        *                       
*********************************************************                       
*                                                                               
AORORDER EQU   1                   A.O.R. ORDER REPORT                          
CLTORDER EQU   2                   CLIENT ORDER                                 
MEDORDER EQU   3                   MEDIA ORDER                                  
ESTORDER EQU   4                   ESTIMATE SUMMARY REPORT                      
*                                                                               
BYAOR    EQU   X'80'               REPORT BY A.O.R                              
BYCLIENT EQU   X'40'               REPORT BY CLT/PRD                            
BYMEDIA  EQU   X'20'               REPORT BY MEDIA                              
ESTSUMRE EQU   X'10'               REPORT BY ESTIMATE SUMMARY                   
DOWNLOAD EQU   X'08'               DOWNLOAD REPORT                              
*                                                                               
ADPERIOD EQU   X'80'               REQUEST PERIOD IS BY ADV MTHS                
GLPERIOD EQU   X'40'               REQUEST PERIOD IS BY G/L MTHS                
*                                                                               
CHEKDATE EQU   C'C'                CHECK DATE                                   
DEPOSDTE EQU   C'D'                DEPOSIT DATE                                 
ESTIMRCV EQU   C'E'                ESTIMATED RECEIVABLE                         
POSTRCV  EQU   C'P'                POSTED RECEIVABLE                            
YES      EQU   C'Y'                YES                                          
NO       EQU   C'N'                NO                                           
TOP      EQU   C'T'                TOP OF BOX                                   
MIDLINE  EQU   C'M'                MIDLINE OF BOX                               
BOTTOM   EQU   C'B'                BOTTOM OF BOX                                
LEFTMAR  EQU   C'L'                LEFT MARGIN OF BOX                           
RIGHTMAR EQU   C'R'                RIGHT MARGIN OF BOX                          
COLUMN   EQU   C'C'                COLUMN OF BOX                                
FOX      EQU   X'FF'               X'FF'                                        
*                                                                               
SOON     EQU   X'20'               SOON REPORT REQUESTED                        
OVERNITE EQU   X'10'               REPORT REQUESTED OVERNIGHT                   
DDSTSO   EQU   X'08'               REPORT REQUESTED BY DDS THRU TSO             
*                                                                               
*                                  **BINARY TABLE EQUATES**                     
CODMAX   EQU   1000                                                             
CODSIZE  EQU   BINLENQ+(CODMAX*CODLEN)                                          
CLTMAX   EQU   1000                                                             
CLTSIZE  EQU   BINLENQ+(CLTMAX*CLTLEN)                                          
MEDMAX   EQU   100                                                              
MEDSIZE  EQU   BINLENQ+(MEDMAX*MEDLEN)                                          
ESTMAX   EQU   15000                                                            
ESTSIZE  EQU   BINLENQ+(ESTMAX*ESTLEN)                                          
*                                                                               
*                                  LENGTH OF ACQUIRED BUFFER                    
*                                  (100 EXTRA BYTES FOR SAFETY)                 
LENBUFF  EQU   CODSIZE+CLTSIZE+MEDSIZE+ESTSIZE+(2*SRTLNQ)+100                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACINT17   02/12/04'                                      
         END                                                                    
