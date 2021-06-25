*          DATA SET ACINT16    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T61916A,*                                                                
*INCLUDE CONVMOS                                                                
*INCLUDE COVAIL                                                                 
         TITLE 'T61916 - ESTIMATE SUMMARY DELETE REPORT'                        
T61916   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61916,RA,RR=R2                                                
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
         BNE   MODE10                                                           
         BAS   RE,VKEY             VALIDATE KEY                                 
         B     XIT                                                              
*                                                                               
MODE10   CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
*                                                                               
         BAS   RE,PEST             READ ESTIMATE RECORDS                        
         BAS   RE,PDET             READ TRANSACTIONS                            
         BAS   RE,REPORT           DO REPORT                                    
         BAS   RE,WRAP             WRAP-UP AFTER REPORT                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO VALIDATE KEY FIELDS                                 *          
**********************************************************************          
         SPACE 1                                                                
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
         LA    R2,DELRCVH                                                       
         GOTO1 SETHEIR                                                          
         MVC   SRLEVELA,LLEVA                                                   
         MVC   SRLEVELB,LLEVAB                                                  
         MVC   SRLEVELC,LLEVABC                                                 
         MVC   SRLEVELD,LLEVABCD                                                
*                                                                               
         CLI   DELRCVH+5,0         WAS ONE INPUT?                               
         BE    VKEY10                                                           
*                                  YES                                          
         MVI   OPTION2,YES         ALLOW INPUT OF HIGH LEVEL ACCOUNT            
         LA    R2,DELRCVH                                                       
         GOTO1 VALACCT                                                          
         MVC   QACCOUNT+L'CUL(L'QACCOUNT-L'CUL),RECCODE                         
*                                                                               
*                                  VALIDATE SJ CLIENT ACCOUNT                   
*                                                                               
VKEY10   LA    R2,DELCLTH          CLIENT IS REQUIRED                           
         MVI   ERROR,INVALID                                                    
         CLI   DELCLTH+5,1         CLIENT MUST BE AT LEAST 2 CHARS              
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
VKEY20   CLI   DELPRDH+5,0         WAS ONE INPUT?                               
         BE    VKEY30              NO - SKIP EDIT                               
         MVI   ERROR,MISSING                                                    
         CLI   DELCLTH+5,0         YES - THEN A CLIENT MUST ALSO BE             
         BNE   *+12                                                             
         LA    R2,DELCLTH                                                       
         B     ERREND                                                           
         LA    R2,DELPRDH          PRODUCT                                      
         MVI   ERROR,INVALID                                                    
         CLI   DELPRDH+5,1         MUST BE AT LEAST 2 CHARS                     
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
VKEY30   CLI   DELMEDH+5,0         WAS ONE INPUT?                               
         BE    VKEY40              NO - SKIP EDIT                               
         LA    R2,DELMEDH          MEDIA                                        
         GOTO1 VALMED              VALIDATE MEDIA                               
         CLC   =C'MI=',DELMED      MI=?                                         
         BNE   *+10                                                             
         MVC   QMEDMI,DELMED                                                    
         MVC   QMEDCOD,MEDIA       SAVE THE TWO DIGIT MEDIA                     
         MVC   QMEDIA,MEDIACOD     SAVE INCOME ACCOUNT                          
         LA    R2,DELMEDNH         MEDIA DESCRIPTION FIELD                      
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         MVC   DELMEDN,MEDNAME     SAVE INCOME ACCOUNT                          
*                                                                               
*                                  VALIDATE ESTIMATE NUMBER                     
*                                                                               
VKEY40   CLI   DELESTH+5,0         WAS ONE INPUT?                               
         BE    VKEY50              NO - SKIP EDIT                               
         LA    R2,DELESTH          ESTIMATE NUMBER                              
         GOTO1 VALEST                                                           
         MVC   QESTIM,ESTIMATE     SAVE ESTIMATE NUMBER                         
*                                                                               
VKEY50   LA    R2,DELPERH          ATLEAST END PERIOD REQUIRED                  
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         GOTO1 VALPERE                                                          
         MVC   QESTART,PERIODS                                                  
         MVC   QEEND,PERIODE       SET END TO HIGHEST VALUE                     
*                                                                               
*                                  VALIDATE REPORT TYPE OPTIONS                 
VKEY60   DS    0H                                                               
         LA    R2,DELOPTH          OPTION                                       
         OI    6(R2),X'80'         SET TRANSMIT                                 
         MVC   QOPT,=C'D'          ASSUME DRAFT                                 
         CLI   5(R2),0                                                          
         BE    VKEY70                                                           
         CLI   8(R2),C'D'                                                       
         BE    VKEY70                                                           
         MVI   ERROR,INVOPT                                                     
         CLI   8(R2),C'L'                                                       
         BNE   ERREND              NOT DRAFT NOT LIVE IT IS AN ERROR            
         MVC   QOPT,8(R2)                                                       
*                                                                               
VKEY70   MVC   8(1,R2),QOPT        DISPLAY OPTION                               
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'         SET TRANSMIT                                 
*                                                                               
         MVC   CUL+1(2),RECVLEDG   RESTORE SR U/L                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VKEYX                                                            
         TM    WHEN,X'20'          TEST FOR 'SOON' REQUEST                      
         BZ    VKEYX               NO                                           
         MVI   ERROR,DRFTONLY      YES                                          
         CLI   QOPT,C'D'           MUST BE DRAFT FOR SOON                       
         BNE   ERREND                                                           
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              PUT ESTIMATES RECORDS TO SORT                        *           
*********************************************************************           
         SPACE 3                                                                
PEST     NTR1                                                                   
         BAS   RE,INITREP              INITIALIZE OUR BUFFER                    
         OC    QESTART,QESTART         IS THERE A START DATE                    
         BNZ   PEST10                  NO-CONTINUE                              
         MVC   PERIODS,FOXES                                                    
         XC    PERIODE,PERIODE                                                  
*                                                                               
PEST10   MVC   KKEY,SPACES             SET KEY FOR X'2D03' EST RECS             
         LA    R6,KKEY                                                          
         USING INTRECD,R6                                                       
         MVI   INTKTYP,INTKTYPQ        X'2D'                                    
         MVI   INTKSUB,INTKSUBQ        X'03'                                    
         MVC   INTKCPY(L'COMPANY),COMPANY                                       
         MVC   INTKUNT(2),RECVLEDG     UNIT/LEDGER                              
*                                                                               
         CLC   QACCOUNT,SPACES         IS THERE AN ACCOUNT                      
         BE    PEST20                  NO - GO READ HIGH                        
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
         MVC   INTKCULA,QACCOUNT                                                
         CLC   QCLIPROD,SPACES                  CLT OR CLT/PROD INPUT?          
         BE    PEST20                           NO-INITIAL KEY FINISHED         
         MVC   INTKCLT(L'QCLIPROD),QCLIPROD                                     
         CLC   QMEDMIE,SPACES                   MEDIA INPUT?                    
         BE    PEST20                           NO-INITIAL KEY FINISHED         
*                                                                               
         MVC   INTKMED,QMEDCOD                                                  
         MVC   INTKEST,QESTIM                   MOVE EST# OR BLANKS             
*                                                                               
PEST20   DS    0H                                                               
         BAS   RE,RDHIGH                                                        
         B     PEST40                                                           
*                                                                               
PEST30   BAS   RE,RDSEQL                                                        
PEST40   CLC   SAVKKEY(L'INTKTYP+L'INTKSUB+L'INTKCUL),KKEY                      
         BNE   PEST130                                                          
         L     R6,AIO                                                           
         CLC   QREQUEST,SPACES      WERE THERE KEY FILTERS?                     
         BE    PEST80               NO- GO TEST FOR EST PERIOD                  
*                                                                               
         CLC   QACCOUNT,SPACES      IS ACCOUNT REQUESTED?                       
         BE    PEST50               NO-CHECK FOR CLIENT FILTERING               
         ZIC   R1,LEN                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   INTKCUL(0),QACCOUNT  DO WE WANT THIS ACCOUNT?                    
         BNE   PEST130              NO - WERE DONE WITH ESTIMATES               
*                                                                               
PEST50   CLC   QCLIPROD,SPACES      FILTERING BY CLIENT/PRODUCT?                
         BE    PEST60               NO - GO CHECK FOR MEDIA                     
         CLC   INTKCLT,QCLI         CLIENT MATCH?                               
         BNE   PEST30               NO - READ NEXT RECORD                       
         CLC   QPROD,SPACES         FILTERING BY PRODUCT?                       
         BE    PEST60               NO - GO CHECK FOR MEDIA                     
         CLC   INTKPRD,QPROD        PRODUCT MATCH?                              
         BNE   PEST30               NO - READ NEXT RECORD                       
PEST60   CLC   QMEDCOD,SPACES       FILTERING BY MEDIA?                         
         BE    PEST70               NO - GO CHECK EST NUMBER                    
         CLC   INTKMED,QMEDCOD                                                  
         BNE   PEST30               NO - READ NEXT RECORD                       
PEST70   CLC   QESTIM,SPACES        FILTERING BY ESTIMATE NUMBER?               
         BE    PEST80               NO - GO CHECK EST DATES                     
         CLC   INTKEST,QESTIM                                                   
         BNE   PEST30               NO - READ NEXT RECORD                       
*                                                                               
PEST80   MVI   ELCODE,IPRELQ        X'C6' ELEMENT                               
         BAS   RE,GETELIO           GET PROFILE ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING IPRELD,R6                                                        
         CLC   IPREND,QESTART      EST PERIOD END LOWER THAN REQ START?         
         BL    PEST30              YES - THEN GET NEXT RECORD                   
         CLC   IPRSTART,QEEND      EST START HIGHER THAN REQ END?               
         BH    PEST30              YES - THEN GET NEXT RECORD                   
*                                                                               
*                                  ** IF START DATE NOT INPUT TAKE **           
*                                  ** IT FROM THE OLDEST ESTIMATE  **           
*                                  ** THAT FITS THE REQUEST DETAILS**           
*                                  ** THIS IS FOR HEADLINES ONLY   **           
*                                                                               
         OC    QESTART,QESTART     IS START DATE SET TO LOW VALUES              
         BNZ   PEST90              NO - CONTINUE                                
         CLC   PERIODS,IPRSTART    COMPARE THIS ESTIMATES START TO OURS         
         BL    *+10                TAKE THE LOWER OF THE TWO                    
         MVC   PERIODS,IPRSTART                                                 
         CLC   PERIODE,IPREND      TAKE THE HIGEST END DATE FOR HEADS           
         BH    PEST90                                                           
         MVC   PERIODE,IPREND                                                   
*                                                                               
PEST90   DS    0H                                                               
*                                                                               
         MVI   ELCODE,IESELQ       X'C7' ELEMENT                                
         BAS   RE,GETELIO          GET RECEIVABLE ESTIMATE ELEM                 
         BE    *+6                                                              
         DC    H'0'                                                             
         USING IESELD,R6                                                        
PEST100  DS     0H                                                              
         TM    IESSTAT,IESSPOST    REC'V POSTED                                 
         BNZ   PEST110             YES                                          
         OC    IESDAT,IESDAT       NO, IS THERE A POSTING DATE                  
         BNZ   PEST30              YES, GO GET NEXT RECORD                      
*                                                                               
PEST110  DS     0H                                                              
         CLC   IESMTH,QESTART      CHECK IF IN RANGE                            
         BL    PEST30              NO - GET NEXT X'C7'                          
         CLC   IESMTH,QEEND                                                     
         BH    PEST30                                                           
*                                                                               
         CP    IESREC,IESPD        IS PAID AMOUNT = RECIEVABLE AMOUNT           
         BNE   PEST30              NO, GET NEXT RECORD                          
*                                                                               
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),IESELQ                                                     
         BE    PEST100             GET NEXT X'C7' EL                            
*                                                                               
         USING INTRECD,R6          RESET R6                                     
         L     R6,AIO              GOOD TO DELETE, PUT TO BUFF NOW              
*                                                                               
         L     R5,ASORTREC          R5 COVERS SORT RECORD                       
         L     R4,AIO               SAVE ADDR OF ESTIMATE RECORD                
         USING SRTD,R5                                                          
         BAS   RE,CLEARSRT                                                      
         MVI   SRTBUFTP,ESTORDER                                                
         MVC   SRTRCUL,INTKCUL               C/U/L                              
         MVC   SRTACCT,INTKACT               AOR AGENCY                         
         MVC   SRTCLT,INTKCLT                CLIENT                             
         MVC   SRTPRD,INTKPRD                PRODUCT                            
         MVC   SRTMDUL,=C'SI'                DEFAULT IS INCOME ACCOUNT          
         MVC   SRTMACC(L'INTKMED),INTKMED    MEDIA                              
         TM    INTRSTA-INTRECD(R4),INTSMI    MEDIA IS MEDIA INTERFACE           
         BZ    *+10                                                             
         MVC   SRTMDUL,=C'**'                TAKEN FROM MEDIA RECORD            
         MVC   SRTESTIM,INTKEST              ESTIMATE NUMBER                    
         MVI   SRTTYPE,SRTEST                                                   
*                                                                               
         MVI   ELCODE,IPRELQ        X'C6' ELEMENT                               
         BAS   RE,GETELIO           GET PROFILE ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING IPRELD,R6                                                        
         MVC   SRTDESC,IPRDES      ESTIMATE DESCRIPTION                         
         MVC   SRTPRS,IPRSTART     YYMM START EST PERIOD                        
         MVC   SRTPRE,IPREND       YYMM END EST PERIOD                          
*                                                                               
         MVI   ELCODE,IESELQ       X'C7' ELEMENT                                
         BAS   RE,GETELIO          GET RECEIVABLE ESTIMATE ELEM                 
         BE    *+6                                                              
         DC    H'0'                                                             
         USING IESELD,R6                                                        
*                                                                               
PEST120  DS    0H                                                               
         MVC   SRTADMTH,IESMTH     YYMM ADVERTISING MONTH                       
         GOTO1 DATCON,DMCB,(2,IESDTO),(1,SRTDATE)                               
         MVC   SRTRDATE,SRTDATE                                                 
         MVC   SRTGLMTH,SRTDATE    G/L MONTH (ACCOUNTING MOS)                   
         ZAP   SRTAPCT,IESFEE      AOR FEE PCT                                  
         ZAP   SRTRPCT,IESRCV      CREATIVE FEE PCT                             
         ZAP   SRTGROSS,IESGRS     GROSS BILLING EST                            
         ZAP   SRTRCV,IESREC       RECEIVABLE AMOUNT EST                        
         ZAP   SRTPOST,=P'0'       CLEAR POSTED                                 
         TM    IESSTAT,IESSPOST    ESTIMATE POSTED?                             
         BZ    *+10                NO SKIP                                      
         ZAP   SRTPOST,IESREC      POSTED                                       
         ZAP   SRTPAID,IESPD       PAID SO FAR                                  
*                                                                               
         BAS   RE,ACCADD           ADD ACCOUNT/LEVELS TO TABLE                  
         BAS   RE,CLIADD           ADD CLIENT/PRODUCT TO TABLE                  
         BAS   RE,MEDADD           ADD MEDIA/NAMES TO TABLE                     
*                                                                               
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
         MVI   ALSORT,1                  ACTIVITY SWITCH                        
*                                                                               
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),IESELQ                                                     
         BE    PEST120                   GET NEXT X'C7' EL                      
*                                                                               
*                                  IF YOU ARE HERE IT'S GOOD TO DELET           
         CLI   QOPT,C'L'           ARE WE RUNNING LIVE?                         
         BNE   PEST30              NO, DONE                                     
         L     R6,AIO              YES, MARK IT FOR DELETION                    
         USING ACTRECD,R6                                                       
         OI    ACTRSTAT,ACTSDELT   TURN ON DELETE BIT                           
         GOTO1 WRITE                                                            
         B     PEST30                    GET NEXT RECORD                        
*                                                                               
*                                        *LOOKUP CLIENT/PRODUCT NAME*           
PEST130  L     R5,ACLTLIST               ADDR OF CLIENT TABLE                   
         USING BIND,R5                                                          
         OC    BININ,BININ               IS TABLE EMPTY?                        
         BZ    PEST160                                                          
         MVC   AIO,AIO2                  USE AIO2                               
         LA    R2,BINTABLE               ADDR TABLE IN R2                       
         L     R4,BININ                  NUMBER IN TABLE IN R4                  
         USING CLTD,R2                                                          
PEST140  MVC   KEY,SPACES                                                       
         MVC   KEY(L'CLTKEY),CLTKEY                                             
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(42),KEY                                                  
         BE    *+14                                                             
         MVC   CLTNAME(23),=C'***ACCOUNT NOT FOUND***'                          
         B     PEST150                                                          
*                                                                               
         LA    R3,CLTNAME                NAMEIT PUT NAME IN ADDR IN R3          
         MVC   0(L'CLTNAME,R3),SPACES                                           
         GOTO1 NAMEIT                                                           
         MVI   ELCODE,PPRELQ             GET PROFILE X'24' ELEMENT              
         BAS   RE,GETELIO                FOR THE OFFICE CODE                    
         BNE   PEST150                                                          
         USING PPRELD,R6                                                        
         MVC   CLTOFF,SPACES                                                    
         CLC   PPRGAOFF,SPACES                                                  
         BNH   *+10                                                             
         MVC   CLTOFF,PPRGAOFF                                                  
PEST150  LA    R2,CLTLEN(R2)             NEXT TABLE ENTRY                       
         BCT   R4,PEST140                                                       
*                                                                               
         BAS   RE,MEDLOOK                LOOKUP MEDIA NAMES                     
         MVC   AIO,AIO1                  RESET AIO                              
PEST160  XC    WORK,WORK                 SETUP HEADLINE DATES                   
         MVC   WORK(L'PERIODS),PERIODS                                          
         MVC   WORK+3(L'PERIODE),PERIODE                                        
         GOTO1 DATCON,DMCB,(X'11',WORK),(6,PERRANGE)                            
         B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
*********************************************************************           
*              READ SR TRANS AND PUT INTERAGENCY TRANS TO SORT      *           
*********************************************************************           
         SPACE 3                                                                
PDET     NTR1                                                                   
         MVC   QESTART,PERIODS                                                  
         MVC   QEEND,PERIODE                                                    
         L     R5,ACODLIST          TABLE OF SR ACCOUNTS TO READ                
         USING BIND,R5                                                          
         OC    BININ,BININ          ANY ACCOUNTS IN TABLE?                      
         BZ    PDET50               NO - NOTHING FOR REQUEST                    
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   WORK,SPACES          YES - MARK END OF TABLE                     
         MVC   WORK(L'CODKEY),FOXES                                             
         GOTO1 BINADD,DMCB,WORK,ACODLIST                                        
         SPACE 3                                                                
*                                   ****READING ACCOUNTS FROM TABLE****         
*                                                                               
         LA    R1,BINTABLE          TABLE OFF OF R1                             
PDET10   ST    R1,AMYPLACE           SAVE ADDR OF MY PLACE IN THE TABLE         
         MVC   SRACCONT,SPACES                                                  
         MVC   SRACCONT,0(R1)        SR ACCOUNT FROM TABLE FOR SR READ          
         CLI   SRACCONT,FOX                                                     
         BE    PDET50                GO DO REPORT                               
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'SRACCONT),SRACCONT                                         
         GOTO1 HIGH                                                             
         B     PDET30                                                           
PDET20   GOTO1 SEQ                   READ NEXT REC FOR THIS SR ACCT             
PDET30   L     R5,AIO                                                           
         USING ACTRECD,R5                                                       
         CLC   ACTKCULA,SRACCONT                                                
         BE    PDET40                                                           
         L     R1,AMYPLACE           ADDR OF MY PLACE IN THE TABLE              
         LA    R1,CODLEN(R1)         NEXT ACCOUNT IN THE TABLE                  
         B     PDET10                                                           
*                                                                               
PDET40   DS    0H                                                               
         BAS   RE,GETADDR            GET ADDRESS OF NAME ELEMENT                
         OC    ANAMEL,ANAMEL         DO WE HAVE A NAME ELEMENT                  
         BZ    PDET20                                                           
         L     R6,ANAMEL             NAME ELEMENT                               
         USING NAMELD,R6                                                        
         L     R1,AMYPLACE                                                      
         ZIC   R2,NAMLN                                                         
         SHI   R2,NAMLN1Q+1        SUBTRACT OVEREHEAD + 1 FOR EX                
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   CODNAME-CODD(0,R1),NAMEREC                                       
         B     PDET20                READ NEXT RECORD                           
PDET50   MVC   AIO,AIO1               RESTORE AIO                               
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
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
*                                                                               
         USING SRTD,R5                                                          
         L     R5,ASORTREC            ADDR OF SORT WORK AREA                    
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
         B     REPT20                                                           
*                                                                               
REPT10   DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',(0,ABUFFA),(R5),0                           
*                                                                               
REPT20   TM    DMCB+8,X'80'                                                     
         BO    XIT                    END OF SORTED RECORDS                     
         L     R1,DMCB+8                                                        
         ST    R1,ALSORT              SAVE ADDR OF LAST SORT                    
*                                                                               
         MVC   XP,XSPACES             CLEAR PRINT LINES                         
         MVC   XP2,XSPACES                                                      
*                                                                               
         MVI   RCSUBPRG,ESTORDER      DEFAULT IS ESTIMATE SUMMARY               
         MVC   CURRRACT,SRTRECV                                                 
         MVC   CURCLPRD,SRTCLPD                                                 
         MVC   CURRMED,SRTMED                                                   
*                                                                               
         MVI   TOTALSW,NO             SET TOTAL RECORD SWITCH TO NO             
         BAS   RE,TOTAL               CHECK FOR BREAKS                          
*                                                                               
         CLI   TOTALSW,YES            IF TOTAL SKIP REFRESHING NAMES            
         BE    *+8                                                              
         BAS   RE,REFRESH             REFRESH NAMES                             
         MVI   FIRSTIME,NO                                                      
         USING PRTD,R4                                                          
         LA    R4,XP                                                            
*                                                                               
         CLC   TRKTOTL,SPACES         TOTAL? GO PRINT IT NOW                    
         BE    *+12                                                             
         MVI   SPACING,2                                                        
         B     REPT30                                                           
*                                                                               
         MVC   TRKENUM,SRTESTIM                                                 
         MVC   TRKDESC,SRTDESC                                                  
*                                                                               
         MVC   TRKMEDIA,CURRMED       MEDIA TO PRINT LINE                       
         XC    WORK,WORK                                                        
         MVC   WORK(L'SRTADMTH),SRTADMTH            ADV MTH                     
         MVC   WORK+3(L'SRTGLMTH),SRTGLMTH          G/L MTH                     
         GOTO1 DATCON,DMCB,(1,WORK),(6,TRKADVM)                                 
         GOTO1 (RF),(R1),(1,WORK+3),(6,TRKGLMTH)                                
         GOTO1 (RF),(R1),(1,SRTRDATE),(5,TRKDAT1)                               
*                                                                               
REPT30   DS    0H                                                               
         CURED SRTGROSS,(L'TRKGROS,TRKGROS),2,MINUS=YES                         
         CURED SRTPOST,(L'TRKRECV,TRKRECV),2,MINUS=YES                          
*                                                                               
         CP    SRTRCV,SRTPOST          IF ESTIMATED RCV AND POSTED              
         BE    REPT40                  ARE THE SAME,ONLY PRINT ONE              
*                                                                               
         MVI   TRKRVTYP,POSTRCV                  POSTED (P)                     
         LA    R3,XP2                                                           
         MVI   TRKRVTYP-PRTD(R3),ESTIMRCV        ESTIMATED (E)                  
         CURED SRTRCV,(L'TRKRECV,TRKRECV-PRTD(R3)),2,MINUS=YES                  
*                                                                               
REPT40   CLI   TOTALSW,YES                                                      
         BE    REPT50                  PCT TOTALS DONT MAKE SENSE               
         CURED SRTAPCT,(L'TRKAORPC,TRKAORPC),2,MINUS=YES                        
         CURED SRTRPCT,(L'TRKCREPC,TRKCREPC),2,MINUS=YES                        
*                                                                               
REPT50   ZAP   MYDUBL,SRTPOST                                                   
         SP    MYDUBL,SRTPAID                                                   
         CURED MYDUBL,(L'TRKVARI,TRKVARI),2,MINUS=YES                           
*                                                                               
         CURED SRTPAID,(L'TRKPAID,TRKPAID),2,MINUS=YES                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     REPT10                                                           
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
         BE    REFR40                                                           
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
REFR10   CLI   0(R3),0                IF LENGTH IS 0 WE'RE THROUGH              
         BE    REFR30                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'SRTRCUL),CURRRCOM                                          
         ZIC   R1,0(R3)               LENGTH OF LEVEL IN R1                     
         BCTR  R1,0                   REDUCE FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SAVERACC(0),CURRRACC   DO WE HAVE THIS LEVEL SAVED?              
         BE    REFR20                 YES SKIP TO NEXT LEVEL                    
         EX    R1,*+8                 NO - GO GET NAME FROM TABLE               
         B     *+10                                                             
         MVC   KEY+L'SRTRCUL(0),CURRRACC                                        
         GOTO1 NAMLOOK,DMCB,KEY,ACODLIST,(R2)                                   
         MVC   SAVE4NAM,0(R2)         SAVE4NAM WILL HAVE POST LEVEL NAM         
*                                                                               
REFR20   LA    R3,SRLEVLEN(R3)        NEXT LEDGER LEVEL LENGTH                  
         LA    R2,SAVNAMLN(R2)        NEXT SAVE NAME AREA                       
         BCT   R4,REFR10                                                        
REFR30   MVC   SAVERACT,CURRRACT      SAVE THE NEW RECEIVABLE ACCOUNT           
*                                                                               
REFR40   DS    0H                                                               
         CLC   CURCLPRD,SAVCLPRD      SAME CLIENT AND PRODUCT CODE              
         BE    REFR60                 YES - WE MUST HAVE CURRENT NAME           
         CLC   SAVCLPRD,SPACES                                                  
         BE    *+8                                                              
         MVI   FORCEHED,YES                                                     
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'COMPANY),COMPANY                                           
         MVC   KEY+L'COMPANY(L'PRODLEDG),PRODLEDG                               
         CLC   CURRCLT,SAVECLT         SAME CLIENT ?                            
         BE    REFR50                  YES - LOOKUP PRODUCT NAME                
*                                                                               
         MVC   KEY+L'COMPANY+L'PRODLEDG(L'CURRCLT),CURRCLT                      
         GOTO1 NAMLOOK,DMCB,KEY,ACLTLIST,SAVECNAM                               
*                                                                               
REFR50   MVC  KEY+L'COMPANY+L'PRODLEDG(L'CURCLPRD),CURCLPRD                     
         GOTO1 NAMLOOK,DMCB,KEY,ACLTLIST,SAVEPNAM                               
         MVC   SAVCLPRD,CURCLPRD       SAVE THE NEW CLIENT PRODUCT              
*                                                                               
REFR60   DS    0H                                                               
         CLC   CURRMED,SAVEMED         SAME MEDIA CODE                          
         BE    REFR70                  YES - WE MUST HAVE CURRENT NAME          
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'COMPANY),COMPANY                                           
         MVC   KEY+L'COMPANY(L'CURRMED),CURRMED                                 
         GOTO1 NAMLOOK,DMCB,KEY,AMEDLIST,SAVEMNAM                               
         MVC   SAVEMED,CURRMED         SAVE MEDIA CODE                          
*                                                                               
REFR70   DS    0H                                                               
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
         CLI   CURRRACT,FOX                SIGNFIES REP TOT                     
         BE    TOT10                                                            
         CLI   CURRCLT,FOX                 SIGNIFIES AOR TOT                    
         BE    TOT20                                                            
         CLI   CURRPRD,FOX                 SIGNIFIES CLIENT TOT                 
         BE    TOT30                                                            
         CLI   CURRMDAC,FOX                SIGNIFIES PRODUCT TOT                
         BE    TOT40                                                            
         CLI   SRTESTIM,FOX                SIGNIFIES MEDIA TOT                  
         BE    TOT50                                                            
         B     TOT60                                                            
*                                                                               
TOT10    MVC   WORK(L'REQUESDC),REQUESDC                                        
         B     TOT80                                                            
*                                                                               
*                                          ***AOR TOTAL***                      
TOT20    MVC   WORK(L'AORDC),AORDC                                              
         MVC   WORK+L'AORDC+1(L'SAVERACC),SAVERACC                              
         MVC   WORK+L'AORDC+1+L'SAVERACC+1(L'SAVE4NAM),SAVE4NAM                 
         B     TOT80                                                            
*                                           ***CLT TOTAL***                     
TOT30    MVC   WORK(L'CLIENTDC),CLIENTDC                                        
         MVC   WORK+L'CLIENTDC+1(L'SAVECLT),SAVECLT                             
         MVC   WORK+L'CLIENTDC+1+L'SAVECLT+1(L'SAVECNAM),SAVECNAM               
         B     TOT80                                                            
*                                           ***PRD TOTAL***                     
TOT40    MVC   WORK(L'PRODDC),PRODDC                                            
         MVC   WORK+L'PRODDC+1(L'SAVEPRD),SAVEPRD                               
         MVC   WORK+L'PRODDC+1+L'SAVEPRD+1(L'SAVEPNAM),SAVEPNAM                 
         B     TOT80                                                            
*                                                                               
*                                           ***MED TOTAL***                     
TOT50    MVC   WORK(L'MEDIADC),MEDIADC                                          
         MVC   WORK+L'MEDIADC+1(L'SAVEMED),SAVEMED                              
         MVC   WORK+L'MEDIADC+1+L'SAVEMED+1(L'SAVEMNAM),SAVEMNAM                
         B     TOT80                                                            
*                                                                               
TOT60    CLI   SRTADMTH,FOX                 ** EST NUMBER TOTAL **              
         BNE   TOT70                                                            
         MVC   WORK(L'ESTIMDC),ESTIMDC                                          
         MVC   WORK+L'ESTIMDC+1(L'SAVEEST),SAVEEST                              
         B     TOT80                                                            
*                                                                               
TOT70    CLI   SRTTYPE,FOX                  ** ADV MTH TOTAL **                 
         BNE   TOTX                                                             
         MVC   WORK(L'ADVERTDC),ADVERTDC                                        
         MVC   WORK+L'ADVERTDC+1(L'SAVEMTH),SAVEMTH                             
*                                                                               
TOT80    MVI   TOTALSW,YES                                                      
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)              SKIP A LINE                         
*                                                                               
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         GOTO1 CHOPPER,DMCB,(L'WORK,WORK),(L'TRKTOTL,TRKTOTL),(L'XP,2)          
*                                                                               
TOTX     B     XIT                                                              
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
         BNE   ACCADD10             SOMETHINGS WRONG                            
         DC    H'0'                                                             
*                                                                               
ACCADD10 CLI   0(R3),0              IS LENGTH OF LEVEL 0                        
         BE    ACCADDX              YES - LEVEL NOT USED --WE'RE DONE           
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
         BCT   R4,ACCADD10                                                      
*                                                                               
ACCADDX  B     XIT                                                              
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
         MVI   ELCODE,MDIELQ             X'19' ELEMENT                          
         BAS   RE,GETELIO                GET MEDIA ELEMENT                      
         BNE   MEDLOOK9                                                         
         USING MDIELD,R6                                                        
         MVC   MEDNAM(L'MDIDESC),MDIDESC                                        
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
         BAS   RE,CLIADD10                                                      
*                                                                               
         MVC   WORK,SPACES                  THEN CLIENT PRODUCT                 
         MVC   WORK(L'COMPANY),COMPANY                                          
         MVC   WORK+L'COMPANY(L'PRODLEDG),PRODLEDG                              
         MVC   WORK+L'COMPANY+L'PRODLEDG(L'SRTCLPD),SRTCLPD                     
         BAS   RE,CLIADD10                                                      
         B     XIT                                                              
         SPACE 3                                                                
CLIADD10 ST    RE,SAVERE                                                        
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
*                                                                               
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    HOOKX                                                            
         L     R2,AWIDE                                                         
         USING WIDED,R2                                                         
*                                                                               
         CLI   QOPT,C'D'           IS THIS A DRAFT?                             
         BNE   HOOK1               NO                                           
         MVC   XHEAD3+59(24),=C'******* D R A F T ******'                       
         GOTO1 CENTER,DMCB,XHEAD3+59,24                                         
*                                                                               
HOOK1    DS    0H                                                               
         MVI   BOXYORN,YES                                                      
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
         MVC   XHEAD3+12(L'SAVERACC),SAVERACC       MUST BE AOR OR              
         MVC   XHEAD3+12+L'SAVERACC(30),SAVE4NAM    ESTIMATE REPORT             
         MVC   XHEAD4+12(L'SAVECLT),SAVECLT                                     
         MVC   XHEAD4+12+L'SAVERACC(30),SAVECNAM                                
         MVC   XHEAD5+12(L'SAVEPRD),SAVEPRD                                     
         MVC   XHEAD5+12+L'SAVERACC(30),SAVEPNAM                                
         MVC   XHEAD4+138(L'PERRANGE),PERRANGE  ADV. PERIOD                     
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
         XC    ANAMEL,ANAMEL       CLEAR ALL ELEMENT ADDRESSES                  
*                                                                               
GETADR10 CLI   0(R6),0             TEST FOR EOR                                 
         BE    GETADDRX                                                         
*                                                                               
         CLI   0(R6),NAMELQ        X'20'  NAME ELEMENT                          
         BNE   *+8                                                              
         ST    R6,ANAMEL           SAVE ADDRESS OF NAME ELEMENT                 
*                                                                               
         IC    R0,1(R6)            GET LENGTH OF CURRENT ELEM                   
         AR    R6,R0               BUMP TO NEXT ELEMENT                         
         B     GETADR10                                                         
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
DMRDHI   DC    C'DMRDHI  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
GETRECD  DC    C'GETREC  '                                                      
NEWO     DC    C'NEWO    '                                                      
ACCFIL   DC    C'ACCOUNT '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ACCARC   DC    C'ACCARC  '                                                      
*                                                                               
         EJECT                                                                  
*              SPECS FOR HEADINGS ETC                                           
         SPACE 3                                                                
MYSPECS  DS    0D                                                               
         RSPEC MAXLINES,51                                                      
*        LEFT HEADLINES                                                         
*                                                                               
         SPROG 4                                                                
         ASPEC H1,2,RUN,WIDE=198                                                
         ASPEC H3,2,C'A.O.R.:',WIDE=198                                         
         ASPEC H4,2,C'CLIENT:',WIDE=198                                         
         ASPEC H5,2,C'PRODUCT:',WIDE=198                                        
*                                                                               
*        RIGHT HEADLINES                                                        
*                                                                               
         ASPEC H1,126,C'REPORT ACE6',WIDE=198                                   
         ASPEC H1,138,PAGE,WIDE=198                                             
         ASPEC H3,126,REQUESTOR,WIDE=198                                        
         ASPEC H4,126,C'ADV. PERIOD',WIDE=198                                   
*        ASPEC H6,126,C'***ESTIMATE SUMMARY***',WIDE=198                        
*                                                                               
*        REPORT TITLE                                                           
*                                                                               
         ASPEC H1,60,C'INTERAGENCY PROFILE PEEL',WIDE=198                       
         ASPEC H2,60,24C'_',WIDE=198                                            
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
         ASPEC H8,60,C'POSTING',WIDE=198                                        
         ASPEC H9,60,C' DATE  ',WIDE=198                                        
*                                                                               
         ASPEC H8,70,C' G/L ',WIDE=198                                          
         ASPEC H9,70,C'MONTH',WIDE=198                                          
*                                                                               
         ASPEC H8,77,C'  A.O.R.  ',WIDE=198                                     
         ASPEC H9,77,C'  FEE %   ',WIDE=198                                     
*                                                                               
         ASPEC H8,88,C' RECV   ',WIDE=198                                       
         ASPEC H9,88,C'   %    ',WIDE=198                                       
*                                                                               
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
*                                                                               
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
       ++INCLUDE ACINTF6D                                                       
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
QOPT     DS    X                                                                
*                                       X'80' EST PERIOD IS ADV PERIOD          
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
*                                                                               
VTYPES   DS    0A                                                               
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
LOCALCLN EQU   *-LOCAL                                                          
*                                                                               
*        ADDRESSES OF BUFFERS                                                   
*                                                                               
ABUFF    DS    A                   A(ACQUIRED BUFFER)                           
ACODLIST DS    A                   A(A.O.R. SR ACCOUNTS)                        
ACLTLIST DS    A                   A(CLIENT/PRODUCT AND NAMES)                  
AMEDLIST DS    A                                                                
ASORTREC DS    A                                                                
ASORTSAV DS    A                                                                
*                                                                               
*        ADDRESSES OF NEEDED ELEMENTS                                           
*                                                                               
ANAMEL   DS    A                   A(NAME)                                      
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
         DS    CL6                                                              
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
         DS    CL3                                                              
TRKAORPC DS    CL6                 A.O.R. PCT                                   
         DS    C                                                                
TRKBOX6  DS    C                                                                
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
         EJECT                                                                  
*********************************************************                       
*        EQUATES                                        *                       
*********************************************************                       
*                                                                               
ESTORDER EQU   4                   ESTIMATE SUMMARY REPORT                      
*                                                                               
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
*                                                                               
*                                  LENGTH OF ACQUIRED BUFFER                    
*                                  (100 EXTRA BYTES FOR SAFETY)                 
LENBUFF  EQU   CODSIZE+CLTSIZE+MEDSIZE+(2*SRTLNQ)+100                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACINT16   05/01/02'                                      
         END                                                                    
