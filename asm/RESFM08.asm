*          DATA SET RESFM08    AT LEVEL 252 AS OF 02/20/13                      
*PHASE T81808A                                                                  
         TITLE 'T81808 - RESFM08 - SWITCH LIST/REPORT'                          
*********************************************************************           
*                                                                   *           
*        RESFM08 (T81808) --- SWITCH RECORD ADD/CHANGE/DISPLAY/LIST *           
*                                                                   *           
*********************************************************************           
*         * * * *  W A R N I N G  * * * *                           *           
*                                                                   *           
*  NOTE:  A SET OF UNUSED SCREEN FIELDS RSWIOPT HAVE HAD THE        *           
*         RECEIVING FIELDS IN THE SWITCH RECORD GENERATED CO-OPTED  *           
*         FOR OTHER FUNCTIONALITY.  IF THESE FIELDS ARE ACTIVATED,  *           
*         **GREAT CARE** MUST BE EXERCISED IN THE USE OF THESE      *           
*         FIELDS.   BILL UHR.  DEC13/01.                            *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* APR11/89 (MRR) --- CHECK THE NUMBER OF SWITCH RECORDS BEFORE      *           
*                     PROCESSING AN 'ADD'.  LIMIT TO 200 PER USER   *           
*                     ID AND 2500 FOR THE FILE.                     *           
*                                                                   *           
* APR18/89 (MRR) --- FIX APR11 FIX.                                 *           
*                                                                   *           
* APR20/89 (MRR) --- ONLY COUNT RECORDS FOR THE CURRENT WEEK.       *           
*                                                                   *           
* 08/30/89  PJS  --- PREVENT ADDING SWITCH REQUESTS FOR 'MASTER'    *           
*                    RECORDS WHEN SIGNED ON AS A SUBSIDIARY REP.    *           
*                    (MASTER RECS=TEAM/GRP/ADV/AGY)                 *           
*                                                                   *           
* 09/15/89  PJS  --- PER KARI, ALLOW GROUP SWITCHES FOR SUB REPS    *           
*                                                                   *           
* 27DEC89   EFJ  --- PREVENT SWITCH RECORD FROM BEING ADDED IF      *           
*                    TARGET STATION RECORD EXISTS (UNLESS DDS TERM) *           
*                                                                   *           
* FEB12/90 (MRR) --- ALLOW BLAIR 500 RECORDS FOR THE WEEK OF        *           
*                     FEB12/90.  LEAVE THIS CODE IN FOR FUTURE      *           
*                     'ONE-TIME-ONLY(S)'.                           *           
*                                                                   *           
* DEC18/90 (MRR) --- ALLOW BLAIR 1300 RECS FOR SELECTED WEEKS       *           
*                                                                   *           
* MAR15/91 (MRR) --- ADD TEL, UNI AND GAL TO AFFILIATE TABLE        *           
*                                                                   *           
* 26APR91  (EFJ) --- ADD FOX TO AFFL TABLE                          *           
*                                                                   *           
* 17JUN91  (EFJ) --- ONLY ALLOW '-C' TO '-C' STATION SWITCH         *           
*                     (IE: NO '-F' TO '-C')                         *           
*                                                                   *           
* 01JUN94  (BU ) --- ALLOW BLAIR 1000 RECS FOR FORESEEABLE FUTURE.  *           
*                                                                   *           
* DEC06/95 (BU ) --- SPECIAL KATZ ACTION FOR STATION...             *           
*                                                                   *           
* JAN10/96 (SKU) --- PREVENT SWITCHING TO THE SAME CODE AS 'OLD CODE*           
*                                                                   *           
* MAR22/96 (WSB) --- ALLOW SZ 500 RECS FOR FORSEEABLE FUTURE        *           
*                                                                   *           
* MAR22/96 (WSB) --- ADD DEVELOPMENTAL SALESPERSON SWITCH.  ADD     *           
*                    'START AT' FILT TO COMP AGAINST CON START DATE *           
*                    ADD GRO, STA, TEA, AGY, SAL, & OFF FILTS FOR   *           
*                    ADV SWITCH, BUT NOT IF HAS PROD CODES. ADD     *           
*                    POINT PERSON SWITCH.                           *           
*                                                                   *           
* APR29/96 (BU ) --- MASTER ACCESS FOR KATZ TV                      *           
*                                                                   *           
* AUG14/96 (WSB) --- FIX BUG WHERE IT ALLOWED A SWITCH FROM AN      *           
*                    AGY/OFF TO JUST AN AGY (WHICH DOES HAVE        *           
*                    OFFICES) -- SHOULD NOT BE ALLOWED              *           
*                                                                   *           
* AUG21/96 (WSB) --- IF OLD CODE IS AGY AND IT HAS OFFICES, DON'T   *           
*                    ALLOW A SWITCH -- THEY MUST ENTER IN EACH      *           
*                    OFFICE INDIVIDUALLY                            *           
*                                                                   *           
* SEP10/96 (WSB) --- ADD UPN AND WBT TO AFFIL TABLE                 *           
*                                                                   *           
* SEP27/96 (WSB) --- TAKE OUT TEAM AND OFFICE SWITCHES (MUST BE     *           
*                    DONE THROUGH SALESPERSON SWITCH)               *           
*                                                                   *           
* OCT04/96 (SEP) --- ALLOW LOW POWER TV STATION ENTRY               *           
*                                                                   *           
* OCT18/96 (WSB) --- PUT BACK IN TEAM AND OFFICE SWITCHES--WAS NOT  *           
*                    ALLOWING TEAM AND OFFICE AS FILTERS            *           
*                                                                   *           
* DEC18/96 (WSB) --- ADD ABILITY TO ENTER SUBSIDARY FILTERS ON A    *           
*                    MASTER RECORD SWITCH                           *           
*                                                                   *           
* JAN31/97 (BU ) --- OPEN UP # SWITCHES FOR:                        *           
*                    PV,IR,SZ,S2,K3,MR --> 1000                     *           
*                                                                   *           
* APR14/97 (BU ) --- OPEN UP # SWITCHES FOR FOX:  1500              *           
*                                                                   *           
* JUL08/98 (BU ) --- PERMIT INTEREP MASTER S/P SWITCH ENTRY         *           
*                                                                   *           
* OCT15/98 (RHV) --- OFC/SAL INSTEAD OF OFC/SAL/TEAM                *           
*                                                                   *           
* 03JUN99  (BU ) --- ADD PAX TO AFFL TABLE                          *           
*                                                                   *           
* 25JUN99  (SKU) --- ADD OTH TO AFFL TABLE                          *           
*                                                                   *           
* NOV15/99 (BU ) --- FLAG CORPORATE AGENCY WHEN TARGET              *           
*                                                                   *           
* MAY15/00 (BU ) --- INCREASE 'BLOC' FROM 150 - 450: HOLD 150 ITEMS *           
*                                                                   *           
* AUG17/00 (JRD) --- INCREASE 'BLOC' FROM 450 - 750: HOLD 250 ITEMS *           
*                                                                   *           
* OCT26/00 (BU ) --- TREAT CLEARCHANNEL (NU) AS A SUB OF KRGNY (K3) *           
*                                                                   *           
* JAN05/01 (BU ) --- ADD WB TO AFF TABLE                            *           
*                                                                   *           
* FEB12/01 (ABOB)---PREVENT USER FROM ENTERING THE SAME STATION     *           
*                   IN OLD AND NEW COLUMN                           *           
*                                                                   *           
* NOV02/01 (BU ) --- DO NOT USE FOR AGY/ADV                         *           
*                                                                   *           
* DEC12/01 (BU ) --- COMPENSATION S/P REQUIREMENTS                  *           
*                                                                   *           
* 24APR02  (BU ) --- ADD TBN TO AFFL TABLE                          *           
*                                                                   *           
* 02APR04  (SKU) --- FIX ADV SWITCH BUG                             *           
*                                                                   *           
* 12AUG04  (BU ) --- FIX ADV FILTER BUG                             *           
*                                                                   *           
* 01OCT04  (BU ) --- FIX ADV FILTER BUG WHEN STA= FILTER USED       *           
*                                                                   *           
* 26AUG06  (HQ ) --- ADD MNT TO AFFL TABLE                          *           
*                                                                   *           
* 19SEP06  (HQ ) --- ADD CW  TO AFFL TABLE                          *           
*                                                                   *           
* 12OCT06  (BU ) --- ADD ION TO AFFL TABLE                          *           
*                                                                   *           
* 02JAN07  (BU ) --- ADD NBW TO AFFL TABLE                          *           
*                                                                   *           
* 07MAY07  (HQ ) --- ADD LAT TO AFFL TABLE                          *           
*                                                                   *           
* 20JUN07  (SKU) --- SPLIT AFFL TABLE TO COMMON INCLUDE BOOK        *           
*                    *****  END TOMBSTONE  *****                    *           
*********************************************************************           
*                                                                               
T81808   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1808**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LIST                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VKEY     DS    0H                                                               
         XC    AAGYCORP,AAGYCORP   CLEAR A(AGENCY CORP D/A)                     
         MVI   AGYCORP,C' '        CLEAR NEW CODE FLAG                          
*                                                                               
         MVI   STATUS,0                                                         
         SPACE 2                                                                
****************************************************************                
*                                FIND MONDAY OF THIS WEEK      *                
****************************************************************                
         GOTO1 =A(GETMON),DMCB,(RC),RR=Y                                        
         SPACE 1                                                                
****************************************************************                
*                                 VALIDATE TYPE                *                
****************************************************************                
         SPACE 1                                                                
         LA    R2,SWITYPH                                                       
         CLI   ACTNUM,ACTLIST      TYPE OPTIONAL FOR LIST, OR                   
         BNE   VK10                                                             
         CLI   5(R2),0                                                          
         BE    VK15                                                             
         CLC   8(2,R2),=C'D='      CAN PUT DATE TO USE AS                       
         BE    VK11                RSWIKYMD INSTEAD OF MTODAY                   
         CLC   8(2,R2),=C'S='      ALSO CAN PUT 'STARTING AT' DATE              
         BNE   VK20                                                             
         OI    STATUS,X'40'                                                     
         B     VK11                                                             
         SPACE 1                                                                
VK10     CLI   ACTNUM,ACTREP       TYPE NOT ALLOWED FOR REPORT                  
         BNE   VK20                                                             
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    VK13                                                             
         CLC   8(2,R2),=C'D='      BUT CAN PUT DATE HERE TO USE AS              
         BE    VK11                AS RSWIKYMD INSTEAD OF MTODAY                
         CLC   8(2,R2),=C'S='       ALSO CAN USE 'STARTING AT' DATE             
         BNE   ERREND              RSWIKYMD INSTEAD OF MTODAY                   
         OI    STATUS,X'40'                                                     
VK11     ZIC   RE,5(R2)                                                         
         SH    RE,=H'3'            REMOVE D= AND 1 FOR EX                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SWITYP+2                                                 
         GOTO1 DATVAL,DMCB,WORK,DUB                                             
         OC    DMCB(4),DMCB        ERROR                                        
         BZ    ERREND                                                           
         GOTO1 GETDAY,DMCB,DUB,FULL             DAY OF WEEK                     
         SR    R3,R3                                                            
         IC    R3,DMCB                                                          
         BCTR  R3,0                                                             
         LNR   R3,R3                            BACK UP TO MONDAY               
         GOTO1 ADDAY,DMCB,DUB,DMCB+12,(R3)                                      
         GOTO1 DATCON,DMCB,DMCB+12,(3,QMDAY)                                    
         SPACE 1                                                                
VK13     LA    R2,SWIOLDH          OLD NOT ALLOWED FOR REPORT                   
         CLI   5(R2),0                                                          
         BNE   ERREND                                                           
VK15     XC    SWITYPE(15),SWITYPE   CLEAR OUT SOME STORAGE - (SWITYPE,         
         B     VKXIT   (RECTYP,TYPE,VALFLT,REQFLT,VFLT,RFLT,SEQNUM,OLD)         
         SPACE 1                                                                
VK20     MVI   ERROR,INVALID                                                    
         CLI   5(R2),3             NEED AT LEAST 3 CHAR. OF KEYWORD             
         BL    ERREND                                                           
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                FOR EXECUTE                                  
         SPACE                                                                  
         LA    R3,TYPTAB                                                        
VK30     CLI   0(R3),X'FF'                                                      
         BE    ERREND                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R3)                                                    
         BE    VK40                                                             
         LA    R3,L'TYPTAB(R3)                                                  
         B     VK30                                                             
         SPACE 1                                                                
VK40     DS    0H                                                               
*VK40     CLI   1(RA),C'*'          CHECK FOR DDS TERMINAL                      
*         BE    VK42                                                            
*         CLI   11(R3),X'02'        ONLY DDS CAN DO STATION CHANGE              
*         BNE   VK42                                                            
*         MVI   ERROR,SECLOCK                                                   
*         B     ERREND                                                          
         SPACE 1                                                                
VK42     MVC   TYPE,11(R3)                                                      
         MVC   SWITYPE,11(R3)      SWITCH RECORD TYPE                           
         MVC   RECTYP,12(R3)       REP RECORD TYPE                              
         MVC   VALFLT,16(R3)       VALID FILTERS                                
         MVC   REQFLT,17(R3)       REQUIRED FILTERS                             
         MVC   FLAGS,18(R3)        TO TELL IF MAY HAVE START DATE               
         EJECT                                                                  
****************************************************************                
*  BLOCK SUBSIDIARY REPS FROM SWITCHING MASTER RECORDS.        *                
****************************************************************                
         SPACE                                                                  
         LA    RE,MASTLIST         MASTER RECORD LIST                           
BLKSUB20 CLI   0(RE),0                                                          
         BE    BLKSUB99            NOT A MASTER REC SWITCH. CONTINUE.           
         CLC   RECTYP,0(RE)                                                     
         BE    BLKSUB30            MASTER RECORD. CHECK REP RECORD.             
         LA    RE,1(RE)                                                         
         B     BLKSUB20                                                         
         SPACE                                                                  
MASTLIST EQU   *                   LIST OF 1 BYTE KEY ID'S                      
         DC    X'05'               TEAM                                         
         DC    X'06'               SALESPERSON                                  
*** KARI DC    X'07'               GROUP                                        
         DC    X'08'               ADVERTISER                                   
         DC    X'0A'               AGENCY                                       
         DC    X'31'               POINT PERSON                                 
         DC    X'3A'               DEVELOPMENTAL SALESPERSON                    
         DC    X'00'               EOT                                          
         DS    0H                  ALIGNMENT                                    
*                                                                               
*- MASTER RECORD SWITCH.  SEE IF REP IS SUBSIDIARY.                             
BLKSUB30 EQU   *                                                                
         MVC   AIO,AIO2            READ REC INTO IO2                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           REP REC ID                                   
         MVC   KEY+25(2),AGENCY    REP CODE                                     
         GOTO1 READ                                                             
         CLI   DMCB+8,X'10'        REC FOUND?                                   
         BNE   *+6                                                              
         DC    H'0'                REP NOT ON FILE.                             
         GOTO1 GETREC              READ IN REP RECORD                           
*                                                                               
         L     R1,AIO                                                           
         USING REPREC,R1                                                        
*                                                                               
         CLC   =C'NU',AGENCY       CLEARCHANNEL?                                
         BE    BLKSUB50            YES - TREAT AS SUBSIDIARY                    
         CLC   RREPMAST,=H'0'      NEITHER MASTER OR SUBSID.                    
         BE    BLKSUB99                                                         
*                                                                               
         CLC   RREPMAST,=H'-1'     MASTER?                                      
         BNE   BLKSUB50            NO  - SUBSIDIARY                             
*                                  YES - MASTER                                 
*                                     SALESPERSON MUST BE PERMITTED             
*                                        FOR ALL MASTERS NOW                    
         CLC   RECTYP,=X'06'       SALESPERSON SWITCH TYPE?                     
         BNE   BLKSUB35            NO  - CHECK FOR TEAM                         
***>>>   CLC   AGENCY,=C'IR'       YES - IS MASTER INTEREP?                     
***>>>   BE    BLKSUB70            YES - REJECT FOR INTEREP                     
*                                     S/P IS A MASTER FOR KATZ                  
         B     BLKSUB99                  SO PERMIT IT                           
*                                                                               
BLKSUB35 EQU   *                                                                
*                                  TEAM MUST BE PERMITTED FOR INTEREP           
*                                     AND KATZ RADIO, REJECTED FOR              
*                                        KATZ TV                                
         CLC   RECTYP,=X'05'       SALESPERSON SWITCH TYPE?                     
         BNE   BLKSUB99            NO  - ALL OTHERS PERMITTED                   
         CLC   AGENCY,=C'MR'       YES - IS MASTER KATZ TV?                     
         BE    BLKSUB70            YES - REJECT FOR KATZ TV                     
*                                     S/P IS A MASTER FOR KATZ RADIO            
*                                        OR INTEREP, SO PERMIT IT               
         B     BLKSUB99                                                         
*                                                                               
BLKSUB50 EQU   *                                                                
*                                                                               
*   ATTEMPTING TO SWITCH MASTER RECORDS FROM SUBSIDIARY.                        
*        S/P RECORD RECORD MUST BE TREATED SEPARATELY.                          
*        PERMIT FOR INTEREP (MASTER = IR), REJECT FOR                           
*             KATZ RADIO (MASTER = K3) AND KATZ TV (MASTER = MR)                
*        TEAM RECORD RECORD MUST BE TREATED SEPARATELY.                         
*        PERMIT FOR KATZ TV (MASTER = MR), REJECT FOR                           
*             KATZ RADIO (MASTER = K3) AND INTEREP (MASTER = IR).               
*                                                                               
         CLC   RECTYP,=X'06'       SALESPERSON SWITCH TYPE?                     
         BNE   BLKSUB55            NO  - DISPLAY ERROR MESSAGE                  
         CLC   RREPMAST,=C'IR'     YES - IS MASTER INTEREP?                     
         BNE   BLKSUB60            NO  - DON'T PERMIT THIS SWITCH               
         B     BLKSUB99            YES - PERMIT THIS SWITCH                     
*                                     S/P IS MASTER FOR KATZ TV/RADIO           
*                                        SO IT CAN'T BE SWITCHED                
*                                        FROM A SUBSIDIARY                      
BLKSUB55 EQU   *                                                                
         CLC   RECTYP,=X'05'       DIV/TEAM SWITCH TYPE?                        
         BNE   BLKSUB60            NO  - DISPLAY ERROR MESSAGE                  
         CLC   RREPMAST,=C'MR'     YES - IS MASTER KATZ TV?                     
         BNE   BLKSUB60            NO  - DON'T PERMIT THIS SWITCH               
         B     BLKSUB99            YES - PERMIT THIS SWITCH                     
*                                     TEAM IS A MASTER FOR KATZ RADIO           
*                                        AND INTEREP                            
*                                        SO IT CAN'T BE SWITCHED                
*                                        FROM A SUBSIDIARY                      
BLKSUB60 EQU   *                                                                
         MVC   CONHEAD+10(L'SUBCANT),SUBCANT                                    
         LA    R2,SWITYPH          POINT TO RECORD TYPE                         
         B     MYEND                                                            
         SPACE                                                                  
BLKSUB70 EQU   *                                                                
         MVC   CONHEAD+10(L'MSTCANT),MSTCANT                                    
         LA    R2,SWITYPH          POINT TO RECORD TYPE                         
         B     MYEND                                                            
         SPACE                                                                  
BLKSUB99 EQU   *                                                                
         DROP  R1                                                               
         EJECT                                                                  
****************************************************************                
*                             VALIDATE OLD CODE                *                
****************************************************************                
         SPACE 1                                                                
         LA    R2,SWIOLDH                                                       
         CLI   ACTNUM,ACTLIST      OLD CODE NOT REQUIRED FOR LIST               
         BNE   VK45                                                             
         CLI   5(R2),0                                                          
         BNE   VK45                                                             
         XC    OLD,OLD                                                          
         B     VKXIT                                                            
         SPACE 1                                                                
VK45     GOTO1 ANY                 PUTS INPUT INTO WORK                         
         XC    OLD,OLD             CLEAR STORAGE FOR 'OLD' CODE                 
         OI    STATUS,X'08'        INDICATE VALIDATING OLD CODE                 
         XC    KEY,KEY                                                          
         MVC   AIO,AIO2            USE IO2 FOR VALIDATION                       
         SR    RF,RF                                                            
         ICM   RF,7,13(R3)                                                      
         A     RF,RELO                                                          
         BASR  RE,RF               VALIDATE OLD RECORD                          
         MVC   OLD,WORK+10                                                      
         NI    STATUS,X'F7'        DONE VALIDATING OLD CODE                     
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         LA    R2,SWIOLDNH         OLD CODE NAME                                
         MVC   8(20,R2),SPACES                                                  
         OI    SWIOLDNH+6,X'80'    TRANSMIT FIELD                               
         EJECT                                                                  
****************************************************************                
*                             VALIDATE SEQUENCE NUMBER         *                
****************************************************************                
         SPACE 1                                                                
         CLI   ACTNUM,ACTLIST      SEQNUM IGNORED FOR LIST                      
         BE    VKXIT                                                            
         CLI   ACTNUM,ACTREP       SEQNUM IGNORED FOR REPORT                    
         BE    VKXIT                                                            
         LA    R2,SWISEQH                                                       
         CLI   ACTNUM,ACTADD       BUILD SEQ NUM MYSELF FOR ADD                 
         BNE   VK65                                                             
*                                                                               
        GOTO1  =A(CHECKCNT),RR=Y   CHECK THAT THE FILE IS NOT 'FULL'            
*                              ---- THIS ROUTINE WILL NOT RETURN IF             
*                                   THERE IS AN ERROR CONDITION                 
*                                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY             BUILD SWITCH KEY TO GET PREV SEQ NO.         
         LA    R4,KEY                                                           
         USING RSWIKEY,R4                                                       
         MVI   RSWIKTYP,X'28'                                                   
         MVC   RSWIKREP,AGENCY                                                  
         MVC   RSWIKYMD,MTODAY                                                  
         MVC   RSWIKCHA,SWITYPE                                                 
         MVC   RSWIKOLD,OLD                                                     
         SR    R3,R3                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(26),KEYSAVE                                                  
         BNE   VK60                                                             
VK50     MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         CLC   KEY(26),KEYSAVE                                                  
         BE    VK50                                                             
VK60     IC    R3,KEYSAVE+26                                                    
         LA    R3,1(R3)                                                         
         STC   R3,SEQNUM           SAVE SEQUENCE NUMBER AND                     
         CVD   R3,DUB                                                           
         UNPK  8(3,R2),DUB+6(2)                                                 
         OI    10(R2),X'F0'                                                     
         OI    SWISEQH+6,X'80'     PUT TO SCREEN                                
         MVC   AIO,AIO1                                                         
         B     VKXIT                                                            
         SPACE 2                                                                
VK65     GOTO1 ANY                                                              
         TM    4(R2),X'08'         FIELD IS VALID NUMERIC                       
         BO    VK70                                                             
         MVI   ERROR,NOTNUM        MUST BE NUMERIC                              
         B     ERREND                                                           
VK70     ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)                                                   
         CVB   R0,DUB                                                           
         STC   R0,SEQNUM                                                        
*                                                                               
VKXIT    EQU   *                                                                
         MVC   AIO,AIO1            BUILD SWITCH KEY IN AIO1                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSWIKEY,R4                                                       
         MVI   RSWIKTYP,X'28'                                                   
         MVC   RSWIKREP,AGENCY                                                  
         MVC   RSWIKYMD,MTODAY                                                  
         MVC   RSWIKCHA,SWITYPE                                                 
         MVC   RSWIKOLD,OLD                                                     
         MVC   RSWIKSEQ,SEQNUM                                                  
         MVC   KEY2CH,KEY          SAVE OFF THE KEY                             
         DROP  R4                                                               
VKXX     B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
VREC     EQU   *                                                                
         MVC   SVDMWORK,DMWORK+4   SAVE D/A (MAY NEED IF LATER GETREC)          
         XC    OPTIONS,OPTIONS                                                  
*                                                                               
         NI    FLAGS,X'FF'-X'40'   NOT AN ADVERTISER WITH PRODUCT (YET)         
         CLC   SWITYP(3),=C'ADV'   IS THIS AN ADVERTISER SWITCH?                
         BNE   VR5                 NO                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'09'           BUILD PRODUCT RECORD                         
         MVC   KEY+18(4),SWIOLD    ADVERTISER CODE                              
         OC    KEY+18(4),SPACES                                                 
         GOTO1 HIGH                                                             
*                                                                               
VR2      DS    0H                                                               
         CLC   KEY(22),KEYSAVE     ANY PRODUCTS FOR THIS ADVERTISER?            
         BNE   VR5                 NO                                           
         CLC   AGENCY,KEY+25       SAME REP?                                    
         BE    VR3                                                              
         GOTO1 SEQ                                                              
         B     VR2                                                              
*                                                                               
VR3      DS    0H                                                               
         OI    FLAGS,X'40'         YES, TURN BIT ON                             
*                                                                               
VR5      DS    0H                                                               
         LA    R3,TYPTAB                                                        
*                                                                               
VR10     CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                IT WAS THERE THE FIRST TIME                  
         CLC   SWITYPE,11(R3)                                                   
         BE    VR20                                                             
         LA    R3,L'TYPTAB(R3)                                                  
         B     VR10                                                             
         SPACE 1                                                                
VR20     MVC   TYPE,SWITYPE                                                     
         SPACE 1                                                                
*         CLI   1(RA),C'*'          CHECK FOR DDS TERMINAL                      
*         BE    VR30                                                            
*         CLI   11(R3),X'02'        ONLY DDS CAN DO STATION CHANGE              
*         BNE   VR30                                                            
*         MVI   ERROR,SECLOCK                                                   
*         LA    R2,SWITYPH                                                      
*         B     ERREND                                                          
         SPACE 1                                                                
****************************************************************                
*                             VALIDATE OPTIONS FIELD           *                
****************************************************************                
         SPACE 1                                                                
VR30     LA    R2,SWIOPTH          CURRENTLY NOT USED                           
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BNE   ERREND                                                           
         SPACE 1                                                                
****************************************************************                
*                             VALIDATE NEW CODE                *                
****************************************************************                
         SPACE 1                                                                
VR35     LA    R2,SWINEWH                                                       
*                                                                               
         CLC   SWIOLD,SWINEW       NEW CODE CANNOT BE SAME AS OLD CODE          
         BE    DUPERR                                                           
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         MVI   AGYCORP,C'A'        SET 'NEW CODE IN TEST'                       
*                                                                               
         XC    KEY,KEY                                                          
         NI    STATUS,X'F5'        TURN OFF OTHER VALIDATIONS AND               
         OI    STATUS,X'04'        INDICATE VALIDATING NEW CODE                 
         SPACE                                                                  
         MVC   AIO,AIO2            USE IO2 FOR VALIDATION                       
         SR    RF,RF                                                            
         ICM   RF,7,13(R3)                                                      
*                                                                               
*   TEST                                                                        
***      MVC   DIE(2),=X'0000'                                                  
*   END DIE TEST                                                                
*                                                                               
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         MVC   NEW,WORK+10                                                      
*                                                                               
*VALIDATE NEW STATION CODE AGAINST PREVIOUSLY ENTERED                           
*OLD STATION CODE                                                               
*ABOB ADDED                                                                     
         CLI   11(R3),X'02'          CHECK FOR CODE TYPE                        
         BNE   VRE351                ONLY STATION                               
         GOTO1 =A(CHOLDNEW),DMCB,(RC),RR=Y                                      
         CLI   DMCB,0                                                           
         BNE   MYEND                                                            
VRE351   DS    0H                                                               
*                                                                               
         NI    STATUS,X'FB'        DONE VALIDATING NEW CODE                     
         MVC   AIO,AIO1            RESTORE AIO AREA                             
         SPACE 1                                                                
         LA    R2,SWINEWNH         NEW CODE NAME                                
         MVC   8(20,R2),SPACES                                                  
         OI    SWINEWNH+6,X'80'    TRANSMIT FIELD                               
         SPACE 3                                                                
****************************************************************                
*                             VALIDATE START DATE              *                
****************************************************************                
         SPACE 1                                                                
VR40     LA    R2,SWISTAH          START DATE                                   
         XC    STADATE,STADATE     CLEAR OUT SINCE OPTIONAL FIELD               
         MVI   DDSFLAGS,0          CLEAR OUT FLAG BYTE                          
*                                                                               
         CLI   SWISTAH+5,0         DID THEY ENTER ANYTHING?                     
         BE    VR400100            NO, GO ON                                    
*                                                                               
         TM    FLAGS,X'40'         ADVERTISER WITH PRODUCT CODE?                
         BZ    *+14                NO                                           
         MVC   RERROR,=AL2(580)    FILTERS NOT ALLOWED                          
         B     MYERR                                                            
*                                                                               
         TM    FLAGS,X'80'         IS START DATE ALLOWED FOR THIS TYPE?         
         BO    *+14                YES                                          
         MVC   CONHEAD+10(L'NOTFILT),NOTFILT    NO                              
         B     MYEND                                                            
*                                                                               
         GOTO1 PERVAL,DMCB,(L'SWISTA,SWISTA),(X'40',WORK)                       
         CLI   DMCB+4,X'04'        VALID DATE?                                  
         BE    *+14                YES                                          
         MVC   RERROR,=AL2(578)    INVALID DATE                                 
         B     MYERR                                                            
*                                                                               
         MVC   STADATE,WORK+28     3 BYTE BINARY DATE                           
*                                                                               
         LA    R2,SWIWINH          'WITHIN' FIELD                               
         CLI   5(R2),0             ANYTHING ENTERED?                            
         BE    VR400100            NO                                           
         CLI   8(R2),C'N'          ENTERED AS 'NO'?                             
         BE    VR400100            YES - LEAVE AS IS                            
         CLI   8(R2),C'Y'          ENTERED AS 'YES'?                            
         BE    VR400020            YES - SET FLAG                               
         MVC   CONHEAD+10(L'WITHINNG),WITHINNG    NO                            
         B     MYEND                                                            
*                                                                               
VR400020 EQU   *                                                                
         OI    DDSFLAGS,X'40'      SET 'DATE START WITHIN FLIGHT'               
VR400100 EQU   *                                                                
         CLC   SWITYP(3),=C'SAL'   IS THIS A SALESPERSON SWITCH?                
         BNE   VR400200            NO                                           
*                                                                               
*   ONLY APPLY COMPENSATION OPTIONS FOR SALESPERSON SWITCHES                    
*                                                                               
         LA    R2,SWICSPH          COMPENSATION OPTIONS                         
         CLI   5(R2),0             ANYTHING ENTERED?                            
         BE    VR400200            NO                                           
         CLI   8(R2),C'A'          COMP S/P -> NEW S/P?                         
         BNE   VR400120            NO                                           
         OI    DDSFLAGS,X'20'      SET 'COMP -> NEW'                            
         B     VR400200                                                         
VR400120 EQU   *                                                                
         CLI   8(R2),C'B'          COMP S/P -> HOUSE ACCOUNT?                   
         BNE   VR400140            NO                                           
         OI    DDSFLAGS,X'10'      SET 'COMP -> HOUSE'                          
         B     VR400200                                                         
VR400140 EQU   *                                                                
         MVC   CONHEAD+10(L'COMPNG),COMPNG    NO                                
         B     MYEND                                                            
VR400200 EQU   *                                                                
         SPACE 3                                                                
****************************************************************                
*                             VALIDATE REQUESTOR NAME          *                
****************************************************************                
         SPACE 1                                                                
VR45     EQU   *                                                                
         GOTO1 =A(VREQUEST),DMCB,(RC),RR=Y                                      
         EJECT                                                                  
****************************************************************                
*                             VALIDATE FILTERS                 *                
****************************************************************                
         SPACE 1                                                                
VR60     MVI   ELCODE,2                                                         
         GOTO1 REMELEM             DELETE OLD X'02' ELEMENT                     
*                                                                               
         LA    R2,SWIF1H           AND REBUILD ELEMENT FOR EACH FILTER          
         LA    R5,6                FOR BCT LOOP THROUGH FILTER FIELDS           
         MVC   VFLT,VALFLT                                                      
         MVC   RFLT,REQFLT                                                      
*                                                                               
VR65     CLI   5(R2),0                                                          
         BE    VR100                                                            
*                                                                               
         TM    FLAGS,X'40'         ADVERTISER WITH PRODUCT CODE?                
         BZ    *+14                NO                                           
         MVC   RERROR,=AL2(580)    FILTERS NOT ALLOWED                          
         B     MYERR                                                            
*                                                                               
         XC    BLOCK,BLOCK                                                      
         MVI   ERROR,INVALID                                                    
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK)                                      
         LA    R4,BLOCK                                                         
         CLI   DMCB+4,1            1 FILTER PER FIELD                           
         BNE   ERREND                                                           
         CLI   0(R4),3             1ST HALF MUST BE 3 CHARACTERS                
         BNE   ERREND                                                           
         CLI   1(R4),0             MUST HAVE 2ND HALF                           
         BE    ERREND                                                           
         LA    R3,TYPTAB                                                        
VR70     CLI   0(R3),X'FF'                                                      
         BE    ERREND                                                           
         CLC   12(3,R4),0(R3)                                                   
         BE    VR80                                                             
         LA    R3,L'TYPTAB(R3)                                                  
         B     VR70                                                             
         SPACE 1                                                                
VR80     MVC   TYPE,11(R3)                                                      
         MVC   BYTE,11(R3)                                                      
         NC    BYTE,VFLT           IS TYPE A VALID FILTER                       
         BNZ   *+14                                                             
         MVC   CONHEAD+10(L'NOTFILT),NOTFILT                                    
         B     MYEND                                                            
         XC    VFLT,TYPE           ONCE USED, IT'S NO LONGER VALID              
         SPACE 1                                                                
         CLI   TYPE,X'02'          IF STATION/GROUP                             
         BNE   VR81                                                             
         MVC   BYTE,VFLT                                                        
         NI    BYTE,X'20'                                                       
         BZ    VR87                                                             
         XI    VFLT,X'20'          INVALIDATE GROUP                             
         SPACE 1                                                                
VR81     CLI   TYPE,X'20'          IF GROUP                                     
         BNE   VR82                                                             
         MVC   BYTE,VFLT                                                        
         NI    BYTE,X'02'                                                       
         BZ    VR87                                                             
         XI    VFLT,X'02'          INVALIDATE STATION/GROUP                     
         SPACE 1                                                                
VR82     CLI   TYPE,X'01'          IF TEAM                                      
         BNE   VR83                                                             
         MVC   BYTE,VFLT                                                        
         NI    BYTE,X'10'                                                       
         BZ    VR82M                                                            
         XI    VFLT,X'10'          INVALIDATE SALESPERSON/TEAM                  
         SPACE 1                                                                
VR82M    MVC   BYTE,VFLT                                                        
         NI    BYTE,X'04'                                                       
         BZ    VR87                                                             
         XI    VFLT,X'04'          AND OFFICE                                   
         SPACE 1                                                                
VR83     CLI   TYPE,X'10'          IF SALESPERSON/TEAM                          
         BNE   VR84                                                             
         MVC   BYTE,VFLT                                                        
         NI    BYTE,X'01'                                                       
         BZ    VR83M                                                            
         XI    VFLT,X'01'          INVALIDATE TEAM                              
         SPACE 1                                                                
VR83M    MVC   BYTE,VFLT                                                        
         NI    BYTE,X'04'                                                       
         BZ    VR87                                                             
         XI    VFLT,X'04'          AND OFFICE                                   
         SPACE 1                                                                
VR84     CLI   TYPE,X'04'          IF OFFICE                                    
         BNE   VR87                                                             
         MVC   BYTE,VFLT                                                        
         NI    BYTE,X'01'                                                       
         BZ    VR84M                                                            
         XI    VFLT,X'01'          INVALIDATE TEAM                              
         SPACE 1                                                                
VR84M    MVC   BYTE,VFLT                                                        
         NI    BYTE,X'10'                                                       
         BZ    VR87                                                             
         XI    VFLT,X'10'          AND SALESPERSON/TEAM                         
         SPACE 1                                                                
VR87     MVC   BYTE,11(R3)                                                      
         NC    BYTE,RFLT           IS TYPE A REQUIRED FILTER                    
         BZ    VR90                                                             
         XC    RFLT,TYPE           YES.  IT'S DONE, SO TURN IT OFF              
         SPACE 1                                                                
*                                                                               
VR90     XC    WORK(40),WORK                                                    
         MVC   WORK(10),22(R4)       (SCANNER IS SPACES FILLED)                 
         NI    STATUS,X'F3'        TURN OFF OTHER VALIDATION AND                
         OI    STATUS,X'02'        TURN ON VALIDATING FILTERS                   
         MVC   AIO,AIO2            USE IO2 FOR VALIDATION                       
         XC    KEY,KEY                                                          
         SR    RF,RF                                                            
         ICM   RF,7,13(R3)                                                      
         A     RF,RELO                                                          
         BASR  RE,RF               VALIDATE FILTER RECORD                       
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RSWIFIEL,R6                                                      
         MVC   RSWIFICD(2),=X'020A'                                             
         MVC   RSWIFTYP,11(R3)                                                  
         MVC   RSWIFFTR,WORK+10                                                 
         OC    RSWIFFTR,SPACES                                                  
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR100    ZIC   RE,0(R2)            POINT TO NAME FIELD                          
         AR    R2,RE                                                            
         MVC   8(20,R2),SPACES                                                  
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         ZIC   RE,0(R2)            POINT TO NEXT FILTER                         
         AR    R2,RE                                                            
         BCT   R5,VR65                                                          
         SPACE 1                                                                
         NI    STATUS,X'FD'        TURN OFF VALIDATING FILTERS                  
         CLI   RFLT,0             DO WE HAVE ALL REQUIRED FILTERS               
         BE    VRXIT                                                            
         MVC   CONHEAD+10(L'MISSFLT),MISSFLT                                    
         LA    R3,TYPTAB                                                        
VR110    CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,11(R3)                                                      
         NC    BYTE,RFLT                                                        
         BNZ   VR120               FILL IN MISSING FILTER                       
         LA    R3,L'TYPTAB(R3)                                                  
         B     VR110                                                            
         SPACE 1                                                                
VR120    MVC   CONHEAD+28(11),0(R3)     MISSING FILTER ON XXXXX                 
         LA    R2,SWIF1H                                                        
         B     MYEND                                                            
VRXIT    DS    0H            IF NOT ADD, MAY NEED TO DO ANOTHER                 
         GOTO1 =A(CHKAGCRP),RR=Y                                                
         CLI   ACTNUM,ACTADD       GETREC BEFORE THE PUTREC                     
         BE    VRXX                                                             
         TM    STATUS,X'80'        HAVE WE DONE ANOTHER GETREC                  
         BZ    VRXX                                                             
         NI    STATUS,X'7F'        YES, AND TURN OFF INDICATOR                  
         MVC   AIO,AIO2            PUT IT IN AIO2                               
         MVC   KEY+28(4),SVDMWORK                                               
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            NEW RECORD IS IN AIO1                        
VRXX     B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DKEY     DS    0H                                                               
         L     R4,AIO              RECORD SELECTED                              
         USING RSWIKEY,R4                                                       
         SPACE 1                                                                
         LA    R2,SWITYPH          RECORD TYPE FIELD                            
         LA    R3,TYPTAB                                                        
DK30     CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RSWIKCHA,11(R3)                                                  
         BE    DK40                                                             
         LA    R3,L'TYPTAB(R3)                                                  
         B     DK30                                                             
         SPACE 1                                                                
DK40     MVC   TYPE,11(R3)                                                      
         MVC   SWITYPE,11(R3)      SWITCH RECORD TYPE                           
         MVC   RECTYP,12(R3)       REP RECORD TYPE                              
         MVC   VALFLT,16(R3)       VALID FILTERS                                
         MVC   REQFLT,17(R3)       REQUIRED FILTERS                             
         MVC   FLAGS,18(R3)        TO TELL IF MAY HAVE START DATE               
         MVC   8(11,R2),0(R3)                                                   
         CLI   SWITYPE,2           STATION RECORD?                              
         BNE   DK400020            NO                                           
         TM    RSWIDDS,X'80'       DDSSTATION RECORD?                           
         BNO   DK400020            NO                                           
         MVC   8(11,R2),=C'DDSSTATION '                                         
DK400020 EQU   *                                                                
         OI    SWITYPH+6,X'80'     TRANSMIT FIELD                               
         SPACE 1                                                                
         LA    R2,SWIOLDH                                                       
         XC    WORK(10),WORK                                                    
         MVC   WORK(7),RSWIKOLD                                                 
         MVC   OLD,RSWIKOLD                                                     
         GOTO1 =A(FMTCODE),RR=Y                                                 
**>>>    BAS   RE,FMTCODE                                                       
         MVC   8(10,R2),WORK+10                                                 
         OI    SWIOLDH+6,X'80'     TRANSMIT FIELD                               
         SPACE 1                                                                
         LA    R2,SWISEQH                                                       
         ZIC   R1,RSWIKSEQ                                                      
         CVD   R1,DUB                                                           
         UNPK  8(3,R2),DUB+6(2)                                                 
         OI    10(R2),X'F0'                                                     
         OI    SWISEQH+6,X'80'     TRANSMIT FIELD                               
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DREC     DS    0H                                                               
         L     R4,AIO                                                           
         USING RSWIREC,R4                                                       
         LA    R6,RSWIELEM                                                      
         USING RSWIELEM,R6                                                      
         LA    R2,SWINEWH          NEW CODE                                     
         MVC   TYPE,RSWIKCHA                                                    
         XC    WORK(10),WORK                                                    
         MVC   WORK(7),RSWINEW                                                  
         GOTO1 =A(FMTCODE),RR=Y                                                 
**>>     BAS   RE,FMTCODE                                                       
         MVC   8(10,R2),WORK+10                                                 
         OI    SWINEWH+6,X'80'     TRANSMIT FIELD                               
         SPACE 1                                                                
         LA    R2,SWIREQH          REQUESTOR NAME                               
         MVC   8(L'RSWIREQN,R2),RSWIREQN                                        
         OI    SWIREQH+6,X'80'     TRANSMIT FIELD                               
         SPACE 1                                                                
         LA    R2,SWIOPTH          OPTIONS FIELD                                
         MVC   8(50,R2),SPACES                                                  
         OI    SWIOPTH+6,X'80'     TRANSMIT FIELD                               
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(3,RSWISTA),(8,SWISTA)   START DATE FIELD            
         OI    SWISTAH+6,X'80'     TRANSMIT FIELD                               
         MVI   SWIWIN,C'N'         INITIALIZE DATES WITHIN VALUE                
         TM    RSWIDDS,X'40'       DATES WITHIN OPT SET?                        
         BNO   DR100020            NO                                           
         MVI   SWIWIN,C'Y'         YES                                          
DR100020 EQU   *                                                                
         OI    SWIWINH+6,X'80'     TRANSMIT FIELD                               
         MVI   SWICSP,C' '         INITIALIZE COMP S/P OPTION                   
         TM    RSWIDDS,X'20'       COMP SP -> NEW S/P?                          
         BNO   DR100040            NO                                           
         MVI   SWICSP,C'A'         YES                                          
         B     DR100060                                                         
DR100040 EQU   *                                                                
         TM    RSWIDDS,X'10'       COMP SP -> HOUSE ACCOUNT?                    
         BNO   DR100060            NO                                           
         MVI   SWICSP,C'B'         YES                                          
         B     DR100060                                                         
DR100060 EQU   *                                                                
         OI    SWICSPH+6,X'80'     TRANSMIT FIELD                               
         SPACE 1                                                                
DR10     SR    RE,RE                                                            
         LA    RF,6                CLEAR OUT 6 FILTER FIELDS                    
         LA    R2,SWIF1H                                                        
DR20     MVC   8(14,R2),SPACES                                                  
         OI    6(R2),X'80'                                                      
         IC    RE,0(R2)            NAME FIELD                                   
         AR    R2,RE                                                            
         IC    RE,0(R2)            NEXT FILTER FIELD                            
         AR    R2,RE                                                            
         BCT   RF,DR20                                                          
         SPACE 1                                                                
         LA    R2,SWIF1H           NOW FILL IN ANY FILTERS                      
         MVI   ELCODE,2                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   DRXIT                                                            
DR40     LA    R3,TYPTAB                                                        
DR50     CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   11(1,R3),2(R6)         FILTER TYPE                               
         BE    DR60                                                             
         LA    R3,L'TYPTAB(R3)                                                  
         B     DR50                                                             
         SPACE 1                                                                
DR60     MVC   8(3,R2),0(R3)                                                    
         MVI   11(R2),C'='                                                      
         MVC   TYPE,2(R6)                                                       
         CLI   2(R6),2             STATION                                      
         BE    DR70                                                             
         CLI   2(R6),X'40'         AGENCY                                       
         BNE   DR80                                                             
DR70     XC    WORK(10),WORK                                                    
         MVC   WORK(7),3(R6)       NEED SPECIAL FORMATING                       
         GOTO1 =A(FMTCODE),RR=Y                                                 
**>>     BAS   RE,FMTCODE                                                       
         MVC   12(10,R2),WORK+10   FILTER NAME                                  
         B     *+10                                                             
DR80     MVC   12(7,R2),3(R6)      FILTER NAME                                  
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         ZIC   RE,0(R2)            POINT TO NAME FIELD                          
         AR    R2,RE                                                            
         ZIC   RE,0(R2)            NEXT FILTER FIELD                            
         AR    R2,RE                                                            
         BAS   RE,NEXTEL                                                        
         BE    DR40                                                             
DRXIT    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*                        LIST AND PRINT ROUTINE                *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
LIST     DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR5                                                              
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
LR5      LA    R4,KEY                                                           
         USING RSWIKEY,R4                                                       
         OC    KEY(27),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVI   RSWIKTYP,X'28'                                                   
         MVC   RSWIKREP,AGENCY                                                  
         MVC   RSWIKYMD,QMDAY                                                   
         MVC   RSWIKCHA,SWITYPE                                                 
         MVC   RSWIKOLD,OLD                                                     
         MVC   RSWIKSEQ,SEQNUM                                                  
         MVC   SAVEKEY,KEY                                                      
LR10     GOTO1 HIGH                                                             
LR15     CLC   KEY(15),SAVEKEY     CORRECT REP                                  
         BNE   LRXIT                                                            
         TM    STATUS,X'40'        WANT ALL WEEKS STARTING AT QMDAY             
         BNZ   LR18                                                             
         CLC   KEY+15(3),SAVEKEY+15   ONLY WANT CORRECT WEEK                    
         BNE   LRXIT                                                            
LR18     CLI   SWITYPE,0                                                        
         BE    LR30                                                             
         CLC   KEY+18(1),SWITYPE                                                
         BNE   LRXIT                                                            
LR20     OC    OLD,OLD                                                          
         BZ    LR30                                                             
         CLC   KEY+19(7),OLD                                                    
         BNE   LRXIT                                                            
LR30     MVC   P,SPACES            USING P AS LIST BUILD AREA                   
         LA    R2,P                                                             
         USING LISTD,R2                                                         
         LA    R3,TYPTAB                                                        
LR40     CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RSWIKCHA,11(R3)                                                  
         BE    LR50                                                             
         LA    R3,L'TYPTAB(R3)                                                  
         B     LR40                                                             
LR50     MVC   LREC,0(R3)                                                       
         MVC   TYPE,11(R3)                                                      
         XC    WORK(10),WORK                                                    
         MVC   WORK(7),RSWIKOLD                                                 
         GOTO1 =A(FMTCODE),RR=Y                                                 
**>>     BAS   RE,FMTCODE                                                       
         MVC   LOLD,WORK+10                                                     
         ZIC   R1,RSWIKSEQ                                                      
         CVD   R1,DUB                                                           
         UNPK  LSEQ(3),DUB+6(2)                                                 
         OI    LSEQ+2,X'F0'                                                     
         SPACE 1                                                                
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LA    R6,RSWIELEM                                                      
         USING RSWIELEM,R6                                                      
         LA    RE,SWITYPH          SET A(RECORD TYPE)                           
         ZIC   RF,5(RE)            GET LENGTH OF INPUT                          
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,LR500020         DDSSTATION RECORD LIST?                      
         BNE   LR500040            NO  - CHECK REGULAR STN REQUEST              
         TM    RSWIDDS,X'80'       YES - DDSSTATION SWITCH REQUEST?             
         BNO   LR200               NO  - SKIP THIS RECORD                       
         B     LR500120            CONTINUE TO PROCESS                          
LR500020 CLC   8(0,RE),=C'DDSSTATION'                                           
         DS    0H                  ALIGNMENT                                    
LR500040 EQU   *                                                                
         EX    RF,LR500050         STATION RECORD LIST?                         
         BNE   LR500120            NO  - CONTINUE TO PROCESS                    
         B     LR500060                                                         
LR500050 CLC   8(0,RE),=C'STATION'                                              
LR500060 EQU   *                                                                
         TM    RSWIDDS,X'80'       DDSSTATION SWITCH REQUEST?                   
         BO    LR200               YES - SKIP THIS REQUEST                      
LR500120 EQU   *                                                                
         MVC   TYPE,RSWIKCHA                                                    
         XC    WORK(10),WORK                                                    
         MVC   WORK(7),RSWINEW                                                  
         GOTO1 =A(FMTCODE),RR=Y                                                 
**>>     BAS   RE,FMTCODE                                                       
         MVC   LNEW,WORK+10        NEW CODE                                     
         MVC   LREQ,RSWIREQN       REQUESTOR NAME                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR58                                                             
         GOTO1 DATCON,DMCB,(3,RSWISTA),(8,LSTADATE)   START DATE                
LR58     DS    0H                                                               
         MVI   ELCODE,2                                                         
         BAS   RE,FIRSTEL                                                       
         B     *+8                                                              
LR60     BAS   RE,NEXTEL                                                        
         BNE   LR100                                                            
         USING RSWIFIEL,R6                                                      
         MVC   TYPE,RSWIFTYP                                                    
         SPACE 1                                                                
         CLI   RSWIFTYP,1          TEAM                                         
         BNE   LR63                                                             
         MVC   LTEAM,RSWIFFTR                                                   
         B     LR60                                                             
         SPACE 1                                                                
LR63     CLI   RSWIFTYP,2          STATION                                      
         BNE   LR70                                                             
         LA    R5,LSTA                                                          
         MVC   0(4,R5),RSWIFFTR                                                 
         CLI   RSWIFFTR+4,C' '                                                  
         BE    LR65                                                             
         LA    R5,3(R5)                                                         
         CLI   RSWIFFTR+3,C' '                                                  
         BE    *+8                                                              
         LA    R5,1(R5)                                                         
         MVI   0(R5),C'-'                                                       
         MVC   1(1,R5),RSWIFFTR+4                                               
         SPACE 1                                                                
LR65     MVC   LGRP,RSWIFFTR+5     AND GROUP                                    
         B     LR60                                                             
         SPACE 1                                                                
LR70     CLI   RSWIFTYP,4          OFFICE                                       
         BNE   LR75                                                             
         MVC   LOFF,RSWIFFTR                                                    
         B     LR60                                                             
         SPACE 1                                                                
LR75     CLI   RSWIFTYP,8          ADVERTISER                                   
         BNE   LR77                                                             
         MVC   LADV,RSWIFFTR                                                    
         B     LR60                                                             
         SPACE 1                                                                
LR77     CLI   RSWIFTYP,X'10'      SALESPERSON                                  
         BNE   LR80                                                             
         MVC   LSAL,RSWIFFTR                                                    
         B     LR60                                                             
         SPACE 1                                                                
LR80     CLI   RSWIFTYP,X'20'      GROUP                                        
         BNE   LR85                                                             
         MVC   LGRP,RSWIFFTR                                                    
         B     LR60                                                             
         SPACE 1                                                                
LR85     CLI   RSWIFTYP,X'40'      AGENCY                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    WORK(10),WORK                                                    
         MVC   WORK(7),RSWIFFTR                                                 
         GOTO1 =A(FMTCODE),RR=Y                                                 
**>>     BAS   RE,FMTCODE                                                       
         MVC   LAGY,WORK+10                                                     
         B     LR60                                                             
         SPACE 1                                                                
LR100    CLI   MODE,PRINTREP                                                    
         BNE   LR150                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR200                                                            
         SPACE 1                                                                
LR150    MVC   LISTAR,P            FOR LIST                                     
         GOTO1 LISTMON                                                          
         SPACE 1                                                                
LR200    GOTO1 SEQ                 NEXT RECORD                                  
         LA    R4,KEY                                                           
         B     LR15                                                             
         SPACE 1                                                                
LRXIT    XIT1                                                                   
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
****************************************************************                
*                              VALIDATE TEAM                   *                
*   INPUT AS: RECORD     TEAM                                  *                
*             OLD CODE   DA/SFS/TB  (OFFICE/SALESPERSON/TEAM)  *                
*             NEW CODE   DA/SFS/TG                             *                
*        VALID FILTERS   GROUP                                 *                
*        REQ FILTERS     NONE                                  *                
*                                                              *                
*  OR WHEN TEAM IS FILTER, INPUT AS TEAM=TB                    *                
*                                                              *                
*  DON'T DELETE OLD TEAM - IT'S ALWAYS FILTERED ON SAL/OFFICE  *                
*  BUT DO DELETE OLD SALESPERSON IF NO FILTERS                 *                
****************************************************************                
         SPACE 1                                                                
VALTEM   NTR1                                                                   
         TM    STATUS,X'02'        TEAM IS FILTER                               
         BO    VT50                                                             
*                       EXTRACT OFFICE/SALESPERSON/TEAM                         
*                       AND VALIDATE THROUGH SALESPERSON RECORD                 
         BAS   RE,VALSAL                                                        
         B     VTXIT                                                            
         SPACE 1                                                                
*   VALIDATE OFFICE RECORD                                                      
         SPACE 1                                                                
VT50     MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         MVI   KEY,5                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),WORK                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         SPACE 1                                                                
         MVC   WORK+10(2),KEY+25                                                
VTXIT    B     XIT                                                              
         EJECT                                                                  
****************************************************************                
*             GET STATION INTO CORRECT FORMAT FOR VALIDATING   *                
*                 AND CHECK FOR STATION/GROUP OR               *                
*                               STATION/AFFILIATE              *                
****************************************************************                
STAFMT   NTR1                                                                   
         OC    WORK(10),SPACES                                                  
         XC    STATION,STATION                                                  
         LA    R5,WORK+3                                                        
         MVC   STATION(3),WORK                                                  
         CLI   0(R5),C'/'          3 CHAR TV, NO BAND                           
         BNE   SF2                                                              
         BCTR  R5,0                                                             
         B     SF5                                                              
SF2      CLI   0(R5),C'-'                                                       
         BNE   SF3                                                              
         MVC   STATION+4(1),1(R5)                                               
         LA    R5,1(R5)                                                         
         B     SF5                                                              
SF3      MVC   STATION+3(1),0(R5)                                               
         CLI   1(R5),C'-'                                                       
         BNE   SF5                                                              
         MVC   STATION+4(1),2(R5)                                               
         LA    R5,2(R5)                                                         
SF5      OC    STATION(5),SPACES                                                
         CLI   STATION+4,C'A'     AM                                            
         BE    SF10                                                             
         CLI   STATION+4,C'F'     FM                                            
         BE    SF10                                                             
         CLI   STATION+4,C'C'     CM                                            
         BE    SF10                                                             
         CLI   STATION+4,C' '     TV                                            
         BE    SF10                                                             
         CLI   STATION+4,C'L'     TV                                            
         BE    SF10                                                             
         CLI   STATION+4,C'T'     TV                                            
         BNE   SF7                                                              
         MVI   STATION+4,C' '                                                   
         B     SF10                                                             
         SPACE 1                                                                
SF7      MVI   ERROR,2             INVALID INPUT                                
         B     ERREND                                                           
         SPACE 1                                                                
SF10     CLI   TYPE,X'80'          COMPETING STATION                            
         BE    SF20                                                             
         CLI   1(R5),C'/'          STATION/GROUP                                
         BE    *+14                                                             
         MVC   CONHEAD+10(L'MISSGRP),MISSGRP                                    
         B     MYEND                                                            
         MVC   WORK(2),2(R5)                                                    
         OC    WORK(2),SPACES                                                   
         B     SFX                                                              
         SPACE 1                                                                
SF20     CLI   1(R5),C'/'          COMP STATION/AFFILIATE                       
         BE    SF40                                                             
         CLI   STATION+4,C' '      RADIO DOESN'T HAVE TO HAVE AFF               
         BE    SF30                                                             
         CLI   1(R5),0                                                          
         BNE   SF30                                                             
         MVC   WORK(3),SPACES                                                   
         B     SFX                                                              
         SPACE 1                                                                
SF30     MVC   CONHEAD+10(L'MISSAFF),MISSAFF                                    
         B     MYEND                                                            
         SPACE 1                                                                
SF40     MVC   WORK(3),2(R5)                                                    
         OC    WORK(3),SPACES                                                   
         GOTO1 =A(AFFCHK),RR=Y     VALIDATE AFFILIATE                           
         BNZ   MYEND               ERROR RETURN                                 
SFX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
****************************************************************                
*                              VALIDATE OFFICE                 *                
*   INPUT AS: RECORD     OFF                                   *                
*             OLD CODE   DA/SFS/TB (OFFICE/SALESPERSON/TEAM)   *                
*             NEW CODE   HO/SSL/TB                             *                
*                                                              *                
*        VALID FILTERS   AGENCY, ADVERTISER, STATION, GROUP    *                
*        REQ FILTERS     NONE                                  *                
*                                                              *                
*    OR WHEN OFFICE IS FILTER, INPUT AS OFF=DA                 *                
*                                                              *                
*  DON'T DELETE OFFICE - IT'S ALWAYS FILTERED ON SAL/TEAM      *                
*  BUT DO DELETE OLD SALESPERSON IF NO FILTERS                 *                
****************************************************************                
         SPACE 1                                                                
VALOFF   NTR1                                                                   
         TM    STATUS,X'02'        OFFICE IS FILTER                             
         BO    VO50                                                             
         SPACE 1                                                                
*                       EXTRACT OFFICE/SALESPERSON/TEAM                         
*                     & VALIDATE THROUGH SALESPERSON RECORD                     
         SPACE 1                                                                
         BAS   RE,VALSAL                                                        
         B     VOXIT                                                            
         SPACE 1                                                                
*  VALIDATE OFFICE RECORD                                                       
         SPACE 1                                                                
VO50     MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         MVI   KEY,4                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),WORK                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         SPACE 1                                                                
         MVC   WORK+10(2),KEY+25                                                
VOXIT    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*                              VALIDATE ADVERTISER                   *          
*   INPUT AS: RECORD     ADV                                         *          
*             OLD CODE   PG                                          *          
*             NEW CODE   PNG                                         *          
*        VALID FILTERS   GROUP,STATION,TEAM,AGENCY,SALESPERSON,OFFICE*          
*                        (BUT ONLY IF ADVERTISER HAS NO PRODUCTS)    *          
*        REQ FILTERS     NONE                                        *          
*                                                                    *          
*  DELETE OLD ADVERTISER (AND ANY PRODUCTS FOR THAT ADVERTISER) IF   *          
*  NO FILTERS                                                        *          
**********************************************************************          
         SPACE 1                                                                
VALADV   NTR1                                                                   
         MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         MVI   KEY,8                                                            
         MVC   KEY+21(4),WORK                                                   
         OC    KEY+21(4),SPACES                                                 
         MVC   KEY+25(2),AGENCY                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         SPACE 1                                                                
         MVC   WORK+10(4),KEY+21                                                
         TM    STATUS,X'02'        ADV  IS FILTER - NO OTHER TESTS              
         BO    VADV0200                                                         
         SPACE 1                                                                
****************************************************************                
*            WHEN VALIDATING NEW ADVERTISER RECORD -           *                
*         MAKE SURE ALL OLD PRODUCTS HAVE BEEN ADDED ON THE    *                
*         NEW ADVERTISER                                       *                
****************************************************************                
         SPACE 1                                                                
         TM    STATUS,X'04'        VALIDATING NEW CODE                          
         BNO   VADV0020            NO  - DOING OLD CODE                         
*                                  YES - CHECK 'DO NOT USE' ON NEW CODE         
         LA    R2,SWINEWH          POINT CURSOR TO NEW ADV.                     
         GOTO1 =A(NEWADVNO),DMCB,(RC),RR=Y                                      
         CLI   DMCB,0                                                           
         BNE   MYEND                                                            
         B     VADV0020            NO ERROR: PROCESS NEW ADV                    
VADV0020 EQU   *                                                                
         XC    BLOC(250),BLOC                                                   
         XC    BLOC+250(250),BLOC+250                                           
         XC    BLOC+500(250),BLOC+500                                           
         LA    R5,BLOC                                                          
         MVI   BYTE,0              ROOM FOR 250 PRODUCTS                        
         XC    KEY,KEY                                                          
         MVI   KEY,9                                                            
         MVC   KEY+18(4),OLD                                                    
         GOTO1 HIGH                                                             
         B     VADV0060                                                         
VADV0040 GOTO1 SEQ                                                              
VADV0060 CLC   KEY(22),KEYSAVE     SAME ADVERTISER                              
         BNE   VADV0080                                                         
         CLC   KEY+25(2),AGENCY     CORRECT REP                                 
         BNE   VADV0040                                                         
         MVC   0(3,R5),KEY+22                                                   
         ZIC   RE,BYTE             COUNT NUMBER OF PRODUCTS                     
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         CH    RE,=H'250'          INCREMENT NUMBER OF PRODUCTS                 
         BNH   *+6                                                              
         DC    H'0'                TOO MANY PRODUCTS                            
         LA    R5,3(R5)            NEXT SPACE IN BLOC                           
         B     VADV0040            SEE IF THERE ARE ANY MORE PRODUCTS           
         SPACE 1                                                                
VADV0080 OC    BLOC(250),BLOC      CHECK FIRST PART OF SPACE                    
         BZ    VADV0200                                                         
         LA    R5,BLOC                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,9                                                            
         MVC   KEY+18(4),WORK+10   NEW ADVERTISER                               
         GOTO1 HIGH                                                             
         B     VADV0120                                                         
VADV0100 GOTO1 SEQ                                                              
VADV0120 CLC   KEY(22),KEYSAVE     SAME ADVERTISER                              
         BNE   VADV0140                                                         
         CLC   KEY+25(2),AGENCY    CORRECT REP                                  
         BNE   VADV0100                                                         
         CLC   KEY+22(3),0(R5)                                                  
         BNE   VADV0100                                                         
         LA    R5,3(R5)                                                         
         ZIC   RE,BYTE                                                          
         BCTR  RE,0                                                             
         STC   RE,BYTE                                                          
         B     VADV0100                                                         
         SPACE 1                                                                
VADV0140 CLI   BYTE,0                                                           
         BE    VADV0200                                                         
         MVC   CONHEAD+10(L'MISSPRD),MISSPRD                                    
*                 'MISSING PRODUCT XXX FOR NEW ADVERTISER'                      
         MVC   CONHEAD+26(3),0(R5)       FILL IN CORRECT PRODUCT                
         LA    R2,SWINEWH          POINT CURSOR TO NEW ADV.                     
         B     MYEND                                                            
         SPACE 1                                                                
VADV0200 B     XIT                                                              
         EJECT                                                                  
****************************************************************                
*                        VALIDATE STATION/GROUP                *                
*  (SEE VALCOM FOR INPUT)      AND COMPETING STATION/AFFILIATE *                
*                                                              *                
*   INPUT AS: RECORD          STA/GROUP                        *                
*             OLD CODE        WLIT-F/TA      WLIT-F/TA         *                
*                                        OR                    *                
*             NEW CODE        WLTW-F/TA      WLTW-F/TC         *                
*             VALID FILTERS   NONE                             *                
*             REQ FILTERS     NONE                             *                
*                                                              *                
*  DELETE OLD STATION RECORD                                   *                
****************************************************************                
         SPACE 1                                                                
VALSTA   NTR1                                                                   
         MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),AGENCY                                                 
         SPACE 1                                                                
         BAS   RE,STAFMT           GET STATION INTO CORRECT FORMAT              
         MVC   KEY+22(5),STATION                                                
         SPACE 1                                                                
VSTA10   GOTO1 HIGH                                                             
*                                                                               
         TM    STATUS,X'04'        VALIDATING NEW CODE?                         
         BO    VSTA14              YES - COMPARE AGAINST OLD                    
         CLC   KEY(27),KEYSAVE     NO  - IS OLD KEY ON FILE?                    
         BE    VSTA16              YES                                          
         TM    STATUS,X'02'        VALIDATING FILTERS?                          
         BZ    ERREND              NO, STATION DOESN'T EXIST: ERROR             
*                                                                               
* FOR FILTERS--MUST CHECK IF VALID SUBID FILTER FOR MASTER REC SWITCH           
*                                                                               
         GOTO1 =A(MRTTEST),(RC),RR=Y  CHECK FOR MASTER AND RECORD TYPE          
         BNE   ERREND              DIDN'T PASS TESTING                          
*                                                                               
*     FIRST 2 BYTES OF WORK ARE THE GROUP                                       
*     SUBSCAN NOT TOTALLY MODULAR BECAUSE IT USES THE GROUP TO CHECK            
*     IF IT IS THE CORRECT STATION                                              
         GOTO1 =A(SUBSCAN),DMCB,(RC),20,RR=Y  CHECK SUBSIDS--20=DISP            
*                                             OF REP IN STA RECORD              
         BNE   ERREND              NOT ON SUBSID                                
         B     VSTA16              YES - GO ON                                  
VSTA14   DS    0H                                                               
         CLI   KEYSAVE+26,C'C'     NEW A COMBO STN?                             
         BNE   *+22                                                             
         CLI   OLD+4,C'C'          THEN OLD MUST BE ALSO                        
         BE    *+14                                                             
         MVC   CONHEAD+10(L'NOTCOMBO),NOTCOMBO                                  
         B     MYEND                                                            
*                                                                               
         MVC   WORK+10(5),KEYSAVE+22                                            
         MVC   WORK+15(2),WORK                                                  
         CLC   KEY(27),KEYSAVE     IS KEY ON FILE?                              
         BNE   VSTA15              NO                                           
*                                  YES - KEY ON FILE - NOW MUST CHECK           
*                                     IF 'STATION' (DON'T PERMIT) OR            
*                                        'DDSSTATION'    (PERMIT)               
***      CLI   1(RA),C'*'          CHECK FOR DDS TERMINAL                       
***      BE    VSTA15              DDS TERMINAL CAN OVERRIDE                    
         LA    RE,SWITYPH          SET A(RECORD TYPE)                           
         ZIC   RF,5(RE)            GET LENGTH OF INPUT                          
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,VSTA1420         COMPARE BY LENGTH                            
         BNE   VSTA1440                                                         
         B     VSTA16              CHECK GROUP SAME AS STATION                  
*                                     FOR 'DDSSTATION' REQUEST                  
VSTA1420 CLC   8(0,RE),=C'DDSSTATION'                                           
         DS    0H                  ALIGNMENT                                    
VSTA1440 EQU   *                                                                
         MVC   CONHEAD+10(L'TRGEXIST),TRGEXIST                                  
         B     MYEND                                                            
         SPACE 1                                                                
VSTA15   DS    0H                                                               
         LA    RE,SWITYPH          SET A(RECORD TYPE)                           
         ZIC   RF,5(RE)            GET LENGTH OF INPUT                          
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,VSTA1420         COMPARE BY LENGTH                            
         BNE   VSTA1540            NOT DDSSTATION - PROCEED                     
*                                     VALIDATE GROUP EXISTENCE                  
         MVC   CONHEAD+10(L'NEEDSTAT),NEEDSTAT                                  
         B     MYEND                                                            
         SPACE 1                                                                
VSTA1540 EQU   *                   GROUP EXISTS FOR 'STATION' TYPE              
         XC    KEY,KEY                                                          
         MVI   KEY,X'07'                                                        
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),WORK                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    VSTA20                                                           
         MVC   CONHEAD+10(L'BADGRP),BADGRP    NOT CORRECT GROUP                 
         B     MYEND                              FOR STATION                   
         SPACE 1                                                                
*                                                                               
VSTA16   MVC   WORK+10(5),KEY+22                                                
         GOTO1 GETREC              RETRIEVE STATION RECORD                      
         OI    STATUS,X'80'        SET STATUS FLAG                              
         L     R6,AIO                                                           
         USING RSTAREC,R6                                                       
*                                  REQUEST TYPE = DDSSTATION?                   
         LA    RE,SWITYPH          SET A(RECORD TYPE):DDSSTATION?               
         ZIC   RF,5(RE)            GET LENGTH OF INPUT                          
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,VSTA1420         COMPARE BY LENGTH                            
         BNE   VSTA1699            NOT DDSSTATION - SKIP NEXT ROUTINE           
         TM    STATUS,X'04'        NEW STATION?                                 
         BO    VSTA1605            YES -                                        
         XC    OLDSIGN(4),OLDSIGN  CLEAR AREA WHEN OLD VALIDATION               
VSTA1605 EQU   *                                                                
         LA    RF,RSTAELEM                                                      
VSTA1610 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BNE   VSTA1612            NO  - CHECK ELEMENT TYPE                     
         TM    STATUS,X'04'        NEW STATION?                                 
         BNO   VSTA1699            NO  - CONTINUE                               
         B     VSTA1630            YES - NO X'05' ELEMENT                       
*                                     COMPARE OLD AND NEW RESULTS               
VSTA1612 EQU   *                                                                
         CLI   0(RF),5             EXTENDED DESCRIP ELEMENT?                    
         BE    VSTA1620            YES                                          
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     VSTA1610            GO BACK FOR NEXT                             
VSTA1620 EQU   *                                                                
         TM    STATUS,X'04'        NEW STATION?                                 
         BO    VSTA1625            YES -                                        
         MVC   OLDSIGN,10(RF)      MOVE SIGNON FROM OLD STATION                 
         B     VSTA1699            FOR OLD CODE, DON'T VALIDATE                 
*                                     LIKE TO LIKE SIGNONS                      
VSTA1625 EQU   *                                                                
         MVC   NEWSIGN,10(RF)      MOVE SIGNON FROM NEW STATION                 
*                                                                               
*   NOW VALIDATE OLD VS NEW:                                                    
*        IF BOTH 0406, BOTH GRAPHNET  =  OKAY                                   
*        IF BOTH NULL, BOTH UNSET     =  OKAY                                   
*        IF 0406 AND OTHER NOT        =  NO GOOD                                
*                                                                               
VSTA1630 EQU   *                                                                
         OC    OLDSIGN(4),OLDSIGN  BOTH NOT SET?                                
         BZ    VSTA1699            YES - ACCEPT                                 
*                                  AT THIS POINT,                               
*                                     ONE OR BOTH ARE SET                       
*                                                                               
         OC    OLDSIGN,OLDSIGN     OLD SIGN NOT SET?                            
         BZ    VSTA1670            YES - NEW SIGN SET = ERROR                   
         OC    NEWSIGN,NEWSIGN     NEW SIGN NOT SET?                            
         BZ    VSTA1670            YES - OLD SIGN SET = ERROR                   
         CLC   =X'0406',OLDSIGN    OLD = GRAPHNET?                              
         BE    VSTA1650            YES                                          
         CLC   =X'0406',NEWSIGN    NO  - IS NEW SIGN GRAPHNET?                  
         BNE   VSTA1699            NO  - ACCEPT IT                              
VSTA1650 EQU   *                                                                
         CLC   =X'0406',NEWSIGN    YES - NEW = GRAPHNET?                        
         BE    VSTA1699            YES - ACCEPT IT                              
VSTA1670 EQU   *                                                                
         MVC   CONHEAD+10(L'NOTLIKE),NOTLIKE   NOT SAME SIGNON'S                
         B     MYEND                              FOR STATION                   
VSTA1699 EQU   *                                                                
*                                                                               
         CLC   =C'AGENCY',SWITYP   AGENCY SWITCH ?                              
         BE    VSTA1698                                                         
         CLC   =C'AGY',SWITYP      AGENCY SWITCH ?                              
         BE    VSTA1698                                                         
         CLC   =C'ADV',SWITYP      ADVERT SWITCH ?                              
         BE    VSTA1698                                                         
*                                                                               
         CLC   =C'SAL',SWITYP      SALESPERSON SWITCH?                          
         BNE   VSTA1700            NO  -  CHECK GROUP                           
*                                                                               
VSTA1698 CLC   AGENCY,=C'IR'       IS REP IR?                                   
         BE    VSTA1750            YES - DON'T CHECK GROUP                      
         CLC   AGENCY,=C'K3'       K3?                                          
         BE    VSTA1750            YES - DON'T CHECK GROUP                      
         CLC   AGENCY,=C'MR'       MR?                                          
         BE    VSTA1750            YES - DON'T CHECK GROUP                      
         CLC   AGENCY,=C'MS'       MS?  (MASTER/SUBX TEST)                      
         BE    VSTA1750            YES - DON'T CHECK GROUP                      
VSTA1700 EQU   *                                                                
         CLC   RSTAGRUP,WORK                                                    
         BNE   *+14                                                             
         MVC   WORK+15(2),WORK                                                  
VSTA1750 EQU   *                                                                
         B     VSTA20                                                           
         MVC   CONHEAD+10(L'BADGRP),BADGRP    NOT CORRECT GROUP                 
         B     MYEND                              FOR STATION                   
         SPACE 1                                                                
*                                                                               
VSTA20   TM    SWITYPE,X'80'          COMPETING STATION                         
         BZ    VSTAXIT                                                          
         TM    STATUS,X'04'        DIFFERENT VALIDATION IF NEW CODE             
         BO    VSTA70                                                           
         MVI   ELCODE,2            OLD COMP STATION MUST EXIST                  
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VSTA30   BAS   RE,NEXTEL                                                        
         BE    VSTA40                                                           
         MVC   CONHEAD+10(L'BADCOMP),BADCOMP    INVALID COMP STATION            
         B     MYEND                            FOR STATION                     
         USING RSTAMKEL,R6                                                      
*                                                                               
VSTA40   CLC   RSTAMKST,OLD                                                     
         BNE   VSTA30                                                           
         CLC   =C'C*',OLD+5        SPECIAL TEST FOR CABLE?                      
         BNE   VSTA50              NO                                           
         CLC   RSTAMKAF(3),=C'CBL' YES - IS IT CABLE?                           
         BNE   VSTA60              NO  - ERROR                                  
         B     VSTAXIT             YES - ACCEPT IT                              
VSTA50   EQU   *                                                                
         CLC   RSTAMKAF(2),OLD+5                                                
         BE    VSTAXIT                                                          
VSTA60   EQU   *                                                                
         MVC   CONHEAD+10(L'BADCAFF),BADCAFF    NOT CORRECT COMP                
         LA    R2,SWIOLDH                    AFFILATE FOR COMP STATION          
         B     MYEND                                                            
         DROP  R6                                                               
*                                                                               
VSTA70   L     R6,AIO              NEW COMP STATION CAN NOT EXIST               
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VSTA80   BAS   RE,NEXTEL                                                        
         BNE   VSTAXIT                                                          
         USING RSTAMKEL,R6                                                      
*                                                                               
VSTA90   CLC   RSTAMKST,NEW                                                     
         BNE   VSTA80                                                           
         MVC   CONHEAD+10(L'BADCOMP2),BADCOMP2                                  
         LA    R2,SWINEWH                                                       
         B     MYEND                                                            
VSTAXIT  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*                     VALIDATE OFFICE/SALESPERSON/TEAM         *                
*   INPUT AS: RECORD     SAL                                   *                
*             OLD CODE   NY/SFS/TB   (OFFICE,SALESPERSON,TEAM) *                
*             NEW CODE   NY/SSL/TB                             *                
*        VALID FILTERS   AGENCY,ADVERTISER, STATION, GROUP     *                
*        REQ FILTERS     NONE                                  *                
*                                                              *                
*  OR WHEN SALESPERSON IS FILTER, INPUT AS SAL=SSL             *                
*                                                              *                
*  THIS ROUTINE IS ALSO USED TO VALIDATE OFFICE AND TEAM WHEN  *                
*     DOING AN OFFICE SWITCH OR A TEAM SWITCH                  *                
*                                                              *                
*  DELETE OLD SALESPERSON IF NO FILTERS                        *                
*  NOTE: OLD SALESPERSON CAN ALSO BE DELETED WHEN RECORD IS    *                
*  OFFICE OR TEAM, IF THOSE RECORDS HAVE NO FILTERS            *                
****************************************************************                
         SPACE 1                                                                
VALSAL   NTR1                                                                   
         TM    STATUS,X'02'        SALESPERSON IS FILTER                        
         BO    VS50                SO INPUT IS JUST SALESPERSON                 
         SPACE 1                                                                
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK),C',=/='  CHANGE DELIMITER            
         LA    R4,BLOCK                                                         
         TM    STATUS,X'04'        NEW CODE?                                    
         BZ    VS10                NO                                           
         CLI   DMCB+4,3      MUST BE OFFICE, SALESPERSON, TEAM FIELDS           
         BE    VS20                                                             
         MVC   CONHEAD+10(L'BADFRMT2),BADFRMT2                                  
         B     MYEND                                                            
*                                                                               
VS10     DS    0H                                                               
         CLI   DMCB+4,2      MUST BE OFFICE, SALESPERSON FIELDS                 
         BE    *+14                                                             
         MVC   CONHEAD+10(L'BADFRMT),BADFRMT                                    
         B     MYEND                                                            
VS20     DS    0H                                                               
         CLI   0(R4),2                                                          
         BNH   *+14                                                             
         MVC   CONHEAD+10(L'BADOFF),BADOFF                                      
         B     MYEND                                                            
         MVC   OFFICE,12(R4)                                                    
         SPACE 1                                                                
         LA    R4,32(R4)                                                        
         CLI   0(R4),3                                                          
         BNH   *+14                                                             
         MVC   CONHEAD+10(L'BADSAL),BADSAL                                      
         B     MYEND                                                            
         MVC   SAL,12(R4)                                                       
         MVC   WORK(3),12(R4)                                                   
*                                                                               
         MVC   TEAM,SPACES                                                      
         TM    STATUS,X'04'        NEW CODE?                                    
         BZ    VS50                NO                                           
         LA    R4,32(R4)           NEW CODE REQUIRES TEAM                       
         CLI   0(R4),2                                                          
         BNH   *+14                                                             
         MVC   CONHEAD+10(L'BADTEM),BADTEM                                      
         B     MYEND                                                            
         MVC   TEAM,12(R4)                                                      
         SPACE 1                                                                
VS50     MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         MVI   KEY,6                                                            
         MVC   KEY+22(2),AGENCY                                                 
         MVC   KEY+24(3),WORK                                                   
         OC    KEY+24(3),SPACES                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         TM    STATUS,X'02'        SALESPERSON IS FILTER                        
         BZ    VS70                                                             
         MVC   WORK+10(3),KEY+24                                                
         B     VSALX                                                            
*                                                                               
VS70     GOTO1 GETREC                                                           
         OI    STATUS,X'80'                                                     
         L     R6,AIO                                                           
         USING RSALREC,R6                                                       
*                                                                               
         CLC   TEAM,SPACES                                                      
         BE    VS80                                                             
         CLC   RSALTEAM,TEAM                                                    
         BE    *+14                                                             
         MVC   CONHEAD+10(L'BADTEM),BADTEM                                      
         B     MYEND                                                            
*                                                                               
VS80     DS    0H                                                               
         CLC   RSALOFF,OFFICE                                                   
         BE    *+14                                                             
         MVC   CONHEAD+10(L'BADOFF),BADOFF                                      
         B     MYEND                                                            
*                                                                               
         MVC   WORK+10(2),OFFICE                                                
         MVC   WORK+12(3),SAL                                                   
         MVC   WORK+15(2),TEAM                                                  
*                                                                               
VSALX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*                     VALIDATE DEVELOPMENTAL SALESPERSON       *                
*   INPUT AS: RECORD     DEVSAL                                *                
*             OLD CODE   CJ                                    *                
*             NEW CODE   JJ                                    *                
*        VALID FILTERS   AGENCY,ADVERTISER, STATION, GROUP     *                
*        REQ FILTERS     NONE                                  *                
*                                                              *                
*  DELETE OLD DEV SALESPERSON IF NO FILTERS                    *                
****************************************************************                
         SPACE 1                                                                
VALDSP   NTR1                                                                   
         TM    STATUS,X'02'        VALIDATING AS FILTER?                        
         BZ    *+14                NO, GO ON                                    
*                                                                               
         MVC   CONHEAD+10(L'NOTFILT),NOTFILT  YES, DEVSAL CAN'T BE FILT         
         B     MYEND                                                            
*                                                                               
         MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         MVI   KEY,X'3A'                                                        
         MVC   KEY+22(2),AGENCY                                                 
         MVC   KEY+24(3),WORK                                                   
         OC    KEY+24(3),SPACES                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         MVC   WORK+10(3),KEY+24                                                
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
*                              VALIDATE POINT PERSON           *                
*   INPUT AS: RECORD     POINT                                 *                
*             OLD CODE   JOE                                   *                
*             NEW CODE   WSB                                   *                
*        VALID FILTERS   ADVERTISER                            *                
*        REQ FILTERS     NONE                                  *                
*                                                              *                
*  DELETE OLD POINT PERSON IF NO FILTERS                       *                
****************************************************************                
         SPACE 1                                                                
VALPTP   NTR1                                                                   
         TM    STATUS,X'02'        VALIDATING AS FILTER?                        
         BZ    *+14                NO, GO ON                                    
*                                                                               
         MVC   CONHEAD+10(L'NOTFILT),NOTFILT  YES, POINT CAN'T BE FILT          
         B     MYEND                                                            
*                                                                               
         MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         MVI   KEY,X'31'                                                        
         MVC   KEY+22(2),AGENCY                                                 
         MVC   KEY+24(3),WORK                                                   
         OC    KEY+24(3),SPACES                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         MVC   WORK+10(3),KEY+24                                                
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
*                              VALIDATE GROUP                  *                
*   INPUT AS: RECORD     GRO  OR GRP                           *                
*             OLD CODE   RA                                    *                
*             NEW CODE   RF                                    *                
*        VALID FILTERS   STATION                               *                
*        REQ FILTERS     STATION                               *                
*                                                              *                
*  NEVER DELETE OLD GROUP                                      *                
****************************************************************                
         SPACE 1                                                                
VALGRP   DS    0H'0'                                                            
         ST    RE,FULL                                                          
         MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         MVI   KEY,7                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),WORK                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         SPACE 1                                                                
         MVC   WORK+10(2),KEY+25                                                
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
****************************************************************                
*                              VALIDATE AGENCY                 *                
*   INPUT AS: RECORD     AGY                                   *                
*             OLD CODE   NH       NH-LA        NH              *                
*             NEW CODE   DDBB     DDBB-LA      NH-LA           *                
*        VALID FILTERS   ADVERTISER, GROUP, STATION, OFFICE    *                
*                        SALESPERSON, TEAM                     *                
*        REQ FILTERS     NONE                                  *                
*                                                              *                
* IF OLD IS INPUT AS AGENCY-OFFICE, DELETE IT                  *                
* IF OLD IS INPUT AS AGENCY, & THERE ARE NO OFFICES, DELETE IT *                
****************************************************************                
         SPACE 1                                                                
VALAGY   NTR1                                                                   
         MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         MVI   KEY,X'0A'                                                        
*                                                                               
         LA    RE,5                                                             
         LA    R1,WORK                                                          
         LA    R5,KEY+19                                                        
VAGY0020 CLI   0(R1),C' '          WORK IS SPACES FILLED                        
         BE    VAGY0100                                                         
         CLI   0(R1),C'-'                                                       
         BNE   VAGY0040                                                         
         CLI   3(R1),C' '          OFFICE IS MAX 2 CHAR                         
         BNE   VAGY0060                                                         
         B     VAGY0080                                                         
VAGY0040 MVC   0(1,R5),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    R5,1(R5)                                                         
         BCT   RE,VAGY0020         AGENCY IS MAX 4 CHAR                         
VAGY0060 MVC   CONHEAD+10(L'BADAOF),BADAOF                                      
         B     MYEND                                                            
VAGY0080 MVC   KEY+23(2),1(R1)                                                  
VAGY0100 EQU   *                                                                
         OC    KEY+19(6),SPACES                                                 
*                                                                               
         CLI   AGYCORP,C'A'        NEW AGENCY BEING TESTED?                     
         BNE   VAGY0120            NO                                           
         CLC   KEY+23(2),SPACES    YES - ANY AGENCY/OFFICE?                     
         BH    VAGY0120            YES - LEAVE IT ALONE                         
         MVI   AGYCORP,C'B'        NO  - SET FLAG TO CORPORATE                  
VAGY0120 EQU   *                                                                
         MVC   KEY+25(2),AGENCY                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         CLI   AGYCORP,C' '        NEW CODE IN PROGRESS?                        
         BNH   VAGY0130            NO                                           
         MVC   SAVAIO2,AIO         YES - SAVE CURRENT IO ADDRESS                
         MVC   AIO,AIO2            SET INPUT TO AIO2                            
         GOTO1 GETREC              READ AGENCY RECORD                           
         L     RF,GETREC                                                        
         BASR  RE,RF                                                            
         L     R6,AIO                                                           
         USING RAGYREC,R6                                                       
         TM    RAGYFLAG,X'02'      'DO NOT USE' SET?                            
         BO    VAGY0400            YES - EXIT WITH ERROR                        
         DROP  R6                                                               
         MVC   AIO,SAVAIO2         RESTORE CURRENT IO ADDR                      
         OI    STATUS,X'80'        SET STATUS FLAG                              
         CLI   AGYCORP,C'B'        CORPORATE BEING PROCESSED?                   
         BNE   VAGY0130            NO                                           
         MVC   AAGYCORP,KEY+28     YES - SAVE DISK ADDR                         
*                                                                               
         MVI   AGYCORP,C' '        CLEAR 'NEW CODE IN TEST' FLAG                
*                                                                               
VAGY0130 EQU   *                                                                
         SPACE 1                                                                
         MVC   WORK+10(6),KEY+19                                                
         SPACE 2                                                                
* IF OLD CODE IS INPUT AS AGENCY/OFFICE, MARK OLD CODE FOR DELETE AND           
*         HAVE A TABLE WITH NO OFFICES (TREAT AS IF OLD CODE HAS                
*         NO OFFICE)                                                            
*         (THEN BRANCH TO *A !!)                                                
*                                                                               
* IF OLD CODE IS INPUT AS JUST AGENCY, CHECK TO SEE IF THERE ARE                
* AGENCY/OFFICES                                                                
*                                                                               
*      IF THERE ARE OFFICES FOR THE OLD CODE, THEN                              
*         IT'S AN ERROR--MUST ENTER EACH OLD OFFICE SEPARATELY                  
*         FOR MULTI-OFFICE (MAY BE SOME IRRELEVANT CODE LEFT IN                 
*         DUE TO ADDING THIS FEATURE)                                           
*                                                                               
****      BUILD A TABLE OF THOSE OFFICES                                        
****                                                                            
****              IF THE NEW CODE IS AGY/OFFICE, THEN OLD OFFICES               
****                 GET CHANGED TO THE ONE NEW OFFICE                          
****              IF THE NEW CODE IS AGY AND NO OFFICES EXIST, THEN             
****                 OLD OFFICES GET CHANGED TO BE NO OFFICE                    
****              IF THE NEW CODE IS AGY AND OFFICES DO EXIST, THEN             
****                 A NEW OFFICE MUST EXIST FOR EVERY OLD OFFICE               
*                                                                               
*      IF THERE ARE NO OFFICES FOR THE OLD CODE,                                
*           MARK OLD CODE FOR DELETE                                            
*A                IF THE NEW CODE IS AGY/OFFICE, THEN THE OLD CODE              
*                    GETS CHANGED TO THE NEW AGENCY/OFFICE                      
*                 IF THE NEW CODE IS AGY AND NO OFFICES EXIST, THEN             
*                    THE OLD CODE GETS CHANGED TO THE NEW ONE                   
*                 IF THE NEW CODE IS AGY AND OFFICES DO EXIST, THEN             
*                     IT'S AN ERROR.  MULTI-OFFICE AGENCIES REQUIRE             
*                     AN OFFICE, AND IF THE OLD ONE DOESN'T HAVE ANY            
*                     THE NEW ONE MUST PROVIDE IT.                              
         SPACE 2                                                                
         TM    STATUS,X'04'        VALIDATING NEW CODE                          
         BNO   VAGY0900                                                         
         SPACE 1                                                                
         XC    BLOC(250),BLOC                                                   
         XC    BLOC+250(200),BLOC+250                                           
         LA    R5,BLOC                                                          
         SPACE 1                                                                
         CLC   OLD+4(2),SPACES     DOES AGENCY HAVE OFFICE INPUT                
         BE    VAGY0140                                                         
         MVI   AGYCLC,5            YES, COMPARE AGENCY/OFFICE                   
         MVI   AGYDEL,C'Y'            DELETE OLD CODE                           
         MVI   0(R5),X'FF'            MARK END OF TABLE (NO OFFICES)            
         B     VAGY0220                                                         
         SPACE 1                                                                
VAGY0140 MVC   KEY+19(6),OLD       NO, SO LOOK UP OLD CODE                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     (IT WAS THERE BEFORE)                        
         BE    *+6                                                              
         DC    H'0'                                                             
VAGY0160 GOTO1 SEQ                 TO SEE IF THERE ARE OFFICES                  
         CLC   KEY(23),KEYSAVE     (SAME AGENCY)                                
         BNE   VAGY0180                                                         
         CLC   KEY+25(2),AGENCY    (SAME REP)                                   
         BNE   VAGY0160                                                         
****     MVC   0(2,R5),KEY+23      AND SAVE OLD AGENCY OFFICES                  
****     LA    R5,2(R5)                                                         
****     B     VAGY0160                                                         
         B     VAGY0380            ERROR--MUST ENTER OLD OFFICES                
*                                  SEPARATELY FOR MULTI-OFFICE                  
         SPACE 1                                                                
VAGY0180 MVI   0(R5),X'FF'         EOT                                          
         SPACE 1                                                                
VAGY80   LA    R5,BLOC                                                          
         CLI   0(R5),X'FF'                                                      
         BNE   VAGY0200                                                         
         MVI   AGYDEL,C'Y'         NO AGENCY OFFICES, SO DELETE IT              
         MVI   AGYCLC,5            COMPARE AGENCY/OFFICE                        
         B     *+8                                                              
         SPACE 1                                                                
VAGY0200 MVI   AGYCLC,3            COMPARE AGENCY ONLY                          
         SPACE 1                                                                
VAGY0220 CLC   WORK+14(2),SPACES   DOES NEW CODE HAVE OFFICE INPUT              
         BE    VAGY0240                                                         
         MVI   AGYMVC,5            YES, MOVE AGY/OFFICE                         
         B     VAGY0900                                                         
         SPACE 1                                                                
VAGY0240 LA    R5,BLOC                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),WORK+10   NO, SO LOOK UP NEW CODE                      
         MVC   KEY+25(2),AGENCY                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     (IT WAS THERE BEFORE)                        
         BE    *+6                                                              
         DC    H'0'                                                             
VAGY0260 GOTO1 SEQ                 TO SEE IF THERE ARE OFFICES                  
         SPACE 1                                                                
         CLC   KEY(23),KEYSAVE     (SAME AGENCY)                                
         BNE   VAGY0320            NO OFFICES AT ALL                            
         SPACE 1                                                                
         CLC   KEY+25(2),AGENCY    (SAME REP)                                   
         BNE   VAGY0260                                                         
         SPACE 1                                                                
         CLI   0(R5),X'FF'                                                      
         BE    VAGY0360            ERROR-MULTI OFFICE NEEDS OFFICE              
         B     VAGY0300                                                         
         SPACE 1                                                                
VAGY0280 GOTO1 SEQ                 CONTINUE MATCHING OFFICES                    
         CLC   KEY(23),KEYSAVE     (SAME AGENCY)                                
         BNE   VAGY0340                                                         
         SPACE 1                                                                
         CLC   KEY+25(2),AGENCY    (SAME REP)                                   
         BNE   VAGY0280                                                         
         SPACE 1                                                                
VAGY0300 CLC   0(2,R5),KEY+23   MUST HAVE ALL OFFICES THE OLD CODE DID          
         BNE   VAGY0280                                                         
         SPACE 1                                                                
         LA    R5,2(R5)                                                         
         CLI   0(R5),X'FF'         EOT                                          
         BNE   VAGY0280                                                         
         MVI   AGYMVC,3            REPLACE AGENCY ONLY                          
         B     VAGY0900                                                         
         SPACE 1                                                                
VAGY0320 MVI   AGYMVC,5            REPLACE AGENCY/OFFICE                        
         B     VAGY0900                                                         
         SPACE 1                                                                
VAGY0340 MVC   CONHEAD+10(L'MISSAOF),MISSAOF                                    
         MVC   CONHEAD+25(2),0(R5)     FILL IN THE MISSING OFFICE               
         B     MYEND                                                            
         SPACE 1                                                                
VAGY0360 MVC   CONHEAD+10(L'MULTOFF),MULTOFF                                    
         B     MYEND                                                            
         SPACE 1                                                                
VAGY0380 MVC   CONHEAD+10(L'ENTROFF),ENTROFF                                    
         LA    R2,SWIOLDH          PUT CURSOR IN OLD CODE FIELD                 
         B     MYEND                                                            
         SPACE 1                                                                
VAGY0400 MVC   CONHEAD+10(L'DONOTUSE),DONOTUSE                                  
         B     MYEND                                                            
         SPACE 1                                                                
VAGY0900 B     XIT                                                              
         SPACE 1                                                                
SAVAIO2  DS    A                                                                
         EJECT                                                                  
****************************************************************                
*   SAVE COMPETING STATION-AFFILIATE FOR LATER VALIDATION      *                
*  TO CHANGE :             CAFF        COMP         BOTH       *                
*  INPUT AS  : RECORD     COMP         COMP         COMP       *                
*              OLD CODE   WYNY-F/ABC   WYNY-F/ABC   WYNY-F/ABC *                
*              NEW CODE   WYNY-F/NBC   WKKK-F/ABC   WKKK-F/NBC *                
*        VALID FILTERS   STATION                               *                
*        REQ FILTERS     STATION                               *                
*              FILTER     STA=WLTW-F   STA=WLTW-F   STA=WLTW-F *                
*                                                              *                
*  DON'T DELETE ANYTHING                                       *                
****************************************************************                
         SPACE 1                                                                
VALCOM   NTR1                                                                   
         BAS   RE,STAFMT           GET STATION INTO CORRECT FORMAT              
         MVC   WORK+10(5),STATION                                               
*                      NOW VALIDATE AFFILIATE                                   
*   SPECIAL TEST FOR CBL (CABLE).                                               
*        THERE IS CONFUSION BECAUSE THE STORED IMAGE OF THE                     
*        SWITCH REQUEST ONLY SAVES THE FIRST TWO CHARACTERS                     
*        OF THE AFFILIATE CODE.  'CB' CANNOT BE DISTINGUISHED                   
*        AS 'CBS' OR 'CBL'.                                                     
*                                                                               
         CLC   WORK(3),=C'CBL'     REQUEST FOR CABLE?                           
         BNE   VALC0020            NO                                           
         MVI   WORK+1,C'*'         YES - SET TO 'C*L'                           
         B     VALC0040                                                         
*                                                                               
VALC0020 EQU   *                                                                
         GOTO1 =A(AFFCHK),RR=Y     CHECK VALID AFFILIATE                        
         BNZ   MYEND                                                            
VALC0040 EQU   *                                                                
         MVC   WORK+15(3),WORK                                                  
         B     XIT                                                              
         SPACE 3                                                                
         EJECT                                                                  
****************************************************************                
*  HEDSPECS                                                    *                
****************************************************************                
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H1,40,C'SWITCH RECORDS - WEEK OF'                                
         SSPEC H2,40,C'---------------------------------'                       
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
         SPACE 4                                                                
HOOK     NTR1                                                                   
         LA    R3,H1+64                                                         
         GOTO1 DATCON,DMCB,(3,QMDAY),(8,(R3))                                   
         MVC   H8(L'HEADING),HEADING                                            
         MVC   H8+38(L'HEADING2),HEADING2                                       
         MVC   H9(84),DASH                                                      
         B     XIT                                                              
         EJECT                                                                  
DASH     DC    84C'-'                                                           
HEADING  DC    CL37'REC OLD CODE  SEQ  NEW CODE  REQ NAME'                      
HEADING2 DC    CL46'STATION GRP SAL TM OF AGENCY  ADV   START DATE'             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
MYEND    MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVI   ERROR,X'FE'         USING MY OWN ERROR MESSAGE                   
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
*                                                                               
DUPERR   MVC   RERROR,=AL2(554)    NEW CODE=OLD CODE ERROR                      
*                                                                               
MYERR    GOTO1 MYERROR                                                          
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* MY OWN ERROR MESSAGES                                                         
         SPACE 2                                                                
NOTFILT  DC    C'INVALID FILTER FOR THIS REC TYPE'                              
WITHINNG DC    C'MUST BE "Y" "N" OR SPACE'                                      
COMPNG   DC    C'MUST BE "A" "B" OR SPACE'                                      
MISSFLT  DC    C'MISSING FILTER ON XX'                                          
BADCOMP  DC    C'WRONG COMPETING STATION FOR STATION'                           
BADCOMP2 DC    C'NEW COMPETING STATION ALREADY EXISTS'                          
BADCAFF  DC    C'WRONG COMPETING AFFIL FOR STATION'                             
MISSAFF  DC    C'MISSING AFFIL-FORMAT IS STATION-BAND/AFFIL'                    
BADAFF   DC    C'INVALID AFFIL'                                                 
MISSPRD  DC    C'MISSING PRODUCT XXX FOR NEW ADVERTISER'                        
BADTEM   DC    C'WRONG TEAM FOR SALESPRSN'                                      
MISSGRP  DC    C'MISSING GRP - FORMAT IS STATION/GRP'                           
BADGRP   DC    C'WRONG GRP FOR STATION'                                         
BADOFF   DC    C'WRONG OFFICE FOR SALESPRSN'                                    
BADSAL   DC    C'INVALID SALESPRSN'                                             
BADFRMT  DC    C'FORMAT IS OFFICE/SALESPRSN'                                    
BADFRMT2 DC    C'FORMAT IS OFFICE/SALESPRSN/TEAM'                               
MISSAOF  DC    C'MISSING OFFICE XX FOR NEW AGENCY'                              
MULTOFF  DC    C'NEW AGENCY REQUIRES OFFICE'                                    
DONOTUSE DC    C'NEW CODE MARKED "INACTIVE" - CANNOT BE USED'                   
ENTROFF  DC    C'ENTER EACH OLD OFFICE SEPARATELY FOR MULTI-OFFICE'             
BADAOF   DC    C'FORMAT IS AGENCY OR AGENCY-OFFICE'                             
TMANYREP DC    C'TOO MANY SWITCH RECS ON FILE FOR THIS USER'                    
TMANYFIL DC    C'TOO MANY SWITCH RECS ON FILE'                                  
SUBCANT  DC    C'SUBSID REP CAN''T SWITCH THIS REC TYPE'                        
MSTCANT  DC    C'MASTER REP CAN''T SWITCH THIS REC TYPE'                        
TRGEXIST DC    C'TARGET STATION EXISTS'                                         
NEEDSTAT DC    C'DDSSTATION: TARGET STATION MUST BE ON FILE'                    
NOTCOMBO DC    C'OLD RECORD NOT A COMBO RECORD'                                 
NOTLIKE  DC    C'MUST BE "LIKE" STATION (EXAMPLE: ACE TO ACE)'                  
         EJECT                                                                  
****************************************************************                
*  TYPTAB                                                      *                
*                   +0-10 - CODE TYPE                          *                
*                     +11 - SWITCH RECORD TYPE (SWITYPE)       *                
*                     +12 - REP RECORD TYPE (RECTYP)           *                
*                  +13-15 - A(VALIDATION ROUTINE)              *                
*                     +16 - VALID FILTERS                      *                
*                     +17 - REQUIRED FILTERS                   *                
*                     +18 - FLAGS:                             *                
*                           X'80' = MAY HAVE START DATE FILTER *                
*                           X'40' = USED FOR ADV W/ PRODUCTS-- *                
*                                     NO FILTERS ALLOWED       *                
****************************************************************                
         SPACE 2                                                                
*** XXXXXX TEAM AND OFFICE SWITCHES HAVE BEEN TAKEN OUT OF THE TABLE            
***        AS VALID SWITCH TYPES.  THEY MUST BE DONE THROUGH THE                
***        SALESPERSON SWITCH (THEY WERE THE SAME SWITCH BEFORE ANYWAY)         
*                                                                               
*** PUT BACK IN TEAM AND OFFICE SWITCHES -- WAS NOT ALLOWING THEM AS            
*** FILTERS                                                                     
*                                                                               
TYPTAB   DS    0CL19                                                            
         DC    CL11'TEAM       ',X'0105',AL3(VALTEM),X'2000',X'00'              
         DC    CL11'STATION    ',X'0202',AL3(VALSTA),X'0000',X'00'              
         DC    CL11'DDSSTATION ',X'0202',AL3(VALSTA),X'0000',X'00'              
         DC    CL11'OFFICE     ',X'0404',AL3(VALOFF),X'6A00',X'80'              
         DC    CL11'ADVERTISER ',X'0808',AL3(VALADV),X'7700',X'80'              
         DC    CL11'SALESPERSON',X'1006',AL3(VALSAL),X'6A00',X'80'              
         DC    CL11'GROUP      ',X'2007',AL3(VALGRP),X'0202',X'00'              
         DC    CL11'GRP        ',X'2007',AL3(VALGRP),X'0202',X'00'              
         DC    CL11'AGY        ',X'400A',AL3(VALAGY),X'3F00',X'80'              
         DC    CL11'AGENCY     ',X'400A',AL3(VALAGY),X'3F00',X'80'              
         DC    CL11'COMPETING  ',X'8002',AL3(VALCOM),X'0202',X'00'              
         DC    CL11'DEVSALESPER',X'033A',AL3(VALDSP),X'6A00',X'80'              
         DC    CL11'POINTPERSON',X'0531',AL3(VALPTP),X'0800',X'00'              
         DC    X'FF'                                                            
         SPACE 3                                                                
LISTD    DSECT                                                                  
LREC     DS    CL3                                                              
         DS    CL1                                                              
LOLD     DS    CL9                                                              
         DS    CL1                                                              
LSEQ     DS    CL4                                                              
         DS    CL1                                                              
LNEW     DS    CL9                                                              
         DS    CL1                                                              
LREQ     DS    CL8                                                              
         DS    CL1                                                              
LSTA     DS    CL6                                                              
         DS    CL2                                                              
LGRP     DS    CL2                                                              
         DS    CL2                                                              
LSAL     DS    CL3                                                              
         DS    CL1                                                              
LTEAM    DS    CL2                                                              
         DS    CL1                                                              
LOFF     DS    CL2                                                              
         DS    CL1                                                              
LAGY     DS    CL7                                                              
         DS    CL1                                                              
LADV     DS    CL4                                                              
         DS    CL2                                                              
LSTADATE DS    CL8                                                              
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMF8D                                                                      
* REGENSWI                                                                      
* REGENSTA                                                                      
* REGENSAL                                                                      
* RESFMWORKD                                                                    
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMF8D                                                       
         EJECT                                                                  
       ++INCLUDE REGENSWI                                                       
         EJECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
MTODAY   DS    CL3                 YMD MONDAY OF THIS WEEK (BINARY)             
QMDAY    DS    CL3                 YMD MONDAY OF REQUESTED WEEK(BINARY)         
SWITYPE  DS    XL1                 SWITCH RECORD TYPE                           
RECTYP   DS    XL1                 REPFILE RECORD TYPE                          
TYPE     DS    XL1                 RECORD TYPE NOW BEING VALIDATED              
VALFLT   DS    XL1                 VALID FILTERS FOR RECORD - TO SAVE           
REQFLT   DS    XL1                 REQUIRED FILTERS FOR RECORD -TO SAVE         
FLAGS    DS    XL1                 INDICATORS                                   
*                                    X'80' - START DATE FILTER ALLOWED          
*                                    X'40' - ADV W/ PRODS--NO FILTERS           
VFLT     DS    XL1                 VALID FILTERS - TO USE                       
RFLT     DS    XL1                 REQUIRED FILTERS - TO USE                    
SEQNUM   DS    XL1                 SEQUENCE NUMBER                              
OLD      DS    CL7                 OLD CODE                                     
NEW      DS    CL7                 NEW CODE                                     
STADATE  DS    XL3                 3 BYTE BINARY START DATE FILTER              
SAL      DS    CL3                 SALESPERSON                                  
TEAM     DS    CL2                 TEAM                                         
OFFICE   DS    CL2                 OFFICE                                       
REQN     DS    CL8                 REQUESTOR NAME                               
OPTIONS  DS    0CL5                                                             
AGYDEL   DS    CL1                                                              
AGYCLC   DS    CL1                                                              
AGYMVC   DS    CL1                                                              
DDSTAT   DS    CL1                 DDSSTATION SWITCH FLAG                       
DDSFLAGS DS    CL1                 DDS FLAGS                                    
*                                  X'40'  =  "DATE START" WITHIN FLIGHT         
*                                  X'20'  =  COMP S/P -> NEW S/P                
*                                  X'10'  =  COMP S/P -> HOUSE ACCOUNT          
*                                                                               
STATUS   DS    XL1                 X'80' - DONE GETREC FOR VALIDATION           
*                                  X'40' - 'STARTING AT' WEEK FOR LIST          
*                                  X'08' - VALIDATING OLD CODE                  
*                                  X'04' - VALIDATING NEW CODE                  
*                                  X'02' - VALIDATING FILTERS                   
STATION  DS    CL5                 STATION                                      
SAVEKEY  DS    CL27                                                             
SAVE2KEY DS    CL27                                                             
OLDKEY   DS    CL27                                                             
KEY2CH   DS    CL27                                                             
ELEMSAVE DS    CL256                                                            
         DS    0F                                                               
SAVERE   DS    F                                                                
SVDMWORK DS    F                                                                
BLOC     DS    CL750               WORK AREA                                    
OLDSIGN  DS    CL2                                                              
NEWSIGN  DS    CL2                                                              
AGYCORP  DS    CL1                 AGENCY CORPORATE FLAG                        
*                                  A  =  NEW CODE IN TEST                       
*                                  B  =  TARGET IS CORPORATE                    
AAGYCORP DS    CL4                 DISK ADDRESS OF AGYCORP REC                  
         SPACE 4                                                                
REPREC   DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
*                                                                               
T81808   CSECT                                                                  
****************************************************************                
*               FORMAT CODE INTO DISPLAY FORM                  *                
****************************************************************                
FMTCODE  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK+10(10),WORK+10                                              
         CLI   TYPE,X'02'          STATION                                      
         BE    FCOD0020                                                         
         CLI   TYPE,X'80'          OR COMPETING STATION                         
         BNE   FCOD0120                                                         
FCOD0020 MVC   WORK+10(3),WORK                                                  
         LA    R4,WORK+13                                                       
         LA    R5,WORK+3                                                        
         CLI   0(R5),C' '                                                       
         BE    *+14                                                             
         MVC   0(1,R4),0(R5)                                                    
         LA    R4,1(R4)                                                         
         CLI   1(R5),C' '                                                       
         BE    FCOD0040                                                         
         MVI   0(R4),C'-'                                                       
         MVC   1(1,R4),1(R5)                                                    
         LA    R4,2(R4)                                                         
FCOD0040 CLC   2(2,R5),SPACES                                                   
         BE    FCOD0900                                                         
         MVI   0(R4),C'/'                                                       
         MVC   1(2,R4),2(R5)                                                    
         SPACE 1                                                                
         CLI   TYPE,X'80'          FOR COMPETING STATIONS                       
         BNE   FCOD0900                                                         
         SPACE 1                                                                
         CLC   =C'C*',1(R4)        SPECIAL INDICATOR FOR 'CBL'?                 
         BNE   FCOD0060            NO                                           
         MVC   1(3,R4),=C'CBL'     YES - FORCE 'CBL' INTO OUTPUT                
         B     FCOD0900                                                         
FCOD0060 EQU   *                                                                
         LA    R5,AFFTAB           GET REST OF AFFILIATE NAME                   
FCOD0080 CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   1(2,R4),0(R5)                                                    
         BE    FCOD0100                                                         
         LA    R5,L'AFFTAB(R5)                                                  
         B     FCOD0080                                                         
FCOD0100 MVC   3(1,R4),2(R5)                                                    
         B     FCOD0900                                                         
         SPACE 1                                                                
FCOD0120 CLI   TYPE,X'01'          TEAM                                         
         BE    FCOD0140                                                         
         CLI   TYPE,X'04'          OR OFFICE                                    
         BE    FCOD0140                                                         
         CLI   TYPE,X'10'          OR SALESPERSON                               
         BNE   FCOD0160                                                         
FCOD0140 MVC   WORK+10(2),WORK                                                  
         MVI   WORK+12,C'/'                                                     
         MVC   WORK+13(3),WORK+2   SALESPERSON                                  
         CLC   WORK+5(2),SPACES    TEAM?                                        
         BE    FCOD0900                                                         
         MVI   WORK+16,C'/'                                                     
         MVC   WORK+17(2),WORK+5   TEAM                                         
         B     FCOD0900                                                         
         SPACE 1                                                                
FCOD0160 CLI   TYPE,X'40'          AGENCY                                       
         BNE   FCOD0220                                                         
         MVC   WORK+10(4),WORK                                                  
         CLC   WORK+4(2),SPACES                                                 
         BE    FCOD0900                                                         
         LA    R5,4                                                             
         LA    R4,WORK+13                                                       
FCOD0180 CLI   0(R4),C' '                                                       
         BNE   FCOD0200                                                         
         BCTR  R4,0                                                             
         BCT   R5,FCOD0180                                                      
         DC    H'0'                                                             
FCOD0200 MVI   1(R4),C'-'                                                       
         MVC   2(2,R4),WORK+4   AGENCY OFFICE                                   
         B     FCOD0900                                                         
         SPACE 1                                                                
FCOD0220 MVC   WORK+10(10),WORK                                                 
         SPACE 1                                                                
FCOD0900 OC    WORK+10(10),SPACES                                               
         XIT1                                                                   
         EJECT                                                                  
****************************************************************                
*                                                              *                
*              UPDATE AGENCY CORPORATE RECORD                  *                
*                     IF NECESSARY                             *                
*                                                              *                
****************************************************************                
CHKAGCRP NTR1  BASE=*,LABEL=*                                                   
         OC    AAGYCORP,AAGYCORP   D/A PRESENT?                                 
         BZ    CAGC0900            NO  - FINISHED                               
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),AAGYCORP  YES - RETRIEVE RECORD                        
         MVC   SAVAIO,AIO          SAVE CURRENT IO ADDRESS                      
         MVC   AIO,AIO2            SET INPUT TO AIO2                            
         GOTO1 GETREC              READ AGENCY RECORD                           
         L     R6,AIO                                                           
         USING RAGYREC,R6                                                       
         OI    RAGYFLAG,X'04'      SET 'CORPORATE USED' FLAG                    
         MVC   SAVELOPT,ACTELOPT   SAVE CURRENT ACTELOPT                        
         MVI   ACTELOPT,C'N'       TURN OFF ACTELOPT                            
         GOTO1 PUTREC                                                           
         MVC   ACTELOPT,SAVELOPT   RESET CURRENT ACTELOPT                       
         MVC   AIO,SAVAIO          RESET A(IO AREA)                             
         OI    STATUS,X'80'        SET 'REDO GETREC' FLAG                       
         XC    AAGYCORP,AAGYCORP   CLEAR FLAG                                   
CAGC0900 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
SAVELOPT DS    CL1                                                              
SAVAIO   DS    A                                                                
         EJECT                                                                  
****************************************************************                
*  CHECKCNT                                                    *                
****************************************************************                
CHECKCNT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEY(27),SAVE2KEY                                                 
         XC    KEY(27),KEY                                                      
*                                                                               
         SR    R1,R1               USE R1 AS FILE COUNTER                       
         SR    R2,R2               USE R2 AS REP COUNTER                        
*                                                                               
         MVI   KEY,X'28'                                                        
         GOTO1 HIGH                                                             
*                                                                               
CCNT10   EQU   *                                                                
         CLI   KEY,X'28'           PAST SWITCH KEYS?                            
         BNE   CCNT50              YES-MOVE ON                                  
         CLC   KEY+15(3),MTODAY    RECORD FOR THIS WEEK?                        
         BNE   CCNT30              NO, DON'T COUNT IT                           
         CLC   KEY+13(2),AGENCY                                                 
         BNE   CCNT20                                                           
         LA    R2,1(R2)                                                         
CCNT20   EQU   *                                                                
         LA    R1,1(R1)                                                         
         CH    R1,=H'2500'                                                      
         BE    CCNTER1                                                          
         CLC   AGENCY(2),=C'FN' FOX NETWORK?                                    
         BE    CCNT23              YES: PERMIT 1500 ENTRIES                     
         CLC   AGENCY(2),=C'NU' CLEAR CHANNEL?                                  
         BE    CCNT23              YES: PERMIT 1500 ENTRIES                     
         GOTO1 =A(CHK1800),DMCB,(RC),RR=Y                                       
         BZ    CCNT25              NOT IN LIST: CHECK 200                       
*                                                                               
*   FOLLOWING IS LEFT AS A SAMPLE.                                              
*                                                                               
*        CLC   MTODAY(3),=X'5B0B0B'    NOV11/91                                 
*        BE    CCNT22                                                           
*        BNE   CCNT25                                                           
*                                                                               
CCNT22   EQU   *                                                                
         CH    R2,=H'1800'         PERMIT LIST 1800 ENTRIES                     
         BNE   CCNT30                                                           
         B     CCNTER2                                                          
CCNT23   EQU   *                                                                
         CH    R2,=H'1500'         PERMIT FN 1500 ENTRIES                       
         BNE   CCNT30                                                           
         B     CCNTER2                                                          
CCNT25   EQU   *                   PERMIT DEFAULT OF 200 ENTRIES                
         CH    R2,=H'200'                                                       
         BE    CCNTER2                                                          
CCNT30   EQU   *                                                                
         GOTO1 SEQ                                                              
         B     CCNT10                                                           
*                                                                               
CCNT50   EQU   *                                                                
         MVC   KEY(27),SAVE2KEY                                                 
         XIT1                                                                   
*                                                                               
*        SET ERROR MESSAGE AND GET OUT                                          
*                                                                               
CCNTER1  EQU   *                                                                
         MVC   CONHEAD+10(L'TMANYFIL),TMANYFIL                                  
         LA    R2,SWIF1H                                                        
         B     MYEND                                                            
CCNTER2  EQU   *                                                                
*                                                                               
*  IGNORE 'TOO MANY IN REP' CONDITION - FIRST COME, FIRST SERVED                
*                                                                               
         B     CCNT30                                                           
*                                                                               
         MVC   CONHEAD+10(L'TMANYREP),TMANYREP                                  
         LA    R2,SWIF1H                                                        
         B     MYEND                                                            
         EJECT                                                                  
****************************************************************                
* MASTER AND RECORD TYPE TESTING                                                
****************************************************************                
MRTTEST  NMOD1 0,*MRTTST*                                                       
         LR    RC,R1                                                            
*                                                                               
         CLC   AGENCY,=C'IR'       IS REP IR?                                   
         BNE   *+12                NO, GO ON                                    
         LA    R4,IRTAB            YES, LOAD IR TABLE                           
         B     MRT100                                                           
         CLC   AGENCY,=C'K3'       K3?                                          
         BNE   *+12                                                             
         LA    R4,K3TAB                                                         
         B     MRT100                                                           
         CLC   AGENCY,=C'MR'       MR?                                          
         BNE   NO                                                               
         LA    R4,MRTAB                                                         
MRT100   DS    0H                                                               
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    YES                 YES, NOT IN TABLE--SUBSID REC                
         CLC   KEY(1),0(R4)        IN TABLE?                                    
         BE    NO                  YES, NOT SUBSID REC                          
         LA    R4,1(R4)                                                         
         B     MRT100                                                           
*                                                                               
*  THESE ARE THE MASTER RECORD TYPES FOR EACH OF THESE REPS                     
IRTAB    DC    X'05'               TEAM                                         
         DC    X'07'               GROUP                                        
         DC    X'08'               ADV                                          
         DC    X'88'               ADV D2 KEY                                   
         DC    X'0A'               AGENCY                                       
         DC    X'1A'               AGENCY ADDITIONAL                            
         DC    X'8A'               AGENCY D2 KEY                                
         DC    X'FF'                                                            
*                                                                               
K3TAB    DC    X'05'               TEAM                                         
         DC    X'06'               SALESPERSON                                  
**NO**>> DC    X'07'               GROUP                                        
         DC    X'08'               ADV                                          
         DC    X'88'               ADV D2 KEY                                   
         DC    X'0A'               AGENCY                                       
         DC    X'1A'               AGENCY ADDITIONAL                            
         DC    X'8A'               AGENCY D2 KEY                                
         DC    X'FF'                                                            
*                                                                               
MRTAB    EQU   *                                                                
**NO**>> DC    X'05'               TEAM                                         
         DC    X'06'               SALESPERSON                                  
         DC    X'07'               GROUP                                        
         DC    X'08'               ADV                                          
         DC    X'88'               ADV D2 KEY                                   
         DC    X'0A'               AGENCY                                       
         DC    X'1A'               AGENCY ADDITIONAL                            
         DC    X'8A'               AGENCY D2 KEY                                
         DC    X'FF'                                                            
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT2     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   GETMON:  GET MONDAY ROUTINE NMOD'ED TO GET ADDRESSABILITY                   
*                                                                               
GETMON   NMOD1 0,*GMON*                                                         
         L     RC,0(R1)                                                         
         GOTO1 DATCON,DMCB,(5,0),DUB            TODAY                           
         GOTO1 GETDAY,DMCB,DUB,FULL             DAY OF WEEK                     
         SR    R3,R3                                                            
         IC    R3,DMCB                                                          
         BCTR  R3,0                                                             
         LNR   R3,R3                            BACK UP TO MONDAY               
         GOTO1 ADDAY,DMCB,DUB,DMCB+12,(R3)                                      
         GOTO1 DATCON,DMCB,DMCB+12,(3,MTODAY)                                   
         MVC   QMDAY,MTODAY                                                     
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHK1800:  LIST OF AGENCIES WHICH ARE PERMITTED 1800 ENTRIES                 
*        RETURN CC WILL BE NOT ZERO IF IN LIST, ZERO IF NOT                     
*                                                                               
CHK1800  NMOD1 0,*CK1K*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   AGENCY(2),=C'BL'                                                 
         BE    CK1K0020            FOUND : SET CC NOT ZERO                      
         CLC   AGENCY(2),=C'PV'                                                 
         BE    CK1K0020            FOUND : SET CC NOT ZERO                      
         CLC   AGENCY(2),=C'IR'                                                 
         BE    CK1K0020            FOUND : SET CC NOT ZERO                      
         CLC   AGENCY(2),=C'SZ'                                                 
         BE    CK1K0020            FOUND : SET CC NOT ZERO                      
         CLC   AGENCY(2),=C'S2'                                                 
         BE    CK1K0020            FOUND : SET CC NOT ZERO                      
         CLC   AGENCY(2),=C'K3'                                                 
         BE    CK1K0020            FOUND : SET CC NOT ZERO                      
         CLC   AGENCY(2),=C'MR'                                                 
         BNE   CK1K0040                                                         
CK1K0020 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     CK1K0060                                                         
CK1K0040 EQU   *                                                                
         SR    R0,R0                                                            
CK1K0060 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
* SCAN SUBSIDIARIES FOR CORRECT RECORD                                          
*                                                                               
* 2ND PARAMETER IS THE DISPLACEMENT OF THE REP CODE INTO THE                    
* RECORD                                                                        
*                                                                               
* THE GROUP IS ALSO CHECKED TO MAKE SURE IT IS THE CORRECT STATION              
*******************************************************************             
SUBSCAN  NMOD1 0,*SUBSCN*                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)            2ND PARAM=DISP OF REP INTO THE REC           
*                                                                               
         MVC   OLDKEY(27),KEYSAVE  SAVE OFF THE KEY FOR TESTING                 
         MVC   AIO,AIO2            READ REP REC INTO IO2                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           REP REC ID                                   
         MVC   KEY+25(2),AGENCY    REP CODE                                     
         GOTO1 READ                                                             
         CLI   DMCB+8,X'10'        REC FOUND?                                   
         BNE   *+6                                                              
         DC    H'0'                REP NOT ON FILE.                             
         GOTO1 GETREC              READ IN REP RECORD                           
*                                                                               
         XC    ELEMSAVE,ELEMSAVE                                                
         L     R6,AIO                                                           
         MVI   ELCODE,2            SUBSID REP ELEMENT                           
         BAS   RE,GETEL                                                         
         ZIC   R3,1(R6)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ELEMSAVE(0),0(R6)   SAVE OFF ELEMENT WITH SUBSID REPS            
         CLC   =C'K3',AGENCY       MASTER = KRGNY?                              
         BNE   SUB080              NO                                           
         LA    RF,ELEMSAVE+10      YES - INSERT 'NU' INTO LIST                  
SUB020   EQU   *                                                                
         OC    0(2,RF),0(RF)       ANY ENTRY IN SLOT?                           
         BZ    SUB040              NO                                           
         LA    RF,2(RF)            YES - BUMP TO NEXT SLOT                      
         B     SUB020              GO BACK FOR NEXT                             
SUB040   EQU   *                                                                
         MVC   0(2,RF),=C'NU'      INSERT NU INTO SLOT                          
SUB080   EQU   *                                                                
         LA    R6,ELEMSAVE+10      POINT TO BEGINNING OF SUBSID REPS            
*                                                                               
         LA    R7,KEY                                                           
         AR    R7,R2               ADD DISP OF REP TO BEGINNING OF KEY          
SUB100   DS    0H                                                               
         CLI   0(R6),0             END OF TABLE?                                
         BE    SUBNO               YES, NOT FOUND FOR SUBSID                    
         MVC   KEY(27),OLDKEY                                                   
*        MVC   KEY+20(2),0(R6)     DISP 20 HARDCODED FOR STA FOR NOW            
         MVC   0(2,R7),0(R6)       LOOK UP RECORD FOR NEXT SUBSID REP           
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   SUB150              NOT EQUAL--GO ON                             
         GOTO1 GETREC                                                           
         L     R5,AIO                                                           
         USING RSTAREC,R5                                                       
         CLC   RSTAGRUP,WORK       CHECK TO SEE IF GRP AGREES ALSO              
         BE    SUBYES                                                           
         DROP  R5                                                               
SUB150   LA    R6,2(R6)                                                         
         B     SUB100                                                           
         EJECT                                                                  
SUBYES   SR    RC,RC                                                            
SUBNO    LTR   RC,RC                                                            
XIT3     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------|         
*CHOLDNEW - SUBROUTINE CHECK FOR OLD STATION NOT = NEW STATION IN     |         
*           PREVIOUSLY ENTRED RECORDS IN FILE SWITCH.                 |         
*           TYPE OF CODE IS X'02' STA                                 |         
*    INPUT- KEY2CH HAS VALID KEY                                      |         
*           NEW HAS NEW CODE TO BE VALIDATED                          |         
*    OUTPUT-                                                          |         
*  ABOB ADDED                                                         |         
*---------------------------------------------------------------------|         
CHOLDNEW NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY               CLEAR KEY                                  
*                                                                               
*1 CHECK IF OLD STATION CODE WAS ENTERED BEFORE                                 
*                                                                               
         MVC   KEY,KEY2CH            RESTORE KEY                                
         MVI   KEY+26,0             REMOVE SEQUENCE NUMBER                      
         GOTO1 HIGH                                                             
         CLC   KEY(26),KEYSAVE       CHECK IF KEY EXISTS                        
         BE    STERR01                                                          
STOLNE01 DS    0H                                                               
*                                                                               
*2 CHECK IF NEW STATION WAS PREVIOUSLY ENTRED AS OLD                            
*                                                                               
         MVC   KEY,KEY2CH            RESTORE KEY                                
         MVC   KEY+19(L'NEW),NEW     MOVE NEW CODE                              
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(26),KEY       CHECK KEY                                  
         BE    STERR02              IF = ERROR                                  
*                                                                               
*3 CHECK IF OLD STATION WAS PREVIOUSLY ENTRED AS NEW                            
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
*RESTORE ONLY REC TYPE, REP, DATE, AND CODE TYPE OF KEY                         
         MVC   KEY(19),KEY2CH        RESTORE KEY                                
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(19),KEY       CHECK IF KEY EXISTS                        
         BNE   STOLNE02              IF NO                                      
         GOTO1 GETREC                ELSE GET RECORD                            
         L     R1,AIO                LOAD ADDRESS OF RECORD                     
         USING RSWIREC,R1                                                       
         CLC   RSWINEW,OLD           CHECK IF PREVIOUS NEW = OLD                
         BE    STERR03                                                          
*                                                                               
STOLNE04 MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         CLC   KEY(19),KEYSAVE                                                  
         BNE   STOLNE02                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING RSWIREC,R1                                                       
         CLC   RSWINEW,OLD                                                      
         BE    STERR03               IF = ERROR                                 
         B     STOLNE04                                                         
STOLNE02 DS    0H                                                               
*                                                                               
STNOERR  DS    0H                                                               
         LA    R0,0                                                             
STSETERR STC   R0,DMCB                                                          
         B     CHSTEXT                                                          
*                                                                               
STERR01  LA    R0,1            SET ERROR TO 1                                   
         MVC   CONHEAD+10(L'NEWSTENT),NEWSTENT                                  
         B     STSETERR                                                         
*                                                                               
STERR02  LA    R0,2            SET ERROR TO 2                                   
         MVC   CONHEAD+10(L'OLDNEWST),OLDNEWST                                  
         B     STSETERR                                                         
*                                                                               
STERR03  LA    R0,3                                                             
         MVC   CONHEAD+10(L'NEWOLDST),NEWOLDST                                  
         B     STSETERR                                                         
*                                                                               
CHSTEXT  DS    0H                                                               
         XIT1                                                                   
*CHSTADBL SUBROUTINE END                                              |         
*---------------------------------------------------------------------|         
*                                                                               
OLDNEWST DC    C'NEW IS SAME AS PREVIOUS OLD STATION.'                          
NEWOLDST DC    C'OLD IS SAME AS PREVIOUS NEW STATION.'                          
NEWSTENT DC    C'OLD STATION PREVIOUSLY ENTERED.'                               
*                                                                               
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
VREQUEST NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SWIREQH                                                       
         GOTO1 ANY                                                              
         MVC   REQN,WORK                                                        
*                                                                               
         MVI   ELCODE,1                                                         
         GOTO1 REMELEM             DELETE OLD X'01' ELEMENT                     
*                                                                               
         XC    ELEM,ELEM           AND REBUILD                                  
         LA    R6,ELEM                                                          
         USING RSWIELEM,R6                                                      
         MVC   RSWICODE(2),=X'011E'                                             
         MVC   RSWIREQN,REQN                                                    
         MVC   RSWINEW,NEW                                                      
         MVC   RSWIOPT,OPTIONS                                                  
         MVC   RSWISTA,STADATE                                                  
         LA    RE,SWITYPH          SET A(RECORD TYPE)                           
         ZIC   RF,5(RE)            GET LENGTH OF INPUT                          
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,VREQ0020         COMPARE BY LENGTH                            
         BNE   VREQ0040                                                         
         OI    RSWIDDS,X'80'       YES - SET DDSSTATION SWITCH                  
         B     VREQ0040                                                         
VREQ0020 CLC   8(0,RE),=C'DDSSTATION'                                           
         DS    0H                  ALIGNMENT                                    
VREQ0040 EQU   *                                                                
         OC    RSWIDDS,DDSFLAGS    ADD WITHIN/COMP FLAGS TO BYTE                
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
NEWADVNO NTR1  LABEL=*,BASE=*                                                   
         MVC   SAVAIO2,AIO         YES - SAVE CURRENT IO ADDRESS                
         MVC   AIO,AIO2            SET INPUT TO AIO2                            
         GOTO1 GETREC              READ AGENCY RECORD                           
         L     RF,GETREC                                                        
         BASR  RE,RF                                                            
         L     R6,AIO                                                           
         USING RADVREC,R6                                                       
         TM    RADVFLGS,X'02'      'DO NOT USE' SET?                            
         BO    NEWA0020            YES - EXIT WITH ERROR                        
         DROP  R6                                                               
         MVC   AIO,SAVAIO2         RESTORE CURRENT IO ADDR                      
         OI    STATUS,X'80'        SET STATUS FLAG                              
         MVI   DMCB,0              CLEAR ERROR INDICATOR                        
         B     NEWA0040            EXIT WITH NO ERROR                           
NEWA0020 MVC   CONHEAD+10(L'ADVNOUSE),ADVNOUSE                                  
         MVI   DMCB,1              SET ERROR INDICATOR                          
NEWA0040 EQU   *                                                                
         XIT1                                                                   
ADVNOUSE DC    C'NEW CODE MARKED "INACTIVE" - CANNOT BE USED'                   
         LTORG                                                                  
         EJECT                                                                  
AFFCHK   NTR1  LABEL=*,BASE=*                                                   
         LA    RE,AFFTAB                                                        
AFF3     CLI   0(RE),X'FF'                                                      
         BE    AFF5                                                             
         CLC   WORK(3),0(RE)                                                    
         BE    AFF20                                                            
         LA    RE,L'AFFTAB(RE)                                                  
         B     AFF3                                                             
AFF5     CLI   STATION+4,C' '                                                   
         BE    AF10                                                             
         CLC   WORK(3),SPACES      RADIO CAN HAVE NO AFFILIATE                  
         BE    AFF20                                                            
AF10     MVC   CONHEAD+10(L'BADAFF),BADAFF                                      
         LTR   RB,RB               SET CC NOT ZERO                              
         B     AFF100                                                           
AFF20    EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
AFF100   EQU   *                                                                
         XIT1                                                                   
AFFTAB   DS    0CL3                                                             
       ++INCLUDE REAFFLIST                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'252RESFM08   02/20/13'                                      
         END                                                                    
