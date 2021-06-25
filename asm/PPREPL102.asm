*          DATA SET PPREPL102  AT LEVEL 051 AS OF 04/08/14                      
*PHASE PPL102A                                                                  
*INCLUDE GETCOST                                                 L24            
*INCLUDE CENTER    THESE ARE RELO MUDULES TO BE INCLUDED IN LINK                
*INCLUDE DLFLD                                                                  
*INCLUDE CHOPPER                                                                
         TITLE 'CHANGE LOG                   '                                  
*                                                                               
* BPLA 04/14     FIX TO SOME EX INSTRUCTIONS                                    
*                                                                               
* BPLA 01/12     RELINK WITH NEW GETCOST                                        
*                                                                               
* BPLA 01/10     SPECIAL CODE FOR DUPONT (QAGENCY=DP) NO-OPTED                  
*                SEE  **DP                                                      
*                                                                               
* BPLA 10/98     FIX BUG IN TLIN10C                                             
*                BE SURE RECORD IS A BUY RECORD BEFORE READING                  
*                WHEN READING USERP FOR MEDIA "C" OR "*" REQUEST                
*                TRY FOR MEDIA "I" FIRST, THEN CHECK FOR MAGAZINES.             
*                IF USERP NOT FOUND - DON'T DIE - TRY TO SKIP                   
*                FOR MULTI-MEDIA REQUEST - CHECK FIRST MEDIA                    
*                FOUND - NOT JUST MAGAZINES                                     
*                                                                               
* SMYE 12/12/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                       
*                                                                               
* BPLA 3/28/91 FIXED ROGER'S BUG (MVI  PPRDKCLT,0)                              
*              CAUSED BILLING FORMULA PROBLEM FOR NEXT REQUEST                  
*              CHANGED TO PPRDKPRD,0                                            
* BPLA 1/3/91  REMOVE GETINS INCLUDE AND CHANGE =V(GETINS) TO                   
*              GETINS                                                           
*                                                                               
* SWON 1/25/90 DITTO MARKS NOT PRINTING PROPERLY                  BUG30         
*              FOR AD NO. AFTER CHANGING MEDIA IN A MULTI-MEDIA   BUG30         
*              REQUEST.                                           BUG30         
*                                                                 BUG29         
* ROSA 1/15/90 DISTRICT CODE NOT PRINTING PROPERLY                BUG29         
*                                                                 BUG29         
* BPLA 12/4/89 ADD LOGIC FOR GROUPS  (&N) IN QCLIENT              L25           
*                                                                               
* BPLA 10/5/89 DIED IF REQ FOR CLT=ALL AND ONE MEDIA              BUG28         
*              + PAGE BREAKING PROBLEM  - FIXED TLCHKHD + TLCKHDY BUG28         
* ROSA 9/14/89 CRASH WHEN READING JOB RECORD AND BUY IS A POL.    BUG27         
*              WAS READING ALLOCATED PRODUCT- SHOULD READ JOB     BUG27         
*              UNDER ZZZ                                          BUG27         
*                                                                 BUG27         
* ROSA 7/19/89 NOT EXTRACTING CORRECTLY WHEN REQUEST IS FOR CLI   BUG26         
*              DIVISION REGION AND PRODUCT.. BUYS WERE BEING READ BUG26         
*              USING ALT KEY (X'21') DUE TO DIVISION/REGION REQ.  BUG26         
*              WHEN THERE WAS A PRODUCT CHANGE WITHIN THE SAME    BUG26         
*              PUB (MORE PUBS TO FOLLOW) PGM WENT TO END OF INPUT.BUG26         
*                                                                 BUG26         
* BPLA 6/15/89 BUG IN PBRKTOTS - IF PUTTING TOTALS ON A SEPERATE  BUG25         
*              LINE LINENEED SHOULD HAVE BEEN SET TO 5 (NOT 3).   BUG25         
*              CAUSED TOTALS TO PRINT AT TOP OF PAGE WITH NO      BUG25         
*              HEADINGS, SINCE TLCKHD WASN'T SETTING FORCEHED     BUG25         
* ROSA 6/2/89  ENSURE THERE IS A BILLING FORMULA.. WAS ORING IN   BUG24         
*              36 BYTES OF BILLING OPTIONS// FIRST FIVE OF BILLINGBUG24         
*              FORMULA MAY BE ZERO REMAINING HAD DATA             BUG24         
* BPLA 5/30/89                                                    BUG23         
*        CAN'T USE X'21' POINTERS UNLESS REUEST IS FOR ZZZ        BUG23         
*        SINCE X'21' POINTERS DO NOT EXIST FOR THE ALLOCATED      BUG23         
*        PRODUCTS                                                 BUG23         
*                                                                               
* ROSA 5/15/89  LOOP OCCURS WHEN THERE IS A CAPTION OVERRIDE    BUG22           
*               CHANGE BRANCH FROM ST09 TO ST99USE              BUG22           
*                                                               BUG22           
* BPLA 5/11/89  SPECIAL DIV HEAD CODE CHANGED FROM X'A0' TO     BUG21           
*               X'F9' TO AVOID CONFUSION WITH OPEN BILLED       BUG21           
*               GROSS (ALSO X'A0')                              BUG21           
* BPLA 4/26/89  CHANGE TO USE GETCOST INSTEAD OF PBILPROC       L24             
*                                                                               
* BPLA 4/26/89  BILLING FORMULA PRINTING                        BUG 20          
*                                                                               
* BPLA 4/5/89   PASS DELETES IF OPEN BILLED/PAID $ REQUESTED    BUG19           
*                                                                               
* BPLA 4/5/89   BILLED,BILLABLE,PAYABLE QOPTS - PASS DELETES    BUG18           
*                                                                               
* BPLA 4/5/89   PAYABLE ITEMS ONLY (QOPT5=Y)                    BUG17           
*                                                                               
* BPLA 3/29/89  SPECIAL HANDLING OF 'C' RATE INSERTIONS         L23             
*               SPECIAL PARAMETER TO GETINS (GROSS=C'CRAT')     L23             
*               USES PBUYREC'S PBDACP (IF NOT 'C' 100PCT)       L23             
*               CHANGE IN $ DISPLAYS TO USE PYABLE,PAID,BILLED  L23             
*               FOR NETS INSTEAD OF GROSS-AGYCOM                L23             
*                                                                               
* ROSA 2/24/89  FCB REQUESTING TOTALS ON DIVISION NAME-- NAME   BUG16           
*               NOT SET UP IN TOTAL TABLE                       BUG16           
*                                                               BUG16           
* ROSA 1/3/89   DOREMUS INTERMITTANT BUG.. SOMETIMES PRINTING   BUG15           
*               ERRONEOUS GROSS DOLLARS.. R2 BEING DESTROYED    BUG15           
*               WHEN EXITING CSECT 'OPNDIFF'                    BUG15           
*                                                               BUG15           
* ROSA 12/9/88  IF NO BUY RECORDS ARE READ IN AT TLIN E.G. BAD  BUG14           
*               REQUEST.. NOTHING IS IN BUY KEY WHICH IS USED   BUG14           
*               TO BUILD KEYS TO READ CLIHDR/EST/PROD FOR THE   BUG14           
*               HEADER.  INVALID DATA IS PRINTED IN HEADER.     BUG14           
*                                                               BUG14           
* ROSA 11/30/88 WHEN CHECKING FOR MASTER AGENCY AT LABEL CK4MAST UG13           
*               PGM DIED IF NO CLI HEADER WAS FOUND WHEN REQUESTBUG13           
*               WAS FOR ALL MEDIA(*). CODE INSERTED TO TEST FORBUG13            
*               THE CONDITION WHERE ALL MEDIA IS REQUESTD FOR   BUG13           
*               A CLIENT BUT THERE MAY NOT BE A CLIENT HEADER FOR G13           
*               THAT CLI./                                      BUG13           
*                                                               BUG13           
*                                                               BUG13           
* ROSA 11/8/8  CHECK 12 POSITIONS FOR ZERO AFTER COMING BACK FRMBUG12           
*              GETINS WHEN FILTERING ON BILLING. WAS ONLY CHECKINGG12           
*              FOR 4. WHEN THERE WAS ZERO GROSS BUT CASH DISCOUNTUG12           
*              WAS BYPASSING RECORD.                            BUG12           
*                                                               BUG12           
* ROSA 11/7/88 ALLOW USERS TO OPTIONALLY SPACE 2 OR THREE LINES   L22           
*                                                                 L22           
* ROSA 11/7/88 DRD NOT BEING HANDLED // R1 ESTROYED BY GOTO     BUG11           
*           FURTHER COMPARES BASED ON R1 SEE SUBDRDBRK          BUG11           
*ROSA 9/29/88 HANDLE MASTER AGENCY REQUEST---                     L21           
*                                                                 L21           
*                                                                 L21           
*ROSA 9/27/88 ADD DISTRICT NAME AS A COLUMN  (JWT)                L20           
*                                                                 L20           
*ROSA 9/14/88 PRINT OAN (OTHER AGENCY NAME) CODE OR NAME AND CODE L19           
*       (JWT) REQUEST                                             L19           
*                                                               BUG10           
*ROSA 8/29/88 BUY COMMENT WILL ONLY PRINT 4 LINES // AGENCY WAS BUG10           
*          ENTERING 5 LINES                                     BUG10           
*                                                               BUG10           
* ROSA 7/26/88 BOZELL JACOBS -- REQUEST FOR OFFICE WHICH INCLUDEDUG09           
*      TEST BUYS.. NO TEST BUYS PRINTED.. SUBROUTINE ESTFLTR    BUG09           
*      CHECKED TO SEE IF EST IS A TEST ESTIMATE AND BYPASSES ALLBUG09           
*      BUYS UNDER THAT EST.  SYSTEM CHANGE-- TEST BUYS ARE AT THEUG09           
*      BUY LEVEL NOW.                                           BUG09           
*                                                               BUG09           
*ROSA 7/20/88 DOREMUS // 1- CHANGE ALL HEADINGS FROM 'DIFF' TO  BUG08           
*              'REBATE' OR 'RBTE' WHEN NOT ENOUGH ROOM          BUG08           
*       2- CALCULATION FIELD NOT TAKING INTO CONSIDERATION      BUG08           
*        OPEN AND DIFFERENCE ACCUMULATORS FOR DOREMUS. SAVEAPTR BUG08           
*        NOT BEING UPDATED IN SUB 'OACCUM'                      BUG08           
*                                                               BUG08           
*ROSA 7/18/88 PGM CRASH WHEN TRYING TO READ A JOB RECORD USING  BUG07           
*  PASSIVE POINTER // JOB RECORDS USE POL PRODUCT               BUG07           
*                                                               BUG07           
*ROSA 4/15/88 PROBLEM WHEN DOREMUS REQUESTING C MEDIA AND SOON  BUG06           
*             ACCUMULATORS MAY NOT BE CLEARED                   BUG06           
*                                                               BUG05           
*ROSA 4/13/88 WHEN ALL DISTRICT REQUESTED, THE CORRECT DISTRICT BUG05           
* BUG05**     IS NOT MOVED INTO THE SORT KEY 000 ARE MOVED      BUG05           
*                                                               BUG05           
*                                                                 L18           
* ROSA 4/12/88 PRINT MESSAGE IN HEADLINE WHEN QOPT3 (BILLED)      L18           
*              QOPT4 (BILLABLE) OR QOPT5 (PAID) OPTIONS WERE      L18           
*              REQUESTED.                                         L18           
*                                                                 L17           
* ROSA 4/11/88 RAN OUT OF BASE REGISTER IN TLOUT  MOVED SUBROUTINEL17           
*             DP01 AND DP07 AND MADE THEM CSESTS                  L17           
*                                                                 L17           
*                                                                 L17           
* ROSA 4/8/88 WHEN CAPTION OR COPY IS SELECTED IN USERP, THE      L16           
*     X'66' BUY COMMENT WILL OVERRIDE IF CAP= OR COPY= IS THE     L16           
*     FIRST BYTES OF COMMENT...                                   L16           
*                                                                 L16           
*                                                                 L15           
* ROSA 4/7/88   ERROR WHEN REQUEST IS FOR OFFICE BY SPECIFIC      L15           
* **BUG04**     PUB NUMBER --SEE NOTE BUG04 IN PGM                L15           
*                                                                 L15           
*                                                                 L15           
* ROSA/3/23/88   USE PASSIVE POINTERS IF REQUEST IS FOR ALL AND   L15           
*                PRODUCT IN KEY IS ZZZ OTHERWISE USE ACTIVE PTRS. L15           
*            USE ZZZ ACTIVE POINTERS WHEN ZZZ IS REQUESTED        L15           
*                                                                 L15           
*    ROSA/ 3/23/88  DRD SCHEME USING ANOTHER'S CLEINT SETUP       L14           
**L14 ** SUBROUTINE 'SETOLDS' WAS DESTROYING VALUES SET UP IN DRDBRK            
*                                                                 L14           
*    ROSA/ 3/21/88  A COMBINED MEDIA REQUEST FOR A CLIENT PRODUCED              
**BUG03** AN INCORRECT CLIENT NAME AND CODE FOR THE FIRST PAGE OF BUG03         
*         THE REPORT.                                             BUG03         
*                                                                 BUG03         
*                                                                 L13           
* ROSA 3/14/88  DO NOT PRINT DOLLAR COLUMNS IF DISPLEN IS X'F0'   L13           
*   *L13*                                                         L13           
*                                                                 L13           
*                                                               BUG02           
* ROSA 3/2/88 BOMB WHEN CAPTION WAS REQUESTED WITH TOTALS       BUG02           
*   *BUG02*   CATION NOT IN SORT TABLE                          BUG02           
*                                                               BUG02           
*                                                                L12            
* ROSA ADD A TITLE FOR 93 CALCULATIONS       3/1/88              L12            
*    L12                                                         L12            
*                                                                 L11           
* ROSA 2/19/88 INCLUDE A MULTIPLIER TO 93 CALCULATION             L11           
*                                                                L10            
* ROSA 2/8/88 INCLUDE WEEKLY TOTALS OPTION                       L10            
*                                                                L09            
* ROSA 2/5/88 ADD ABILITY TO RUN L1 BY DUMMY CLIENT DRD SCHEME   L09            
*                                                                 L08           
* ROSA 1/29/88 ADD STANDARD COMMENTS TO REPORT                    L08           
*                                                               L07             
* ROSA 1/25/88 ADD COST COLUMN <WHAT CLIENT WAS BILLED)         L07             
*                                                               L06             
* L06 INSERT COMMON PUB NAME OR CODE FOR DUPONT   1/19/88       L06             
*                                                               L06             
* ROSA L05 1/15/88 CORRECT PROBLEM IN SPACE DESCRIPTION          L05            
*                                                                L05            
* ROSA  1/14/88   ADD DIFFERENCE OF 3 FIELDS WITH +- OPTIONS     L04            
*          ID CHANGE L04                                         L04            
*                                                                L04            
* ROSA  1/12/88                                                  L03            
*                   ADD TWO FILTERS 1-BILLED 2-PAID DATES        L03            
*             ID CHANGE L03                                      L03            
*                                                                L03            
*  ROSA 1/11/88  CHANGE TITLE OF ASPO IN HEADLINE AND ADD DESCRIPTION           
*                IN TABLE 'NAMETBL'                                L02          
*             ID CHANGE L02                                        L02          
*                                                                  L02          
*   ROSA 1/7/88  CHANGE METHOD OF ACCUMULATING AND EDITING OF DOLLARL01         
*                AMOUNTS FROM BINARY TO PACKED.  THE ACCUMULATORS   L01         
*                AFFECTED ARE 'ACCUMS' AND 'TEMPTOTS'.              L01         
*                THIS CORRECTS PROBLEM WITH DUPONT                  L01         
*         CODE ADDED/CHNGED WILL HAVE AN ID OF L01.                 L01         
*                                                                   L01         
         TITLE 'PPL102 - PRINTPAK USER REPORT'                                  
*                                                                               
*        USER REPORT                                                            
*                                                                               
*        QOPT1 CANNOT BE USED UNLESS PLACEMENT OF ADCODE IN REQ REC IS          
*              CHANGED SINCE THE LAST CHARACTER OF THE ADCODE OVERLAYS          
*              QOPT1. THEREFORE I HAVE NOOPED ALL REFERENCES TO QOPT1.          
*                                                                               
*        QOPT2 Y= SUPPRESS BOXES                                                
*        QOPT3 Y= BILLED ITEMS ONLY                                             
*        QOPT4 Y= BILLABLE ITEMS ONLY AND:                                      
*                 NO DELETED BUYS W/ CONTRACT GROSS ORDERED = 0                 
*                                 OR CONTRACT GROSS BILLED  = 0                 
*        QOPT5 Y= PAYABLE ITEMS ONLY                                            
*        QOPT6 Y= TRACE                                                         
*        QOPT7 Y= INCLUDE TEST INSERTIONS                                       
*                                                                               
* ************************************************                              
*                                               RCMULTIQ  QMEDIA                
* NOTE // IF QMEDIA  IS * (MULTI MEDIA REQ)        Y       M                    
*         IF QMEDIA  IS C (COMBINED MEDIA )        C       M                    
*      MULTI MEDIA REQUEST WILL PRINT WITH TOTALS ALL BUYS WITHIN               
*         THAT REQUEST.  BUYS WILL BE IDENTIFIED BY A TITLE                     
*         IN THE BODY OF THE REPORT.                                            
*      COMBINED MEDIA REQUEST WILL INCLUDE ALL BUYS IRRESPECTIVE                
*         OF MEDIA AND WILL NOT GIVE A MEDIA TOTAL.                             
* *************************************************                             
*                                                                               
*******************************************************************             
*                                                                 *             
*                                                                 *             
*  THIS REPORT READS BUY RECORDS AND CAN PRINT DATA ON 3 LEVELS:  *             
*                                                                 *             
*  1. EVERY INDIVIDUAL RECORD (DETAILS).                          *             
*  2. ACCUMULATES TOTALS AND PRINTS A LINE ON CHANGE              *             
*     OF A FIELD OR FIELDS.                                       *             
*  3. PRINTS TOTALS AT REQUESTED FIELD BREAKS.                    *             
*                                                                 *             
*                                                                 *             
*  GENERALLY, THE REPORT WILL SUPPRESS DETAILS AND ACCUMULATE     *             
*  DOLLARS, PRINTING A LINE OF DATA ONLY WHEN THERE IS A CHANGE   *             
*  IN SOME FIELD.  IN CERTAIN CASES - SUCH AS WHEN THE INSERTION  *             
*  DATE IS PART OF THE REQUESTED OUTPUT - THE REPORT PRINTS EVERY *             
*  LINE SINCE EACH INSERTION DATE IS UNIQUE AND THUS CAUSES A     *             
*  LINEBREAK FOR EACH BUY RECORD.  ONE CAN ALSO REQUEST TOTALS    *             
*  TO BE PRINTED ON SPECIFIED BREAKS IN THE FIELDS.               *             
*                                                                 *             
*  THIS REPORT(L1) RUNS UNDER PPGZ: THIS IS PPG BUT WITH          *             
*       REPORTZ (NTR1+EXIT FEATURE FOR HEADHOOK) FOR REPORTN      *             
*       PPRQREPZ (SUPPORTS BOXES)                FOR PPREPREQ     *             
*                                                                 *             
*       REPORTN + PPREPREQ HAVE BEEN CATALP AS REPORTZ/PPRQREPZ   *             
*       NO OTHER REPORTS RUN UNDER THIS AS OF NOW 9/25/84         *             
*                                                                 *             
*  TEMPTOT-  10- 12 BYTE AREAS WHERE COLUMN ID'S, PRINTING        *             
*                DISPLACEMENT, BREAK TOTAL ID'S AND $ ARE SAVED.  *             
********************************************************************            
         SPACE 3                                                                
PPL102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPL102A,RR=R9                                                  
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP81WRKD,R8                                                      
*                                                                BUG06          
*  CLEAR ALL TOTALS //  WHENEVER THERE IS A NEW REQUEST          BUG06          
*                                                                BUG06          
         CLI   MODE,0           NEW REQUEST                      BUG06          
         BNE   NODENTFS                                          BUG06          
         LA    R4,FINLTOTS                                       BUG06          
         MVI   OLDOAN,0            FORCE TO PRINT OTHER AGY IF RQSTDL19         
         LA    R1,(L'FINLTOTS/12)    GET REPITITION              BUG06          
ZIPZAP   ZAP   0(12,R4),=P'0'                                    BUG06          
         LA    R4,12(R4)                                         BUG06          
         BCT   R1,ZIPZAP                                         BUG06          
         MVC   TEMPTOTS,FINLTOTS                                 BUG06          
         MVC   TOTTOT,FINLTOTS                                   BUG06          
         MVC   ACCUMS(L'TOTTOT),TOTTOT                           BUG06          
         MVC   ACCUMS+120(240),ACCUMS              BUG06                        
NODENTFS DS    0H                                                BUG06          
*                                                                               
*                                                                               
         SPACE                                                                  
         CLI   MODE,LBUYXRQ                                                     
         BE    TLLAST                                                           
         CLI   MODE,UNSRTREC                                                    
         BNE   EXIT                                                             
         MVC   SAVPARS,DMCB+4                                                   
         LM    R2,R3,SAVPARS                                                    
         B     TLBRTAB(R2)                                                      
*                                                                               
TLBRTAB  B     TLFIRST         SETS PARAMETERS OF REPORT                        
         B     TLINPUT         PASSES RECORDS TO SORT                           
         B     TLOUTPUT        TAKES SORTED RECS/PROCESSES THEM/PRINTS          
         B     TLLAST          PRINTS REPORT TOTALS                             
         TITLE 'TLFIRST-- INITIALIZE'                                           
TLFIRST  DS    0H                                                               
*                                                                               
         MVI   RC2DSECT,C'Y'                                                    
         L     R0,=A(TLIN)                                                      
         A     R0,RELO        INITIALIZE SUB-ROUTINE ADDRESSES                  
         ST    R0,ATLIN                                                         
         L     R0,=A(WEEKDATE)   ADDRESSABILITY                  L10            
         A     R0,RELO                                           L10            
         ST    R0,AWEEK                                          L10            
         XC    WEEK,WEEK        INITIALIZE                       L10            
         CLC   QPRODUCT,=C'ZZZ'     REQUESTED PRODUCT            L15            
         MVI   PASSPTRS,C'N'                                      L15           
         BE    *+8                                                L15           
         MVI   PASSPTRS,C'Y'                                      L15           
*                                                                 L16           
*                                                                L10            
         L     R0,=A(TLOUT)                                                     
         A     R0,RELO                                                          
         ST    R0,ATLOUT                                                        
         L     R0,=A(DISPTAB)                                   L07             
         ST    R0,ADISPTAB                                      L07             
         L     R0,=A(PPBYOWRK)                                                  
         A     R0,RELO                                                          
         ST    R0,ABYOWRK                                                       
         L     R1,=A(RDSAVE)                                                    
         A     R1,RELO                                                          
         ST    R1,ARDSAVE                                                       
         ST    R1,ARDSAVE1                                                      
         MVI   0(R1),0    INITIALIZE                                            
         L     R1,=A(NAMETBL)                                                   
         A     R1,RELO                                                          
         ST    R1,ANAMETBL                                                      
         L     R1,=A(RDSPLIT)                                                   
         A     R1,RELO                                                          
         ST    R1,ARDSPLIT                                                      
         L     R1,=A(OPNDIFF)                                                   
         A     R1,RELO                                                          
         ST    R1,AOPNDIFF                                                      
         L     R1,=A(HEADRTN)                                                   
         A     R1,RELO                                                          
         ST    R1,AHEADRTN                                                      
*                                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         L     R2,BOXAWIDE                                                      
         ST    R2,AWIDEC                                                        
         DROP  R1                                                               
*                                                                               
         L     R3,AWIDEC                                                        
         USING WIDED,R3                                                         
*                                                                               
         MVI   PBNAMX,C' '                                                      
         MVC   XP,XSPACES                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    DIVSD,DIVSD                                                      
         XC    OLDCLT(36),OLDCLT    OJOB,ODIV,OREG,ODST,OPRD,OEST               
         XC    PDIVKEY,PDIVKEY                                                  
         XC    PREGKEY,PREGKEY                                                  
         XC    PDSTKEY,PDSTKEY                                                  
         XC    SRTRCSV,SRTRCSV                                                  
         XC    SAVEKEY2(36),SAVEKEY2        SAVEKEY ALSO                        
         MVI   DRDSW,0       INITIALIZE DUMMY REGION-CLIENT      L09            
         CLC   QPUB+1(3),=C'RD='   WAS DUMMY DRD SCHEME REQSTD   L09            
         BNE   *+8                                               L09            
         MVI   DRDSW,255                                         L09            
         XC    KEY,KEY                                                          
         MVI   FRSTSW,C'Y'     Y                                                
         MVI   BRKSW,0                                                          
MULMED5  CLI   RCMULTIQ,C'C'       SEE IF COMBINED                              
         BE    MULMED5C                                                         
         CLI   RCMULTIQ,C'Y'                                                    
         BNE   DONTWORY                                                         
         MVC   MEDNMSV,PAGYMED     SAVE MEDIA NAME                              
MULMED5C CLI   FMULTMED,C' '      CHECK IF FIRST MEDIA FOUND                    
         BH    MULMED7                                                          
         MVC   FMULTMED,QMEDIA  SAVE FIRST MEDIA FOR "C" OR "*" REQ             
         CLI   RCMULTIQ,C'C'                                                    
         BE    DONTWORY                                                         
         B     MULMED8                                                          
*****                                                                           
*****    CLI   QMEDIA,C'M'       ON ALL MEDIA READ/IS 'M' DONE                  
*****    BE    MULMED8                                                          
*****                                                                           
MULMED7  XC    SVINSDSP(3),SVINSDSP                                             
         CLC   PAGE,=H'1'                                                       
         BNE   NOHEAD                                                           
         CLI   LINE,X'63'                                                       
         BE    NOHEAD1                                                          
         CLI   LINE,X'13'                                                       
         BNH   NOHEAD1                                                          
NOHEAD   MVI   FORCEHED,C'N'                                                    
NOHEAD1  MVC   SAVPARS+4(4),SVSRTLN    PXZ SRT +KEY LN                          
         XC    SVINSDSP(3),SVINSDSP                                             
         B     EXIT                YES/SKIP REST OF TLFIRST                     
MULMED8  XC    TTINSRT,TTINSRT                                                  
         MVI   ANYPRTSW,C'N'                                                    
         LA    R4,FINLTOTS                                                      
         LA    R1,20                 LOOP CLEARS FINLTOTS & TOTTOTS             
TOTZAP   ZAP   0(12,R4),=P'0'                                                   
         LA    R4,12(R4)                                                        
         BCT   R1,TOTZAP                                                        
DONTWORY MVC   PAGE,=H'1'                                                       
         MVI   DUPSW,0                                                          
*                                                                               
         B     TLF1                                                             
*                                                                               
**DP     CLC   QAGENCY,=C'DP'                                                   
**DP     BNE   TLF1                                                             
**DP     MVC   SVQCLI,QCLIENT                                                   
**DP     MVC   QCLIENT,=C'DP '                                                  
*                                                                               
*              HANDLE DUPONT AGENCY REQUESTING CLIENT DUPONT FOR                
*              A SPECIFIC PUB NO                                                
*       BBD&O (BD) IS THE MASTER AGENCY FOR PUB NUMBERS.  THEY RECORD           
*       ALL OTHER AGENCIES PUB NUMBER USAGE. LINKS ARE CROSS REFERENCED         
*       FROM AND TO THESE AGENCIES.  EG PUB 9 IN BBDO IS TIME INC.              
*       ANOTHER AGENCY USES PUB NO. 876 FOR TIME. BBDO WILL KEEP A              
*       X'14' ELEMENT IN PUBMASTER FILE INDICATING WHAT THE AGENCY              
*       ID IS AS WELL AS THEIR PUB NUMBER FOR TIME. KEEP IN MIND THAT           
*       MORE THAN 1 AGENCY MIGHT USE TIME WITH DIFF PUB NO. BBDO WILL           
*       HAVE MULTIPLE ELEMENTS IN THAT CASE. NOTE THAT THE OTHER AGENCY         
*       WILL HAVE A X'14' ELEMENT WITH BD AS ID AND BD'S PUB NO FOR             
*       TIME.                                                                   
*                                                                               
**DP     DS    0H                                                               
**DP     CLI   QPUB,C'0'                                                        
**DP     BL    TLF1                                                             
**DP     XC    LNKPUBS,LNKPUBS         BUILD LIST OF PUB LINKS                  
**DP     MVC   KEY(1),QMEDIA                                                    
**DP     PACK  KEY+1(6),QPUB(11)       PUB NUMBER AND ZONE                      
**DP     MVC   KEY+6(1),QPUB+10        EDITION                                  
**DP     CLI   QPUB+10,C' '                                                     
**DP     BNE   *+8                                                              
**DP     MVI   KEY+6,0                                                          
**DP     MVC   KEY+7(2),=C'BD'         GO READ BD PUB TO GET LNKS               
**DP     MVI   KEY+9,X'81'                                                      
**DP                                                                            
**DP     LA    R0,DMREAD                                                        
**DP     MVC   KEYSAVE,KEY             READ DIRECTORY                           
* PUB-IO                                                                        
**DP     GOTO1 DATAMGR,DMCB,(R0),PUBDIR,KEY,KEY                                 
**DP     MVC   BYTE,DMCB+8                                                      
**DP     NC    BYTE,DMOUTBTS                                                    
**DP     BZ    *+6                                                              
**DP     DC    H'0'                                                             
*  PUB-IO                               READ PUBREC                             
**DP     GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,PUBREC,(0,DMWORK)             
**DP     MVC   BYTE,DMCB+8                                                      
**DP     NC    BYTE,DMOUTBTS                                                    
**DP     BZ    *+6                                                              
**DP     DC    H'0'                                                             
**DP     MVC   LNKPUBS(2),=C'BD'                                                
**DP     MVC   LNKPUBS+2(6),KEY+1          FIRST ENTRY IS BD PUB                
**DP     LA    R5,LNKPUBS+9                SET R5 TO NEXT ENTRY                 
**DP                                                                            
**DP     LA    R6,PUBREC+33               POINT TO 1ST ELEMENT                  
**DP     MVI   ELCODE,X'14'               LOOK FOR CROSS REFERENCE              
**DP0C   BAS   RE,NEXTEL1                                                       
**DP     BNE   TLIN0D                                                           
**DP     CLI   2(R6),255  ENSURE THIS IS A LINK TO OTHER AGENCY L06             
**DP                      PUB NUMBER                                            
**DP     BNE   TLIN0C                                                           
**DP     MVC   0(2,R5),3(R6)             MOVE AGENCY'S ID                       
**DP     MVC   2(6,R5),17(R6)            SAVE AGECY'S PUB NUMBER                
**DP     LA    R5,9(R5)                                                         
**DP     B     TLIN0C                                                           
**DP                                                                            
**DP0D   MVC   0(5,R5),=5X'FF'             END OF TABLE MARKER.                 
**DP                                                                            
         SPACE                                                                  
TLF1     CLC   QDIV,=C'ALL'                                                     
         BE    TLF1A                                                            
         CLC   QDIV,SPACES                                                      
         BE    *+10                                                             
         MVC   OLDDIV,QDIV                                                      
TLF1A    CLC   QREGION,=C'ALL'                                                  
         BE    TLF1B                                                            
         CLC   QREGION,SPACES                                                   
         BE    *+10                                                             
         MVC   OLDREG,QREGION                                                   
TLF1B    CLC   QDIST,=C'ALL'                                                    
         BE    TLF1C                                                            
         CLC   QDIST,SPACES                                                     
         BE    *+10                                                             
         MVC   OLDDST,QDIST                                                     
         SPACE                                                                  
TLF1C    CLI   QOPT2,C'Y'          IF BOXES ARE SUPPRESSED                      
         BNE   TLF2                THEN UNDERLINE HEADLINE,                     
         MVC   DASHES,=40C'-'      ELSE                                         
         B     TLF4                                                             
TLF2     MVC   DASHES,SPACES       MOVE IN SPACES.                              
*                                                                               
TLF4     MVI   RUNSW,0             IF PUB =NNN AND/OR REGION =NNN               
         CLI   QPUB,C'0'           THEN SET TO READ IN PUB ORDER                
         BL    TLF4A                                                            
*                                                                 L15           
* IF A SPECIFIC PRODUCT WAS REQUESTED FOR A PUB DO NOT USE        L15           
*    PUB POINTER KEYS "21".  PASSIVE POINTERS NOT SET UP FOR 21'S L15           
*                                                                 L15           
*                                                                 BUG23         
*        CAN'T USE X'21' POINTERS UNLESS REUEST IS FOR ZZZ        BUG23         
*        SINCE X'21' POINTERS DO NOT EXIST FOR THE ALLOCATED      BUG23         
*        PRODUCTS                                                 BUG23         
         CLI   PASSPTRS,C'N'       MEANS ZZZ REQUESTED            BUG23         
         BNE   TLF4A                                              BUG23         
         MVI   RUNSW,X'21'         READ IN PUB ORDER                            
         B     TLF4D                                                            
TLF4A    CLC   QREGION,SPACES                                                   
         BE    TLF4D                                                            
         CLC   =C'ALL',QREGION                                                  
         BE    TLF4D                                                            
         MVI   RUNSW,X'21'         READ IN PUB ORDER                            
TLF4D    MVI   FIRSTT,C'Y'                                                      
         XC    KEY,KEY                                                          
         XC    PESTREC(50),PESTREC                                              
         XC    PBUYREC(256),PBUYREC                                             
         XC    SRTLIN,SRTLIN                                                    
         XC    RPTTAB,RPTTAB                                                    
         XC    DISPLINE,DISPLINE                                                
         XC    ACCODE(2),ACCODE                                                 
         XC    BRKKEY,BRKKEY                                                    
SIGN     EQU   12                                                               
*                                                                               
         XC    ACCUMS(256),ACCUMS   CLEAR                       L01             
         MVC   ACCUMS+256(104),ACCUMS                   BUG06                   
*                                                                BUG06          
         LA    RF,ACCUMS+11         POINT TO SIGN               L01             
         LA    RE,30                                            L01             
ACCZAP   OI    0(RF),SIGN                                       L01             
         LA    RF,12(RF)                                       L01              
         BCT   RE,ACCZAP                                        L01             
*                                                               L01             
        XC    MULTIPLY,MULTIPLY   CLEAR MULTIPLIER                L11           
        XC    NOPUBLKU,NOPUBLKU   CLEAR CLIENT PUBLOOKUP TABLE    L06           
        MVI   NOPUBLKU+(L'NOPUBLKU-1),255                         L06           
*                                                                 L11           
         XC    ADSDST,ADSDST                                                    
         MVI   PRTSW,C'Y'                                                       
         XC    AP1,AP1                                                          
         XC    INSDTSW(7),INSDTSW                                               
         MVI   LINENEED,4                                                       
         TITLE 'PPL102  - BUILD REPORT TABLE'                                   
         SPACE                                                                  
*                                                                               
*        BUILD RPTTAB - REPORT TABLE                                            
*                                                                               
*  5 BYTE ENTRIES   CODE (1), SORT LEN,DISPLAY LEN,TOTALS,SORTSW                
*        SORT SW  =X'FF' NOT SORTING ON THIS FIELD                              
*        DISP LEN =X'FF' NOT DISPLAYING THIS FIELD                              
*        TOTALS   =X'FF' NO TOTALS ON THIS FIELD                                
*        IF DISP LEN HAS HIGH ORDER BIT ON (X'80')                              
*        FIELD IS DISPLAYED ONLY IN HEADS(** NOT USING THIS OPT**)              
*                                                                               
*                                                                   L19         
         XC    OLDNAMC,SPACES                                       L19         
         MVI   PPRDKPRD,0          FORCE TO READ PRODUCT EVERY NEW  L19         
*                                  REQUEST OAN PROBLEM              L19         
         XC    OLDNAM,SPACES                                        L19         
         MVC   OLDOAN,SPACES                                        L19         
         XC    KEY,KEY                                                          
         MVI   COSTOPT,0    INITIALIZE                                          
         MVI   DIFFACCU,0        DIFFERENCE ACCUMULATOR INDICATOR L04           
         MVC   KEY(2),QAGENCY                                                   
         MVI   KEY+2,C'I'         FIRST TRY FOR MEDIA I                         
         CLI   RCMULTIQ,C'C'      COMBINED MEDIA                                
         BE    USERP5                                                           
         CLI   RCMULTIQ,C'Y'      OR MEDIA *                                    
         BE    USERP5                                                           
         MVC   KEY+2(1),QMEDIA                                                  
USERP5   MVI   KEY+3,X'30'                                                      
         MVC   KEY+4(4),QPAY       QPAY HAS REC CODE                            
* USERP-IO DIRECTORY                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    USERP6                                                           
*                                                                               
         CLI   RCMULTIQ,C'C'     SEE IF COMBINED MEDIA REQUEST                  
         BE    USERP5C                                                          
         CLI   RCMULTIQ,C'Y'     OR MEDIA "*"                                   
         BNE   USERP5X                                                          
USERP5C  CLI   KEYSAVE+2,C'I'    SEE IF I WAS LOOKING FOR MEDIA 'I'             
         BNE   USERP5X                                                          
         XC    KEY,KEY           IF I DIDN'T FIND "I"                           
         MVC   KEY(8),KEYSAVE                                                   
         MVI   KEY+2,C'M'        TRY FOR MAGAZINE                               
         B     USERP5                                                           
*                                                                               
USERP5X  MVI   MODE,LBUYREQ      IF USERP RECORD NOT FOUND                      
         MVI   FMULTMED,C' '     CLEAR FIRST MEDIA (MULTIMED REQ)               
         B     EXIT              MAYBE I CAN JUST SKIP THIS REQ                 
***                              WAS A DC   H'0'                                
         SPACE                                                                  
USERP6   DS    0H                                                               
         LA    R2,RPTTAB                                                        
         LA    R6,PBUYREC                                                       
         ST    R6,AREC                                                          
* USERP-IO  GET RECORD                                                          
         GOTO1 GETPRT                                                           
         SPACE                                                                  
         MVI   OFCLTSW,0                                       BUG01            
         CLI   QCLIENT,C'*'        IS IT OFFICE                                 
         BE    USERP7                                          L25              
         CLI   QCLIENT,C'&&'       OR GROUP                    L25              
         BNE   CK4MAST  CHECK FOR MATER AGENCY                 L21              
*                                                              BUG01            
USERP7   MVI   OFCLTSW,C'Y'                                    BUG01            
USERP8   LA    R6,33(R6)    POINT TO FIRST ELEMENT             BUG28            
         MVI   ELCODE,X'20' SEE IF FIRST EL IS A CLIENT        BUG01            
         BAS   RE,NEXTEL1                                      BUG01            
         BE    *+6                                             BUG01            
         DC    X'0000'                                         BUG01            
         CLI   2(R6),1      IS THIS A CLIENT SELECTION ELEMENT BUG01            
         L     R6,AREC      REINITIALIZE                       BUG01            
         BNE   TLF3D                                           BUG01            
         B     TLF3                                            BUG01            
*                                                              BUG01            
CK4MAST  XC    MASTCLI,MASTCLI    CLEAR MASTER AGENCY             L21           
         CLI   RCMULTIQ,C'C'     CANNOT HAVE COMBINE MEDIA REQUESTL21           
         BE    TLF3              CLIENT                           L21           
*  PPG DOES NOT READ CLIENT                                       L21           
*                                                                 L21           
         MVI   KEY+3,2           CLI CODE                         L21           
         MVC   KEY+4(3),QCLIENT                                   L21           
         XC    KEY+7(20),KEY+7                                    L21           
         GOTO1 HIGH                                               L21           
         CLC   KEY(20),KEYSAVE                                    L21           
         BE    GETMCL                                          BUG13            
         CLI   RCMULTIQ,C'Y'     MULTI MEDIA REQUEST           BUG13            
         BE    TLF3              CLIENT                        BUG13            
         CLC   QCLIENT,=C'ALL'   SEE IF ALL CLIENTS            BUG28            
         BE    TLF3              CAN'T READ CLIENT             BUG28            
         DC    H'0'              CRASH                                          
GETMCL   GOTO1 GETCLI                                             L21           
*                                                                 L21           
         CLI   PCLTPROF+5,C'1'    MASTER CLIENT INDICATOR         L21           
         BNE   TLF3                                               L21           
         MVC   MASTCLNA,PCLTNAME   SAVE MASTER CLIENT NAME        L21           
         MVC   MASTCLI,QCLIENT     SAVE MASTER CLIENT             L21           
         B     TLF3                                               L21           
*                                                                 L21           
TLF3D    MVI   0(R2),X'01'       YES/ INCLUDE CLIENT IN SORT/TOTAL              
         MVI   1(R2),X'03'                                                      
         MVI   2(R2),X'FF'       BUT DO NOT DISPLAY                             
         LA    R2,5(R2)                                                         
         SPACE                                                                  
TLF3     CLC   QDIV,=C'ALL'        DIVISION=ALL                                 
         BNE   TLF3A                                                            
         MVC   0(4,R2),=X'F903FF00'  INCLUDE DIV IN SORT         BUG21          
*                               DO NOT DISPLAY NO TOTAL          L09            
         LA    R2,5(R2)                                                         
         SPACE                                                                  
TLF3A    CLC   QREGION,=C'ALL'     REGION=ALL                                   
         BNE   TLF3B                                                            
         MVC   0(4,R2),=X'020BFF00'                              L09            
         LA    R2,5(R2)                                                         
         SPACE                                                                  
TLF3B    CLC   QDIST,=C'ALL'       DISTRICT=ALL                                 
         BNE   TLF5                                                             
         MVC   0(4,R2),=X'0303FF00'                              L09            
*                                                                L09            
*                                                                L09            
*                                                                L09            
         LA    R2,5(R2)                                                         
         EJECT                                                                  
TLF5     XC    KEY,KEY             TLIN EXPECTS CLEAR KEY FIRST TIME            
         LA    R6,33(R6)                                                        
         CLI   0(R6),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PUSRTELM,R6                                                      
         LA    R4,PUSRTNM                                                       
         GOTO1 =V(CENTER),DMCB,(R4),30,RR=RELO                                  
         MVC   HEADSAVE,0(R4)                                                   
         ST    R6,DUB                                             L12           
         XC    DESC93,DESC93                                      L12           
         MVI   ELCODE,93                                          L12           
         BAS   RE,NEXTEL1                                         L12           
         BNE   LR6DUB                                             L12           
         ZIC   RE,1(R6)    LENGTH                                 L12           
         BCTR  RE,0                                               L12           
         EX    RE,*+8                                             L12           
         B     LR6DUB                                             L12           
         MVC   DESC93(0),0(R6)      SAVE DESCRIPTION              L12           
LR6DUB   L     R6,DUB                                             L12           
*                                                                 L12           
         MVI   ELCODE,X'20'                                                     
         USING PUSRSELM,R6                                                      
         BAS   RE,NEXTEL1                                                       
         BNE   TLF7                                                             
TLF6     MVC   0(1,R2),PUSRSSEL     SELECTION CODE                              
         CLI   0(R1),X'13'         IS IT COMMENTS                               
         BNE   *+8                                                              
         MVI   CMNTSW,C'Y'         SET ON COMMENT SWITCH                        
         MVC   1(1,R2),PUSRSSRT    SET SORT LENGTH                              
         MVC   2(1,R2),PUSRSDSP    SET DISPLAY LENGTH                           
         CLI   0(R2),93    CALCULATION ELEMENT                    L11           
         BNE   NOTCALC                                            L11           
         CLI   1(R6),13    IS THERE A MULTIPLIER                  L11           
         BNE   NOTCALC                                            L11           
         MVC   MULTIPLY,PUSRMULT SAVE MULTIPLIER                  L11           
         MVC   ARITHOPT,PUSROPT  IS THERE A SUM AS WELL           L11           
         OI    MULTIPLY+4,12     FORCE SIGN                       L11           
NOTCALC  DS    0H                                                 L11           
         CLI   PUSRSDSP,X'F0'    DO NOT PRINT DATA                L13           
         BE    TLF6AA                                             L13           
         ZIC   R0,PUSRSDSP         ADD DISP LEN                                 
         L     R1,DISPLINE             TO DISP                                  
         AR    R1,R0                      LINE.                                 
         ST    R1,DISPLINE                                                      
         EJECT                                                                  
**************************************************************                  
* IF IT'S M/T/A ON DATE FIELDS, SET APPROPRIATE DATE SWITCH                     
* NOTE THAT M OPTION GIVES MONTHLY ACCUMS                                       
*      THAT T OPTION GIVES INDIV INSERT DATE+SUB-TOTAL MONTH BRK                
*      THAT A OPTION GIVES COMBINATION OF M + T                                 
* SO ON A OPTION YOU GET MONTHLY ACCUM OF INS DATE PLUS A TOTAL                 
* WHEN THE MONTH CHANGES                                                        
*                                                                               
         TM    PUSRSTS,X'08'      WEEKLY TOTALS                  L10            
         BNO   NOMORE                                            L10            
         MVI   WEEK,255           SET TO LOAD START & END DATE   L10            
*                                                                L10            
         ST    R2,DBL                                            L10            
         L     R2,AWEEK                                          L10            
*                                                                L10            
         MVC   STDATE,QSTART      START DATE                     L10            
         PRINT GEN                                                              
DATEAGN  LA    RF,6                                              L10            
         GOTO1 ADDAY,DMCB,STDATE,EDATE,(RF)                      L10            
         PRINT NOGEN                                             L10            
*        GOTO1 DTCNV,DMCB,(0,STDATE),(2,0(R2))                   L10            
         GOTO1 DATCON,DMCB,(0,STDATE),(2,0(R2))                  L10            
*        GOTO1 DTCNV,DMCB,(0,EDATE),(2,2(R2))                   L10             
         GOTO1 DATCON,DMCB,(0,EDATE),(2,2(R2))                  L10             
         LA    R2,4(R2)     TO NEXT ENTRY                        L10            
         CLC   EDATE,QEND      ENSURE SOME STOP POINT            L10            
         BH    LOADIT                                            L10            
         LA    RF,1                                              L10            
         GOTO1 ADDAY,DMCB,EDATE,STDATE,(RF)                      L10            
         B     DATEAGN                                           L10            
*                                                                L10            
LOADIT   L     R2,DBL     RESTORE R2                             L10            
NOMORE   DS    0H                                                L10            
         TM    PUSRSTS,X'04'       X'04'=M,X'02'=T,X'06'=A                      
         BZ    TLF6A                                                            
         CLI   0(R2),X'1E'         INSERTION DATE                               
         BNE   MT2                                                              
         MVI   INSDTSW,C'Y'                                                     
         TM    PUSRSTS,X'02'       IS IT TOTALS                                 
         BO    TLF6AA                                                           
         MVI   3(R2),X'FF'         SET NO TOTALS SWITCH                         
         B     TLF6AA                                                           
MT2      CLI   0(R2),X'1F'         CLOSING DATE                                 
         BNE   MT4                                                              
         MVI   CLSDTSW,C'Y'                                                     
         TM    PUSRSTS,X'02'       IS IT TOTALS                                 
         BO    TLF6AA                                                           
         MVI   3(R2),X'FF'         SET NO TOTALS SWITCH                         
         B     TLF6AA                                                           
MT4      CLI   0(R2),X'20'         ON-SALE DATE                                 
         BNE   MT6                                                              
         MVI   ONSLDTSW,C'Y'                                                    
         TM    PUSRSTS,X'02'       IS IT TOTALS                                 
         BO    TLF6AA                                                           
         MVI   3(R2),X'FF'         SET NO TOTALS SWITCH                         
         B     TLF6AA                                                           
MT6      CLI   0(R2),X'21'         PAYABLE DATE                                 
         BNE   MT8                                                              
         MVI   PAYDTSW,C'Y'                                                     
         TM    PUSRSTS,X'02'       IS IT TOTALS                                 
         BO    TLF6AA                                                           
         MVI   3(R2),X'FF'         SET NO TOTALS SWITCH                         
         B     TLF6AA                                                           
MT8      CLI   0(R2),X'22'         BILLABLE DATE                                
         BNE   MT9                                                              
         MVI   BILDTSW,C'Y'                                                     
         TM    PUSRSTS,X'02'       IS IT TOTALS                                 
         BO    TLF6AA                                                           
         MVI   3(R2),X'FF'         SET NO TOTALS SWITCH                         
         B     TLF6AA                                                           
MT9      CLI   0(R2),X'23'         MAT CLOSING DATE                             
         BNE   TLF6A                                                            
         MVI   MATDTSW,C'Y'                                                     
         TM    PUSRSTS,X'02'       IS IT TOTALS                                 
         BO    TLF6AA                                                           
         MVI   3(R2),X'FF'         SET NO TOTALS SWITCH                         
         B     TLF6AA                                                           
         SPACE                                                                  
TLF6A    TM    PUSRSTS,X'01'       IS IT A SORT                                 
         BNZ   *+8                                                              
         MVI   4(R2),X'FF'         NO.                                          
         TM    PUSRSTS,X'02'       IS IT TOTALS                                 
         BNZ   *+8                                                              
         MVI   3(R2),X'FF'         NO                                           
TLF6AA   LA    R2,5(R2)                                                         
         BAS   RE,NEXTEL1                                                       
         BE    TLF6                                                             
TLF7     MVC   0(2,R2),=X'FFFF'                                                 
         XC    STDCODE1(12),STDCODE1    CLEAR OPTIONAL STD COMMNT L08           
*                                                                 L08           
*                                                                               
         MVI   SUPRNSW,C'N'                                                     
         MVI   ELCODE,X'15'                                                     
         LA    R6,PBUYREC+33                                                    
         BAS   RE,NEXTEL1                                                       
         BNE   CNTR                                                             
         USING PUSRPELM,R6                                                      
         CLI   PUSRPSUP,C'Y'                                                    
         BNE   CNTRA                                              L08           
         MVI   SUPRNSW,C'Y'                                                     
CNTRA    DS    0H                                                 L08           
*                                                                 L08           
         MVC   STDCODE1(12),PUSRCOM1  OPTIONAL USER STD COMM 1+2  L08           
*                                                                 L08           
*                                                                               
*        B     CNTR ***********************************************             
         CLI   PUSRSPAC,0         OLD OR DEFAULT VALUE                          
         BNE   *+12                                                             
         MVI   SPACEING,2         DEFAULT IS 2                                  
         B     CNTR                                                             
         ZIC   R1,PUSRSPAC        IF VALUE IS THERE THEN MUST INCREMNT          
         LA    R1,1(R1)           TO GET TRUE SPACING// DONE THIS WAY           
         STC   R1,SPACEING        FOR CLARITY FOR USER ON LINE   L22            
*                                                                 L22           
* THIS ROUTINE WILL CENTER OUTPUT BY SUBTRACTING TOTAL DISPLAY *                
* LINE LENGTH FROM 165 OR 131                                                   
* AND DIVIDE BY 2, THE QUOTIENT IS ADDED TO THE PRINT LINES                     
*                                                              *                
*                                                                               
CNTR     DS    0H                                                               
         L     R1,DISPLINE                                                      
         LA    R2,131                                                           
         C     R2,DISPLINE                                                      
         BNL   CNTR10                                                           
         LA    R2,165                                                           
CNTR10   SR    R2,R1                                                            
         LTR   R2,R2                                                            
         BZ    TLF7A                                                            
         BP    *+6                                                              
         DC    H'0'                MUST NOT  BE NEGATIVE                        
         SRA   R2,1                DIV BY 2(SRA IGNORES REMAINDER)              
         SPACE                                                                  
* R2 NOW CONTAINS NUMBER TO BE ADDED TO P LINES                                 
TLF7A    LTR   R2,R2               IF R2=0,ADD 1 FOR BOXES                      
         BNZ   *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R1,XP1                                                           
         AR    R1,R2                                                            
         ST    R1,AP1                                                           
         LA    R1,SAVHD7                                                        
         AR    R1,R2                                                            
         ST    R1,ASVHD7                                                        
         LA    R1,SAVHD8                                                        
         AR    R1,R2                                                            
         ST    R1,ASVHD8                                                        
         LA    R1,SAVHD9                                                        
         AR    R1,R2                                                            
         ST    R1,ASVHD9                                                        
         EJECT                                                                  
*                                                                               
* SET UP BRKKEYS TO CHK FOR REQUESTED SUB-TOTAL BREAKS                          
*        CL3=SELECTION ID/DISP INTO SORT KEY/SORT LENGTH                        
         SPACE                                                                  
TLF7C    XC    HALF,HALF                                                        
         XC    FULL,FULL                                                        
         LA    R1,RPTTAB                                                        
TLF8     CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    TLF10                                                            
         CLI   2(R1),X'F0'     INVISIBLE FIELD                    L13           
         BE    TLF9                                               L13           
         CLI   4(R1),X'FF'         SEE IF SORTING ON THIS FIELD                 
         BE    TLF9A               NO BUT ADD TO SORT L'                        
TLF8AA   CLI   3(R1),X'FF'         SEE IF TOTALS ON THIS FIELD                  
         BE    TLF8E               NO SKIP BRKKEY BUT ADD TO SORT L'            
         SPACE                                                                  
         LA    R4,BRKKEY                                                        
*                                                                L10            
* PREVENT HAVING MORE THAN ONE SELECTION IN BRKKEY  PROBLEM      L10            
*  OCCURRS WHEN OPTIONS FOR DIVISION,DISTRICT OR REGION =ALL     L10            
*  WITH ANY OF THESE OPTIONS AN ENTRY FOR DRD IS GENERATED IN    L10            
*  RPTTAB.  IF DRD IS DEFINED IN THE USERP REC, A DUPLICATE      L10            
*  OPTION IS GENERATED CAUSING PROBLEMS WITH TOTALS              L10            
*                                                                L10            
TLF8A    DS    0H                                                L10            
         CLC   0(1,R4),0(R1)  IS BRKKEY = RPPTAB                 L10            
         BE    TLF8E   DO NOT INCLUDE IN BRKKEY BUT MUST USE   L10              
*                      LENGTH TO CALCULATE LENGTH FOR SORT     L10              
*  NOTE** SORT RECORD AND KEY LENGTH CALCULATED HERE HOWEVER                    
*         DISPLACEMENT FOR RECORD DISK ADDRESS IS DETERMINED AT                 
*         LABEL TLIN20.  THIS ROUTIINE USES RPTTAB ONLY TO CALCULATE            
*         THE DISPLACEMENT.  DESCREPANCIES IN LENGTHS HAVE OCCURRED.            
*                                                                               
         CLI   0(R4),0                                           L10            
         BE    TLF8B                                                            
         LA    R4,3(R4)                                                         
         B     TLF8A                                                            
TLF8B    MVC   0(1,R4),0(R1)      MOVE IN SELECTION ID                          
         LH    R5,HALF                                                          
         STCM  R5,1,1(R4)          SET SORT DISP                                
         MVC   2(1,R4),1(R1)       SET SORT LENGTH                              
         SPACE                                                                  
TLF8E    ZIC   R0,1(R1)                                                         
         LH    R2,HALF                                                          
         AR    R2,R0                                                            
         STH   R2,HALF                                                          
         SPACE                                                                  
TLF9     LA    R1,5(R1)                                                         
         B     TLF8                                                             
*                                                                               
TLF9A    CLI   0(R1),X'27'         IF $ FIELD SKIP ENTIRELY                     
         BH    TLF9                                                             
         CLI   0(R1),X'13'         IF COMMENTS SKIP ENTIRELY                    
         BE    TLF9                                                             
         CLI   0(R1),X'1A'      BLANK COLUMN   DO NOT ADD FOR NON-              
         BE    TLF9             PRIMARY SORT                                    
         CLI   1(R1),X'FF'      NO SORT ON THIS COLUMN           -              
         BE    TLF9             PRIMARY SORT                                    
         ZIC   R0,1(R1)          SORT LEN OF NON-PRIMARY SORT FIELDS            
         A     R0,FULL                                                          
         ST    R0,FULL                                                          
         B     TLF9                                                             
*                                                                               
TLF10    LH    R0,HALF             HALF HAS PRIMARY SORT L'                     
         A     R0,FULL             ADD L' NON-PRIMARY SORT FIELDS               
         CH    R0,=H'90'           90 IS MAX SORT LENGTH                        
         BNH   *+8                                                              
         LH    R0,=H'90'                                                        
         AH    R0,=H'2'            INCREASE SORT LEN BY 2                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAVPARS+6(2),DUB    KEY LENGTH                                   
         AH    R0,=H'4'            FOR DISK ADDR                                
         AH    R0,=H'3'     ALLOW FOR PASSIVE POINTER PRODUCT     L15           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAVPARS+4(2),DUB    SORT RECORD LENGTH                           
         MVC   SVSRTLN(4),SAVPARS+4   PXZ                                       
*  BUILD HEADLINES            *                                                 
*  AND SAVE IN SAVHD7,8,9     *                                                 
*                             *                                                 
TLH      DS    0H                                                               
         MVC   SAVHD7,XSPACES                                                   
         MVC   SAVHD8,XSPACES                                                   
         MVC   SAVHD9,XSPACES                                                   
         XC    DDISP,DDISP         DISPLACEMENT INTO P                          
*                                  CHK RPTTAB TO SEE IF ALL DOLLAR              
         LA    R4,RPTTAB           FIELDS ARE OPEN, IF SO DO NOT                
         MVI   ONLYOPN,C'Y'                                                     
         MVI   ANYOPN,C'N'                                                      
TLH2A    CLI   0(R4),X'FF'         DISPLAY OPEN IN HEADLINES                    
         BE    TLH4                                                             
         CLI   0(R4),40                                                         
         BL    TLH2X                                                            
         CLI   0(R4),85                                                         
         BH    TLH2D                                                            
         MVI   ONLYOPN,C'N'                                                     
         B     TLH2X                                                            
TLH2D    CLI   0(R4),140                                                        
         BL    TLH2X                                                            
         CLI   0(R4),185                                                        
         BH    TLH2F                                                            
         MVI   ANYOPN,C'Y'                                                      
         B     TLH2X                                                            
TLH2F    CLI   0(R4),200                                                        
         BL    TLH2X                                                            
         CLI   0(R4),245                                                        
         BH    TLH2X                                                            
         MVI   ONLYOPN,C'N'                                                     
TLH2X    LA    R4,5(R4)                                                         
         B     TLH2A                                                            
*                                                                               
TLH4     LA    R4,RPTTAB                                                        
TLH5     CLI   0(R4),X'FF'       END OF TABLE                                   
         BE    TLHX                                                             
         CLI   2(R4),X'FF'                                                      
         BE    TLH40               NOT DISPLAYING THIS FIELD                    
         CLI   2(R4),X'F0'     INVISIBLE FIELD                    L13           
         BNE   NOXCET                                            L13            
         CLI   0(R4),46  IF COST AND NO DISPLAY FORCE TO READ     L13           
         BNE   TLH40     ESTIMATE FOR BILLING OPTIONS             L13           
         MVI   COSTOPT,255                                        L13           
         B     TLH40                                              L13           
NOXCET   DS    0H                                                 L13           
*                                                                               
         GOTO1 AHEADRTN,DMCB,(R4)                                               
*                                                                               
TLH40    LA    R4,5(R4)            NEXT ENTRY IN RPTTAB                         
         B     TLH5                                                             
         SPACE                                                                  
* TOTALS ACCUMULATORS                 *                                         
* SET UP ACCUMS/ CL1= $ FIELD ID      *                                         
*                CL1= SELECTION ID    *                                         
*                CL2= P LINE DISP     *                                         
*                CL8= $ ACCUMULATOR   *                                         
*                ---                                                            
*                CL12                                                           
         SPACE                                                                  
TLHX     DS    0H                                                               
         LA    R2,RPTTAB                                                        
         LA    R5,ACCUMS                                                        
LAC      CLI   0(R2),X'FF'         END OF RPTTAB                                
         BE    TLHXX                      SO EXIT.                              
         CLI   3(R2),X'FF'       IS THERE A TOTAL REQUEST ON THIS FIELD         
         BE    LAC10                                                            
         LA    R6,RPTTAB           YES.                                         
LAC02    CLI   0(R6),X'F9'        IS IT A DIV HEAD FIELD        BUG21           
         BE    LAC04                                                            
         CLI   0(R6),X'28'        IS IT A $ FIELD                               
         BL    LAC04                                                            
         CLI   0(R6),X'56'        IS IT NUMB OF INSRTS(THIS IS IN $COL)         
         BE    LAC04                                  (OF MENU        )         
         MVC   0(1,R5),0(R6)       ITS A DOLLAR FIELD/SET $ FIELD ID            
*  PRINT THIS TOTAL WHEN THERE IS A BREAK ON THIS ID                            
         MVC   1(1,R5),0(R2)       SET SELECTION ID                             
* *      LA    R5,8(R5)                                                         
         LA    R5,12(R5)                                          L01           
LAC04    LA    R6,5(R6)                                                         
         CLI   0(R6),X'FF'         END OF RPTTAB SEARCH FOR $ FIELD             
         BNE   LAC02                                                            
LAC10    LA    R2,5(R2)            YES.                                         
         B     LAC                                                              
         SPACE                                                                  
TLHXX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
TLINPUT  DS    0H                                                               
         GOTO1 ATLIN,DMCB,(RA)                                                  
         B     EXIT                                                             
         SPACE 3                                                                
TLOUTPUT DS    0H                                                               
         GOTO1 ATLOUT,DMCB,(RA)                                                 
         B     EXIT                                                             
         SPACE 3                                                                
TLLAST   DS    0H                                                               
         MVI   PBUYREC,X'FF'                                                    
         MVC   PBUYREC+1(24),PBUYREC                                            
         GOTO1 ATLOUT,DMCB,(RA)                                                 
         L     R1,ABOX                  RESET BOXFONT TO ZERO SO WILL           
         USING BOXD,R1                  HAVE UNIFORM REQUEST PAGES              
         MVI   BOXFONT,0                                                        
         CLI   RCMULTIQ,C'C'     SEE IF COMBINED                                
         BE    TLLASTC                                                          
         CLI   MODE,LBUYXRQ      OR LAST FOR MEDIA "*" REQ                      
         BNE   EXIT                                                             
TLLASTC  MVI   FMULTMED,C' '     CLEAR FIRST MEDIA FOUND                        
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
NEXTEL1  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTEL3                                                          
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     NEXTEL1+2                                                        
NEXTEL3  LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE                                                                  
*                                                                               
EXIT     DS    0H                                                               
         MVC   DMCB+4(8),SAVPARS                                                
XIT      DS    0H                                                               
         XIT1                                                                   
         TITLE  'HEADRTN- PPL102 - PRINT HEADLINES'                             
         LTORG                                                                  
**                                                                              
         EJECT                                                                  
HEADRTN  NMOD1 0,HEADRTN                                                        
         L     RC,PPFILEC                                                       
*                                                                               
         L     R5,AHEADTAB                                                      
HED10    CLI   0(R5),X'FF'         END OF DISPLAY TABLE                         
         BNE   *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         CLC   0(1,R4),0(R5)                                                    
         BE    HED15                                                            
HED10A   DS    0H                                                 L13           
*                                                                 L13           
         LA    R5,5(R5)                                                         
         B     HED10               NEXT ENTRY                                   
*                                                                               
HED15    DS    0H                                                 L13           
         CLI   2(R4),X'F0'       DO NOT PRINT DATA                L13           
         BNE   NOXCEPT                                            L13           
         CLI   0(R4),46  IF COST AND NO DISPLAY FORCE TO READ     L13           
         BNE   OUTROUT   ESTIMATE FOR BILLING OPTIONS             L13           
         MVI   COSTOPT,255                                        L13           
         B     OUTROUT                                            L13           
NOXCEPT  DS    0H                                                 L13           
         MVC   FULL,1(R5)                                         L13           
         L     RF,FULL                                                          
         BAS   RE,0(RF)                                                         
OUTROUT  DS    0H                                                 L13           
*                                                                 L13           
         XIT1                                                                   
**                                                                              
HD00     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
*                                                                               
         B     HD00A                                                            
*                                                                               
**DP     CLC   QAGENCY,=C'DP'          IF DOING DUPONT MOVE "AGENCY"            
**DP     BNE   HD00A                   TO COLUMN HEADER INSTEAD OF              
**DP     MVC   0(6,R5),=C'AGENCY'      "CLIENT".                                
**DP     B     *+10                                                             
HD00A    MVC   0(6,R5),=C'CLIENT'                                               
         MVC   198(6,R5),DASHES                                                 
         B     HD0XX                                                            
*                                                                               
HD0A     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(6,R5),=C'REGION'                                               
         MVC   198(6,R5),DASHES                                                 
         B     HD0XX                                                            
*                                                                               
HD0B     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(8,R5),=C'DISTRICT'                                             
         MVC   198(8,R5),DASHES                                                 
         B     HD0XX                                                            
*                                                                               
HD0C     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(11,R5),=C'REGION NAME'                                         
         MVC   198(11,5),DASHES                                                 
         B     HD0XX                                                            
*                                                                               
HD0D     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(13,R5),=C'DISTRICT NAME'                                       
         MVC   198(13,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD01     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(08,R5),=C'PUB CODE'                                            
         MVC   198(08,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD02     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(08,R5),=C'PUB NAME'                                            
         MVC   198(08,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD03     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(03,R5),=C'PRD'                                                 
         MVC   198(03,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD04     DS    0H                                                               
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(07,R5),=C'CLOSING'                                             
         MVC   198(07,R5),=C' DATE  '                                           
         CLI   CLSDTSW,C'Y'        IS IT M REQUEST                              
         BNE   *+10                                                             
         MVC   198(7,R5),=C'  MONTH'                                            
         MVC   396(07,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD05     DS    0H                                                               
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(09,R5),=C'INSERTION'                                           
         MVC   198(09,R5),=C'  DATE   '                                         
         CLI   WEEK,255       WEEKLY OPTION SET                  L10            
         BNE   *+10                                              L10            
         MVC   198(9,R5),=C' WEEK OF  '                          L10            
*                                                                L10            
         CLI   INSDTSW,C'Y'        IS IT M REQUEST                              
         BNE   *+10                                                             
         MVC   198(7,R5),=C'  MONTH'                                            
         MVC   396(09,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD06     DS    0H                                                               
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(07,R5),=C'ON-SALE'                                             
         MVC   198(07,R5),=C' DATE  '                                           
         CLI   ONSLDTSW,C'Y'        IS IT M REQUEST                             
         BNE   *+10                                                             
         MVC   198(7,R5),=C'  MONTH'                                            
         MVC   396(07,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD07     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(17,R5),=C'SPACE DESCRIPTION'                                   
         MVC   198(17,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD08     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(08,R5),=C'PRD NAME'                                            
         MVC   198(08,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
*                                                                               
HD09     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(11,R5),=C'COPY NUMBER'                                         
         MVC   198(11,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD10     DS    0H                                                               
         TM    2(R4),X'80'         SEE IF IN HEADLINES                          
         BNZ   0(RE)               RETURN                                       
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(03,R5),=C'DIV'                                                 
         MVC   198(03,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD11     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(03,R5),=C'EST'                                                 
         MVC   198(03,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD12     DS    0H                                                               
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(07,R5),=C'PAYABLE'                                             
         MVC   198(07,R5),=C' DATE  '                                           
         CLI   PAYDTSW,C'Y'        IS IT M REQUEST                              
         BNE   *+10                                                             
         MVC   198(7,R5),=C' MONTH '                                            
         MVC   396(07,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD13     DS    0H                                                               
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(08,R5),=C'BILLABLE'                                            
         MVC   198(08,R5),=C'  DATE  '                                          
         CLI   BILDTSW,C'Y'        IS IT M REQUEST                              
         BNE   *+10                                                             
         MVC   198(8,R5),=C'  MONTH '                                           
         MVC   396(08,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
*                                                                               
HD14     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(07,R5),=C'AD CODE'                                             
         MVC   198(07,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD15     DS    0H                                                               
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(08,R5),=C'MAT CLOS'                                            
         MVC   198(08,R5),=C'  DATE  '                                          
         CLI   MATDTSW,C'Y'        IS IT M REQUEST                              
         BNE   *+10                                                             
         MVC   198(8,R5),=C'  MONTH '                                           
         MVC   396(08,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
*  OAN CODE                                                        L19          
*                                                                  L19          
HD16     DS    0H                                                  L19          
         L     R5,ASVHD7                                           L19          
         AH    R5,DDISP                                            L19          
         MVC   0(04,R5),=C'OAN '                                   L19          
         MVC   198(04,R5),=C'CODE'                                 L19          
         MVC   396(08,R5),DASHES                                   L19          
         B     HD0XX                                               L19          
*                                                                  L19          
HD17     DS    0H                                                  L19          
         L     R5,ASVHD7                                           L19          
         AH    R5,DDISP                                            L19          
         MVC   0(12,R5),=C'AGENCY NAME '                           L19          
         MVC   198(08,R5),DASHES                                   L19          
         B     HD0XX                                               L19          
*                                                                  L19          
HD19     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(8,R5),=C'COMMENTS'                                             
         MVC   198(8,R5),DASHES                                                 
         B     HD0XX                                                            
*                                                                               
HD20     DS    0H                  GROSS ORDERED                                
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD20H                                                            
         CLI   0(R4),140                                                        
         BNE   HD20D                                                            
         MVC   4(10,R5),=C'OPEN GROSS'                                          
         B     HD25X                                                            
HD20D    CLI   0(R4),200                                                        
         BNE   HD20H                                                            
         MVC   2(12,R5),=C'REBATE GROSS'                          BUG08         
         B     HD25X                                                            
HD20H    MVC   8(5,R5),=C'GROSS'                                                
         B     HD25X                                                            
*                                                                               
HD21     DS    0H                  NET ORDERED                                  
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD21H                                                            
         CLI   0(R4),141                                                        
         BNE   HD21D                                                            
         MVC   6(8,R5),=C'OPEN NET'                                             
         B     HD25X                                                            
HD21D    CLI   0(R4),201                                                        
         BNE   HD21H                                                            
         MVC   4(10,R5),=C'REBATE NET'                           BUG08O         
         B     HD25X                                                            
HD21H    MVC   9(3,R5),=C'NET'                                                  
         B     HD25X                                                            
*                                                                               
HD22     DS    0H                  GROSS-CD ORDERED                             
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD22H                                                            
         CLI   0(R4),142                                                        
         BNE   HD22D                                                            
         MVC   1(4,R5),=C'OPEN'                                                 
         B     HD22H                                                            
HD22D    CLI   0(R4),202                                                        
         BNE   HD22H                                                            
         MVC   1(4,R5),=C'REBT'                                 BUG08           
HD22H    MVC   6(8,R5),=C'GROSS-CD'                                             
         B     HD25X                                                            
*                                                                               
HD23     DS    0H                  NET-CD ORDERED                               
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD23H                                                            
         CLI   0(R4),143                                                        
         BNE   HD23D                                                            
         MVC   3(4,R5),=C'OPEN'                                                 
         B     HD23H                                                            
HD23D    CLI   0(R4),203                                                        
         BNE   HD23H                                                            
         MVC   1(6,R5),=C'REBATE'                                BUG08          
HD23H    MVC   8(6,R5),=C'NET-CD'                                               
         B     HD25X                                                            
*                                                                               
HD24     DS    0H                  AGYCOM ORDERED                               
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD24H                                                            
         CLI   0(R4),144                                                        
         BNE   HD24D                                                            
         MVC   2(4,R5),=C'OPEN'                                                 
         B     HD24H                                                            
HD24D    CLI   0(R4),204                                                        
         BNE   HD24H                                                            
         MVC   0(6,R5),=C'REBATE'                                               
HD24H    MVC   7(7,R5),=C'AGY COM'                                              
         B     HD25X                                                            
*                                                                               
HD25     DS    0H                  CD ORDERED                                   
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD25H                                                            
         CLI   0(R4),145                                                        
         BNE   HD25D                                                            
         MVC   7(7,R5),=C'OPEN CD'                                              
         B     HD25X                                                            
HD25D    CLI   0(R4),205                                                        
         BNE   HD25H                                                            
         MVC   5(9,R5),=C'REBATE CD'                           BUG08            
         B     HD25X                                                            
HD25H    MVC   9(2,R5),=C'CD'                                                   
HD25X    MVC   198+7(07,R5),=C'ORDERED'                                         
         MVC   396+7(07,R5),DASHES                                              
         B     HD0XX                                                            
*                                                                               
*                                                                               
HD26     DS    0H                  GROSS PAID                                   
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   9(5,R5),=C'GROSS'                                                
         B     HD31X                                                            
*                                                                               
HD27     DS    0H                  NET PAID                                     
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   11(3,R5),=C'NET'                                                 
         B     HD31X                                                            
*                                                                               
HD28     DS    0H                  GROSS-CD PAID                                
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   6(8,R5),=C'GROSS-CD'                                             
         B     HD31X                                                            
*                                                                               
HD29     DS    0H                  NET-CD PAID                                  
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   8(6,R5),=C'NET-CD'                                               
         B     HD31X                                                            
*                                                                               
HD30     DS    0H                  AGYCOM PAID                                  
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   7(7,R5),=C'AGY COM'                                              
         B     HD31X                                                            
*                                                                               
HD31     DS    0H                  CD PAID                                      
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   11(2,R5),=C'CD'                                                  
HD31X    MVC   198+10(04,R5),=C'PAID'                                           
         MVC   396+10(04,R5),DASHES                                             
         B     HD0XX                                                            
*                                                                               
HD32     DS    0H                  GROSS BILLED                                 
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD32H                                                            
         CLI   0(R4),160                                                        
         BNE   HD32D                                                            
         MVC   4(4,R5),=C'OPEN'                                                 
         B     HD32H                                                            
HD32D    CLI   0(R4),220                                                        
         BNE   HD32H                                                            
         MVC   2(6,R5),=C'REBATE'                                BUG08          
HD32H    MVC   9(5,R5),=C'GROSS'                                                
         B     HD37X                                                            
*                                                                               
HD33     DS    0H                  NET BILLED                                   
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD33H                                                            
         CLI   0(R4),161                                                        
         BNE   HD33D                                                            
         MVC   6(8,R5),=C'OPEN NET'                                             
         B     HD37X                                                            
HD33D    CLI   0(R4),221                                                        
         BNE   HD33H                                                            
         MVC  4(10,R5),=C'REBATE NET'                             BUG08         
         B     HD37X                                                            
HD33H    MVC   10(3,R5),=C'NET'                                                 
         B     HD37X                                                            
*                                                                               
HD34     DS    0H                  GROSS-CD BILLED                              
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD34H                                                            
         CLI   0(R4),162                                                        
         BNE   HD34D                                                            
         MVC   1(4,R5),=C'OPEN'                                                 
         B     HD34H                                                            
HD34D    CLI   0(R4),222                                                        
         BNE   HD34H                                                            
         MVC   1(4,R5),=C'REBT'                                 BUG08           
HD34H    MVC   6(8,R5),=C'GROSS-CD'                                             
         B     HD37X                                                            
*                                                                               
HD35     DS    0H                  NET-CD BILLED                                
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD35H                                                            
         CLI   0(R4),163                                                        
         BNE   HD35D                                                            
         MVC   3(4,R5),=C'OPEN'                                                 
         B     HD35H                                                            
HD35D    CLI   0(R4),223                                                        
         BNE   HD35H                                                            
         MVC   1(6,R5),=C'REBATE'                                BUG08          
HD35H    MVC   8(6,R5),=C'NET-CD'                                               
         B     HD37X                                                            
*                                                                               
HD36     DS    0H                  AGYCOM BILLED                                
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD36H                                                            
         CLI   0(R4),164                                                        
         BNE   HD36D                                                            
         MVC   2(4,R5),=C'OPEN'                                                 
         B     HD36H                                                            
HD36D    CLI   0(R4),224                                                        
         BNE   HD36H                                                            
         MVC   0(6,R5),=C'REBATE'                                BUG08          
HD36H    MVC   7(7,R5),=C'AGY COM'                                              
         B     HD37X                                                            
*                                                                               
HD37     DS    0H                  CD BILLED                                    
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD37H                                                            
         CLI   0(R4),165                                                        
         BNE   HD37D                                                            
         MVC   7(7,R5),=C'OPEN CD'                                              
         B     HD37X                                                            
HD37D    CLI   0(R4),225                                                        
         BNE   HD37H                                                            
         MVC   5(9,R5),=C'REBATE CD'                            BUG08           
         B     HD37X                                                            
HD37H    MVC   10(2,R5),=C'CD'                                                  
HD37X    MVC   198+8(06,R5),=C'BILLED'                                          
         MVC   396+8(06,R5),DASHES                                              
         B     HD0XX                                                            
*                                                                               
HD38     DS    0H                  GROSS UNPAID                                 
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   9(5,R5),=C'GROSS'                                                
         B     HD43X                                                            
*                                                                               
HD39     DS    0H                  NET UNPAID                                   
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   10(3,R5),=C'NET'                                                 
         B     HD43X                                                            
*                                                                               
HD40     DS    0H                  GROSS-CD UNPAID                              
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   6(8,R5),=C'GROSS-CD'                                             
         B     HD43X                                                            
*                                                                               
HD41     DS    0H                  NET-CD UNPAID                                
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   8(6,R5),=C'NET-CD'                                               
         B     HD43X                                                            
*                                                                               
HD42     DS    0H                  AGYCOM UNPAID                                
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   7(7,R5),=C'AGY COM'                                              
         B     HD43X                                                            
*                                                                               
HD43     DS    0H                  CD UNPAID                                    
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   10(2,R5),=C'CD'                                                  
HD43X    MVC   198+8(06,R5),=C'UNPAID'                                          
         MVC   396+8(06,R5),DASHES                                              
         B     HD0XX                                                            
*                                                                               
*                                                                               
HD44     DS    0H                  GROSS BILLABLE                               
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD44H                                                            
         CLI   0(R4),180                                                        
         BNE   HD44D                                                            
         MVC   4(4,R5),=C'OPEN'                                                 
         B     HD44H                                                            
HD44D    CLI   0(R4),240                                                        
         BNE   HD44H                                                            
         MVC   2(6,R5),=C'REBATE'                               BUG08           
HD44H    MVC   9(5,R5),=C'GROSS'                                                
         B     HD49X                                                            
*                                                                               
HD45     DS    0H                  NET BILLABLE                                 
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD45H                                                            
         CLI   0(R4),181                                                        
         BNE   HD45D                                                            
         MVC   6(8,R5),=C'OPEN NET'                                             
         B     HD49X                                                            
HD45D    CLI   0(R4),241                                                        
         BNE   HD45H                                                            
         MVC  4(10,R5),=C'REBATE NET'                         BUG08             
         B     HD49X                                                            
HD45H    MVC   10(3,R5),=C'NET'                                                 
         B     HD49X                                                            
*                                                                               
HD46     DS    0H                  GROSS-CD BILLABLE                            
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD46H                                                            
         CLI   0(R4),182                                                        
         BNE   HD46D                                                            
         MVC   1(4,R5),=C'OPEN'                                                 
         B     HD46H                                                            
HD46D    CLI   0(R4),242                                                        
         BNE   HD46H                                                            
         MVC   1(4,R5),=C'REBT'                                 BUG08           
HD46H    MVC   6(8,R5),=C'GROSS-CD'                                             
         B     HD49X                                                            
*                                                                               
HD47     DS    0H                  NET-CD BILLABLE                              
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD47H                                                            
         CLI   0(R4),183                                                        
         BNE   HD47D                                                            
         MVC   3(4,R5),=C'OPEN'                                                 
         B     HD47H                                                            
HD47D    CLI   0(R4),243                                                        
         BNE   HD47H                                                            
         MVC   1(6,R5),=C'REBATE'                                 BUG08         
HD47H    MVC   8(6,R5),=C'NET-CD'                                               
         B     HD49X                                                            
*                                                                               
HD48     DS    0H                  AGYCOM BILLABLE                              
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD48H                                                            
         CLI   0(R4),184                                                        
         BNE   HD48D                                                            
         MVC   2(4,R5),=C'OPEN'                                                 
         B     HD48H                                                            
HD48D    CLI   0(R4),244                                                        
         BNE   HD48H                                                            
         MVC   0(6,R5),=C'REBATE'                                BUG08          
HD48H    MVC   7(7,R5),=C'AGY COM'                                              
         B     HD49X                                                            
*                                                                               
HD49     DS    0H                  CD BILLABLE                                  
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         CLI   ONLYOPN,C'Y'                                                     
         BE    HD49H                                                            
         CLI   0(R4),185                                                        
         BNE   HD49D                                                            
         MVC   7(7,R5),=C'OPEN CD'                                              
         B     HD49X                                                            
HD49D    CLI   0(R4),245                                                        
         BNE   HD49H                                                            
         MVC   5(9,R5),=C'REBATE CD'                            BUG08           
         B     HD49X                                                            
HD49H    MVC   10(2,R5),=C'CD'                                                  
HD49X    MVC   198+6(08,R5),=C'BILLABLE'                                        
         MVC   396+6(08,R5),DASHES                                              
         B     HD0XX                                                            
*                                                                               
HD50     DS    0H                  NUM OF INSERTIONS                            
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(5,R5),=C'TOTAL'                                                
         MVC   198(05,R5),=C'INSRT'                                             
         MVC   396(05,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                 L20           
* DIVISION NAME                                                   L20           
*                                                                 L20           
HD51     DS    0H                                                 L20           
         L     R5,ASVHD8                                          L20           
         AH    R5,DDISP                                           L20           
         MVC   0(13,R5),=C'DIVISION NAME'                         L20           
         MVC   198(13,R5),DASHES                                  L20           
         B     HD0XX                                              L20           
*                                                                 L20           
*        COST COLUMN                                              L07           
HD54     DS    0H                                                 L07           
         L     R5,ASVHD8  POINT TO CORRECT HEADLINE               L07           
         AH    R5,DDISP                                           L07           
         MVI   COSTOPT,255                                       L07            
*                                                                L07            
*                                                                L07            
         MVC   10(4,R5),=C'COST'                                  L07           
         B     HD0XX                                              L07           
*                                                                               
HD53     DS    0H           DIFFERENCE IN COLUMNS                 L04           
         MVI   DIFFACCU,255     DIFFERENCE INDICATOR              L04           
         L     R5,ASVHD7                                          L04           
         AH    R5,DDISP                                           L04           
         OC    DESC93,DESC93         ANY USER DEFINED DESCRIPTION L12           
         BZ    NO93DESC                                           L12           
         ZIC   RF,DESC93+1           ELEM LENGTH                  L12           
         SH    RF,=H'3'              LESS SEL CODE, LEN AND -1    L12           
         LA    R3,DESC93+2           BEGINING OF DESCRIPTION      L12           
LOOKDELI CLI   0(R3),C'/'            SPLIT ON TWO LINES           L12           
         BE    DELIMHIT                                           L12           
         LA    R3,1(R3)              NOSPLIT                      L12           
         BCT   RF,LOOKDELI                                        L12           
         MVC   0(14,R5),DESC93+2     MOVE WHOLE DES TO LINE       L12           
         B     HDAXX                 ALIGN RIGHT                  L12           
*                                                                 L12           
DELIMHIT BCTR   RF,0                 REDUCE REMAINDER FOR EX      L12           
         EX     RF,*+8                                            L12           
         B      *+10                                              L12           
         MVC    198(0,R5),1(R3)      MOVE 2ND PART                L12           
         ZIC    R3,DESC93+1          LENGTH OF ELEMENT            L12           
         SR     R3,RF                REDUCE BY REMAINDER-1        L12           
         SH     R3,=H'5'             MAKE UP FOR ELM,LEN & EX     L12           
         EX     R3,*+8                                            L12           
         B      *+10                                              L12           
         MVC    0(0,R5),DESC93+2                                  L12           
         B      HDAXX                                             L12           
*                                                                 L12           
NO93DESC DS     0H                                                L12           
         MVC   2(12,R5),=CL12'    SUM'                           L04            
         MVC   201(5,R5),=C'C1+C2'  ASSUME TWO COL ADD            L04           
         TM    1(R4),X'80'     IF PRESENT THREE COL CALC          L04           
         BNO   NO3COL                                            L04            
         MVC   2(12,R5),=CL12'  TOTAL'                           L04            
         MVC   201(8,R5),=C'C1+C2+C3'                             L04           
NO3COL   DS    0H                                                L04            
         TM    1(R4),X'40'       DIFF BTWN 1ST TWO OPERANDS       L04           
         BNO   DIFF23                                            L04            
         MVI   203(R5),C'-'                                       L04           
         TM    1(R4),X'80'      THREE COL                        L04            
         BO    DIFF23                                            L04            
         MVC   2(12,R5),=C' DIFFERENCE '                         L04            
DIFF23   DS    0H                                                L04            
*                                                                L10            
         TM    1(R4),X'20'       DIFF BTWN 2ND + 3RD              L04           
         BNO   *+8                                                L04           
         MVI   206(R5),C'-'                                       L04           
         OC    MULTIPLY,MULTIPLY  IS  THERE A MULTIPLIER IN       L11           
         BZ    HDAXX              FORMULA                         L11           
         CLI   ARITHOPT,C'A'      SUMS AS WELL                    L11           
         BE    ASWELL                                             L11           
         MVC   0(14,R5),=C'PREVIOUS COL *'                                      
         B     OVERCLRX                                                         
ASWELL   MVC    2(12,R5),201(R5)   SHIFT OPERATORS UP A LINE     L11            
         MVI   11(R5),C'*'         TIMES SYMBOL                   L11           
OVERCLRX DS    0H                                                               
         XC   201(8,R5),201(R5)                                   L11           
         MVC  WORK(11),=X'40202021204B2020202020'                 L11           
         XC   WORK+11(6),WORK+11                                  L11           
         ED   WORK(11),MULTIPLY                                   L11           
         MVC    198(10,R5),WORK+1                                L11            
*                                                                 L11           
*                                                                 L11           
HDAXX    DS   0H                                                  L11           
*                                                                 L11           
         BAS   RF,RIGHTLNE      RIGHT JUSTIFY  R5 POINTS TO LINE  L11           
         LA    R5,198(R5)                                         L11           
         BAS   RF,RIGHTLNE                                        L11           
         B     HD0XX                                              L04           
****                                                              L11           
         DS    F             ALIGN COMMENTS TO THE RIGHT          L11           
RIGHTLNE ST    RF,RIGHTLNE-4                                      L11           
         CLC   0(14,R5),=14C' '                                   L11           
         BNH   NOSHIFT                                            L11           
         XC    WORK(30),WORK                                      L11           
         MVC   WORK+14(14),0(R5)                                  L11           
         LA    RF,WORK+29        END OF CLEARED AREA              L11           
CLI441   CLI   0(RF),X'41'                                        L11           
         BH    HEREMOVE          NON-BLANK                        L11           
         BCTR  RF,0                                               L11           
         B     CLI441                                             L11           
*                                                                 L11           
HEREMOVE SH    RF,=H'13'                                          L11           
         MVC   0(14,R5),0(RF)                                     L11           
NOSHIFT  L     RF,RIGHTLNE-4                                      L11           
         BR    RF                                                 L11           
*                                                                 L11           
*                                                                               
HD90     DS    0H                  PLANNED COST                                 
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   2(12,R5),=C'PLANNED COST'                                        
         B     HD0XX                                                            
*                                                                               
HD91     DS    0H                  ACTUAL LESS PLANNED COST                     
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   2(11,R5),=C'ACTUAL LESS'                                         
         MVC   198(12,R5),=C'PLANNED COST'                                      
         B     HD0XX                                                            
*                                                                               
HD92     DS    0H                  ESTIMATE DESCRIPTION                         
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(20,R5),=C'ESTIMATE DESCRIPTION'                                
         MVC   198(20,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
*                                                                               
HD93     DS    0H                      COLUMN HEADER FOR ASPO NUM.              
         L     R5,ASVHD7                                           L02          
         AH    R5,DDISP                                                         
* *      MVC   0(11,R5),=C'ASPO NUMBER'                                         
* *      MVC   198(11,R5),DASHES                                                
         MVC   2(4,R5),=C'ASPO'                                    L02          
         MVC   198(6,R5),=C'NUMBER'                                L02          
         MVC   396(10,R5),DASHES                                   L02          
         B     HD0XX                                                            
*                                                                               
HD94     DS    0H                                                               
         L     R5,ASVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(4,R5),=C'LIST'                                                 
         MVC   198(4,R5),=C'CODE'                                               
         MVC   396(4,R5),DASHES                                                 
         B     HD0XX                                                            
*                                                                               
HD95     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(8,R5),=C'LAST I/O'                                             
         MVC   198(8,R5),DASHES                                                 
         B     HD0XX                                                            
*                                                                               
HD96     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   11(3,R5),=C'TAX'                                                 
         MVC   198+11(3,R5),DASHES                                              
         B     HD0XX                                                            
*                                                                               
HD97     DS    0H                                                               
         L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(7,R5),=C'CAPTION'                                              
         MVC   198(7,R5),DASHES                                                 
         B     HD0XX                                                            
*                                                                               
HD98     L     R5,ASVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(11,R5),=C'LAST AD USE'                                         
         MVC   198(11,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD99     B     HD0XX               BLANK COLUMN                                 
*                                                                               
*                                                                               
HD0XX    LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         BR    RE                                                               
AHEADTAB DC    A(HEADTAB)                                                       
         EJECT                                                                  
         LTORG                                                                  
         TITLE  'TLIN--- PPL102 - PRINT HEADLINES'                              
*                                  INPUT                                        
         PRINT NOGEN                                                            
TLIN     NMOD1 0,TLIN                                                           
*                                                                               
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP81WRKD,R8                                                      
         LA    R6,1(RB)                                                         
         LA    R6,4095(R6)                                                      
         USING TLIN,RB,R6                                                       
*                                                                               
TLIN001  OC    KEY,KEY                                                          
         BNZ   TLIN10                                                           
*                                       FIRST TIME                              
         XC    BSTART,BSTART                                                    
         L     R2,ALTLREC                                                       
         XC    0(25,R2),0(R2)      CLEAR LTLREC                                 
         MVI   DMINBTS,0                                                        
         SPACE                                                                  
         LA    R1,RPTTAB          IF REQUESTING PAID/BILLED $                   
TLIN01   CLI   0(R1),X'FF'        PASS DELETED RECS TO GET                      
         BE    TLIN06             BILLED $ OF DELETED RECS                      
         CLI   0(R1),X'32'                                                      
         BL    TLIN05                                                           
         CLI   0(R1),X'55'                                                      
         BH    TLIN03                                          BUG19            
TLIN02   OI    DMINBTS,X'08'       PASS DELETED RECS                            
         B     TLIN07                                                           
TLIN03   CLI   0(R1),150      OPEN PAID/BILLED/PAYABLE/BILLABLE BUG19           
         BL    TLIN05                                           BUG19           
         CLI   0(R1),185                                        BUG19           
         BH    TLIN04                                           BUG19           
         B     TLIN02          PASS DELETES                     BUG19           
*                                                               BUG19           
TLIN04   CLI   0(R1),210      DIFF PAID/BILLED/PAYABLE/BILLABLE BUG19           
         BL    TLIN05                                           BUG19           
         CLI   0(R1),245                                        BUG19           
         BH    TLIN05                                           BUG19           
         B     TLIN02            PASS DELETES                   BUG19           
*                                                               BUG19           
TLIN05   LA    R1,5(R1)                                                         
         B     TLIN01                                                           
         SPACE                                                                  
*                                                               BUG18           
TLIN06   CLI   QOPT3,C'Y'          BILLED ITEMS - PASS DELETES  BUG18           
         BE    TLIN06C                                          BUG18           
         CLI   QOPT4,C'Y'          BILLABLE ITEMS - PASS DELETES BUG18          
         BE    TLIN06C                                           BUG18          
         CLI   QOPT5,C'Y'          PAYABLE ITEMS - PASS DELETES  BUG18          
         BNE   TLIN07                                            BUG18          
*                                                                BUG18          
TLIN06C  OI    DMINBTS,X'08'       PASS DELETES                  BUG18          
*                                                                BUG18          
TLIN07   MVC   BEND,=3X'FF'                                                     
         CLC   QSTART(2),=C'ES'                                                 
         BNE   TLIN1                                                            
         MVC   QSTART(2),SPACES    RESET TO SPACES                              
         CLC   QEST,=C'ALL'                                                     
         BE    TLIN1                                                            
         CLC   QEST,SPACES                                                      
         BE    TLIN1                                                            
*                                                                               
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(3),QCLIENT                                                 
         MVC   KEY+7(3),QPRODUCT                                                
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   KEY+10(2),HALF                                                   
* EST-IO   GET DIRECTORY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   TLIN1               EST NOT READABLE OR NOT FOUND                
*                                  LEAVE QSTART BLANK                           
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
*EST-IO   GET RECORD                                                            
         GOTO1 GETPRT                                                           
         MVC   QSTART(12),PESTST   GET DATES FROM ESTIMATE                      
TLIN1    CLI   QSTART,C' '                                                      
         BE    TLIN2                                                            
*        GOTO1 DTCNV,DMCB,QSTART,(1,BSTART)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*                                                                               
TLIN2    DS    0H                                                               
         CLI   QEND,C' '                                                        
         BE    TLIN4                                                            
*        GOTO1 DTCNV,DMCB,QEND,(1,BEND)                                         
         GOTO1 DATCON,DMCB,(0,QEND),(3,BEND)                                    
*                                                                               
TLIN4    DS    0H                                                               
*                                                                               
         B     TLIN4A                                                           
*                                                                               
**DP     CLC   QAGENCY,=C'DP'          FOR DUPONT READ ACROSS ITS               
**DP     BNE   TLIN4A                  AGENCIES.                                
**DP     BAS   RE,DUPONT                                                        
**DP     CLC   SAVEKEY2(4),KEYSAVE                                              
**DP     BNE   TLIN90A                                                          
**DP     B     TLIN5                                                            
*                                                                               
TLIN4A   XC    KEY,KEY             RECLEAR KEY - MAY HAVE EST                   
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'20'                                                      
         OC    KEY+3(1),RUNSW                                                   
         CLC   QCLIENT,SPACES                                                   
         BE    TLIN6                                                            
         CLC   QCLIENT,=C'ALL'                                                  
         BE    TLIN6                                                            
         CLI   QCLIENT,C'*'                                                     
         BE    TLIN6                                                            
         CLI   QCLIENT,C'&&'                                       L25          
         BE    TLIN6                                               L25          
         CLI   MASTCLI,0     IF MASTER CLIENT DO NOT MOVE RQSTD CLI L21         
         BE    TLIN4AA       TO KEY// CLIENT WILL BE PROVIDED BY    L21         
*                            SUB ROUTINE TLIN10X AFTER FIRST TIME               
         MVC   SAVEKEY,KEY                                        L21           
         MVI   KEY+3,2                                            L21           
         MVC   KEY+4(3),=C'AAB' FIND FIRST SLAVE                  L21           
         XC    KEY+7(10),KEY+10                                   L21           
GOTHI    GOTO1 HIGH                                               L21           
GOTHISR  CLC   KEY(3),KEYSAVE                                     L21           
         BNE   TLIN90A           CHANGE OF AGENCY/MEDIA --SORT    L21           
         CLI   KEY+3,2           IS THIS A CLIENT HEADER          L21           
         BL    MVI02                                              L21           
         BE    MASTRDCL          READ CLIENT HEADER               L21           
*     KEY IS HIGHER   -- MISSING CLIENT HEADER -- GET NEXT CLI    L21           
TONXTCLT MVI   KEY+7,255         FORCE TO NEXT CLI                L21           
         B     GOTHI                                              L21           
*     REC CODE IS LOW -- SLAP IN CLI REC CODE                     L21           
MVI02    MVI   KEY+3,2                                            L21           
         B     GOTHI                                              L21           
*                                                                 L21           
*  READ TO SEE IF SLAVE FOR MASTER CLIENT                         L21           
*                                                                 L21           
MASTRDCL GOTO1 GETCLI                                             L21           
*                                                                 L21           
         CLI   PCLTPROF+5,C'2'    SLAVE                           L21           
         BNE   TONXTCLT                                           L21           
         CLC   PCLTPROF+6(3),MASTCLI      SLAVE MUST EQ MASTER    L21           
         BNE   TONXTCLT                                           L21           
         MVC   SLAVELST,KEY+4             SLAVE BEING PROCESSED   L21           
         MVC   SAVEKEY+4(3),SLAVELST                              L21           
         MVC   KEY,SAVEKEY                                        L21           
         B     TLIN5                                              L21           
*                                                                 L21           
*                                                                               
*                                                                               
*                                                                               
TLIN4AA  MVC   KEY+4(3),QCLIENT                                                 
*                                                                               
TLIN5    DS    0H                                                               
         CLC   QPRODUCT,SPACES                                                  
         BE    TLIN6                                                            
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    TLIN6                                                            
         LA    R1,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+13                                                        
         MVC   0(3,R1),QPRODUCT                                                 
*                                                                               
TLIN6    DS    0H                                                               
         XC    BPUB,BPUB                                                        
         CLI   QPUB,C'0'                                                        
         BL    TLIN8                                                            
         MVC   WORK(11),QPUB                                                    
         CLC   QZONE(3),=3C'Z'                                                  
         BNE   *+10                                                             
         MVC   WORK+8(3),SPACES                                                 
         GOTO1 PUBVAL,DMCB,(0,WORK),BPUB                                        
*                                                                               
         LA    R1,KEY+10                                                        
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+7                                                         
         MVC   0(6,R1),BPUB                                                     
*                                                                               
TLIN8    DS    0H                                                               
* BUY-IO  GET DIRECTORY                                                         
         GOTO1 HIGH                                                             
         B     TLIN10C                                                          
*                                                                               
TLIN10   DS    0H                                                               
         BAS   RE,MULTREC          CHK IF MULT RECS TO SORTER                   
         LTR   R1,R1          (MUST I PASS SAME SORTREC WITH DIFF               
         BZ    TLIN80         -REG/DIST/SHR/SUM TO SORTER )                     
         SPACE                                                                  
* BUY-IO GET NEXT DIRECTORY                                                     
TLIN10A  GOTO1 SEQ                                                              
TLIN10C  DS    0H                                                               
         BAS   RE,PASSPTR                                         L15           
         B     TLIN10A                                            L15           
         CLI   KEY+25,X'FF'                                                     
         BE    TLIN10                                                           
*                                                                               
*****                                                                           
         CLC   KEY(4),KEYSAVE    SEE IF RIGHT AGY/MED/RECORD CODE               
         BNE   TLIN10D           DON'T TRY TO READ                              
*                                                                               
         MVC   SAVKEYS(64),KEY                                                  
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
* BUY-IO  GET RECORD                                                            
         GOTO1 GETPRT                                                           
         MVC   KEY(64),SAVKEYS                                                  
         CLI   PBDBFD,C'T'        IS IT A TEST BUY?                             
         BNE   TLIN10D                                                          
         CLI   QOPT7,C'Y'         IF IT IS A TEST BUY ARE THEY TO BE            
         BNE   TLIN10A            INCLUDED?                                     
*                                                                               
TLIN10D  CLI   RCMULTIQ,C'C'                                                    
         BNE   TLIN10F                                                          
         CLC   KEY(2),KEYSAVE                                                   
         BNE   TLIN90A                                                          
*                                           ***                                 
TLIN10F  CLC   KEY(4),KEYSAVE      AGENCY/MEDIA/REC-CODE                        
         BNE   TLIN90                                                           
TLIN10J  CLC   QCLIENT,SPACES                                                   
         BE    CLTST                                                            
         CLC   QCLIENT,=C'ALL'                                                  
         BNE   OFFICE                                                           
*                                                                               
CLTST    CLC   KEY+4(3),KEYSAVE+4        TEST CLIENT CHANGED                    
         BE    TLIN12                                                           
         XC    SRTLIN,SRTLIN  CLEAR SORT LINE NUMBER                            
         BAS   RE,IGETCLT                                                       
         B     TLIN12                                                           
*                                                                               
OFFICE   CLI   QCLIENT,C'*'              TEST OFFICE REQ           L25          
         BE    OFFICE5                                             L25          
         CLI   QCLIENT,C'&&'             OR GROUP                  L25          
         BNE   TLIN10MX                                            L21          
OFFICE5  CLC   KEY(3),PCLTKEY            YES/OFFICE                             
         BNE   *+14                                                             
         CLC   KEY+4(3),PCLTKCLT                                                
         BE    TLIN12                                                           
         MVC   PBUYKEY,KEY                                                      
         BAS   RE,IGETCLT           GET CLIENT                                  
         CLI   QCLIENT,C'*'             SEE IF OFFICE              L25          
         BNE   OFFICE7                                                          
         CLC   QCLIENT+1(1),PCLTOFF     IF OFFICE NOT EQ                        
***                                                                             
*  BUG 04  OCCURS WHEN OFFICE BY SPECIFIC PUB IS REQUESTED                      
*>     FILE IS READ HIGH W NO CLIENT -- PUB IS SECONDARY --                     
*      INCORRECT PUB IS READ-- TLIN13AA CHECKS TO SEE IF REPORT IS              
*      BY PUB-- IF SO AND PUB IS NOT EQUAL BYPASS ALL RECORDS FOR               
*      THIS CLIENT..                                                            
*****                                                            BUG04          
         BE    CK4PUBRQ                                          BUG04          
         B     OFFICE9                                                          
*                                                                               
*         MUST BE GROUP REQ                                      L25            
OFFICE7  CLC   QCLIENT+1(1),PCLTBLGP     GROUP                   L25            
         BE    CK4PUBRQ                                          L25            
*                                                                               
OFFICE9  IC    RF,KEY+6            INCREMENT CLIENT ONE BIT                     
         LA    RF,1(RF)            AND                                          
         STC   RF,KEY+6            RESET KEY FOR READ HIGH FOR NXT CLI          
         XC    KEY+7(18),KEY+7     CLEAR PRODUCT ETC IN KEY                     
         B     TLIN5               EVENTUALLY READ HIGH                         
*                                                                               
CK4PUBRQ CLI   QPUB,C'0'     SPECIFIC PUB REQUESTED              BUG04          
         BL    TLIN12                                            BUG04          
         MVC   KEY,PBUYKEY      RESTORE KEY                      BUG04          
         MVC   KEY+7(6),BPUB                                     BUG04          
         XC    KEY+13(18),KEY+13                                 BUG04          
         B     TLIN5                                             BUG04          
*****                                                            BUG04          
*****                                                            BUG04          
TLIN10MX CLI   MASTCLI,0        WAS MASTER AGENCY RQSTD           L21           
         BE    TLIN10M                                            L21           
         CLC   SLAVELST,KEY+4   SAME SLAVE                        L21           
         BE    TLIN10MZ                                           L21           
*                                                                 L21           
* PREPARE KEY TO READ NEXT CLIENT HEADER                          L21           
*                                                                 L21           
         XC    SAVEKEY,SAVEKEY                                    L21           
         MVC   SAVEKEY(7),KEY                                     L21           
         MVI   SAVEKEY+3,X'20'                                    L21           
         MVC   KEY,SAVEKEY                                        L21           
         MVI   KEY+3,X'02'                                        L21           
         B     GOTHI                                              L21           
*                                                                 L21           
*                                                                               
TLIN10M  DS    0H                                                               
         CLC   QCLIENT,KEY+4                                                    
         BNE   TLIN90                                                           
*                                                                               
TLIN10MZ CLC   QPRODUCT,SPACES                                                  
         BE    TLIN12                                                           
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    TLIN12                                                           
         LA    R1,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+13                                                        
         CLC   QPRODUCT,0(R1)                                                   
         BE    TLIN12           WAS BNE TLIN90                    BUG26         
*                                                                 BUG26         
         CLI   KEY+3,X'20'      SEE IF USING REGULAR KEY          BUG26         
         BE    TLIN90                                             BUG26         
         OC    BPUB,BPUB        SEE IF ONE PUB REQUESTED          BUG26         
         BNZ   TLIN90              GET OUT                        BUG26         
*        MVI   KEY+13,255       FORCE TO NEXT PUB                 BUG26         
         B     TLIN10                                             BUG26         
*                                                                 BUG26         
*                                                                 BUG26         
*                                                                               
TLIN12   DS    0H                                                               
         BAS   RE,PASSPTR                                         L15           
         B     TLIN10                                             L15           
         CLC   QDIV,SPACES                                                      
         BE    TLIN13                                                           
         CLC   QDIV,=C'ALL'                                                     
         BE    TLIN13                                                           
         MVC   PBUYKEY,KEY                                                      
         BAS   RE,IGETPRD          MUST READ PRDOUCT TO CHK DIV                 
         CLC   QDIV,PPRDDIV                                                     
         BNE   TLIN10              NO MATCH - SKIP                              
*                                                                               
TLIN13   LA    R1,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+13                                                        
*                                                                               
         CLI   0(R1),C'*'           SKIP OTHER AGY DATA                         
         BE    TLIN10                                                           
*                                                                               
         OC    BPUB,BPUB                                                        
         BZ    TLIN14                                                           
         LA    R1,KEY+10                                                        
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+7                                                         
         CLC   QZONE(3),=3C'Z'                                                  
         BNE   TLIN13A                                                          
         CLC   BPUB(4),0(R1)                                                    
         B     TLIN13AA                                                         
TLIN13A  CLC   BPUB,0(R1)                                                       
***** IF REQUEST IS FOR A SPECIFIC PUB NUMBER / FORCE READ TO BYPASS            
*     ALL PUBS AND FORCE BREAK IN KEY AT A HIGER LEVEL          L06             
TLIN13AA BE   TLIN14                                            L06             
         CLI  KEY+3,X'21' KEYS IN PUB ORDER (ONE PUB RQESTED)   L06             
         BNE  TLIN10      READ NEXT BUY SEQUENTIALLY            L06             
         MVI   KEY+7,X'FF' FORCE READ TO BYPASS ALL PUBS FOR CLI L06            
         B    TLIN8       READ HIGH                             L06             
*                                                                               
TLIN14   DS    0H                                                               
         CLC   QREGION,SPACES     FILTER ON REGION/DISTRICT                     
         BE    TLIN14D                                                          
         CLC   QREGION,=C'ALL'                                                  
         BE    TLIN14D                                                          
         MVC   PBUYKEY,KEY       FRSTTIME PBUYKEY MAYBE EMPTY                   
         BAS   RE,IGETLTL          GET LTLREC                                   
         CLI   DRDSW,255  IF DUMMY REGION RQST NO NEED TO READ   L09            
         BE    *+8        PRODUCT FOR DIVISION                   L09            
*                                                                L09            
         BAS   RE,IGETPRD          GET PRDREC TO GET DIVISON                    
         BAS   RE,CHKRGDST         CHECK REG/DIST AGAINST LTLREC                
         LTR   R1,R1              R1=ZERO MEANS MATCH                           
         BNZ   TLIN10                                                           
         SPACE                                                                  
TLIN14D  CLC   QEST,=C'ALL'                                                     
         BE    TLIN15                                                           
         CLC   QEST,SPACES                                                      
         BE    TLIN15                                                           
         MVC   HALF,KEY+19                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         CLC   WORK(3),QEST                                                     
         BE    TLIN15                                                           
         BL    TLIN10                                                           
         CLC   QESTEND,SPACES                                                   
         BE    TLIN10                                                           
         CLC   WORK(3),QESTEND                                                  
         BH    TLIN10                                                           
*                                                                               
TLIN15   DS    0H                                                               
         CLI   QBPDATE,C' '                                                     
         BNE   TLIN16                                                           
         CLC   BSTART,KEY+16                                                    
         BH    TLIN10                                                           
         CLC   BEND,KEY+16                                                      
         BL    TLIN10                                                           
TLIN16   DS    0H                                                               
         MVI   CHGSW,0                                                          
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
*   BUY-IO  GET RECORD                                                          
         GOTO1 GETPRT                                                           
         CLC   QJOB,SPACES                                                      
         BE    TLIN18                                                           
         CLC   QJOB,=C'ALL   '                                                  
         BE    TLIN18                                                           
         CLC   QJOB,PBDJOB                                                      
         BE    TLIN18                                                           
         CLC   =C'NONE',QJOB                                                    
         BNE   TLIN10                                                           
         OC    PBDJOB,PBDJOB                                                    
         BNZ   TLIN10                                                           
*                                                                               
TLIN18   DS    0H                                                               
         CLI   QOPT4,C'Y'                  BILLABLE ITEMS ONLY                  
         BNE   TLIN18B                                                          
         MVC   GROSS,=C'CRAT'  FOR SPECIAL HANDLING OF 'C' RATES  L23           
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKEY+7                              
         TM    PBUYCNTL,X'80'                                                   
         BNO   TLIN18A                                                          
         OC    BGROSS(12),BGROSS                               BUG12            
         BNZ   TLIN18C                                                          
         B     TLIN10                                                           
TLIN18A  CLC   GROSS(12),BGROSS                                                 
         BNE   TLIN18C                                                          
         MVC   GROSS,=C'CRAT'  FOR SPECIAL HANDLING OF 'C' RATES  L23           
         GOTO1 GETINS,DMCB,PBUYREC,(C'O',GROSS),PBUYKEY+7                       
         CLC   GROSS(12),BGROSS   WILL BE OPEN AMTS IN CONTRACT BUG12           
         BE    TLIN10                BLOCK                                      
*                                                                               
TLIN18B  DS    0H                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   TLIN18C             PAYABLE ITEMS ONLY                           
         MVC   GROSS,=C'CRAT'  FOR SPECIAL HANDLING OF 'C' RATES  L23           
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKEY+7                              
         TM    PBUYCNTL,X'80'                                                   
         BNO   TLIN18B2                                                         
         OC    PGROSS(12),PGROSS         SEE IF PAID     BUG17                  
         BZ    TLIN10                                                           
         B     TLIN18C                                                          
TLIN18B2 CLC   GROSS(12),PGROSS          SEE IF FULLY PAID                      
         BE    TLIN10                                                           
*                                                                               
TLIN18C  CLI   QBPDATE,C' '                                                     
         BE    TLIN19                                                           
         LA    R2,PBDCDATE                                                      
         CLI   QBPDATE,C'C'                                                     
         BE    TLIN18D                                                          
***MAT***                                                                       
         LA    R2,PBDMDATE            MATERIALS CLOSING DATE                    
         CLI   QBPDATE,C'M'                                                     
         BE    TLIN18D                                                          
***MAT***                                                                       
         LA    R2,PBDBDATE                                                      
         CLI   QBPDATE,C'B'      BILLABLE OPTION FILTER                         
         BE    TLIN18D                                                          
         LA    R2,PBDPDATE                                                      
         CLI   QBPDATE,C'P'     PAYABLE DATE OPTION FILER                       
         BE    TLIN18D                                                          
         LA    R2,PBDSDATE                                                      
         CLI   QBPDATE,C'S'     ON STAND DATE  FILTER                           
         BE    TLIN18D                                                          
*   CHECK FOR PAID OR BILLED DATE FILTER // ALSO CHECK FOR OPEN     L03         
*      BILLED ITEMS                                                 L03         
*  MUST DETERIMINE IF REQUEST WAS TO FILTER BY PAID/BILLED DATES  L03           
*                                                                 L03           
*                                                                   L03         
         MVI   OPTINS,0   INITIALIZE OPTION BYTE FOR GETINS   )  L03            
         CLI   QBPDATE,C'A'  FILTER ON PAID DATES                 L03           
         BNE   CH4BILL                                            L03           
         MVI   OPTINS,C'P'  PAID INDICATOR FOR    GETINS         L03            
         PRINT GEN                                                              
         MVC   GROSS,=C'CRAT'  FOR SPECIAL HANDLING OF 'C' RATES  L23           
         GOTO1 GETINS,DMCB,(OPTINS,PBUYREC),GROSS,PBUYKEY+7,           X        
               BSTART                                                           
         OC    PGROSS(12),PGROSS  PAID GROSS IN THIS TIME FRAMEBUG12            
         BZ    TLIN10     GET NEXT REC                            L03           
         B     TLIN19                                             L03           
*                                                                 L03           
OPTINS   DS    CL1                                                              
         PRINT NOGEN                                                            
CH4BILL  CLI   QBPDATE,C'I'  FILTER ON BILLING DATES              L03           
         BNE   TLIN18D                                            L03           
         MVI   OPTINS,C'B'  BILLING INDICATOR FOR    GETINS      L03            
         MVC   GROSS,=C'CRAT'  FOR SPECIAL HANDLING OF 'C' RATES  L23           
         GOTO1 GETINS,DMCB,(OPTINS,PBUYREC),GROSS,                     C        
               PBUYKEY+7,BSTART                                                 
*                                                                               
         OC    BGROSS(12),BGROSS   ANY GROSS DOLLARS THERE          L03         
         BZ    CK4OPEN                                              L03         
         MVI   QOPT3,0        FORCE NO LOOKUP AT TLIN19A            L03         
         B     TLIN19                                               L03         
*                                                                   L03         
CK4OPEN  CLI   ONLYOPN,C'N'                                         L03         
         BNE   TLIN10         NEXT RECORD                           L03         
*                                                                   L03         
         MVC   GROSS,=C'CRAT'  FOR SPECIAL HANDLING OF 'C' RATES  L23           
         GOTO1 GETINS,DMCB,(OPTINS,PBUYREC),(C'O',GROSS),              C        
               PBUYKEY+7,BSTART                                                 
         OC    BGROSS(12),BGROSS   ANY $$                  L03 BUG122           
         BZ     TLIN10                                              L03         
         B      TLIN19                                              L03         
*                                                                               
*                                                                               
TLIN18D  DS    0H                                                               
         CLI   0(R2),0                                                          
         BNE   *+8                                                              
TLIN18G  DS    0H                                                               
         LA    R2,PBUYKDAT         OR USE DATE                                  
         CLC   BSTART,0(R2)                                                     
         BH    TLIN10                                                           
         CLC   BEND,0(R2)                                                       
         BL    TLIN10                                                           
*                                                                               
*                                                                               
TLIN19   DS    0H                                                               
         CLC   QEST,=C'000'       IF LOW MUST BE ALL OR BLANKS                  
         BL    TLIN19A5                                                         
         CLC   QESTEND,=C'   '    IF ONE EST SPEC. DO NO GOTO ESTFLTR           
         BE    TLIN19A                                                          
*                                 NEW CODE FOR TEST ESTS ABOVE                  
TLIN19A5 BAS   RE,ESTFLTR                                                       
         BZ    TLIN10              COND CODE ZERO,REJECT ESTIMATE               
         SPACE                                                                  
TLIN19A  CLI   QOPT3,C'Y'          TEST IF BILLED ITEMS ONLY                    
         BNE   TLIN20                                                           
         LA    R2,PBUYREC+33                                                    
         USING PBILELEM,R2                                                      
         MVI   ELCODE,X'26'        SET BILL ELEM CODE                           
         CLI   0(R2),X'26'                                                      
         BE    TLIN19C                                                          
TLIN19B  BAS   RE,NEXTEL4                                                       
         BNE   TLIN10              NOT BILLED/SKIP                              
TLIN19C  CLC   PBLDATE,=3X'00'                                                  
         BE    TLIN19B             NOT BILLED/SEE IF MORE BILL ELEMS            
         DROP  R2                                                               
*                                                                               
TLIN20   DS    0H                                                               
         XC    SORTREC,SORTREC                                                  
         XC    SDISP,SDISP                                                      
*                               CHK RPTTAB TO SEE IF SORTING ON FIELD           
         LA    R4,RPTTAB                                                        
TLIN25   CLI   0(R4),X'FF'       END OF TABLE                                   
         BE    TLIN60                                                           
         CLI   4(R4),X'FF'                                                      
         BE    TLIN40              NOT SORTING ON THIS FLD SKIP                 
         CLI   2(R4),X'F0'     INVISIBLE FIELD                    L13           
         BE    TLIN40                                             L13           
*                                                                 L13           
         LA    R5,SORTTAB                                                       
TLIN30   CLI   0(R5),X'FF'        END OF SORTTAB                                
         BNE   *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         CLC   0(1,R4),0(R5)                                                    
         BE    TLIN35                                                           
         LA    R5,5(R5)                                                         
         B     TLIN30              NEXT ENTRY                                   
*                                                                               
TLIN35   MVC   FULL,1(R5)                                                       
         L     RF,FULL                                                          
         BAS   RE,0(RF)                                                         
*                                                                               
TLIN40   LA    R4,5(R4)            NEXT ENTRY IN RPTTAB                         
         B     TLIN25                                                           
*                                                                               
         EJECT                                                                  
**DP                                                                            
**DP     CODE BELOW WAS USED FOR DUPONT PROCESSING                              
**DP                                                                            
*                                                                               
* IF THE AGENCY IS DUPONT.  CLIENT HEADERS ARE REALLY THE AGENCIES              
*    THAT DO THEIR ADVERTISING. THERE SHOULD BE A CLIENT FOR DUPONT             
*    SINCE THEY DO SOME OF THEIR OWN BUYING...                                  
*                                                                               
*                                                                               
         DS    F                                                                
DUPONT   DS    0H                                                               
         ST    RE,DUPONT-4                                                      
         MVC   KEY,SAVEKEY                                                      
         OC    KEY,KEY   IS THIS THE FIRST TIME IN                              
         BNZ   DP3       GET NEXT CLIENT HEADER                                 
         MVC   KEY(3),RCSVAGY  CONSTRUCT KEY TO READ CLIENT HDRS                
         MVI   KEY+3,X'02'     NOTE- CLIENT HEADERS FOR DUPONT HAVE             
         CLC   SVQCLI,=C'ALL'  AGENCIES FOR CLIENT CODES--                      
         BE    DP2                                                              
         MVC   KEY+4(3),SVQCLI READ FOR SPECIFIC CLIENT PER REQUST              
         B     DP2                                                              
*                                                                               
DP1      MVC   SAVEKEY2,KEY                                                     
         CLC   KEYSAVE(4),KEY                                                   
         BNE   DPEND               LAST CLIENT (AGY)                            
         CLC   KEYSAVE(7),KEY  BREAK ON AGENCY/MEDIA/RECORD TYPE                
         BE    DP6                                                              
         CLC   SVQCLI,=C'ALL'                                                   
         BE    DP6                                                              
         B     DP4               READ NEXT CLIENT                               
* CLIENT-IO  DIRECTORY                                                          
DP2      GOTO1 HIGH                                                             
         B     DP1                                                              
*                                                                               
DP3      GOTO1 HIGH                                                             
* CLIENT-IO  GET NEXT                                                           
DP4      GOTO1 SEQ                                                              
         B     DP1                                                              
*              PROCESS THIS CLIENT (AGY)                                        
*                                                                               
DP6      TM    KEY+25,X'80'          NO DELETED CLTHDRS                         
         BNZ   DP4                                                              
* CLIENT-IO READ CLIENT HEADER                                                  
         GOTO1 GETCLI                                                           
         CLC   PCLTAGYR(3),=3X'00'                                              
         BE    DP4                 BYPASS THIS CLIENT                           
         MVC   SAVEKEY,KEY         SAVE THIS KEY                                
         XC    KEY,KEY             READ BUYS                                    
         MVC   KEY(2),PCLTKCLT                                                  
         MVC   KEY+2(1),PCLTKMED                                                
         MVI   KEY+3,X'20'                                                      
         CLI   QPUB,C'0'           SEE IF DOING ONE PUB                         
         BL    *+8                                                              
         MVI   KEY+3,X'21'         ONE PUB SO USE 21 POINTER                    
         MVC   KEY+4(2),QAGENCY                                                 
         MVI   KEY+6,C' '                                                       
         CLI   QPUB,C'0'           SEE IF DOING ONE PUB                         
         BL    DPEND               NO - GO SEE IF ONE PRD                       
*                                  ONE PUB                                      
         MVC   KEY+7(6),LNKPUBS+2         BD PUB                                
*                                                                               
         LA    R5,LNKPUBS                                                       
DP6C     CLC   0(2,R5),PCLTKCLT        REALLY DP AGY CODE                       
         BE    DP6E                                                             
         CLC   0(5,R5),=5X'FF'                                                  
         BE    DPEND               GO DO NEXT CLT (AGY)                         
         LA    R5,9(R5)                                                         
         B     DP6C                                                             
*                                                                               
DP6E     MVC   KEY+7(6),2(R5)                                                   
*   FORCE CHANGE IN REQUEST TO NEW PUB NUMBER OF NEW AGENCY     L06             
*     IF ONLY ONE PUB WAS REQUESTED                                             
         UNPK  DMCB(11),2(6,R5)  UNPACK PUB NUMBER              L06             
         MVC   QPUB(10),DMCB     MOVE TO REQUEST                L06             
DPEND    L     RE,DUPONT-4                                                      
         BR    RE                                                               
*                                                                               
*                                                                               
         TITLE 'ST0A    SET UP SORT RECORD'                                     
ST0A     DS    0H                                                               
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
*                                                                               
         B     ST0A1                                                            
*                                                                               
**DP     CLC   QAGENCY,=C'DP'                                                   
**DP     BNE   ST0A1                                                            
**DP     MVC   0(2,R1),PBUYKAGY                                                 
**DP     B     *+10                                                             
*                                                                               
ST0A1    MVC   0(3,R1),PBUYKCLT                                                 
         BR    RE                                                               
         SPACE                                                                  
         DS    F                                                                
ST0B     DS    0H                 REGION                                        
         ST    RE,ST0B-4                                                        
         MVC   ADSPRD,SDISP    SAVE DISP OF REG FOR MULT SORT RECS              
         BAS   RE,DISPRTN                                                       
         BAS   RE,IGETLTL      GET LTLREC                                       
         CLI   DRDSW,255  IF DUMMY REGION RQST NO NEDD TO READ   L09            
         BE    *+8        PRODUCT FOR DIVISION                   L09            
         BAS   RE,IGETPRD      GET PRDREC TO GET DIVISION                       
         BAS   RE,ST0CHKRD     EXTRACT REG/DIST OF PUB FROM LTLREC              
         MVC   0(11,R1),WORK    DIV/REG/DIST/SHR/SUM                            
ST0BX    L     RE,ST0B-4                                                        
         BR    RE                                                               
         SPACE 2                                                                
*                                                                  L19          
ST0E     ST    RE,ST19-4                                           L19          
*                                                                  L19          
         MVC   PBUYKEY,KEY        OAN CODE                                      
         BAS   RE,IGETPRD                                          L19          
         BAS   RE,DISPRTN                                          L19          
         BCTR  R3,0                                                L19          
         EX    R3,*+8                                              L19          
         B     *+10                                                L19          
         MVC   0(0,R1),PPRDOAN                                     L19          
         L     RE,ST19-4                                           L19          
         BR    RE                                                  L19          
ST0FF    DS    0H                                                  L19          
ST0F     ST    RE,ST19-4                                           L19          
*                                                                  L19          
         MVC   PBUYKEY,KEY        OAN NAME                                      
         BAS   RE,IGETPRD                                          L19          
         CLI   PPRDOAN,C' '      ANY VALUE PRESENT                L19           
         BH    ST0FFA                                              L19          
         BAS   RE,DISPRTN                                          L19          
         B     LANDRET                                             L19          
ST0FFA   MVC   SAVKEYS,KEY                                         L19          
         XC    KEY,KEY                                             L19          
*                                                                  L19          
         MVC   KEY(3),PBUYKAGY                                     L19          
         MVI   KEY+3,X'16'                                         L19          
         MVC   KEY+4(2),PPRDOAN                                    L19          
         CLC   PJOBREC(17),KEY                                     L19          
         BE    ST0FC               HAVE OAN REC                    L19          
* BUY-IO DIRECTORY FOR JOB RECORD                                  L19          
         GOTO1 HIGH                                                L19          
         CLC   KEY(17),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                OAN NOT ON FILE                              
         LA    R0,PJOBREC                                                       
         ST    R0,AREC                                                          
* BUY-IO READ FOR OAN RECORD                                                    
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
ST0FC    MVC   KEY(64),SAVKEYS        RESTORE KEYS                              
* BUY-IO DIRECTORY FOR JOB RECORD                                               
         GOTO1 HIGH                RESTORE FOR SEQ READ                         
         BAS   RE,DISPRTN                                                       
         LA    RE,PJOBREC                                                       
         SH    R3,=H'3'                                            L19          
         EX    R3,*+8                                              L19          
         B     *+10                                                L19          
*                                                                  L19          
         MVC   0(0,R1),35(RE)     OTHERAGENCY NAMES               L19           
         LA    R1,3(R3,R1)         INCREMENT SORT POINTER          L19          
         LA    R3,1                                                L19          
         EX    R3,*+8                                              L19          
         B     *+10                                                L19          
         MVC   0(0,R1),PPRDOAN                                    L19           
LANDRET  L     RE,ST19-4                                           L19          
         BR    RE                                                  L19          
*                                                                  L19          
*                                                                  L19          
*                                                                  L19          
         SPACE 2                                                                
******************************************************************              
*  MATCHES PUBLICATION OF BUYREC WITH REG/DST ELEM(71) OF LTLREC *              
*                                                                *              
*  IT PASSES FIRST X'71'ELEM (DIV/REG/DIST/SHR/SUM) TO WORK,     *              
*  THE OTHER MATCHING ELEMS ARE STORED IN RDSAVE AND WILL BE     *              
*  ACCESSED WHEN SORTREC IS PASSED TO SORTER                     *              
*                                                                *              
******************************************************************              
ST0CHKRD NTR1                                                                   
         XC    WORK(12),WORK        SET WORK                                    
         L     R5,ARDSAVE          DIV/REG/DIST/SHR/SUM SAVEAREA                
         MVI   0(R5),0                                                          
         L     R2,ALTLREC         GET LTLREC ADDRSABILITY                       
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'71'                                                     
         CLI   0(R2),X'71'                                                      
         BE    STRD12                                                           
STRD10   BAS   RE,NXTEL                                                         
         USING PUBDSTEL,R2                                                      
         BNE   STRD30                                                           
         SPACE                                                                  
STRD12   CLI   DRDSW,255  IF DUMMY REGION RQST                   L09            
         BNE   STRD121                                           L09            
*                                                                L09            
         CLC   PUBDDIV,=C'000'   DIVISION M/B 00                 L09            
         BNE   STRD10                                            L09            
         CLC   PUBDCLT,QPUB+4  USE DUMMY CLIENT TO SEARCH        L09            
         BNE   STRD10          LOOP BACK                         L09            
         B     STRD122                                           L09            
STRD121  CLC   PBUYKCLT,PUBDCLT TEST CLT OF BUYREC VS X'71' ELEM L09            
         BNE   STRD10                                                           
         CLC   PPRDDIV,PUBDDIV     TEST DIV OF PRDREC VS X'71'ELEM              
         BNE   STRD10                                                           
STRD122  DS    0H                                                L09            
*                                                                L10            
         CLC   QREGION,SPACES      IS THERE A REQION FILTER                     
         BE    STRD12A                                                          
         CLC   QREGION,=C'ALL'                                                  
         BE    STRD12A                                                          
         CLC   QREGION,PUBDREG                                                  
         BNE   STRD10                                                           
STRD12A  CLC   QDIST,=C'ALL'        IS THERE A DISTRICT FILTER                  
         BE    STRD12B                                                          
         CLC   QDIST,SPACES                                                     
         BE    STRD12B                                                          
         CLC   QDIST,PUBDDST                                                    
         BNE   STRD10                                                           
         SPACE                                                                  
STRD12B  CLI   WORK+11,X'FF'                                                    
         BE    STRD20                                                           
         MVC   WORK(9),PUBDDIV     DIV/REG/DIST (NOT SHARE/SUM)                 
         MVI   WORK+11,X'FF'                                                    
         B     STRD10              ARE THERE MORE MATCHING X'71' ELEM           
         SPACE                                                                  
STRD20   MVC   0(9,R5),PUBDDIV     SET D/R/D TO SAVEAREA (NOT SHR/SUM)          
*                                 WAS SHR/SUM ORIGINALLY SO I HAVE              
*                                 KEPT L' OF 11                                 
         MVI   11(R5),0         SET EOF TO ZERO                                 
         LA    R5,11(R5)               INCREMENT SAVE AREA                      
         B     STRD10                 ANY MORE ELEMS                            
         SPACE                                                                  
STRD30   DS    0H                                                               
         SPACE                                                                  
STRDX    XIT1                                                                   
         DROP R2                                                                
         SPACE                                                                  
         DS    F                                                                
ST0C     DS    0H                  DISTRICT                                     
         ST    RE,ST0C-4                                                        
         MVC   ADSDST,SDISP        SAVE DISP OF DST FOR MULT RECS               
         BAS   RE,DISPRTN                                       BUG05           
         BAS   RE,IGETLTL                                                       
         CLI   DRDSW,255  IF DUMMY REGION RQST NO NEDD TO READ   L09            
         BE    *+8        PRODUCT FOR DIVISION                   L09            
         BAS   RE,IGETPRD                                                       
*                                                               BUG05           
         BAS   RE,ST0CHKRD     GET CORRECT DISTRICT             BUG05           
         MVC   0(3,R1),WORK+6  DISTRICT                         BUG05           
*                                                               BUG05           
         L     RE,ST0C-4                                                        
         BR    RE                                                               
         SPACE                                                                  
         DS    F                                                                
ST0D     DS    0H                  DIVISION                                     
         ST    RE,ST0D-4                                                        
         BAS   RE,IGETPRD          GET PRDREC TO GET DIVISION                   
         BAS   RE,DISPRTN                                                       
         MVC   0(3,R1),PPRDDIV                                                  
         L     RE,ST0D-4                                                        
         BR    RE                                                               
         SPACE                                                                  
ST01     DS    0H                  PUB SORT                                     
         LA    R2,KEY+10                                                        
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R2,KEY+7                                                         
         ST    RE,ST0D-4                                                        
         BAS   RE,DISPRTN                                                       
         L     RE,ST0D-4                                                        
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),0(R2)                                                    
*                                                                               
         DS    F                                                                
ST02     DS    0H                  PUB SORT                                     
         ST    RE,ST02-4           SAVE RETURN REGISTER                         
         LA    R1,KEY+10                                                        
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+7                                                         
         CLC   OLDPUB,0(R1)                                                     
         BE    ST02H                                                            
         MVC   OLDPUB,0(R1)                                                     
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
***      MVC   KEY(1),PAGYKMED                                                  
***      MVC   KEY(1),QMEDIA                                                    
         MVC   KEY(1),PBUYKMED              FOR QMEDIA=C                        
         MVC   KEY+1(6),OLDPUB                                                  
*                                                                               
         MVC   KEY+7(2),PBUYKAGY                                                
         MVI   KEY+9,X'81'                                                      
         CLC   PUBKEY(7),KEY                                                    
         BE    ST02E                                                            
* PUB-IO  READ DIRECTORY PUB-RECORD                                             
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    ST02D                                                            
*                                                                               
* IF PUB NOT THERE, CLIENT MAY BE USING STANDARD SRDS PUB FILE UNDER            
*   CLIENT CODE OF ZZ. THIS ONLY HAPPENS WHEN PAGYPROF+16 IN                    
*   AGENCY PROFILE IS ZERO.                                                     
*                                                                               
         CLI   PAGYPROF+16,C'0'    TEST DEFAULT                                 
         BNE   *+8                                                              
         BAS   RE,INOPUB            BOMB PGM                                    
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+8                                                              
         BAS   RE,INOPUB            BOMB PGM                                    
         MVC   KEY+7(2),=C'ZZ'                                                  
         MVI   KEY+9,X'81'                                                      
         CLC   KEY(9),KEYSAVE                                                   
         BE    ST02D               HAVE SRDS                                    
* PUB-IO  READ DIRECTORY PUB-RECORD                                             
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    ST02D                                                            
INOPUB   DC    H'0'                PUB NOT FOUND                                
*                                                                               
ST02D    DS    0H                                                               
         BAS   RE,IGETP                                                         
*                                                                               
         B     ST02E                                                            
*                                                                               
*      TEST TO SEE IF DUPONT AGENCY                             L06             
**DP     CLC   QAGENCY,=C'DP'   DUPONT                          L06             
**DP     BNE   ST02E                                            L06             
**DP     CLC   PUBKAGY,=C'BD'   BBD & O PUB                     L06             
**DP     BE    ST02E           NO NEED TO CONVERT               L06             
*    IF CLIENT HEADER OPTION TO TRANSLATE PUB IS NOT ON TREAT     L06           
*    PUB AS IF AGENCY BD// THESE CLIENTS ARE OTHER AGENCIES THAT  L06           
*    ARE NOT ON DDS AND ARE ENTERED BY DU PONT                    L06           
*                                                                 L06           
         TM   PCLTACTL,X'80'                                      L06           
         BO   CNTINUE                                             L06           
         LA   RE,NOPUBLKU   ADD CLIENT TO LIST                    L06           
XDDNOPUB CLC  0(3,RE),PCLTKCLT   IF EQUAL TREAT AS IF CLI BD      L06           
         BE   ST02E                                               L06           
         CLI  0(RE),0                                             L06           
         BE   MOVECCLI                                            L06           
         CLI  0(RE),255                                           L06           
         BNE  *+6                                                               
         DC   H'0'                                                              
         LA   RE,3(RE)                                            L06           
         B    XDDNOPUB                                            L06           
MOVECCLI MVC  0(3,RE),PCLTKCLT                                    L06           
         B    ST02E                                                             
CNTINUE  DS   0H                                                  L06           
         LA    R2,PUBREC+33    POINT TO FIRST ELEMENT           L06             
         MVI   ELCODE,X'14'    LINK BACK TO BBD&O                L06            
AGAIN2   BAS   RE,NEXTEL4      FIND FIRST                        L06            
         BNE   INOPUB        **CRASH**                           L06            
*                                                                L06            
         CLC   2(3,R2),=C'DP ' ELEMENT LINKING PUBS WILL HAVE    L06            
         BNE   AGAIN2          DP PRESENT                        L06            
         MVC   KEY+7(2),=C'BD' MOVE AGENCY (HOPEFULLY BD)        L06            
         MVC   KEY+1(6),17(R2) PUB NUMBER IN BBD&O'S FILE        L06            
         MVC   WORK(9),KEY SAVE KEY                              L06            
         GOTO1 HIGHPUB         GET DISK ADDRESS                  L06            
         CLC   KEY(9),WORK      GOOD READ                        L06            
         BNE   INOPUB      ** CRASH**                            L06            
         BAS   RE,IGETP                                          L06            
***                                                                             
***      END OF OLD SPECIAL CODE FOR DUPONT                                     
***                                                                             
*************                                                    L06            
ST02E    DS    0H                                                               
         MVC   KEY(64),SAVKEYS                                                  
ST02H    DS    0H                                                               
         MVC   SKPUB(13),PUBNAME                                                
         CLC   SKPUB(4),=C'THE '                                                
         BNE   *+10                                                             
         MVC   SKPUB(13),PUBNAME+4                                              
         MVC   SKPUB+13(6),PUBKPUB                                              
*                                                                               
         BAS   RE,DISPRTN                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     ST02X                                                            
         MVC   0(0,R1),SKPUB                                                    
ST02X    L     RE,ST02-4                                                        
         BR    RE                                                               
*                                                                               
ST03     DS    0H                  PRODUCT CODE                                 
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LA    R2,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R2,KEY+13                                                        
         ZIC   R3,1(R4)            SORT LENGTH                                  
         LH    R0,SDISP                                                         
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),0(R2)       EXECUTED                                     
*                                                                               
ST04     LA    R1,SORTREC          CLOSING DATE                                 
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         CLI   CLSDTSW,C'Y'                                                     
         BNE   *+18                                                             
         MVC   0(2,R1),PBDCDATE                                                 
         MVI   2(R1),1                                                          
         BR    RE                                                               
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),PBDCDATE    EXECUTED                                     
*                                                                               
ST05     LA    R1,SORTREC          INSERTION DATE                               
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         MVC   WEKPTR,SDISP    WEEK ADDRESS FOR TEST IN TLOUT   L10             
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         CLI   INSDTSW,C'Y'                                                     
         BNE   *+18                                                             
         MVC   0(2,R1),KEY+16                                                   
         MVI   2(R1),1                                                          
         B     ST05X                                                            
         MVC   0(3,R1),KEY+16                                                   
         LH    R0,SRTLIN                                                        
         AH    R0,=H'1'                                                         
         STH   R0,SRTLIN                                                        
         MVC   3(2,R1),SRTLIN      TO PRESERVE ORIG SEQ                         
*                                                                L10            
         OC    WEEK,WEEK      WAS WEEK BREAK REQUESTED           L10            
         BZR   RE             FIND CORRECT WEEK ST AND END DATE  L10            
         ST    RE,DBL         SAVE RE                            L10            
         ST    R1,DBL+4       SAVE R1                            L10            
         LR    RF,R1                                             L10            
*        GOTO1 DTCNV,DMCB,(1,(RF)),(2,THISBUY)                   L10            
         GOTO1 DATCON,DMCB,(3,(RF)),(2,THISBUY)                  L10            
         L     R1,DBL+4       RESOTRE                            L10            
         CLC   THISBUY,WEEK   DOES INSERT DATE FALL WITHIN       L10            
         BL    WEEKOUT        NO                                 L10            
         CLC   THISBUY,WEEK+2 END DATE                           L10            
         BNH   MOVEWHT        WITHIN PRIOR BUY'S WEEK            L10            
WEEKOUT  L     RE,AWEEK       BEGINING OF TABLE                  L10            
         LA    RF,1           COUNT WEEKS                        L10            
WEEKLOOP CLI   0(RE),255      END CHECK                          L10            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   THISBUY,0(RE)   START DATE WEEK                   L10            
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   THISBUY,2(RE)   END DATE WEEK                     L10            
         BNH   MOVEDATE                                          L10            
         LA    RE,4(RE)        NEXT SET OF DATES                 L10            
         LA    RF,1(RF)        INCREMENT                         L10            
         B     WEEKLOOP                                          L10            
MOVEDATE MVC   WEEK,0(RE)      SAVE START & END                  L10            
         STC   RF,WHATWEEK                                       L10            
MOVEWHT  MVC   0(1,R1),WHATWEEK OVERLAY YEAR WITH WEEK NO.       L10            
         XC    1(4,R1),1(R1)    KNOCK OUT MMDD AND SEQ NO        L10            
         L     RE,DBL         RESTORE                            L10            
*                                                                L10            
*                                                                L10            
ST05X    BR    RE                                                               
*                                                                               
ST06     LA    R1,SORTREC          ON-SALE DATE                                 
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         CLI   ONSLDTSW,C'Y'                                                    
         BNE   *+18                                                             
         MVC   0(2,R1),PBDSDATE                                                 
         MVI   2(R1),1                                                          
         BR    RE                                                               
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),PBDSDATE    EXECUTED                                     
*                                                                               
ST07     DS 0H                    SPACE DESCRIPTION                             
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,ST07B                                                         
***      CLI   QMEDIA,C'M'                                                      
         CLI   PBUYKMED,C'M'       FOR QMEDIA=C                                 
         BNER  RE                                                               
         EX    R3,ST07A                                                         
         BR    RE                                                               
ST07A    OC    0(0,R1),=17X'40'                                                 
ST07B    MVC   0(0,R1),PBDSPACE    EXECUTED                                     
         SPACE                                                                  
*                                                                               
         DS    F                                                                
ST08     DS    0H                  PRODUCT NAME                                 
         ST    RE,ST08-4                                                        
         MVC   PBUYKEY,KEY                                                      
         BAS   RE,IGETPRD                                                       
         BAS   RE,DISPRTN                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     ST08X                                                            
         MVC   0(0,R1),PPRDNAME    EXECUTED FROM PRODUCT REC                    
*                                                                               
ST08X    L     RE,ST08-4                                                        
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
ST99     ST    RE,ST09-4  SAVE RETURN REGISTER                    L16           
         PRINT GEN                                                              
         GOTO1 =A(COMCAPT),DMCB,(C'T',=C'CAPTION')                L16           
         PRINT NOGEN                                                            
         L     RF,DMCB+4     COMCAPT RETURNS ADDRESS OF MESSAGE   L16           
         OC    0(12,RF),0(RF)    ANY OVERRIDES                    L16           
         BZ    ST9909                                             L16           
         BAS   RE,DISPRTN                                         L16           
         MVC   0(25,R1),0(RF)    MOVE CAPTION TO SORTKEY          L16           
         L     RE,ST09-4                                          L16           
         BR    RE                RETURN TO CALLER                 L16           
*                                                                 L16           
ST9909   OI    BUG02+1,X'F0'     FORCE BRANCH                     L16           
*                                                                 L16           
         B      ST99USE                              BUG22/     BUG02           
*                                                               BUG02           
*                                                                               
         DS    F                                                                
ST09     DS    0H                  COPY NUMBER                                  
         ST    RE,ST09-4                                                        
         GOTO1 =A(COMCAPT),DMCB,(C'C',=C'COPY')                   L16           
         L     RF,DMCB+4     COMCAPT RETURNS ADDRESS OF MESSAGE   L16           
         OC    0(12,RF),0(RF)    ANY OVERRIDES                    L16           
         BZ    ST99USE                                            L16           
         BAS   RE,DISPRTN                                         L16           
         MVC   0(17,R1),0(RF)    MOVE COPY    TO SORTKEY          L16           
         B     STO9X                                              L16           
*                                                                               
ST99USE  DS    0H                                                 L16           
*                                                                 L16           
         OC    PBDJOB,PBDJOB                                                    
         BZ    ST09W               NO JOB NUMBER                                
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
*                                                                               
         MVC   KEY(2),PBUYKAGY                                                  
***      MVC   KEY+2(1),QMEDIA                                                  
***      MVC   KEY+2(1),PAGYKMED                                                
         MVC   KEY+2(1),PBUYKMED            FOR QMEDIA=C                        
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         CLI   PBUYKRCD,X'20'                                                   
         BE    ST09B                                                            
         MVC   KEY+7(3),PBUYKEY+13                                              
         B     *+10                                                             
ST09B    MVC   KEY+7(3),PBUYKPRD                                                
*                                                                 BUG27         
         CLC   PBUYKACT,=C'ZZZ'                                   BUG27         
         BNE   *+10                                               BUG27         
         MVC   KEY+7(3),=C'ZZZ'                                   BUG27         
*                                                                 BUG27         
         MVC   KEY+10(6),PBDJOB                                                 
         CLC   PJOBREC(17),KEY                                                  
         BE    ST09C               HAVE JOB REC                                 
* BUY-IO DIRECTORY FOR JOB RECORD                                               
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JOB NOT ON FILE                              
         LA    R0,PJOBREC                                                       
         ST    R0,AREC                                                          
* BUY-IO READ FOR JOB RECORD                                                    
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
ST09C    MVC   KEY(64),SAVKEYS        RESTORE KEYS                              
* BUY-IO DIRECTORY FOR JOB RECORD                                               
         GOTO1 HIGH                RESTORE FOR SEQ READ                         
         BAS   RE,DISPRTN                                                       
         BCTR  R3,0                                                             
BUG02    BC     0,CAPTIONS                                      BUG02           
*                                                               BUG02           
         EX    R3,*+8                                                           
         B     STO9X                                                            
         MVC   0(0,R1),PJOBCPY     EXECUTED                                     
         B     STO9X                                              L16           
ST09W    BAS   RE,DISPRTN                                         L16           
*                                                                 L16           
STO9X    L     RE,ST09-4                                                        
         BR    RE                                                               
*                                                                               
CAPTIONS MVI    BUG02+1,0                                       BUG02           
         EX     R3,*+8                                          BUG02           
         B      STO9X                                           BUG02           
         MVC    0(0,R1),PJOBCAP1                                BUG02           
*                                                               BUG02           
         DS    F                                                                
ST10     DS    0H                  DIVISION  CODE                               
         ST    RE,ST10-4                                                        
         MVC   PBUYKEY,KEY                                                      
         BAS   RE,IGETPRD                                                       
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         STH   R0,DIVSD            SAVE DIV SORT DISPLACEMENT                   
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         BCTR  R3,0                                                             
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         EX    R3,*+8                                                           
         B     ST10X                                                            
         MVC   0(0,R1),PPRDDIV     EXECUTED FROM PRODUCT REC                    
*                                                                               
ST10X    L     RE,ST10-4                                                        
         BR    RE                                                               
         SPACE 2                                                                
         DS    F                                                                
ST28     DS    0H                  DIVISION  NAME                  L20          
         ST    RE,ST28-4                                           L20          
         MVC   PBUYKEY,KEY                                         L20          
         BAS   RE,IGETPRD                                           L20         
*                                                                   L20         
*   READ DIVISION RECORD                                            L20         
*                                                                   L20         
         LA    RE,PDIVREC                                           L20         
         ST    RE,AREC                                              L20         
         MVC   KEY(25),PPRDREC                                  L20             
         MVI   KEY+3,3                                              L20         
         MVC   KEY+7(3),PPRDDIV      DIVISION                       L20         
         CLC   PDIVREC(25),KEY       ALREADY THERE                  L20         
         BE    NODIVH                                              L20          
         GOTO1 HIGH                                                 L20         
         CLC   KEY(25),KEYSAVE                                      L20         
         BE    *+14                                                 L20         
         XC    PDIVNAME,PDIVNAME                                    L20         
         B     NODIVH                                               L20         
         GOTO1 GETDIV                                               L20         
*                                                                   L20         
NODIVH   MVC   KEY,PBUYKEY                                          L20         
         GOTO1 HIGH                                                 L20         
*                                                                   L20         
*                                                                   L20         
*                                                                   L20         
*                                                                   L20         
*                                                                   L20         
*                                                                   L20         
MOVEDIR  BAS   RE,DISPRTN           POINT TO SORTREC FOR DIV NAME   L20         
*                                                                   L20         
*                                                                   L20         
*                                                                   L20         
         BCTR  R3,0                                                 L20         
         EX    R3,*+8                                               L20         
         B     ST28X                                                L20         
         MVC   0(0,R1),PDIVNAME    EXECUTED FROM PRODUCT REC        L20         
*                                                                   L20         
ST28X    L     RE,ST28-4                                            L20         
         BR    RE                                                   L20         
*                                                                               
ST11     DS    0H                  ESTIMATE                                     
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         ZIC   R3,1(R4)            SORT LENGTH                                  
         LH    R0,SDISP                                                         
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),KEY+19      EXECUTED                                     
*                                                                               
ST12     LA    R1,SORTREC          PAYABLE DATE                                 
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         CLI   PAYDTSW,C'Y'                                                     
         BNE   *+18                                                             
         MVC   0(2,R1),PBDPDATE                                                 
         MVI   2(R1),1                                                          
         BR    RE                                                               
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),PBDPDATE    EXECUTED                                     
*                                                                               
ST13     LA    R1,SORTREC          BILLABLE DATE                                
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         CLI   BILDTSW,C'Y'                                                     
         BNE   *+18                                                             
         MVC   0(2,R1),PBDBDATE                                                 
         MVI   2(R1),1                                                          
         BR    RE                                                               
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),PBDBDATE    EXECUTED                                     
*                                                                               
ST14     LA    R1,SORTREC          AD CODE                                      
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),PBDJOB      EXECUTED                                     
*                                                                               
ST15     LA    R1,SORTREC          MATERIALS CLOSING DATE                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         CLI   MATDTSW,C'Y'                                                     
         BNE   *+18                                                             
         MVC   0(2,R1),PBDMDATE                                                 
         MVI   2(R1),1                                                          
         BR    RE                                                               
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),PBDMDATE    EXECUTED                                     
*                                                                               
         DS    F                                                                
ST19     DS    0H                                                               
         ST    RE,ST19-4                                                        
         BAS   RE,DISPRTN                                                       
         LA    R2,PBUYREC                                                       
         MVI   ELCODE,X'66'                                                     
         LA    R2,33(R2)                                                        
         BAS   RE,NEXTEL4                                                       
         BNE   ST19X                                                            
         MVC   0(10,R1),2(R2)                                                   
ST19X    L     RE,ST19-4                                                        
         BR    RE                                                               
*                                                                               
         DS    F                   ESTIMATE DESCRIPTION                         
ST20     ST    RE,ST20-4                                                        
         BAS   RE,IGETEST          GET ESTIMATE HEADER                          
         BAS   RE,DISPRTN                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),PESTNAME    EXECUTED                                     
         L     RE,ST20-4                                                        
         BR    RE                                                               
*                                                                               
*        GETASPO NUMBER ROUTINE FROM PP6602                                     
         DS    F                                                                
ST93     DS    0H                                                               
         ST    RE,ST93-4                     GETASPO                            
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         MVC   SAVKEYS,KEY                                                      
         CLC   PBUYREC(2),=C'BD'                USE JOB FOR BD                  
         BNE   ST93I                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+10(2),PBUYKEY+19                                             
         CLC   PESTKEY(13),KEY              GET ASPO NO FROM EST REC.           
         BE    ST93D                                                            
* BUY-IO READ ESTIMATE DIRECORY                                                 
         GOTO1 READ                                                             
* BUY-IO READ ESTIMATE RECORD                                                   
         GOTO1 GETPRT                                                           
ST93D    CLC   PESTNAME(2),=C'**'                                               
         BNE   ST93E                                                            
         MVC   WORK(9),PESTNAME+2                                               
         B     ST93R                                                            
*                                                                               
ST93E    MVC   WORK(2),PBUYKAGY                                                 
         MVI   WORK+1,C'S'         SET                                          
         MVI   WORK+2,C'-'                                                      
         MVC   WORK+3(6),PBDJOB                                                 
         B     ST93R                                                            
*                                                                               
ST93I    XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+10(2),PBUYKEY+19                                             
         CLC   PESTKEY(13),KEY              GET ASPO NO FROM EST REC.           
         BE    ST93M                                                            
* BUY-IO READ ESTIMATE DIRECTORY                                                
         GOTO1 READ                                                             
* BUY-IO READ ESTIMATE RECORD                                                   
         GOTO1 GETPRT                                                           
ST93M    MVC   WORK(9),PESTNAME       ASPO NO FIRST 9 CHARS OF EST NAME         
*                                                                               
ST93R    LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         MVC   KEY(64),SAVKEYS                                                  
* BUY-IO READ ESTIMATE DIRECTORY                                                
         GOTO1 HIGH                                                             
*                                                                               
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         MVC   0(9,R1),WORK             EXECUTED INSTRUCTION                    
         L     RE,ST93-4                                                        
         BR    RE                                                               
*                                                                               
         DS    F                                                                
ST94     DS    0H                  LIST CODE                                    
         ST    RE,ST94-4                                                        
         LA    R1,SORTREC          AD CODE                                      
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),PBDLIST     EXECUTED                                     
*                                                                               
         DS    F                                                                
ST95     DS    0H                                                               
         ST    RE,ST95-4                                                        
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'70'                                                     
ST95B    BAS   RE,NEXTEL4                                                       
         BNE   ST95G                                                            
         OC    2(3,R2),2(R2)                                                    
         BZ    ST95B                                                            
         CLC   SV70ELM+2(3),2(R2)                                               
         BNL   ST95B                                                            
         MVC   SV70ELM(11),0(R2)                                                
         B     ST95B                                                            
*                                                                               
ST95G    LA    R1,SORTREC                                                       
         AH    R1,SDISP                                                         
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)                                                         
         AR    R0,R3                                                            
         STH   R0,SDISP                                                         
         BCTR  R3,0                                                             
         EX    R3,ST95Z                                                         
ST95X    L     RE,ST95-4                                                        
         BR    RE                                                               
ST95Z    MVC   0(0,R1),SV70ELM+2                                                
*                                                                               
*                                                                               
         SPACE                                                                  
* ROUTINE TO ADD DISP TO SORTREC AND STORE NEW DISP *                           
DISPRTN  DS    0H                                                               
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
TLIN60   DS    0H                                                               
*        NOW  AT THE END OF EACH SORTREC                                        
* ADD THE FIELDS THAT ARE NOT THE PRIMARY SORT FIELDS                           
         LA    R4,RPTTAB                                                        
TLIN62   LA    R5,SORTTAB                                                       
         CLI   0(R4),X'FF'       END OF TABLE                                   
         BE    TLIN80                                                           
         CLI   4(R4),X'FF'       X'FF'=NOT PRIMARY SORT                         
         BNE   TLIN75            PRIMARY SORT, ALREADY ADDED                    
         CLI   0(R4),X'27'         IS IT DOLLAR FIELD                           
         BH    TLIN75              IF SO,SKIP                                   
         CLI   0(R4),X'13'         IS IT COMMENTS FIELD/SKIP                    
         BE    TLIN75                                                           
         CLI   0(R4),X'14'         IS IT EST DESCR/SKIP                         
         BE    TLIN75                                                           
         CLI   0(R4),X'04'         IS IT REGION NAME/SKIP                       
         BE    TLIN75                                                           
         CLI   0(R4),X'05'         IS IT DISTRICT NAME/SKIP                     
         BE    TLIN75                                                           
         CLI   0(R4),X'18'         IS IT CAPTION/SKIP                           
         BE    TLIN75                                                           
         CLI   0(R4),X'19'         IS IT LAST AD USE FIELD                      
         BE    TLIN75                                                           
         CLI   0(R4),X'1A'         BLANK COLUMN                                 
         BE    TLIN75                                                           
TLIN64   CLI   0(R5),X'FF'         END OF SORTTAB                               
         BNE   *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         CLC   0(1,R4),0(R5)                                                    
         BE    TLIN66                                                           
         LA    R5,5(R5)                                                         
         B     TLIN64              NEXT ENTRY                                   
*                                                                               
TLIN66   MVC   FULL,1(R5)                                                       
         L     RF,FULL                                                          
         BAS   RE,0(RF)                                                         
*                                                                               
TLIN75   LA    R4,5(R4)            NEXT ENTRY IN RPTTAB                         
         B     TLIN62                                                           
         SPACE                                                                  
TLIN80   DS    0H                                                               
         LA    R1,SORTREC                                                       
         CLC   SDISP,=H'90'        90 IS MAX SORT LENGTH FOR PPG                
         BNH   *+10                                                             
         MVC   SDISP,=H'90'                                                     
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         AH    R1,=H'2'                                                         
         MVC   0(4,R1),KEY+27   DISK ADDR                                       
         MVC   4(3,R1),PASSPRD      MOVE PRODUCT IF  ANY          L15           
*                                                                 L15           
         L     R3,SAVPARS+4                                                     
         LH    R1,SDISP                                                         
         AH    R1,=H'8'    ADJUST FOR DISK ADDR/AND PRODUCT       L15           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SORTREC                                                  
         B     TLINX                                                            
         EJECT                                                                  
*                                                                               
TLIN90   DS    0H                                                               
*                                                                               
**DP     CLC   QAGENCY,=C'DP'       FOR DUPONT KEEP READING FOR ALL             
**DP     BE    TLIN4                AGENCIES                                    
*                                                                               
         CLI   RCMULTIQ,C'C'        IF MEDIA IS C COMBINE INFO FOR ONE          
*****                                                                           
         BNE   TLIN90A              ESTIMATE DISREGARDING MEDIA BREAK           
         ZIC   R1,RCSVAGY+2                                                     
         LA    R1,1(R1)                                                         
         STC   R1,RCSVAGY+2                                                     
         B     TLIN4A                                                           
*****                                                                           
*                                                                               
TLIN90A  XC    OLDPUB,OLDPUB                                                    
         MVI   SAVPARS+3,8                                                      
*                                                                               
TLINX    DS    0H                                                               
         CLI   QOPT6,C'Y'                                                       
         BNE   TLIXIT                                                           
         L     R2,AP1                                                           
         CLI   SAVPARS+3,8    ABOUT TO SORT                                     
         BNE   *+14                                                             
         MVC   0(15,R2),=C'COMMAND TO SORT'                                     
         B     GOOOT                                                            
         MVC   0(8,R2),=C'SORTREC='                                             
         GOTO1 HEXOUT,DMCB,SORTREC,9(R2),50,0                                   
GOOOT    DS    0H                                                               
         GOTO1 REPORT                                                           
         SPACE 3                                                                
TLIXIT   DS    0H                                                               
TLIXX    XIT1                                                                   
         SPACE                                                                  
NEXTEL4  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL6                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL4+2                                                        
NEXTEL6  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                 L15           
*                                                                 L15           
PASSPTR  LA    RF,KEY+7      DETERMINE LOCATION OF PRODUCT IN KEY L15           
         CLI   KEY+3,X'20'                                        L15           
         BE    *+8                                                L15           
         LA    RF,KEY+13                                          L15           
         XC    PASSPRD,PASSPRD                                    L15           
         CLI   PASSPTRS,C'N'     ZZZ PRODUCT REQUESTED            L15           
         BNE   NONZZZPR                                           L15           
         OC    KEY+21(3),KEY+21   ANYTHING IN PASSIVE POINTER     L15           
         BNZ   0(RE)           IF PROD THERE READ ANOTHER         L15           
         B     4(RE)           OK                                 L15           
*                                                                 L15           
* NON ZZZ REQUESTED PRODUCT (ALL OR BLANK)                        L15           
NONZZZPR CLC   0(3,RF),=C'ZZZ'   POL PROD                         L15           
         BER   RE                                                 L15           
*                                                                 L15           
         OC    KEY+21(3),KEY+21                                   L15           
         BZ    *+10                                               L15           
         MVC   PASSPRD,0(RF)                                      L15           
         B     4(RE)                                              L15           
         EJECT                                                                  
****************************************************************                
*  TEST IF BUY IS SHARED AMONG REGIONS/ARE THERE 71S IN RDSAVE *                
*  IF YES/ PASS THESE TO SORTREC                               *                
*  I ALWAYS MOVE IN 0 TO 1ST BYTE OF RDSAVE WHEN I HIT EOF     *                
*  INPUT: SORTREC                                              *                
*         RDSAVE,NEXTSAVE                                      *                
*         DISPLACEMENT INTO SORTREC                            *                
*  OUTPUT: SORTRECS WITH DIFF RE/DIST/SHR/SUM                  *                
****************************************************************                
MULTREC  L     R1,ARDSAVE                                                       
         CLI   0(R1),0                                                          
         BE    MLTXX                                                            
         LA    R2,SORTREC                                                       
         AH    R2,ADSPRD                                                        
         MVC   0(11,R2),0(R1)       SET IN NEW DIV/REG/DST/SHR/SUM              
         LA    R2,SORTREC                                                       
         CLC   ADSDST,=2X'0'         IF ADSDST=0/THEN NO DIST COLUMN            
         BE    MLT5                          AND NO DIST=ALL                    
         AH    R2,ADSDST                                                        
         MVC   0(3,R2),6(R1)       SET IN DST IN ITS OWN AREA ALSO              
         SPACE                                                                  
MLT5     CLC   QREGION,=C'ALL'      IF REG=ALL                                  
         BNE   MLT7                SET IN NEW DIV/ETC IN 1ST REG AREA           
         LA    R4,BRKKEY                                                        
MLT5A    CLI   0(R4),X'02'                                                      
         BNE   MLT6                                                             
         ZIC   R0,1(R4)                                                         
         LA    R4,SORTREC                                                       
         AR    R4,R0                                                            
         MVC   0(11,R4),0(R1)      MOVE IT IN                                   
         B     MLT7                                                             
MLT6     LA    R4,3(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   MLT5A                                                            
         DC    H'0'                                                             
MLT7     MVI   0(R1),0                                                          
         LA    R1,11(R1)                                                        
         CLI   0(R1),0            TEST ANY MORE ELEMS                           
         BE    MLT10                                                            
         ST    R1,ARDSAVE          YES/STORE NXT ADDRS IN ARDSAVE               
         B     MLTX                                                             
MLT10    MVC   ARDSAVE,ARDSAVE1    NO/RESET ADDRES/MAYBE BUMPED                 
         L     R1,ARDSAVE                                                       
         MVI   0(R1),0             SET FIRST BYTE = 0                           
MLTX     SR    R1,R1               R1 SET TO ZERO = REC FOR SORTER              
MLTXX    BR    RE                                                               
         EJECT                                                                  
*************************************                                           
*  ROUTINE CHECKS ESTIMATE FILTERS  *                                           
*                                   *                                           
*  CALLED ONLY FROM TLIN19          *                                           
*************************************                                           
         SPACE                                                                  
ESTFLTR  NTR1                                                                   
         CLC   PBUYKEY(3),PESTKAGY     IS EST HEADER AROUND (AGY/MED)           
         BNE   EF05                                                             
         CLC   PBUYKCLT(6),PESTKCLT    (CLT/PRD)                                
         BNE   EF05                                                             
         CLC   PBUYKEST,PESTKEST       (EST)                                    
         BE    EF06                                                             
EF05     BAS   RE,IGETEST                                                       
*                                                                               
EF06     CLC   QESTEND,=C'   '        FOUR CONDITIONS I GET HERE WITH           
         BE    TSTEST                 IN QEST-QESTEND :                         
         CLC   QEST,=C'   '                    ALL-BBB                          
         BE    EF07                            BBB-BBB                          
         B     TSTEST                          BBB-FFF                          
*                                              NNN-NNN                          
EF07     LA    RE,QESTEND          TEST ESTIMATE GROUPS                         
         LA    RF,PESTGRPS                                                      
         LA    R0,3                                                             
         SPACE                                                                  
EF10     DS    0H                                                               
         CLI   0(RE),C'*'                                                       
         BE    EF14                                                             
         CLI   0(RE),C' '                                                       
         BE    EF14                                                             
         SPACE                                                                  
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    EF12                                                             
         SPACE                                                                  
         CLC   0(1,RE),0(RF)       POSITIVE FILTER                              
         BNE   EF15                                                             
         B     EF14                                                             
         SPACE                                                                  
EF12     DS    0H                                                               
         MVC   DUB(1),0(RE)                                                     
         OI    DUB,X'40'                                                        
         CLC   DUB(1),0(RF)        CHECK NEGATIVE FILTER                        
         BE    EF15                                                             
         SPACE                                                                  
EF14     DS    0H                                                               
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,EF10                                                          
         LA    R1,1                EST IS VALID - SET TO NON-ZERO               
         B     EFX                                                              
         SPACE                                                                  
EF15     SR    R1,R1               ERROR - SET TO ZERO                          
         B     EFX                                                              
*                                                                               
TSTEST   DS    0H                                                               
         LA    R1,1                                               BUG09         
*        TM    PESTTEST,X'80'                                     BUG09         
*        BZ    EFX                                                BUG09         
*        SR    R1,R1                                              BUG09         
*                                                                               
EFX      LTR   R1,R1                                                            
         XIT1                                                                   
         EJECT                                                                  
****************************************                                        
*  CHECKS REGION/DISTRICT OF REQUEST   *                                        
*  AGAINST LTLREC                      *                                        
*  OUT: R1 NOT ZERO IF NO MATCH        *                                        
*                                      *                                        
****************************************                                        
CHKRGDST NTR1                                                                   
         L     R2,ALTLREC                                                       
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'71'                                                     
         CLI   0(R2),X'71'                                                      
         BE    *+8                                                              
CRD10    BAS   RE,NXTEL                                                         
         USING PUBDSTEL,R2                                                      
         BNE   CRD30                                                            
         CLI   DRDSW,255  IF DUMMY REGION RQST                   L09            
         BNE   CRD11                                             L09            
         CLC   PUBDDIV,=C'000'   DIVISION M/B 00                 L09            
         BNE   CRD11                                             L09            
         CLC   PUBDCLT,QPUB+4  USE DUMMY CLIENT TO SEARCH        L09            
         BNE   CRD10           LOOP BACK                         L09            
         B     CRD12                                             L09            
CRD11    DS    0H                                                L09            
         CLC   PBUYKCLT,PUBDCLT                                                 
         BNE   CRD10                                                            
         CLC   PPRDDIV,PUBDDIV     TEST DIV AGAINST PRDREC                      
         BNE   CRD10                                                            
CRD12    DS    0H                                                L09            
         CLC   PUBDREG,QREGION                                                  
         BNE   CRD10                                                            
CRD15    CLC   QDIST,SPACES                                                     
         BE    CRD25                                                            
         CLC   QDIST,=C'ALL'                                                    
         BE    CRD25                                                            
         CLC   PUBDDST,QDIST                                                    
         BNE   CRD10                                                            
CRD25    SR    R1,R1                                                            
         MVC   WORK(3),PUBDREG                                                  
         MVC   WORK+3(3),PUBDDST                                                
         B     CRDX                                                             
CRD30    LA    R1,1                                                             
         SPACE                                                                  
CRDX     XIT1  REGS=(R1)                                                        
         DROP R2                                                                
         EJECT                                                                  
NXTEL    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NXTEL1                                                           
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NXTEL+2                                                          
NXTEL1   LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 2                                                                
IGETLTL  NTR1                                                                   
         DS    0H              TEST IF LTLREC ALREADY THERE                     
         LA    R1,PBUYKEY+10       X'20'KEY-PRD/PUB                             
         CLI   PBUYKRCD,X'20'                                                   
         BE    *+8                                                              
         LA    R1,PBUYKEY+7        X'21'KEY-PUB/PRD                             
         L     R2,ALTLREC                                                       
         USING LTLREC,R2                                                        
         CLC   LTLKPUB(6),0(R1)    PUB/Z/ED OF BUY= OF LTL                      
         DROP  R2                                                               
         BE    LTLX                                                             
         SPACE                                                                  
         MVC   WORK(64),KEY     REQUIRED LTLREC NOT THERE/GET IT                
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING LTLKEY,R2                                                        
***      MVC   LTLKMED,QMEDIA                                                   
         MVC   LTLKMED,PBUYKMED          FOR QMEDIA=C                           
         MVC   LTLKPUB(6),0(R1)        PUB/Z/ED                                 
         MVC   LTLKAGY,QAGENCY                                                  
         MVI   LTLKCOD,X'85'                                                    
         DROP R2                                                                
* PUB-IO  READ DIR LTLREC                                                       
         GOTO1 HIGHPUB                                                          
         CLC   KEY(11),KEYSAVE                                                  
         BE    LTL5                                                             
         L     R2,ALTLREC                                                       
         XC    0(40,R2),0(R2)             CLEAR LTLREC 71 AREA                  
         B     LTL10                                                            
LTL5     L     R2,ALTLREC                                                       
* PUB-IO  READ RECORD LTLREC                                                    
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,(R2),(0,DMWORK)               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
LTL10    MVC   KEY,WORK            RESET KEY                                    
* PUB-IO READ DIRECTORY                                                         
         GOTO1 HIGH                RESTORE SEQUENTIAL READ                      
LTLX     B     TLIXIT              XIT1                                         
         SPACE 2                                                                
IGETCLT  NTR1                                                                   
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),PBUYKEY                                                   
         MVI   KEY+3,2                                                          
* BUY-IO READ DIRECTORY  CLIENT HEADER                                          
         GOTO1 READ                                                             
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
* BUY-IO READ  CLIENT HEADER                                                    
         GOTO1 GETPRT                                                           
         MVC   KEY(64),WORK                                                     
* BUY-IO READ  DIRECTORY RESTORE DATA MGR POINTERS OF LAST READ                 
         GOTO1 HIGH                                                             
         B     TLIXIT        XIT1                                               
         SPACE 3                                                                
IGETPRD  NTR1                                                                   
         LA    R1,PBUYKEY+7        X'20'KEY-PRD/PUB/Z/ED                        
         CLI   PBUYKRCD,X'20'                                                   
         BE    *+8                                                              
         LA    R1,PBUYKEY+13       X'21'KEY-PUB/Z/ED/PRD                        
         CLC   PPRDKPRD(3),0(R1)                                                
         BNE   IPRD10                                                           
         CLC   PPRDKAGY(3),PBUYKAGY       AGY/MED                               
         BNE   IPRD10                                                           
         CLC   PPRDKCLT,PBUYKCLT          CLT                                   
         BE    IPRDX                                                            
IPRD10   MVC   WORK(64),KEY        PRDREC NOT THERE/GET IT                      
         XC    KEY,KEY                                                          
         MVC   KEY(3),PBUYKEY      AGY/MED                                      
         MVI   KEY+3,6                                                          
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),0(R1)      PRD                                          
* BUY-IO READ DIRECTORY PRODUCT HEADER                                          
         GOTO1 READ                                                             
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
* BUY-IO READ  PRODUCT HEADER                                                   
         GOTO1 GETPRT                                                           
         MVC   KEY(64),WORK                                                     
* BUY-IO RESTORE POINTERS IN D/M                                                
         GOTO1 HIGH                                                             
IPRDX    B     TLIXIT        XIT1                                               
         SPACE 2                                                                
* IGETDST ONLY CALLED FROM ST0C *                                               
IGETDST  NTR1                       GET DISTRICT FROM LTLRC                     
         L     R2,ALTLREC                                                       
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'71'                                                     
         CLI   0(R2),X'71'                                                      
         BE    IDST12                                                           
IDST10   BAS   RE,NXTEL                                                         
         USING PUBDSTEL,R2                                                      
         BNE   IDST20                                                           
IDST12   CLC   PBUYKCLT,PUBDCLT    TEST CLT OF BUYREC VS X'71'ELEM              
         BNE   IDST10                                                           
         CLC   PPRDDIV,PUBDDIV     TEST DIV OF PRDREC VS X'71'ELEM              
         BNE   IDST10                                                           
         MVC   WORK(3),PUBDDST                                                  
IDST20   DS    0H                                                               
         B     TLIXIT                                                           
         SPACE 2                                                                
IGETEST  NTR1                                                                   
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVC   KEY+10(2),PBUYKEST                                               
         MVI   KEY+3,7                                                          
* BUY-IO READ EST DIR                                                           
         GOTO1 READ                                                             
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
* BUY-IO READ EST REC                                                           
         GOTO1 GETPRT                                                           
         MVC   KEY(64),WORK                                                     
* BUY-IO READ EST DIR RESET POINTERS D/M                                        
         GOTO1 HIGH                                                             
         B     TLIXIT      XIT1                                                 
*                                                                               
IGETP    NTR1                                                                   
* PUB-IO READ PUB RECORD                                                        
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,PUBREC,(0,DMWORK)             
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     TLIXIT              XIT1                                         
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
SORTTAB  DC    AL1(01),AL4(ST0A)     CLIENT          3                          
         DC    AL1(02),AL4(ST0B)     REGION          11                         
         DC    AL1(03),AL4(ST0C)     DISTRICT        3                          
         DC    AL1(249),AL4(ST0D)    DIVISION      FOR HEADER ONLY              
         DC    AL1(10),AL4(ST01)     PUB NUMBER      6                          
         DC    AL1(11),AL4(ST02)     PUB NAME      20                           
         DC    AL1(12),AL4(ST03)     PRODUCT CODE 3                             
         DC    AL1(13),AL4(ST08)     PRODUCT NAME   20                          
         DC    AL1(14),AL4(ST11)     ESTIMATE     2                             
         DC    AL1(15),AL4(ST07)     SPACE DESCRIPTION 17 P1,P2                 
         DC    AL1(16),AL4(ST10)     DIVISION     3                             
         DC    AL1(17),AL4(ST14)     AD CODE 6                                  
         DC    AL1(18),AL4(ST09)     COPY NUMBER    17                          
         DC    AL1(19),AL4(ST19)     COMMENTS       10                          
         DC    AL1(20),AL4(ST20)     ESTIMATE DESCRIPTION 17                    
         DC    AL1(21),AL4(ST93)     ASPO NUMBER                                
         DC    AL1(22),AL4(ST94)     LIST CODE                                  
         DC    AL1(23),AL4(ST95)     LAST I/O                                   
         DC     AL1(24),AL4(ST99)     CAPTION                   BUG02           
         DC    AL1(27),AL4(ST14)     AD CODE 5                                  
         DC    AL1(28),AL4(ST28)     DIVISION NAME                 L20          
         DC    AL1(30),AL4(ST05)     INSERTION DATE 5                           
         DC    AL1(31),AL4(ST04)     CLOSING DATE 3                             
         DC    AL1(32),AL4(ST06)     ON-SALE DATE 3                             
         DC    AL1(33),AL4(ST12)     PAYABLE DATE 3                             
         DC    AL1(34),AL4(ST13)     BILLABLE DATE 3                            
         DC    AL1(35),AL4(ST15)     MAT CLOSING DATE 3                         
         DC    AL1(36),AL4(ST0E)     OAN CODE         2                         
         DC    AL1(37),AL4(ST0F)     OAN NAME        33                         
         DC    AL1(38),AL4(ST0FF)    OAN NAME + CODE 35                         
         DC    X'FFFF'               END OF TABLE                               
*                                                                               
*              40 THRU 89 ARE $ AMOUNTS -  NO SORTING                           
*                                                                               
         TITLE 'TLOUT - HANDLE OUTPUT OF SORT'                                  
         SPACE 2                                                                
*                                  OUTPUT                                       
         PRINT NOGEN                                                            
TLOUT    NMOD1 0,TLOUT                                                          
*                                                                               
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,SPACEND                                                       
         USING PP81WRKD,R8                                                      
         LA    R6,1(RB)                                                         
         LA    R6,4095(R6)                                                      
         LA    R9,1(R6)                                                         
         LA    R9,4095(R9)                                                      
         USING TLOUT,RB,R6,R9                                                   
*****                                                                           
         CLI   QOPT2,C'D'                                                       
         BNE   *+8                                                              
         MVI   DOWNLOAD,C'Y'                                                    
*****                                                                           
*****  THIS MAY BE A CALL FROM ROUTINE COMPRT0 TO UTILIZE SUBROUTINE            
*****         TLPRT                                                             
        L      RE,4(R1)       POINT TO CONSTANT                  L08            
        CLC    0(4,RE),=C'COMP'                                  L08            
        BNE    CONTINU                                           L08            
        BAS    RE,TLPRT                                          L08            
        XIT1                                                     L08            
CONTINU DS     0H                                                L08            
         CLI   DOWNLOAD,C'Y'                                                    
         BE    *+12                                                             
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         MVC   SORTREC,0(R3)                                                    
*                                                                               
*                                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVI   BOXFONT,0                                                        
         LA    R2,131                                                           
         C     R2,DISPLINE                                                      
         BNL   TLOUTWD                                                          
         MVI   BOXFONT,1                                                        
         DROP  R1                                                               
*                                                                               
TLOUTWD  L     R5,AWIDEC              FOR WIDE PRINTING                         
         USING WIDED,R5                                                         
*                                                                               
         CLI   MODE,LBUYXRQ                                                     
         BE    LASTOTS                                                          
         MVI   LINEBRK,C'Y'                                                     
         MVI   LINBRKSW,C'N'                                                    
         CLI   PBUYREC,X'FF'       E-O-F                                        
         BNE   TL01                                                             
         CLI   FRSTSW,C'N'         IS IT FIRST TIME                             
         BE    TL00                                                             
         MVC   PBUYKEY,KEYSAVE                                                  
         BAS   RE,GETCLT                                                        
         MVI   OLDOAN,0          FORCE TO PRINT OTHER AGY IF RQSTDL19           
         BAS   RE,GETPRD                                                        
         BAS   RE,GETESTM                                                       
         CLI   RCMULTIQ,C'Y'                                                    
         BE    TLOXIT                                                           
TLOA     L     R1,AP1                                                           
         MVC   0(16,R1),=C'NO DATA TO PRINT'                                    
         BAS   RE,TLPRT                                                         
         B     TLOXIT                                                           
TL00     CLI   INSFLG,C'Y'         NUM OF INSERT ON                             
         BE    TL02A               YES/DO LINEBREAK/THEN GOTO TLOUT2            
         LA    R1,TEMPTOTS         IF TEMPS HAVE $                              
         CLC   1(4,R1),=4X'00'     THEN PUT TEMPS TO P LINE                     
         BNE   TL02A               THEN GOTO TLOUT2                             
         BAS   RE,TLPRT                                                         
         B     TLOUT2                                                           
TL01     MVI   LINEBRK,C'N'                                                     
         MVC   SORTREC,0(R3)                                                    
         CLI   QOPT6,C'Y'                                                       
         BNE   TLTEMP                                                           
         LA    R2,XP1                                                           
         MVC   0(8,R2),=C'SORTOUT='                                             
         GOTO1 HEXOUT,DMCB,SORTREC,9(R2),100,0                                  
         GOTO1 REPORT                                                           
         SPACE                                                                  
TLTEMP   CLI   FRSTSW,C'N'         IS IT FIRST TIME                             
         BE    TL02                                                             
*          DATA SET DOWNLOAD   AT LEVEL 042 AS OF 01/08/87                      
*                                  YES/FIRST TIME                               
*        MVI   DOWNLOAD,C'Y'       FOR TEST PURPOSES                            
         CLI   DOWNLOAD,C'Y'                                                    
         BNE   SKPDNL              IF DOWNLOAD=Y                                
         XC    BRKKEY,BRKKEY       CLEAR BREAKEY FOR DOWNLOAD                   
         MVI   FRSTPAGE,C'Y'       SET AND SEND FIRST PAGE                      
         BAS   RE,TLPRT                                                         
         MVI   FRSTPAGE,0                                                       
*                                                                               
SKPDNL   MVI   FRSTSW,C'N'         YES/FIRST TIME                               
         XC    OLDKEY,OLDKEY                                                    
         XC    OLDPUB,OLDPUB                                                    
         CLI   RCMULTIQ,C'Y'                                                    
         BNE   CLRFTOT                                                          
*****                                                                           
         CLI   DOWNLOAD,C'Y'                                                    
         BE    CLRFTOT                                                          
*****                                                                           
         MVC   PBUYKEY+4(3),QCLIENT                              BUG03          
         SPACE                                                                  
         LA    R0,PBUYREC                                        BUG03          
         ST    R0,AREC                                           BUG03          
         LA    R1,SORTREC                                        BUG03          
         AH    R1,SDISP            SHOULD STILL HAVE SORT LEN    BUG03          
         AH    R1,=H'2'            EXTRA KEY BYTES               BUG03          
         MVC   KEY+27(4),0(R1)    DISK ADDR                      BUG03          
         LR    R2,R1   SAVE R1                                   BUG03          
*                                                                BUG03          
* BUY-IO GET BUY RECORD                                                         
         GOTO1 GETPRT                                            BUG03          
         LR    R1,R2     RESTORE R1                              BUG03          
         BAS   RE,GETCLT                                         BUG03          
         BAS   RE,GETPRD                                                        
         BAS   RE,GETESTM                                                       
         L     R1,AP1                                                           
         MVC   5(10,R1),MEDNMSV                                                 
         BAS   RE,TLPRT                                                         
         L     R1,AP1                                                           
*                                                                               
PRTDASH  OC    MEDNMSV,SPACES                                                   
         LA    R3,MEDNMSV+9                                                     
         LA    R4,9                                                             
PRTDS1   CLI   0(R3),C' '                                                       
         BE    PRTDS8                                                           
         EX    R4,MVCSPCS                                                       
         B     PRTDS20                                                          
MVCSPCS  MVC   5(0,R1),=10X'BF'                                                 
PRTDS8   BCTR  R3,R0                                                            
         BCTR  R4,R0                                                            
         B     PRTDS1                                                           
PRTDS20  BAS   RE,TLPRT                                                         
         BAS   RE,TLPRT                                                         
*    ONLY CLEAR FINAL TOTALS IF QMEDIA = M  THIS MAY BE A COMBINED              
*    MEDIA REQUEST                                                              
*                                                                               
         CLI   RCMULTIQ,C'C'       SEE IF COMBINED MEDIA                        
         BE    CKFIRST                                                          
         CLI   RCMULTIQ,C'Y'       OR MEDIA "*"                                 
         BNE   XCTOTS                                                           
*                                                                               
CKFIRST  CLC   QMEDIA,FMULTMED        SEE IF FIRST MEDIA                        
         BNE   XCTOTS                                                           
****     CLI   QMEDIA,C'M'                                                      
****     BNE   XCTOTS                                                           
CLRFTOT  LA    R4,FINLTOTS                                                      
         LA    R1,20                 LOOP CLEARS FINLTOTS & TOTTOTS             
FZAP1    ZAP   0(12,R4),=P'0'                                                   
         LA    R4,12(R4)                                                        
         BCT   R1,FZAP1                                                         
XCTOTS   LA    R4,TEMPTOTS                                       BUG06          
         LA    R1,(L'TEMPTOTS/12)    GET REPITITION              BUG06          
ZIPZAPP  ZAP   0(12,R4),=P'0'                                    BUG06          
         LA    R4,12(R4)                                         BUG06          
         BCT   R1,ZIPZAPP                                        BUG06          
         XC    BILPROF,BILPROF       CLEAR PROFILE              L07             
         XC    BFORMD,BFORMD                                    L07             
         MVI   XSCR,0    INITIALIZE PRINT BILLING FORMULA SW    L07             
         XC    INSCNT,INSCNT                                                    
         XC    INSTOT,INSTOT                                                    
         XC    INSDISP,INSDISP                                                  
         MVI   INSFLG,0                                                         
         LA    R1,RPTTAB                                                        
TL01A    CLI   0(R1),X'56'         IS IT NUM OF INSRT CODE                      
         BE    TL01D                                                            
         CLI   0(R1),X'2E'  COST FIELD REQUESTED                 L07            
         BNE   *+8          IF SO PRINT BILLING FORMULA          L07            
         MVI   XSCR,1      FORCE FORMULA TRANSLATION                            
         CLI   0(R1),X'FF'                                                      
         BE    TL01E                                                            
         CLI   2(R1),X'FF'         IS THIS FIELD DISPLAYED                      
         BE    TL01B               NO/ SO DON'T ADD TO INSDISP                  
         ZIC   R0,2(R1)            YES/ADD TO DISP OF INSRT                     
         AH    R0,INSDISP                                                       
         STH   R0,INSDISP                                                       
         MVC   SVINSDSP,INSDISP                                                 
TL01B    LA    R1,5(R1)            INCREMENT RPTTAB                             
         B     TL01A                                                            
TL01D    MVI   INSFLG,C'Y'         SET NUM OF INS FLAG                          
         MVC   SVINSFLG,INSFLG                                                  
TL01E    CLI   CMNTSW,C'Y'         IF THERE ARE COMMENTS                        
         BNE   TL01F               SET TO PRINT EACH LINE                       
         MVI   LINEBRK,C'Y'                                                     
TL01F    DS    0H                                                 L14           
         BAS   RE,DRDBRK       SET OLDDIV/R/D FOR COLS (FROM RPTTAB)            
         MVC   SRTRCSV,0(R3)       SET OLDSORTREC                               
         B     TL10                                                             
         SPACE                                                                  
TL02     DS    0H                                                               
         CLI   CMNTSW,C'Y'         IF COMMENTS, PRINT EACH LINE                 
         BNE   *+12                AND SKIP LINEBRK.                            
         MVI   LINEBRK,C'Y'                                                     
         B     TL05                                                             
         LH    R1,SDISP                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRTRCSV(0),SORTREC     IS THERE A CHANGE IN RECORDS              
         BE    TL05                NO. SKIP LINEBRK ROUTINE                     
         MVI   LINBRKSW,C'Y'                                                    
         EJECT                                                                  
* BELOW IS MAINLINE FOR LINEBRK ROUTINE (TO TL05)                               
* IT MAKES USE OF ROUTINES USED IN REQUESTED BREAKS                             
*                                                                               
         LA    R2,TEMPTOTS         ADD TEMPTOTS TO ACCUMS                       
         BAS   RE,PRT00                                                         
         L     R5,AWIDEC                                                        
TL02A    LA    R4,TEMPTOTS                                                      
TL03     CLI   0(R4),0             PUT TEMPTOTS TO P LINE                       
         BE    TL03D                                                            
         L     R3,AP1                                                           
         AH    R3,2(R4)                                                         
         CLI   0(R4),X'5A'         IS IT PLANNED COST                           
         BE    TL03A                                                            
         CLI   0(R4),X'5B'         IS IT ACTUAL LESS PLANNED                    
         BNE   TL03B                                                            
TL03A    CP    4(8,R4),=P'0'       PLANNED/ACT LESS PLAN          L01           
         BNE   TL03B                                              L01           
         CLI   DOWNLOAD,C'Y'                                                    
         BNE   TL03C                                                            
TL03B    EDIT  (P8,4(R4)),(14,0(R3)),2,COMMAS=YES,FLOAT=-         L01           
TL03C    LA    R4,12(R4)                                          L01           
         B     TL03                                                             
         SPACE                                                                  
TL03D    CLI   INSFLG,C'Y'         IS THERE A NUM OF INSRT                      
         BE    TL03DD                                                           
         CLI   PBUYREC,X'FF'       IS IT E-O-F                                  
         BNE   TL04                                                             
         BAS   RE,TLPRT                                                         
         B     TLOUT2              THEN GO TO FINAL RTNS                        
TL03DD   L     R3,AP1                                                           
         AH    R3,INSDISP                                                       
         EDIT  (B4,INSCNT),(5,0(R3)),ZERO=BLANK                                 
         XC    INSCNT,INSCNT                                                    
         CLI   PBUYREC,X'FF'       TEST E-O-F                                   
         BNE   TL04                                                             
         BAS   RE,TLPRT     YES/EOF                                             
         B     TLOUT2                                                           
TL04     MVC   SPACING,SPACEING NOT EOF                                         
         BAS   RE,TLPRT                                                         
         B     PRT23          GOTO ADD TEMP TO FINAL/CLEAR TEMPTOTS             
         EJECT                                                                  
TL05     BAS   RE,BRKCHK                                                        
         SPACE                                                                  
TL10     LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            SHOULD STILL HAVE SORT LEN                   
         AH    R1,=H'2'            EXTRA KEY BYTES                              
         MVC   KEY+27(4),0(R1)    DISK ADDR                                     
         LR    R2,R1   SAVE R1                                    L15           
*                                                                 L15           
* BUY-IO GET BUY RECORD                                                         
         GOTO1 GETPRT                                                           
         LR    R1,R2     RESTORE R1                               L15           
         MVC   PASSPROD,PBUYKEY+7  DEFAULT                        L15           
         OC    4(3,R1),4(R1)   SEE IF THERE IS A PASSIVE PROD     L15           
         BZ    *+10                                               L15           
         MVC   PASSPROD,4(R1)                                 L15               
         MVC   SAVEPRD,PBUYKEY+7      SAVE ORIGINAL KEY           L15           
         MVC   PBUYKEY+7(3),PASSPROD  SLUG IN PASSIVE PRODUCT     L15           
*                                                                 L15           
         BAS   RE,GETCLT                                                        
         BAS   RE,GETPRD                                                        
         BAS   RE,GETESTM                                                       
         CLC   OLDPUB,PBUYKPUB                                                  
         BE    *+8                                                              
         BAS   RE,GETLTLRC                                                      
         CLC   OLDCLT,PBUYKCLT                                                  
         BE    TLOUT1                                                           
         CLI   OFCLTSW,C'Y'        CHK IF OFFICE/CLIENT SPECIAL                 
         BE    TLOUT1                                                           
         CLI   RCMULTIQ,C'Y'                                                    
         BE    TLOUT1                                                           
         CLI   MASTCLI,0            MASTER CLIENT                  L21          
         BNE   TLOUT1                                              L21          
         CLC   QCLIENT,=C'ALL'      SEE IF ALL CLT REQUEST       BUG28          
         BNE   TL15                                              BUG28          
         LA    R2,RPTTAB                                         BUG28          
         CLI   0(R2),X'01'          SEE IF CLIENT IS FIRST FIELD BUG28          
         BNE   TLOUT1         NO  - THEN DON'T SKIP TO NEW PAGE  BUG28          
*                                                                               
TL15     MVI   FORCEHED,C'Y'        NO SET FORCEHED                             
         SPACE                                                                  
TLOUT1   BAS   RE,DRDBRK          CHECK DIV/REG/DST BREAK OF SORTREC            
         MVC   SRTRCSV,SORTREC                                                  
         SPACE                                                                  
*                                                                 L03           
*  MUST DETERIMINE IF REQUEST WAS TO FILTER BY PAID/BILLED DATES  L03           
*                                                                 L03           
*                                                                   L03         
         MVI   OPTIONS,0   INITIALIZE OPTION BYTE FOR GETINS   )  L03           
         CLI   QBPDATE,C'A'  FILTER ON PAID DATES                 L03           
         BNE   *+8                                                L03           
         MVI   OPTIONS,C'P'  PAID INDICATOR FOR    GETINS         L03           
         CLI   QBPDATE,C'I'  FILTER ON BILLING DATES              L03           
         BNE   *+8                                                L03           
         MVI   OPTIONS,C'B'  BILLING INDICATOR FOR    GETINS      L03           
*                                                                               
         PRINT GEN                                                              
         MVC   PBUYKEY+7(3),SAVEPRD    FORCE TO ORIGINAL PRODUCT  L15           
         MVC   GROSS,=C'CRAT'  FOR SPECIAL HANDLING OF 'C' RATES  L23           
         GOTO1 GETINS,DMCB,(OPTIONS,PBUYREC),GROSS,PASSPROD,           X        
               BSTART                                                           
         MVC   OGROSS,=C'CRAT' FOR SPECIAL HANDLING OF 'C' RATES  L23           
         GOTO1 GETINS,DMCB,(OPTIONS,PBUYREC),(C'O',OGROSS),            X        
               PASSPROD,BSTART                                    L15           
         MVC   PBUYKEY+7(3),PASSPROD    FORCE TO PASSIVE PRODUCT  L15           
         PRINT NOGEN                                                            
         TM    PBUYCNTL,X'80'                                                   
         BNO   TLOUT1A                                                          
         XC    GROSS(20),GROSS                                                  
         XC    OGROSS(20),OGROSS                                                
         DROP  R5                          DROP USING FOR WIDE PRINT            
*                                                                               
TLOUT1A  L     R5,OGROSS                                                        
         L     R4,GROSS                                                         
         SR    R5,R4                                                            
         ST    R5,DGROSS                                                        
*                                                                               
         L     R5,OAGYCOM                                                       
         L     R4,AGYCOM                                                        
         SR    R5,R4                                                            
         ST    R5,DAGYCOM                                                       
*                                                                               
         L     R5,OCSHDSC                                                       
         L     R4,CSHDSC                                                        
         SR    R5,R4                                                            
         ST    R5,DCSHDSC                                                       
*                                                                               
         L     R5,OPYABLE                                                       
         L     R4,PYABLE                                                        
         SR    R5,R4                                                            
         ST    R5,DPYABLE                                                       
*                                                                               
         L     R5,OBLABLE                                                       
         L     R4,BLABLE                                                        
         SR    R5,R4                                                            
         ST    R5,DBLABLE                                                       
*                                                                               
         L     R5,OPREMIUM                                                      
         L     R4,PREMIUM                                                       
         SR    R5,R4                                                            
         ST    R5,DPREMIUM                                                      
*                                                                               
         L     R5,OUNITS                                                        
         L     R4,UNITS                                                         
         SR    R5,R4                                                            
         ST    R5,DUNITS                                                        
*                                                                               
         L     R5,OPGROSS                                                       
         L     R4,PGROSS                                                        
         SR    R5,R4                                                            
         ST    R5,DPGROSS                                                       
*                                                                               
         L     R5,OPAGYCOM                                                      
         L     R4,PAGYCOM                                                       
         SR    R5,R4                                                            
         ST    R5,DPAGYCOM                                                      
*                                                                               
         L     R5,OPCSHDSC                                                      
         L     R4,PCSHDSC                                                       
         SR    R5,R4                                                            
         ST    R5,DPCSHDSC                                                      
*                                                                               
         L     R5,OPAID                                                         
         L     R4,PAID                                                          
         SR    R5,R4                                                            
         ST    R5,DPAID                                                         
*                                                                               
         L     R5,OTAX                                                          
         L     R4,TAX                                                           
         SR    R5,R4                                                            
         ST    R5,DTAX                                                          
*                                                                               
         L     R5,OBGROSS                                                       
         L     R4,BGROSS                                                        
         SR    R5,R4                                                            
         ST    R5,DBGROSS                                                       
*                                                                               
         L     R5,OBAGYCOM                                                      
         L     R4,BAGYCOM                                                       
         SR    R5,R4                                                            
         ST    R5,DBAGYCOM                                                      
*                                                                               
         L     R5,OBCSHDSC                                                      
         L     R4,BCSHDSC                                                       
         SR    R5,R4                                                            
         ST    R5,DBCSHDSC                                                      
*                                                                               
         L     R5,OBILLED                                                       
         L     R4,BILLED                                                        
         SR    R5,R4                                                            
         ST    R5,DBILLED                                                       
*                                                                               
*                                                                               
         TM    PBUYCNTL,X'80'                                                   
         BZ    *+10                                                             
         XC    GROSS(20),GROSS                                                  
*        CLC   QREGION,SPACES      CHK IF REG/DST SHARE CALC                    
*        BE    *+8                                                              
         GOTO1 ARDSPLIT            YES/GOTO RDSPLIT                             
         L     R5,ABYOWRK                                        L01            
         USING PPBYOUTD,R5                                                      
         LA    RF,PBUYREC                                                       
         ST    RF,PBYOINPT                                                      
         LA    RF,GROSS                                                         
         ST    RF,PBYOVALS                                                      
         MVC   PBYODTCN,DATCON                                                  
         MVI   PBYOCTL,X'24'       FOR ZZZ ALLOCATNS AND IO COMMENTS            
*                                                                               
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
*                                                                               
TLOUT2   DS    0H                                                               
         CLI   PBUYREC,X'FF'                                                    
         BE    TLOUTXX                                                          
         XC    DDISP,DDISP         DISPLACEMENT INTO P                          
*                                                                L04            
         XC    SAVEAMT(64),SAVEAMT                               L04            
         LA    R4,SAVEAMT                                        L04            
         ST    R4,SAVEAPTR                                       L04            
*                                  CHK RPTTAB TO SEE IF DISPLAY FIELD           
         LA    R4,RPTTAB                                                        
TLOUT5   CLI   0(R4),X'FF'       END OF TABLE                                   
         BE    TLOUTX                                                           
         CLI   2(R4),X'FF'                                                      
         BE    TLOUT40             NOT DISPLAYING THIS FIELD                    
         L     R5,ADISPTAB                                       L07            
TLOUT10  CLI   0(R5),X'FF'         END OF DISPLAY TABLE                         
         BNE   *+6                                                              
         DC    H'0'                FATAL ERROR                                  
*                                                                               
         CLI   0(R4),140               CANNOT ASSIGN SELECTION CODE             
         BL    TLOUT13                 #'S 140-245, ONLY TO OPEN AND            
         CLI   0(R4),245               DIFFERENCE AMOUNTS.                      
         BH    TLOUT13                                                          
         GOTO1 AOPNDIFF,DMCB,(R4)                                               
         BAS   RE,DOLDISP                                                       
         B     TLOUT40                                                          
*                                                                               
TLOUT13  CLC   0(1,R4),0(R5)                                                    
         BE    TLOUT15                                                          
         LA    R5,5(R5)                                                         
         B     TLOUT10             NEXT ENTRY                                   
*                                                                               
TLOUT15  DS    0H                                                               
TLOUT20  MVC   FULL,1(R5)                                                       
         L     RF,FULL                                                          
         BAS   RE,0(RF)                                                         
*                                                                               
TLOUT40  DS    0H                                                               
         LA    R4,5(R4)            NEXT ENTRY IN RPTTAB                         
         B     TLOUT5                                                           
*                                                                               
TLOUTX   DS    0H                                                               
         L     R2,AWIDEC              TO ACCOMODATE WIDE PRINTING               
         USING WIDED,R2                                                         
*                                                                               
         MVC   OLDPUB,PBUYKPUB                                                  
         MVC   OLDPRD,PBUYKPRD                                                  
         MVC   OLDCLT,PBUYKCLT                                                  
*        MVC   OLDJOB,PBDJOB                                                    
         MVC   OLDEST,PBUYKEST                                                  
         B     TLOUTX8                                                          
*                                                                               
*                                                                               
TLOUTX8  MVC   SPACING,SPACEING    X'02'                                        
         CLI   LINEBRK,C'Y'        DO NOT PRINT LINE                            
         BNE   TLOXIT              EXCEPT AT LINEBRK TIME                       
         BAS   RE,TLPRT                                                         
*****                                                                           
         CLI   EXTRAPRT,C'Y'                                                    
         BNE   TLOXIT                                                           
         L     RF,AP1                                                           
         MVC   0(198,RF),MYPRINT                                                
         BAS   RE,TLPRT                                                         
*****                                                                           
         B     TLOXIT                                                           
TLOUTXX  DS    0H                                                               
         CLI   PBUYREC,X'FF'       LAST RECORD                                  
         BNE   TLOXIT                                                           
         B     TLTXX                                                            
*****          PREVIOUS BRANCH TO CORRECT QOPT1 CONFLICT WITH ADCODE            
*                                                                               
*****                                                                           
         CLI   EXTRAPRT,C'Y'                                                    
         BNE   TLTXX                                                            
         L     RF,AP1                                                           
         MVC   0(198,RF),MYPRINT                                                
         BAS   RE,TLPRT                                                         
*****                                                                           
*          DATA SET DOWNLOAD   AT LEVEL 042 AS OF 01/08/87                      
TLTXX    DS    0H                                                               
         CLI   DOWNLOAD,C'Y'                                                    
         BNE   TLTXXA                                                           
         MVI   ENDRPRT,C'Y'        FOR DOWNLOAD                                 
         GOTO1 =A(DOWN),DMCB                                                    
         MVI   ENDRPRT,C'0'        CLEAR FOR MULTIPLE REQS                      
         B     TLOXIT                                                           
TLTXXA   LA    R1,BRKKEY           ARE THERE ANY BREAK CHECKS                   
         CLI   0(R1),0                                                          
         BE    TLTXXX                                                           
         MVC   SRTRCSV,TLTXXA      YES. SO FORCE GARBAGE IN SRTRSV              
         BAS   RE,BRKCHK             FORCE BRK PRINT OF ALL BRKS.               
         B     LASTOTS                 DO RUN TOTALS.                           
TLTXXX   MVI   BRKSW,C'N'          NO. SO SKIP BRK CHECK AND                    
         BAS   RE,PRT23              GO DIRECT TO ADD OF TEMP TO FINL.          
         B     LASTOTS                 DO RUN TOTALS.                           
TLOXIT   XIT1                                                                   
         DROP  R2                                                               
PASSPROD DS    CL3                                                L15           
SAVEPRD  DS    CL3                                                L15           
*                                                                 L15           
         EJECT                                                                  
*                                                                               
DP0A     DS    0H                  CLIENT                                       
         L     R5,AP1                                                           
         LH    R0,DDISP                                                         
         AR    R5,R0                                                            
*                                                                               
         B     DP0A1                                                            
*                                                                               
**DP     CLC   QAGENCY,=C'DP'                 FOR DUPONT.                       
**DP     BNE   DP0A1                                                            
**DP     MVC   0(2,R5),PBUYKAGY                                                 
**DP     B     *+10                                                             
*                                                                               
DP0A1    MVC   0(3,R5),PBUYKCLT                                                 
         LH    R0,DDISP                                                         
         ZIC   R3,2(R4)                                                         
         AR    R0,R3                                                            
         STH   R0,DDISP                                                         
         BR    RE                                                               
*                                                                               
DP0B     DS    0H                  REGION CARRIED IN SORTREC                    
         L     R5,AP1                                                           
         LH    R0,DDISP                                                         
         AR    R5,R0                                                            
         LA    R1,SORTREC                                                       
         AH    R1,ADSPRD                                                        
         MVC   0(3,R5),3(R1)       DIV/REG/DIST/SHR/SUM                         
         LH    R0,DDISP             3   3   3    1   1                          
         ZIC   R3,2(R4)                                                         
         AR    R0,R3                                                            
         STH   R0,DDISP                                                         
         BR    RE                                                               
*                                                                               
DP0C     DS    0H                  DISTRICT CARRIED IN SORTREC                  
         L     R5,AP1                                                           
         LH    R0,DDISP                                                         
         AR    R5,R0                                                            
         LA    R1,SORTREC                                                       
         AH    R1,ADSPRD                                                        
         MVC   0(3,R5),0(R1)       DIV/REG/DIST/SHR/SUM          BUG29          
         B     DP09WW               3   3   3    1   1                          
*                                                                               
DP0D     DS    0H                  REGION NAME                                  
         ST    RE,DP09-4                                                        
         L     R5,AP1                                                           
         LH    R0,DDISP                                                         
         AR    R5,R0                                                            
         BAS   RE,OTGTREG                                                       
         CLC   REGNMSV,PREGNAME                                                 
         BNE   DP0D1                                                            
         BAS   RE,TLCKHD                                                        
         CLI   FORCEHED,C'Y'                                                    
         BE    DP0D1                                                            
         CLC   REGNMSV,SPACES                                                   
         BNH   DP0D2                                                            
         MVC   8(2,R5),=C''''''                                                 
         B     DP0D2                                                            
DP0D1    MVC   0(20,R5),PREGNAME                                                
DP0D2    B     DP09W                                                            
*                                                                               
DP0E     DS    0H                  DISTRICT NAME                                
         ST    RE,DP09-4                                                        
         L     R5,AP1                                                           
         LH    R0,DDISP                                                         
         AR    R5,R0                                                            
         BAS   RE,OTGTDST                                                       
         CLC   DISTNMSV,PDSTNAME                                                
         BNE   DP0E1                                                            
         BAS   RE,TLCKHD                                                        
         CLI   FORCEHED,C'Y'                                                    
         BE    DP0E1                                                            
         CLC   DISTNMSV,SPACES                                                  
         BNH   DP0E2                                                            
         MVC   8(2,R5),=C''''''                                                 
         B     DP0E2                                                            
DP0E1    MVC   0(20,R5),PDSTNAME                                                
DP0E2    B     DP09W                                                            
         DS    F                                                                
DP02     DS    0H                                                               
         ST    RE,DP02-4                                                        
         LA    R3,1(RC)                                                         
         LA    R3,4095(R3)                                                      
         USING PPFILED+4096,R3                                                  
         CLC   OLDPUB,PBUYKPUB                                                  
         BNE   DP02B                                                            
SAMEPUB  DS    0H                                               L06             
         BAS   RE,TLCKHD                                                        
         CLI   FORCEHED,C'Y'                                                    
         BE    DP02D5                                                           
         L     R1,AP1         IF PUBNAME ON P LINE, DO NOT USE DITTOS           
         AH    R1,DDISP          UNTIL AFTER A LINEBREAK                        
         CLC   0(20,R1),PBNAM1                                                  
         BE    DP02HX                                                           
         MVC   PBNAM1(61),SPACES                                                
         MVC   PBNAM1+4(2),=C''''''     DITTOS                                  
         MVI   PBNAM1,0                                                         
         B     DP02H                                                            
*                                                                               
DP02B    DS    0H                  GET PUBNAME                                  
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),PBUYKMED     FOR QMEDIA=C                                 
         MVC   KEY+1(6),PBUYKPUB                                                
*                                                                               
         MVC   KEY+7(2),PBUYKAGY                                                
         MVI   KEY+9,X'81'                                                      
         CLC   PUBKEY(07),KEY                                                   
         BE    DP02D3              ALREADY HAVE RECORD                          
* PUB-IO READ PUBREC DIR                                                        
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    DP02D                                                            
         CLI   PAGYPROF+16,C'0'    TEST SRDS DEFAULT                            
         BNE   *+8                                                              
         BAS   RE,NOPUB  BOMB                                                   
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+8                                                              
         BAS   RE,NOPUB  BOMB                                                   
         MVC   KEY+7(2),=C'ZZ'                                                  
         MVI   KEY+9,X'81'                                                      
         CLC   KEY(25),KEYSAVE                                                  
         BE    DP02D               HAVE SRDS                                    
* PUB-IO  READ PUB RECORD                                                       
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
NOPUB    DC    H'0'                PUB NOT FOUND                                
*                                                                               
DP02D    DS    0H                                                               
         BAS   RE,GETP                                                          
DP02D3   DS    0H                                                               
         MVC   KEY(64),SAVKEYS                                                  
*                                                                               
         B     DP02D3X                                                          
*                                                                               
**     TEST TO SEE IF DUPONT AGENCY                             L06             
**DP     CLC   QAGENCY,=C'DP'   DUPONT                          L06             
**DP     BNE   DP02D3X                                          L06             
**  SEE IF DUPONT CONTROL IS =                                  L06             
**DP     CLC   DPUBCTRL,PBUYKPUB  IF EQUAL NO CHANGE            L06             
**DP     BE    SAMEPUB                                          L06             
**DP     LA   RE,NOPUBLKU   ADD CLIENT TO LIST                    L06           
**DNOPUB CLC  0(2,RE),PUBKAGY    IF EQUAL TREAT AS IF CLI BD      L06           
**DP     BE   MOVECNT                                             L06           
**DP     CLI  0(RE),0                                             L06           
**DP     BE   BDDNOP                                              L06           
**DP     CLI  0(RE),255                                           L06           
**DP     BE   BDDNOP                                              L06           
**DP     LA   RE,3(RE)                                            L06           
**DP     B    BDDNOPUB                                            L06           
**DNOP   DS   0H                                                  L06           
**DP     CLC   PUBKAGY,=C'BD'   BBD & O PUB                     L06             
**DP     BNE   CONVERTP        NO NEED TO CONVERT               L06             
**VECNT  DS   0H                                                  L06           
**DP     MVC   DPUBCTRL,PBUYKPUB   MOVE CONTROL PUB             L06             
**DP     B     DP02D3X                                          L06             
**NVERTP LA    R2,PUBREC+33    POINT TO FIRST ELEMENT           L06             
**DP     MVI   ELCODE,X'14'    LINK BACK TO BBD&O                L06            
**AIN3   BAS   RE,NEXTEL       FIND FIRST                        L06            
**DP     BNE   NOPUB        **CRASH**                           L06             
**DP                                                             L06            
**DP     CLC   2(3,R2),=C'DP ' ELEMENT LINKING PUBS WILL HAVE    L06            
**DP     BNE   AGAIN3          DP PRESENT                        L06            
**DP     CLC   DPUBCTRL,17(R2)     WAS THERE A CHANGE IN PUB NUM L06            
**DP     BE    SAMEPUB     EVEN THO POSSIBLE AGENCY CHANGE       L06            
**DP     MVC   DPUBCTRL,17(R2)  SAVE CONTROL USING BBDO PUB#     L06            
*                                                                               
DP02D3X  DS    0H                                                L06            
DP02D5   DS    0H                                                               
         MVC   PBNAM1(61),SPACES                                                
         MVC   PBNAM1(40),PUBNAME       NAME & ZONE NAME                        
         CLI   PBUYKMED,C'N'           FOR QMEDIA=C                             
         BNE   DP02F                                                            
*                                                                               
         LA    RF,PBNAM2                                                        
         CLI   PBNAM2,C' '                                                      
         BNH   *+8                                                              
         LA    RF,PBNAM3                                                        
         MVC   0(16,RF),PUBCITY                                                 
         LA    RF,16(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C','                                                       
         MVC   3(2,RF),PUBSTATE                                                 
         DROP  R3                                                               
*                                                                               
DP02F    DS    0H                                                               
         OC    PBNAM1(60),SPACES                                                
DP02H    DS    0H                                                               
         LA    RF,1                                                             
         CLI   PBNAM2,C' '                                                      
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         CLI   PBNAM3,C' '                                                      
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         CLC   BYTE,LINENEED                                                    
         BNH   *+10                                                             
         MVC   LINENEED,BYTE                                                    
         L     R1,AP1                                                           
         AH    R1,DDISP                                                         
         MVC   0(20,R1),PBNAM1         P1                                       
         MVC   198(20,R1),PBNAM2       P2                                       
         MVC   396(20,R1),PBNAM3       P3                                       
DP02HX   LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
DP02X    L     RE,DP02-4                                                        
         BR    RE                                                               
*                                                                               
DP03     DS    0H                  PRODUCT                                      
         ST    RE,DP09-4                                                        
         L     R1,AP1                                                           
         AH    R1,DDISP                                                         
         CLC   OLDPRD,PBUYKPRD                                                  
         BNE   DP03B                                                            
         BAS   RE,TLCKHD                                                        
         CLI   FORCEHED,C'Y'                                                    
         BE    DP03B                                                            
         OC    0(3,R1),=3X'40'                                                  
         CLC   0(3,R1),=3X'40'                                                  
         BNE   DP03C                                                            
         MVC   0(2,R1),=C''''''                                                 
         B     DP03C                                                            
DP03B    MVC   0(3,R1),PBUYKPRD                                                 
DP03C    B     DP09W                                                            
*                                                                               
DP04     DS    0H                                                               
         LA    R2,PBDCDATE         CLOSING DATE                                 
         CLI   CLSDTSW,C'Y'                                                     
         BNE   DDATE                                                            
         MVI   DTSW,5                                                           
         B     DDATE                                                            
*                                                                               
DP05     DS    0H                  INSERTION DATE                               
*      WEEKLY SUMMARY TEST                                       L10            
        OC     WEEK,WEEK                                         L10            
        BZ     NOWEEK                                            L10            
        ST     R1,DBL      SAVE                                  L10            
        ST     RE,DP09-4   RETURN                                L10            
        LA     R2,SORTREC                                        L10            
        ZIC    RF,WEKPTR+1  DISPLACEMENT INTO SORT REC          L10             
        AR     R2,RF       NOW POINTS TO WEEK NUMBER             L10            
        ZIC    RF,0(R2)    CALCULATE DISPLACEMENT INTO WEEK      L10            
        BCTR   RF,0        TABLE                                 L10            
        SLL    RF,2        MULT X 4                              L10            
        L      RE,AWEEK                                          L10            
        AR     RF,RE       POINT TO ELEMENT                      L10            
        L      R3,AP1      PRINT LINE                            L10            
        AH     R3,DDISP    PROPER POSITION IN PRINT LINE         L10            
*       GOTO1 DTCNV,DMCB,(2,(RF)),(3,1(R3))                      L10            
        GOTO1 DATCON,DMCB,(2,(RF)),(5,1(R3))                     L10            
******  MVC    0(3,R3),=C'WO-'                                   L10            
        B      DP09W                                             L19            
*                                                                L10            
NOWEEK  DS     0H                                                L10            
         CLI   INSDTSW,C'Y'                                                     
         BNE   DP05A                                                            
         LA    R2,PBUYKDAT                                                      
         MVI   DTSW,5                                                           
         B     DDATE                                                            
DP05A    L     R5,ABYOWRK                                                       
         L     R3,AP1                                                           
         AH    R3,DDISP                                                         
         MVC   0(11,R3),PBYOMDY                                                 
         MVC   BYTE,PBDBFD                                                      
         CLI   BYTE,C'B'                                                        
         BE    DP05C                                                            
         CLI   BYTE,C'W'                                                        
         BE    DP05C                                                            
         MVI   BYTE,C' '                                                        
DP05C    TM    PBUYCNTL,X'80'                                                   
         BZ    *+8                                                              
         MVI   BYTE,C'D'                                                        
*                                                                               
         LA    RF,8(R3)                                                         
         CLI   0(RF),C' '                                                       
         BE    *+8                                                              
         LA    RF,11(R3)                                                        
         MVC   0(1,RF),BYTE                                                     
*****                                                                           
         CLI   PBDBFD,C'T'           IS IT A TEST BUY?                          
         BNE   DP05D                                                            
         MVI   0(R3),C'T'            IF YES MOVE A 'T' IN BEFORE                
         XC    1(10,R3),1(R3)        INSERTION DATE, AND OMIT LINE NO.          
         CLI   PBYOMDY+3,C'/'                                                   
         BNE   DP05C5                                                           
         MVC   2(6,R3),PBYOMDY                                                  
         B     DP05D                                                            
DP05C5   MVC   2(8,R3),PBYOMDY       WHEN PUTTING OUT DATE                      
*****                                                                           
DP05D    MVC   199(8,R3),PBYOMDY2    PSECOND                                    
         CLI   PBYOMDY2,C' '       SEE IF I HAVE SECOND DATE                    
         BNH   DP05E                                                            
         MVI   198(R3),C'+'                                                     
         CLI   LINENEED,2                                                       
         BH    DP05E                                                            
         MVI   LINENEED,2                                                       
DP05E    B     DP09WW                                                           
*                                                                               
DP06     DS    0H                                                               
         LA    R2,PBDSDATE         ON SALE DATE                                 
         CLI   ONSLDTSW,C'Y'                                                    
         BNE   DDATE                                                            
         MVI   DTSW,5                                                           
         B     DDATE                                                            
*                                                                               
         DS    F                                                                
DDATE    ST    RE,DP09-4                                                        
         L     R3,AP1                                                           
         AH    R3,DDISP                                                         
         OC    0(3,R2),0(R2)      SEE IF I HAVE A DATE                          
         BZ    DDATE5             (IF IT'S SATURDAY NIGHT,PROBABLY NOT)         
         CLI   DTSW,5             IF DTSW=5,M TOTALS                            
         BNE   DDATE4             SO DO MMM/YY                                  
*        GOTO1 DTCNV,DMCB,(1,0(R2)),(5,0(R3))                                   
         GOTO1 DATCON,DMCB,(3,0(R2)),(9,0(R3))                                  
         MVI   DTSW,0                                                           
         B     DDATE5                                                           
*DDATE4   GOTO1 DTCNV,DMCB,(1,0(R2)),(3,0(R3))                                  
DDATE4   GOTO1 DATCON,DMCB,(3,0(R2)),(5,0(R3))                                  
DDATE5   B     DP09W                                                            
*                                                                               
*                                                                               
         DS    F                                                                
DP08     DS    0H                  PRODUCT NAME                                 
         ST    RE,DP09-4                                                        
         CLC   OLDPRD,PBUYKPRD                                                  
         BNE   DP08B                                                            
         BAS   RE,TLCKHD                                                        
         CLI   FORCEHED,C'Y'                                                    
         BE    DP08S                                                            
         L     R3,AP1                                                           
         AH    R3,DDISP                                                         
         CLC   0(20,R3),PPRDNAME   IF PRODNAME ON P LINE                        
         BE    DP08W                  DO NOT USE DITTOS .                       
         MVC   4(2,R3),=C''''''    DITTOS                                       
         B     DP08W                                                            
*                                                                               
DP08B    MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
*                                                                               
         MVC   KEY(2),PBUYKAGY                                                  
***      MVC   KEY+2(1),QMEDIA                                                  
***      MVC   KEY+2(1),PAGYKMED                                                
         MVC   KEY+2(1),PBUYKMED   FOR QMEDIA=C                                 
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
         CLC   PPRDREC(17),KEY                                                  
         BE    DP08C               HAVE PRODUCT                                 
* BUY-IO READ PROD DIR                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                PRD NOT ON FILE                              
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
* BUY-IO READ PROD REC                                                          
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
DP08C    MVC   KEY(64),SAVKEYS        RESTORE KEYS                              
* BUY-IO READ RESTORE POINTERS IN D/M                                           
         GOTO1 HIGH                RESTORE FOR SEQ READ                         
DP08S    L     R3,AP1                                                           
         AH    R3,DDISP                                                         
         MVC   0(20,R3),PPRDNAME                                                
DP08W    B     DP09W                                                            
*                                                                               
*                                                                               
         DS    F                                                                
DP09     DS    0H                                                               
         ST    RE,DP09-4                                                        
*                                                                 L16           
*                                                                               
         LA    R3,1(RC)                                                         
         LA    R3,4095(R3)                                                      
         USING PPFILED+4096,R3                                                  
*                                                                               
*                                                                 L16           
         LR    R2,R1      SAVE REGISTER 1                         L16           
         GOTO1 =A(COMCAPT),DMCB,(C'C',=C'COPY')                   L16           
         LR    R1,R2      RESTORE REGISTER 1                      L16           
         L     RE,DMCB+4     COMCAPT RETURNS ADDRESS OF MESSAGE   L16           
         OC    0(12,RE),0(RE)    ANY OVERRIDES                    L16           
         BZ    DT99USE                                            L16           
         L     R1,AP1                                            L16            
         AH    R1,DDISP                                          L16            
         MVC   0(17,R1),0(RE)   MOVE COPY    TO SORTKEY          L16            
*                                                                 L16           
         B     DP09CM                                             L16           
*                                                                 L16           
DT99USE  DS    0H                                                 L16           
         OC    PBDJOB,PBDJOB                                                    
         BZ    DP09W               NO JOB NUMBER                                
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),PBUYKAGY                                                  
         MVC   KEY+2(1),PBUYKMED   FOR QMEDIA=C                                 
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
* THIS MAY BE A PASSIVE PRODUCT// IF SO USE ZZZ FOR KEY           BUG07         
         CLC   SAVEPRD,PASSPROD                                   BUG07         
         BE    *+10                                               BUG07         
         MVC   KEY+7(3),SAVEPRD        ZZZ PRODUCT                BUG07         
         MVC   KEY+10(6),PBDJOB                                                 
         CLC   PJOBREC(17),KEY                                                  
         BE    DP09C               HAVE JOB REC                                 
* BUY-IO JOB DIR                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JOB NOT ON FILE                              
         LA    R0,PJOBREC                                                       
         ST    R0,AREC                                                          
* BUY-IO JOB REC                                                                
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
DP09C    MVC   KEY(64),SAVKEYS        RESTORE KEYS                              
* BUY-IO RESTORE POINTERS IN D/M                                                
         GOTO1 HIGH                RESTORE FOR SEQ READ                         
         L     RE,AP1                                                           
         AH    RE,DDISP                                                         
         MVC   0(17,RE),PJOBCPY                                                 
DP09CM   DS    0H                                                 L16           
DP09W    L     RE,DP09-4                                                        
DP09WW   LH    R0,DDISP                                                         
         DROP  R3                                                 L16           
*                                                                 L16           
         ZIC   R3,2(R4)            DISP LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         BR    RE                                                               
*                                                                               
DP10     DS    0H                  DIVISION CODE IN SORT REC                    
         ST    RE,DP09-4                                                        
         LA    R5,SORTREC                                                       
         AH    R5,DIVSD            DISPLACEMENT TO DIV                          
         TM    2(R4),X'80'         SEE IF IN HEADLINES                          
         BNZ   DP10D                                                            
         L     R1,AP1                                                           
         AH    R1,DDISP                                                         
         MVC   0(3,R1),0(R5)                                                    
         B     DP09W                                                            
*                                                                               
DP10D    DS    0H                                                               
         CLC   OLDDIV,0(R5)        SEE IF NEW DIVISION                          
         BE    DP10XX                                                           
DP10E    MVC   SAVKEYS,KEY                                                      
         MVC   OLDDIV,0(R5)                                                     
         MVI   FORCEHED,C'Y'                                                    
         XC    KEY,KEY                                                          
*                                                                               
         MVC   KEY(2),PBUYKAGY                                                  
***      MVC   KEY+2(1),QMEDIA                                                  
***      MVC   KEY+2(1),PAGYKMED                                                
         MVC   KEY+2(1),PBUYKMED   FOR QMEDIA=C                                 
         MVI   KEY+3,X'03'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),0(R5)         DIVISION                                  
         CLC   PDIVREC(17),KEY                                                  
         BE    DP10F               HAVE DIVISION                                
* BUY-IO DIVISION REC DIR                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                PRD NOT ON FILE                              
         LA    R0,PDIVREC                                                       
         ST    R0,AREC                                                          
* BUY-IO DIVISION RECORD                                                        
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
DP10F    MVC   KEY(64),SAVKEYS        RESTORE KEYS                              
* BUY-IO RESTORE D/M POINTERS                                                   
         GOTO1 HIGH                RESTORE FOR SEQ READ                         
*                                                                               
DP10XX   L     RE,DP09-4                                                        
         BR    RE                                                               
*                                                                               
DP11     DS    0H                  ESTIMATE                                     
         ST    RE,DP09-4                                                        
         L     R1,AP1                                                           
         AH    R1,DDISP                                                         
         CLC   OLDEST,PBUYKEST                                                  
         BNE   DP11B                                                            
         BAS   RE,TLCKHD                                                        
         CLI   FORCEHED,C'Y'                                                    
         BE    DP11B                                                            
         OC    0(3,R1),=3X'40'                                                  
         CLC   0(3,R1),=3X'40'                                                  
         BNE   DP11D                                                            
         MVC   0(2,R1),=C''''''                                                 
         B     DP11D                                                            
DP11B    MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R1),DUB                                                      
DP11D    B     DP09W                                                            
*                                                                               
DP12     DS    0H                                                               
         LA    R2,PBDPDATE         PAYABLE DATE                                 
         CLI   PAYDTSW,C'Y'                                                     
         BNE   DDATE                                                            
         MVI   DTSW,5                                                           
         B     DDATE                                                            
*                                                                               
DP13     DS    0H                                                               
         LA    R2,PBDBDATE         BILLABLE DATE                                
         CLI   BILDTSW,C'Y'                                                     
         BNE   DDATE                                                            
         MVI   DTSW,5                                                           
         B     DDATE                                                            
*                                                                               
DP14     DS    0H                  AD CODE                                      
         ST    RE,DP09-4                                                        
         L     R1,AP1                                                           
         AH    R1,DDISP                                                         
         CLC   OLDJOB,PBDJOB                                                    
         BNE   DP14D                                                            
         CLI   DIFFMED,1           CHANGE OF MEDIA               BUG30          
         BNE   NOMCHA                                            BUG30          
         MVI   DIFFMED,0           RESET FLAG                    BUG30          
         B     DP14D                                             BUG30          
NOMCHA   BAS   RE,TLCKHD                                                        
         CLI   FORCEHED,C'Y'                                                    
         BE    DP14D                                                            
         CLC   OLDJOB,SPACES                                                    
         BNH   DP14F                                                            
         CLI   0(R1),C'A'                                        BUG30          
         BNL   DP14D                                             BUG30          
         MVC   2(2,R1),=C''''''                                                 
         B     DP14F                                                            
DP14D    MVC   0(6,R1),PBDJOB                                                   
DP14F    B     DP09W                                                            
*                                                                               
DP15     DS    0H                                                               
         LA    R2,PBDMDATE         MAT CLOSING DATE                             
         CLI   MATDTSW,C'Y'                                                     
         BNE   DDATE                                                            
         MVI   DTSW,5                                                           
         B     DDATE                                                            
*                                                                               
DP16     ST    RE,DP09-4                                           L19          
         L     R1,AP1                                              L19          
         AH    R1,DDISP                                            L19          
         CLC   OLDOAN,PPRDOAN                                      L19          
         BNE   MOVEOAN                                             L19          
         MVC   0(2,R1),=C''''''                                    L19          
         B     DP14F                                               L19          
*                                                                  L19          
MOVEOAN  MVC   OLDOAN,PPRDOAN                                      L19          
         MVC   0(2,R1),PPRDOAN                                     L19          
         B     DP14F                                               L19          
*                                                                  L19          
*                                                                  L19          
DP17     DS    0H              OTHER AGENCY NAME                   L19          
         ST    RE,DP09-4                                           L19          
         L     R1,AP1                                              L19          
         AH    R1,DDISP                                            L19          
         CLC   OLDNAM,OANNAME                                      L19          
         BNE   MOVEOANA                                            L19          
         MVC   0(2,R1),=C''''''                                    L19          
         B     DP14F                                               L19          
MOVEOANA MVC   0(33,R1),OANNAME                                    L19          
         MVC   OLDNAM,OANNAME                                                   
         B     DP09W                                               L19          
*                                                                  L19          
DP18     DS    0H N                  MOVE NAME & CODE              L19          
         ST    RE,DP09-4                                           L19          
         L     R1,AP1                                              L19          
         AH    R1,DDISP                                            L19          
         CLC   OLDNAMC(2),PPRDOAN                                  L19          
         BNE   MOVEOANC                                            L19          
         CLC   OLDNAMC+3(33),OANNAME                               L19          
         BNE   MOVEOANC                                            L19          
         MVC   0(2,R1),=C''''''                                    L19          
         B     DP14F                                               L19          
MOVEOANC DS    0H                                                  L19          
         MVC   3(33,R1),OANNAME                                    L19          
         MVI   2(R1),C'-'                                         L19           
         MVC   00(2,R1),PPRDOAN                                    L19          
         MVC   OLDNAMC,0(R1)                                                    
         B     DP14F                                               L19          
*                                                                  L19          
*                                                                  L19          
*                                                                  L19          
*                                                                  L19          
*                                                                  L19          
         DS    F                                                                
DP19     DS    0H                  COMMENTS                                     
         MVI   BYTE,0                                                           
*****                                                                           
         MVI   EXTRAPRT,0                                                       
         MVI   MYPRINT,C' '                                                     
         MVC   MYPRINT+1(L'MYPRINT-1),MYPRINT                                   
         XC    CHINPUT,CHINPUT                                                  
*****                                                                           
         ST    RE,DP19-4                                                        
         L     R3,AP1                                                           
         AH    R3,DDISP                                                         
         LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'20'         SHOULD BE BUY ELEM                           
         BE    *+6                                                              
         DC    H'0'                                                             
******************************************************************              
         CLI   2(R4),X'30'                                                      
         BE    DP1905                                                           
         LA    RF,CHINPUT                                                       
         MVI   ELCODE,X'66'                                                     
DP1901   BAS   RE,NEXTEL                                                        
         BNE   DP1902                                                           
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         SH    R1,=H'3'                                                         
         BM    DP1901         ZERO LENGHT COMMENT ELEM                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),2(R2)                                                    
         AR    RF,R1                                                            
         LA    RF,1(RF)       TO SPACE BETWEEN ELEMS                            
         MVI   0(RF),C' '                                                       
         LA    RF,1(RF)                                                         
         B     DP1901                                                           
*                                                                               
DP1902   OC    CHINPUT,CHINPUT                                                  
         BZ    DP19X                                                            
         SR    R0,R0                                                            
         IC    R0,2(R4)                                                         
         BCTR  R0,0                                                             
         STC   R0,BYTE                                                          
         MVI   LINENEED,4                                                       
*                                                                               
         GOTO1 =V(CHOPPER),DMCB,(192,CHINPUT),(BYTE,MYPRINT),4                  
*                                                                               
         L     RF,AP1                                                           
         AH    RF,DDISP                                                         
         LA    R2,MYPRINT                                                       
         ZAP   HALF2,=P'0'                                                      
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         BCTR  R1,0                                                             
DP1903   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)                                                    
         LA    RF,198(RF)                                                       
         AR    R2,R0                                                            
         AP    HALF2,=P'1'                                                      
         CP    HALF2,=P'4'                                                      
         BE    DP19X                                                            
         B     DP1903                                                           
*                                                                               
*****                                                                           
DP1905   MVI   ELCODE,X'66'                                                     
         ZAP   HALF2,=P'0'                                                      
DP19A    BAS   RE,NEXTEL                                                        
         BNE   DP19X                                                            
         AP    HALF2,=P'1'                                                      
         ZIC   R1,1(R2)                                                         
         SH    R1,=H'2'                                                         
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
DP19E    BCTR  R1,0                  DECREMENT BY ONE FOR EXC. MOVE             
*****                                                                           
         CP    HALF2,=P'5'                                                      
         BL    DP19J                                                            
         MVI   EXTRAPRT,C'Y'                                                    
         LA    R3,MYPRINT                                                       
         AH    R3,DDISP                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),2(R2)                                                    
         MVI   LINENEED,5                                                       
         B     DP19X                                                            
*****                                                                           
DP19J    EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),2(R2)                                                    
         LA    R3,198(R3)                                                       
         ZIC   RF,BYTE                                                          
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         CLC   LINENEED,BYTE                                                    
         BNL   *+10                                                             
         MVC   LINENEED,BYTE                                                    
         B     DP19A                                                            
*                                                                               
DP19X    LH    R0,DDISP                                                         
         ZIC   R1,2(R4)                                                         
         AR    R0,R1                                                            
         STH   R0,DDISP                                                         
         L     RE,DP19-4                                                        
         BR    RE                                                               
*                                                                               
DP20     L     R2,GROSS            GROSS                                        
         MVI   ACCODE,X'28'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP21     L     R2,PYABLE           NET                          L23             
         A     R2,CSHDSC           (NET-CD +CD)                 L23             
         MVI   ACCODE,X'29'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP22     L     R2,GROSS            GR-CD                                        
         S     R2,CSHDSC                                                        
         MVI   ACCODE,X'2A'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP23     L     R2,PYABLE           NET-CD                        L23            
         MVI   ACCODE,X'2B'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP24     L     R2,AGYCOM           AGY COM                                      
         MVI   ACCODE,X'2C'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP25     L     R2,CSHDSC           CD                                           
         MVI   ACCODE,X'2D'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP26     L     R2,PGROSS         PAID  GROSS                                    
         MVI   ACCODE,X'32'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP27     L     R2,PAID           PAID  NET                        L23           
         A     R2,PCSHDSC        (NET-CD + CD)                    L23           
         MVI   ACCODE,X'33'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP28     L     R2,PGROSS         PAID  GR-CD                                    
         S     R2,PCSHDSC                                                       
         MVI   ACCODE,X'34'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP29     L     R2,PAID          PAID   NET-CD                     L23           
         MVI   ACCODE,X'35'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP30     L     R2,PAGYCOM          PAID AGY COM                                 
         MVI   ACCODE,X'36'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP31     L     R2,PCSHDSC          PAID CD                                      
         MVI   ACCODE,X'37'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP32     L     R2,BGROSS         BILLED GROSS                                   
         MVI   ACCODE,X'3C'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP33     L     R2,BILLED         BILLED NET                        L23          
         A     R2,BCSHDSC        (NET-CD + CD)                     L23          
         MVI   ACCODE,X'3D'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP34     L     R2,BGROSS         BILLED GR-CD                                   
         S     R2,BCSHDSC                                                       
         MVI   ACCODE,X'3E'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP35     L     R2,BILLED        BILLED NET-CD                    L23            
         MVI   ACCODE,X'3F'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP36     L     R2,BAGYCOM          BILLED AGY COM                               
         MVI   ACCODE,X'40'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP37     L     R2,BCSHDSC          BILLED CD                                    
         MVI   ACCODE,X'41'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP38     L     R2,GROSS            UNPAID GROSS                                 
         S     R2,PGROSS                                                        
         MVI   ACCODE,X'46'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP39     L     RF,PAID             UNPAID NET                    L23            
         A     RF,PCSHDSC          (PAID NET) = (NET-CD + CD)    L23            
         L     R2,PYABLE                                         L23            
         A     R2,CSHDSC           (NET) = (NET-CD +CD)          L23            
         SR    R2,RF                                                            
         MVI   ACCODE,X'47'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP40     L     RF,PGROSS           UNPAID GR-CD                                 
         S     RF,PCSHDSC          (PAID GR-CD)                                 
         L     R2,GROSS                                                         
         S     R2,CSHDSC           (GR-CD)                                      
         SR    R2,RF                                                            
         MVI   ACCODE,X'48'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP41     L     RF,PAID             UNPAID NET-CD                   L23          
         L     R2,PYABLE           NET-CD                          L23          
         SR    R2,RF                                                            
         MVI   ACCODE,X'49'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP42     L     R2,AGYCOM           UNPAID AGYCOM                                
         S     R2,PAGYCOM                                                       
         MVI   ACCODE,X'4A'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP43     L     R2,CSHDSC           UNPAID CD                                    
         S     R2,PCSHDSC                                                       
         MVI   ACCODE,X'4B'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP44     L     R2,GROSS            BILLABLE GROSS                               
         S     R2,BGROSS                                                        
         MVI   ACCODE,X'50'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP45     L     RF,BILLED           BILLABLE NET                    L23          
         A     RF,BCSHDSC          (BILLED NET) = (NET-CD +CD)     L23          
         L     R2,PYABLE                                           L23          
         A     R2,CSHDSC           (NET) = (NET-CD +CD)            L23          
         SR    R2,RF                                                            
         MVI   ACCODE,X'51'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP46     L     RF,BGROSS           BILLABLE GR-CD                               
         S     RF,BCSHDSC          (BILLED GR-CD)                               
         L     R2,GROSS                                                         
         S     R2,CSHDSC           (GR-CD)                                      
         SR    R2,RF                                                            
         MVI   ACCODE,X'52'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP47     L     RF,BILLED           BILLABLE NET-CD                 L23          
         L     R2,PYABLE           NET - CD                        L23          
         SR    R2,RF                                                            
         MVI   ACCODE,X'53'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP48     L     R2,AGYCOM           BILLABLE AGY COM                             
         S     R2,BAGYCOM                                                       
         MVI   ACCODE,X'54'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP49     L     R2,CSHDSC           BILLABLE CD                                  
         S     R2,BCSHDSC                                                       
         MVI   ACCODE,X'55'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
*                                                                               
DP50     DS    0H                 NUMBER OF INSERTS                             
         CLI   PBDSPACE,C'*'    TEST IF SPECIAL BUY                             
         BE    DP55            YES/DO NOT COUNT AS INSERT                       
         TM    PBUYCNTL,X'80'      IS IT DELETED BUY                            
         BNZ   DP55                IF YES/DO NOT ADD TO INSERT TOTS             
         L     R1,INSCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,INSCNT                                                        
         L     R1,INSTOT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,INSTOT                                                        
         L     R1,INSFTOT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,INSFTOT                                                       
         CLI   RCMULTIQ,C'Y'                                                    
         BNE   DP53                                                             
         L     R1,TTINSRT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,TTINSRT                                                       
DP53     L     R1,AP1                                                           
         AH    R1,DDISP                                                         
         MVI   4(R1),C'1'                                                       
DP55     LH    R0,DDISP                                                         
         ZIC   R3,2(R4)                                                         
         AR    R0,R3                                                            
         STH   R0,DDISP                                                         
         BR    RE                                                               
*                                                                  L04          
DP56     DS    0H                  DIVISION  NAME                  L20          
         ST    RE,DP09-4                                           L20          
         MVC   SAVKEYS,KEY                                        L20           
*                                                                   L20         
*   READ DIVISION RECORD                                            L20         
*                                                                   L20         
         MVC   KEY(25),PPRDREC                                  L20             
         MVI   KEY+3,3                                              L20         
         MVC   KEY+7(3),PPRDDIV      DIVISION                       L20         
         CLC   PDIVREC(25),KEY       ALREADY THERE                  L20         
         BE    DOVEDIR                                             L20          
         GOTO1 HIGH                                                 L20         
         CLC   KEY(25),KEYSAVE                                      L20         
         BE    *+14                                                 L20         
         XC    PDIVNAME,PDIVNAME                                    L20         
         B     NODIVHD                                              L20         
         LA    RE,PDIVREC                                           L20         
         ST    RE,AREC                                              L20         
         GOTO1 GETPRT                                               L20         
*                                                                   L20         
NODIVHD  MVC   KEY,SAVKEYS                                         L20          
         GOTO1 HIGH                                                 L20         
*                                                                   L20         
DOVEDIR  L     R1,AP1           POINT TO PRINT LINE FOR DIV NAME   L20          
         AH    R1,DDISP                                             L20         
*                                                                   L20         
*                                                                   L20         
         MVC   0(20,R1),PDIVNAME   EXECUTED FROM PRODUCT REC        L20         
*                                                                   L20         
         B     DP09W                                                L20         
         SPACE 3                                                    L20         
DP51     L     R3,SAVEAPTR    POINTS TO LAST USED ACCUMULATOR      L04          
         L     R2,0(R3)       LOAD COSTS                         BUG08          
         OC    MULTIPLY,MULTIPLY  IS  THERE A MULTIPLIER IN       L11           
         BZ    HDXX               FORMULA                         L11           
         CLI   ARITHOPT,C'A'      SUMS AS WELL                    L11           
         BNE   ARITHM                                             L11           
HDXX     DS    0H                                                 L11           
         SH    R3,=H'4'       ASSUME TWO COLUMNS                   L04          
         TM    1(R4),X'80'    IF ON THREE COLUMNS                  L04          
         BNO   *+8                                                 L04          
         SH    R3,=H'4'       POINT TO FIRST ACCUM FOR FOURMULA    L04          
         L     R2,0(R3)       LOAD COSTS                           L04          
         TM    1(R4),X'40'    NEGATIVE RELATIONSHIP COL 1&2        L04          
         BNO   *+12                                                L04          
         S     R2,4(R3)       SUBTRACT                             L04          
         B     *+8                                                 L04          
         A     R2,4(R3)                                            L04          
         TM   1(R4),X'80'     THREE COLUMN?                        L04          
         BNO   MVIADCOD                                            L04          
         TM    1(R4),X'20'    OPERATOR BTWN COL2& 3                L04          
         BNO   *+12                                                L04          
         S     R2,8(R3)                                            L04          
         B     *+8                                                 L04          
         A     R2,8(R3)                                            L04          
*                                                                  L04          
MVIADCOD OC    MULTIPLY,MULTIPLY  IS  THERE A MULTIPLIER IN       L11           
         BZ    NOARITHM           FORMULA                         L11           
ARITHM   CVD   R2,DBL                                             L11           
         ZAP   SAVEAMT(16),DBL                                    L11           
         MP    SAVEAMT(16),MULTIPLY                               L11           
         DP    SAVEAMT(16),=P'100000'   ALIGN DECIMAL POINT       L11           
         ZAP   DBL,SAVEAMT(12)                                    L11           
         CVB   R2,DBL                                             L11           
**     ROUNDING                                                                 
         OI    SAVEAMT+15,15      FORCE SIGN TO POSITIVE          L13           
         CP     SAVEAMT+12(4),=PL4'0050000'                                     
         BL     NOROUND                                                         
         A     R2,=F'1'                                                         
         LTR   R2,R2              SEE IF NEGATIVE                               
         BC    10,*+8             IF POS OR ZERO GO AROUND                      
         S     R2,=F'2'           ROUND NEGATIVE                                
NOROUND  DS     0H                                                              
         XC    SAVEAMT(16),SAVEAMT                                L11           
NOARITHM DS    0H                                                 L11           
         LA    R3,SAVEAMT                                         L04           
         ST    R3,SAVEAPTR   POINT TO BEGINING                     L04          
         MVI   ACCODE,93                                         L04            
         BAS   RF,ADDCUMS                                          L04          
         B     DOLDISP                                             L04          
*                                                                  L04          
*                                                                  L04          
*                                                                  L04          
*                                                                  L04          
*                                                                  L07          
*   COST TO CLIENT .... USE BILLING FORMULAE                       L07          
*                                                                  L07          
         DS    F   SAVE R14                                      L07            
DP52     DS    0H                                                  L07          
         ST    RE,DP52-4                                         L07            
        XC     BILPROF,BILPROF                                   L07            
         MVC   BFORMD,SPACES                                       L07          
*                                                                L07            
* LOOK FOR BILLING OPTIONS IN THIS SEQUENCE - 1 FOR THAT EST     L07            
*   2 FOR THAT EST PROD AAA  3 FOR THAT PROD  4- FOR PROD AAA    L07            
*   4- THEN DEFAULT                                              L07            
*                                                                L07            
         OC    BILPROF,PESTBILP   ARE THERE ESTIMATE OPTIONS     L07            
         BNZ   ESM95                                             L07            
         MVC   WORK(64),KEY       SAVE KEY FOR REREAD            L07            
         XC    KEY,KEY                                           L07            
         MVC   KEY,PESTKEY     FROM ESTIMATE                     L07            
         MVC   KEY+7(3),=C'AAA'                                  L07            
         MVC   KEYSAVE,KEY          SAVE KEY                       L07          
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                  L07          
         CLC   KEY(25),KEYSAVE                                   L07            
         BNE   ECK4PROD                                          L07            
         GOTO1 (RF),(R1),GETREC,PRTFILE,KEY+27,ALISREC,DMWORK      L07          
*                                                                  L07          
         L     RE,ALISREC                    POINT TO I/O          L07          
         LA    R3,PESTBILP-PESTREC(RE)                           L07            
         OC    BILPROF,0(R3)                                     L07            
         BNZ   ESM95                                             L07            
*  NO BILLING OPTIONS FOR EST /PROD AAA                          L07            
ECK4PROD OC    BILPROF,PPRDBILP   ANY PROD BILLING OPTIONS       L07            
         BNZ   ESM95                                             L07            
         MVC   KEY,PPRDREC                                       L07            
         MVC   KEY+7(3),=C'AAA'                                  L07            
         MVC   KEYSAVE,KEY          SAVE KEY                       L07          
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                  L07          
*                                                                               
         CLC   KEY(25),KEYSAVE     GOOD READ                       L07          
         BNE   DEFAULT                                           L07            
         GOTO1 (RF),(R1),GETREC,PRTFILE,KEY+27,ALISREC,DMWORK      L07          
*                                                                  L07          
         L     RE,ALISREC                    POINT TO I/O          L07          
         LA    R3,PPRDBILP-PPRDREC(RE)  POINT TO PRODUCT OPTION    L07          
       OC    BILPROF,0(R3)        LOAD UP BILLING OPTIONS          L07          
         BNZ   ESM95                                             L07            
DEFAULT  MVC   BILBASA(5),DFLTFORM                               L07            
         B     ESM94C                                          BUG24            
ESM95    DS    0H                                                L07            
         OC    BILBASA(5),BILBASA                              BUG24            
         BZ    DEFAULT                                         BUG24            
ESM94C   DS    0H                                                  L07          
         MVC   KEY(64),WORK             RESTORE KEY FOR SEQ        L07          
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                  L07          
*                                                                L07            
*                                                                L07            
*                                                                L07            
         MVC   WORK(4),GROSS            GROSS DOLLARS            L07            
         MVC   WORK+4(4),CSHDSC         CASH DISCOUNT            L07            
         MVC   WORK+8(4),AGYCOM         COMMISSION               L07            
         CLI   DESCR,1       WAS BILLING FORMULA REQUESTED       L07            
         BNE   NODESCR                                           L07            
         GOTO1 =V(GETCOST),DMCB,BILBASA,(DESCR,BFORMD)          L24             
         MVI   DESCR,0                                           L07            
         L     RE,DP52-4                                         L07            
         BR    RE             RETURN TO HEADLINE ROULTINE        L07            
NODESCR  DS    0H                                                L07            
*                                                                  L07          
         GOTO1 =V(GETCOST),DMCB,BILBASA,WORK,(C'C',PBUYREC)      L24            
*                                                                  L07          
         L     R2,DMCB+4   TRUE COST FOR BUY                     L07            
         MVI   ACCODE,X'2E'     POINT TO CORRECT ACCUMULATOR      L07           
         BAS   RF,ADDCUMS                                          L07          
         L     RE,DP52-4                                          L07           
         B     DOLDISP                                            L07           
*        *                                                                      
DP90     ICM   R2,15,PBDPLCOS      PLANNED COST                                 
         LTR   R2,R2                                                            
         BZ    DP90A                                                            
         MVI   ACCODE,X'5A'                                                     
         MVI   DOLSW,C'Y'                                                       
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
DP90A    MVI   DOLSW,C'N'          NO DOLLARS/SKIP PRINTING                     
         LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         BR    RE                                                               
*                                                                               
DP91     CLI   DOLSW,C'Y'          DID PLANNED COST HAVE DOLLARS                
         BNE   DP91A               IF NO / SKIP THIS ALSO.                      
         MVI   DOLSW,C'N'                                                       
         ICM   R3,15,PBDPLCOS      ACTUAL LESS PLANNED COST                     
         L     R2,GROSS                                                         
         SR    R2,R3                                                            
         MVI   ACCODE,X'5B'                                                     
         BAS   RF,ADDCUMS                                                       
         B     DOLDISP                                                          
DP91A    LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         BR    RE                                                               
*                                                                               
         DS    F                                                                
DP92     DS    0H                  ESTIMATE DESCRIPTION                         
         ST    RE,DP92-4                                                        
         L     R1,AP1                                                           
         AH    R1,DDISP                                                         
         CLC   OLDEST,PBUYKEST     IF EST OR                                    
         BNE   DP92B                                                            
         CLC   OLDPRD,PBUYKPRD     PRD CHANGE/GET NEW DESCR                     
         BNE   DP92B                                                            
         BAS   RE,TLCKHD                                                        
         CLI   FORCEHED,C'Y'                                                    
         BE    DP92B                                                            
         OC    0(3,R1),=3X'40'                                                  
         CLC   0(3,R1),=3X'40'                                                  
         BNE   DP92D                                                            
         MVC   9(2,R1),=C''''''                                                 
         B     DP92D                                                            
DP92B    MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
*                                                                               
         MVC   KEY(2),PBUYKAGY                                                  
***      MVC   KEY+2(1),QMEDIA                                                  
***      MVC   KEY+2(1),PAGYKMED                                                
         MVC   KEY+2(1),PBUYKMED   FOR QMEDIA=C                                 
         MVI   KEY+3,X'07'              ESTIMATE HEADER ID                      
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
         MVC   KEY+10(2),PBUYKEST                                               
* BUY-IO GET EST DIR                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
* BUY-IO GET EST RECORD                                                         
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC          RESET AIO                                    
         ST    R0,AREC                                                          
         MVC   KEY(64),SAVKEYS     AND KEYS FOR                                 
* BUY-IO RESTORE FOR D/M POINTERS                                               
         GOTO1 HIGH                SEQ READ                                     
         SPACE                                                                  
         L     R1,AP1                                                           
         AH    R1,DDISP                                                         
         MVC   0(L'PESTNAME,R1),PESTNAME                                        
         CLI   PESTNAM2,0                                                       
         BE    *+14                                                             
         LA    R1,198(R1)                                                       
         MVC   0(L'PESTNAM2,R1),PESTNAM2                                        
         SPACE                                                                  
DP92D    LH    R0,DDISP                                                         
         ZIC   R1,2(R4)                                                         
         AR    R0,R1                                                            
         STH   R0,DDISP                                                         
         SPACE                                                                  
         L     RE,DP92-4                                                        
         BR    RE                                                               
*                                                                               
         DS    F                                                                
DP93     DS    0H                       DISPLAY ASPO NUMBER                     
         ST    RE,DP93-4                                                        
         L     R5,AP1                                                           
         LH    R0,DDISP                                                         
         AR    R5,R0                                                            
*                                                                               
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         MVC   SAVKEYS,KEY                                                      
         CLC   PBUYREC(2),=C'BD'                DIFFEWREFOR BD                  
         BNE   DP93I                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+10(2),PBUYKEY+19                                             
         CLC   PESTKEY(13),KEY              GET ASPO NO FROM EST REC.           
         BE    DP93D                                                            
* BUY-IO GET EST DIR                                                            
         GOTO1 READ                                                             
* BUY-IO GET EST REC                                                            
         GOTO1 GETPRT                                                           
DP93D    CLC   PESTNAME(2),=C'**'                                               
         BNE   DP93E                                                            
         MVC   WORK(9),PESTNAME+2                                               
         CLI   WORK+8,C'*'                                                      
         BNE   DP93R                                                            
         MVI   WORK+8,C' '                                                      
         B     DP93R                                                            
*                                                                               
DP93E    MVC   WORK(2),PBUYKAGY                                                 
         MVI   WORK+1,C'S'         SET                                          
         MVI   WORK+2,C'-'                                                      
         MVC   WORK+3(6),PBDJOB                                                 
         B     DP93R                                                            
*                                                                               
DP93I    MVC   WORK+20(L'KEY),KEY      SAVE KEY                                 
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+10(2),PBUYKEY+19                                             
         CLC   PESTKEY(13),KEY              GET ASPO NO FROM EST REC.           
         BE    DP93M                                                            
* BUY-IO GET EST DIR                                                            
         GOTO1 READ                                                             
* BUY-IO GET EST REC                                                            
         GOTO1 GETPRT                                                           
DP93M    MVC   WORK(9),PESTNAME       ASPO NO FIRST 9 CHARS OF EST NAME         
*                                                                               
DP93R    MVC   0(9,R5),WORK             EXECUTED INSTRUCTION                    
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         MVC   KEY(64),SAVKEYS                                                  
* BUY-IO GET BUY DIR                                                            
         GOTO1 HIGH                                                             
*                                                                               
         LH    R0,DDISP                                                         
         ZIC   R3,2(R4)                                                         
         AR    R0,R3                                                            
         STH   R0,DDISP                                                         
         L     RE,DP93-4                                                        
         BR    RE                                                               
*                                                                               
DP94     DS    0H                      ROUTINE TO PUT OUT LIST CODE             
         L     R1,AP1                                                           
         AH    R1,DDISP                                                         
         MVC   0(3,R1),PBDLIST                                                  
         LH    R0,DDISP                                                         
         ZIC   R3,2(R4)                                                         
         AR    R0,R3                                                            
         STH   R0,DDISP                                                         
         BR    RE                                                               
*                                                                               
         DS    F                                                                
DP95     DS    0H                      ROUTINE TO PUT OUT LAST I/O              
         ST    RE,DP95-4                                                        
         XC    SV70ELM,SV70ELM                                                  
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'70'                                                     
DP95B    BAS   RE,NEXTEL                                                        
         BNE   DP95G                                                            
         OC    2(3,R2),2(R2)                                                    
         BZ    DP95B                                                            
         CLC   SV70ELM+2(3),2(R2)                                               
         BNL   DP95B                                                            
         MVC   SV70ELM(11),0(R2)                                                
         B     DP95B                                                            
*                                                                               
DP95G    OC    SV70ELM,SV70ELM                                                  
         BZ    DP95H                                                            
*                                                                               
         L     R3,AP1                                                           
         AH    R3,DDISP                                                         
*        GOTO1 DTCNV,DMCB,(1,SV70ELM+2),(0,1(R3))                               
         GOTO1 DATCON,DMCB,(3,SV70ELM+2),(0,1(R3))                              
         MVC   0(1,R3),PBUYREC+2                                                
         MVI   1(R3),C'-'                                                       
         MVI   7(R3),C'-'                                                       
         MVC   HALF,SV70ELM+5                                                   
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R3),DUB                                                      
DP95H    LH    R0,DDISP                                                         
         ZIC   R3,2(R4)                                                         
         AR    R0,R3                                                            
         STH   R0,DDISP                                                         
*                                                                               
DP95X    L     RE,DP95-4                                                        
         BR    RE                                                               
*                                                                               
DP96     DS    0H                 ROUTINE FOR TAX  -  GOES TO ADDCUMS           
         L     R2,TAX             TO ACCUMULATE TAX FOR TOTAL   -  GOES         
         MVI   ACCODE,X'5C'       TO DOLDISP TO PUT TAX DOLLARS TO              
         BAS   RF,ADDCUMS         PRINT LINE                                    
         B     DOLDISP                                                          
*                                                                               
         DS    F                                                                
DP97     ST    RE,DP97-4                                                        
*                                                                 L16           
*                                                                 L16           
         LR    R2,R1      SAVE REGISTER 1                         L16           
         GOTO1 =A(COMCAPT),DMCB,(C'T',=C'COPY')                   L16           
         LR    R1,R2      RESTORE REGISTER 1                      L16           
         L     RE,DMCB+4     COMCAPT RETURNS ADDRESS OF MESSAGE   L16           
         OC    0(12,RE),0(RE)    ANY OVERRIDES                    L16           
         BZ    DT97USE                                            L16           
         CLI   LINENEED,2                                         L16           
         BH    *+8                                                L16           
         MVI   LINENEED,2                                         L16           
         L     R1,AP1                                             L16           
         AH    R1,DDISP                                           L16           
         MVC   0(25,R1),0(RE)   OVERRIDE CAPTION 1               L16            
         MVC   198(25,R1),25(RE) VERRIDE CAPTION 2               L16            
         B     DP97W                                              L16           
*                                                                 L16           
DT97USE  DS    0H                                                 L16           
*                                                                 L16           
         OC    PBDJOB,PBDJOB                                                    
         BZ    DP97W               NO JOB NUMBER                                
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
*                                                                               
         LA    R3,1(RC)                                                         
         LA    R3,4095(R3)                                                      
         USING PPFILED+4096,R3                                                  
*                                                                               
         MVC   KEY(2),PBUYKAGY                                                  
         MVC   KEY+2(1),PBUYKMED                                                
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
* THIS MAY BE A PASSIVE PRODUCT// IF SO USE ZZZ FOR KEY           BUG07         
         CLC   SAVEPRD,PASSPROD                                   BUG07         
         BE    *+10                                               BUG07         
         MVC   KEY+7(3),SAVEPRD        ZZZ PRODUCT                BUG07         
         MVC   KEY+10(6),PBDJOB                                                 
         CLC   PJOBREC(16),KEY                                                  
         BE    DP97C               HAVE JOB REC                                 
* BUY-IO JOB RECORD DIRECTORY                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JOB NOT ON FILE                              
         LA    R0,PJOBREC                                                       
         ST    R0,AREC                                                          
* BUY-IO JOB RECORD                                                             
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
DP97C    MVC   KEY(64),SAVKEYS        RESTORE KEYS                              
* BUY-IO RESTORE D/M POINTERS                                                   
         GOTO1 HIGH                RESTORE FOR SEQ READ                         
*                                                                               
         CLI   LINENEED,2                                                       
         BH    DP97F                                                            
         MVI   LINENEED,2                                                       
DP97F    L     R1,AP1                                                           
         AH    R1,DDISP                                                         
         MVC   0(25,R1),PJOBCAP1                                                
         MVC   198(25,R1),PJOBCAP2                                              
DP97W    LH    R0,DDISP                                                         
         ZIC   R3,2(R4)                                                         
         AR    R0,R3                                                            
         STH   R0,DDISP                                                         
         L     RE,DP97-4                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
         TITLE 'DP99 -- PPL102 - PRINTPAK USER REPORT'                          
*                                                                               
DP99     LH    R0,DDISP                                                         
         ZIC   R3,2(R4)                                                         
         AR    R0,R3                                                            
         STH   R0,DDISP                                                         
         BR    RE                                                               
*                                                                               
         DS    F                                                                
DOLDISP  DS    0H                                                               
         CLI   2(R4),X'F0'       DO NOT PRINT DATA                L13           
         BER   RE                                                 L13           
         ST    RE,DOLDISP-4                                                     
         L     R1,AP1                                                           
         AH    R1,DDISP                                                         
         MVC   0(6,R1),PBDJOB                                                   
         LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         LR    R3,R1                                                            
*                                                                               
         EDIT  (R2),(14,0(R3)),2,COMMAS=YES,FLOAT=-                             
         L     RE,DOLDISP-4                                                     
         BR    RE                                                               
*                                                                               
         SPACE 3                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
         SPACE 2                                                                
* IN ADDCUMS DOLLAR FIELDS ARE ACCUMULATED IN TEMPTOTS.        *                
* AT BREAK OF KEY, TEMPTOTS ARE ADDED TO ACCUMS AND FINAL TOTS.*                
* ACCUMS ARE PRINTED AT BREAK, THEN THAT PARTICULAR ACCUM AND  *                
* THE TEMPTOTS ARE CLEARED.                                    *                
* AT RUNLAST ALL ACCUMS AND FINAL TOTS ARE PRINTED.            *                
         SPACE                                                                  
ADDCUMS  DS    0H                                                               
         LA    R3,TEMPTOTS      ** R2=$ AMOUNT **                               
         CLI   0(R3),0             IS IT FIRST TIME                             
         BE    ACC10                                                            
ACC05    CLC   0(1,R3),ACCODE        NO. MATCH ACCODE                           
         BE    ACC12                    ELSE ADD AT END                         
         LA    R3,12(R3)                    OF TEMPTOTS.      L01               
         CLI   0(R3),0                                                          
         BNE   ACC05                                                            
ACC10    MVC   0(1,R3),ACCODE                                                   
* *12    L     R5,4(R3)            ADD $ AMOUNT                                 
* *      AR    R5,R2                  IN R2 TO TEMPTOTS.                        
* *      ST    R5,4(R3)                                                         
ACC12    CVD   R2,DUB              ADD $ AMOUNT              L01                
         AP    4(8,R3),DUB                                   L01                
         MVC   2(2,R3),DDISP       SET P LINE DISP                              
*                                                              L04              
*   DETERMINE IF DIFFERENCE IN COLUMNS WAS REQUESTED           L04              
*                                                              L04              
         CLI   DIFFACCU,255                                    L04              
         BNE   NODIFF                                          L04              
         CLI   ACCODE,93     DO NOT STORE AMOUNTS FOR THIS     L04              
         BE    NODIFF                                          L04              
         L     R1,SAVEAPTR                                     L04              
         LA    R1,4(R1)                                        L04              
         ST    R1,SAVEAPTR    SAVE LAST USED FIELD             L04              
         ST    R2,0(R1)       SAVE AMOUNT                      L04              
NODIFF   DS    0H                                              L04              
*                                                              L04              
         BR    RF                                                               
         SPACE 3                                                                
* CHECK FOR BREAK OF SORTREC KEY ON REQUESTED BREAKS                            
*            IF SO PRINT TOTALS                                                 
BRKCHK   NTR1                                                                   
         LA    R1,BRKKEY                                                        
BRK10    CLI   0(R1),0                                                          
         BE    BRK30                                                            
         LA    R2,SORTREC                                                       
         LA    R3,SRTRCSV                                                       
         ZIC   R4,1(R1)            GET DISP OF BRKKEY FIELD                     
         CLI   0(R1),X'02'         IS IT REGION                                 
         BE    B4BY3                          XXX                               
         CLI   0(R1),3    IS IT DIVISION                        BUG05           
         BNE   YBYX                                             BUG05           
         ZIC   R4,ADSDST+1  DISTRICT DISPLACEMTNT               BUG05           
YBYX     B     *+8     DO NOT INCREMENT                         BUG05           
B4BY3    DS    0H                             XXX                               
         LA    R4,3(R4)         YES/BUMP BY 3 FOR DIV/REG/DST/SHR/SUM           
*                                   IS CARRIED IN REGION SORT AREA              
         AR    R2,R4               ADD TO SORTREC                               
         AR    R3,R4               ADD TO SORTREC SAVE                          
         CLI   0(R1),X'1E'         DATE FIELDS (C'30'-C'39')                    
         BL    BRK12                  ARE COMPARED                              
         CLI   0(R1),X'27'                FOR YEAR/MONTH                        
         BH    BRK12                          ONLY.                             
         LA    R4,1                                                             
         B     BRK14                                                            
BRK12    ZIC   R4,2(R1)            L'OF FIELD TO COMPARE                        
         CLI   0(R1),X'02'           IS IT REGION                               
         BNE   *+8                                                              
         LA    R4,3          YES/SET 3 FOR COMPARE SINCE SORT L'=11             
         BCTR  R4,0                                                             
BRK14    EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(R3)                                                    
         BNE   BRK20                                                            
         LA    R1,3(R1)                                                         
         B     BRK10                                                            
BRK20    BAS   RE,PRTCUMS                                                       
BRK30    DS 0H                                                                  
         B     TLOXIT                                                           
         EJECT                                                                  
         SPACE                                                                  
* ADD TEMPTOTS TO CUM TOTS                     *                                
* DO NOT TOUCH R1 WHICH HAS BRKKEY FROM BRKCHK *                                
PRTCUMS  NTR1                                                                   
         LA    R2,TEMPTOTS                                                      
PRT00    LA    R3,ACCUMS                                                        
         B     PRT02                                                            
PRT01    LA    R3,12(R3)         INCREMENT ACCUMS                L01            
         CLI   0(R3),0             END OF ACCUMS                                
         BE    PRT06                                                            
PRT02    CLC   0(1,R3),0(R2)         DO  $ FIELDS CODES MATCH                   
         BNE   PRT01                                                            
* *04    L     R4,4(R2)            YES / ADD TEMP TOTS                          
* *      A     R4,4(R3)               TO CUM TOTS                               
* *      ST    R4,4(R3)                                                         
PRT04    AP    4(8,R3),4(8,R2)     YES / ADD TEMP TOTS           L01            
         MVC   2(2,R3),2(R2)       SET DISP                                     
         B     PRT01                                                            
PRT06    LA    R2,12(R2)            INCREMENT TEMPTOTS           L01            
         CLI   0(R2),0               END OF TEMPTOTS                            
         BNE   PRT00                                                            
         CLI   LINBRKSW,C'Y'      IS THIS PART OF A LINEBREAK ROUTINE           
         BER   RE                 IF SO, RETURN TO LINEBREAK MAINLINE.          
         SPACE                                                                  
* PRINT CUM TOTS, R1 HAS BRKKEY FROM BRKCHK *                                   
* SET UP BRKEY ID OF FOUND BREAK PLUS ID'S  *                                   
* THAT COME AFTER IT IN RDSAVE, THEN ACCESS*                                    
* IN REVERSE ORDER                          *                                   
         SPACE                                                                  
         L     R2,ARDSAVE                                                       
         XC    0(20,R2),0(R2)                                                   
         MVI   0(R2),X'FF'                                                      
         LA    R2,1(R2)                                                         
PRT08    MVC   0(1,R2),0(R1)                                                    
         LA    R1,3(R1)                                                         
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     PRT08                                                            
PRT08A   LR    R5,R2               R5=BRKKEY                                    
         ST    R2,ABRKEY                                                        
         B     PRT09                                                            
* ARE THERE ANY BRKEYS AFTER FOUND ONE *                                        
PRT08B   L     R2,ABRKEY                                                        
         BCTR  R2,0                                                             
         CLI   0(R2),X'FF'         END OF BRKEYS                                
         BE    PRT23                                                            
         B     PRT08A                                                           
         SPACE                                                                  
PRT09    L     R3,AP1              R3=PRINT LINE                                
         LA    R2,RPTTAB           R2=PRTTAB                                    
PRT10    CLC   0(1,R5),0(R2)                                                    
         BE    PRT12                                                            
         CLI   2(R2),X'FF'         NOT FOR DISPLAY                              
         BE    PRT11               DONT DISPLAY/SKIP                            
         CLI   2(R2),X'F0'       DO NOT PRINT DATA                L13           
         BE    PRT11                                              L13           
*                                                                 L13           
*                                                                 L13           
*                                                                 L13           
         ZIC   R0,2(R2)            INCREMENT P LINE                             
         AR    R3,R0                                                            
PRT11    LA    R2,5(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   PRT10                                                            
         DC    H'0'                                                             
         SPACE                                                                  
PRT12    ST    R3,FULL                                                          
         LA    R4,ACCUMS            R4=ACCUMS TBL                               
* IF BREAK ID IS = TO ACCUM BREAK ID PRINT -- THE ACCUM ID IS SET UP            
*  AT LABEL 'LAC02'                                                             
PRT15    CLC   1(1,R4),0(R2)       COMPARE SELECTION IDS                        
         BNE   PRT17                                                            
         L     R3,AP1                                                           
         AH    R3,2(R4)            PRINT LINE DISP TO R3                        
         EDIT  (P8,4(R4)),(14,0(R3)),2,COMMAS=YES,FLOAT=-         L01           
         MVC   198(14,R3),=22X'1C'                                              
         SPACE                                                                  
PRT17    LA    R4,12(R4)           INCREMENT ACCUMS TBL           L01           
         CLI   0(R4),0                                                          
         BNE   PRT15                                                            
         SPACE                                                                  
* PRINT INSERTION TOTALS IF REQUESTED *                                         
         CLI   INSFLG,C'Y'                                                      
         BNE   PRT18                                                            
         L     R3,AP1                                                           
         AH    R3,INSDISP                                                       
         EDIT  (B4,INSTOT),(5,0(R3)),ZERO=BLANK                                 
         MVC   198(5,R3),=22X'1C'                                               
         XC    INSTOT,INSTOT                                                    
         SPACE                                                                  
PRT18    GOTO1 =A(PBRKTOTS)                                                     
         MVC   SPACING,SPACEING   X'02'                                         
         BAS   RE,TLPRT                                                         
         EJECT                                                                  
* CLEAR ACCUM OF BREAKKEY *                                                     
         LA    R4,ACCUMS                                                        
PRT22    CLC   1(1,R4),0(R5)                                                    
         BNE   BMPUPR4                                                          
         XC    4(8,R4),4(R4)                                        L01         
         OI    11(R4),SIGN                                          L01         
BMPUPR4  LA    R4,12(R4)                                            L01         
         CLI   0(R4),0                                                          
         BNE   PRT22                                                            
         B     PRT08B                                                           
         SPACE                                                                  
* ADD TEMPTOTS TO FINLTOTS (FINLTOTS =PL8 ALLOTHER TOTS=F)                      
PRT23    LA    R3,FINLTOTS                                                      
         LA    R2,TEMPTOTS                                                      
         LA    R5,TOTTOT                                                        
PRT24    MVC   0(4,R3),0(R2)                                                    
         MVC   0(4,R5),0(R2)                                                    
* *      L     R4,4(R2)            ADD TEMP TOTS                                
* *      CVD   R4,DUB              TO FINAL                                     
* *      AP    4(8,R3),DUB         TOTS.                                        
* *      AP    4(8,R5),DUB         TOTS.                                        
         AP    4(8,R3),4(8,R2)     TOTS.                         L01            
         AP    4(8,R5),4(8,R2)     TOTS.                         L01            
         LA    R3,12(3)                                                         
         LA    R2,12(R2)                                         L01            
         LA    R5,12(R5)                                                        
         CLI   0(R2),0             END OF TEMPTOTS                              
         BNE   PRT24                                                            
         CLI   BRKSW,C'N'          ARE THERE ANY BRK CHECKS                     
         BER   RE                  NO/ RETURN TO TLTXXX.                        
         SPACE                                                                  
* CLEAR TEMPTOTS *                                                              
         MVI   PRTSW,C'Y'                                                       
         LA    R2,TEMPTOTS                                                      
PRT26    CLI   0(R2),0                                                          
         BE    PRTXIT                                                           
         XC    4(8,R2),4(R2)                                       L01          
         OI    11(R2),SIGN                                         L01          
         LA    R2,12(R2)                                           L01          
         B     PRT26                                                            
         SPACE                                                                  
PRTXIT   DS    0H                                                               
         CLI   LINBRKSW,C'Y'       IS IT A LINEBRK                              
         BNE   TLOXIT              NO. EXIT.                                    
         MVI   LINBRKSW,C'N'       YES. RESET LINEBRK                           
         B     TL05                RETURN TO NORMAL PROC FLOW                   
         EJECT                                                                  
         SPACE                                                                  
* THIS PRINTS RUN TOTALS AT RUN LAST *                                          
LASTOTS  DS    0H                                                               
         L     R5,AWIDEC                                                        
         USING WIDED,R5                                                         
         MVI   LINENEED,8                                                       
*        MVI   XP1,0                                                            
         BAS   RE,TLPRT                                                         
         MVI   FIRSTT,C'N'                                                      
         CLI   QOPT2,C'Y'          ARE BOXES TO BE SUPPRESSED                   
         BE    LST04                                                            
         CLI   MODE,LBUYXRQ                                                     
         BE    DOFNLBOX                                                         
         CLI   RCMULTIQ,C'Y'       SUPPRESS FINAL BOX FOR CROSSMEDIA            
         BE    LST04                                                            
         SPACE 2                                                                
* SET UP BOX FOR FINAL TOTS *                                                   
DOFNLBOX DS    0H                                                               
*                                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVI   BOXINIT,0                                                        
         LA    R3,BOXROWS                                                       
         MVC   BOXROWS,XSPACES                                                  
         MVI   0(R3),C'T'                                                       
         ZIC   R2,LINE                                                          
         AR    R3,R2                                                            
         BCTR  R3,0                                                             
         MVI   0(R3),C'B'                                                       
         BAS   RE,TLPRT                                                         
         SPACE                                                                  
         L     R1,ABOX           * A NEW ROW FOR TOTALS BOX *                   
         USING BOXD,R1                                                          
         LA    R3,BOXROWS                                                       
         MVC   BOXROWS,XSPACES                                                  
         ZIC   R2,LINE                                                          
         AR    R3,R2                                                            
         BCTR  R3,0                                                             
         MVI   BOXINIT,0                                                        
         MVI   0(R3),C'T'                                                       
         MVI   4(R3),C'B'                                                       
         SPACE                                                                  
         L     R3,AP1              * THIS ROUTINE CLEARS THE                    
         LA    R2,XP1              * COLS LINES FROM                            
         SR    R3,R2               * '*** REPORT TOTALS ***'                    
         LTR   R3,R3                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    R2,BOXCOLS                                                       
*                                                                               
         LA    R4,BOXCOLS+198                                                   
*                                                                               
         AR    R2,R3               R2=START OF PRINTING ON P LINE               
         MVC   10(21,R2),SPACES                                                 
* NEED TO CHK THAT SPACES MOVE ABOVE HAS NOT ERASED           *                 
* BOXCOLS  END DELIMITER.  CAN HAPPEN ON VERY NARROW REPORTS  *                 
         LA    R2,31(R2)           R2=START OF DATA AFTER HEADER OF P           
         SR    R4,R2                                                            
         BCTR  R4,0         R4=REMAINING LEN OF P LINE AFTER HEADER             
         LTR   R4,R4                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LR    R1,R4                                                            
         EX    R1,SPCIT           TEST IF END DELIMITER HAS BEEN ERASED         
         EX    R1,CLCIT                                                         
         BNE   LST03                                                            
         LA    R2,R4(R2)                                                        
         MVI   1(R2),C'R'         IF YES/ MOVE IN END DELIMITER                 
         B     LST03                                                            
*                                                                               
SPCIT    OC    0(0,R2),XSPACES                                                  
CLCIT    CLC   0(0,R2),XSPACES                                                  
*                                                                               
LST03    BAS   RE,TLPRT                                                         
         SPACE                                                                  
LST04    LA    R3,FINLTOTS                                                      
         L     R2,AP1                                                           
         CLI   RCMULTIQ,C'Y'                                                    
         BNE   LST04B                                                           
*****                                                                           
         CLI   DOWNLOAD,C'Y'                                                    
         BE    LST04B                                                           
*****                                                                           
         MVC   10(21,R2),=C'*** MEDIA  TOTALS ***'                              
         CLI   MODE,LBUYXRQ                                                     
         BNE   LST04C                                                           
         MVC   10(21,R2),=C'***  RUN  TOTALS  ***'                              
         LA    R3,TOTTOT                                                        
         B     LST04C                                                           
LST04B   MVC   10(21,R2),=C'*** REPORT TOTALS ***'                              
LST04C   CLI   QOPT2,C'Y'                                                       
         MVI   SPACING,1                                                        
         BNE   *+10                                                             
         MVC   198+10(21,R2),DASHES                                             
         BAS   RE,TLPRT                                                         
         SPACE                                                                  
*                                                                               
LST05    CLI   SVINSFLG,C'Y'          DO I NEED TO PRINT NO. OF INSERTS         
         BNE   LST05A                 TOTAL FOR C OR * MEDIA                    
         AH    R2,SVINSDSP                                                      
         EDIT  (B4,TTINSRT),(5,0(R2)),ZERO=BLANK                                
         L     R2,AP1                                                           
*                                                                               
LST05A   AH    R2,2(R3)            ADD DISP TO  LINE                            
         OC    4(8,R3),4(R3)                                                    
         BNZ   LST08                                                            
         ZAP   4(8,R3),=P'0'                                                    
LST08    EDIT  (P8,4(R3)),(14,0(R2)),2,COMMAS=YES,FLOAT=-                       
ZAPIT    ZAP   0(12,R3),=P'0'     CLEAR FINLTOT                                 
         CLI   QOPT2,C'Y'                                                       
         BNE   *+10                                                             
         MVC   198(14,R2),DASHES                                                
         LA    R3,12(R3)                                                        
         CLI   0(R3),0                                                          
         BE    LST10                                                            
         L     R2,AP1                                                           
         B     LST05                                                            
LST10    CLI   INSFLG,C'Y'                                                      
         BNE   LST15                                                            
         L     R3,AP1                                                           
         AH    R3,INSDISP                                                       
         EDIT  (B4,INSFTOT),(5,0(R3)),ZERO=BLANK                                
LST15    BAS   RE,TLPRT                                                         
         BAS   RE,TLPRT                                                         
         BAS   RE,TLPRT                                                         
         MVI   FIRSTT,C'Y'                                                      
* *      XC    TEMPTOTS,TEMPTOTS      CLEAR TEMPTOTS               L01          
*                                                                  L01          
         LA    R2,TEMPTOTS                                         L01          
CLRTMP   CLI   0(R2),0                                             L01          
         BE    OVERCLR                                             L01          
         ZAP   0(12,R2),=P'0'  ENSURE SIGN                         L01          
         LA    R2,12(R2)                                           L01          
         B     CLRTMP                                              L01          
OVERCLR  DS    0H                                                  L01          
*                                                                  L01          
         XC    INSCNT(15),INSCNT    + INSTOT/INSFTOT/INSDISP/INSFLG             
         XC    RCSUBPRG,RCSUBPRG                                                
*                                                                 L08           
* CHECK TO SEE IF STANDARD COMMENTS WERE REQUESTED IN USERP       L08           
*                                                                 L08           
         CLI   STDCODE1+5,X'40'   ANY STD COMMENT 1               L08           
         LA     R2,STDCODE1                                       L08           
         BNH   NOCOMPR0                                           L08           
         GOTO1  =A(COMPRT0)                                       L08           
NOCOMPR0 DS    0H                                                               
*                                                                 L08           
         CLI   STDCODE2+5,X'40'   ANY STD COMMENT 2               L08           
         LA     R2,STDCODE2                                       L08           
        BNH    REQ4SCOM            TEST FOR STANDARD COMMENT                    
         GOTO1  =A(COMPRT0)                                       L08           
*                                                                 L08           
REQ4SCOM CLC   QPUB+1(3),=C'SC='    STANDARD COMMENT IN REQUEST  L08            
        BNE    TLOXIT                                            L08            
        LA     R2,QPUB+4            POINT TO COMMENT NO.         L08            
        GOTO1  =A(COMPRT0)                                       L08            
*                                                                L08            
*                                                                L08            
*                                                                 L08           
         B     TLOXIT                                                           
         EJECT                                                                  
*                                                                               
TLPRT    NTR1                                                                   
*                                                                               
         L     R5,AWIDEC                                                        
         MVC   DISTNMSV,PDSTNAME         SO DITTO MARKS WON'T PRINT IN          
         MVC   REGNMSV,PREGNAME          MIDDLE OF LINE OF DATA WHEN            
         CLI   RCMULTIQ,C'Y'             MULTIMEDIA?              BUG30         
         BNE   NOMULTI                                            BUG30         
         CLC   SVMED,QMEDIA             START OF DIFF. MEDIA?    BUG30          
         BE    NOMULTI                  THEN MOVE BLANK INTO OLD  BUG30         
         MVI   DIFFMED,1                TURN ON FLAG              BUG30         
NOMULTI  MVC   OLDJOB,PBDJOB             COMBINING INSERTIONS                   
*        MVC   SPACING,SPACEING          FORCE REQUESTED           L22          
         BAS   RE,TLCKHD                                                        
*                                                                               
         CLI   DOWNLOAD,C'Y'                                                    
         BNE   TLPRT2                                                           
         GOTO1 =A(DOWN),DMCB                                                    
         L     R1,AP1              CLEAR PRINT LINE                             
         MVI   0(R1),X'40'                                                      
         MVC   1(197,R1),0(R1)                                                  
         B     TLPRTX                                                           
*                                                                               
TLPRT2   DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,TLHEAD                                                        
         CLI   MASTCLI,0                                           L21          
         BNE   MVCXC                                               L21          
         CLI   OFCLTSW,C'Y'                                                     
         BNE   TLPRT2B                                                          
MVCXC    MVC   XHEAD3(11),XHEAD5                                                
         XC    XHEAD5(11),XHEAD5                                                
TLPRT2B  DS    0H                                                               
*                                                                 BUG10         
* REPORT DOES NOT HANDLE MORE THAN 4 LINES AT A TIME.  THEREFORE  BUG10         
*  A BUY  WITH FIVE BUY COMMENT LINES WILL NOT HAVE THE FIFTH LINEBUG10         
*  PRINTED.                                                       BUG10         
*                                                                 BUG10         
         CLI   EXTRAPRT,C'Y'    IS THERE A 5TH LINE               BUG10         
         BNE   NOT5LINE                                           BUG10         
*         PRINT ONE LINE AT A TIME TO GET ALL FIVE LINES TOGETHER BUG10         
*         ALSO PRINT ONE LINE OF SPACES                           BUG10         
*                                                                 BUG10         
         L     R2,ASPEC          USE R2 AS WORK                   BUG10         
         L     RF,AP1                                             BUG10         
         MVC   000(198,R2),198(RF)                                BUG10         
         MVI   198(RF),C' '                                       BUG10         
         MVC   199(197,RF),198(RF)  TO BLANKS  CLEAR LINE 2       BUG10         
         MVC   198(198,R2),396(RF)                                BUG10         
         MVC   396(198,RF),198(RF)   CLEAR LINE 3                 BUG10         
         MVC   396(198,R2),594(RF)   SAVE LINE 4                  BUG10         
         MVC   594(198,RF),198(RF)   CLEAR LINE 4                 BUG10         
         MVI   SPACING,1             PRINT ONLY ONE LINE          BUG10         
         GOTO1 REPORT                PRINT LINE 1                 BUG10         
         L     RF,AP1                                             BUG10         
         MVC   0(198,RF),0(R2)       MOVE SECOND LINE             BUG10         
         GOTO1 REPORT                PRINT LINE 2                 BUG10         
         L     RF,AP1                                             BUG10         
         MVC   0(198,RF),198(R2)     MOVE THIRD  LINE             BUG10         
         GOTO1 REPORT                PRINT LINE 3                 BUG10         
         L     RF,AP1                                             BUG10         
         MVC   0(198,RF),396(R2)     MOVE FOURTH LINE             BUG10         
         GOTO1 REPORT                PRINT LINE 4                 BUG10         
         L     RF,AP1                                             BUG10         
         MVC   0(198,RF),MYPRINT    FIFTH LINE                    BUG10         
         GOTO1 REPORT                                             BUG10         
         GOTO1 REPORT               ***SPACE***                   BUG10         
         MVI   EXTRAPRT,0                                         BUG10         
         MVI   MYPRINT,C' '                                       BUG10         
         MVC   MYPRINT+1(L'MYPRINT-1),MYPRINT                     BUG10         
*                                                                 BUG10         
         B     NEED2LNE                                         BUG10           
ASPEC    DC    A(SPECIALA)                                                      
NOT5LINE DS    0H                                                               
         GOTO1 REPORT                                                           
NEED2LNE MVI   LINENEED,2                                                       
         CLI   MASTCLI,0                                            L21         
         BE    NOMASTE                                              L21         
         MVC   PCLTKCLT,REALCLI            MOVE REAL CLIENT CODE ANDL21         
         MVC   PCLTNAME,REALCLI+3          NAME                                 
NOMASTE  DS    0H                                                   L21         
TLPRTX   CLI   RCMULTIQ,C'Y'               MULTIMEDIA?            BUG30         
         BNE   TLPRTX1                                            BUG30         
         CLC   SVMED,QMEDIA                CHANGE OF MEDIA?       BUG30         
         BE    TLPRTX1                     YES DON'T CHANGE SVMED BUG30         
         MVC   SVMED,QMEDIA                ELSE SAVE OFF NEW MEDIABUG30         
TLPRTX1  XIT1                                                                   
SVMED    DS    CL1                                                              
         SPACE 3                                                                
TLHEAD   NTR1                                                                   
*                                                                               
         CLI   QOPT7,C'Y'                                                       
         BNE   *+8                                                              
         MVI   XHEAD5+116,C'T'                                                  
         MVC   XHEAD1(5),=C'MEDIA'                                              
         CLI   RCMULTIQ,C'C'                                                    
         BNE   TLHEAD0C                                                         
         MVI   XHEAD1+9,C'C'                                                    
         MVC   XHEAD1+13(8),=C'COMBINED'                                        
         B     TLHEAD1                                                          
TLHEAD0C CLI   RCMULTIQ,C'Y'                                                    
         BNE   TLHEAD0E                                                         
         MVC   XHEAD1+13(3),=C'ALL'                                             
         B     TLHEAD1                                                          
TLHEAD0E MVC   XHEAD1+9(1),PAGYKMED                                             
         MVC   XHEAD1+13(10),PAGYMED                                            
TLHEAD1  DS    0H                                                               
*                                                               BUG14           
         L     RF,AP1                                           BUG14           
         CLC   =C'NO DATA TO PRINT',0(RF)                       BUG14           
         BNE   OKDATAH                                          BUG14           
         CLI   QCLIENT,C'A'                                     BUG14           
         BL    *+16                                             BUG14           
         XC    PCLTNAME,PCLTNAME                                BUG14           
         MVC   PCLTKCLT,QCLIENT                                 BUG14           
         CLI   QPRODUCT,C'A'                                    BUG14           
         BL    *+16                                             BUG14           
         XC    PPRDNAME,PPRDNAME                                BUG14           
         MVC   PPRDKPRD,QPRODUCT                                BUG14           
         CLI   QEST,C'0'                                    BUG14               
         BL    *+22                                             BUG14           
         XC    PESTNAME,PESTNAME                                BUG14           
         XC    PESTNAM2,PESTNAM2                                BUG14           
         MVC   PESTKEST,BQEST                                   BUG14           
         MVC   PBUYKEY+7(3),QPRODUCT                            BUG14           
         MVC   PRD,QPRODUCT                                                     
         MVC   EST,QEST                                                         
OKDATAH  DS    0H                                               BUG14           
*                                                               BUG14           
*                                                               BUG14           
*                                                               BUG14           
*                                                               BUG14           
*                                                               BUG14           
         MVI   RCSUBPRG,0          CLEAR RCSUBPRG                               
         BAS   RE,GETCLT                                                        
         CLI   MASTCLI,0                                            L21         
         BE    NOMASTER                                             L21         
         MVC   REALCLI(3),PCLTKCLT         SWAP REAL CLIENT CODE ANDL21         
         MVC   REALCLI+3(20),PCLTNAME      NAME FOR MASTER CLIENT CODE          
         MVC   PCLTKCLT,MASTCLI            AND NAME                 L21         
         MVC   PCLTNAME,MASTCLNA                                    L21         
NOMASTER DS    0H                                                   L21         
         CLC   QPRODUCT,SPACES                                                  
         BE    TLHEAD1C                                                         
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    TLHEAD1C                                                         
         B     TLHEAD1F                                                         
TLHEAD1C XC    PPRDKEY,PPRDKEY                                                  
TLHEAD1F CLC   QEST,SPACES                                                      
         BE    TLHEAD1H                                                         
         CLC   QEST,=C'ALL'                                                     
         BE    TLHEAD1H                                                         
         B     TLHEAD1J                                                         
TLHEAD1H XC    PESTKEY,PESTKEY                                                  
*                                                                               
TLHEAD1J CLC   QEST,SPACES         ESTIMATE FILTERS                             
         BNE   TLHEAD2                                                          
         CLC   QESTEND,SPACES                                                   
         BE    TLHEAD2                                                          
         MVI   RCSUBPRG,2                                                       
         SPACE                                                                  
TLHEAD2  OC    QDIV,SPACES         CHK FOR DIV IN HEADS                         
         CLC   QDIV,SPACES         END OF TABLE                                 
         BE    TLHEAD3                                                          
         BAS   RE,OTGTDIV             GET DIV RECORD                            
         MVI   RCSUBPRG,1                                                       
         SPACE                                                                  
TLHEAD3  MVC   XHEAD9,SAVHD7                                                    
         MVC   XHEAD10,SAVHD8                                                   
         MVC   XHEAD11,SAVHD9                                                   
         MVC   XHEAD1+45(30),HEADSAVE                                           
TLHEAD4  DS    0H                                                               
         LA    RF,XSPACES                                                       
         CLI   QBPDATE,C' '                                                     
         BE    TLHEAD5                                                          
         LA    RF,BILHD                                                         
         CLI   QBPDATE,C'B'                                                     
         BE    TLHEAD5                                                          
         LA    RF,PAYHD                                                         
         CLI   QBPDATE,C'P'                                                     
         BE    TLHEAD5                                                          
         LA    RF,CLOSHD                                                        
         CLI   QBPDATE,C'C'                                                     
         BE    TLHEAD5                                                          
***MAT***                                                                       
         LA    RF,MCLOSHD                MATERIALS CLOSING DATE                 
         CLI   QBPDATE,C'M'                                                     
         BE    TLHEAD5                                                          
***MAT***                                                                       
         LA    RF,OSDHD                                                         
         CLI   QBPDATE,C'S'                                                     
         BE    TLHEAD5                                                          
         LA    RF,BILLDTES      FILTER ON BILLING DATES             L04         
         CLI   QBPDATE,C'I'                                                     
         BE    TLHEAD5                                              L04         
         LA    RF,PAIDDTES      FIFLTER ON PAID DATE MSG            L04         
         CLI   QBPDATE,C'A'                                         L04         
         BNE   *+10                                                 L04         
TLHEAD5  DS    0H                                                               
         MVC   XHEAD5+49(22),0(RF)                                              
         CLI   QCLIENT,C'*'                                                     
         BNE   TLHEAD6                                                          
         MVC   XHEAD5(6),=C'OFFICE'                                             
         MVC   XHEAD5+9(2),QCLIENT+1                                            
TLHEAD6  DS    0H                                                               
         CLI   QCLIENT,C'&&'        GROUP REQUEST              L25              
         BNE   TLHEAD7                                                          
         MVC   XHEAD5(5),=C'GROUP'                                              
         MVC   XHEAD5+8(2),QCLIENT+1                                            
TLHEAD7  DS    0H                                                               
         CLC   QREGION,SPACES      CHK REGION IN HEADS                          
         BE    TLHEADX                                                          
         BAS   RE,OTGTREG          GET REGION REC                               
         MVI   RCSUBPRG,3                                                       
         CLC   QDIST,SPACES        CHK DISTRICT IN HEADS                        
         BE    TLHEADX                                                          
         BAS   RE,OTGTDST          GET DIST REC                                 
*                                                                               
TLHEADX  DS    0H                                                               
*                                                                               
         ZIC   R1,RCSUBPRG                                                      
         CLC   QCLIENT,=C'ALL'                                 BUG28            
         BNE   TLHEAD2X                                        BUG28            
         CLC   RPTTAB(2),=X'0103'  SEE IF CLIENT IS FIRST SORT BUG28            
         BE    TLHEAD5X                                        BUG28            
         B     TLHEAD3X     NO - THEN NO CLIENT IN HEADLINES   BUG28            
TLHEAD2X CLI   OFCLTSW,C'Y'                                                     
         BNE   TLHEAD5X                                                         
TLHEAD3X AH    R1,=H'10'                                                        
TLHEAD5X CLC   QPRODUCT,SPACES                                                  
         BE    TLHEAD6X                                                         
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    TLHEAD6X                                                         
         AH    R1,=H'20'                                                        
TLHEAD6X CLI   SUPRNSW,C'Y'          SUPRESSING RUN ON DATE?                    
         BNE   TLHEAD7X                                                         
         AH    R1,=H'50'                                                        
TLHEAD7X STC   R1,RCSUBPRG                                                      
*                                                                               
         CLI   QOPT7,C'Y'             SEE IF INCLUDING TEST INSERTIONS          
         BNE   TLHEAD8X                                                         
         MVC   XHEAD6+98(34),=C'*INCLUDES ANY PROPOSED INSERTIONS*'             
         CLI   RCMULTIQ,C'Y'        MULTI-MEDIA REQ                             
         BE    TLHEAD8X                                                         
         CLI   RCMULTIQ,C'C'        OR COMBINED                                 
         BE    TLHEAD8X                                                         
         CLI   QMEDIA,C'O'                                                      
         BNE   *+10                                                             
         MVC   XHEAD6+121(11),=C'POSTINGS*  '                                   
*                                                                               
TLHEAD8X CLC   QREGION,SPACES         CAN ONLY SHOW 2ND EST NAME LINE           
         BNE   TLHEADXX               IF NOT DOING REGIONS                      
         CLC   QEST,SPACES                                                      
         BE    TLHEADXX                                                         
         CLC   QEST,=C'ALL'                                                     
         BE    TLHEADXX                                                         
         CLC   QESTEND,SPACES                                                   
         BNE   TLHEADXX                                                         
         MVC   XHEAD5+13(20),PESTNAM2                                           
         CLC   QAGENCY(2),=C'DM'            SPECIAL FOR DOREMUS                 
         BE    TLHEAD9W                                                         
         CLC   QAGENCY(2),=C'SJ'            AND SJR                             
         BNE   TLHEADXX                                                         
TLHEAD9W OC    PESTREVN,PESTREVN                                                
         BZ    TLHEAD9X                                                         
         MVC   XHEAD6+9(3),PESTREVN                                             
         MVC   XHEAD6(8),=C'REVISION'                                           
TLHEAD9X TM    PESTTEST,X'80'                                                   
         BZ    TLHEADXX                                                         
         MVC   XHEAD7(6),=C'STATUS'                                             
         MVC   XHEAD7+9(4),=C'TEST'                                             
TLHEADXX CLI   XSCR,0     WAS BILLING DESCRIPTION REQUESTED  L07                
         BE    GOXIT1                                            L07            
*                         SHOW FORMULA IF ONE PRD AND EST        BUG20          
         CLC   QPRODUCT,=C'   '                                  BUG20          
         BE    GOXIT1                                                           
         CLC   QPRODUCT,=C'ALL'                                  BUG20          
         BE    GOXIT1                                                           
         CLI   QEST,C'0'      SEE IF EST SPECIFIED               L04            
         BL    GOXIT1         NO                                 L04            
         CLI   QESTEND,C'0'   SEE IF RANGE OF ESTS               BUG20          
         BH    GOXIT1         YES                                BUG20          
         MVI   DESCR,01       FORCE                              L07            
         BAS   RE,DP52        GET DESCRIPTION                    L07            
         MVI   DESCR,0                                           L07            
         MVC   XHEAD7+40(16),=C'BILLING FORMULA-'                L07            
         MVC   XHEAD7+57(52),BFORMD                              L07            
GOXIT1    LA    R1,BILLONLY+2                                     L18           
          XC    BILLONLY,BILLONLY                                 L18           
          MVC   BILLONLY(2),=C'**'                                L18           
          CLI   QOPT3,C'Y'                                        L18           
          BNE   *+14                                              L18           
          MVC   0(7,R1),=C'BILLED/'                               L18           
          LA    R1,7(R1)                                          L18           
          CLI   QOPT4,C'Y'                                        L18           
          BNE   *+14                                              L18           
          MVC   0(9,R1),=C'BILLABLE/'                             L18           
          LA    R1,9(R1)                                          L18           
          CLI   QOPT5,C'Y'                                        L18           
          BNE   *+14                                              L18           
          MVC   0(9,R1),=C'UNCLEARED '                            L18           
          LA    R1,10(R1)                                         L18           
          BCTR  R1,0      REMOVE POSSIBLE /                       L18           
          CLI   BILLONLY+4,0 ANY OPTIONS SELECTED                 L18           
          BE    GGOXIT1                                           L18           
          MVC   0(7,R1),=C' ITEMS*'                               L18           
*                                                                 L18           
FINDMOVE  LA    RE,XHEAD6+98                                      L18           
          LA    RF,3*198(RE)     LIMIT                                          
OCWHERE   CLC   0(20,RE),SPACES  ANYTHING MOVED THERE ALREADY     L18           
          BE    OKCLEAR                                           L18           
          CLI   SUPRNSW,C'N'     PRINT REQUESTOR                                
          BE    TLFDEF                                                          
          LA    RE,198(RE)        BUMP ONE LINE                   L18           
          CR    RF,RE                                             L18           
          BNH   OCWHERE                                           L18           
*                                                                 L18           
TLFDEF    LA    RE,XHEAD6+49      DEFAULT                         L18           
OKCLEAR   MVC   0(L'BILLONLY,RE),BILLONLY                        L18            
GGOXIT1   XIT1                                                    L18           
*                                                                 L18           
*                                                                               
BILHD    DC    C' ** BILLING PERIOD ** '                                        
PAYHD    DC    C' ** PAYABLE DATES **  '                                        
CLOSHD   DC    C' ** CLOSING DATES **  '                                        
PAIDDTES DC    C' **PAID DATE FILTER** '                                        
BILLDTES DC    C'**BILLED DATE FILTER**'                                        
MCLOSHD  DC    C'**MAT. CLOSING DATES**'                                        
IODHD    DC    C'** INS. ORDER DATES **'                                        
OSDHD    DC    C' ** ON-SALE DATES **  '                                        
BILLONLY  DS    CL40                                              L18           
*                                                                 L18           
*                                                                               
         SPACE 3                                                                
*                                                                               
TLCKHD   DS    0H                                                               
         CLI   DOWNLOAD,C'Y'                                                    
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   RCMULTIQ,C'Y'                                                    
         BNE   TLCKHD5                                                          
         CLI   ANYPRTSW,C'N'                                                    
         BNE   TLCKHD5                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVI   ANYPRTSW,C'Y'                                                    
         BR    RE                                                               
*                                                                               
TLCKHD5  DS    0H                                                               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    R0,LINE                                                          
         IC    RF,LINENEED                                                      
         AR    R0,RF                                                            
*                                                                               
         ZIC   RF,SPACING                                  BUG28                
         CLC   RCSPACNG,SPACING                            BUG28                
         BNH   *+8                                         BUG28                
         IC    RF,RCSPACNG                                 BUG28                
         BCTR  RF,0                                        BUG28                
         AR    R0,RF                                       BUG28                
*                                                                               
         STC   R0,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BLR   RE                                                               
         MVI   FORCEHED,C'Y'                                                    
         BR    RE                                                               
*                                  PRINTING                                     
***************************************************                             
*  THIS RTN CHECKS DIV/REG/DST BREAKS OF SORTREC  *                             
*   (SORTREC VS SRTRCSV)                         *                              
*  AND GETS DIV/REG/DIST RECS                     *                             
*  INPUT:   SORTREC,SRTRECSV                     *                              
*  OUTPUT:  DIV/REG/DIST RECS                     *                             
***************************************************                             
DRDBRK   NTR1                                                                   
         LA    R1,RPTTAB                                                        
DRD2     CLI   0(R1),X'FF'                                                      
          BE    DRDXX                                                           
         CLI   0(R1),X'02'         TEST FOR REGION CODE                         
         BE    DRD5                                                             
         LA    R1,5(R1)                                                         
         B     DRD2                                                             
DRD5     LA    R1,SORTREC                                                       
         AH    R1,ADSPRD                                                        
         LA    R2,SRTRCSV                                                       
         AH    R2,ADSPRD                                                        
         CLC   0(9,R1),0(R2)       COMPARE DIV/REG/DST                          
         BE    DRDX                                                             
         CLC   0(3,R1),0(R2)       TEST DIV                                     
         BNE   DRD9A                                                            
         CLC   3(3,R1),3(R2)       TEST REG                                     
         BNE   DRD9B                                                            
         CLC   6(3,R1),6(R2)       TEST DIST                                    
         BNE   DRD9C                                                            
         DC    H'0'                A REAL PROBLEM                               
DRD9A    MVC   OLDDIV,0(R1)        SET IN NEW DIV                               
         BAS   RE,OTGTDIV                                                       
         CLC   QDIV,=C'ALL'        IF ALL/PAGE BREAK ON CHANGE OF DIV           
         BNE   DRD9B                                                            
         MVI   FORCEHED,C'Y'                                                    
DRD9B    MVC   OLDREG,3(R1)        SET IN NEW REG                               
         BAS   RE,OTGTREG                                                       
         CLC   QREGION,=C'ALL'     IF ALL/PAGE BREAK ON CHANGE OF REG           
         BNE   DRD9C                                                            
         MVI   FORCEHED,C'Y'                                                    
DRD9C    MVC   OLDDST,6(R1)        SET IN NEW DIST                              
         BAS   RE,OTGTDST                                                       
         CLC   QDIST,=C'ALL'     IF ALL/PAGE BREAK ON CHANGE OF DIST            
         BNE   DRDX                                                             
         MVI   FORCEHED,C'Y'                                                    
DRDX     B     TLOXIT              (XIT1)                                       
*                                                                 L15           
*  NO REGION=ALL REQUESTED // NOT IN RPTTAB MUST NOW VERIFY       L15           
*     TO SEE IF DIV=ALL OR DISTRICT=ALL -- THE TEST FOR REGION IN L15           
*     SETOLDS IS REDUNDANT                                        L15           
*                                                                 L15           
DRDXX     GOTO1 =A(SETOLDS)                                       L15           
          B     TLOXIT                                            L15           
*                                                                 L15           
         EJECT                                                                  
FORETURN DS    F                                                                
FORR1    DS    F                                                  BUG11         
INDEX    DC    X'0'                                                             
*                                                                               
GETCLT   MVI   INDEX,0                                                          
IOCOMMON ST    RE,FORETURN                                                      
         ST    R1,FORR1                REG 1 DESTROYED BY GOTO   BUG11          
         GOTO1 =A(IOREAD),DMCB,INDEX                                            
         L     R1,FORR1                RESTORE R1                 BUG11         
         L     RE,FORETURN                                                      
         BR    RE                                                               
GETPRD   MVI   INDEX,1                                                          
         B     IOCOMMON                                                         
GETESTM  MVI   INDEX,2                                                          
         B     IOCOMMON                                                         
GETP     MVI   INDEX,3                                                          
         B     IOCOMMON                                                         
OTGTDIV  MVI   INDEX,4            GET DIVISION RECORD                           
         B     IOCOMMON                                                         
*                                                                               
OTGTREG  MVI   INDEX,5            GET REGION RECORD                             
         B     IOCOMMON                                                         
*                                                                               
OTGTDST  MVI   INDEX,6            GET DISTRICT RECORD                           
         B     IOCOMMON                                                         
         SPACE 2                                                                
GETLTLRC MVI   INDEX,7                                                          
         B     IOCOMMON        TEST IF LTLREC ALREADY THERE                     
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         L     R5,AWIDEC                                                        
         USING WIDED,R5                                                         
*                                                                               
         SPACE                                                                  
         CLI   FIRSTT,C'Y'                                                      
         BNE   HDRTNX                                                           
* SET UP BOXES PARAMETERS *                                                     
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXCOLS(198),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXYORN,C'N'                                                     
         CLI   QOPT2,C'Y'          ARE BOXES TO BE SUPPRESSED                   
         BE    HDRTNX                                                           
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         SPACE                                                                  
         LA    R2,RPTTAB                                                        
HDRTN10  CLI   2(R2),X'FF'         TEST DONT DISPLAY                            
         BNE   HDRTN15                                                          
         LA    R2,5(R2)            SKIP SINCE NOT DISPLAYING                    
         B     HDRTN10                                                          
HDRTN15  L     R3,AP1              R3=START OF BREAK                            
         BCTR  R3,0                SUBTRACT 1 FOR BOXES                         
         LA    R4,XP1              R4=START OF P LINE                           
         SR    R3,R4                                                            
         LTR   R3,R3                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    R4,BOXCOLS                                                       
         AR    R4,R3                                                            
         MVI   0(R4),C'L'                                                       
HDRTN20  ZIC   R0,2(R2)            DISP L' TO R0                                
         AR    R4,R0                                                            
         MVI   0(R4),C'C'                                                       
         LA    R2,5(R2)                                                         
         CLI   2(R2),X'F0'     INVISIBLE FIELD                    L13           
         BE    *-8                                                L13           
         CLI   0(R2),0                                                          
         BNE   HDRTN20                                                          
         SR    R4,R0                                                            
         MVI   0(R4),C'R'                                                       
         MVC   BOXROWS,XSPACES                                                  
         LA    R4,BOXROWS                                                       
         LA    R4,7(R4)                                                         
         MVI   0(R4),C'T'                                                       
         LA    R4,3(R4)                                                         
         MVI   0(R4),C'M'                                                       
         LA    R4,47(R4)                                                        
         MVI   0(R4),C'B'                                                       
         SPACE                                                                  
HDRTNX   B     TLOXIT                                                           
         DROP  R5                                                               
         EJECT                                                                  
         SPACE 2                                                                
ABRKEY   DS    F                                                                
*                                                                               
         DS    0F                                                               
INSCNT   DS    F                   TEMPORARY INSERTION COUNTER                  
INSTOT   DS    F                   INSERT CUMS FOR BREAKS                       
INSFTOT  DS    F                   INSERTS FOR FINAL REPORT TOTAL               
INSDISP  DS    H                                                                
INSFLG   DS    CL1                                                              
OPTIONS  DS    CL1                 OPTIONS FOR  GETINS         L03              
DFLTFORM DC    X'0505000000'  DEFAULT BILLING FORMULA (G-CD)   L07              
DESCR    DC    X'0'                                             L07             
XSCR     DC    X'0'                                             L07             
       ++INCLUDE PBILPROF                                      L07              
BFORMD   DS    CL53                                            L07              
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*          DATA SET PPREPFILM  AT LEVEL 138 AS OF 01/07/85                      
*                             GET SHARE FOR REGION/DISTRICT                     
*                                  THIS ROUTINE FORCES THE SHARES FOR           
*                                  EACH REG/DIST TO ADD TO THE TOTAL.           
*                                  THE LOGIC PARALELLS THAT FOR POOL            
*                                  ALLOCATION IN GETINS.                        
*                                                                               
***********************************************                                 
* THIS ROUTINE PRINTS HEADLINE OF BREAK TOTS  *                                 
*      ON SAME OR DIFFERENT LINE              *                                 
PBRKTOTS NMOD1 0,**PBRKTOTS                                                     
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
*                                                                               
         L     R6,AWIDEC                                                        
         USING WIDED,R6                                                         
*                                                                               
         L     R2,ANAMETBL                                                      
BTK05    CLC   0(1,R2),0(R5)       R5 HAS SELECTION ID                          
         BE    BTK10                                                            
         LA    R2,24(R2)                                                        
         CLI   0(R2),0                                                          
         BNE   BTK05                                                            
         LA    R2,DEFTOTL                                                       
         SPACE                                                                  
BTK10    DS    0H                                                               
*        MVI   LINENEED,3                                                       
         L     R1,AP1                                                           
         L     R5,FULL             R1=P LINE START/R5,R4=SEL START              
         LR    R4,R5                                                            
         SR    R4,R1                                                            
         LTR   R4,R4                                                            
         BZ    BTK15               SELECTION START = P LINE START               
         C     R4,=F'22'       R4=DIFF BETWEEN PLIN ST AND SEL ST               
         BL    BTK17               PRINT ON SAME OR SEP LINE                    
         S     R5,=F'22'           R5=SEL START                                 
         OC    0(22,R5),SPACES                                                  
         CLC   0(22,R5),SPACES                                                  
         BNE   BTK18                                                            
* SAME LINE PRINT OF BREAK HEADLINE *                                           
*                                                                               
BTK12    DS    0H                                                               
*                                                                               
         B     BTK14                                                            
*                                                                               
**DP     CLC   QAGENCY,=C'DP'                                                   
**DP     BNE   BTK14                                                            
**DP     CLI   0(R2),1                                                          
**DP     BNE   BTK14                                                            
**DP     MVC   0(22,R5),=C'*AGENCY TOTAL*        '                              
**DP     B     *+10                                                             
*                                                                               
BTK14    MVC   0(22,R5),2(R2)                                                   
         LR    R3,R5                                                            
         B     BTK20                                                            
BTK15    OC    0(22,R5),SPACES                                                  
         CLC   0(22,R5),SPACES                                                  
         BNE   BTK18                                                            
*                                                                               
         B     BTK16                                                            
*                                                                               
**DP     CLC   QAGENCY,=C'DP'                                                   
**DP     BNE   BTK16                                                            
**DP     CLI   0(R2),1                                                          
**DP     BNE   BTK16                                                            
**DP     MVC   0(22,R5),=C'*AGENCY TOTAL*        '                              
**DP     B     *+10                                                             
*                                                                               
BTK16    MVC   0(22,R5),2(R2)                                                   
         LR    R3,R5                                                            
         B     BTK20                                                            
*                                                                               
BTK17    L     R1,AP1        SEE IF LINE ONE FREE                               
         OC    0(22,R1),SPACES                                                  
         CLC   0(22,R1),SPACES                                                  
         BE    BTK18C            LEAVE LINENEED AT 3                            
* SEPARATE LINE PRINT OF BREAK HEADLINE *                                       
*                                NEEDS 5 LINES                                  
BTK18    MVC   XP4,XP2                                                          
         MVC   XP3,XP1                                                          
         MVC   XP1,XSPACES                                                      
         MVC   XP2,XSPACES                                                      
         MVI   LINENEED,5                                      BUG25            
*                                                                               
BTK18C   L     R3,AP1                                                           
*                                                                               
         B     BTK19                                                            
*                                                                               
**DP     CLC   QAGENCY,=C'DP'                                                   
**DP     BNE   BTK19                                                            
**DP     CLI   0(R2),1                                                          
**DP     BNE   BTK19                                                            
**DP     MVC   0(22,R5),=C'*AGENCY TOTAL*        '                              
**DP     XC    DPUBCTRL,DPUBCTRL  CLEAR CONTROL                  L06            
**DP     B     *+10                                                             
*                                                                               
BTK19    MVC   0(22,R3),2(R2)                                                   
*                                                                               
BTK20    ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   198(0,R3),=22X'1C'                                               
         XIT1                      XIT1                                         
DEFTOTL  DC    AL1(88,11),CL22'** TOTAL **'                                     
         EJECT                                                                  
***************************************                                         
* THIS RTN TAKES DIV/REG/DST FROM     *                                         
* SORTREC AND SETS THEM IN OLDDIV/    *                                         
* OLDREG/OLDDST                       *                                         
*                                     *                                         
* USED ONLY IF QDIV/REG/DST = ALL     *                                         
*                                     *                                         
***************************************                                         
         SPACE                                                                  
SETOLDS  NMOD1 0,**SETOLDS                                                      
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         CLI   DOWNLOAD,C'Y'                                                    
         BE    SETX                                                             
         CLC   QDIV,=C'ALL'                                                     
         BNE   SET9                                                             
         LA    R1,BRKKEY                                                        
SET5     CLI   0(R1),X'F9'       DIV IN HEADS - SPECIAL      BUG21              
         BE    SET7                                                             
         LA    R1,3(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   SET5                                                             
         DC    H'0'                                                             
SET7     ZIC   R0,1(R1)                                                         
         LA    R1,SORTREC                                                       
         AR    R1,R0                                                            
         CLC   OLDDIV,0(R1)                                                     
         BE    SET9                                                             
         MVC   OLDDIV,0(R1)                                                     
         MVI   FORCEHED,C'Y'                                                    
SET9     CLC   QREGION,=C'ALL'                                                  
         BNE   SET12                                                            
         LA    R1,BRKKEY                                                        
SET10    CLI   0(R1),X'02'                                                      
         BE    SET11                                                            
         LA    R1,3(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   SET10                                                            
         DC    H'0'                                                             
SET11    ZIC   R0,1(R1)                                                         
         LA    R1,SORTREC                                                       
         AR    R1,R0                                                            
         CLC   OLDREG,3(R1)        3 OFF R1 SINCE REG=D/R/DST/SHR/SUM           
         BE    SET12                                                            
         MVC   OLDREG,3(R1)                                                     
         MVI   FORCEHED,C'Y'                                                    
SET12    CLC   QDIST,=C'ALL'                                                    
         BNE   SETX                                                             
         LA    R1,BRKKEY                                                        
SET14    CLI   0(R1),X'03'                                                      
         BE    SET15                                                            
         LA    R1,3(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   SET14                                                            
         DC    H'0'                                                             
SET15    ZIC   R0,1(R1)                                                         
         LA    R1,SORTREC                                                       
         AR    R1,R0                                                            
         CLC   OLDDST,0(R1)                                                     
         BE    SETX                                                             
         MVC   OLDDST,0(R1)                                                     
         MVI   FORCEHED,C'Y'                                                    
SETX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
OPNDIFF  NMOD1 0,OPNDIFF                                                        
         L     RC,PPFILEC                                                       
*                                                                               
         LA    R5,OPDFTAB                                                       
OPND10   CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),0(R5)                                                    
         BE    OPND20                                                           
         LA    R5,5(R5)                                                         
         B     OPND10                                                           
*                                                                               
OPND20   MVC   FULL,1(R5)                                                       
         L     RF,FULL                                                          
         BAS   RE,0(RF)                                                         
         BAS   RE,OADDCUMS                                                      
         XIT1  REGS=(R2)                                        BUG15           
*                                                                               
OADDCUMS DS    0H                                                               
         LA    R3,TEMPTOTS      ** R2=$ AMOUNT **                               
         CLI   0(R3),0             IS IT FIRST TIME                             
         BE    OACC10                                                           
OACC05   CLC   0(1,R3),ACCODE        NO. MATCH ACCODE                           
         BE    OACC12                    ELSE ADD AT END                        
         LA    R3,12(R3)                    OF TEMPTOTS.           L01          
         CLI   0(R3),0                                                          
         BNE   OACC05                                                           
OACC10   MVC   0(1,R3),ACCODE                                                   
* *C12   L     R5,4(R3)            ADD $ AMOUNT                                 
* *      AR    R5,R2                  IN R2 TO TEMPTOTS.                        
* *      ST    R5,4(R3)                                                         
OACC12   CVD   R2,DUB              ADD $ AMOUNT                    L01          
         AP    4(8,R3),DUB         IN R2 TO TEMPTOTS.              L01          
         MVC   2(2,R3),DDISP       SET P LINE DISP                              
*                                                             BUG08             
*   DETERMINE IF DIFFERENCE IN COLUMNS WAS REQUESTED          BUG08             
*                                                             BUG08             
         CLI   DIFFACCU,255                                   BUG08             
         BNE   NODIFFF                                        BUG08             
         CLI   ACCODE,93     DO NOT STORE AMOUNTS FOR THIS    BUG08             
         BE    NODIFFF                                        BUG08             
         L     R1,SAVEAPTR                                    BUG08             
         LA    R1,4(R1)                                       BUG08             
         ST    R1,SAVEAPTR    SAVE LAST USED FIELD            BUG08             
         ST    R2,0(R1)       SAVE AMOUNT                     BUG08             
NODIFFF  DS    0H                                             BUG08             
         BR    RE                                                               
*                                                                               
*    OPEN ROUTINES                                                              
OP20     L     R2,OGROSS            GROSS                                       
         MVI   ACCODE,140                                                       
         BR    RE                                                               
*                                                                               
OP21     L     R2,OPYABLE           NET                          L23            
         A     R2,OCSHDSC           (NET-CD +CD)                 L23            
         MVI   ACCODE,141                                                       
         BR    RE                                                               
*                                                                               
OP22     L     R2,OGROSS            GR-CD                                       
         S     R2,OCSHDSC                                                       
         MVI   ACCODE,142                                                       
         BR    RE                                                               
*                                                                               
OP23     L     R2,OPYABLE           NET-CD                       L23            
         MVI   ACCODE,143                                                       
         BR    RE                                                               
*                                                                               
OP24     L     R2,OAGYCOM           AGY COM                                     
         MVI   ACCODE,144                                                       
         BR    RE                                                               
*                                                                               
OP25     L     R2,OCSHDSC           CD                                          
         MVI   ACCODE,145                                                       
         BR    RE                                                               
*                                                                               
OP26     L     R2,OPGROSS         PAID  GROSS                                   
         MVI   ACCODE,150                                                       
         BR    RE                                                               
*                                                                               
OP27     L     R2,OPAID           PAID  NET                       L23           
         A     R2,OPCSHDSC        (NET-CD +CD)                    L23           
         MVI   ACCODE,151                                                       
         BR    RE                                                               
*                                                                               
OP28     L     R2,OPGROSS         PAID  GR-CD                                   
         S     R2,OPCSHDSC                                                      
         MVI   ACCODE,152                                                       
         BR    RE                                                               
*                                                                               
OP29     L     R2,OPAID          PAID   NET-CD                    L23           
         MVI   ACCODE,153                                                       
         BR    RE                                                               
*                                                                               
OP30     L     R2,OPAGYCOM          PAID AGY COM                                
         MVI   ACCODE,154                                                       
         BR    RE                                                               
*                                                                               
OP31     L     R2,OPCSHDSC          PAID CD                                     
         MVI   ACCODE,155                                                       
         BR    RE                                                               
*                                                                               
OP32     L     R2,OBGROSS         BILLED GROSS                                  
         MVI   ACCODE,160                                                       
         BR    RE                                                               
*                                                                               
OP33     L     R2,OBILLED         BILLED NET                       L23          
         A     R2,OBCSHDSC        (NET-CD +CD)                     L23          
         MVI   ACCODE,161                                                       
         BR    RE                                                               
*                                                                               
OP34     L     R2,OBGROSS         BILLED GR-CD                                  
         S     R2,OBCSHDSC                                                      
         MVI   ACCODE,162                                                       
         BR    RE                                                               
*                                                                               
OP35     L     R2,OBILLED        BILLED NET-CD                    L23           
         MVI   ACCODE,163                                                       
         BR    RE                                                               
*                                                                               
OP36     L     R2,OBAGYCOM          BILLED AGY COM                              
         MVI   ACCODE,164                                                       
         BR    RE                                                               
*                                                                               
OP37     L     R2,OBCSHDSC          BILLED CD                                   
         MVI   ACCODE,165                                                       
         BR    RE                                                               
*                                                                               
OP38     L     R2,OGROSS            UNPAID GROSS                                
         S     R2,OPGROSS                                                       
         MVI   ACCODE,170                                                       
         BR    RE                                                               
*                                                                               
OP39     L     RF,OPAID             UNPAID NET                    L23           
         A     RF,OPCSHDSC          (PAID NET) = (NET-CD +CD)     L23           
         L     R2,OPYABLE                                         L23           
         A     R2,OCSHDSC           (NET) = (NET-CD +CD)          L23           
         SR    R2,RF                                                            
         MVI   ACCODE,171                                                       
         BR    RE                                                               
*                                                                               
OP40     L     RF,OPGROSS           UNPAID GR-CD                                
         S     RF,OPCSHDSC          (PAID GR-CD)                                
         L     R2,OGROSS                                                        
         S     R2,OCSHDSC           (GR-CD)                                     
         SR    R2,RF                                                            
         MVI   ACCODE,172                                                       
         BR    RE                                                               
*                                                                               
OP41     L     RF,OPAID             UNPAID NET-CD               L23             
         L     R2,OPYABLE                                       L23             
         SR    R2,RF                                                            
         MVI   ACCODE,173                                                       
         BR    RE                                                               
*                                                                               
OP42     L     R2,OAGYCOM           UNPAID AGYCOM                               
         S     R2,OPAGYCOM                                                      
         MVI   ACCODE,174                                                       
         BR    RE                                                               
*                                                                               
OP43     L     R2,OCSHDSC           UNPAID CD                                   
         S     R2,OPCSHDSC                                                      
         MVI   ACCODE,175                                                       
         BR    RE                                                               
*                                                                               
OP44     L     R2,OGROSS            BILLABLE GROSS                              
         S     R2,OBGROSS                                                       
         MVI   ACCODE,180                                                       
         BR    RE                                                               
*                                                                               
OP45     L     RF,OBILLED           BILLABLE NET                 L23            
         A     RF,OBCSHDSC          (BILLED NET) = (NET-CD +CD)  L23            
         L     R2,OPYABLE                                        L23            
         A     R2,OCSHDSC           (NET) = (NET-CD +CD)         L23            
         SR    R2,RF                                                            
         MVI   ACCODE,181                                                       
         BR    RE                                                               
*                                                                               
OP46     L     RF,OBGROSS           BILLABLE GR-CD                              
         S     RF,OBCSHDSC          (BILLED GR-CD)                              
         L     R2,OGROSS                                                        
         S     R2,OCSHDSC           (GR-CD)                                     
         SR    R2,RF                                                            
         MVI   ACCODE,182                                                       
         BR    RE                                                               
*                                                                               
OP47     L     RF,OBILLED           BILLABLE NET-CD              L23            
         L     R2,OPYABLE           NET-CD                       L23            
         SR    R2,RF                                                            
         MVI   ACCODE,183                                                       
         BR    RE                                                               
*                                                                               
OP48     L     R2,OAGYCOM           BILLABLE AGY COM                            
         S     R2,OBAGYCOM                                                      
         MVI   ACCODE,184                                                       
         BR    RE                                                               
*                                                                               
OP49     L     R2,OCSHDSC           BILLABLE CD                                 
         S     R2,OBCSHDSC                                                      
         MVI   ACCODE,185                                                       
         BR    RE                                                               
*                                                                               
*     DIFFERENCE ROUTINES                                                       
DF20     L     R2,DGROSS            GROSS                                       
         MVI   ACCODE,200                                                       
         BR    RE                                                               
*                                                                               
DF21     L     R2,DPYABLE           NET                         L23             
         A     R2,DCSHDSC           (NET-CD +CD)                L23             
         MVI   ACCODE,201                                                       
         BR    RE                                                               
*                                                                               
DF22     L     R2,DGROSS            GR-CD                                       
         S     R2,DCSHDSC                                                       
         MVI   ACCODE,202                                                       
         BR    RE                                                               
*                                                                               
DF23     L     R2,DPYABLE           NET-CD                       L23            
         MVI   ACCODE,203                                                       
         BR    RE                                                               
*                                                                               
DF24     L     R2,DAGYCOM           AGY COM                                     
         MVI   ACCODE,204                                                       
         BR    RE                                                               
*                                                                               
DF25     L     R2,DCSHDSC           CD                                          
         MVI   ACCODE,205                                                       
         BR    RE                                                               
*                                                                               
DF26     L     R2,DPGROSS         PAID  GROSS                                   
         MVI   ACCODE,210                                                       
         BR    RE                                                               
*                                                                               
DF27     L     R2,DPAID           PAID  NET                        L23          
         A     R2,DPCSHDSC        (NET-CD + CD)                    L23          
         MVI   ACCODE,211                                                       
         BR    RE                                                               
*                                                                               
DF28     L     R2,DPGROSS         PAID  GR-CD                                   
         S     R2,DPCSHDSC                                                      
         MVI   ACCODE,212                                                       
         BR    RE                                                               
*                                                                               
DF29     L     R2,DPAID          PAID   NET-CD                     L23          
         MVI   ACCODE,213                                                       
         BR    RE                                                               
*                                                                               
DF30     L     R2,DPAGYCOM          PAID AGY COM                                
         MVI   ACCODE,214                                                       
         BR    RE                                                               
*                                                                               
DF31     L     R2,DPCSHDSC          PAID CD                                     
         MVI   ACCODE,215                                                       
         BR    RE                                                               
*                                                                               
DF32     L     R2,DBGROSS         BILLED GROSS                                  
         MVI   ACCODE,220                                                       
         BR    RE                                                               
*                                                                               
DF33     L     R2,DBILLED         BILLED NET                       L23          
         A     R2,DBCSHDSC        (NET-CD + CD)                    L23          
         MVI   ACCODE,221                                                       
         BR    RE                                                               
*                                                                               
DF34     L     R2,DBGROSS         BILLED GR-CD                                  
         S     R2,DBCSHDSC                                                      
         MVI   ACCODE,222                                                       
         BR    RE                                                               
*                                                                               
DF35     L     R2,DBILLED        BILLED NET-CD                    L23           
         MVI   ACCODE,223                                                       
         BR    RE                                                               
*                                                                               
DF36     L     R2,DBAGYCOM          BILLED AGY COM                              
         MVI   ACCODE,224                                                       
         BR    RE                                                               
*                                                                               
DF37     L     R2,DBCSHDSC          BILLED CD                                   
         MVI   ACCODE,225                                                       
         BR    RE                                                               
*                                                                               
DF38     L     R2,DGROSS            UNPAID GROSS                                
         S     R2,DPGROSS                                                       
         MVI   ACCODE,230                                                       
         BR    RE                                                               
*                                                                               
DF39     L     RF,DPAID             UNPAID NET                    L23           
         A     RF,DPCSHDSC          (PAID NET) = (NET-CD +CD)     L23           
         L     R2,DPYABLE                                         L23           
         A     R2,DCSHDSC           (NET) = (NET-CD +CD)          L23           
         SR    R2,RF                                                            
         MVI   ACCODE,231                                                       
         BR    RE                                                               
*                                                                               
DF40     L     RF,DPGROSS           UNPAID GR-CD                                
         S     RF,DPCSHDSC          (PAID GR-CD)                                
         L     R2,DGROSS                                                        
         S     R2,DCSHDSC           (GR-CD)                                     
         SR    R2,RF                                                            
         MVI   ACCODE,232                                                       
         BR    RE                                                               
*                                                                               
DF41     L     RF,DPAID             UNPAID NET-CD                L23            
         L     R2,DPYABLE                                        L23            
         SR    R2,RF                                                            
         MVI   ACCODE,233                                                       
         BR    RE                                                               
*                                                                               
DF42     L     R2,DAGYCOM           UNPAID AGYCOM                               
         S     R2,DPAGYCOM                                                      
         MVI   ACCODE,234                                                       
         BR    RE                                                               
*                                                                               
DF43     L     R2,DCSHDSC           UNPAID CD                                   
         S     R2,DPCSHDSC                                                      
         MVI   ACCODE,235                                                       
         BR    RE                                                               
*                                                                               
DF44     L     R2,DGROSS            BILLABLE GROSS                              
         S     R2,DBGROSS                                                       
         MVI   ACCODE,240                                                       
         BR    RE                                                               
*                                                                               
DF45     L     RF,DBILLED           BILLABLE NET                   L23          
         A     RF,DBCSHDSC          (BILLED NET) = (NET-CD +CD)    L23          
         L     R2,DPYABLE                                          L23          
         A     R2,DCSHDSC           (NET) = (NET-CD +CD)           L23          
         SR    R2,RF                                                            
         MVI   ACCODE,241                                                       
         BR    RE                                                               
*                                                                               
DF46     L     RF,DBGROSS           BILLABLE GR-CD                              
         S     RF,DBCSHDSC          (BILLED GR-CD)                              
         L     R2,DGROSS                                                        
         S     R2,DCSHDSC           (GR-CD)                                     
         SR    R2,RF                                                            
         MVI   ACCODE,242                                                       
         BR    RE                                                               
*                                                                               
DF47     L     RF,DBILLED          BILLABLE NET-CD              L23             
         L     R2,DPYABLE                                       L23             
         SR    R2,RF                                                            
         MVI   ACCODE,243                                                       
         BR    RE                                                               
*                                                                               
DF48     L     R2,DAGYCOM           BILLABLE AGY COM                            
         S     R2,DBAGYCOM                                                      
         MVI   ACCODE,244                                                       
         BR    RE                                                               
*                                                                               
DF49     L     R2,DCSHDSC           BILLABLE CD                                 
         S     R2,DBCSHDSC                                                      
         MVI   ACCODE,245                                                       
         BR    RE                                                               
*                                                                               
*                                                                               
***         OPEN AMOUNTS                                                        
OPDFTAB  DC    AL1(140),AL4(OP20)     ORDERED GROSS                             
         DC    AL1(141),AL4(OP21)     ORDERED NET                               
         DC    AL1(142),AL4(OP22)     ORDERED GR-CD                             
         DC    AL1(143),AL4(OP23)     ORDERED NET-CD                            
         DC    AL1(144),AL4(OP24)     ORDERED AGY COM                           
         DC    AL1(145),AL4(OP25)     ORDERED CD                                
         DC    AL1(150),AL4(OP26)     PAID GROSS                                
         DC    AL1(151),AL4(OP27)     PAID NET                                  
         DC    AL1(152),AL4(OP28)     PAID GR-CD                                
         DC    AL1(153),AL4(OP29)     PAID NET-CD                               
         DC    AL1(154),AL4(OP30)     PAID AGY COM                              
         DC    AL1(155),AL4(OP31)     PAID CD                                   
         DC    AL1(160),AL4(OP32)     BILLED GROSS                              
         DC    AL1(161),AL4(OP33)     BILLED NET                                
         DC    AL1(162),AL4(OP34)     BILLED GR-CD                              
         DC    AL1(163),AL4(OP35)     BILLED NET-CD                             
         DC    AL1(164),AL4(OP36)     BILLED AGY COM                            
         DC    AL1(165),AL4(OP37)     BILLED CD                                 
         DC    AL1(170),AL4(OP38)     UNPAID GROSS                              
         DC    AL1(171),AL4(OP39)     UNPAID NET                                
         DC    AL1(172),AL4(OP40)     UNPAID GR-CD                              
         DC    AL1(173),AL4(OP41)     UNPAID NET-CD                             
         DC    AL1(174),AL4(OP42)     UNPAID AGY COM                            
         DC    AL1(175),AL4(OP43)     UNPAID CD                                 
         DC    AL1(180),AL4(OP44)     BILLABLE GROSS                            
         DC    AL1(181),AL4(OP45)     BILLABLE NET                              
         DC    AL1(182),AL4(OP46)     BILLABLE GR-CD                            
         DC    AL1(183),AL4(OP47)     BILLABLE NET-CD                           
         DC    AL1(184),AL4(OP48)     BILLABLE AGY COM                          
         DC    AL1(185),AL4(OP49)     BILLABLE CD                               
*                                                                               
*           DIFF AMOUNTS                                                        
         DC    AL1(200),AL4(DF20)     ORDERED GROSS                             
         DC    AL1(201),AL4(DF21)     ORDERED NET                               
         DC    AL1(202),AL4(DF22)     ORDERED GR-CD                             
         DC    AL1(203),AL4(DF23)     ORDERED NET-CD                            
         DC    AL1(204),AL4(DF24)     ORDERED AGY COM                           
         DC    AL1(205),AL4(DF25)     ORDERED CD                                
         DC    AL1(210),AL4(DF26)     PAID GROSS                                
         DC    AL1(211),AL4(DF27)     PAID NET                                  
         DC    AL1(212),AL4(DF28)     PAID GR-CD                                
         DC    AL1(213),AL4(DF29)     PAID NET-CD                               
         DC    AL1(214),AL4(DF30)     PAID AGY COM                              
         DC    AL1(215),AL4(DF31)     PAID CD                                   
         DC    AL1(220),AL4(DF32)     BILLED GROSS                              
         DC    AL1(221),AL4(DF33)     BILLED NET                                
         DC    AL1(222),AL4(DF34)     BILLED GR-CD                              
         DC    AL1(223),AL4(DF35)     BILLED NET-CD                             
         DC    AL1(224),AL4(DF36)     BILLED AGY COM                            
         DC    AL1(225),AL4(DF37)     BILLED CD                                 
         DC    AL1(230),AL4(DF38)     UNPAID GROSS                              
         DC    AL1(231),AL4(DF39)     UNPAID NET                                
         DC    AL1(232),AL4(DF40)     UNPAID GR-CD                              
         DC    AL1(233),AL4(DF41)     UNPAID NET-CD                             
         DC    AL1(234),AL4(DF42)     UNPAID AGY COM                            
         DC    AL1(235),AL4(DF43)     UNPAID CD                                 
         DC    AL1(240),AL4(DF44)     BILLABLE GROSS                            
         DC    AL1(241),AL4(DF45)     BILLABLE NET                              
         DC    AL1(242),AL4(DF46)     BILLABLE GR-CD                            
         DC    AL1(243),AL4(DF47)     BILLABLE NET-CD                           
         DC    AL1(244),AL4(DF48)     BILLABLE AGY COM                          
         DC    AL1(245),AL4(DF49)     BILLABLE CD                               
         DC    X'FFFF'                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***             END OF CSECT OPNDIFF                                            
*                                                                               
RDSPLIT  NMOD1 0,RDSPLIT                                                        
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
*        CLI   QPROG,C'7'          NO REG/DSTS FOR IO PROGS                     
*        BE    RDSPLITX                                                         
*        L     RF,0(R1)                                                         
*        MVC   DOUBLE(6),0(RF)     REG/DIST                                     
         MVC   DOUBLE(3),OLDREG    REG                                          
         MVC   DOUBLE+3(3),OLDDST  DIST                                         
         OC    DOUBLE(6),SPACES                                                 
         MVC   RDDIFS(48),GROSS                                                 
         MVC   RDDIFS+48(16),BGROSS    BILLED DATA                              
         XC    RDSUMS,RDSUMS                                                    
         XC    RDBLSUMS,RDBLSUMS                                                
         XC    FULL,FULL                                                        
*        MVC   RDCLTDIV(3),PCLTKCLT     SET CLT/DIV                             
*        MVC   RDCLTDIV+3(3),PPRDDIV                                            
         MVC   RDCLTDIV(3),QCLIENT      CLIENT                                  
*    IF USING DRD SCHEME USE CLIENT IN QPUB+3                                   
         CLI   DRDSW,255                                                        
         BNE   *+10                                                             
         MVC   RDCLTDIV(3),QPUB+4       PSUEDO PUB                              
         MVC   RDCLTDIV+3(3),OLDDIV     DIV                                     
*        CLC   QPUB+1(3),=C'RD='        REQUESTED REG/DST OVERRIDE              
*        BNE   *+16                                                             
*        MVC   RDCLTDIV(3),QPUB+4                                               
*        MVC   RDCLTDIV+3(3),=C'000'                                            
*                                                                               
*                                  FIRST PASS                                   
         L     R2,ALTLREC                                                       
         LA    R2,33(R2)                                                        
         MVC   WORK,0(R2)      *** FOR CHK /LTLREC NOT IN DUMP AREA             
         CLI   0(R2),X'71'                                                      
         BE    RDSP4                                                            
RDSP3    DS    0H                                                               
         BAS   RE,RDNXTEL                                                       
         BNE   RDSP8                                                            
RDSP4    DS    0H                                                               
         USING PUBDSTEL,R2                                                      
         OC    PUBDCLT,SPACES                                                   
         CLC   PUBDCLT(6),RDCLTDIV                                              
         MVC   WORK(6),PUBDCLT                                                  
         BNE   RDSP3                                                            
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),PUBDSHR                                                
         LA    R4,GROSS                                                         
         LA    R3,RDDIFS                                                        
         ZAP   COUNT,=P'1'                                                      
RDSP5    DS    0H                                                               
         L     R1,0(R4)                                                         
         LTR   R1,R1                                                            
         BZ    RDSP6                                                            
         M     R0,FULL                                                          
         BAS   RE,RDDIV            ROUNDED DIVISION                             
         L     R0,0(R3)                                                         
         SR    R0,R1                                                            
         ST    R0,0(R3)                                                         
RDSP6    DS    0H                                                               
         CP    COUNT,=P'16'                                                     
         BE    RDSP3                                                            
         AP    COUNT,=P'1'                                                      
RDSP6C   LA    R4,4(R4)                                                         
         LA    R3,4(R3)                                                         
RDSP6D   B     RDSP5               NEXT AMOUNT                                  
*                                                                               
*                                                                               
RDSP8    DS    0H                                                               
         OC    FULL,FULL                                                        
         BZ    RDSPLITX            NO SHARES                                    
*                                  END OF PASS 1                                
*                                  NOW RDDIFS HAS DIFFERENCES                   
*                                  BETWEEN ORIGINAL AMOUNTS AND                 
*                                  SUM OF ROUNDED SHARES                        
*                                                                               
*                                                                               
*                                  PASS 2                                       
         L     R2,ALTLREC                                                       
         LA    R2,33(R2)                                                        
         CLI   0(R2),X'71'                                                      
         BE    RDSP10                                                           
RDSP9    DS    0H                                                               
         BAS   RE,RDNXTEL                                                       
         BNE   RDSP30                                                           
RDSP10   DS    0H                                                               
         CLC   PUBDCLT(6),RDCLTDIV                                              
         BNE   RDSP9                                                            
*                                  IS THIS A SELECTED REG/DST                   
         MVI   DOUBLE+6,0               NO                                      
         CLC   DOUBLE(3),=C'ALL'                                                
         BE    RDSP11                                                           
         CLC   DOUBLE(3),SPACES                                                 
         BE    RDSP11                                                           
         CLC   DOUBLE(3),PUBDREG                                                
         BH    RDSP13                                                           
         BL    RDSP30                                                           
RDSP11   DS    0H                                                               
         CLC   DOUBLE+3(3),=C'ALL'                                              
         BE    RDSP12                                                           
         CLC   DOUBLE+3(3),SPACES                                               
         BE    RDSP12                                                           
         CLC   DOUBLE+3(3),PUBDDST                                              
         BH    RDSP13                                                           
         BL    RDSP30                                                           
RDSP12   DS    0H                                                               
         MVI   DOUBLE+6,1               YES                                     
RDSP13   DS    0H                                                               
         MVC   FULL+2(2),PUBDSHR                                                
         LA    R4,GROSS                                                         
         LA    R3,RDDIFS                                                        
         ZAP   COUNT,=P'1'                                                      
RDSP13A  DS    0H                                                               
         OC    0(4,R4),0(R4)                                                    
         BZ    RDSP20              NO AMOUNT                                    
         OC    0(4,R3),0(R3)                                                    
         BNZ   RDSP14                                                           
*                                  NO DIFF                                      
         CLI   DOUBLE+6,0          TEST IF A SELECTED REG/DST                   
         BE    RDSP20              NO                                           
         L     R1,0(R4)                                                         
         M     R0,FULL             YES COMPUTE SHARE                            
         BAS   RE,RDDIV                                                         
*                                  AND ADD TO RDSUMS                            
RDSP13B  DS    0H                                                               
         A     R1,64(R3)                                                        
         ST    R1,64(R3)                                                        
         B     RDSP20                                                           
RDSP13D  DS    0H                                                               
         CLI   DOUBLE+6,0          TEST IF A SELECTED REG/DST                   
         BE    RDSP20              NO                                           
         B     RDSP13B             YES                                          
*                                  DIFFERENCE NO-ZERO                           
RDSP14   DS    0H                                                               
         L     R1,0(R4)                                                         
         M     R0,FULL             SHARE                                        
         D     R0,=F'10000'        TRUNCATED DIVISION                           
         LTR   R0,R0               TEST REMAINDER                               
         BZ    RDSP13D             ZERO                                         
         BM    RDSP17              NEG                                          
*                                                                               
*                                  AMOUNT POS                                   
         TM    0(R3),X'80'         TEST DIFF                                    
         BNZ   RDSP15                                                           
*                                  AMOUNT POS, DIFF POS                         
         C     R0,=F'5000'         TEST IF ROUNDED DOWN                         
         BNL   RDSP13D             NO                                           
         L     RF,0(R3)            YES - SUBTRACT 1 FROM DIFF                   
         BCTR  RF,R0                                                            
         ST    RF,0(R3)                                                         
         A     R1,=F'1'            ADD 1 TO AMOUNT                              
         B     RDSP13D                                                          
*                                  AMOUNT POS, DIFF NEG                         
RDSP15   DS    0H                                                               
         C     R0,=F'5000'         TEST IF ROUNDED UP                           
         BL    RDSP13D             NO                                           
         L     RF,0(R3)            YES - ADD 1 TO DIFF                          
         A     RF,=F'1'                                                         
         ST    RF,0(R3)                                                         
         B     RDSP13D                                                          
*                                                                               
*                                  AMOUNT NEG                                   
RDSP17   DS    0H                                                               
         TM    0(R3),X'80'         TEST DIFF                                    
         BZ    RDSP18                                                           
*                                  AMOUNT NEG, DIFF NEG                         
         C     R0,=F'-5000'       TEST IF ROUNDED UP                            
         BL    RDSP13D             NO                                           
         L     RF,0(R3)            YES - ADD 1 TO DIFF                          
         A     RF,=F'1'                                                         
         ST    RF,0(R3)                                                         
         BCTR  R1,R0               SUBTRACT 1 FROM AMT                          
         B     RDSP13D                                                          
*                                  AMOUNT NEG, DIFF POS                         
RDSP18   DS    0H                                                               
         C     R0,=F'-5000'        TEST IF ROUNDED DOWN                         
         BNL   RDSP13D             NO                                           
         L     RF,0(R3)            YES - SUB 1 FROM DIFF                        
         BCTR  RF,R0                                                            
         ST    RF,0(R3)                                                         
         B     RDSP13D                                                          
*                                                                               
*                                                                               
RDSP20   DS    0H                                                               
         CP    COUNT,=P'16'                                                     
         BE    RDSP9                                                            
         AP    COUNT,=P'1'                                                      
RDSP23   LA    R4,4(R4)                                                         
         LA    R3,4(R3)                                                         
RDSP24   B     RDSP13A             NEXT AMOUNT                                  
*                                                                               
RDSP30   DS    0H                                                               
         MVC   GROSS(48),RDSUMS                                                 
         MVC   BGROSS(16),RDBLSUMS                                              
*                                                                               
*                                                                               
RDSPLITX DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
RDNXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    RDNXTEL2                                                         
         CLI   0(R2),X'71'                                                      
         BER   RE                                                               
         B     RDNXTEL+2                                                        
RDNXTEL2 LTR   R2,R2                                                            
         BR    RE                                                               
         SPACE 3                                                                
RDDIV    DS    0H                                                               
         SLDA  R0,1                                                             
         D     R0,=F'10000'                                                     
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
*                                                                               
         DS    0F                                                               
RDDIFS   DS    CL64                                                             
RDSUMS   DS    CL48                                                             
RDBLSUMS DS    CL16                                                             
RDCLTDIV DS    CL6                                                              
COUNT    DC    PL2'0'                                                           
*                                                                               
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*          DATA SET DOWNLOAD   AT LEVEL 042 AS OF 01/08/87                      
********************************************                                    
* DOWNLOAD ROUTINE                                                              
*                                                                               
*                                                                               
DOWN     NMOD1 0,**DOWNLD                                                       
         L     RC,PPFILEC                                                       
*                                                                               
         LA    R4,DOWNWORK                                                      
         USING DLCBD,R4                                                         
         LA    R1,PRNTLN                                                        
         ST    R1,DLCBAPL          PRINT LINE                                   
         MVI   PRNTLN,X'40'                                                     
         MVC   PRNTLN+1(L'PRNTLN-1),PRNTLN                                      
         LA    R1,DOWNHK                                                        
         ST    R1,DLCBAPR          PRINT ROUTINE                                
*                                                                               
         CLI   FRSTPAGE,C'Y'       FIRST PAGE                                   
         BE    FRSTDN                                                           
         CLC   =C'NO DATA',XP1                                                  
         BE    DNNODATA                                                         
         CLI   ENDRPRT,C'Y'        END OF REPORT                                
         BE    DNLDEND                                                          
*                                                                               
         LA    R2,RPTTAB                                                        
         L     R3,AP1                                                           
DNLOOP   CLI   2(R2),X'FF'         TEST IF DISPLAY FIELD                        
         BE    DN12                NO/SKIP                                      
         LA    R5,DLCBFLD          DATA FIELD                                   
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'N'        TYPE(NUMERIC)                                
         CLI   0(R2),40            CHK CODE(ABOVE 40 IS DOLLARS)                
         BL    DNTEXT                                                           
         BAS   RE,LEFTADJS                                                      
         B     DN9                                                              
DNTEXT   MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         ZIC   R1,2(R2)            GET DISPLAY LENGTH                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R3)         MOVE PRINT LINE DATA TO DATA FIELD         
DN9      LR    R1,R4                                                            
         L     RF,=V(DLFLD)                                                     
         BASR  RE,RF                                                            
*                                                                               
DN10     DS    0H                                                               
         ZIC   R1,2(R2)            GET DISPLAY LENGTH                           
         AR    R3,R1               ADD DIPLAY LENGTH TO PRINT LINE              
*                                  NOW R3 POINTS TO NEXT DATA FIELD             
*                                                                               
DN12     LA    R2,5(R2)            BUMP RPTTAB                                  
         CLC   0(2,R2),=X'FFFF'                                                 
         BNE   DNLOOP                                                           
*                                                                               
         MVI   DLCBACT,C'L'        END OF PRINT LINE                            
         B     DN15                                                             
*                                                                               
DNLDEND  MVI   DLCBACT,C'R'        END OF REPORT                                
*                                                                               
DN15     DS    0H                                                               
         LR    R1,R4                                                            
         L     RF,=V(DLFLD)                                                     
         BASR  RE,RF                                                            
DOWNX    XIT1                                                                   
         SPACE                                                                  
*                                                                               
*  LEFT ADJUST NUMERIC FIELDS FOR EFFICIENT DOWNLOADING                         
*  R3-POINTS TO PRINT LINE, R2-RPTTAB, R5-OUT FIELD                             
*                                                                               
LEFTADJS NTR1                                                                   
         ZIC   R1,2(R2)            DISPLAY LENGTH                               
LFT3     CLI   0(R3),X'40'                                                      
         BH    LFT5                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,LFT3                                                          
****     DC    H'0'                BOMB IF HERE/MUST BE DATA                    
         MVI   0(R5),C'0'          *** LETS TRY SENDING ONE ZERO                
         B     LFTX                                                             
LFT5     BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R3)                                                    
LFTX     XIT1                                                                   
*                                                                               
*                                                                               
FRSTDN   MVI   FRSTPAGE,0          START OF REPORT                              
         MVI   DLCBACT,C'S'                                                     
         MVI   DLCBFLD,X'40'                                                    
         MVC   DLCBFLD+1(L'DLCBFLD-1),DLCBFLD                                   
         GOTO1 PRINT,DMCB,PRNTLN,=C'BC01'     TO START PAGE                     
         B     DN15                                                             
*                                                                               
DNNODATA DS    0H                  NO DATA TO SEND                              
         MVC   DLCBFLD(7),XP1                                                   
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,C'P'                                                     
         LR    R1,R4                                                            
         L     RF,=V(DLFLD)                                                     
         BASR  RE,RF                                                            
         MVI   DLCBACT,C'L'                                                     
         LR    R1,R4                                                            
         L     RF,=V(DLFLD)                                                     
         BASR  RE,RF                                                            
         B     DNLDEND                                                          
*                                                                               
DOWNHK   NTR1                                                                   
         PRINT GEN                                                              
         GOTO1 PRINT,DMCB,PRNTLN,=C'BL01'                                       
         PRINT NOGEN                                                            
         XIT1                                                                   
         LTORG                                                                  
DOWNWORK DS    CL100                                              L08           
PRNTLN   DS    CL168                                              L08           
*                                                                 L08           
***                                                                             
*                                                                               
         EJECT                                                                  
DP98     NMOD1 0,DP98                                             L08           
         USING PPWORKD,RA                                         L08           
         USING PPWORK2D,R7                                        L08           
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                         L08           
         USING PP81WRKD,R8                                        L08           
         USING DP98,RB                                         L08              
         CLC   PBDJOB,SPACES             IF NO ADCODE CAN BE NO LAST            
         BNH   DP98Y                     AD USE                                 
*                                                                               
         MVC   SAVKEYS(25),PBUYREC       SAVE EXISTING KEYS                     
         MVC   SVADCD,PBDJOB             SAVE THIS JOBCODE                      
         MVC   SKPUB(18),PBDSPACE        USING SKPUB TO SAVE PBDSPACE           
         XC    SVLSTDAT(4),SVLSTDAT                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(14),PBUYREC                                                  
* BUY-IO READ BUY DIR                                                           
         GOTO1 HIGH                                                             
DP98A    CLC   KEY(14),KEYSAVE           SAME CLI,PRD,PUB                       
         BNE   DP98R                                                            
         TM    KEY+25,X'80'              BYPASS DELETES                         
         BO    DP98C                                                            
         LA    R0,PBUYREC                IF PASSED ALL THESE TESTS GET          
         ST    R0,AREC                   BUYREC                                 
* BUY-IO READ BUY REC                                                           
         GOTO1 GETPRT                                                           
         CLC   SVADCD,PBDJOB             IF ADCODE NOT SAME SKIP AND            
         BNE   DP98C                     GET NEXT                               
*                                                                               
         SR    R3,R3                                                            
         LA    R3,8                                                             
         CLI   PBUYKMED,C'N'                                                    
         BE    DP98A5                                                           
         LA    R3,17                                                            
DP98A5   EX    R3,DP98EX                                                        
         BNE   DP98C                                                            
         B     DP98F                                                            
DP98EX   CLC   SKPUB(0),PBDSPACE         USED SKPUB TO SAVE PBDSPACE            
* BUY-IO READ SEQUENTIAL                                                        
DP98C    GOTO1 SEQ                                                              
         B     DP98A                                                            
*                                                                               
DP98D    GOTO1 SEQ                                                              
         CLC   KEY(14),KEYSAVE           SAME CLI,PRD,PUB                       
         BNE   DP98R                                                            
         TM    KEY+25,X'80'              BYPASS DELETES                         
         BO    DP98D                                                            
         LA    R0,PBUYREC                IF PASSED ALL THESE TESTS GET          
         ST    R0,AREC                   BUYREC                                 
* BUY-IO READ BUY RECORD                                                        
         GOTO1 GETPRT                                                           
         CLC   SVADCD,PBDJOB             IF ADCODE NOT SAME SKIP AND            
         BNE   DP98D                     GET NEXT                               
         MVI   MLTREC,C'Y'                                                      
*                                                                               
         SR    R3,R3                                                            
         LA    R3,8                                                             
         CLI   PBUYKMED,C'N'                                                    
         BE    DP98D5                                                           
         LA    R3,17                                                            
DP98D5   EX    R3,DP98EX                                                        
         BNE   DP98D                                                            
*                                                                               
*                                                                               
DP98F    CLC   PBUYKDAT,SAVKEYS+16                                              
         BNL   DP98D                                                            
         CLC   SVLSTDAT,PBUYKDAT                                                
         BH    DP98D                                                            
DP98J    MVC   SVLSTDAT,PBUYKDAT                                                
         B     DP98D                                                            
*                                                                               
DP98R    DS    0H                                                               
         L     R3,AP1                                                           
         AH    R3,DDISP                                                         
         CLI   MLTREC,C'Y'                                                      
         BE    DP98S                                                            
DP98R5   MVC   0(3,R3),=C'NEW'                                                  
         B     DP98X                                                            
DP98S    CLC   SVLSTDAT,SAVKEYS+16                                              
         BE    DP98R5                                                           
         OC    SVLSTDAT,SVLSTDAT                                                
         BZ    DP98R5                                                           
*        GOTO1 DTCNV,DMCB,(1,SVLSTDAT),(3,0(R3))                                
         GOTO1 DATCON,DMCB,(3,SVLSTDAT),(5,0(R3))                               
DP98X    MVC   KEY(25),SAVKEYS           DISC ADDRESS OF ORIGINAL BUY           
         GOTO1 HIGH                                                             
         LA    R0,PBUYREC                IF PASSED ALL THESE TESTS GET          
         ST    R0,AREC                   BUYREC                                 
         GOTO1 GETPRT                                                           
*                                                                               
DP98Y    LH    R0,DDISP                                                         
         ZIC   R3,2(R4)                                                         
         AR    R0,R3                                                            
         STH   R0,DDISP                                                         
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                 L08           
*  R2 POINTS TO COMMENT ID FOR KEY                                L08           
*                                                                 L08           
COMPRT0  NMOD1  0,COMPRT0                                         L08           
         USING PPWORKD,RA                                         L08           
         USING PPWORK2D,R7                                        L08           
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                         L08           
         USING PP81WRKD,R8                                        L08           
         USING COMPRT0,RB                                         L08           
*                                                                 L08           
*                                                                 L08           
         MVC   WORK(32),KEY        SAVE KEY FOR PPG                             
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),0(R2)                                                   
         BAS   RE,COMHI                                                         
         CLC   KEY(10),KEYSAVE                                                  
         BNE   COMPRTX2                                                         
         L     R6,AWIDEC                                          L08           
         USING WIDED,R6                                           L08           
         LA    RE,XP1+42                                          L08           
         DROP  R6                                                 L08           
*                                                                 L08           
         GOTO1 =A(TLOUT),WORK,(RA),=C'COMP' FORCE TO CHECK TOP    L08           
         BAS   RE,COMGET                                                        
*                                  RESTORE PPG READ                             
         MVC   KEY,WORK                                                         
         BAS   RE,COMHI                                                         
* NOTE - COMMENT REC READ INTO PLISREC                                          
         L     R2,ALISREC     POINT TO BEGINING OF RECORD         L08           
         LA    R2,33(R2)      TO FIRST ELEMENT                    L08           
         LA    RF,0           DETERMINE LINES NEEDED F            L08           
RCOMPRT2 CLI   0(R2),0        END OF ELEMENTS                    L08            
         BE    ENDOFREC                                           L08           
         CLI   0(R2),X'40'    COMMENT ELEMENT                     L08           
         BNE   *+8                                                L08           
         LA    RF,1(RF)       UP COUNT                            L08           
         SR    R0,R0                                              L08           
         IC    R0,1(R2)       ELEMENT LENGTH                      L08           
         AR    R2,R0          POINT TO NEXT                       L08           
         B     RCOMPRT2                                           L08           
ENDOFREC STC   RF,LINENEED                                        L08           
COMPRT1  DS    0H                                                               
         L     R2,ALISREC                                                       
         LA    R2,33(R2)                                                        
COMPRT2  DS    0H                                                               
         CLI   0(R2),0             EOR                                          
         BE    COMPRTX                                                          
         CLI   0(R2),X'40'                                                      
         BE    COMPRT6                                                          
COMPRT4  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     COMPRT2                                                          
COMPRT6  DS    0H                                                               
*                                                                               
         CLC   =C'++START',2(R2)   IF ++START                                   
         BE    COMPRT4                                            L08           
COMPRT6B DS    0H                                                               
         CLC   =C'++END',2(R2)     IF ++END                                     
         BNE   COMPRT6D                                                         
         BE    COMPRT4                                            L08           
COMPRT6D DS    0H                                                               
         LA    R4,2(R2)                                                         
         SR    R5,R5                                                            
         IC    R5,1(R2)                                                         
         SH    R5,=H'2'                                                         
         CLI   0(R4),C'+'                                                       
         BNE   COMPRT8                                                          
         GOTO1 =A(TLOUT),WORK,(RA),=C'COMP' FORCE TO CHECK TOP                  
         MVI   LINENEED,0                                         L08           
         LA    R4,2(R4)                                                         
         SH    R5,=H'2'                                                         
         GOTO1 =A(TLOUT),WORK,(RA),=C'COMP' FORCE TO CHECK TOP                  
         MVI   LINENEED,0                                         L08           
COMPRT8  DS    0H                                                               
         LTR   R5,R5                                                            
         BNP   COMPRT4                                                          
         L     R6,AWIDEC                                          L08           
         USING WIDED,R6                                           L08           
         LA    RE,XP1+42                                          L08           
         DROP  R6                                                 L08           
*                                                                 L08           
*                                                                 L08           
         BCTR  R5,R0                                                            
         EX    R5,COMMVC                                                        
         GOTO1 =A(TLOUT),WORK,(RA),=C'COMP' FORCE TO CHECK TOP    L08           
         MVI   LINENEED,0                                         L08           
         B     COMPRT4           LOOP THRU ELEMENTS                             
*******                                                                         
COMMVC   MVC   0(0,RE),0(R4)                                      L08           
COMPRTX  DS    0H                                                               
*                                                                               
COMPRTXX DS    0H                                                               
COMPRTX2 DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
COMHI    MVC   KEYSAVE,KEY                                                      
         ST    RE,SAVERE                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),PRTDIR,KEY,KEY                     
         SPACE 1                                                                
         B     COMCHK                                                           
COMGET   ST    RE,SAVERE                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),PRTFILE,KEY+27,           X        
               ALISREC,DMWORK                                                   
         SPACE 1                                                                
COMCHK   DS    0H                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
WORKX    DS    CL12                                               L08           
*                                                                 L08           
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
NAMETBL  DC    AL1(01,14),CL22'*CLIENT TOTAL*'                                  
         DC    AL1(02,14),CL22'*REGION TOTAL*'                                  
         DC    AL1(03,14),CL22'*DISTRICT TOTAL*'                                
         DC    AL1(249,14),CL22'*DIVISION TOTAL*'                               
         DC    AL1(10,19),CL22'*PUBLICATION TOTAL*'                             
         DC    AL1(11,19),CL22'*PUBLICATION TOTAL*'                             
         DC    AL1(12,15),CL22'*PRODUCT TOTAL*'                                 
         DC    AL1(13,15),CL22'*PRODUCT TOTAL*'                                 
         DC    AL1(14,16),CL22'*ESTIMATE TOTAL*'                                
         DC    AL1(15,18),CL22'*SPACE DSCR TOTAL*'                              
         DC    AL1(16,16),CL22'*DIVISION TOTAL*'                                
         DC    AL1(17,15),CL22'*AD CODE TOTAL*'                                 
         DC    AL1(18,16),CL22'*COPY NUM TOTAL*'                                
         DC    AL1(19,16),CL22'*COMMENTS TOTAL*'                                
         DC    AL1(20,17),CL22'*EST DESCR TOTAL*'                               
         DC    AL1(21,12),CL22'*ASPO TOTAL*'                      L02           
         DC    AL1(22,17),CL22'*LIST CODE TOTAL*'              BUG02            
         DC     AL1(24,16),CL22'*CAPTION TOTAL*'                BUG02           
         DC    AL1(27,15),CL22'*AD CODE TOTAL*'                                 
         DC    AL1(28,14),CL22'*DIVISION TOTAL*'                 BUG16          
         DC    AL1(30,20),CL22'*INSERT MONTH TOTAL*'                            
         DC    AL1(31,19),CL22'*CLOSE MONTH TOTAL*'                             
         DC    AL1(32,21),CL22'*ON-SALE MONTH TOTAL*'                           
         DC    AL1(33,21),CL22'*PAYABLE MONTH TOTAL*'                           
         DC    AL1(34,22),CL22'*BILLABLE MONTH TOTAL*'                          
         DC    AL1(35,22),CL22'*MAT CLOSE MTH TOTAL*'                           
         DC    AL1(36,22),CL22'*AGENCY TOTAL*'               L19                
         DC    AL1(37,22),CL22'*AGENCY TOTAL*'               L19                
         DC    AL1(38,22),CL22'*AGENCY TOTAL*'               L19                
         DC    X'00'                                                            
*                                                                               
*                                                                               
PPBYOWRK DS    CL600                                                            
*DSAVE   CSECT      **** 3K SAVE AREA FOR DIV/REG/DIST/SHR/SUM  ***             
*                   **** FOR X'71' ELEMS OF LTLREC              ***             
RDSAVE   DS    CL3000    USED IN TLIN TO STORE MULT X'71' ELEMS                 
*                        USED IN TLOUT AT PRT06, ONLY 1ST 20 BYTES              
*        CSECT                                                                  
WEEKDATE DS    CL241  2 TWO BYTE START & END DATES + X'FF'        L10           
*                                                                 L10           
*                                                                 L10           
*                                                                 L16           
          TITLE 'FIND CAPTION OR COPY IN COMMENT'                 L16           
*OMCAPT   CSECT                                                                 
COMCAPT   NMOD1 0,COMCAP                                          L16           
         USING PPWORKD,RA                                         L16           
         USING PPWORK2D,R7                                        L16           
         USING PPFILED,RC,R9                                      L16           
         USING PP81WRKD,R8                                        L16           
*                                                                 L16           
* RC IS DESTROYED BY NMOD                                         L16           
*                                                                 L16           
         L     RC,PPFILEC        RESTORE RC                       L16           
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
*                                                                 L16           
*                                                                 L16           
         XC    MSGAREA,MSGAREA                                    L16           
         MVC   R1SAVE,0(R1)      SAVE INDICATOR                   L16           
         MVC   COMPOPT,COPYEQ    ASSUME COPY MOVE EXECUTE LENGTH  L16           
         CLI   R1SAVE,C'C'       MAX LENGTH OF MOVE AND CONSTANT  L16           
         BE    *+10                                               L16           
         MVC   COMPOPT,CAPTEQ                                     L16           
*                                                                 L16           
*  PREPARE TO READ COMMENT ELEMENTS                               L16           
*                                                                 L16           
         LA    RE,MSGAREA                                         L16           
         ST    RE,4(R1)          GIVE CALLER ADDRESS OF AREA      L16           
         LA    R2,PBUYREC+33                                      L16           
         MVI   ELCODE,X'66'      COMMENT ELEMENT ID               L16           
COMLOOP  BAS   RE,LOOPCOMM                                        L16           
         BNE   NOMORE66                                           L16           
         ZIC   RE,COMPOPT         INSERT LENGTH -1                L16           
         EX    RE,*+8             CHECK FOR CAP= OR COPY=        L16            
         B     *+10                                               L16           
         CLC   2(0,R2),COMPOPT+2                                  L16           
         BNE   COMLOOP            HIT ON CAP= OR COPY=            L16           
         LR    R1,R2              SAVE ELEMENT ADDRESS            L16           
         LA    R1,3(RE,R1)   PUSH PAST EL LEN,ID AND LEN OF COMPARE16           
         ZIC   RF,1(R2)      LENGTH OF ELEMENT                    L16           
         SH    RF,=H'4'      REDUCE BY EL ID, LEN AND FOR EXECUTE L16           
         SR    RF,RE         SHOULD CONTAIN LENGTH OF COMMENT     L16           
         ZIC   RE,COMPOPT+1  LIMIT ON MOVE                        L16           
         CR    RE,RF                                              L16           
         BH    *+6                                                L16           
         LR    RF,RE         USE MAX LIMIT                        L16           
         LTR   RF,RF         ENSURE POSITIVE                      L16           
         BNM   *+6           B ON NOT MINUS                       L16           
         LR    RF,RE                                              L16           
         LA    RE,MSGAREA                                         L16           
         CLI   COMPOPT,3     CAN HAVE MORE THAN 1 CAPTION         L16           
         BNE   JUSTCOPY                                           L16           
         OC    0(8,RE),0(RE) WAS A CAPTION MOVED IN ALREADY       L16           
         BZ    *+8                                                L16           
         LA    RE,25(RE)                                         L16            
JUSTCOPY EX    RF,*+8                                             L16           
         B     *+10                                               L16           
         MVC   0(0,RE),0(R1)                                      L16           
         B     COMLOOP        FORCE TO END OF COMMENTS            L16           
NOMORE66 XIT1                                                     L16           
*                                                                 L16           
*                                                                 L16           
*                                                                 L16           
LOOPCOMM ZIC   R0,1(R2)     LOAD ELEMENT LENGTH                   L16           
         AR    R2,R0        GET TO NEXT ELEMENT                   L16           
         CLI   0(R2),0      END OF RECORD                         L16           
         BE    NXXTL                                              L16           
         CLC   ELCODE,0(R2)                                       L16           
         BER   RE                                                 L16           
         B     LOOPCOMM     CONTINUE                              L16           
NXXTL    LTR   RE,RE        FORCE CONDITION CODE TO BNE           L16           
         BR    RE                                                 L16           
*                                                                 L16           
MSGAREA  DS    CL60                                               L16           
*                                                                 L16           
COMPOPT  DS    CL7                                                L16           
*                                                                 L16           
COPYEQ   DC    X'0416',C'COPY='  LEN OF COMPARE/ MOVE LIMIT       L16           
CAPTEQ   DC    X'0324',C'CAP= '  LEN OF COMPARE/ MOVE LIMIT       L16           
R1SAVE   DS    F                                                  L16           
         LTORG                                                    L16           
         XIT1                                                     BUG10         
SPECIALA DS    3CL198                                                           
*                                                                 BUG10         
         LTORG                                                                  
*                                                                 BUG10         
         TITLE 'DP01  CLIENT CODE      '                          L17           
*P01     CSECT                                                    L17           
DP01      NMOD1 0,DP01                                            L17           
         USING PPWORKD,RA                                         L17           
         USING PPWORK2D,R7                                        L17           
         USING PPFILED,RC,R9                                      L17           
         USING PP81WRKD,R8                                        L17           
*                                                                 L17           
* RC IS DESTROYED BY NMOD                                         L17           
*                                                                 L17           
         L     RC,PPFILEC        RESTORE RC                       L17           
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         CLC   OLDPUB,PBUYKPUB    IF PUB REMAINS THE SAME MOVE IN DITTO         
         BNE   DP01A                                                            
         CLI   RCMULTIQ,C'Y'                                                    
         BNE   TLCKHDY                                                          
         CLI   ANYPRTSW,C'N'                                                    
         BNE   TLCKHDY                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVI   ANYPRTSW,C'Y'                                                    
         B     ARNDTLC                                                          
TLCKHDY  SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    R0,LINE                                                          
         IC    RF,LINENEED                                                      
         AR    R0,RF                                                            
*                                                                               
         ZIC   RF,SPACING                                  BUG28                
         CLC   RCSPACNG,SPACING                            BUG28                
         BNH   *+8                                         BUG28                
         IC    RF,RCSPACNG                                 BUG28                
         BCTR  RF,0                                        BUG28                
         AR    R0,RF                                       BUG28                
*                                                                               
         STC   R0,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    ARNDTLC                                                          
         MVI   FORCEHED,C'Y'                                                    
ARNDTLC  DS    0H                                                               
         CLI   FORCEHED,C'Y'      ON PAGE BREAK                                 
         BE    DP01A              MOVE IN NAME.                                 
         L     R5,AP1                                                           
         LH    R0,DDISP                                                         
         AR    R5,R0                                                            
         OC    0(2,R5),=X'4040'    ONLY MOVE IN DITTOS IF LINE IS               
         CLC   0(2,R5),=X'4040'    BLANK/PREVENTS DITTO FROM COVERING           
         BNE   DP01B               A NUMBER                                     
         MVC   3(2,R5),=C''''''                                                 
         B     DP01B                                                            
DP01A    IC    R3,PAGYPROF+12                                                   
         L     R5,AP1                                                           
         LH    R0,DDISP                                                         
         AR    R5,R0                                                            
         GOTO1 PUBEDIT,DMCB,((R3),PBUYKPUB),(C'S',0(R5))                        
DP01B    LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         XIT1                                                    L17            
         LTORG                                                                  
         TITLE 'DP07  SPACE DESCRIPTION'                          L17           
*P07     CSECT                                                    L17           
DP07      NMOD1 0,DP07                                            L17           
         USING PPWORKD,RA                                         L17           
         USING PPWORK2D,R7                                        L17           
         USING PPFILED,RC,R9                                      L17           
         USING PP81WRKD,R8                                        L17           
*                                                                 L17           
* RC IS DESTROYED BY NMOD                                         L17           
*                                                                 L17           
         L     RC,PPFILEC        RESTORE RC                       L17           
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPBYOUTD,R5                                                      
         L     R5,ABYOWRK                                                       
         L     R3,AP1                                                           
         AH    R3,DDISP                                                         
         MVC   0(17,R3),PBYOSPC                                                 
         MVC   198(17,R3),PBYOSPC2                                              
*                                                                               
         CLI   PBUYKMED,C'N'                                                    
         BNE   DP07D                                                            
*                                                                               
         CLI   PBYOSPC,C' '                                                     
         BH    *+10                                                             
         MVC   0(7,R3),PBYOUNTS   UNITS                                         
         LA    RF,15(R3)                                                        
*  L05 IF SPACES IN PBYOUNTS THEN POINTER WILL BE BACKED UP TO ANOTHER          
*  COLUMN L05                                                                   
         CLC   0(7,R3),SPACES  ENSURE BCT DOESN'T BACK INTO ANOTHER L05         
         BNE   NSPACES                                                          
         MVC   0(2,R3),PBYOPRM   PREMIOUM CHARGE NO INDENT                      
         B     YSPACES                                                          
*                                                                               
*                                                                               
NSPACES  CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(2,RF),PBYOPRM                                   L05            
*                                                                               
YSPACES  CLI   ANYOPN,C'Y'               IF ANY OPEN RATES IN USERP REC         
         BE    DP07B                     GET RATE FROM 30 EL                    
         CLI   PBDCOSTY,C'U'             IF NO UNIT COST SKIP                   
         BNE   DP07C                                                            
         LA    RF,7(R3)                  IS THERE ENOUGH SPACE TO PUT           
         CLI   6(R3),C' '                OUT UNIT COST                          
         BNH   DP07A                     IF NOT GO TO NEXT LINE,IF USED         
         LA    RF,198(R3)                GOTO THIRD LINE                        
         CLI   198(R3),C' '                                                     
         BNH   DP07A                                                            
         LA    RF,198(RF)                                                       
         MVI   LINENEED,3                                                       
DP07A    EDIT  (P5,PBDCOS),(9,(RF)),5,ALIGN=LEFT,DROP=3                         
         AR    RF,R0                   THIS EDIT WILL EDIT OUT TO               
         CLI   0(RF),C' '              A MAX OF NN.NNNNN                        
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'/'                                                       
         MVC   2(1,RF),PBDUIND                                                  
         OI    2(RF),X'40'                                                      
         B     DP07C                                                            
*                                                                               
DP07B    LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'30'                                                     
         BAS   RE,XNEXTEL                                                       
         BE    *+8                         IF NO 30 EL SKIP                     
         B     DP07C                                                            
         USING PORELMD,R2                                                       
         CLI   PORCOSTY,C'U'               IF NO UNIT COST SKIP                 
         BNE   DP07C                                                            
         LA    RF,7(R3)                                                         
         CLI   6(R3),C' '                                                       
         BNH   DP07B5                                                           
         LA    RF,198(R3)                                                       
         CLI   198(R3),C' '                                                     
         BNH   DP07B5                                                           
         LA    RF,198(RF)                                                       
DP07B5   EDIT  (P5,PORCOS),(9,(RF)),5,ALIGN=LEFT,DROP=3                         
         AR    RF,R0                       EDITS OUT TO A MAX OF                
         CLI   0(RF),C' '                  NN.NNNNN                             
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'/'                                                       
         MVC   2(1,RF),PBDUIND                                                  
         OI    2(RF),X'40'                                                      
         DROP  R2                                                               
*                                                                               
DP07C    CLI   PBYOLBC,C' '        LINES X COLS                                 
         BNH   DP07F                                                            
         MVC   198(17,R3),PBYOLBC                                               
         B     DP07E                                                            
*                                                                               
DP07D    CLI   PBYOSPC2,C' '                                                    
         BNH   DP07F                                                            
DP07E    CLI   LINENEED,2                                                       
         BH    DP07F                                                            
         MVI   LINENEED,2                                                       
DP07F    LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGTH                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         XIT1                                                    L17            
*                                                                               
*                                                                               
XNEXTEL  ZIC   R0,1(R2)     LOAD ELEMENT LENGTH                   L16           
         AR    R2,R0        GET TO NEXT ELEMENT                   L16           
         CLI   0(R2),0      END OF RECORD                         L16           
         BE    NXXTLX                                             L16           
         CLC   ELCODE,0(R2)                                       L16           
         BER   RE                                                 L16           
         B     XNEXTEL      CONTINUE                              L16           
NXXTLX   LTR   RE,RE        FORCE CONDITION CODE TO BNE           L16           
         BR    RE                                                 L16           
*                                                                 L16           
         LTORG                                                                  
         TITLE 'I/O ROUTINES FOR TLOUT'                                         
*OREAD    CSECT                                                                 
IOREAD    NMOD1 0,IOROUT                                          L16           
         USING PPWORKD,RA                                         L16           
         USING PPWORK2D,R7                                        L16           
         USING PPFILED,RC,R9                                      L16           
         USING PP81WRKD,R8                                        L16           
*                                                                 L16           
* RC IS DESTROYED BY NMOD                                         L16           
*                                                                 L16           
         L     RC,PPFILEC        RESTORE RC                       L16           
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
*                                                                 L16           
         L     R1,0(R1)                                           L16           
         ZIC   RF,0(R1)                                           L16           
         SLL   RF,2                                               L16           
         B     INDEXX(RF)                                         L16           
INDEXX   B     CLTREAD                 (0)                        L16           
         B     PRDREAD                  1                        L16            
         B     ESTREAD                  2                        L16            
         B     GETPPP                   3                         L16           
         B     DIVREAD                  4                         L16           
         B     REGREAD                  5                         L16           
         B     DSTREAD                  6                         L16           
         B     LTLREAD                  7                         L16           
*                                                                 L16           
CLTREAD  DS    0H                                                               
         CLC   PCLTKCLT,QCLIENT                                                 
         BE    THISEXIT                                                         
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),PBUYKEY                                                   
         MVI   KEY+3,2                                                          
         CLC   KEY(7),PCLTKEY                                                   
         BE    GETCLT5                                                          
* BUY-IO READ CLIENT DIR                                                        
         GOTO1 READ                                                             
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
* BUY-IO READ CLIENT REC                                                        
         GOTO1 GETPRT                                                           
GETCLT5  MVC   KEY(64),WORK                                                     
* BUY-IO READ RESTORE D/M POINTERS                                              
         GOTO1 HIGH                                                             
         B     THISEXIT            XIT1                                         
         SPACE 3                                                                
PRDREAD  MVC   WORK(64),KEY                                                     
         CLC   PPRDKPRD,QPRODUCT                                                
         BE    SEEIFOAN                                        L19              
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVI   KEY+3,6                                                          
         GOTO1 READ                                                             
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                               L19             
* CHECK TO SEE IF OAN REQUESTED                                 L19             
*                                                               L19             
SEEIFOAN LA    R1,RPTTAB                                        L19             
ISITOAN  CLI   0(R1),255                                        L19             
         BE    REREAD                                           L19             
         CLI   0(R1),36      OTHER AGENCY NAME REQST            L19             
         BL    BMBTAB                                           L19             
         CLI   0(R1),39                                         L19             
         BL    OANREQ                                           L19             
BMBTAB   LA    R1,5(R1)                                         L19             
         B     ISITOAN                                          L19             
*                                                               L19             
*                                                               L19             
OANREQ   DS    0H                                              L19              
         CLI   PPRDOAN,C' '  ANY CODE PRESENT                   L19             
         BH    OANPRES                                          L19             
*                                                               L19             
JUSTMOAN MVC   OANNAME,PAGYNAME GENCY NAME                      L19             
         MVC   PPRDOAN,PAGYKAGY                      L19                        
         B     REREAD                                           L19             
*                                                               L19             
OANPRES  CLC   PPRDOAN,PAGYKAGY    ENSURE NO READ IF PPRDOAN    L19             
         BE    JUSTMOAN            HAS AGENCY CODE IN IT AS A   L19             
         XC    KEY,KEY             RESULT OF CODE BLANK AND AGY L19             
*                                  CODE MOVED IN AS A DEFAULT.  L19             
         MVC   KEY(3),PBUYKAGY                                  L19             
         MVI   KEY+3,X'16'                                      L19             
         MVC   KEY+4(2),PPRDOAN                                 L19             
* BUY-IO DIRECTORY FOR JOB RECORD                               L19             
         GOTO1 HIGH                                             L19             
         CLC   KEY(17),KEYSAVE                                  L19             
         BE    *+6                                              L19             
         DC    H'0'                OAN NOT ON FILE              L10             
         LA    R0,PJOBREC                                                       
         ST    R0,AREC                                                          
* BUY-IO READ FOR OAN RECORD                                                    
         GOTO1 GETPRT                                                           
         L     R1,AREC                                          L19             
         MVC   OANNAME,35(R1)                                   L19             
*                                                               L19             
*                                                               L19             
REREAD   MVC   KEY(64),WORK                                     L19             
         GOTO1 HIGH                                             L19             
         B     THISEXIT            XIT1                         L19             
*                                                                               
         SPACE 2                                                                
ESTREAD  DS    0H                                                               
         CLC   QEST,SPACES                                                      
         BE    CK4COST                                           L10            
         CLC   QEST,=C'ALL'                                                     
         BE    CK4COST                                           L10            
         CLC   PESTKEST,BQEST                                                   
         BE    THISEXIT                                                         
*                                                                               
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),PCLTKEY                                                   
         MVC   KEY+2(1),PBUYKMED                                                
         MVC   KEY+7(3),PPRDKPRD                                                
         MVI   KEY+3,7                                                          
         MVC   KEY+10(2),BQEST                                                  
GETESTM5 GOTO1 READ                                                             
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
GETESTM8 MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
         B     THISEXIT            XIT1                                         
*                                                                               
CK4COST  CLI   COSTOPT,255     WAS COST IN USERP RECORD          L10            
         BNE   THISEXIT                                          L10            
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),PCLTKEY                                                   
         MVC   KEY+2(1),PBUYKMED                                                
         MVC   KEY+7(3),PPRDKPRD                                                
         MVI   KEY+3,7                                                          
         MVC   KEY+10(2),PBUYKEST  READ THIS ESTIMATE NEED IT    L10            
         B     GETESTM5            FOR BILLING OPTIONS           L10            
*                                                                L10            
*                                                                L10            
*                                                                L10            
*                                                                L10            
*                                                                L10            
GETPPP   DS    0H                                                               
         LA    R3,1(RC)                                                         
         LA    R3,4095(R3)                                                      
         USING PPFILED+4096,R3                                                  
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,PUBREC,(0,DMWORK)             
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     THISEXIT            XIT1                                         
         DROP  R3                                                               
         SPACE 2                                                                
*                                                                               
DIVREAD  DS    0H                 GET DIVISION RECORD                           
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
***      MVC   KEY+2(1),QMEDIA                                                  
         MVC   KEY+2(1),PBUYKMED   FOR QMEDIA=C                                 
         MVI   KEY+3,X'03'         DIV REC CODE                                 
         MVC   KEY+4(3),QCLIENT                                                 
         MVC   KEY+7(3),OLDDIV                                                  
         SPACE                                                                  
         GOTO1 HIGH                NO/GET IT                                    
         CLC   KEY(25),KEYSAVE                                                  
         BE    OTDIV55                                                          
         XC    PDIVKEY,PDIVKEY     NOT ASSIGNED                                 
         XC    PDIVNAME,PDIVNAME   CLEAR PREV REC                               
         B     OTDIV77                                                          
OTDIV55  DS    0H                                                               
         LA    R0,PDIVREC                                                       
         ST    R0,AREC                                                          
* BUY-IO DIVISION RECORD                                                        
         GOTO1 GETPRT                                                           
OTDIV77  MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
         SPACE 2                                                                
*                                                                               
THISEXIT XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
REGREAD  DS    0H                 GET REGION RECORD                             
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
***      MVC   KEY+2(1),QMEDIA                                                  
         MVC   KEY+2(1),PBUYKMED   FOR QMEDI=C                                  
         MVI   KEY+3,X'04'         REG REC CODE                                 
         MVC   KEY+4(3),QCLIENT                                                 
         CLI   DRDSW,255  IF DUMMY REGION RQST                   L09            
         BNE   *+10                                              L09            
         MVC   KEY+4(3),QPUB+4 USE PSUEDO CLIENT                 L09            
*                                                                L09            
         MVC   KEY+7(3),OLDDIV                                                  
         MVC   KEY+10(3),OLDREG                                                 
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    OTREG55                                                          
         XC    PREGKEY,PREGKEY     NOT ASSIGNED                                 
         XC    PREGNAME,PREGNAME   CLEAR PREV REG REC                           
         B     OTREG7                                                           
OTREG55  GOTO1 GETREG                                                           
OTREG7   MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
OTREGX   B     THISEXIT                                                         
         SPACE 2                                                                
*                                                                               
DSTREAD  DS    0H                 GET DISTRICT RECORD                           
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
***      MVC   KEY+2(1),QMEDIA                                                  
         MVC   KEY+2(1),PBUYKMED   FOR QMEDIA=C                                 
         MVI   KEY+3,X'05'         DST REC CODE                                 
         MVC   KEY+4(3),QCLIENT                                                 
         CLI   DRDSW,255  IF DUMMY REGION RQST                   L09            
         BNE   *+10                                              L09            
         MVC   KEY+4(3),QPUB+4 USE PSUEDO CLIENT                 L09            
*                                                                L09            
         MVC   KEY+7(3),OLDDIV                                                  
         MVC   KEY+10(3),OLDREG                                                 
         MVC   KEY+13(3),OLDDST                                                 
*        CLC   KEY(16),PDSTREC     IS REC ALREADY THERE                         
*        BNE   OTDST3                                                           
*        MVC   KEY(64),WORK        YES/RESET KEY/EXIT                           
*        B     OTDSTX                                                           
         SPACE                                                                  
OTDST3   GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    OTDST5                                                           
         XC    PDSTKEY,PDSTKEY     NOT ASSIGNED                                 
         XC    PDSTNAME,PDSTNAME   CLEAR PREV REC                               
         B     OTDST7                                                           
OTDST5   GOTO1 GETDIST                                                          
OTDST7   MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
OTDSTX   B     THISEXIT                                                         
         EJECT                                                                  
         SPACE 2                                                                
LTLREAD  DS    0H                                                               
         DS    0H              TEST IF LTLREC ALREADY THERE                     
         SPACE                                                                  
         MVC   WORK(64),KEY     REQUIRED LTLREC NOT THERE/GET IT                
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING LTLKEY,R2                                                        
         MVC   LTLKMED,PBUYKMED    FOR QMEDIA=C                                 
         MVC   LTLKPUB(6),PBUYKPUB                                              
         MVC   LTLKAGY,QAGENCY                                                  
         MVI   LTLKCOD,X'85'                                                    
         DROP R2                                                                
         GOTO1 HIGHPUB                                                          
         CLC   KEY(11),KEYSAVE                                                  
         BE    GTL5                                                             
         L     R2,ALTLREC                                                       
         XC    0(40,R2),0(R2)            CLEAR X'71' AREA                       
         B     GTL10                                                            
GTL5     L     R2,ALTLREC                                                       
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,(R2),(0,DMWORK)               
         CLI   8(R1),0                                                          
         BE    GTL10                                                            
         DC    H'0'                                                             
GTL10    MVC   KEY,WORK            RESET KEY                                    
         GOTO1 HIGH                RESTORE SEQUENTIAL READ                      
GTLX     XIT1                                                                   
         EJECT                                                                  
*                                                                               
DISPTAB  DC    AL1(01),AL4(DP0A)     CLIENT                                     
         DC    AL1(02),AL4(DP0B)     REGION                                     
         DC    AL1(03),AL4(DP0C)     DISTRICT                                   
         DC    AL1(04),AL4(DP0D)     REGION NAME                                
         DC    AL1(05),AL4(DP0E)     DISTRICT NAME                              
         DC    AL1(10),AL4(DP01)     PUB NUMBER                                 
         DC    AL1(11),AL4(DP02)     PUB NAME                                   
         DC    AL1(12),AL4(DP03)     PRODUCT CODE                               
         DC    AL1(13),AL4(DP08)     PRODUCT NAME                               
         DC    AL1(14),AL4(DP11)     ESTIMATE                                   
         DC    AL1(15),AL4(DP07)     SPACE DESCRIPTION                          
         DC    AL1(16),AL4(DP10)     DIVISION                                   
         DC    AL1(17),AL4(DP14)     AD CODE                                    
         DC    AL1(18),AL4(DP09)     COPY NUMBER                                
         DC    AL1(19),AL4(DP19)     COMMENTS                                   
         DC    AL1(20),AL4(DP92)     ESTIMATE DESCRIPTION                       
         DC    AL1(21),AL4(DP93)     ASPO NUMBER                                
         DC    AL1(22),AL4(DP94)     LIST CODE                                  
         DC    AL1(23),AL4(DP95)     LAST I/O                                   
         DC    AL1(24),AL4(DP97)     CAPTION                                    
         DC    AL1(25),AL4(DP98)     LAST AD USE                                
         DC    AL1(26),AL4(DP99)     BLANK COLUMN                               
         DC    AL1(27),AL4(DP14)     AD CODE                                    
         DC    AL1(28),AL4(DP56)     DISTRICT NAME                  L20         
         DC    AL1(30),AL4(DP05)     INSERTION DATE                             
         DC    AL1(31),AL4(DP04)     CLOSING DATE                               
         DC    AL1(32),AL4(DP06)     ON-SALE DATE                               
         DC    AL1(33),AL4(DP12)     PAYABLE DATE                               
         DC    AL1(34),AL4(DP13)     BILLABLE DATE                              
         DC    AL1(35),AL4(DP15)     MAT CLOSING DATE                           
         DC    AL1(36),AL4(DP16)     OAN CODE                     L19           
         DC    AL1(37),AL4(DP17)     OAN NAME                     L19           
         DC    AL1(38),AL4(DP18)     OAN NAME & CODE              L19           
         DC    AL1(40),AL4(DP20)     ORDERED GROSS                              
         DC    AL1(41),AL4(DP21)     ORDERED NET                                
         DC    AL1(42),AL4(DP22)     ORDERED GR-CD                              
         DC    AL1(43),AL4(DP23)     ORDERED NET-CD                             
         DC    AL1(44),AL4(DP24)     ORDERED AGY COM                            
         DC    AL1(45),AL4(DP25)     ORDERED CD                                 
         DC    AL1(46),AL4(DP52)                                   L07          
         DC    AL1(50),AL4(DP26)     PAID GROSS                                 
         DC    AL1(51),AL4(DP27)     PAID NET                                   
         DC    AL1(52),AL4(DP28)     PAID GR-CD                                 
         DC    AL1(53),AL4(DP29)     PAID NET-CD                                
         DC    AL1(54),AL4(DP30)     PAID AGY COM                               
         DC    AL1(55),AL4(DP31)     PAID CD                                    
         DC    AL1(60),AL4(DP32)     BILLED GROSS                               
         DC    AL1(61),AL4(DP33)     BILLED NET                                 
         DC    AL1(62),AL4(DP34)     BILLED GR-CD                               
         DC    AL1(63),AL4(DP35)     BILLED NET-CD                              
         DC    AL1(64),AL4(DP36)     BILLED AGY COM                             
         DC    AL1(65),AL4(DP37)     BILLED CD                                  
         DC    AL1(70),AL4(DP38)     UNPAID GROSS                               
         DC    AL1(71),AL4(DP39)     UNPAID NET                                 
         DC    AL1(72),AL4(DP40)     UNPAID GR-CD                               
         DC    AL1(73),AL4(DP41)     UNPAID NET-CD                              
         DC    AL1(74),AL4(DP42)     UNPAID AGY COM                             
         DC    AL1(75),AL4(DP43)     UNPAID CD                                  
         DC    AL1(80),AL4(DP44)     BILLABLE GROSS                             
         DC    AL1(81),AL4(DP45)     BILLABLE NET                               
         DC    AL1(82),AL4(DP46)     BILLABLE GR-CD                             
         DC    AL1(83),AL4(DP47)     BILLABLE NET-CD                            
         DC    AL1(84),AL4(DP48)     BILLABLE AGY COM                           
         DC    AL1(85),AL4(DP49)     BILLABLE CD                                
         DC    AL1(86),AL4(DP50)     NUM OF INSERTIONS                          
         DC    AL1(90),AL4(DP90)     PLANNED COST                               
         DC    AL1(91),AL4(DP91)     ACTUAL LESS PLANNED COST                   
         DC    AL1(92),AL4(DP96)     TAX                                        
         DC    AL1(93),AL4(DP51)     DIFFERENCE COL                             
         DC    X'FFFF'             END OF TABLE                                 
         EJECT                                                                  
*                                                                               
HEADTAB  DC    AL1(01),AL4(HD00)     CLIENT                                     
         DC    AL1(02),AL4(HD0A)     REGION                                     
         DC    AL1(03),AL4(HD0B)     DISTRICT                                   
         DC    AL1(04),AL4(HD0C)     REGION NAME                                
         DC    AL1(05),AL4(HD0D)     DISTRICT NAME                              
         DC    AL1(10),AL4(HD01)     PUB NUMBER                                 
         DC    AL1(11),AL4(HD02)     PUB NAME                                   
         DC    AL1(12),AL4(HD03)     PRODUCT CODE                               
         DC    AL1(13),AL4(HD08)     PRODUCT NAME                               
         DC    AL1(14),AL4(HD11)     ESTIMATE                                   
         DC    AL1(15),AL4(HD07)     SPACE DESCRIPTION                          
         DC    AL1(16),AL4(HD10)     DIVISION                                   
         DC    AL1(17),AL4(HD14)     AD CODE                                    
         DC    AL1(18),AL4(HD09)     COPY NUMBER                                
         DC    AL1(19),AL4(HD19)     COMMENTS                                   
         DC    AL1(20),AL4(HD92)     ESTIMATE DESCRIPTION                       
         DC    AL1(21),AL4(HD93)     ASPO NUMBER                                
         DC    AL1(22),AL4(HD94)     LIST CODE                                  
         DC    AL1(23),AL4(HD95)     LAST I/O                                   
         DC    AL1(24),AL4(HD97)     CAPTION                                    
         DC    AL1(25),AL4(HD98)     LAST ADCODE USE                            
         DC    AL1(26),AL4(HD99)     BLANK COLUMN                               
         DC    AL1(27),AL4(HD14)     USE SAME ADCODE HEAD ROUTINE               
         DC    AL1(28),AL4(HD51)     DIVISION NAME                L20           
         DC    AL1(30),AL4(HD05)     INSERTION DATE                             
         DC    AL1(31),AL4(HD04)     CLOSING DATE                               
         DC    AL1(32),AL4(HD06)     ON-SALE DATE                               
         DC    AL1(33),AL4(HD12)     PAYABLE DATE                               
         DC    AL1(34),AL4(HD13)     BILLABLE DATE                              
         DC    AL1(35),AL4(HD15)     MAT CLOSING DATE                           
         DC    AL1(36),AL4(HD16)     OTHER AGENCY CODE             L19          
         DC    AL1(37),AL4(HD17)     OTHER AGENCY NAME             L19          
         DC    AL1(38),AL4(HD17)     OTHER AGENCY NAME/CODE        L19          
         DC    AL1(40),AL4(HD20)     ORDERED GROSS                              
         DC    AL1(41),AL4(HD21)     ORDERED NET                                
         DC    AL1(42),AL4(HD22)     ORDERED GR-CD                              
         DC    AL1(43),AL4(HD23)     ORDERED NET-CD                             
         DC    AL1(44),AL4(HD24)     ORDERED AGY COM                            
         DC    AL1(45),AL4(HD25)     ORDERED CD                                 
         DC   AL1(46),AL4(HD54)                                 L07             
         DC    AL1(50),AL4(HD26)     PAID GROSS                                 
         DC    AL1(51),AL4(HD27)     PAID NET                                   
         DC    AL1(52),AL4(HD28)     PAID GR-CD                                 
         DC    AL1(53),AL4(HD29)     PAID NET-CD                                
         DC    AL1(54),AL4(HD30)     PAID AGY COM                               
         DC    AL1(55),AL4(HD31)     PAID CD                                    
         DC    AL1(60),AL4(HD32)     BILLED GROSS                               
         DC    AL1(61),AL4(HD33)     BILLED NET                                 
         DC    AL1(62),AL4(HD34)     BILLED GR-CD                               
         DC    AL1(63),AL4(HD35)     BILLED NET-CD                              
         DC    AL1(64),AL4(HD36)     BILLED AGY COM                             
         DC    AL1(65),AL4(HD37)     BILLED CD                                  
         DC    AL1(70),AL4(HD38)     UNPAID GROSS                               
         DC    AL1(71),AL4(HD39)     UNPAID NET                                 
         DC    AL1(72),AL4(HD40)     UNPAID GR-CD                               
         DC    AL1(73),AL4(HD41)     UNPAID NET-CD                              
         DC    AL1(74),AL4(HD42)     UNPAID AGY COM                             
         DC    AL1(75),AL4(HD43)     UNPAID CD                                  
         DC    AL1(80),AL4(HD44)     BILLABLE GROSS                             
         DC    AL1(81),AL4(HD45)     BILLABLE NET                               
         DC    AL1(82),AL4(HD46)     BILLABLE GR-CD                             
         DC    AL1(83),AL4(HD47)     BILLABLE NET-CD                            
         DC    AL1(84),AL4(HD48)     BILLABLE AGY COM                           
         DC    AL1(85),AL4(HD49)     BILLABLE CD                                
         DC    AL1(86),AL4(HD50)     NUM OF INSERTIONS                          
         DC    AL1(90),AL4(HD90)     PLANNED COST                               
         DC    AL1(91),AL4(HD91)     ACTUAL LESS PLANNED COST                   
         DC    AL1(92),AL4(HD96)     TAX                                        
         DC    AL1(93),AL4(HD53)     DIFFERENCE COLUMNS            L04          
*                                                                               
*              THE FOLLOWING ARE OPEN AMOUNTS                                   
*                                                                               
         DC    AL1(140),AL4(HD20)     ORDERED GROSS                             
         DC    AL1(141),AL4(HD21)     ORDERED NET                               
         DC    AL1(142),AL4(HD22)     ORDERED GR-CD                             
         DC    AL1(143),AL4(HD23)     ORDERED NET-CD                            
         DC    AL1(144),AL4(HD24)     ORDERED AGY COM                           
         DC    AL1(145),AL4(HD25)     ORDERED CD                                
         DC    AL1(150),AL4(HD26)     PAID GROSS                                
         DC    AL1(151),AL4(HD27)     PAID NET                                  
         DC    AL1(152),AL4(HD28)     PAID GR-CD                                
         DC    AL1(153),AL4(HD29)     PAID NET-CD                               
         DC    AL1(154),AL4(HD30)     PAID AGY COM                              
         DC    AL1(155),AL4(HD31)     PAID CD                                   
         DC    AL1(160),AL4(HD32)     BILLED GROSS                              
         DC    AL1(161),AL4(HD33)     BILLED NET                                
         DC    AL1(162),AL4(HD34)     BILLED GR-CD                              
         DC    AL1(163),AL4(HD35)     BILLED NET-CD                             
         DC    AL1(164),AL4(HD36)     BILLED AGY COM                            
         DC    AL1(165),AL4(HD37)     BILLED CD                                 
         DC    AL1(170),AL4(HD38)     UNPAID GROSS                              
         DC    AL1(171),AL4(HD39)     UNPAID NET                                
         DC    AL1(172),AL4(HD40)     UNPAID GR-CD                              
         DC    AL1(173),AL4(HD41)     UNPAID NET-CD                             
         DC    AL1(174),AL4(HD42)     UNPAID AGY COM                            
         DC    AL1(175),AL4(HD43)     UNPAID CD                                 
         DC    AL1(180),AL4(HD44)     BILLABLE GROSS                            
         DC    AL1(181),AL4(HD45)     BILLABLE NET                              
         DC    AL1(182),AL4(HD46)     BILLABLE GR-CD                            
         DC    AL1(183),AL4(HD47)     BILLABLE NET-CD                           
         DC    AL1(184),AL4(HD48)     BILLABLE AGY COM                          
         DC    AL1(185),AL4(HD49)     BILLABLE CD                               
*           END OF OPEN AMOUNTS                                                 
*                                                                               
*                                                                               
*           THE FOLLOWING ARE DIFF AMOUNTS                                      
         DC    AL1(200),AL4(HD20)     ORDERED GROSS                             
         DC    AL1(201),AL4(HD21)     ORDERED NET                               
         DC    AL1(202),AL4(HD22)     ORDERED GR-CD                             
         DC    AL1(203),AL4(HD23)     ORDERED NET-CD                            
         DC    AL1(204),AL4(HD24)     ORDERED AGY COM                           
         DC    AL1(205),AL4(HD25)     ORDERED CD                                
         DC    AL1(210),AL4(HD26)     PAID GROSS                                
         DC    AL1(211),AL4(HD27)     PAID NET                                  
         DC    AL1(212),AL4(HD28)     PAID GR-CD                                
         DC    AL1(213),AL4(HD29)     PAID NET-CD                               
         DC    AL1(214),AL4(HD30)     PAID AGY COM                              
         DC    AL1(215),AL4(HD31)     PAID CD                                   
         DC    AL1(220),AL4(HD32)     BILLED GROSS                              
         DC    AL1(221),AL4(HD33)     BILLED NET                                
         DC    AL1(222),AL4(HD34)     BILLED GR-CD                              
         DC    AL1(223),AL4(HD35)     BILLED NET-CD                             
         DC    AL1(224),AL4(HD36)     BILLED AGY COM                            
         DC    AL1(225),AL4(HD37)     BILLED CD                                 
         DC    AL1(230),AL4(HD38)     UNPAID GROSS                              
         DC    AL1(231),AL4(HD39)     UNPAID NET                                
         DC    AL1(232),AL4(HD40)     UNPAID GR-CD                              
         DC    AL1(233),AL4(HD41)     UNPAID NET-CD                             
         DC    AL1(234),AL4(HD42)     UNPAID AGY COM                            
         DC    AL1(235),AL4(HD43)     UNPAID CD                                 
         DC    AL1(240),AL4(HD44)     BILLABLE GROSS                            
         DC    AL1(241),AL4(HD45)     BILLABLE NET                              
         DC    AL1(242),AL4(HD46)     BILLABLE GR-CD                            
         DC    AL1(243),AL4(HD47)     BILLABLE NET-CD                           
         DC    AL1(244),AL4(HD48)     BILLABLE AGY COM                          
         DC    AL1(245),AL4(HD49)     BILLABLE CD                               
*           END OF DIFF AMOUNTS                                                 
*                                                                               
         DC    X'FFFF'             END OF TABLE                                 
         EJECT                                                                  
*                                                                               
PP81WRKD DSECT                                                                  
AP1      DS    F                                                                
ASVHD7   DS    F                                                                
ASVHD8   DS    F                                                                
ASVHD9   DS    F                                                                
DISPLINE DS    F                                                                
AWIDEC   DS    A                                                                
ATLIN    DS    A                                                                
ATLOUT   DS    A                                                                
ABYOWRK  DS    A                                                                
ADISPTAB DS    F                                                L07             
AWEEK    DS    F                                                  L10           
RUNSW    DS    X                                                                
SDISP    DS    H                                                                
DDISP    DS    H                                                                
DIVSD    DS    H                   DIV SORT DISPLACEMENT                        
CHGSW    DS    CL1                                                              
DBL      DS    D                                                  L10           
WEKPTR   DS    F                                                  L10           
FULL7    DS    F                                                  L10           
WEEKPTR  DS    H                                                  L10           
WEEK     DS    CL4                                                L10           
THISBUY  DS    CL2                                                L10           
WHATWEEK DS    CL1                                                L10           
MULTIPLY DS    PL5                                                L11           
ARITHOPT DS    CL1                                                L11           
NOPUBLKU DS    CL30   10 CLIENTS                                  L11           
*                                                                 L11           
*                                                                 L11           
*                                                                 L11           
*                                                                 L11           
*                                                                 L11           
*                                                                 L11           
STDATE   DS    CL6                                                L01           
EDATE    DS    CL6                                                L10           
COSTOPT  DS    CL1                                                L10           
*                                                                 L10           
DESC93   DS     CL22                                              L12           
*                                                                 L12           
*                                                                 L12           
*                                                                 L12           
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
SKPUB    DS    CL20                                                             
*                                                                               
OLDKEY   DS    CL250                                                            
         DS    CL4                                                              
OLDCLT   DS    CL3                                                              
OLDPRD   DS    CL3                                                              
OLDPUB   DS    CL6                                                              
         DS    CL3                                                              
OLDEST   DS    CL2                                                              
         DS    CL4                                                              
OLDJOB   DS    CL6                                                              
OLDDIV   DS    CL3                                                              
OLDREG   DS    CL3                                                              
OLDDST   DS    CL3                                                              
*                                                                               
         DS    0F                                                               
SAVPARS  DS    0CL24                                                            
         DS    6F                                                               
SAVR2    DS    F                                                                
ARDSAVE1 DS    F                                                                
ARDSAVE  DS    F                                                                
ANAMETBL DS    F                                                                
ARDSPLIT DS    F                                                                
AOPNDIFF DS    F                                                                
AHEADRTN DS    F                                                                
ADSPRD   DS    H                                                                
ADSDST   DS    H                                                                
DUPSW    DS    C                                                                
LINENEED DS    X                                                                
SAVLND   DS    X                   SAVED LINENEED FOR SAVP AND SAVP2            
SRTLIN   DS    H                                                                
***                                                                             
SAVERE   DS    F                                                                
TTINSRT  DS    F                                                                
SAVEAPTR DS    F                                                    L04         
SAVEAMT  DS    16F                                                  L04         
DIFFACCU DS    XL1                                                  L04         
SVQCLI   DS    CL3                                                              
SAVEKEY2 DS    CL4   DO NOT INSERT ANY LABEL BETWEEN SAVEKEY2 &SAVEKEY          
SAVEKEY  DS    CL32  THEY ARE CLEARED TOGETHER                                  
SVINSDSP DS    H                                                                
SVINSFLG DS    CL1                                                              
LNKPUBS  DS    CL95                                                             
SORTASPO DS    CL9                                                              
MEDNMSV  DS    CL10                                                             
FMULTMED DS    CL1      FIRST MEDIA FOR MULTIMED REQ                            
SV70ELM  DS    CL11                                                             
ONLYOPN  DS    CL1                                                              
ANYOPN   DS    CL1                                                              
SUPRNSW  DS    CL1                                                              
***                                                                             
SAVKEYS  DS    CL64                                                             
SAVDAT2  DS    CL8                                                              
REGNMSV  DS    CL20                                                             
DISTNMSV DS    CL20                                                             
OFCLTSW  DS    CL1                                                              
SVSRTLN  DS    CL2                                                              
SVKEYLN  DS    CL2                                                              
DOWNLOAD DS    CL1                                                              
ENDRPRT  DS    CL1                                                              
FRSTPAGE DS    CL1                                                              
DIFFMED  DS    CL1                                           BUG30              
         DS    CL3             SPARE SPARE SPARE SPARE                          
*                                                                               
*                                                                               
PBNAM1   DS    CL20                                                             
PBNAM2   DS    CL20                                                             
PBNAM3   DS    CL20                                                             
PBNAMX   DS    CL1                                                              
         DS    0F                                                               
TEMPTOTS DS    CL120         CL12= $FIELD/00/DISP/$ AMT       L01               
FINLTOTS DS    CL120         CL12= $FIELD/00/DISP/$ AMT                         
TOTTOT   DS    CL120         CL12= $FIELD/00/DISP/$ AMT                         
*                                                                               
SRTRCSV  DS    CL250                                                            
*                                     1   1   2     8 =12                       
*                                     1   1   2     8 =12                       
SAVHD7   DS    CL198                                                            
SAVHD8   DS    CL198                                                            
SAVHD9   DS    CL198                                                            
SAVP     DS    CL1                                                              
SAVP2    DS    CL1                                                              
SAVP3    DS    CL1                                                              
NSAVP    DS    CL1                                                              
NSAVP2   DS    CL1                                                              
NSAVP3   DS    CL1                                                              
*                                                                               
         SPACE 3                                                                
RPTTAB    DS    CL102       ROOM FOR 20 FIELDS +2X'FF'                          
*        FORMAT IS FLD CODE/SORT LEN/DISPLAY LEN/TOTALS/SORT SW                 
*              IF SORT SW  IS X'FF' DON'T SORT ON THIS FLD                      
*              IF DISP LEN IS X'FF' DON'T DISPLAY THIS FLD                      
*              IF DISP LEN IS ORED WITH X'80' HEADLINE DISPLAY                  
*              IF TOTALS   IS X'FF' DON'T TOTAL ON THIS FLD                     
*                                                                               
BRKSW    DS    CL1                                                              
ACCODE   DS    CL1                                                              
PRTSW    DS    CL1                                                              
FRSTSW   DS    CL1                                                              
FIRSTT   DS    CL1                                                              
LINEBRK  DS    CL1                                                              
LINBRKSW DS    CL1                                                              
INSDTSW  DS    CL1                                                              
CLSDTSW  DS    CL1                                                              
ONSLDTSW DS    CL1                                                              
PAYDTSW  DS    CL1                                                              
BILDTSW  DS    CL1                                                              
MATDTSW  DS    CL1                                                              
DTSW     DS    CL1                                                              
CMNTSW   DS    CL1                                                              
BRKKEY   DS    CL33                SELECTION ID/DISP/SORT LENGTH                
DOLSW    DS    CL1                                                              
ANYPRTSW DS    CL1             FOR * MEDIA, ANYTHING PRINTED YET?               
HEADSAVE DS    CL30                                                             
SORTREC  DS    CL250               MAX SORT REC                                 
CHINPUT  DS    CL192                                                            
EXTRAPRT DS    CL1                                                              
SVDISCAD DS    F                                                                
SVADCD   DS    CL6                                                              
SVLSTDAT DS    CL3                                                              
MLTREC   DS    CL1                                                              
STDCODE1 DC     XL6'0'         STANDARD COMMENT OPTION 1          L08           
STDCODE2 DC     XL6'0'         STANDARD COMMENT OPTION 2          L08           
*           NOTE-- THE ABOVE 2 STORAGE AREAS M/B TOGETHER         L08           
*                                                                 L08           
DRDSW    DS    CL1                                                L09           
MYPRINT  DS    CL198                                                            
         DS    CL20                                                             
SPACEING DC    XL1'03'                                            L22           
*                                                                 L08           
DPUBCTRL DC    XL6'0'                                            L06            
PASSPTRS  DS    CL1                                               L15           
PASSPRD   DS    CL3                                               L15           
OANNAME   DS    CL33                                                            
OLDOAN    DS    CL2                                                             
OLDNAMC   DS    CL36     PREVIOUS OAN CODE + NAME                               
OLDNAM    DS    CL33     PREVIOUS NAME                                          
SLAVELST  DS    CL3      SLAVE CLIENT                            L21            
MASTCLI   DS    CL3      MASTER CLIENT                           L21            
MASTCLNA  DS    CL20     MASTER CLIENT NAME                      L21            
REALCLI   DS    CL23     REAL CLIENT CODE AND NAME               L21            
*          DATA SET PVALUES    AT LEVEL 005 AS OF 06/30/86                      
* ORDERED DATA FOR OPEN RATES                                                   
OGROSS   DS    F                   GROSS ORDERED                                
OAGYCOM  DS    F                   AGENCY COMMISSION                            
OCSHDSC  DS    F                   CASH DISCOUNT                                
OPYABLE  DS    F                   GROSS-AGYCOMM-CASHDSC                        
OBLABLE  DS    F                   GROSS-CASH DSC                               
OPREMIUM DS    F                   (INCLUDED IN ABOVE FIELDS)                   
OUNITS   DS    F                   NUMBER OF LINES BOUGHT                       
***   NOTE ORDERED TAX UNDER PAID DATA                                          
* PAID DATA FOR OPEN RATES                                                      
OPGROSS  DS    F                   GROSS PAID                                   
OPAGYCOM DS    F                   AGY COMM PAID                                
OPCSHDSC DS    F                   CASH DISCOUNT PAID                           
OPAID    DS    F                   ACTUAL PAID AMOUNT                           
OTAX     DS    F                   ORDERED TAX - WAS PAYABLE DATE               
*                          (INCLUDED IN ORDERED GROSS,PYABLE,BLABLE)            
*                                  NET X PBDTAX (4 DECIMALS)                    
* BILLED DATA FOR OPEN RATES                                                    
OBGROSS  DS    F                   GROSS BILLED                                 
OBAGYCOM DS    F                   AGY COMM BILLED                              
OBCSHDSC DS    F                   CASH DISCOUNT BILLED                         
OBILLED  DS    F                   ACTUAL BILLED AMOUNT                         
OBLBLDT  DS    CL3                 BILLABLE DATE -YMD                           
*                                                                               
*                                                                               
*                                                                               
* ORDERED DATA FOR DIFFERENCE OF OPEN AND CONTRACT                              
DGROSS   DS    F                   GROSS ORDERED                                
DAGYCOM  DS    F                   AGENCY COMMISSION                            
DCSHDSC  DS    F                   CASH DISCOUNT                                
DPYABLE  DS    F                   GROSS-AGYCOMM-CASHDSC                        
DBLABLE  DS    F                   GROSS-CASH DSC                               
DPREMIUM DS    F                   (INCLUDED IN ABOVE FIELDS)                   
DUNITS   DS    F                   NUMBER OF LINES BOUGHT                       
***   NOTE ORDERED TAX UNDER PAID DATA                                          
* PAID DATA FOR DIFFERENCE                                                      
DPGROSS  DS    F                   GROSS PAID                                   
DPAGYCOM DS    F                   AGY COMM PAID                                
DPCSHDSC DS    F                   CASH DISCOUNT PAID                           
DPAID    DS    F                   ACTUAL PAID AMOUNT                           
DTAX     DS    F                   ORDERED TAX - WAS PAYABLE DATE               
*                          (INCLUDED IN ORDERED GROSS,PYABLE,BLABLE)            
*                                  NET X PBDTAX (4 DECIMALS)                    
* BILLED DATA FOR DIFFERENCE                                                    
DBGROSS  DS    F                   GROSS BILLED                                 
DBAGYCOM DS    F                   AGY COMM BILLED                              
DBCSHDSC DS    F                   CASH DISCOUNT BILLED                         
DBILLED  DS    F                   ACTUAL BILLED AMOUNT                         
DBLBLDT  DS    CL3                 BILLABLE DATE -YMD                           
ACCUMS   DS    0F            $ FIELD /SELECTION ID /P LINE DISP /$AMT           
         DS    CL360  L01      CL1   /    CL1      /  CL2       / CL8           
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
*                                                                               
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
         PRINT ON                                                               
**                                                                              
       ++INCLUDE DDBIGBOX                                                       
*                                                                               
PPWORKD  DSECT                                                                  
         ORG   QBILMODE                                                         
QJOB     DS    CL6                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
PUSRECD  DSECT                                                                  
       ++INCLUDE PUSEREC                                                        
PORELMD  DSECT                                                                  
       ++INCLUDE PORELEM                                                        
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDWIDED                                                        
*                                                                               
*     /*                                                                        
*     //ASM.SYSPRINT DD SYSOUT=*,CHARS=BX12                                     
*     /*                                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051PPREPL102 04/08/14'                                      
         END                                                                    
