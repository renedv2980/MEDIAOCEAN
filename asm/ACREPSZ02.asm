*          DATA SET ACREPSZ02  AT LEVEL 120 AS OF 08/16/00                      
*PHASE ACSZ02A,+0                                                               
*INCLUDE PRNTBL                                                                 
**********************************************************************          
* PROGRAM OPTIONS :                                                  *          
*                                                                    *          
*         QOPT1    - ' '  RUN BOTH PRINTOUTS                         *          
*                    '1'  RUN SZ LEDGER ACCOUNTS ONLY PRINTOUT       *          
*                    '2'  RUN SZ CONTRA CLIENT CODE ONLY PRINTOUT    *          
*         QOPT2    - ' '  RUN ON ALL INPUT TYPE                      *          
*                  - 'Y'  RUN ON LIMITED INPUT TYPE FROM TABLE       *          
*                  - 'N'  RUN ON LIMITED INPUT TYPE NOT IN TABLE     *          
*         QOPT3    - ' '  PRINT ALL                                  *          
*                  - 'Y'  ONLY PRINT THOSE BALANCES THAT ARE SIMILAR *          
*                  - 'N'  ONLY PRINT THOSE BALANCES THAT DIFFER      *          
*         QOPT4    - 'Y'  SUPPRESS CLIENT TOTALS IF ZERO             *          
*         QOPT5    - 'P'  CREATE POSTINGS TO PUT ACC SIDE IN BALANCE *          
*                         WITH MEDIA SIDE                            *          
*         QSELECT  -      CLIENT CODE TO LIMIT REQUEST               *          
*         QSTART   -      START DATE (YYMMDD)                        *          
*         QEND     -      END   DATE (YYMMDD) NOTE: ** REQUIRED **   *          
**********************************************************************          
         TITLE 'SZ LEDGER BALANCE'                                              
ACSZ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACSZ**,R7,R9    BASE REGISTERS 11,9                          
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         LA    RC,SPACEND                                                       
         USING ACSZD,RC            RC = A(SAVE W/S)                             
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,REQLAST        RUN LAST                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
                                                                                
RUNF     DS    0H                                                               
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         GOTO1 DATCON,DMCB,(5,0),(1,TODAYP)                                     
         CLC   RCDATE,SPACES                                                    
         BNH   RUNF10                                                           
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,RUNDATE)                               
*                                                                               
         USING ACCRECD,R3                                                       
RUNF10   L     R3,AIO1                                                          
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
                                                                                
REQF     DS    0H                                                               
         MVC   SVACT,SPACES                                                     
         MVI   FILOPN,C'N'                                                      
         L     R1,ADCOMP                                                        
         MVC   HEXCOMP,0(R1)                                                    
         USING CPYELD,R1                                                        
         L     R1,ADCMPEL                                                       
         TM    CPYSTAT4,CPYSOFF2                                                
         BZ    *+8                                                              
         MVI   NEWOFFC,C'Y'                                                     
*                                                                               
         USING BIND,R1                                                          
         L     R1,AACTTAB          A(ACCOUNT TABLE)                             
         XC    BININ,BININ         CLEAR AGENCY TABLE - BIN TABLE 2             
         DROP  R1                                                               
*                                                                               
         USING BIND,R1                                                          
         L     R1,AACTTAB          A(ACCOUNT TABLE)                             
         USING ACTD,R2                                                          
         LA    R2,BINTAB                                                        
         ZAP   ACTDBKT,=P'0'                                                    
         ZAP   ACTCBKT,=P'0'                                                    
         DROP  R1,R2                                                            
*                                                                               
         LA    R1,PKFLDS           INITIALIZE PACKED FIELDS R1=(START)          
         LA    R0,PKNUMQ                                                        
         ZAP   0(L'PKFLDS,R1),=P'0'                                             
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         XC    STDTE,STDTE         CLEAR START DATE TO ZERO                     
         CLC   QSTART,SPACES                                                    
         BE    REQF05                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(2,STDTE)                                 
*                                                                               
REQF05   MVC   ENDDTE,=X'FFFF'     INIT  END   DATE TO HIGH VALUES              
         CLC   QEND,SPACES                                                      
         BNE   REQF10                                                           
         DC    H'00'               ABEND                                        
*                                                                               
REQF10   GOTO1 DATCON,DMCB,(0,QEND),(2,ENDDTE)                                  
         CLC   STDTE,ENDDTE        START DATE  > END  DATE ?                    
         BNH   *+6                 NO,   CONTINUE                               
         DC    H'00'               YES,  ABEND                                  
*                                                                               
         USING TRNRECD,R4                                                       
         LA    R4,DKEY             R4 = A(KEY)                                  
         MVC   DKEY,SPACES                                                      
         MVC   TRNKCPY,RCCOMPFL    COMPANY CODE                                 
         MVC   TRNKUNT(2),QUNIT    MOVE IN UNIT/LEDGER                          
         MVC   TRNKACT,QACCOUNT                                                 
*                                                                               
         LA    R1,14               SET KEY COMPARE LENGTH                       
         LA    R0,L'QACCOUNT       R0=LOOP COUNTER                              
         LA    RE,QACCOUNT+L'QACCOUNT-1                                         
         CLI   0(RE),C' '          TEST FOR LAST SIGNIFICANT CHARACTER          
         BH    *+12                                                             
         BCTR  RE,0                BACK UP CHARACTER POINTER                    
         BCTR  R1,0                DECREMENT KEY LENGTH                         
         BCT   R0,*-12                                                          
         STC   R1,COMPLEN                                                       
*                                                                               
         BAS   RE,DMHIADIR         READ HI ON ACCT DIR                          
         B     REQF30                                                           
*                                                                               
REQF20   DS    0H                                                               
         BAS   RE,DMSEADIR         READ SEQ FOR ACCT DIR                        
REQF30   LA    R4,DIR              R4 = A(RETURNED KEY)                         
         SR    R1,R1                                                            
         IC    R1,COMPLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DIR(0),DKEY         COMPARE TYPE WITH ORIGINAL KEY               
         BNE   REQFX                                                            
*                                                                               
         USING ACTD,R3                                                          
         LA    R3,ACTWRK                                                        
*                                                                               
         CLC   TRNKACT,SPACES      ANY ACCOUNT?                                 
         BE    REQF20              NO - NOT AN ACCOUNT RECORD                   
*MN      CLC   TRNKCULC,SPACES     ANY CONTRA ACCOUNT?                          
*MN      BNE   REQF70              NO - NOT AN ACCOUNT RECORD                   
         CLC   TRNKOFF(L'TRNKOFF+L'TRNKCULC),SPACES                             
         BNE   REQF70                                                           
*                                                                               
         BAS   RE,DMGETACC                                                      
         XC    ACTWRK,ACTWRK       CLEAR BIN WORK AREA                          
         MVC   ACTACCT,TRNKACT     FULL ACCOUNT FOR KEY                         
         MVC   ACTNME,SPACES                                                    
*                                                                               
         USING NAMELD,R2                                                        
         L     R2,AIO1             R2 = A(IO1)                                  
         AH    R2,DISP2                                                         
REQF40   CLI   0(R2),0                                                          
         BE    REQF20                                                           
         CLI   0(R2),NAMELQ        X'20' - NAME ELEMENT                         
         BE    REQF50                                                           
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF40                                                           
*                                                                               
REQF50   SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   ACTNME(0),NAMEREC                                                
         DROP  R2                                                               
*                                                                               
* COMBINE MULTIPLE SIMILAR ACCOUNTS INTO ONE ACC ACCOUNT                        
*                                                                               
         USING MEDD,R1                                                          
         LA    R1,MEDTAB           R1=A(SYSTEM/MEDIA TABLE)                     
REQF60   CLI   0(R1),EOF                                                        
         BE    REQF20              NOT IN TABLE - CONTINUE                      
         CLC   MEDSYS,ACTACCT      ACTACCT HAS ACTUAL SZ ACCOUNT                
         BE    *+12                                                             
         LA    R1,MEDLNQ(R1)                                                    
         B     REQF60                                                           
*                                                                               
         MVC   ACTRACT,ACTACCT               SAVE REAL ACCOUNT                  
         MVC   ACTACCT(L'MEDACT),MEDACT      REPLACE WITH NEW CODE              
         B     REQF20                                                           
         DROP  R1                                                               
*                                                                               
REQF70   CLC   TRNKDATE,SPACES     ANY TRANSACTION DATE?                        
         BNH   REQF20              NO - NOT A TRANSACTION                       
         CLC   QSELECT,SPACES      FILTER ON CLI CODE IF ENTERED                
         BE    *+14                IN QSELECT                                   
         CLC   TRNKCACT(3),QSELECT                                              
         BNE   REQF20              DOESN'T MATCH - READ SEQUENTIAL              
*                                                                               
         BAS   RE,DMGETACC                                                      
*                                                                               
         L     R4,AIO1                                                          
         USING TRNELD,R2                                                        
         LR    R2,R4               R2 = A(IO1)                                  
         AH    R2,DISP2                                                         
REQF72   CLI   0(R2),0                                                          
         BE    REQF20                                                           
         CLI   0(R2),TRNELQ        X'44' - TRANSACTION ELEMENT                  
         BE    REQF76                                                           
REQF74   SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF72                                                           
*                                                                               
         USING TRNELD,R2                                                        
REQF76   CLC   TRNREF,=C'ADJUST'                                                
         BE    REQF95                                                           
         DROP  R2                                                               
*                                                                               
         USING TRSELD,R2                                                        
         L     R2,AIO1             R2 = A(IO1)                                  
         AH    R2,DISP2                                                         
REQF80   CLI   0(R2),0                                                          
         BE    REQF20                                                           
         CLI   0(R2),TRSELQ        X'60' - TRANSACTION STATUS ELEMENT           
         BE    REQF90                                                           
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF80                                                           
*                                                                               
REQF90   DS    0H                                                               
         CLC   TRSDATE,STDTE         FILTER ON DATES                            
         BL    REQF20                                                           
         CLC   TRSDATE,ENDDTE                                                   
         BH    REQF20                                                           
*                                                                               
REQF95   L     R4,AIO1                                                          
         USING TRNELD,R2                                                        
         LR    R2,R4               R2 = A(IO1)                                  
         AH    R2,DISP2                                                         
REQF100  CLI   0(R2),0                                                          
         BE    REQF20                                                           
         CLI   0(R2),TRNELQ        X'44' - TRANSACTION ELEMENT                  
         BE    REQF120                                                          
REQF110  SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF100                                                          
*                                                                               
REQF120  CLI   QOPT2,C' '          IF BLANK - INCLUDE ALL                       
         BE    REQF140                                                          
         CLI   QOPT2,C'Y'          LIMIT REQ TO TYPES NOT IN TABLE              
         BE    *+12                                                             
         CLI   QOPT2,C'N'                                                       
         BNE   REQF140                                                          
*                                                                               
         LA    R0,TYPTABN          # OF TYPES IN TABLE                          
         LA    R1,TYPTAB                                                        
         CLC   TRNTYPE,0(R1)       ONLY INCLUDE THOSE TYPES IN TABLE            
         BE    REQF130                                                          
         LA    R1,L'TYPTAB(R1)                                                  
         BCT   R0,*-14                                                          
         CLI   QOPT2,C'N'          LIMIT REQ TO TYPES NOT IN TABLE              
         BE    REQF140                                                          
         B     REQF20              DOES NOT MATCH OPTIONS - SKIP                
*                                                                               
REQF130  CLI   QOPT2,C'Y'                                                       
         BNE   REQF20              DOES NOT MATCH OPTIONS - SKIP                
*                                                                               
REQF140  ZAP   ACTDBKT,=P'0'       SZ SIDE DEBIT BUCKET                         
         ZAP   ACTCBKT,=P'0'       SZ SIDE CREDIT BUCKET                        
         ZAP   ACTMDDB,=P'0'       MEDIA SIDE DEBIT BUCKET                      
         ZAP   ACTMDCR,=P'0'       MEDIA SIDE DEBIT BUCKET                      
         MVC   ACTSYSMD,SPACES     CLEAR OUT SYSTEM/MEDIA FIELD                 
*                                                                               
         MVC   ACTCLI,TRNKCACT     CONTRA CLIENT CODE                           
         LA    R1,ACTDBKT          R1=A(DEBIT BUCKET)                           
         TM    TRNSTAT,TRNSDR      DEBIT?                                       
         BO    *+8                                                              
         LA    R1,ACTCBKT          R1=A(CREDIT BUCKET)                          
         ZAP   0(16,R1),TRNAMNT                                                 
         MVC   MSG1,=CL20'TRANSACTION RECORD'                                   
         SR    R5,R5                                                            
         ICM   R5,3,TRNRLEN                                                     
         GOTO1 DUMP,DMCB,AIO1,(R5)                                              
         GOTO1 BINADD,DMCB,ACTWRK,AACTTAB                                       
         B     REQF20                                                           
*                                                                               
REQFX    B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
                                                                                
REQL     DS    0H                                                               
         CLI   QOPT1,C'1'                                                       
         BNE   REQL10                                                           
         MVC   HEADNM,=CL36'SZ LEDGER ACCOUNT CODES'                            
         MVC   PAGE,=H'1'                                                       
         XC    FLAG,FLAG           CLEAR RUN FLAG                               
         MVI   RCSUBPRG,0          SET FOR P/O 1 - BY ACCOUNT CODE              
         BAS   RE,READTAB          READ TABLE AND PRINT                         
*                                                                               
REQL10   CLI   QOPT1,C' '                                                       
         BE    *+12                                                             
         CLI   QOPT1,C'2'                                                       
         BNE   REQLX                                                            
*                                                                               
         CLI   QOPT2,C'N'          IF RUN ON TRN TYPES NOT IN TABLE             
         BE    *+8                     SKIP MEDIA SIDE                          
         BAS   RE,GETMED           GET MEDIA SIDE AND ADD TO TABLE              
         MVC   HEADNM,=CL36'SZ CONTRA CLIENT CODES'                             
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVC   PAGE,=H'1'                                                       
         XC    FLAG,FLAG           CLEAR RUN FLAG                               
         MVI   RCSUBPRG,1          SET FOR P/O 2 - BY CLIENT CODE               
*                                                                               
         BAS   RE,CLISRT           SORT TABLE BY CLIENT AND PRINT               
*                                                                               
         CLI   QOPT2,C'N'          IF I HAVEN'T PROCESSED MEDIA SIDE -          
         BE    REQLX               DON'T TRY TO CREATE POSTINGS                 
         CLI   QOPT5,C'P'                                                       
         BNE   REQLX                                                            
         MVI   FILOPN,C'N'                                                      
         BAS   RE,DOPOST                                                        
*                                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ TABLE AND PROCESS ACCOUNTS - THIS ROUTING IS ONLY PROCESSED    *         
* WHEN QOPT1=1 (SZ ACCOUNTS ONLY - NOT PROCESSING MEDIA SIDE)         *         
***********************************************************************         
                                                                                
READTAB  NTR1                                                                   
         OI    FLAG,FLGACT         TURN ON ACCT RUN BIT                         
         XC    LSTAC,LSTAC         CLEAR AREA FOR LAST ACCOUNT/CLIENT           
         USING PLINED,R4                                                        
         LA    R4,XP                                                            
         USING BIND,R1                                                          
         L     R1,AACTTAB          A(ACCOUNT TABLE)                             
         USING ACTD,R2                                                          
         LA    R2,BINTAB                                                        
         ICM   R5,15,BININ                                                      
         BZ    READTX                                                           
         B     READT20                                                          
*                                                                               
READT10  MVC   XP,XSPACES                                                       
         CLC   ACTACCT,LSTACT      SAME AS BEFORE?                              
         BE    READT25             ONLY PRINT ACCT/NAME ONCE                    
         BAS   RE,ACCTOT           DO ACCOUNT TOTAL HEADINGS                    
         BAS   RE,TOTALS           DO ACCOUNT TOTALS                            
*                                                                               
READT20  MVC   PSZACCT,ACTACCT     SZ ACCOUNT                                   
         MVC   PSZANME,ACTNME      SZ ACCOUNT NAME                              
READT25  MVC   PSZCCLI,ACTCLI      CLIENT CODE                                  
*                                                                               
         CURED (P16,ACTCBKT),(16,PSZCRD),2,ZERO=NOBLANK,MINUS=YES               
         CURED (P16,ACTDBKT),(16,PSZDBT),2,ZERO=NOBLANK,MINUS=YES               
         ZAP   PKSZBAL,ACTCBKT     BAL = CR-DB                                  
         SP    PKSZBAL,ACTDBKT                                                  
         CURED (P16,PKSZBAL),(16,PSZBAL),2,ZERO=NOBLANK,MINUS=YES               
*                                                                               
READT80  GOTO1 ACREPORT                                                         
         AP    PKCRD,ACTCBKT       ADD ACCOUNT TOTALS                           
         AP    PKDEB,ACTDBKT                                                    
         AP    PKBAL,PKSZBAL                                                    
*                                                                               
         AP    PKCDTOT,ACTCBKT     ADD RUN TOTALS                               
         AP    PKDBTOT,ACTDBKT                                                  
*                                                                               
         MVC   LSTACT,ACTACCT                                                   
         LA    R2,ACTLNQ(R2)                                                    
         BCT   R5,READT10                                                       
*                                                                               
READTX   BAS   RE,TOTRUN                                                        
         B     EXIT                                                             
         DROP  R1,R2,R4                                                         
         EJECT                                                                  
***********************************************************************         
* TOTALS FOR ACCOUNT                                                  *         
*         R4 = A(PRINT LINE)                                          *         
***********************************************************************         
                                                                                
         USING PLINED,R4                                                        
TOTALS   NTR1                                                                   
         LA    R4,XP                                                            
         CURED (P16,PKCRD),(16,PSZCRD),2,ZERO=NOBLANK,MINUS=YES                 
         CURED (P16,PKDEB),(16,PSZDBT),2,ZERO=NOBLANK,MINUS=YES                 
         CURED (P16,PKBAL),(16,PSZBAL),2,ZERO=NOBLANK,MINUS=YES                 
*                                                                               
TOTS60   TM    FLAG,FLGACT         ARE WE DOING ACCOUNT TOTALS?                 
         BO    TOTS120             YES - SKIP THE REST                          
*                                                                               
         CURED (P16,PKMDEB),(16,PMDDBT),2,ZERO=NOBLANK,MINUS=YES                
         CURED (P16,PKMCRD),(16,PMDCRD),2,ZERO=NOBLANK,MINUS=YES                
         CURED (P16,PKMBAL),(16,PMDBAL),2,ZERO=NOBLANK,MINUS=YES                
*                                                                               
TOTS120  ZAP   PKCRD,=P'0'                                                      
         ZAP   PKDEB,=P'0'                                                      
         ZAP   PKBAL,=P'0'                                                      
         ZAP   PKMCRD,=P'0'                                                     
         ZAP   PKMDEB,=P'0'                                                     
         ZAP   PKMBAL,=P'0'                                                     
         GOTO1 ACREPORT                                                         
         CLC   LINE,MAXLINES       IF AT BOTTOM OF PAGE                         
         BE    TOTSX               DON'T SKIP LINES                             
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
TOTSX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* TOTALS FOR RUN                                                      *         
*         R4 = A(PRINT LINE)                                          *         
***********************************************************************         
                                                                                
         USING PLINED,R4                                                        
TOTRUN   NTR1                                                                   
         LA    R4,XP                                                            
         LA    RF,ACCTOT           ASSUME IT IS TOTALS FOR ACCOUNT              
         TM    FLAG,FLGACT         ARE WE DOING ACCOUNT TOTALS?                 
         BO    *+8                 YES - DO HEADING                             
         LA    RF,CLITOT           NO - WE ARE DOING CLIENT TOTALS              
         BASR  RE,RF               DO HEADINGS                                  
*                                                                               
         BAS   RE,TOTALS           DO ACCOUNT TOTALS BEFORE RUN TOTALS          
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVC   PRTLNE+2(20),=C'OPENING BALANCE FOR '                            
         GOTO1 ACREPORT                                                         
         LA    R5,1                                                             
         GOTO1 ADDAY,DMCB,QEND,OPDTE,(R5)                                       
         GOTO1 DATCON,DMCB,(0,OPDTE),(10,PSZACCT)                               
*                                                                               
         CURED (P16,PKCDTOT),(16,PSZCRD),2,ZERO=NOBLANK,MINUS=YES               
         CURED (P16,PKDBTOT),(16,PSZDBT),2,ZERO=NOBLANK,MINUS=YES               
         ZAP   PKTOTAL,PKCDTOT                                                  
         SP    PKTOTAL,PKDBTOT                                                  
         CURED (P16,PKTOTAL),(16,PSZBAL),2,ZERO=NOBLANK,MINUS=YES               
*                                                                               
TOTR60   TM    FLAG,FLGACT         ARE WE DOING ACCOUNT TOTALS?                 
         BO    TOTRX               YES - SKIP THE REST                          
*                                                                               
         CURED (P16,PKMDBTOT),(16,PMDDBT),2,ZERO=NOBLANK,MINUS=YES              
         CURED (P16,PKMCDTOT),(16,PMDCRD),2,ZERO=NOBLANK,MINUS=YES              
         ZAP   PKMTOTAL,PKMCDTOT                                                
         SP    PKMTOTAL,PKMDBTOT                                                
         CURED (P16,PKMTOTAL),(16,PMDBAL),2,ZERO=NOBLANK,MINUS=YES              
*                                                                               
TOTRX    ZAP   PKCDTOT,=P'0'                                                    
         ZAP   PKDBTOT,=P'0'                                                    
         ZAP   PKTOTAL,=P'0'                                                    
         ZAP   PKMCDTOT,=P'0'                                                   
         ZAP   PKMDBTOT,=P'0'                                                   
         ZAP   PKMTOTAL,=P'0'                                                   
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* READ MEDIA SIDE AND ADD TO BIN TABLE                                *         
***********************************************************************         
                                                                                
GETMED   NTR1                                                                   
         XC    LSTCLI,LSTCLI                                                    
         OPEN  (INFIL,(INPUT))                                                  
         LTR   RF,RF                                                            
         BZ    GETM10                                                           
         B     EXIT                                                             
*                                                                               
GETM10   XC    FREC,FREC           STORAGE TO READ RECORD INTO                  
         GET   INFIL,FREC                                                       
*                                                                               
         CLC   QSELECT,SPACES      FILTER ON CLI CODE IF ENTERED                
         BE    *+14                IN QSELECT                                   
         CLC   FCLI,QSELECT                                                     
         BNE   GETM10              DOESN'T MATCH - GET NEXT                     
*                                                                               
         USING ACTD,R2                                                          
         LA    R2,ACTWRK                                                        
         XC    ACTWRK,ACTWRK                                                    
         MVC   ACTACCT,SPACES           ACCT CODE IS SPACE FILLED               
         MVC   ACTACCT(2),FSYS          SYSTEM AND MEDIA                        
         MVC   ACTSYSMD,FSYS            SYSTEM AND MEDIA - MEDIA SIDE           
         MVC   ACTCLI,FCLI              CLIENT CODE                             
         ZAP   ACTDBKT,=P'0'            SZ SIDE DEBIT BUCKET                    
         ZAP   ACTCBKT,=P'0'            SZ SIDE CREDIT BUCKET                   
         ZAP   ACTMDDB,FBILL            MEDIA SIDE DEBIT                        
         ZAP   ACTMDCR,FPAID            MEDIA SIDE CREDIT                       
*                                                                               
* COMBINE MULTIPLE SIMILAR SYSTEM/MEDIA INTO ONE                                
*                                                                               
         USING MEDD,R1                                                          
         LA    R1,MEDTAB           R1=A(SYSTEM/MEDIA TABLE)                     
GETM20   CLI   0(R1),EOF                                                        
         BE    GETM30              NOT IN TABLE - CONTINUE                      
         CLC   MEDSYS,ACTACCT      ACTACCT HAS ACTUAL SYSTEM/MEDIA              
         BE    *+12                                                             
         LA    R1,MEDLNQ(R1)                                                    
         B     GETM20                                                           
*                                                                               
         MVC   ACTACCT(L'MEDACT),MEDACT      REPLACE WITH NEW CODE              
         MVC   ACTNME,SPACES                                                    
         MVC   ACTNME(L'MEDNME),MEDNME                                          
         MVC   ACTSYSMD,FSYS                                                    
         MVC   ACTRACT,FSYS                                                     
         DROP  R1                                                               
*                                                                               
GETM30   GOTO1 BINADD,DMCB,ACTWRK,AACTTAB                                       
         B     GETM10                                                           
*                                                                               
GETMEDX  CLOSE INFIL                                                            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SORT BIN TABLE BY CLIENT AND PRINT                                  *         
***********************************************************************         
                                                                                
CLISRT   NTR1                                                                   
         USING BIND,R1                                                          
         L     R1,AACTTAB                                                       
         SR    R0,R0                                                            
         ICM   R0,15,BININ         R0 = NUMBER OF ENTRIES                       
         BZ    CLISX                                                            
         USING ACTD,R3                                                          
         LA    R3,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
* SORT BY CLIENT                                                                
*                                                                               
         LA    R2,ACTLNQ           R2 = LENGTH OF RECORD                        
         LA    R4,ACTSRLNQ         R4 = LENGTH OF FIELD TO QSRT                 
         LA    R5,ACTSRDSP         R5 = DISPLACEMENT TO FIELD                   
         GOTO1 XSORT,DMCB,(0,(R3)),(R0),(R2),(R4),(R5)                          
         DROP  R3                                                               
*                                                                               
         XC    LSTAC,LSTAC         CLEAR AREA FOR LAST ACCOUNT/CLIENT           
         LA    R0,PRTAREA                                                       
         LA    R1,PRTALNQ                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,XP                                                            
         USING BIND,R1                                                          
         L     R1,AACTTAB          A(ACCOUNT TABLE)                             
         USING ACTD,R2                                                          
         LA    R2,BINTAB                                                        
         ICM   R5,15,BININ                                                      
         BZ    CLISX                                                            
         B     CLIS20                                                           
*                                                                               
CLIS10   MVC   XP,XSPACES                                                       
         XC    FLAG,FLAG                                                        
         CLC   ACTCLI,LSTCLI       SAME AS BEFORE?                              
         BE    CLIS30              ONLY PRINT ACCT/NAME ONCE                    
         BAS   RE,CLITOT           DO CLIENT TOTALS HEADING                     
         TM    FLAG,FLGCLPRT       ARE THE TOTALS ZEROS?                        
         BO    *+8                                                              
         BAS   RE,TOTALS           DO CLIENT TOTALS                             
*                                                                               
CLIS20   MVC   P2CLI,ACTCLI        CLIENT CODE                                  
         CLC   P2CLI,SPACES                                                     
         BNE   *+10                                                             
         MVC   P2CLI(4),=C'NONE'                                                
         GOTO1 ACREPORT            PRINT OUT CLIENT CODE                        
*                                                                               
         USING MEDD,R1                                                          
CLIS30   MVC   P2ACCT,ACTACCT      SZ ACCOUNT                                   
         LA    R1,MEDTAB                                                        
CLIS40   CLI   0(R1),EOF                                                        
         BE    CLIS70                                                           
         CLC   MEDACT,ACTACCT                                                   
         BE    *+12                                                             
         LA    R1,MEDLNQ(R1)                                                    
         B     CLIS40                                                           
*                                                                               
         OI    FLAG,FLGSYSMD       SHOW IT IS A SYS MEDIA RUN                   
         MVC   XP,XSPACES                                                       
         MVC   SVNME,MEDNME                                                     
         OC    LSTACT,LSTACT                                                    
         BNZ   CLIS50              IF 1ST TIME THRU - SKIP                      
         GOTO1 ACREPORT                                                         
         B     CLIS60                                                           
CLIS50   CLC   LSTACT,ACTACCT      SAME ACCOUNT AS BEFORE?                      
         BE    CLIS60                                                           
         BAS   RE,PRTPREV          PRINT AND ZAP PREVIOUS TOTALS                
*                                                                               
CLIS60   MVC   LSTACT,ACTACCT                                                   
         MVC   P2ACCT,SPACES                                                    
         MVC   P2ACCT(2),ACTRACT   SZ ACCOUNT                                   
*                                                                               
CLIS70   MVC   P2ANME,ACTNME       SZ ACCOUNT NAME                              
         MVC   SVMEDNAM,ACTNME     NEED THIS FOR FINAL S9 POSTING               
         MVC   PSYSMD,ACTSYSMD     ORIGINAL MEDIA SIDE SYSTEM/MEDIA             
*                                                                               
         CURED (P16,ACTCBKT),(16,PSZCRD),2,ZERO=NOBLANK,MINUS=YES               
         CURED (P16,ACTDBKT),(16,PSZDBT),2,ZERO=NOBLANK,MINUS=YES               
*                                                                               
         ZAP   PKSZBAL,ACTCBKT     BAL = CR-DB                                  
         SP    PKSZBAL,ACTDBKT                                                  
*                                                                               
         CURED (P16,PKSZBAL),(16,PSZBAL),2,ZERO=NOBLANK,MINUS=YES               
         CURED (P16,ACTMDDB),(16,PMDDBT),2,ZERO=NOBLANK,MINUS=YES               
         CURED (P16,ACTMDCR),(16,PMDCRD),2,ZERO=NOBLANK,MINUS=YES               
*                                                                               
         ZAP   PKMDBAL,ACTMDDB      BAL = DB-CR                                 
         SP    PKMDBAL,ACTMDCR                                                  
*                                                                               
         CURED (P16,PKMDBAL),(16,PMDBAL),2,ZERO=NOBLANK,MINUS=YES               
*                                                                               
CLIS90   TM    FLAG,FLGSYSMD       IF SYS/MED - DON'T PRINT                     
         BNO   *+12                                                             
         BAS   RE,PUTPRT                                                        
         B     CLIS140             DONT ADD TO TOTAL NOW-DO IN PRTPREV          
*                                                                               
         CLI   QOPT3,C' '          IF BLANK - INCLUDE ALL                       
         BE    CLIS110                                                          
         CLI   QOPT3,C'N'          SKIP EQUAL AMOUNTS                           
         BNE   *+12                                                             
         MVI   CLIS100+1,X'80'     SET BRANCH FOR EQUAL                         
         B     *+16                                                             
         CLI   QOPT3,C'Y'          SKIP NON-EQUAL AMOUNTS                       
         BNE   CLIS110                                                          
         MVI   CLIS100+1,X'70'     SET BRANCH FOR NOT EQUAL                     
*                                                                               
         CP    PKSZBAL,PKMDBAL                                                  
CLIS100  B     CLIS140                                                          
*                                                                               
CLIS110  CLI   QOPT4,C'Y'          SUPPRESS ZERO TOTALS                         
         BNE   CLIS130                                                          
*                                                                               
         LA    R0,PKSYMDQ                                                       
         LA    R1,PKSYMD                                                        
CLIS120  CP    0(L'PKFLDS,R1),=P'0'                                             
         BNE   CLIS130                                                          
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,CLIS120                                                       
         B     PRTPX                                                            
*                                                                               
CLIS130  GOTO1 ACREPORT                                                         
*                                                                               
         AP    PKCRD,ACTCBKT       ADD CLIENT TOTALS                            
         AP    PKDEB,ACTDBKT                                                    
         AP    PKBAL,PKSZBAL                                                    
*                                                                               
         AP    PKMDEB,ACTMDDB      ADD MEDIA TOTALS                             
         AP    PKMCRD,ACTMDCR                                                   
         AP    PKMBAL,PKMDBAL                                                   
*                                                                               
         AP    PKCDTOT,ACTCBKT     ADD RUN TOTALS                               
         AP    PKDBTOT,ACTDBKT                                                  
         AP    PKMDBTOT,ACTMDDB                                                 
         AP    PKMCDTOT,ACTMDCR                                                 
*                                                                               
CLIS140  DS    0H                                                               
         CLI   QOPT5,C'P'                                                       
         BNE   CLIS150                                                          
         ZAP   PKWORK,PKSZBAL          IF MEDIA BAL=SZ BAL, NO POSTINGS         
         SP    PKWORK,PKMDBAL          NEED TO BE MADE                          
         CP    PKWORK,=P'0'                                                     
         BE    CLIS150                                                          
*                                                                               
         L     RE,NUMBCLMD             INCREMENT TABLE COUNTER AND              
         LA    RE,1(RE)                CHECK IT AGAINST TABLE MAX               
         C     RE,MXCLIMED                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         ST    RE,NUMBCLMD             STILL WITHIN LIMITS, SAVE COUNT          
*                                                                               
         USING CLIMEDD,RE                                                       
         L     RE,ACURCLMD             CURRENT SPOT IN TABLE                    
         MVC   PSTMED,ACTRACT          POSTING MEDIA                            
         MVC   PSTCLI,ACTCLI           POSTING CLIENT                           
         ZAP   PSTAMT,PKWORK           DEBIT AMT TO BE POSTED                   
         LA    RE,PSTLNQ(RE)           BUMP TO NEXT TABLE ENTRY                 
         ST    RE,ACURCLMD             SAVE ADDRESS                             
         DROP  RE                                                               
*                                                                               
CLIS150  MVC   LSTCLI,ACTCLI                                                    
         MVC   LSTNME,SVNME                                                     
         LA    R2,ACTLNQ(R2)                                                    
         BCT   R5,CLIS10                                                        
*                                                                               
CLISX    BAS   RE,TOTRUN                                                        
         B     EXIT                                                             
         DROP  R1,R2,R4                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT PREVIOUS TOTALS AND ZAP AMOUNTS                               *         
***********************************************************************         
                                                                                
PRTPREV  NTR1                                                                   
         LA    R0,PRTAQ            # OF SAVED AREA                              
         LA    R1,PRTAREA          FIND OPEN PRINT AREA                         
         OC    0(PAREAQ,R1),0(R1)                                               
         BNZ   *+16                                                             
         LA    R1,PAREAQ(R1)                                                    
         BCT   R0,*-14                                                          
         B     PRTPX                                                            
*                                                                               
         LA    R0,PKSYMDQ                                                       
         LA    R1,PKSYMD                                                        
         ZAP   0(L'PKFLDS,R1),=P'0'                                             
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         USING PAREAD,R1                                                        
         LA    R1,PRTAREA                                                       
PRTP10   OC    0(PAREAQ,R1),0(R1)                                               
         BZ    PRTP20                                                           
         AP    PKSDEB,PASZDBT      DEBITS                                       
         AP    PKSCRD,PASZCRD      CREDITS                                      
         AP    PKSMDEB,PAMDDBT     MEDIA SIDE DEBITS                            
         AP    PKSMCRD,PAMDCRD     MEDIA SIDE CREDITS                           
         LA    R1,PAREAQ(R1)                                                    
         B     PRTP10                                                           
         DROP  R1                                                               
*                                                                               
PRTP20   ZAP   PKSBAL,PKSCRD       BAL = CR-DB                                  
         SP    PKSBAL,PKSDEB                                                    
         ZAP   PKSMBAL,PKSMCRD     BAL = CR-DB                                  
         SP    PKSMBAL,PKSMDEB                                                  
*                                                                               
         CLI   QOPT3,C' '          IF BLANK - INCLUDE ALL                       
         BE    PRTP40                                                           
         CLI   QOPT3,C'N'          SKIP EQUAL AMOUNTS                           
         BNE   *+12                                                             
         MVI   PRTP30+1,X'80'      SET BRANCH FOR EQUAL                         
         B     *+16                                                             
         CLI   QOPT3,C'Y'          SKIP NON-EQUAL AMOUNTS                       
         BNE   PRTP40                                                           
         MVI   PRTP30+1,X'70'      SET BRANCH FOR NOT EQUAL                     
*                                                                               
         CP    PKSBAL,PKSMBAL                                                   
PRTP30   B     PRTPX                                                            
*                                                                               
PRTP40   CLI   QOPT4,C'Y'          SUPPRESS ZERO TOTALS                         
         BNE   PRTP60                                                           
*                                                                               
         LA    R0,PKSYMDQ                                                       
         LA    R1,PKSYMD                                                        
PRTP50   CP    0(L'PKFLDS,R1),=P'0'                                             
         BNE   PRTP60                                                           
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,PRTP50                                                        
         B     PRTPX                                                            
*                                                                               
PRTP60   AP    PKCRD,PKSCRD        ADD CLIENT TOTALS                            
         AP    PKDEB,PKSDEB                                                     
         AP    PKBAL,PKSBAL                                                     
*                                                                               
         AP    PKMDEB,PKSMDEB      ADD MEDIA TOTALS                             
         AP    PKMCRD,PKSMCRD                                                   
         AP    PKMBAL,PKSMBAL                                                   
*                                                                               
         AP    PKCDTOT,PKSCRD      ADD RUN TOTALS                               
         AP    PKDBTOT,PKSDEB                                                   
         AP    PKMDBTOT,PKSMDEB                                                 
         AP    PKMCDTOT,PKSMCRD                                                 
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,XP                                                            
         MVC   XP,XSPACES                                                       
*                                                                               
         LA    R1,PRTAREA                                                       
PRTP70   OC    0(PAREAQ,R1),0(R1)                                               
         BZ    PRTP80              PRINT TOTALS                                 
         MVC   0(PRLNQ,R4),0(R1)   SEND BUFFER AREA TO PRINT                    
         GOTO1 ACREPORT                                                         
         LA    R1,PAREAQ(R1)                                                    
         B     PRTP70                                                           
*                                                                               
PRTP80   MVC   P2ACCT+4(10),=C'SUBTOTAL :'                                      
         MVC   P2ANME,LSTNME                                                    
*                                                                               
         CURED (P16,PKSCRD),(16,PSZCRD),2,ZERO=NOBLANK,MINUS=YES                
         CURED (P16,PKSDEB),(16,PSZDBT),2,ZERO=NOBLANK,MINUS=YES                
         CURED (P16,PKSBAL),(16,PSZBAL),2,ZERO=NOBLANK,MINUS=YES                
         CURED (P16,PKSMDEB),(16,PMDDBT),2,ZERO=NOBLANK,MINUS=YES               
         CURED (P16,PKSMCRD),(16,PMDCRD),2,ZERO=NOBLANK,MINUS=YES               
         CURED (P16,PKSMBAL),(16,PMDBAL),2,ZERO=NOBLANK,MINUS=YES               
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
PRTPX    LA    R0,PRTAREA                                                       
         LA    R1,PRTALNQ                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,PKSYMDQ                                                       
         LA    R1,PKSYMD                                                        
         ZAP   0(L'PKFLDS,R1),=P'0'                                             
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PUT CURRENT LINE TO SAVED AREA                                      *         
*     R2 = A(BINADD ENTRY)                                            *         
***********************************************************************         
                                                                                
         USING PAREAD,R1                                                        
         USING ACTD,R2                                                          
PUTPRT   NTR1                                                                   
         LA    R1,PRTAREA          FIND OPEN PRINT AREA                         
         OC    0(PAREAQ,R1),0(R1)                                               
         BZ    *+12                                                             
         LA    R1,PAREAQ(R1)                                                    
         B     *-14                                                             
*                                                                               
         MVC   0(PRLNQ,R1),0(R4)   MOVE PRINT LINE INTO BUFFER                  
         MVC   XP,XSPACES                                                       
*                                                                               
         ZAP   PASZCRD,ACTCBKT                                                  
         ZAP   PASZDBT,ACTDBKT                                                  
         ZAP   PAMDDBT,ACTMDDB                                                  
         ZAP   PAMDCRD,ACTMDCR                                                  
*                                                                               
PUTPX    B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* HEADING FOR CLIENT TOTALS                                           *         
*         R4 = A(PRINT LINE)                                          *         
***********************************************************************         
                                                                                
         USING PLINED,R4                                                        
CLITOT   NTR1                                                                   
         LA    R4,XP                                                            
         BAS   RE,PRTPREV          PRINT PREVIOUS SYS/MED SUB TOTALS            
         GOTO1 ACREPORT                                                         
*                                                                               
         CLI   QOPT4,C'Y'          SUPPRESS ZERO TOTALS                         
         BNE   CLIT20                                                           
*                                                                               
         LA    R0,PKCLIQ                                                        
         LA    R1,PKCLI                                                         
CLIT10   CP    0(L'PKFLDS,R1),=P'0'                                             
         BNE   CLIT20                                                           
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,CLIT10                                                        
         MVC   P2ACCT+4(25),=C'ZERO TOTALS - NOT PRINTED'                       
         OI    FLAG,FLGCLPRT       ZEROES - DON'T PRINT                         
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         B     CLITX                                                            
*                                                                               
CLIT20   MVC   PRTLNE+2(20),=C'TOTALS FOR CLIENT  :'                            
         MVC   PRTLNE+23(3),LSTCLI        LAST CLIENT TOTALS                    
CLITX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* HEADING FOR ACCOUNT TOTALS                                          *         
*         R4 = A(PRINT LINE)                                          *         
***********************************************************************         
                                                                                
         USING PLINED,R4                                                        
ACCTOT   NTR1                                                                   
         MVC   PRTLNE+2(20),=C'TOTALS FOR ACCOUNT :'                            
         GOTO1 ACREPORT                                                         
         MVC   PSZACCT,LSTACT      LAST ACCOUNT TOTALS                          
ACCTOTX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
                                                                                
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
*                                                                               
         L     R1,DMCB             A(RECORD FOUND)                              
         USING ACTD,R2                                                          
         LR    R2,R3                 ITEM TO ADDED                              
         CLC   ACTSYSMD,SPACES       IF CLEAR DONT ADD                          
         BE    *+10                                                             
         MVC   ACTSYSMD-ACTD(L'ACTSYSMD,R1),ACTSYSMD                            
         DROP  R2                                                               
*                                                                               
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINXIT              NO BUCKETS - EXIT                            
         SR    R6,R6                                                            
         IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
         AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
         AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
         AP    0(16,R4),0(16,R3)   ADD TO BUCKET                                
         LA    R3,L'ACTBKT(R3)     BUMP TO NEXT ENTRY IN NEW ITEM               
         LA    R4,L'ACTBKT(R4)     BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,*-14                                                          
*                                                                               
BINXIT   B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CREATE POSTINGS TO PUT FILE BACK IN BALANCE WITH MEDIA SIDE         *         
***********************************************************************         
DOPOST   NTR1                                                                   
*                                                                               
         USING BOXD,R4                                                          
         L     R4,ADBOX                                                         
         MVI   BOXCOLS+(PSYSMD-PRTLNE-1),C' '     RESET COLUMN                  
         DROP  R4                                                               
*                                                                               
         L     R6,NUMBCLMD                    DO ANY MEDIA/CLIENTS              
         LTR   R6,R6                          NEED POSTINGS                     
         BNZ   DOP10                                                            
         MVI   FORCEHED,C'Y'                  FORCE NEW PAGE                    
         USING PLINED,R4                                                        
         LA    R4,XP                                                            
         MVC   XP,XSPACES                                                       
         MVC   PSZACCT(19),=C'NO POSTINGS CREATED'                              
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
*                                                                               
DOP10    CLI   FILOPN,C'Y'                                                      
         BE    DOP20                                                            
         CLI   RCPOSTNG,C'N'                                                    
         BE    DOP20                                                            
         XC    ID,ID                                                            
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'AA1'                WORKER FILE ID                    
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
         OI    ID+12,X'08'                CREATE FILE ON KEEP                   
         OI    ID+13,X'01'                ALLOW DUPLICATE FILE ID'S             
         MVC   COMMAND,=CL6'OPEN'                                               
         L     R4,=A(POSTBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,POSTHEAD                             
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
DOP20    MVI   FILOPN,C'Y'                                                      
*                                                                               
         ZAP   CSHOUTD,=P'0'                                                    
         ZAP   RECOUT,=P'0'                                                     
         USING CLIMEDD,R5                                                       
         L     R5,ACLIMED                                                       
         L     R6,NUMBCLMD                                                      
         MVC   SVMEDIA,PSTMED                                                   
*                                                                               
         USING PSHEADD,R2                                                       
DOP30    LA    R2,POSTAREA                                                      
         MVC   0(PSHEADL,R2),SPACES                                             
         MVC   PSHDEL(2),=X'5046'           BUILD POSTING HEADER                
         MVC   PSHDACC(1),RCCOMPFL                                              
         MVC   PSHDACC+1(2),=C'SZ'                                              
         MVC   PSHDACC+3(L'PSTMED),PSTMED                                       
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC(1),RCSVCOMP                                             
         MVC   PSHDSBAC+1(2),=C'SJ'                                             
         MVC   PSHDSBAC+3(L'PSTCLI),PSTCLI                                      
         MVC   PSHDANAL,SPACES                                                  
*                                                                               
         BAS   RE,READCLI                                                       
         MVC   PSHDSBNM,CLINAME             CLIENT NAME                         
*                                                                               
         ZIC   R1,1(R2)                     BUMP TO NEXT ELEMENT                
         AR    R2,R1                                                            
         USING TRNELD,R2                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q                                                    
         MVC   TRNDATE,RUNDATE                                                  
         MVC   TRNREF,=C'ADJUST'                                                
         MVI   TRNSUB,0                     SUB REFNO                           
         MVI   TRNTYPE,0                    ONE SIDED POSTING                   
         MVI   TRNSTAT,X'80'                DEBITS                              
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNMOS,=C'6C'                MONTH OF SERVICE                    
         ZAP   TRNAMNT,PSTAMT               AMOUNT                              
         MVC   TRNOFFC,CLIOFF               CLIENT OFFICE                       
*                                                                               
         AP    RECOUT,=P'1'                 RECORD COUNTER                      
         AP    CSHOUTD,PSTAMT               CASH ACCUMULATOR                    
         BAS   RE,POSTIT                    POST THE RECORD                     
         LA    R5,PSTLNQ(R5)                                                    
         BCT   R6,DOP30                                                         
*                                                                               
         USING PSHEADD,R2                                                       
         LA    R2,POSTAREA                                                      
         MVC   0(PSHEADL,R2),SPACES                                             
         MVC   PSHDEL(2),=X'5046'           BUILD POSTING HEADER                
         MVC   PSHDACC(1),RCCOMPFL                                              
         MVC   PSHDACC+1(3),=C'S9Z'                                             
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC(1),RCSVCOMP                                             
         MVC   PSHDSBAC+1(2),=C'SZ'                                             
         MVC   PSHDSBAC+3(L'PSTMED),SVMEDIA                                     
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBNM,SVMEDNAM            MEDIA NAME                          
*                                                                               
         ZIC   R1,1(R2)                     BUMP TO NEXT ELEMENT                
         AR    R2,R1                                                            
         USING TRNELD,R2                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q                                                    
         MVC   TRNDATE,RUNDATE                                                  
         MVC   TRNREF,=C'ADJUST'                                                
         MVI   TRNSUB,0                     SUB REFNO                           
         MVI   TRNTYPE,0                    ONE SIDED POSTING                   
         MVI   TRNSTAT,X'00'                CREDITS                             
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNMOS,=C'6C'                MONTH OF SERVICE                    
         ZAP   TRNAMNT,CSHOUTD              AMOUNT                              
         MVC   TRNOFFC,CLIOFF               CLIENT OFFICE                       
         AP    RECOUT,=P'1'                 RECORD COUNTER                      
         BAS   RE,POSTIT                    POST THE RECORD                     
*                                                                               
         LA    R2,POSTAREA                                                      
         USING PSSUBFD,R2                                                       
         MVI   PSSBEL,PSSBELQ                                                   
         MVI   PSSBLEN,X'1D'                                                    
         MVC   PSSBDESC,=CL16'SZ BALANCING'                                     
         ZAP   PSSBRECS,RECOUT                                                  
         ZAP   PSSBCASH,CSHOUTD                                                 
         SR    R3,R3                                                            
         IC    R3,PSSBLEN                                                       
         AR    R3,R2                                                            
         MVI   0(R3),X'00'                     END OF RECORD                    
         LA    R3,1(R3)                        COMPUTE RECORD LENGTH            
         LA    R1,POSTHEAD                                                      
         SR    R3,R1                           END LESS START                   
         XC    POSTHEAD,POSTHEAD                                                
         STH   R3,POSTHEAD                                                      
         MVC   COMMAND,=CL6'ADD'                                                
         BAS   RE,FILE                      POST TO WORKER FILE                 
*                                                                               
         MVC   COMMAND,=CL6'CLOSE'                                              
         BAS   RE,FILE                      POST TO WORKER FILE                 
*                                                                               
DOPXIT   B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
*                                                                               
*********************************************************************           
POSTIT   NTR1                                                                   
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         MVI   0(R2),0                      SET END OF RECORD                   
         LA    R2,1(R2)                     COMPUTE RECORD LENGTH               
         LA    R1,POSTHEAD                                                      
         SR    R2,R1                        END LESS START                      
         XC    POSTHEAD,POSTHEAD                                                
         STH   R2,POSTHEAD                                                      
         MVC   COMMAND,=CL6'ADD'                                                
         BAS   RE,FILE                      POST TO WORKER FILE                 
PST150   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
*              WORKER FILE INTERFACE                                *           
*********************************************************************           
FILE     NTR1                                                                   
         CLI   RCPOSTNG,C'N'                                                    
         BE    EXIT                                                             
         L     R4,=A(POSTBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,POSTHEAD                             
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ CLIENT RECORD TO GET CLIENT NAME AND CLIENT OFFICE             *         
***********************************************************************         
READCLI  NTR1                                                                   
         MVC   CLINAME,SPACES              CLIENT NAME FOR CONTRA               
         MVC   CLIOFF,SPACES                                                    
*                                                                               
         USING ACTRECD,R4                                                       
         LA    R4,DKEY                     R4 = A(KEY)                          
         MVC   DKEY,SPACES                                                      
         MVC   ACTKCPY,RCCOMPFL            COMPANY CODE                         
         MVC   ACTKUNT(2),=C'SJ'           PROD U/L                             
         MVC   ACTKACT(L'PSTCLI),PSTCLI    CLIENT CODE                          
*                                                                               
         BAS   RE,DMHIADIR                 DIRECTORY READ                       
         CLC   DKEY,DIR                    DID I GET A MATCH                    
         BE    RDCLI10                     IF YES, GET RECORD                   
*                                                                               
         MVC   CLINAME(L'PSTCLI),PSTCLI    USE CLIENT CODE FOR NAME             
RDCLI02  CLI   NEWOFFC,C'Y'                IS ID ON NEW OFFICES                 
         BNE   EXIT                        IF NOT, ISN'T NECESSARY              
*                                                                               
         USING OFFTABD,R1                                                       
         LA    R1,OFFTAB                   ELSE GET DEFAULT FROM TABLE          
RDCLI03  CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TABCOMP,HEXCOMP             MATCH COMPANY CODE                   
         BE    RDCLI05                                                          
         LA    R1,OFFTLEN(R1)                                                   
         B     RDCLI03                                                          
RDCLI05  MVC   CLIOFF,DFLTOFF              EXTRACT OFFICE FROM TABLE            
         B     EXIT                                                             
*                                                                               
RDCLI10  BAS   RE,DMGETACC                                                      
*                                                                               
         USING NAMELD,R3                                                        
         L     R3,AIO1                     R2 = A(IO1)                          
         AH    R3,DISP2                                                         
RDCLI15  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),NAMELQ                GET NAME ELEMENT                     
         BE    RDCLI20                                                          
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     RDCLI15                                                          
*                                                                               
RDCLI20  ZIC   R1,1(R3)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLINAME(0),NAMEREC                                               
*                                                                               
         USING PPRELD,R3                                                        
         L     R3,AIO1                     R2 = A(IO1)                          
         AH    R3,DISP2                                                         
RDCLI30  CLI   0(R3),0                                                          
         BE    RDCLI02                                                          
         CLI   0(R3),PPRELQ                GET PROFILE ELEMENT                  
         BE    RDCLI40                                                          
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     RDCLI30                                                          
RDCLI40  MVC   CLIOFF,PPRGAOFF             EXTRACT CLIENT OFFICE                
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
                                                                                
         USING ACCRECD,R2                                                       
DMHIADIR NTR1                      DMRDHI FOR ACCT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCDIR',DKEY,DIR,0                
         LA    R2,DIR                                                           
         MVC   DA,ACCKDA           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMSEADIR NTR1                      DMRSEQ FOR ACCT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'ACCDIR',DKEY,DIR,0                
         LA    R2,DIR                                                           
         MVC   DA,ACCKDA           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMGETACC NTR1                      GETREC FOR ACCT FILE                         
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'ACCMST',DA,AIO1,DMWORK            
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMHISDIR NTR1                      DMRDHI FOR SPOT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'SPTDIR',DKEY,DIR,0                
         MVC   DA,DIR+14           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMSESDIR NTR1                      DMRSEQ FOR SPOT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'SPTDIR',DKEY,DIR,0                
         MVC   DA,DIR+14           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMGETSPT NTR1                      GETREC FOR SPOT FILE                         
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'SPTFILE',DA,AIO1,DMWORK           
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMHIPDIR NTR1                      DMRDHI FOR PRINT DIR                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'PRTDIR',DKEY,DIR,0                
         MVC   DA,DIR+27           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMSEPDIR NTR1                      DMRSEQ FOR PRINT DIR                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'PRTDIR',DKEY,DIR,0                
         MVC   DA,DIR+27           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMGETPRT NTR1                      GETREC FOR PRINT FILE                        
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'PRTFILE',DA,AIO1,DMWORK           
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMCHK    NTR1                      CHECK RETURNS ON DATAMGR CALLS               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
                                                                                
DUMP     NTR1                                                                   
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         L     R3,0(R1)            A(RECORD TO DUMP)                            
         L     R4,4(R1)            LENGTH OF RECORD                             
         CP    MAXDMP,DMPTOT       CHECK IF MAXIMUM WAS REACHED                 
         BNH   DUMPX                                                            
         AP    DMPTOT,=P'1'                                                     
         LA    R0,L'MSG1                                                        
         LA    R2,MSG1                                                          
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOCAL STORAGE                                                       *         
***********************************************************************         
                                                                                
FREC     DS    0CL31               OUTPUT FILE RECORD                           
FSYS     DS    CL1                 SYSTEM                                       
FMED     DS    CL1                 MEDIA                                        
FCLI     DS    CL3                 CLIENT                                       
FMOA     DS    XL2                 MOA(PWOS)                                    
FBILL    DS    PL8                 BILLED                                       
FPAID    DS    PL8                 PAID                                         
FBALN    DS    PL8                 BALANCE                                      
*                                                                               
PRTAREA  DS    0C                  SAVED AREA FOR PRINT LINES                   
PRTAREA1 DS    CL(PAREAQ)                                                       
PRTAREA2 DS    CL(PAREAQ)                                                       
PRTAREA3 DS    CL(PAREAQ)                                                       
PRTAREA4 DS    CL(PAREAQ)                                                       
PRTALNQ  EQU   *-PRTAREA                                                        
PRTAQ    EQU   (*-PRTAREA)/PAREAQ                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
                                                                                
         USING BIGPRNTD,R8                                                      
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R8,VBIGPRNT                                                      
         L     R4,ADBOX                                                         
*                                                                               
         CLI   RCSUBPRG,0                                                       
         BNE   BXHK10                                                           
         MVC   XHEAD2+69(L'HEADNM),HEADNM          SYSTEM NAME                  
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+(PSZANME-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PSZCCLI-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PSZDBT-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PSZCRD-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PSZBAL-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PSYSMD-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PMDDBT-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PMDCRD-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PMDBAL-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+PRLNQ,C'R'                                               
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
BXHK10   CLI   RCSUBPRG,1                                                       
         BNE   BXXIT                                                            
         MVC   XHEAD2+69(L'HEADNM),HEADNM          SYSTEM NAME                  
*                                                                               
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+(PSYSMD-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+PRLNQ,C'R'                                               
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
BXXIT    XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
                                                                                
UTL      DS    0D                                                               
         DC    F'0',X'02'                                                       
         DC    XL3'00'                                                          
*                                                                               
MXRLNQ   EQU   2000                MAX RECORD SIZE                              
*                                                                               
MXCLIMED DC    F'5000'                                                          
*                                                                               
DMPTOT   DC    PL4'0'              DUMP COUNT                                   
MAXDMP   DC    PL4'15'             MAXIMUM DUMPS                                
*                                                                               
RECOUT   DC    PL8'0'              COUNT OF POSTING RECORDS ADDED               
CSHOUTD  DC    PL16'0'             CASH ADDED                                   
FILOPN   DC    C'N'                HAS POSTING FILE BEEN OPEN                   
NEWOFFC  DC    C'N'                IS ID 2 CHAR OFFICES                         
HEXCOMP  DC    X'00'                                                            
*                                                                               
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
*                                                                               
PRNTBL   DC    V(PRNTBL)                                                        
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
*                                                                               
AIO1     DC    A(IO1)                                                           
AACTTAB  DC    A(ACTTAB)                                                        
*                                                                               
ACLIMED  DC    A(CLIMED)           TABLE OF MED/CLI TO POST TO                  
ACURCLMD DC    A(CLIMED)           NEXT AVAILABLE ENTRY IN TABLE                
NUMBCLMD DC    F'0'                NUMBER OF ENTRIES                            
*                                                                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=116 '                                     
SORTCARD DC    C'SORT FIELDS=(1,8,A),FORMAT=BI,WORK=1  '                        
         EJECT                                                                  
***********************************************************************         
* DCB                                                                 *         
***********************************************************************         
                                                                                
INFIL    DCB   DDNAME=INFIL,DSORG=PS,MACRF=(GM),RECFM=FB,              X        
               LRECL=31,BLKSIZE=6200,EODAD=GETMEDX                              
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
                                                                                
TYPTAB   DS    0X                                                               
         DC    X'00'               '00' - CLEARENCES                            
         DC    X'09'               '09' - BILLING TRANSFER TYPE                 
         DC    X'22'               '33' - BILLING TRANSFER TYPE                 
         DC    X'23'               '34' - BILLING TRANSFER TYPE                 
         DC    X'31'               '49' - BILLING TRANSFER TYPE                 
         DC    X'32'               '50' - BILLING TRANSFER TYPE                 
TYPTABN  EQU   (*-TYPTAB)/L'TYPTAB                                              
*                                                                               
MEDTAB   DS    0C      TABLE OF ORIGINAL CODES AND THEIR REPLACEMENTS           
         DC    C'MA',C'11',CL25'MAGAZINE'                                       
         DC    C'NA',C'10',CL25'NEWSPAPER'                                      
         DC    C'NC',C'10',CL25'CABLE'                                          
         DC    C'NN',C'10',CL25'NETWORK'                                        
         DC    C'NS',C'10',CL25'SYNDICATED'                                     
         DC    C'OA',C'12',CL25'OUTDOOR'                                        
         DC    C'PA',C'13',CL25'TRADE'                                          
         DC    C'PM',C'11',CL25'MAGAZINE'                                       
         DC    C'PN',C'10',CL25'NEWSPAPER'                                      
         DC    C'PO',C'12',CL25'OUTDOOR'                                        
         DC    C'PS',C'PS',CL25'SUPPLEMENT'                                     
         DC    C'PT',C'13',CL25'TRADE'                                          
         DC    C'RA',C'14',CL25'SPOT RADIO'                                     
         DC    C'SA',C'17',CL25'SPOT TELEVISION'                                
         DC    C'SN',C'15',CL25'NETWORK MEDIA CONTROL'                          
         DC    C'SR',C'14',CL25'SPOT RADIO'                                     
         DC    C'ST',C'17',CL25'SPOT TELEVISION'                                
         DC    C'SX',C'16',CL25'SPOT NETWORK RADIO'                             
         DC    C'UA',C'15',CL25'NETWORK MEDIA CONTROL'                          
         DC    C'UG',C'15',CL25'NETWORK MEDIA CONTROL'                          
         DC    C'UN',C'15',CL25'NETWORK MEDIA CONTROL'                          
         DC    C'VA',C'16',CL25'SPOT NETWORK RADIO'                             
         DC    AL1(EOF)                                                         
*                                                                               
OFFTAB   DS    0C                                                               
YBMSFS   DC    X'94',C'BO'                                                      
YSHNY    DC    X'B2',C'H6'                                                      
YCHNY    DC    X'B8',C'C9'                                                      
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
                                                                                
         DC    C'**IO1***'                                                      
IO1      DC    (MXRLNQ)X'00'                                                    
*                                                                               
* BINTABLE CONSTANTS FOR TABLE                                                  
*                                                                               
ACTTAB   DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(ACTLNQ)         LENGTH OF ENTRY                              
         DC    AL1(0)              DISP. TO KEY                                 
         DC    AL3(ACTKLNQ)        KEY LENGTH                                   
         DC    AL4(ACTMAX)         MAX IN TABLE                                 
         DC    AL1(ACTBKCT)        NUMBER OF BUCKETS                            
         DC    AL1(ACTBKT-ACTD)    DISPLACEMENT TO FIRST BUCKET                 
         DS    (ACTMAX*ACTLNQ)XL1  TABLE                                        
*                                                                               
ACTMAX   EQU   8000                                                             
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
                                                                                
ACSZD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    F                                                                
DISP2    DS    H                                                                
*                                                                               
SAVEKEY  DS    CL(L'ACCKEY)        SAVED AREA FOR KEY                           
SVACT    DS    CL12                SAVED AREA FOR ACCOUNT                       
SVNME    DS    CL25                SAVED AREA FOR SYS/MEDIA NAME                
*                                                                               
CLINAME  DS    CL36                CLIENT NAME                                  
CLIOFF   DS    CL2                 CLIENT OFFICE                                
SVMEDIA  DS    CL2                 SAVE MEDIA FOR FINAL S9 POSTING              
SVMEDNAM DS    CL36                SAVE MEDIA NAME                              
RUNDATE  DS    PL3                 CONTROL CARD RUN DATE                        
*                                                                               
PKFLDS   DS    0PL16               START OF PACKED FIELDS                       
PKCLI    DS    0P                  CLIENT AMOUNTS                               
PKCRD    DS    PL16                PACKED CREDIT FIELDS                         
PKDEB    DS    PL16                PACKED DEBIT FIELDS                          
PKBAL    DS    PL16                PACKED BALANCE(DB-CR)                        
*                                                                               
PKMCRD   DS    PL16                PACKED MEDIA SIDE CREDIT FIELDS              
PKMDEB   DS    PL16                PACKED MEDIA SIDE DEBIT FIELDS               
PKMBAL   DS    PL16                PACKED MEDIA SIDE BALANCE(DB-CR)             
PKCLIQ   EQU   (*-PKCLI)/L'PKFLDS                                               
*                                                                               
PKRUN    DS    0P                  RUN AMOUNTS                                  
PKCDTOT  DS    PL16                TOTAL FOR CREDITS                            
PKDBTOT  DS    PL16                TOTAL FOR DEBITS                             
PKTOTAL  DS    PL16                TOTAL(DB-CR)                                 
*                                                                               
PKMCDTOT DS    PL16                TOTAL FOR CREDITS - MEDIA SIDE               
PKMDBTOT DS    PL16                TOTAL FOR DEBITS - MEDIA SIDE                
PKMTOTAL DS    PL16                TOTAL(DB-CR) - MEDIA SIDE                    
PKRUNQ   EQU   (*-PKRUN)/L'PKFLDS                                               
*                                                                               
PKSZBAL  DS    PL16                PACKED BALANCE(DB-CR)                        
PKMDBAL  DS    PL16                PACKED BALANCE(DB-CR)                        
*                                                                               
PKSYMD   DS    0P                  SYS/MED AMOUNTS                              
PKSCRD   DS    PL16                PACKED CREDIT FIELDS - SYS/MEDIA             
PKSDEB   DS    PL16                PACKED DEBIT FIELDS - SYS/MEDIA              
PKSBAL   DS    PL16                PACKED BALANCE(DB-CR) - SYS/MEDIA            
*                                                                               
PKSMCRD  DS    PL16                PACKED MEDIA SIDE CREDIT FIELDS              
PKSMDEB  DS    PL16                PACKED MEDIA SIDE DEBIT FIELDS               
PKSMBAL  DS    PL16                PACKED MEDIA SIDE BALANCE(DB-CR)             
PKSYMDQ  EQU   (*-PKSYMD)/L'PKFLDS                                              
PKNUMQ   EQU   (*-PKFLDS)/L'PKFLDS                                              
*                                                                               
PKWORK   DS    PL16                                                             
*                                                                               
FLAG     DS    XL1                 FLAG FOR RUN TOTALS                          
FLGACT   EQU   X'80'               ACCOUNT TOTALS                               
FLGCLI   EQU   X'40'               CLIENT TOTALS                                
FLGCLPRT EQU   X'20'               CLIENT TOTALS ARE ZERO DON'T PRINT           
FLGSYSMD EQU   X'10'               SYSMED RUN DON'T PRINT                       
*                                                                               
LSTAC    DS    0CL15               LAST ACCOUNT/SYSTEM/MEDIA/CLIENT             
LSTACT   DS    CL12                LAST ACCOUNT                                 
LSTCLI   DS    CL3                 LAST CLIENT                                  
LSTNME   DS    CL25                                                             
*                                                                               
MSG1     DS    CL20                                                             
COMPLEN  DS    XL1                 COMPARE LENGTH                               
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
STDTE    DS    XL2                 REQUEST START DATE COMPRESSED                
ENDDTE   DS    XL2                 REQUEST END DATE COMPRESSED                  
OPDTE    DS    CL6                 OPENING BALANCE DATE                         
HEADNM   DS    CL36                HEADLINE FOR REPORT                          
*                                                                               
ACTWRK   DS    CL(ACTLNQ)                                                       
ELEMENT  DS    XL255                                                            
EOF      EQU   X'FF'                                                            
*                                                                               
ID       DS    CL16                ID OF POSTING FILE                           
COMMAND  DS    CL8                                                              
*                                                                               
         DS    0F                  POSTING AREA NEEDS TO BE FULL                
POSTREC  DS    0CL504              WORD ALIGNED                                 
POSTHEAD DS    F                                                                
POSTAREA DS    CL500                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN ACCOUNT TABLE                                    *         
***********************************************************************         
                                                                                
ACTD     DSECT                                                                  
ACTACCT  DS    CL12                ACCOUNT ACCOUNT CODE                         
ACTSRDSP EQU   *-ACTACCT           DISPLACEMENT TO SORT FIELD                   
ACTCLI   DS    CL3                                                              
ACTSRLNQ EQU   *-ACTCLI            LENGTH OF SORT FIELD                         
ACTRACT  DS    CL2                 REAL ACCOUNT CODE                            
ACTKLNQ  EQU   *-ACTD              LENGTH OF KEY                                
ACTSYSMD DS    CL2                 SYSTEM/MEDIA - ONLY ON MEDIA SIDE            
ACTNME   DS    CL36                ACCOUNT NAME                                 
ACTBKT   DS    0PL16               START OF BUCKETS                             
ACTCBKT  DS    PL16                SZ SIDE CREDIT BUCKET                        
ACTBKLN  EQU   *-ACTBKT            SZ SIDE BUCKET LENGTH                        
ACTDBKT  DS    PL16                DEBIT BUCKET                                 
ACTMDDB  DS    PL16                MEDIA DEBIT BUCKET                           
ACTMDCR  DS    PL16                MEDIA SIDE CREDIT BUCKET                     
ACTBKCT  EQU   (*-ACTBKT)/ACTBKLN  NUMBER OF BUCKETS                            
ACTLNQ   EQU   *-ACTD              LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER MEDIA TABLE                                          *         
***********************************************************************         
                                                                                
MEDD     DSECT                                                                  
MEDSYS   DS    CL2         ACTUAL SYSTEM/MEDIA AND SZ ACCOUNT CODE              
MEDACT   DS    CL2         NEW CODE TO COMBILE SIMILAR ACCOUNTS                 
MEDNME   DS    CL25        NEW ACCOUNT NAME TO REPLACE PREVIOUS                 
MEDLNQ   EQU   *-MEDSYS                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER OFFICE DEFAULT TABLE                                 *         
***********************************************************************         
                                                                                
OFFTABD  DSECT                                                                  
TABCOMP  DS    XL1         COMPANY CODE                                         
DFLTOFF  DS    CL2         DEFAULT OFFICE                                       
OFFTLEN  EQU   *-OFFTABD                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
                                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
* PRINT DESCT                                                         *         
***********************************************************************         
                                                                                
PLINED   DSECT                                                                  
PRTLNE   DS    0C                  PRINT LINE                                   
         DS    CL2                                                              
PSZACCT  DS    CL12                SZ ACCOUNT CODE                              
         DS    CL2                                                              
PSZANME  DS    CL25                SZ ACCOUNT NAME                              
         DS    CL2                                                              
PSZCCLI  DS    CL3                 SZ CONTRA ACCOUNT CLIENT                     
         DS    CL2                                                              
         ORG   PSZACCT                                                          
P2CLI    DS    CL3                 SZ CONTRA ACCOUNT CLIENT                     
         DS    CL1                                                              
P2ACCT   DS    CL12                SZ ACCOUNT CODE                              
         DS    CL3                                                              
P2ANME   DS    CL25                SZ ACCOUNT NAME                              
         DS    CL2                                                              
PSZCRD   DS    CL16                SZ SIDE CREDIT AMOUNT                        
         DS    CL2                                                              
PSZDBT   DS    CL16                SZ SIDE DEBIT AMOUNT                         
         DS    CL2                                                              
PSZBAL   DS    CL16                SZ SIDE BALANCE TOTAL                        
         DS    CL2                                                              
PSYSMD   DS    CL2                 ORIGINAL SYSTEM/MEDIA                        
         DS    CL2                                                              
PMDCRD   DS    CL16                MEDIA SIDE CREDIT AMOUNT                     
         DS    CL2                                                              
PMDDBT   DS    CL16                MEDIA SIDE DEBIT AMOUNT                      
         DS    CL2                                                              
PMDBAL   DS    CL16                MEDIA SIDE BALANCE TOTAL                     
         DS    CL2                                                              
PRLNQ    EQU   *-PLINED            LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* BUFFER AREA FOR PRINT LINES                                         *         
***********************************************************************         
                                                                                
PAREAD   DSECT                                                                  
         DS    CL(PRLNQ)           SAVED PLINED LINE AREA                       
PASZDBT  DS    PL8                 SZ SIDE DEBITS                               
PASZCRD  DS    PL8                 SZ SIDE CREDITS                              
PAMDDBT  DS    PL8                 SYS/MEDIA SIDE DEBITS                        
PAMDCRD  DS    PL8                 SYS/MEDIA SIDE CREDITS                       
PAREAQ   EQU   *-PAREAD                                                         
         EJECT                                                                  
***********************************************************************         
* BUFFER AREA FOR PRINT LINES                                         *         
***********************************************************************         
CLIMEDD  DSECT                                                                  
PSTMED   DS    CL2                 POSTING MEDIA                                
PSTCLI   DS    CL3                 POSTING CLIENT                               
PSTAMT   DS    PL16                DEBIT ADDED TO SZ                            
PSTLNQ   EQU   *-CLIMEDD                                                        
*                                                                               
***********************************************************************         
*        CLIENT MEDIA TABLE                                           *         
***********************************************************************         
CLIMED   CSECT                        POSTING BUFFER                            
         DS    0D                                                               
         DC    5000XL5'00'                                                      
***********************************************************************         
*        POSTING BUFFER                                               *         
***********************************************************************         
POSTBUFF CSECT                        POSTING BUFFER                            
         DS    0D                                                               
         DC    CL8'POSTBUFF'                                                    
         DC    4500X'00'                                                        
         EJECT                                                                  
***********************************************************************         
*              ++INCLUDES                                             *         
***********************************************************************         
                                                                                
* DDCNTRL                                                                       
       ++INCLUDE DDCNTRL                                                        
* DMWRKRK                                                                       
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* ACBIGPRINTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'120ACREPSZ02 08/16/00'                                      
         END                                                                    
