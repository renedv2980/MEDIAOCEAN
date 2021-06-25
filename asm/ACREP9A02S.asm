*          DATA SET ACREP9A02S AT LEVEL 078 AS OF 05/01/02                      
*PHASE AC9A02A,+0                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE ACCDIV                                                                 
*INCLUDE BUDACC                                                                 
*INCLUDE COVAIL                                                                 
*INCLUDE DLFLD                                                                  
         EJECT ,                                                                
*--------------------------------------------------------------------*          
* PROFILE OPTIONS                                                               
*                                                                               
* 1      REVERSE PRINTING ORDER OF 13/14                                        
* 2      USE LEDGER NAMES NOT TABLE NAMES FOR TOTAL LINES                       
* 3      SKIP DIRECT MARGIN LINE(PRE OVERHEAD PROFIT)                           
* 4      SKIP TOTAL OVERHEAD LINE                                               
* 5      SKIP TOTAL TIME+EXPENSE LINE                                           
* 6      CHICKEN TRACK TOTAL EXPENSE LINE                                       
* 7      CHICKEN TRACK TIME+EXPENSE LINE                                        
* 8      PRINT SPECIAL SUMMARY IN MONEY MODE ALSO                               
* 9      PRINT TOTAL LINES ONLY                                                 
* 10     CALCULATE BILLINGS FROM INCOME(CAPITALISE)                             
* 11     DO NOT PRINT DETAILS OF BILLING/INCOME                                 
* 13     SUPPRESS 13 DETAILS                                                    
* 14     SUPPRESS 14 DETAILS                                                    
* 15     SUPPRESS 15 DETAILS N,Y  (O TO ADD 15 AND 16 AS OVERHEAD)              
* 16     TAX RATE                                                               
*--------------------------------------------------------------------*          
         TITLE 'CLIENT PROFITABILITY REPORT'                                    
AC9A02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC9A02,R8,R9                                                 
         L     RC,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RC                                                       
*                                                                               
         LA    RA,SPACEND                                                       
*                                                                               
         USING AC9A02D,RA                                                       
*                                                                               
         ST    RB,APGM             ADDR OF AC9A02                               
         ST    RC,AWRKD            ADDR OF WORKD                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        INITIAL ROUTINES                                                       
*--------------------------------------------------------------------*          
         CLI   MODE,RUNFRST                                                     
         BNE   PR01                                                             
         RELOC (R3)                                                             
         ST    R3,RELO                                                          
         LA    RE,RELOTAB          RELOCATE A-TYPES                             
         LA    R1,ATYPES                                                        
*                                                                               
RELOOP   L     RF,0(,RE)                                                        
         A     RF,RELO                                                          
         ST    RF,0(,R1)                                                        
         LA    R1,4(,R1)                                                        
         LA    RE,4(,RE)                                                        
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
*&&UK                                                                           
         LA    RE,BILTAB                                                        
         LA    RF,600                                                           
         XCEF                                                                   
*&&                                                                             
         GOTO1 =V(COVAIL),DMCB,C'SETB',20000,300000,ADBUFC                      
         MVC   ADBUFC,12(R1)                                                    
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         BAS   RE,BUDGNM           FILL IN LINTAB NAMES                         
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         CLI   PROGPROF,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,INVERT           REVERSE PRINTING ORDER OF 13/14              
         CLI   PROGPROF+1,C'Y'                                                  
         BNE   XIT                                                              
         BAS   RE,REPLACE          USE LEDGER NAMES FOR TOTAL LINES             
*&&US                                                                           
         CLC   ALPHAID,=C'BS'       SPECIAL FOR BACKER, BATES                   
         BNE   XIT                                                              
         MVI   SELECTAB+2,52            CHANGE COL 18 TO 52                     
         MVI   SELECTAB+3,53            CHANGE COL 19 TO 53                     
         MVI   SELECTAB+6,0             CHANGE COL 27 TO NOTHING                
         MVI   SELECTAB+7,0             CHANGE COL 28 TO NOTHING                
*&&                                                                             
         B     XIT                                                              
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*                                                                               
*--------------------------------------------------------------------*          
PR01     DS    0H                                                               
         CLI   MODE,REQFRST                                                     
         BNE   PR20                                                             
         MVI   METHOD,C'1'         *** SET METHOD TO DEFAULT ****               
         CLI   QMTHD,X'40'                                                      
         BNH   *+10                                                             
         MVC   METHOD,QMTHD        METHOD FROM REQUEST                          
         L     R5,ADGRPTOT                                                      
*                                                                               
         USING BIND,R5                                                          
*                                                                               
         XC    BININ,BININ         CLEAR NUMBER IN GROUP TOTAL TABLE            
         MVC   AGYTOTS(48),=8PL8'0'                                             
         MVI   REQACTIV,X'FF'      FOR REQUEST ACTIVITY                         
         MVC   QSTART+4(2),=C'01'                                               
         MVC   QEND+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,QEND),(1,DATE)                                    
         MVC   HDAT,SPACES                                                      
         MVC   HDAT(17),=C'FOR THE MONTH OF '                                   
         LA    R3,HDAT+17                                                       
         CLI   QOPT1,C'9'                                                       
         BNE   *+14                                                             
         MVC   HDAT(17),=C'AS OF            '                                   
         LA    R3,HDAT+6                                                        
         GOTO1 (RF),(R1),,(9,0(R3)) MMM/YY                                      
         GOTO1 (RF),(R1),(0,QSTART),(1,SDATE)                                   
         MVI   MYBOXSW,1                                                        
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   GRPMODE,C'N'                                                     
         ZAP   BNUMBER,=P'0'                                                    
         ZAP   CNUMBER,=P'0'                                                    
         ZAP   DNUMBER,=P'0'                                                    
*&&UK*&& ZAP   LOWNO,=P'1'         DEFAULT VALUES FOR LOW LEVEL                 
*&&US*&& ZAP   LOWNO,=P'0'         NUMBER TEST                                  
         CLI   PROGPROF+11,0       ELSE USE PROFILE VALUE                       
         BE    PR01A                                                            
         ZIC   RE,PROGPROF+11                                                   
         CVD   RE,DUB                                                           
         ZAP   LOWNO,DUB                                                        
*                                                                               
PR01A    DS    0H                                                               
         CLI   QOPT1,C' '          ESTABLISH A DEFAULT                          
         BNE   *+8                                                              
         MVI   QOPT1,C'1'                                                       
         LA    R7,MENUS                                                         
*                                                                               
PR02     CLI   0(R7),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   QOPT1,0(R7)                                                      
         BE    *+12                                                             
         LA    R7,5(,R7)                                                        
         B     PR02                                                             
*                                                                               
         MVC   MENUSV,1(R7)        SAVE MENU FOR THIS REQUEST                   
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT3,C'Y'          SPROG FOR INCOME REPORT                      
         BNE   *+8                                                              
         MVI   RCSUBPRG,1                                                       
*                                  SET UP DOWN-LOAD CONTROL BLOC                
         CLI   QOPT7,C' '                                                       
         BE    PR03                                                             
         MVI   RCSUBPRG,3                                                       
         MVI   DLSTAT,0                                                         
         XC    HEADHOOK,HEADHOOK                                                
         LA    R5,DLBUFF                                                        
*                                                                               
         USING DLCBD,R5                                                         
*                                                                               
         LA    RE,DLPLINE                                                       
         ST    RE,DLCBAPL                                                       
         LA    RE,DLPRINT                                                       
         ST    RE,DLCBAPR                                                       
         MVI   DLCBACT,DLCBSOR                                                  
         GOTO1 DOWNLOAD,(R5)                                                    
         MVC   DLPLINE,SPACES                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
PR03     L     R1,ADWORK                                                        
         LR    R2,R1                                                            
         POINT (R2),TTRFIRST                                                    
*                                                                               
         MVC   BUDNAME,SPACES      BUDGET SHORT NAME                            
         L     R7,ABUDC                                                         
*                                                                               
         USING BUDACCD,R7                                                       
*                                                                               
         MVC   BUADMGR,DATAMGR                                                  
         MVC   BUDTCON,DATCON                                                   
         MVI   BUTYPE,READING                                                   
         XC    BUBUDNO,BUBUDNO                                                  
         XC    BULSTKEY,BULSTKEY                                                
         CLI   QSRTAREA,X'40'                                                   
         BE    PR04                NO BUDGET INPUT                              
         CLI   QSRTAREA,X'00'                                                   
         BE    PR04                NO BUDGET INPUT                              
         MVC   BUBUDNO+1(1),QSRTAREA                                            
         MVI   BUCMND,TYPEGET                                                   
         MVC   BUSERKEY(1),QCOMPANY                                             
         GOTO1 BUDACC,DMCB,ABUDC                                                
         CLI   BUMODE,PROCTYPE     GOOD READ                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,BUTYPREC         GET BUDGET SHORT NAME                        
*                                                                               
         USING ACBTKEY,RF                                                       
*                                                                               
         MVC   BUDNAME,ACBTKCOD                                                 
         XC    BUBUDNO,BUBUDNO                                                  
*                                                                               
         DROP  RF                                                               
*                                                                               
PR04     DS    0H                                                               
         XC    BUSTDATE,BUSTDATE                                                
         XC    BUNDDATE,BUNDDATE                                                
         XC    BULSTKEY,BULSTKEY                                                
         MVC   BUSERKEY,SPACES                                                  
*                                                                               
         DROP  R7                                                               
*                                                                               
*                                  INITIALIZE DATE COLUMNS                      
         GOTO1 =A(INITDATE),DMCB,(RC),RR=RELO                                   
         B     XIT                                                              
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        LEDGFRST - GET NUMBER OF LEVELS                                        
*--------------------------------------------------------------------*          
*                                                                               
         USING ACHEIRD,R4                                                       
*                                                                               
PR20     CLI   MODE,LEDGFRST                                                    
         BNE   PR22                                                             
         L     R4,ADLDGHIR                                                      
         MVI   HILEV,1                                                          
         CLI   ACHRLEVB,0                                                       
         BE    XIT                                                              
         MVI   HILEV,2                                                          
         CLI   ACHRLEVC,0                                                       
         BE    XIT                                                              
         MVI   HILEV,3                                                          
         CLI   ACHRLEVD,0                                                       
         BE    XIT                                                              
         MVI   HILEV,4                                                          
         B     XIT                                                              
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*                                                                               
*--------------------------------------------------------------------*          
PR22     CLI   MODE,LEVAFRST                                                    
         BNE   PR24                                                             
         L     RF,ADHEIRA                                                       
         B     PR28                                                             
*                                                                               
PR24     CLI   MODE,LEVBFRST                                                    
         BNE   PR26                                                             
         L     RF,ADHEIRB                                                       
         B     PR28                                                             
*                                                                               
PR26     CLI   MODE,LEVCFRST                                                    
         BNE   PR30                                                             
         L     RF,ADHEIRC                                                       
         B     PR28                                                             
*                                                                               
PR28     MVC   BUDKEY,0(RF)                                                     
         GOTO1 =A(BUDGET),DMCB,(RC)                                             
         B     XIT                                                              
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        ROUTINE FOR ACCFRST                                                    
*--------------------------------------------------------------------*          
PR30     CLI   MODE,PROCACC                                                     
         BNE   PR40                                                             
         MVI   MYBYTE,X'FF'                                                     
         LA    R6,PLUS                                                          
         LA    R7,MINUS                                                         
         LA    RF,MAXCOLS                                                       
*                                                                               
PR32     ZAP   0(6,R6),=P'0'                                                    
         ZAP   0(6,R7),=P'0'                                                    
         LA    R6,6(,R6)                                                        
         LA    R7,6(,R7)                                                        
         BCT   RF,PR32                                                          
         MVI   FORCEHED,C'Y'       NEW PAGE PER CLIENT                          
         L     R4,ADACCSTA         TEST FOR START OF SUB-GROUP                  
*                                                                               
         USING ACSTATD,R4                                                       
*                                                                               
         CLI   ACSTCOST,C'('                                                    
         BNE   PR34                                                             
         MVI   GRPMODE,C'Y'                                                     
*                                                                               
PR34     L     RF,ADACC                                                         
         MVC   BUDKEY,0(RF)                                                     
         GOTO1 =A(BUDGET),DMCB,(RC) GET ACCOUNT LEVEL BUDGETS                   
         ZAP   TOTREV,=P'0'                                                     
         ZAP   TOTREV2,=P'0'                                                    
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R4                                                               
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        ROUTINE FOR PROCTRNS                                                   
*--------------------------------------------------------------------*          
*                                                                               
         USING TRSUBHD,R4                                                       
*                                                                               
PR40     CLI   MODE,PROCHIST                                                    
         BNE   PR80                                                             
         L     R4,ADSUBAC                                                       
         L     R5,ADTRANS                                                       
         CLI   0(R5),X'45'                                                      
         BNE   XIT                                                              
         L     RF,ADSUBAC                                                       
         SH    RF,DATADISP                                                      
         CLI   ACKEYREF+1-ACKEYD(RF),C' '                                       
         BNH   PR40D                      NEW COST BUCKET                       
         CLC   METHOD,ACKEYREF-ACKEYD(RF) MATCH REQUESTED METHOD                
         BNE   XIT                                                              
         CLI   PROGPROF+14,C'O'      COMBINE 15 AND 16 AS OVERHEAD              
         BNE   PR40A                                                            
         CLC   TRSBACNT+1(2),=C'15'  COMPRESS 15 AND 16 FOR NEW COST            
         BNE   PR40A                                                            
         MVC   TRSBACNT+1(3),=C'16A'                                            
         B     PR40N                                                            
*                                                                               
PR40A    CLC   TRSBACNT+1(2),=C'16'  COMPRESS 16 FOR NEW COST                   
         BNE   PR40N                                                            
         MVI   TRSBACNT+3,C'A'                                                  
         B     PR40N                                                            
*                                                                               
PR40D    CLI   BUCKTYPE,C' '                                                    
         BNE   XIT                                                              
*                                                                               
PR40N    CLI   TRSBACNT+1,C'1'     MUST BE UNIT 1                               
         BNE   XIT                                                              
         CLI   TRSBACNT+2,C'P'     TRANSLATE LEDGER P / EXPENSES                
         BNE   PR42                TO LEDGER 3 / EXPENSES                       
         MVC   WORK(1),TRSBACNT+5  DEAL WITH 2/3 LONG DEPTS                     
         CLI   TRSBACNT+6,C' '                                                  
         BE    PR41                                                             
         MVC   WORK(1),TRSBACNT+6                                               
*                                                                               
PR41     MVC   TRSBACNT+3(12),SPACES                                            
         MVC   TRSBACNT+3(1),WORK                                               
         MVI   TRSBACNT+2,C'3'                                                  
*                                                                               
PR42     MVI   BYTE,X'FF'                                                       
         CLI   QOPT3,C'Y'          INCOME REPORT OPTION                         
         BNE   PR43                                                             
         CLI   TRSBACNT+2,C'2'                                                  
         BH    XIT                                                              
         B     PR43A                                                            
*                                                                               
PR43     DS    0H                                                               
         CLI   PROGPROF+9,C'Y'     PROFILE OPTION TO CALCULATE BILLINGS         
         BNE   PR43A               FROM INCOME (CAPITALISE)                     
         CLI   TRSBACNT+2,C'1'                                                  
         BE    XIT                                                              
*                                                                               
         USING TRHISTD,R5                                                       
*                                                                               
PR43A    ZAP   DUB,TRHSCR          FOR BILLING/COMMISSIONS WE WANT              
         CLI   TRSBACNT+2,C'2'     DEBITS / ALL OTHERS-CREDITS                  
         BH    PR44                                                             
         ZAP   DUB,TRHSDR                                                       
*                                                                               
PR44     ZAP   DOUBLE,=P'0'                                                     
         CP    DUB,=P'0'           ZERO? - DON'T BOTHER                         
         BE    XIT                                                              
         SP    DOUBLE,DUB          MINUS AMOUNT                                 
         L     R6,ACOLTAB                                                       
*                                                                               
         USING COLTABD,R6                                                       
*                                                                               
         LA    R3,PLUS                                                          
         LA    R7,MINUS                                                         
*                                                                               
PR46     CLC   COLSTRT,TRHSYEAR                                                 
         BH    PR48                                                             
         CLC   COLEND,TRHSYEAR                                                  
         BL    PR48                                                             
         CLI   COLTYPE,C'B'        IGNORE BUDGET COLUMNS                        
         BE    PR48                                                             
*                                                                               
         ZAP   0(6,R3),DUB         ADD TO CORRECT PLACE IN                      
         ZAP   0(6,R7),DOUBLE      A PLUS & MINUS LINE                          
         MVI   BYTE,0              GOT ONE                                      
         MVI   MYBYTE,0                                                         
*                                                                               
PR48     LA    R3,6(,R3)                                                        
         LA    R7,6(,R7)                                                        
         LA    R6,L'COLTAB(,R6)                                                 
         CLI   0(R6),X'FF'         END OF COLTAB?                               
         BNE   PR46                                                             
*                                                                               
         CLI   BYTE,0              DIDN'T GET A MATCH ON                        
         BNE   XIT                 THIS HISTORY ELEMENT                         
*                                                                               
         DROP  R6                                                               
*                                                                               
PR50     MVC   WORK(4),MENUSV                                                   
*                                                                               
PR52     SR    R6,R6                                                            
         IC    R6,WORK             REPORT NUMBER                                
         BCTR  R6,R0                                                            
         MH    R6,=H'8'                                                         
         LA    R7,SELECTAB(R6)                                                  
         LA    RF,8                                                             
         LA    R5,PLUS8            BUILD 2 8-BYTE LINES                         
         LA    R6,MINUS8           FROM 2 44-BYTE LINES                         
         MVC   PLUS8,=8PL8'0'                                                   
         MVC   MINUS8,=8PL8'0'                                                  
*                                                                               
PR54     LA    R2,PLUS                                                          
         LA    R3,MINUS                                                         
         SR    R1,R1                                                            
         IC    R1,0(,R7)                                                        
         LTR   R1,R1               CAN FINISH AT LESS THAN 8                    
         BZ    PR55                                                             
         BCTR  R1,R0                                                            
         MH    R1,=H'6'                                                         
         AR    R2,R1                                                            
         AR    R3,R1                                                            
         ZAP   0(8,R5),0(6,R2)                                                  
         ZAP   0(8,R6),0(6,R3)                                                  
         LA    R7,1(,R7)                                                        
         LA    R5,8(,R5)                                                        
         LA    R6,8(,R6)                                                        
         BCT   RF,PR54                                                          
*                                                                               
         USING LINTABD,R6                                                       
*                                                                               
PR55     L     R6,ADLINTAB                                                      
*                                                                               
PR56     CLI   0(R6),X'FF'                                                      
         BE    XIT                                                              
         CLC   LINKEY,TRSBACNT+2                                                
         BE    *+12                                                             
         LA    R6,L'LINTAB(,R6)                                                 
         B     PR56                                                             
*                                                                               
         MVC   HALF,=H'6'          USE HALF AS COUNTER                          
         MVC   INSTRUCS,LINPARM                                                 
*                                                                               
PR58     MVC   BUFKEY+1(3),LINID   BUILD A BUFFALO RECORD                       
         MVC   BUFKEY(1),WORK      REPORT NO.                                   
         LA    RF,PLUS8                                                         
         TM    INSTRUCS,X'40'                                                   
         BO    *+8                                                              
         LA    RF,MINUS8                                                        
         MVC   BUFACCS,0(RF)                                                    
         CLI   BUFKEY,11                                                        
         BNE   *+8                                                              
         BAS   RE,ROUND            TYPE 11 REPORT F                             
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFLINE                              
         CLI   GRPMODE,C'Y'                     IF PART OF GROUP                
         BNE   PR581                                                            
         GOTO1 BINADD,DMCB,BUFLINE,ADGRPTOT,(RC) ADD TO GROUP TABLE             
*                                                                               
PR581    CLI   PROGPROF+9,C'Y'     CALCULATE BILLING FIGURES                    
         BNE   PR59                                                             
         CLI   QOPT3,C'Y'                                                       
         BE    PR59                                                             
         CLI   BUFKEY+2,C'2'                                                    
         BNE   PR59                                                             
         MVC   SVBUFACS,BUFACCS    SAVE VALUES FOR THIS LINE                    
         MVC   SVBUFKY,BUFKEY+1                                                 
         ZIC   RF,BUFKEY+1         C'E2' TO C'A1'                               
         SH    RF,=H'4'                                                         
         STC   RF,BUFKEY+1                                                      
         MVI   BUFKEY+2,C'1'                                                    
         LA    RF,8                                                             
         LA    RE,BUFACCS                                                       
*                                                                               
PR58A    ZAP   DIV,0(8,RE)         INCOME OF 15 BECOMES BILLING OF 100          
         MP    DIV,=P'100'                                                      
         DP    DIV,=P'15'                                                       
         ZAP   0(8,RE),DIV(12)                                                  
         LA    RE,8(,RE)                                                        
         BCT   RF,PR58A                                                         
         CLI   BUFKEY,11                                                        
         BNE   *+8                                                              
         BAS   RE,ROUND            TYPE 11 REPORT F                             
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFLINE                              
         CLI   GRPMODE,C'Y'                     IF PART OF GROUP                
         BNE   PR58B                                                            
         GOTO1 BINADD,DMCB,BUFLINE,ADGRPTOT,(RC) ADD TO GROUP TABLE             
*                                                                               
PR58B    MVC   BUFKEY+1(2),SVBUFKY                                              
         MVC   BUFACCS,SVBUFACS                                                 
*                                                                               
PR59     DS    0H                                                               
         LH    RF,HALF                                                          
         BCTR  RF,R0                                                            
         LTR   RF,RF                                                            
         BZ    PR62                                                             
         STH   RF,HALF                                                          
         MVC   INSTRUCS(5),INSTRUCS+1                                           
         MVI   INSTRUCS+5,C' '                                                  
         CLI   INSTRUCS,C' '       LAST OF UP TO SIX LINE INSTRUCTIONS          
         BE    PR62                                                             
         MVC   BYTE,INSTRUCS                                                    
         OI    BYTE,X'40'                                                       
*                                                                               
PR60     CLI   0(R6),X'FF'         TRY TO FIND NEXT LINE                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE,LINID                                                       
         BE    PR58                                                             
PR61     LA    R6,L'LINTAB(,R6)                                                 
         B     PR60                                                             
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
*                                                                               
PR62     MVC   WORK(3),WORK+1      ANY MORE IN MENU/REPORT LIST                 
         MVI   WORK+3,C' '                                                      
         CLI   WORK,C' '                                                        
         BNE   PR52                                                             
         LA    R6,PLUS                                                          
         LA    R7,MINUS                                                         
         LA    RF,MAXCOLS                                                       
*                                                                               
PR64     ZAP   0(6,R6),=P'0'                                                    
         ZAP   0(6,R7),=P'0'                                                    
         LA    R6,6(,R6)                                                        
         LA    R7,6(,R7)                                                        
         BCT   RF,PR64                                                          
         B     XIT                                                              
*                                                                               
         DROP  R4                                                               
         DROP  R5                                                               
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        ROUTINE FOR SBACFRST                                                   
*--------------------------------------------------------------------*          
PR80     CLI   MODE,SBACFRST                                                    
         BNE   PR90                                                             
*&&US*&& B     XIT                                                              
         L     R4,ADSUBAC          MOVE CONTRA-A/C NAME                         
*                                                                               
         USING TRSUBHD,R4          INTO TABLE                                   
*                                                                               
         MVC   WORK(2),TRSBACNT+2                                               
         CLI   TRSBACNT+2,C'P'                                                  
         BNE   PR81                                                             
         MVI   WORK,C'3'                                                        
         MVC   WORK+1(1),TRSBACNT+5                                             
         CLI   TRSBACNT+6,C' '                                                  
         BE    PR81                                                             
         MVC   WORK+1(1),TRSBACNT+6   2/3 LONG DEPTS                            
*                                                                               
PR81     DS    0H                                                               
         L     R6,ADLINTAB                                                      
*                                                                               
         USING LINTABD,R6                                                       
*                                                                               
         CLI   TRSBACNT+3,C' '     IGNORE FUNNIES                               
         BE    XIT                                                              
*                                                                               
PR82     CLI   LINID,X'FF'                                                      
         BE    XIT                                                              
         CLC   LINKEY,WORK                                                      
         BE    PR84                                                             
         LA    R6,L'LINTAB(R6)                                                  
         B     PR82                                                             
*                                                                               
PR84     SR    R3,R3                                                            
         MVC   LINHEAD,SPACES                                                   
         IC    R3,TRSBLEN                                                       
         SH    R3,=H'17'                                                        
         GOTO1 CHOPPER,DMCB,((R3),TRSBNAME),(29,LINHEAD),1                      
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        ACCLAST AND SOME OTHER LAST TIMES                                      
*--------------------------------------------------------------------*          
PR90     CLI   MODE,ACCLAST                                                     
         BNE   PR94                                                             
         CLI   MYBYTE,X'FF'                                                     
         BE    PR92                                                             
         MVI   REQACTIV,0                                                       
         GOTO1 BUFFALO,PLIST,=C'ADD',ADBUFC,1,2,3,4,(X'80',5)                   
         MVI   THISLEV,1                                                        
         BAS   RE,LEVEL                                                         
         CLI   HILEV,4                                                          
         BNE   *+10                                                             
         AP    DNUMBER,=P'1'                                                    
         CLI   HILEV,3                                                          
         BNE   *+10                                                             
         AP    CNUMBER,=P'1'                                                    
         CLI   HILEV,2                                                          
         BNE   PR92                                                             
         AP    BNUMBER,=P'1'                                                    
*                                                                               
PR92     L     R4,ADACCSTA         TEST END OF GROUP                            
*                                                                               
         USING ACSTATD,R4                                                       
*                                                                               
         CLI   GRPMODE,C'Y'                                                     
         BNE   XIT                                                              
         CLI   ACSTCOST,C')'                                                    
         BNE   XIT                                                              
         MVI   THISLEV,X'FF'                                                    
         BAS   RE,LEVEL                                                         
         L     R5,ADGRPTOT                                                      
*                                                                               
         USING BIND,R5                                                          
*                                                                               
         XC    BININ,BININ         CLEAR NUMBER IN GROUP TOTAL TABLE            
         MVI   GRPMODE,C'N'                                                     
         B     XIT                                                              
*                                                                               
PR94     CLI   MODE,LEVALAST                                                    
         BNE   PR96                                                             
         MVC   THISLEV,HILEV                                                    
         BAS   RE,LEVEL                                                         
         ZAP   BNUMBER,=P'0'                                                    
         B     XIT                                                              
*                                                                               
PR96     CLI   MODE,LEVBLAST                                                    
         BNE   PR98                                                             
         ZIC   R1,HILEV                                                         
         BCTR  R1,0                                                             
         STC   R1,THISLEV                                                       
         BAS   RE,LEVEL                                                         
         AP    BNUMBER,=P'1'                                                    
         ZAP   CNUMBER,=P'0'                                                    
         B     XIT                                                              
*                                                                               
PR98     CLI   MODE,LEVCLAST                                                    
         BNE   PR100                                                            
         MVI   THISLEV,2                                                        
         BAS   RE,LEVEL                                                         
         AP    CNUMBER,=P'1'                                                    
         ZAP   DNUMBER,=P'0'                                                    
         B     XIT                                                              
*                                                                               
         DROP  R4,R5               KEEP IT CLEAN                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        GENERAL ROUTINE FOR END-OF-LEVELS                                      
*--------------------------------------------------------------------*          
LEVEL    NTR1                                                                   
         SR    R2,R2                                                            
         IC    R2,THISLEV                                                       
         MVC   FIVE(4),MENUSV                                                   
         MVI   FIVE+4,C' '                                                      
         CLI   QOPT2,C'S'                                                       
         BE    TEA40                                                            
         CLI   THISLEV,X'FF'       PRINT GROUP TOTALS                           
         BNE   TEA2                                                             
         L     R5,ADGRPTOT                                                      
*                                                                               
         USING BIND,R5                                                          
*                                                                               
         MVC   NUMSAVE,BININ       SAVE NO OF ITEMS                             
         B     TEA20                                                            
*                                                                               
TEA2     CLI   MODE,LEVALAST                                                    
         BNE   TEA10                                                            
         CLI   QOPT2,C' '                                                       
         BNE   TEA20                                                            
         CP    BNUMBER,LOWNO                                                    
         BH    TEA20                                                            
         B     TEA40                                                            
*                                                                               
TEA10    CLI   MODE,LEVBLAST                                                    
         BNE   TEA12                                                            
         CLI   QOPT2,C'1'                                                       
         BE    TEA40                                                            
         CP    CNUMBER,=P'0'                                                    
         BH    TEA20                                                            
         B     TEA40                                                            
*                                                                               
TEA12    CLI   MODE,LEVCLAST                                                    
         BNE   TEA14                                                            
         CLI   QOPT2,C'1'                                                       
         BE    TEA40                                                            
         CLI   QOPT2,C'2'                                                       
         BE    TEA40                                                            
         CP    DNUMBER,=P'0'                                                    
         BH    TEA20                                                            
         B     TEA40                                                            
*                                                                               
TEA14    CLI   QOPT2,C' '          PROCACC                                      
         BE    TEA20                                                            
         PACK  DUB,QOPT2(1)        IF HILEV IS GREATER THAN QOPT2               
         CVB   RF,DUB              IGNORE                                       
         ZIC   RE,HILEV                                                         
         CR    RE,RF                                                            
         BH    TEA40                                                            
*                                                                               
TEA20    BAS   RE,FORMAT                                                        
         MVC   FIVE(4),FIVE+1                                                   
         CLI   FIVE,C' '                                                        
         BNE   TEA20                                                            
*                                                                               
TEA40    DS    0H                                                               
         CLI   THISLEV,X'FF'                                                    
         BE    XIT                 GROUP TOTALS DON'T CLEAR ANYTHING            
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(0,ADBUFC),(X'80',(R2))                   
         B     XIT                                                              
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        ROUTINE FOR REQLAST                                                    
*--------------------------------------------------------------------*          
PR100    CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         CLI   RCSUBPRG,3                                                       
         BNE   PR101                                                            
         MVI   DLSTAT,4                                                         
         GOTO1 ADLOAD,(RC)                                                      
         B     XIT                                                              
*                                                                               
PR101    MVC   SAVEH1,SPACES                                                    
         MVC   SAVEH1(12),=CL12'REQUEST'                                        
         MVC   SAVEH1+12(6),=C'TOTALS'                                          
         CLI   REQACTIV,X'FF'      NO ACTIVITY ON REQUEST                       
         BE    PR104                                                            
         MVC   THISLEV,HILEV                                                    
         LA    R2,5                REQUEST TOTAL IS IN FIFTH ROW                
         MVC   FIVE(4),MENUSV                                                   
         MVI   FIVE+4,C' '                                                      
*                                                                               
PR102    BAS   RE,FORMAT                                                        
         MVC   FIVE(4),FIVE+1                                                   
         CLI   FIVE,C' '                                                        
         BE    PR104                                                            
         MVI   FORCEHED,C'Y'                                                    
         B     PR102                                                            
*                                                                               
PR104    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFC                                    
         CLI   QOPT2,C'S'                                                       
         BE    PR106                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         L     R1,ADWORK                                                        
         MVC   SUMREC(2),=C'/*'                                                 
         LA    R5,SUMREC                                                        
         LR    R2,R1                                                            
         WRITE DECB0002,SF,(R2),(R5)                                            
         CHECK DECB0002                CHECK                                    
         MVI   COMPBYTE,0          PERCENT MODE                                 
         BAS   RE,COMPAREM                                                      
         CLI   PROGPROF+7,C'Y'                                                  
         BNE   PR106                                                            
         MVI   COMPBYTE,1          MONEY MODE                                   
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,COMPAREM                                                      
*                                                                               
PR106    DS    0H                                                               
XIT      XMOD1 1                                                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        PRINT THE TABLES                                                       
*--------------------------------------------------------------------*          
FORMAT   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   INCOME,=8PL8'0'                                                  
         MVC   PROFIT,=8PL8'0'                                                  
         MVC   SUMACCS(48),=8PL8'0'                                             
         MVC   BUFKEY,SPACES                                                    
         MVC   BUFKEY(1),FIVE                                                   
         SR    R1,R1                                                            
         IC    R1,FIVE                                                          
         BCTR  R1,R0                                                            
         MH    R1,=H'8'                                                         
         LA    R6,SELECTAB(R1)                                                  
         ST    R6,MYWORD           SAVE FOR DATA-PRINTING                       
         L     R7,ACOLTAB                                                       
*                                                                               
         USING COLTABD,R7                                                       
*                                                                               
         ST    R2,WORD                                                          
         CLI   THISLEV,X'FF'                                                    
         BNE   F0G                 NOT GROUP TOTALS                             
         L     R5,ADGRPTOT                                                      
*                                                                               
         USING BIND,R5                                                          
*                                                                               
         MVC   BININ,NUMSAVE      RESTORE NUMBER IN TABLE(MULTIPLES)            
         OC    BININ,BININ                                                      
         BZ    XIT                 NOTHING IN TABLE                             
         L     RF,BININ            NUMBER IN TABLE                              
         BCTR  RF,0                LESS ONE                                     
         ST    RF,BININ            NUMER REMAINING                              
         LA    R5,BINTABLE                                                      
         ST    R5,LSTBINRC         A(LAST BIN RECORD)                           
*                                                                               
F0A      MVC   BUFLINE,0(R5)       RECORD TO BUFF AREA                          
         CLC   FIVE(1),BUFKEY                                                   
         BE    F0J                 SAME REPORT - CARRY ON                       
         L     R5,LSTBINRC         BUT COULD BE DIFFERENT FOR MULTIPLES         
         LA    R5,L'BUFLINE(,R5)   SO CARRY ON DOWN THE TABLE                   
         ST    R5,LSTBINRC                                                      
         L     R5,ADGRPTOT                                                      
         L     RF,BININ            NUMBER IN TABLE                              
         BCTR  RF,0                LESS ONE                                     
         ST    RF,BININ            NUMER REMAINING                              
         OC    BININ,BININ                                                      
         BZ    XIT                 NOTHING LEFT IN TABLE                        
         L     R5,LSTBINRC                                                      
         B     F0A                 SEE IF THERE YET                             
*                                                                               
F0G      GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFLINE,(R2)                        
         TM    DMCB+8,X'80'        EOF                                          
         BO    XIT                                                              
         CLC   FIVE(1),BUFKEY                                                   
         BNE   XIT                                                              
*                                                                               
F0J      CLI   QOPT3,C'Y'          INCOME REPORT OPTION                         
         BNE   F1                                                               
         CLI   BUFKEY+1,C'G'                                                    
         BH    FENDEND                                                          
*                                                                               
F1       DS    0H                                                               
         LA    RF,8                                                             
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   SAVEH5,SPACES                                                    
         MVC   SAVEH6,SPACES                                                    
         LA    R1,SAVEH5+31                                                     
         LA    R2,SAVEH6+31                                                     
         CLI   RCSUBPRG,3                                                       
         BNE   F2                                                               
         GOTO1 ADLOAD,(RC)                                                      
         B     F3                                                               
*                                                                               
F2       SR    R3,R3                                                            
         IC    R3,0(,R6)                                                        
         LTR   R3,R3                                                            
         BZ    F3                                                               
         BCTR  R3,R0                                                            
         LA    R5,L'COLTAB                                                      
         STH   R5,HALF                                                          
         MH    R3,HALF                                                          
         L     R7,ACOLTAB                                                       
         AR    R7,R3                                                            
         MVC   0(10,R1),COLHEAD1                                                
         MVC   0(10,R2),COLHEAD2                                                
         LA    R1,10(,R1)                                                       
         LA    R2,10(,R2)                                                       
         LA    R7,COLTAB                                                        
         LA    R6,1(,R6)                                                        
         BCT   RF,F2                                                            
*                                                                               
F3       GOTO1 =A(MYREPORT),DMCB,(RC),RR=RELO                                   
         L     R6,ADLINTAB                                                      
*                                                                               
         USING LINTABD,R6                                                       
*                                                                               
F4       CLC   FIVE(1),BUFKEY      SAME REPORT                                  
         BNE   FEND                                                             
         IF    BUFKEY+1,=,C'C',OR,C'G',OR,C'K',OR,C'M',OR,C'S',OR,     X        
               C'O',OR,C'Q',OR,C'Y',F4A                                         
         B     PR4C                                                             
*                                                                               
F4A      DS    0H                  ONLY WANT CERTAIN TOTAL LINES                
         LA    RE,SUMACCS          FOR NEW SUMMARY                              
         CLI   MODE,REQLAST                                                     
         BNE   *+8                                                              
         LA    RE,AGYTOTS                                                       
         CLI   BUFKEY+1,C'S'                                                    
         BL    F4C                                                              
         LA    RF,5                                                             
         BE    F4E                                                              
         LA    RF,6                                                             
         B     F4E                                                              
*                                                                               
F4C      DS    0H                                                               
         PACK  DUB,BUFKEY+2(1)                                                  
         CVB   RF,DUB                                                           
*                                                                               
F4E      BCTR  RF,0                                                             
         MH    RF,=H'8'                                                         
         AR    RE,RF               POINT TO CORRECT SUMMARY COUNTER             
         LA    RF,BUFACCS          POINT TO CORRECT BUFFALO COUNTER             
         CLI   BUFKEY,4              MONTH DATA ON SUMMARY                      
         BE    PR4A                                                             
         LA    RF,8(,RF)                                                        
         CLI   BUFKEY,2                                                         
         BE    PR4A                  YTD DATA ON SUMMARY                        
         CLI   BUFKEY,12                                                        
         BE    PR4A                                                             
         LA    RF,8(,RF)                                                        
         CLI   BUFKEY,3                                                         
         BE    PR4A                                                             
         LA    RF,8(,RF)                                                        
         CLI   BUFKEY,8                                                         
         BE    PR4A                                                             
         LA    RF,8(,RF)                                                        
         CLI   BUFKEY,1                                                         
         BE    PR4A                                                             
         LA    RF,16(,RF)                                                       
*                                                                               
PR4A     DS    0H                                                               
         ZAP   0(8,RE),0(8,RF)                                                  
*                                                                               
PR4C     DS    0H                                                               
         CLI   PROGPROF+8,C'Y'     PROFILE OPTION TO SKIP ALL BUT               
         BNE   F4G                 TOTAL LINES                                  
         CLI   BUFKEY+3,C' '                                                    
         BE    F4G                                                              
         CLI   BUFKEY+1,C'S'                                                    
         BE    F4G                                                              
         CLI   BUFKEY+1,C'Y'                                                    
         BNE   F40                                                              
*                                                                               
F4G      DS    0H                                                               
         CLI   PROGPROF+12,C'Y'    OPTION TO SKIP C/A 13 DETAILS                
         BNE   F4GN                                                             
         CLI   PROGPROF,C'Y'                                                    
         BE    F4GI                                                             
         CLI   BUFKEY+1,C'I'       IF 13 AND 14 NOT INVERTED                    
         BE    F40                                                              
         B     F4GN                                                             
*                                                                               
F4GI     CLI   BUFKEY+1,C'P'       IF 13 AND 14 INVERTED                        
         BE    F40                                                              
*                                                                               
F4GN     CLI   PROGPROF+13,C'Y'    OPTION TO SKIP C/A 14 DETAILS                
         BNE   *+12                                                             
         CLI   BUFKEY+1,C'L'                                                    
         BE    F40                                                              
         CLI   PROGPROF+14,C'Y'    OPTION TO SKIP C/A 15 DETAILS                
         BNE   *+12                                                             
         CLI   BUFKEY+1,C'U'                                                    
         BE    F40                                                              
*&&US                                                                           
*                                                                               
*                                  FOR US - DROP SOME TOTAL LINES               
*                                                                               
         CLI   BUFKEY+1,C'R'                                                    
         BNE   *+12                                                             
         CLI   PROGPROF+4,C'Y'     PROF OPTION FOR TOTAL TIME/EXPENSES          
         BNE   F40                                                              
         CLI   BUFKEY+1,C'S'                                                    
         BNE   *+12                                                             
         CLI   PROGPROF+2,C'Y'     PROFILE OPT FOR  DIRECT MARGIN LINE          
         BNE   F40                                                              
         CLI   BUFKEY+1,C'W'                                                    
         BNE   *+12                                                             
         CLI   PROGPROF+3,C'Y'     PROFILE OPT FOR OVERHEAD TOTAL LINE          
         BNE   F40                                                              
         CLI   QOPT3,C'Y'                                                       
         BE    F5                                                               
         CLI   BUFKEY+1,C'A'       SUPPRESS BILLING & INCOME DETAILS            
         BE    F40                                                              
         CLI   BUFKEY+1,C'E'                                                    
         BE    F40                                                              
*&&                                                                             
*&&UK                                                                           
         CLI   BUFKEY+1,C'X'       SKIP TOTAL O'HEAD,TIME,EXPENSE LINE          
         BE    F40                                                              
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   F5                                                               
         CLI   BUFKEY+1,C'A'       SUPPRESS BILLING & INCOME DETAILS            
         BE    F40                                                              
         CLI   BUFKEY+1,C'E'                                                    
         BE    F40                                                              
*&&                                                                             
F5       DS    0H                                                               
         CLI   LINID,X'FF'         DIE IF NO MATCH ON LINE NO                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
F6       CLC   LINID(3),BUFKEY+1                                                
         BE    F8                                                               
         LA    R6,L'LINTAB(,R6)                                                 
         B     F5                                                               
*                                                                               
F8       MVC   P+1(29),LINHEAD                                                  
         CLC   P+1(29),SPACES                                                   
         BE    F40                 DON'T PRINT IF NO NAME                       
         CLI   RCSUBPRG,3                                                       
         BNE   F9                                                               
         CLI   BUFKEY+3,C' '       NO TOTAL LINES FOR DOWNLOADING               
         BE    F40                                                              
         CLI   BUFKEY+1,C'S'                                                    
         BE    F40                                                              
         CLI   BUFKEY+1,C'Y'                                                    
         BE    F40                                                              
         MVI   DLSTAT,1                                                         
         GOTO1 ADLOAD,(RC)                                                      
*                                                                               
F9       LA    R2,P+30                                                          
         L     R7,ACOLTAB                                                       
         L     R3,MYWORD           ADDRESS OF COLUMN LIST                       
         LA    RF,8                                                             
         LA    R5,BUFACCS                                                       
*                                                                               
F10      LA    R1,L'COLTAB                                                      
         STH   R1,HALF                                                          
         IC    R1,0(,R3)                                                        
         BCTR  R1,R0                                                            
         MH    R1,HALF                                                          
         L     R7,ACOLTAB                                                       
         AR    R7,R1                                                            
         CLI   BUFKEY,9                                                         
         BNE   F11                                                              
         BAS   RE,TYPE3FIX                                                      
         B     F22                                                              
*                                                                               
F11      CLI   BUFKEY,3                                                         
         BNE   F11A                                                             
         BAS   RE,TYPE3FIX                                                      
         B     F22                                                              
*                                                                               
F11A     CLI   BUFKEY,12                                                        
         BNE   F11B                                                             
         BAS   RE,TYPE3FIX                                                      
         B     F22                                                              
*                                                                               
F11B     CLI   BUFKEY,15                                                        
         BNE   F12                                                              
         BAS   RE,TYPE3FIX                                                      
         B     F22                                                              
*                                                                               
F12      CLI   BUFKEY,8                                                         
         BNE   F14                                                              
         BAS   RE,TYPE8FIX                                                      
         B     F22                                                              
*                                                                               
F14      CLI   BUFKEY,10                                                        
         BNE   F15                                                              
         BAS   RE,TYPE8FIX                                                      
         B     F22                                                              
*                                                                               
F15      CLI   BUFKEY,11                                                        
         BNE   F16                                                              
         BAS   RE,TYPEFFIX                                                      
         B     F22                                                              
*                                                                               
F16      DS    0H                                                               
         CLI   COLTYPE,C'P'        PERCENT CALCULATION REQUIRED                 
         BNE   F20                                                              
*                                                                               
         ZAP   DIV,0(8,R5)                                                      
         MP    DIV,=P'10000'                                                    
         CP    DUB,=P'0'           DUB HAS BUDGET FROM PREV. CALCN              
         BE    F22                                                              
         DP    DIV,DUB+2(6)                                                     
         AP    DIV(8),=P'50'                                                    
         DP    DIV(8),=P'100'                                                   
         CLI   RCSUBPRG,3                                                       
         BNE   F17                                                              
         MVI   DLSTAT,2                                                         
         LR    R0,RF               SAVE BCT COUNT                               
         GOTO1 ADLOAD,(RC)                                                      
         LR    RF,R0                                                            
         B     F22                                                              
*                                                                               
F17      CLI   PROGPROF+10,C'Y'                                                 
         BNE   F18                                                              
         CP    DIV(6),=P'0'                                                     
         BNL   F18                                                              
         ZAP   DUB3,DIV(6)                                                      
         ST    RE,FULL2                                                         
         BAS   RE,EDIT9                                                         
         L     RE,FULL2                                                         
         B     F18A                                                             
*                                                                               
F18      EDIT  (P6,DIV),(9,(R2)),MINUS=YES                                      
*                                                                               
F18A     CP    DUB,=P'0'                                                        
         BNE   F22                                                              
         MVC   3(7,R2),SPACES                                                   
         B     F22                                                              
*                                                                               
F20      ZAP   DUB2,0(8,R5)                                                     
         AP    DUB2,=P'50'                                                      
         CP    DUB2,=P'0'          MINUS ROUNDING                               
         BNL   *+10                                                             
         SP    DUB2,=P'100'                                                     
         DP    DUB2,=P'100'        LOSE PENNIES                                 
         CLI   RCSUBPRG,3                                                       
         BNE   F21                                                              
         MVI   DLSTAT,2                                                         
         LR    R0,RF               SAVE BCT COUNT                               
         GOTO1 ADLOAD,(RC)                                                      
         LR    RF,R0                                                            
         B     F21H                                                             
*                                                                               
F21      CLI   PROGPROF+10,C'Y'                                                 
         BNE   F21B                                                             
         CP    DUB2(6),=P'0'                                                    
         BNL   F21B                                                             
         ZAP   DUB3,DUB2(6)                                                     
         ST    RE,FULL2                                                         
         BAS   RE,EDIT10                                                        
         L     RE,FULL2                                                         
         B     F21H                                                             
*                                                                               
F21B     EDIT  (P6,DUB2),(10,(R2)),MINUS=YES                                    
*                                                                               
F21H     ZAP   DUB,0(8,R5)         RESTORE WHOLE VALUE FOR BUDGET               
*                                                                               
F22      L     R7,ACOLTAB                                                       
         LA    R5,8(,R5)           ACCUMS                                       
         LA    R3,1(,R3)           LINE LIST                                    
         LA    R2,10(,R2)          PRINT LINE                                   
         BCT   RF,F10                                                           
         CLI   RCSUBPRG,3                                                       
         BNE   F23                                                              
         MVI   DLSTAT,3                                                         
         GOTO1 ADLOAD,(RC)                                                      
         B     F40                                                              
*                                                                               
F23      CLC   P+31(100),SPACES    NOW THE FANCY PRINTING                       
         BE    F40                                                              
         MVI   DIV,0               USE AS SPACING SWITCH                        
         MVC   P+1(29),LINHEAD                                                  
         CLI   PROGPROF+8,C'Y'     NO CHICKEN TRACKS IF COMPRESSING             
         BE    F36                                                              
*&&US                                                                           
         CLI   BUFKEY+1,C'X'                                                    
         BE    F34                                                              
*&&                                                                             
         IF    BUFKEY+1,=,C'G',OR,C'S',OR,C'Y',F34                              
         CLI   BUFKEY+1,C'K'                                                    
         BE    F24                                                              
         CLI   BUFKEY+1,C'Q'                                                    
         BNE   F26                                                              
*                                                                               
F24      CLI   PROGPROF+5,C'Y'     PROFILE OPTION FOR FANCY                     
         BE    F34                 PRINTING ON TOTAL EXPENSES                   
         B     F36                                                              
*                                                                               
F26      CLI   BUFKEY+1,C'R'                                                    
         BNE   F36                                                              
         CLI   PROGPROF+6,C'Y'     FANCY PRINTING ON TIME+EXPENSE LINE          
         BE    F34                                                              
         B     F36                                                              
*                                                                               
F34      BAS   RE,SETBOX           PRINT BOX LINE BEFORE DATA LINE              
*                                                                               
         L     R4,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,R4                                                      
*                                                                               
         L     R4,ADMASTD                                                       
*                                                                               
         USING MASTD,R4                                                         
*                                                                               
         L     R4,MCVREMOT                                                      
*                                                                               
         USING REMOTED,R4                                                       
*                                                                               
         OC    REMOTKEY,REMOTKEY                                                
         BNZ   F34A1               BRANCH IF REMOTE                             
*&&UK*&& B     F34A1               OR IN UK                                     
         GOTO1 =A(MYREPORT),DMCB,(RC),RR=RELO                                   
*&&US                                                                           
         CLI   BUFKEY+1,C'X'                                                    
         BE    F36                 DON'T UNDERLINE EXPENSE LINE                 
         CLI   BUFKEY+1,C'Y'                                                    
         BE    F36                 OR BOTTOM LINE                               
*&&                                                                             
         ZIC   RF,LINE                                                          
         LA    RE,MYROW(RF)                                                     
         BCTR  RE,0                                                             
         MVI   0(RE),C'M'                                                       
         B     F36                                                              
*                                                                               
F34A1    MVI   P+1,C'-'                                                         
*&&UK*&& LA    R4,27               EX IS 27 + (COL*10)                          
*&&US*&& LA    R4,28               EX IS 28 + (COL*10)                          
         LA    RF,8                MAX IS EIGHT                                 
         SR    R1,R1                                                            
         ICM   R1,1,BUFKEY         GET REPORT #                                 
         BZ    *+6                 USE DEFAULT (MIGHT BE WRONG THOUGH)          
         BCTR  R1,0                                                             
         MH    R1,=H'08'           ENTRY LENGTH IN TABLE                        
*                                                                               
         LA    R1,SELECTAB(R1)     POINT TO ENTRY                               
*                                                                               
F35      CLI   0(R1),0                                                          
         BE    F35A                                                             
         AH    R4,=H'10'                                                        
         LA    R1,1(,R1)                                                        
         BCT   RF,F35                                                           
*                                                                               
F35A     EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+2(0),P+1                                                       
         MVC   PTHIRD,P                                                         
*                                                                               
F36      CLI   P+3,C'*'            DO WE WANT TO SPACE                          
         BNE   *+8                 FOR TOTAL LINE                               
         MVI   SPACING,2                                                        
         GOTO1 =A(MYREPORT),DMCB,(RC),RR=RELO                                   
*                                                                               
F40      L     R2,WORD                                                          
         MVC   P,SPACES                                                         
         CLI   THISLEV,X'FF'                                                    
         BNE   F40A                NOT GROUP TOTALS                             
         L     R5,ADGRPTOT                                                      
*                                                                               
         USING BIND,R5                                                          
*                                                                               
         OC    BININ,BININ                                                      
         BZ    FEND                NOTHING IN TABLE                             
         L     RF,BININ            NUMBER IN TABLE                              
         BCTR  RF,0                LESS ONE                                     
         ST    RF,BININ            NUMER REMAINING                              
         L     R5,LSTBINRC                                                      
         LA    R5,L'BUFLINE(,R5)                                                
         ST    R5,LSTBINRC         A(LAST BIN RECORD)                           
         MVC   BUFLINE,0(R5)       RECORD TO BUFF AREA                          
         B     F40B                                                             
*                                                                               
F40A     CLI   BUFKEY+1,C'G'       JUST IN CASE GROSS                           
         BNE   *+10                IS FIRST BUFFALO RECORD                      
         MVC   INCOME,BUFACCS                                                   
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFLINE,(R2)                         
         TM    DMCB+8,X'80'                                                     
         BO    FEND                                                             
*                                                                               
F40B     CLI   BUFKEY+1,C'G'                                                    
         BNE   F42                                                              
         MVC   INCOME,BUFACCS                                                   
         B     F44                                                              
*                                                                               
F42      CLI   BUFKEY+1,C'Y'       NET PROFIT AND LOSS                          
         BNE   F44                                                              
         MVC   PROFIT,BUFACCS                                                   
*                                                                               
F44      DS    0H                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   F46                                                              
         CLI   BUFKEY+1,C'G'                                                    
         BH    FENDEND                                                          
*&&UK                                                                           
         B     F4                                                               
*&&                                                                             
*                                                                               
F46      CLI   PROGPROF,C'Y'       IF ORDER IS INVERTED - WE MUST               
         BNE   F4                  START AGAIN AT FRONT OF TABLE                
         L     R6,ADLINTAB                                                      
         B     F4                                                               
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
*                                                                               
FEND     DS    0H                                                               
         CLI   PROGPROF+15,0                                                    
         BE    FEND01                                                           
*T       BAS   RE,TAXIT            CALCULATE POST TAX PROFIT                    
         GOTO1 ATAXIT,DMCB,(RC)                                                 
*                                  NOW DO PERCENTAGE LINE                       
FEND01   L     R6,MYWORD           A(SELECTAB)                                  
         LA    R2,P+31                                                          
         LA    R4,INCOME                                                        
         LA    R5,PROFIT                                                        
         LA    RF,8                                                             
*                                                                               
FEND1    ZAP   DUB,0(8,R4)         LOSE PENNIES (NUMBERS TOO BIG)               
         AP    DUB,=P'50'                                                       
         CP    DUB,=P'0'                                                        
         BNL   *+10                                                             
         SP    DUB,=P'100'                                                      
         DP    DUB,=P'100'                                                      
         ZAP   0(8,R4),DUB(6)                                                   
         ZAP   DUB,0(8,R5)                                                      
         AP    DUB,=P'50'                                                       
         CP    DUB,=P'0'                                                        
         BNL   *+10                                                             
         SP    DUB,=P'100'                                                      
         DP    DUB,=P'100'                                                      
         ZAP   0(8,R5),DUB(6)                                                   
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   RF,FEND1                                                         
         LA    R4,INCOME                                                        
         LA    R5,PROFIT                                                        
         LA    RF,8                                                             
*                                                                               
FEND2    DS    0H                                                               
         SR    R1,R1               GO FROM SELECTAB ENTRY TO COLTAB             
         IC    R1,0(,R6)                                                        
         BCTR  R1,0                                                             
         MH    R1,=H'25'                                                        
         L     R7,ACOLTAB                                                       
         AR    R7,R1                                                            
         CLI   0(R7),C'P'          AND SEE IF WE SHOULD PRINT PERCENT           
         BE    FEND4                                                            
*                                                                               
         CP    0(8,R4),=P'0'                                                    
         BNH   FEND4                                                            
         CP    0(8,R5),=P'0'       TRY TO AVOID NEGATIVE PCT                    
         BL    FEND4                                                            
         CVB   R1,0(,R5)                                                        
         CVB   RE,0(,R4)                                                        
         M     R0,=F'10000'        CALCULATE PCT=PROFITX100/INCOME              
         DR    R0,RE                                                            
         LH    RE,=H'50'                                                        
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         LCR   RE,RE                                                            
         AR    R1,RE                                                            
         LR    R0,R1                                                            
         SRDA  R0,32                                                            
         CLI   QOPT1,C'F'                                                       
         BE    FEND3                                                            
         D     R0,=F'100'                                                       
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   FEND2B                                                           
         LTR   R1,R1                                                            
         BNM   FEND2B                                                           
         CVD   R1,DUB3                                                          
         ST    RE,FULL2                                                         
         BAS   RE,EDIT9                                                         
         L     RE,FULL2                                                         
         B     FEND4                                                            
*                                                                               
FEND2B   EDIT  (R1),(9,(R2)),MINUS=YES                                          
         B     FEND4                                                            
*                                                                               
FEND3    D     R0,=F'10'                                                        
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   FEND3B                                                           
         LTR   R1,R1                                                            
         BNM   FEND3B                                                           
         CVD   R1,DUB3                                                          
         ST    RE,FULL2                                                         
         BAS   RE,EDIT1                                                         
         L     RE,FULL2                                                         
         B     FEND3C                                                           
*                                                                               
FEND3B   EDIT  (R1),(9,WORK+30),1                                               
         LTR   R1,R1                                                            
         BNM   *+8                                                              
         MVI   WORK+39,C'-'                                                     
*                                                                               
FEND3C   BCTR  R2,0                                                             
         MVC   0(10,R2),WORK+30                                                 
         B     FEND4                                                            
*                                                                               
FEND4    LA    R6,1(,R6)                                                        
         LA    R2,10(,R2)                                                       
         LA    R4,8(,R4)                                                        
         LA    R5,8(,R5)                                                        
         BCT   RF,FEND2                                                         
*                                                                               
FEND10   DS    0H                                                               
         CLC   P+31(80),SPACES     ANY NUMBERS TO PRINT                         
         BE    FENDEND                                                          
         MVI   SPACING,2                                                        
*&&US                                                                           
         MVI   SPACING,1                                                        
         MVC   P+1(17),=C'PROFIT PERCENTAGE'                                    
         CLC   ALPHAID,=C'TB'      FOR TBC                                      
         BNE   *+10                                                             
         MVC   P+1(27),=C'PERCENT OF OPERATING INCOME'                          
         BAS   RE,SETBOX                                                        
*&&                                                                             
*&&UK*&& MVC   P+1(25),=C'PROFIT/REVENUE PERCENTAGE'                            
         GOTO1 =A(MYREPORT),DMCB,(RC),RR=RELO                                   
*                                                                               
FENDEND  CLI   MODE,REQLAST                                                     
         BE    XIT                                                              
         CLI   QOPT2,C'S'                                                       
         BE    XIT                                                              
         CLC   SUMACCS(40),=8PL8'0'                                             
         BE    XIT                                                              
         L     R1,ADWORK                                                        
         LA    R5,SUMREC                                                        
         LR    R2,R1                                                            
         WRITE DECB0001,SF,(R2),(R5)                                            
         CHECK DECB0001                CHECK                                    
         B     XIT                                                              
*                                                                               
         DROP  R4,R5,R7            KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        SETUP BOXES                                                  *         
*---------------------------------------------------------------------*         
SETBOX   NTR1                                                                   
         MVC   PSECOND,P           PUT DATA ON SECOND PRINT LINE                
         MVI   DIV,X'FF'                                                        
         MVC   P,SPACES                                                         
         MVC   MYROW,SPACES                                                     
         ZIC   RF,LINE                                                          
         AH    RF,=H'3'                                                         
         ZIC   RE,MAXLINES     IF I'M ABOUT TO GO OVER END OF PAGE              
         CR    RF,RE           SAVE PRINT LINE, FORCE NEW PAGE                  
         BNH   SETBX01                                                          
         MVC   PSV,PSECOND                                                      
         MVC   PSECOND,SPACES                                                   
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 =A(MYREPORT),DMCB,(RC),RR=RELO                                   
         MVC   PSECOND,PSV                                                      
*                                                                               
SETBX01  ZIC   RF,LINE                                                          
         LA    RE,MYROW(RF)                                                     
         BCTR  RE,0                                                             
         MVI   0(RE),C'M'                                                       
         B     XIT                                                              
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        PRODUCE NEW SUMMARY                                                    
*--------------------------------------------------------------------*          
COMPAREM NTR1                                                                   
         MVI   MYBOXSW,2                                                        
         LA    RF,6                                                             
         LA    RE,HEAD7                                                         
*                                                                               
COMP1    MVC   0(L'P,RE),SPACES                                                 
         LA    RE,L'P(,RE)                                                      
         BCT   RF,COMP1                                                         
         LA    RF,3                                                             
         LA    RE,P                                                             
*                                                                               
COMP1A   MVC   0(L'P,RE),SPACES                                                 
         LA    RE,L'P(,RE)                                                      
         BCT   RF,COMP1A                                                        
         L     R1,ADWORK                                                        
         LR    R2,R1                                                            
         POINT (R2),TTRFIRST                                                    
         CHECK DECB0002                CHECK                                    
         MVC   HEAD5+87(23),HDAT                                                
         MVC   P+1(14),=C'COMPANY TOTALS'                                       
         LA    RE,AGYTOTS                                                       
         LA    R2,P+43                                                          
         LA    RF,6                                                             
*                                                                               
COMP2    ZAP   DUB2,0(8,RE)                                                     
         AP    DUB2,=P'50'                                                      
         CP    DUB2,=P'0'          MINUS ROUNDING                               
         BNL   *+10                                                             
         SP    DUB2,=P'100'                                                     
         DP    DUB2,=P'100'        LOSE PENNIES                                 
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   COMP3A                                                           
         CP    DUB2(6),=P'0'                                                    
         BNL   COMP3A                                                           
         ZAP   DUB3,DUB2(6)                                                     
         ST    RE,FULL2                                                         
         BAS   RE,EDIT11                                                        
         L     RE,FULL2                                                         
         B     COMP3B                                                           
*                                                                               
COMP3A   EDIT  (P6,DUB2),(11,(R2)),MINUS=YES                                    
*                                                                               
COMP3B   LA    R2,11(,R2)                                                       
         LA    RE,8(,RE)                                                        
         BCT   RF,COMP2                                                         
         GOTO1 ACREPORT                                                         
         CLI   COMPBYTE,1          MONEY MODE                                   
         BE    COMP6                                                            
*                                                                               
         MVC   P+9(3),=C'PCT'                                                   
         LA    R2,P+50                                                          
         LA    RF,6                                                             
*                                                                               
COMP4    MVC   0(3,R2),=C'100'                                                  
         LA    R2,11(,R2)                                                       
         BCT   RF,COMP4                                                         
*                                                                               
COMP6    GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+1(17),=C'ACCOUNT CODE/NAME'                                    
         MVI   PSECOND+1,C'-'                                                   
         MVC   PSECOND+2(16),PSECOND+1                                          
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
COMP10   DS    0H                                                               
         L     R1,ADWORK                                                        
         LA    R5,SUMREC                                                        
         LR    R2,R1                                                            
         READ  DECB0003,SF,(R2),(R5)                                            
         CHECK DECB0003                CHECK                                    
         CLC   SUMREC(2),=C'/*'                                                 
         BE    COMP20                                                           
         MVC   P+1(12),SUMNUM                                                   
         GOTO1 CHOPPER,DMCB,(36,SUMNAME),(28,P+14),(C'P',2)                     
         LA    RF,6                                                             
         LA    R2,P+49                                                          
         CLI   COMPBYTE,0                                                       
         BE    *+8                                                              
         LA    R2,P+43                                                          
         LA    R4,AGYTOTS                                                       
         LA    R5,SUMACCS                                                       
*                                                                               
COMP12   CP    0(8,R4),=P'0'                                                    
         BE    COMP14                                                           
         CLI   COMPBYTE,0                                                       
         BE    COMP13                                                           
         ZAP   DUB2,0(8,R5)        FORMAT FOR MONEY MODE                        
         AP    DUB2,=P'50'                                                      
         CP    DUB2,=P'0'          MINUS ROUNDING                               
         BNL   *+10                                                             
         SP    DUB2,=P'100'                                                     
         DP    DUB2,=P'100'        LOSE PENNIES                                 
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   COMP12B                                                          
         CP    DUB2(6),=P'0'                                                    
         BNL   COMP12B                                                          
         ZAP   DUB3,DUB2(6)                                                     
         ST    RE,FULL2                                                         
         BAS   RE,EDIT11                                                        
         L     RE,FULL2                                                         
         B     COMP14                                                           
*                                                                               
COMP12B  EDIT  (P6,DUB2),(11,(R2)),MINUS=YES                                    
         B     COMP14                                                           
*                                                                               
COMP13   DS    0H                  FORMAT FOR PERCENT MODE                      
         ZAP   DIV,0(8,R5)                                                      
         MP    DIV,=P'10000'                                                    
         DP    DIV,2(6,R4)                                                      
         AP    DIV(8),=P'50'                                                    
         CP    DIV(8),=P'0'                                                     
         BNL   *+10                                                             
         SP    DIV(8),=P'100'                                                   
         DP    DIV(8),=P'100'                                                   
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   COMP13B                                                          
         CP    DIV(6),=P'0'                                                     
         BNL   COMP13B                                                          
         ST    RE,FULL2                                                         
         BAS   RE,EDIT5                                                         
         L     RE,FULL2                                                         
         B     COMP14                                                           
*                                                                               
COMP13B  EDIT  (P6,DIV),(5,(R2)),MINUS=YES                                      
*                                                                               
COMP14   LA    R2,11(,R2)                                                       
         LA    R4,8(,R4)                                                        
         LA    R5,8(,R5)                                                        
         BCT   RF,COMP12                                                        
         MVC   HEAD5+87(23),HDAT                                                
         CLC   P+50(60),SPACES                                                  
         BNE   COMP16                                                           
         MVC   P+1(50),SPACES                                                   
         B     COMP10                                                           
*                                                                               
COMP16   DS    0H                                                               
         MVI   MYBOXSW,2                                                        
         GOTO1 ACREPORT                                                         
         B     COMP10                                                           
*                                                                               
COMP20   DS    0H                                                               
         B     XIT                                                              
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        DO FUNNY BUSINESS FOR REPORT 3                                         
*--------------------------------------------------------------------*          
*                                                                               
         USING COLTABD,R7                                                       
*                                                                               
TYPE3FIX NTR1                                                                   
         CLI   COLTYPE,C'S'        REVENUE PCT COLUMN                           
         BNE   FIX10                                                            
         CLI   BUFKEY+1,C'G'                                                    
         BNE   FIX2                                                             
         ZAP   TOTREV,0(8,R5)      TOTAL REVENUE                                
         B     XIT                                                              
*                                                                               
FIX2     CLI   BUFKEY+1,C'E'       NO PERCENTAGE FOR BILLING/INCOME             
         BNH   XIT                                                              
*                                                                               
FIX4     CP    TOTREV,=P'0'                                                     
         BNH   XIT                                                              
         ZAP   DIV,0(8,R5)                                                      
         MP    DIV,=P'10000'                                                    
         DP    DIV,TOTREV+2(6)                                                  
         AP    DIV(8),=P'50'                                                    
         DP    DIV(8),=P'100'                                                   
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   FIX5                                                             
         CP    DIV(6),=P'0'                                                     
         BNL   FIX5                                                             
         ZAP   DUB3,DIV(6)                                                      
         ST    RE,FULL2                                                         
         BAS   RE,EDIT10                                                        
         L     RE,FULL2                                                         
         B     FIX6                                                             
*                                                                               
FIX5     EDIT  (P6,DIV),(10,(R2)),MINUS=YES                                     
*                                                                               
FIX6     CP    DUB,=P'0'                                                        
         BNE   XIT                                                              
         MVC   3(7,R2),SPACES                                                   
         B     XIT                                                              
*                                                                               
FIX10    ZAP   DUB2,0(8,R5)                                                     
         CLI   COLTYPE,C'V'        VARIANCE COLUMN                              
         BE    FIX12                                                            
         CLI   COLTYPE,C'P'                                                     
         BE    FIX22                                                            
         B     FIX20                                                            
*                                                                               
FIX12    LR    R1,R5                                                            
         SH    R1,=H'8'            ACTUAL LESS BUDGET                           
         SP    DUB2,0(8,R1)                                                     
         CLI   BUFKEY,9            FOR TYPE 9 REVERSE SIGN ON VARIANCE          
         BNE   FIX20               FOR EXPENSES                                 
         CLI   BUFKEY+1,C'G'                                                    
         BNH   FIX20                                                            
         CLI   BUFKEY+1,C'Y'                                                    
         BNL   FIX20                                                            
         MP    DUB2(8),=P'-1'                                                   
*                                                                               
FIX20    AP    DUB2,=P'50'                                                      
         CP    DUB2,=P'0'          MINUS ROUNDING                               
         BNL   *+10                                                             
         SP    DUB2,=P'100'                                                     
         DP    DUB2,=P'100'        LOSE PENNIES                                 
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   FIX21                                                            
         CP    DUB2(6),=P'0'                                                    
         BNL   FIX21                                                            
         ZAP   DUB3,DUB2(6)                                                     
         ST    RE,FULL2                                                         
         BAS   RE,EDIT10                                                        
         L     RE,FULL2                                                         
         B     XIT                                                              
*                                                                               
FIX21    EDIT  (P6,DUB2),(10,(R2)),MINUS=YES                                    
         B     XIT                                                              
*                                                                               
FIX22    DS    0H                                                               
         LR    R1,R5                                                            
         SH    R1,=H'8'                                                         
         ZAP   DUB,0(8,R1)                                                      
         ZAP   DIV,0(8,R5)                                                      
         MP    DIV,=P'10000'                                                    
         CP    DUB,=P'0'                                                        
         BE    XIT                                                              
         DP    DIV,DUB+2(6)                                                     
         AP    DIV(8),=P'50'                                                    
         DP    DIV(8),=P'100'                                                   
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   FIX25                                                            
         CP    DIV(6),=P'0'                                                     
         BNL   FIX25                                                            
         ZAP   DUB3,DIV(6)                                                      
         ST    RE,FULL2                                                         
         BAS   RE,EDIT9                                                         
         L     RE,FULL2                                                         
         B     FIX26                                                            
*                                                                               
FIX25    EDIT  (P6,DIV),(9,(R2)),MINUS=YES                                      
*                                                                               
FIX26    CP    DUB,=P'0'                                                        
         BNE   XIT                                                              
         MVC   3(7,R2),SPACES                                                   
         B     XIT                                                              
*                                                                               
         DROP  R7                  KEEP IT CLEAN                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        HANDLE REPORT 8                                                        
*--------------------------------------------------------------------*          
*                                                                               
         USING COLTABD,R7                                                       
*                                                                               
*&&US                                                                           
TYPE8FIX NTR1                                                                   
         CLI   COLTYPE,C'S'        BILLING PCT COLUMN                           
         BNE   FIX810                                                           
         CLI   BUFKEY+1,C'C'       TOTAL BILLING                                
         BNE   FIX82                                                            
         ZAP   TOTREV,0(8,R5)                                                   
         B     XIT                                                              
*                                                                               
FIX82    CLI   BUFKEY+1,C'A'       NO PCT FOR BILLING                           
         BE    XIT                                                              
         CP    TOTREV,=P'0'                                                     
         BNH   XIT                                                              
         ZAP   DIV,0(8,R5)                                                      
         MP    DIV,=P'10000'                                                    
         DP    DIV,TOTREV+2(6)                                                  
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   FIX85                                                            
         CP    DIV(8),=P'0'                                                     
         BNL   FIX85                                                            
         ST    RE,FULL2                                                         
         BAS   RE,EDIT2                                                         
         L     RE,FULL2                                                         
         B     FIX86                                                            
*                                                                               
FIX85    EDIT  (P8,DIV),(10,(R2)),2,MINUS=YES                                   
*                                                                               
FIX86    CP    DUB,=P'0'                                                        
         BNE   XIT                                                              
         MVC   3(7,R2),SPACES                                                   
         B     XIT                                                              
*                                                                               
FIX810   ZAP   DUB2,0(8,R5)                                                     
         CLI   COLTYPE,C'P'        PERCENT                                      
         BNE   FIX820                                                           
         LR    R1,R5                                                            
         SH    R1,=H'8'                                                         
         ZAP   DUB,0(8,R1)                                                      
         ZAP   DIV,0(8,R5)                                                      
         MP    DIV,=P'10000'                                                    
         CP    DUB,=P'0'                                                        
         BE    XIT                                                              
         DP    DIV,DUB+2(6)                                                     
         AP    DIV(8),=P'50'                                                    
         DP    DIV(8),=P'100'                                                   
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   FIX815                                                           
         CP    DIV(6),=P'0'                                                     
         BNL   FIX815                                                           
         ZAP   DUB3,DIV(6)                                                      
         ST    RE,FULL2                                                         
         BAS   RE,EDIT9                                                         
         L     RE,FULL2                                                         
         B     FIX816                                                           
*                                                                               
FIX815   EDIT  (P6,DIV),(9,(R2)),MINUS=YES                                      
*                                                                               
FIX816   CP    DUB,=P'0'                                                        
         BNE   XIT                                                              
         MVC   3(7,R2),SPACES                                                   
         B     XIT                                                              
*                                                                               
FIX820   DS    0H                                                               
         AP    DUB2,=P'50'                                                      
         CP    DUB2,=P'0'          MINUS ROUNDING                               
         BNL   *+10                                                             
         SP    DUB2,=P'100'                                                     
         DP    DUB2,=P'100'        LOSE PENNIES                                 
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   FIX830                                                           
         CP    DUB2(6),=P'0'                                                    
         BNL   FIX830                                                           
         ZAP   DUB3,DUB2(6)                                                     
         ST    RE,FULL2                                                         
         BAS   RE,EDIT10                                                        
         L     RE,FULL2                                                         
         B     XIT                                                              
*                                                                               
FIX830   EDIT  (P6,DUB2),(10,(R2)),MINUS=YES                                    
         B     XIT                                                              
*&&                                                                             
*                                                                               
*&&UK                                                                           
TYPE8FIX NTR1                                                                   
         MVI   HALF,C'M'           DOING EITHER THIS MONTH OR YTD               
         CLC   COLSTRT,COLEND                                                   
         BE    *+8                                                              
         MVI   HALF,C'Y'                                                        
         CLI   COLTYPE,C'S'        BILLING PCT COLUMN                           
         BNE   FIX880                                                           
         CLI   BUFKEY+1,C'A'       PUT EACH INDIVIDUAL BILLING AMT INTO         
         BNE   FIX87                                                            
         LA    RF,BILTAB           THE CORRECT PLACE BY BILL CATEGORY           
*                                                                               
FIX82    CLI   0(RF),0                                                          
         BE    FIX84                                                            
         CLC   BUFKEY+3(1),0(RF)                                                
         BE    FIX86                                                            
         LA    RF,17(,RF)                                                       
         B     FIX82                                                            
*                                                                               
FIX84    MVC   0(1,RF),BUFKEY+3                                                 
*                                                                               
FIX86    LA    RE,1(,RF)                                                        
         CLI   HALF,C'M'           ADD TO MONTH OR YTD COUNTER                  
         BE    *+8                                                              
         LA    RE,8(,RE)                                                        
         ZAP   0(8,RE),0(8,R5)                                                  
         B     XIT                                                              
*                                                                               
FIX87    CLI   BUFKEY+1,C'C'       TOTAL BILLING                                
         BNE   FIX88                                                            
         LA    RF,TOTREV           MONTHLY                                      
         CLI   HALF,C'M'                                                        
         BE    *+8                                                              
         LA    RF,TOTREV2          OR YTD                                       
         ZAP   0(8,RF),0(8,R5)                                                  
         B     XIT                                                              
*                                                                               
FIX88    ZAP   DUB2,TOTREV         DEFAULT VALUE IN CASE OF PROBS               
         CLI   HALF,C'M'                                                        
         BE    *+10                                                             
         ZAP   DUB2,TOTREV2        BUT USE THE CORRECT TOTAL FIGURE             
         CLI   BUFKEY+1,C'E'       DO PCT FOR INCOME ONLY                       
         BE    FIX810                                                           
         CLI   BUFKEY+1,C'G'                                                    
         BNE   XIT                                                              
         B     FIX822                                                           
*                                                                               
FIX810   LA    RF,BILTAB           FIND CORRESPONDING BILLING VALUE             
*                                                                               
FIX812   CLI   0(RF),0                                                          
         BE    FIX822                                                           
         CLC   BUFKEY+3(1),0(RF)                                                
         BE    FIX820                                                           
         LA    RF,17(,RF)                                                       
         B     FIX812                                                           
*                                                                               
FIX820   ZAP   DUB2,1(8,RF)        MONTH FIGURE                                 
         CLI   HALF,C'M'                                                        
         BE    *+10                                                             
         ZAP   DUB2,9(8,RF)        OR YTD                                       
*                                                                               
FIX822   CP    DUB2,=P'0'                                                       
         BE    XIT                                                              
         ZAP   DIV,0(8,R5)                                                      
         MP    DIV,=P'10000'                                                    
         DP    DIV,DUB2+2(6)                                                    
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   FIX870                                                           
         CP    DIV(8),=P'0'                                                     
         BNL   FIX870                                                           
         ST    RE,FULL2                                                         
         BAS   RE,EDIT2                                                         
         L     RE,FULL2                                                         
         B     FIX871                                                           
*                                                                               
FIX870   EDIT  (P8,DIV),(10,(R2)),2,MINUS=YES                                   
*                                                                               
FIX871   CP    DUB,=P'0'                                                        
         BNE   XIT                                                              
         MVC   3(7,R2),SPACES                                                   
         B     XIT                                                              
*                                                                               
FIX880   ZAP   DUB2,0(8,R5)                                                     
         CLI   COLTYPE,C'V'        VARIANCE COLUMN                              
         BNE   FIX890                                                           
         LR    R1,R5                                                            
         SH    R1,=H'8'            POINT BACK TO YTD BUDGET                     
         CP    0(8,R1),=P'0'                                                    
         BE    XIT                 EXIT IF NO BUDGET                            
         ZAP   DUB2,0(8,R5)        ACTUAL LESS BUDGET                           
         SP    DUB2,0(8,R1)                                                     
*                                                                               
FIX890   AP    DUB2,=P'50'                                                      
         CP    DUB2,=P'0'          MINUS ROUNDING                               
         BNL   *+10                                                             
         SP    DUB2,=P'100'                                                     
         DP    DUB2,=P'100'        LOSE PENNIES                                 
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   FIX895                                                           
         CP    DUB2(6),=P'0'                                                    
         BNL   FIX895                                                           
         ZAP   DUB3,DUB2(6)                                                     
         ST    RE,FULL2                                                         
         BAS   RE,EDIT10                                                        
         L     RE,FULL2                                                         
         B     XIT                                                              
*                                                                               
FIX895   EDIT  (P6,DUB2),(10,(R2)),MINUS=YES                                    
         B     XIT                                                              
*&&                                                                             
*                                                                               
         DROP  R7                  KEEP IT CLEAN                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        HANDLE REPORT F                                                        
*--------------------------------------------------------------------*          
TYPEFFIX NTR1                                                                   
         CP    0(8,R5),=P'0'                                                    
         BE    XIT                                                              
         ZAP   DUB2,0(8,R5)                                                     
         MVC   WORK+30(10),SPACES                                               
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   TYP10                                                            
         CP    DUB2(8),=P'0'                                                    
         BNL   TYP10                                                            
         ST    RE,FULL2                                                         
         BAS   RE,EDIT1                                                         
         L     RE,FULL2                                                         
         B     TYP11                                                            
*                                                                               
TYP10    EDIT  (P8,DUB2),(9,WORK+30),1                                          
*                                                                               
         CP    DUB2,=P'0'                                                       
         BNL   *+8                                                              
         MVI   WORK+39,C'-'                                                     
*                                                                               
TYP11    MVC   0(10,R2),WORK+30                                                 
         B     XIT                                                              
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        ROUND                                                                  
*---------------------------------------------------------------------*         
ROUND    NTR1                                                                   
         LA    R1,BUFACCS                                                       
         LA    R0,8                                                             
*                                                                               
ROUND1   SRP   0(8,R1),60,5                                                     
         LA    R1,8(,R1)                                                        
         BCT   R0,ROUND1                                                        
         B     XIT                                                              
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        CHANGE PRINTING ORDER OF 13/14 POSTINGS                                
*--------------------------------------------------------------------*          
INVERT   NTR1                                                                   
         L     R6,ADLINTAB                                                      
*                                                                               
         USING LINTABD,R6                                                       
*                                                                               
INV2     CLI   LINID,X'FF'                                                      
         BE    XIT                                                              
         CLI   LINID,C'I'                                                       
         BE    INV6                                                             
         CLI   LINID,C'K'                                                       
         BE    INV8                                                             
*                                                                               
INV4     LA    R6,L'LINTAB(R6)                                                  
         B     INV2                                                             
*                                                                               
INV6     MVI   LINID,C'P'                                                       
         MVC   LINPARM(2),=C'PQ'                                                
         B     INV4                                                             
*                                                                               
INV8     MVI   LINID,C'Q'                                                       
         B     INV4                                                             
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*                                                                               
*--------------------------------------------------------------------*          
DLPRINT  NTR1                                                                   
         MVC   P,DLPLINE                                                        
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   SPACING,1                                                        
         GOTO1 ACREPORT                                                         
         MVC   DLPLINE,SPACES                                                   
         MVI   LINE,1                                                           
         XIT1                      XIT UNADDRESSABLE                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        REPLACE TABLE NAMES WITH LEDGER NAMES                                  
*--------------------------------------------------------------------*          
*                                                                               
         USING LINTABD,R6                                                       
*                                                                               
REPLACE  NTR1                                                                   
         L     R6,ADLINTAB                                                      
         MVC   SAVEKEY1,SPACES                                                  
         MVC   SAVEKEY1(L'KEY),KEY                                              
         L     R2,ADCOMP           NOW SET UP KEY                               
         MVC   IOA(1),0(R2)        COMPANY                                      
         MVI   IOA+1,C'1'          UNIT                                         
         MVC   IOA+2(44),SPACES                                                 
*                                                                               
REPL2    CLI   LINID,X'FF'                                                      
         BE    REPL20                                                           
         CLI   LINKEY+1,C' '       FIND A LEDGER VALUE                          
         BNE   REPL4                                                            
         CLI   LINKEY,C' '                                                      
         BNE   REPL6                                                            
*                                                                               
REPL4    LA    R6,L'LINTAB(R6)                                                  
         B     REPL2                                                            
*                                                                               
REPL6    DC    0H'0'                                                            
         MVC   IOA(1),0(R2)        COMPANY                                      
         MVI   IOA+1,C'1'          UNIT                                         
         MVC   IOA+2(44),SPACES                                                 
         MVC   IOA+2(1),LINKEY     LEDGER CODE                                  
         MVC   SAVEH6(15),IOA                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',IOA,IOA                          
         CLC   IOA(3),SAVEH6                                                    
         BNE   REPL4                                                            
         SR    RF,RF                                                            
         LA    R5,IOA                                                           
         AH    R5,DATADISP                                                      
         MVC   IOA+2(44),SPACES                                                 
*                                                                               
REPL10   CLI   0(R5),0                                                          
         BE    REPL4                                                            
         CLI   0(R5),X'20'                                                      
         BE    REPL12                                                           
         IC    RF,1(,R5)                                                        
         AR    R5,RF                                                            
         B     REPL10                                                           
*                                                                               
         USING ACNAMED,R5                                                       
*                                                                               
REPL12   ZIC   R7,ACNMLEN                                                       
         SH    R7,=H'2'                                                         
         MVC   LINHEAD+5(24),SPACES                                             
         GOTO1 CHOPPER,DMCB,((R7),ACNMNAME),(21,LINHEAD+5),1                    
         LA    R7,LINHEAD+26                                                    
         CLI   0(R7),C' '                                                       
         BNE   *+8                                                              
         BCT   R7,*-8                                                           
         MVC   2(2,R7),=C'**'                                                   
         B     REPL4                                                            
*                                                                               
REPL20   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',SAVEKEY1,IOA                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
*                                                                               
         DROP  R5,R6               KEEP IT CLEAN                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        FILL LINE-TABLE NAME FIELDS FROM FILE                                  
*--------------------------------------------------------------------*          
*                                                                               
         USING LINTABD,R6                                                       
*                                                                               
BUDGNM   NTR1                                                                   
         L     R6,ADLINTAB                                                      
         MVC   SAVEKEY1,SPACES                                                  
         MVC   SAVEKEY1(L'KEY),KEY                                              
         L     R2,ADCOMP           NOW SET UP KEY                               
         MVC   IOA(1),0(R2)        COMPANY                                      
         MVI   IOA+1,C'1'          UNIT                                         
         MVC   IOA+2(44),SPACES                                                 
*                                                                               
BUDN2    CLI   LINID,X'FF'                                                      
         BE    BUDN20                                                           
         CLI   LINKEY+1,C' '       FIND A LINE FOR AN ACCOUNT                   
         BNE   BUDN6                                                            
*                                                                               
BUDN4    LA    R6,L'LINTAB(,R6)                                                 
         B     BUDN2                                                            
*                                                                               
BUDN6    DC    0H'0'                                                            
         MVC   IOA(1),0(R2)        COMPANY                                      
         MVI   IOA+1,C'1'          UNIT                                         
         MVC   IOA+2(44),SPACES                                                 
         MVC   IOA+2(2),LINKEY     LEDGER AND 1-CHARACTER ACCOUNT               
         MVC   SAVEH6(15),IOA                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',IOA,IOA                          
         CLC   IOA(4),SAVEH6                                                    
         BNE   BUDN4                                                            
         SR    RF,RF                                                            
         LA    R5,IOA                                                           
         AH    R5,DATADISP                                                      
         MVC   IOA+2(44),SPACES                                                 
*                                                                               
BUDN10   CLI   0(R5),0                                                          
         BE    BUDN4                                                            
         CLI   0(R5),X'20'                                                      
         BE    BUDN12                                                           
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     BUDN10                                                           
*                                                                               
         USING ACNAMED,R5                                                       
*                                                                               
BUDN12   ZIC   R7,ACNMLEN                                                       
         SH    R7,=H'2'                                                         
         MVC   LINHEAD,SPACES                                                   
         GOTO1 CHOPPER,DMCB,((R7),ACNMNAME),(29,LINHEAD),1                      
         B     BUDN4                                                            
*                                                                               
BUDN20   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',SAVEKEY1,IOA                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
*                                                                               
         DROP  R5,R6               KEEP IT CLEAN                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        HEADLINE ROUTINES (HOOK)                                               
*--------------------------------------------------------------------*          
         PRINT GEN                                                              
EDIT5    NTR1                                                                   
         BCTR  R2,0                                                             
         EDIT  (P6,DIV),(5,0(R2)),BRACKET=YES                                   
         B     XIT                                                              
*                                                                               
EDIT9    NTR1                                                                   
         EDIT  (P8,DUB3),(8,0(R2)),BRACKET=YES                                  
         B     XIT                                                              
*                                                                               
EDIT10   NTR1                                                                   
         EDIT  (P8,DUB3),(9,0(R2)),BRACKET=YES                                  
         B     XIT                                                              
*                                                                               
EDIT11   NTR1                                                                   
         EDIT  (P8,DUB3),(10,0(R2)),BRACKET=YES                                 
         B     XIT                                                              
*                                                                               
*                                                                               
EDIT1    NTR1                                                                   
         EDIT  (P8,DUB2),(9,WORK+30),1,BRACKET=YES                              
         B     XIT                                                              
*                                                                               
EDIT2    NTR1                                                                   
         EDIT  (P8,DIV),(9,0(R2)),2,BRACKET=YES                                 
         B     XIT                                                              
*                                                                               
         PRINT NOGEN                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        HEADLINE ROUTINES (HOOK)                                               
*--------------------------------------------------------------------*          
         DS    0H                                                               
HOOK     NTR1                                                                   
         L     RB,APGM             ADDR OF   AC9A02                             
         L     RC,AWRKD            ADDR OF   WORKD                              
*                                                                               
         L     R4,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,R4                                                      
*                                                                               
         L     R4,ADMASTD                                                       
*                                                                               
         USING MASTD,R4                                                         
*                                                                               
         L     R4,MCBXAREA                                                      
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
         CLI   MYBOXSW,1                                                        
         BNE   HOOK10                                                           
         MVC   MYCOL,SPACES                                                     
         MVI   MYCOL,C'L'          FIX UP BOX COLUMNS                           
         LA    RF,MYCOL+30         START POINT FOR MIDDLES                      
         LA    RE,8                NO OF MIDDLES VARIES BY REQUEST NO.          
*&&US                                                                           
         CLC   ALPHAID,=C'BS'      EXCEPT FOR BATES                             
         BNE   *+8                                                              
         LA    RE,6                                                             
*&&                                                                             
         CLI   QOPT1,C'1'                                                       
         BE    HOOK6                                                            
         LA    RE,8                                                             
         CLI   QOPT1,C'5'                                                       
         BE    HOOK6                                                            
         CLI   QOPT1,C'6'                                                       
         BE    HOOK6                                                            
         CLI   QOPT1,C'7'                                                       
         BE    HOOK6                                                            
         CLI   QOPT1,C'D'                                                       
         BE    HOOK6                                                            
         LA    RE,4                                                             
         CLI   QOPT1,C'2'                                                       
         BE    HOOK6                                                            
         CLI   QOPT1,C'J'                                                       
         BE    HOOK6                                                            
*&&UK                                                                           
         LA    RE,6                                                             
         CLI   QOPT1,C'3'                                                       
         BE    HOOK6                                                            
*&&                                                                             
*&&US*&& LA    RE,2                                                             
*&&UK*&& LA    RE,7                                                             
         CLI   QOPT1,C'8'                                                       
         BE    HOOK6                                                            
         LA    RE,6                                                             
         CLI   QOPT1,C'4'                                                       
         BE    HOOK6                                                            
         LA    RE,3                                                             
         CLI   QOPT1,C'E'                                                       
         BE    HOOK6                                                            
         CLI   QOPT1,C'G'                                                       
         BE    HOOK6                                                            
         CLI   QOPT1,C'F'                                                       
         BNE   HOOK5                                                            
         LA    R1,SELECTF                                                       
         SR    RE,RE                                                            
HOOK4    CLI   0(R1),0                                                          
         BE    HOOK6                                                            
         AH    RE,=H'1'                                                         
         LA    R1,1(R1)                                                         
         B     HOOK4                                                            
*                                                                               
HOOK5    LA    RE,7                                                             
*                                                                               
HOOK6    MVI   0(RF),C'C'                                                       
         LA    RF,10(,RF)                                                       
         BCT   RE,HOOK6                                                         
         MVI   0(RF),C'R'          RIGHT BOX                                    
*                                                                               
         MVC   MYROW,SPACES                                                     
         MVI   MYROW+8,C'T'                                                     
         MVI   MYROW+11,C'M'                                                    
*&&US*&& MVI   MYROW+57,C'B'                                                    
*&&UK*&& MVI   MYROW+60,C'B'                                                    
         B     HOOKX                                                            
*                                                                               
HOOK10   CLI   MYBOXSW,2                                                        
         BNE   HOOKEX                                                           
         MVC   MYCOL,SPACES        BOXES FOR SPECIAL SUMMARY                    
         MVC   MYROW,SPACES                                                     
         MVI   MYCOL,C'L'                                                       
         MVI   MYCOL+109,C'R'                                                   
         LA    RE,6                                                             
         LA    RF,MYCOL+43                                                      
*                                                                               
HOOK12   MVI   0(RF),C'C'                                                       
         LA    RF,11(,RF)                                                       
         BCT   RE,HOOK12                                                        
*                                                                               
         MVI   MYROW+5,C'T'                                                     
         MVI   MYROW+8,C'M'                                                     
*&&US*&& MVI   MYROW+57,C'B'                                                    
*&&UK*&& MVI   MYROW+60,C'B'                                                    
         B     HOOKX                                                            
*                                                                               
HOOKX    MVC   BOXCOLS,MYCOL                                                    
         MVC   BOXROWS,MYROW                                                    
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
*                                                                               
HOOKEX   XMOD1 1                   EXIT - XIT NOT ALWAYS ADDRESSABLE            
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        CONSTANTS                                                              
*--------------------------------------------------------------------*          
RELOTAB  DS    0A                  A AND V TYPES                                
         DC    A(BUFFALOC)                                                      
         DC    A(REPCS)                                                         
         DC    A(LINTABC)                                                       
         DC    A(BUDC)                                                          
         DC    A(GRPTOT)                                                        
         DC    A(BINADDC)                                                       
         DC    V(BUDACC)                                                        
         DC    V(ACCDIV)                                                        
         DC    V(SQUASHER)                                                      
         DC    V(DLFLD)                                                         
         DC    A(COLTAB)                                                        
         DC    A(DLOAD)                                                         
         DC    A(TAXIT)                                                         
         DC    X'FF'                                                            
         SPACE 3                                                                
         DROP  R8,R9               KEEP IT CLEAN                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        LITERALS                                                               
*--------------------------------------------------------------------*          
         LTORG                                                                  
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        LOCAL FIELDS                                                           
*--------------------------------------------------------------------*          
*                                                                               
SVBUFKY  DS    CL2                                                              
SVBUFACS DS    CL64                                                             
BILTAB   DS    600C                                                             
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        TABLE FOR COLUMNS OF REPORT                                            
*        REPORT MENUS AND COLUMN LISTS                                          
*--------------------------------------------------------------------*          
MENUS    DC    C'1',AL1(1),CL3' '                                               
         DC    C'2',AL1(2),CL3' '                                               
         DC    C'3',AL1(3),CL3' '                                               
         DC    C'4',AL1(4),CL3' '                                               
         DC    C'5',AL1(5),CL3' '                                               
         DC    C'6',AL1(6),CL3' '                                               
         DC    C'7',AL1(7),CL3' '                                               
         DC    C'8',AL1(8),CL3' '                                               
         DC    C'9',AL1(9),CL3' '                                               
         DC    C'A',AL1(5,6,7),C' '     MULTI-REPORT MENUS                      
         DC    C'B',AL1(5,7),CL2' '                                             
         DC    C'C',AL1(6,7),CL2' '                                             
         DC    C'D',AL1(5,6),CL2' '                                             
         DC    C'E',AL1(10),CL3' '                                              
         DC    C'F',AL1(11),CL3' '                                              
         DC    C'G',AL1(12),CL3' '                                              
         DC    C'H',AL1(13),CL3' '                                              
         DC    C'I',AL1(14),CL3' '                                              
         DC    C'J',AL1(15),CL3' '                                              
         DC    X'FF'                                                            
*                                                                               
******************************************************************              
* WHEN ADDING CERTAIN COLUMNS YOU MUST ADD CONDITION FOR TYPEFIX *              
* ALSO DEPENDING ON THE NUMBER OF COLUMNS YOU MUST EDIT BOX HOOK *              
* DEFAULT IS 7 COLUMNS                                           *              
******************************************************************              
*                                                                               
SELECTAB DC    AL1(1,2,18,19,23,24,27,28)    NO 1      DEFAULT                  
         DC    AL1(1,23,40,41,0,0,0,0)       NO 2      DFS FORMAT               
*&&US*&& DC    AL1(1,42,23,25,43,40,37,0)    NO 3      FCB FORMAT               
*&&UK*&& DC    AL1(1,42,23,40,26,33,0,0)     NO 3      GREY FORMAT              
         DC    AL1(1,4,5,23,25,26,0,0)       NO 4                               
         DC    AL1(6,7,8,9,10,11,38,39)      NO 5      1ST HALF YR              
         DC    AL1(12,13,14,15,16,17,23,37)  NO 6      2ND HALF YR              
         DC    AL1(31,32,33,34,35,36,23,37)  NO 7      MOVING ANN. TOT          
*&&US*&& DC    AL1(23,44,0,0,0,0,0,0)        NO 8      BBDO FORMAT              
*&&UK*&& DC    AL1(1,51,4,23,44,25,43,0)     NO 8      LDS FORMAT               
         DC    AL1(18,42,23,25,43,40,37,0)   NO 9      TBC FORMAT               
         DC    AL1(1,23,45,0,0,0,0,0)        NO 10     QJ FORMAT                
SELECTF  DC    AL1(46,47,48,49,50,0,0,0)     NO 11     SCALI FORMAT             
         DC    AL1(1,23,42,0,0,0,0,0)        NO 12     GRIFFIN FORMAT           
         DC    AL1(6,7,8,9,10,11,38,0)       NO 13     SAATCHI FORMAT           
         DC    AL1(12,13,14,15,16,17,23,0)   NO 14     SAATCHI FORMAT           
         DC    AL1(1,23,42,37,0,0,0,0)       NO 15     FCB FORMAT               
*                                                                               
TTRFIRST DC    A(1)                                                             
         EJECT ,                                                                
COLTAB   DS    0CL25                                                            
         DC    C'A',XL4'00',CL10'  MONTH',CL10'THIS YEAR'          1            
         DC    C'A',XL4'00',CL10'  MONTH',CL10'LAST YEAR'          2            
         DC    C'A',XL4'00',CL10'  LAST',CL10'  MONTH'             3            
         DC    C'B',XL4'00',CL10'BUDGET',CL10'FOR MONTH'           4            
         DC    C'P',XL4'00',CL10'PCT ACT.',CL10'VS BUDGET'         5            
         DC    C'A',XL4'00',CL10'   JAN',CL10' '                   6            
         DC    C'A',XL4'00',CL10'   FEB',CL10' '                   7            
         DC    C'A',XL4'00',CL10'   MAR',CL10' '                   8            
         DC    C'A',XL4'00',CL10'   APR',CL10' '                   9            
         DC    C'A',XL4'00',CL10'   MAY',CL10' '                   10           
         DC    C'A',XL4'00',CL10'   JUN',CL10' '                   11           
         DC    C'A',XL4'00',CL10'   JUL',CL10' '                   12           
         DC    C'A',XL4'00',CL10'   AUG',CL10' '                   13           
         DC    C'A',XL4'00',CL10'   SEP',CL10' '                   14           
         DC    C'A',XL4'00',CL10'   OCT',CL10' '                   15           
         DC    C'A',XL4'00',CL10'   NOV',CL10' '                   16           
         DC    C'A',XL4'00',CL10'   DEC',CL10' '                   17           
         DC    C'A',XL4'00',CL10' QUARTER',CL10'THIS YEAR'         18           
         DC    C'A',XL4'00',CL10' QUARTER',CL10'LAST YEAR'         19           
         DC    C'A',XL4'00',CL10'  LAST',CL10'QUARTER'             20           
         DC    C'B',XL4'00',CL10' BUDGET',CL10' FOR QTR'           21           
         DC    C'P',XL4'00',CL10'PCT ACT.',CL10'VS BUDGET'         22           
         DC    C'A',XL4'00',CL10'   YTD',CL10'THIS YEAR'           23           
         DC    C'A',XL4'00',CL10'   YTD',CL10'LAST YEAR'           24           
         DC    C'B',XL4'00',CL10'   YTD',CL10'  BUDGET'            25           
         DC    C'P',XL4'00',CL10'PCT ACT.',CL10'VS BUDGET'         26           
         DC    C'A',XL4'00',CL10'12 MONTHS',CL10'THIS YEAR'        27           
         DC    C'A',XL4'00',CL10'12 MONTHS',CL10'LAST YEAR'        28           
         DC    C'B',XL4'00',CL10'12 MONTHS',CL10'  BUDGET'         29           
         DC    C'P',XL4'00',CL10'PCT ACT.',CL10'VS BUDGET'         30           
         DC    C'A',XL4'00',CL10'12 MONTHS',CL10' '                31           
         DC    C'A',XL4'00',CL10'12 MONTHS',CL10' '                32           
         DC    C'A',XL4'00',CL10'12 MONTHS',CL10' '                33           
         DC    C'A',XL4'00',CL10'12 MONTHS',CL10' '                34           
         DC    C'A',XL4'00',CL10'12 MONTHS',CL10' '                35           
         DC    C'A',XL4'00',CL10'12 MONTHS',CL10' '                36           
         DC    C'A',XL4'00',CL10'   YTD ',CL10'LAST YEAR'          37           
         DC    C'A',XL4'00',CL10'   YTD',CL10'THIS YEAR'           38           
         DC    C'A',XL4'00',CL10'   YTD',CL10'LAST YEAR'           39           
         DC    C'B',XL4'00',CL10' BUDGET',CL10' FOR YEAR'          40           
         DC    C'P',XL4'00',CL10'PCT ACT.',CL10'VS BUDGET'         41           
         DC    C'S',XL4'00',CL10'YTD AS PC',CL10'OF INCOME'        42           
         DC    C'V',XL4'00',CL10'YTD BUDG.',CL10' VARIANCE '       43           
         DC    C'S',XL4'00',CL10'YTD AS PC',CL10'OF BILL.'         44           
         DC    C'S',XL4'00',CL10'YTD PC OF',CL10'CAP SALES'        45           
COL46    DC    C'A',XL4'00',CL10' 1ST QTR  ',CL10'          '      46           
COL47    DC    C'A',XL4'00',CL10' 2ND QTR  ',CL10'          '      47           
COL48    DC    C'A',XL4'00',CL10' 3RD QTR  ',CL10'          '      48           
COL49    DC    C'A',XL4'00',CL10' 4TH QTR  ',CL10'          '      49           
COL50    DC    C'A',XL4'00',CL10' Y T D    ',CL10'          '      50           
COL51    DC    C'S',XL4'00',CL10'MTH AS PC',CL10'OF BILL.'         51           
         DC    C'A',XL4'00',CL10'   QTD',CL10'THIS YEAR'           52           
         DC    C'A',XL4'00',CL10'   QTD',CL10'LAST YEAR'           53           
MAXCOLS  EQU    (*-COLTAB)/COLLEN                                               
         DC    X'FF'                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        HEADLINE CONTROL FOR TWO PAGE PROBLEM                                  
*--------------------------------------------------------------------*          
MYREPORT NMOD1 0,*MYREP*                                                        
         L     RC,0(,R1)                                                        
         CLI   RCSUBPRG,3          NO PRINTING FOR DOWNLOADING                  
         BE    MYRPTEX                                                          
         CLI   MODE,REQLAST                                                     
         BE    MYRPT20                                                          
         L     R4,ADLDGHIR                                                      
*                                                                               
         USING ACHEIRD,R4                                                       
*                                                                               
         GOTO1 ACCDIV,DMCB,(R4),ADACC,LEVSAVE                                   
         MVC   WORK,SPACES                                                      
         ZIC   R5,HILEV                                                         
         LA    R2,ACHRDESA                                                      
         LA    R4,WORK                                                          
         LA    R3,LEVSAVE+1                                                     
         LA    R6,SAVEH1                                                        
         LA    RE,ADLVANAM                                                      
         CLI   MODE,ACCLAST                                                     
         BE    MYRPT2                                                           
         LA    R5,1                                                             
         CLI   MODE,LEVALAST                                                    
         BE    MYRPT2                                                           
         LA    R5,2                                                             
         CLI   MODE,LEVBLAST                                                    
         BE    MYRPT2                                                           
         LA    R5,3                                                             
*                                                                               
MYRPT2   DS    0H                                                               
         L     R7,0(,RE)                                                        
         MVC   0(15,R6),0(R2)      NAME OF LEVEL                                
         MVC   0(12,R4),0(R3)                                                   
         MVC   17(12,R6),0(R3)     CODE                                         
         MVC   30(36,R6),SPACES                                                 
         ZIC   RF,1(,R7)                                                        
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   30(0,R6),2(R7)      NAME                                         
         MVC   SUMNAME,SPACES                                                   
         MVC   SUMNAME,30(R6)                                                   
         LA    R2,16(,R2)                                                       
         LA    R3,13(,R3)                                                       
         LA    R4,12(,R4)                                                       
         LA    R6,80(,R6)                                                       
         LA    RE,20(,RE)                                                       
         BCT   R5,MYRPT2                                                        
         GOTO1 SQUASHER,DMCB,WORK,60                                            
         MVC   SUMNUM,WORK                                                      
*                                                                               
MYRPT20  LA    R5,4                                                             
         LA    R6,SAVEH1                                                        
*                                                                               
MYRPT22  DS    0H                                                               
         GOTO1 SQUASHER,DMCB,0(R6),80                                           
         LA    R6,80(,R6)                                                       
         BCT   R5,MYRPT22                                                       
*                                                                               
         LA    R5,4                                                             
         LA    R2,HEAD5+1                                                       
         LA    R6,SAVEH1                                                        
*                                                                               
MYRPT30  CLC   0(80,R6),SPACES                                                  
         BE    MYRPT32                                                          
         MVC   0(80,R2),0(R6)                                                   
         MVC   0(80,R6),SPACES                                                  
         LA    R2,132(,R2)                                                      
         LA    R6,80(,R6)                                                       
         BCT   R5,MYRPT30                                                       
*                                                                               
MYRPT32  DS    0H                                                               
         LA    R2,HEAD10                                                        
*                                                                               
MYRPT34  MVC   0(132,R2),SAVEH5                                                 
         MVI   1(R2),0                                                          
         MVC   132(132,R2),SAVEH6                                               
         MVI   133(R2),0                                                        
         CLI   QOPT1,C'2'          WANT BUDGET NAME ONLY IF                     
         BL    MYRPT36             IT IS A BUDGET REPORT                        
         CLI   QOPT1,C'4'                                                       
         BH    MYRPT36                                                          
         MVC   HEAD6+87(10),BUDNAME                                             
*                                                                               
MYRPT36  CLI   QOPT4,C'Y'          SKIP IF THEY ONLY WANT SUMMARY               
         BE    MYRPTEX                                                          
         MVC   HEAD5+87(23),HDAT                                                
         MVI   MYROW+8,C'T'                                                     
         MVI   MYROW+11,C'M'                                                    
         MVI   MYROW+57,C'B'                                                    
         L     R4,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,R4                                                      
*                                                                               
         L     R4,ADMASTD                                                       
*                                                                               
         USING MASTD,R4                                                         
*                                                                               
         L     R4,MCBXAREA                                                      
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
         MVC   BOXROWS,MYROW                                                    
         MVI   BOXINIT,0                                                        
         GOTO1 ACREPORT                                                         
         MVC   MYROW,SPACES                                                     
*                                                                               
MYRPTEX  XMOD1 1                   RETURN                                       
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*       INITIALIZE DATE COLUMNS                                                 
*---------------------------------------------------------------------*         
*                                                                               
         USING COLTABD,R7          YEARLY, QUARTERLY ETC FIELDS                 
*                                                                               
         DS    0D                                                               
INITDATE NMOD1 0,*INTDAT*                                                       
         L     RC,0(,R1)                                                        
         L     R7,ACOLTAB          FILL IN   THE  MONTHS    FOR                 
*                                  YEARLY,   QUARTERLY,     ETC  FIELDS         
         SR    R3,R3               IN   THE  COLUMN    TABLE                    
         XC    HALF,HALF                                                        
         GOTO1 DATCON,DMCB,(0,QSTART),(0,SDATEC)                                
         GOTO1 DATCON,DMCB,(0,QEND),(0,EDATEC)                                  
         MVC   STEND(2),SDATE      START/END IN   ONE  FIELD                    
         MVC   STEND+2(2),DATE                                                  
*                                  **** END  DATE                               
*                                  **** FOR  ONE  MONTH               1         
         MVC   COLSTRT,DATE        MONTH     THIS YEAR                          
         MVC   COLEND,DATE                                                      
*                                                                               
*                                  **** END  DATE MINUS     ONE  YEAR 2         
*                                  **** FOR  ONE  MONTH                         
         LA    R7,L'COLTAB(,R7)                                                 
         GOTO1 ADDAY,DMCB,(C'Y',EDATEC),WORK,F'-1'                              
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   COLSTRT,WORK+6                                                   
         MVC   COLEND,COLSTRT                                                   
*                                                                               
*                                  **** END  DATE MINUS     ONE  MON  3         
*                                  **** FOR  ONE  MONTH                         
         LA    R7,L'COLTAB(,R7)                                                 
         GOTO1 ADDAY,DMCB,(C'M',EDATEC),WORK,F'-1'                              
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   COLSTRT,WORK+6                                                   
         MVC   COLEND,COLSTRT                                                   
*                                                                               
*                                  **** BUDGET    END  DATE           4         
*                                  **** FOR  ONE  MONTH                         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT,DATE                                                     
         MVC   COLEND,DATE                                                      
*                                                                               
*                                  **** %    ACT  VS   BUDGET         5         
*                                  **** END  DATE FOR  ONE  MONTH               
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT,DATE                                                     
         MVC   COLEND,DATE                                                      
*                                                                               
*                                  **** START     DATE TO   END  DATE           
*                                  **** FOR  12   MONTHS    MAX    6-17         
         LA    R6,12               12   MONTHS                                  
         MVC   WORK(6),SDATEC      START     DATE                               
         MVC   WORK+6,SDATE        START     DATE PACKED                        
*                                                                               
PR1B     LA    R7,L'COLTAB(,R7)                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(6,WORK+9)         MMM/YY                   
         MVC   COLHEAD1+3(3),WORK+9                    SAVE MMM                 
         MVC   COLSTRT,WORK+6      SAVE ONE       MONTH    (PACKED)             
         MVC   COLEND,COLSTRT                                                   
         CLC   COLSTRT,DATE        THIS MONTH     PAST END  DATE ?              
         BNH   *+8                 NO,  DATE IS   OKAY                          
*                                  YES, DO   NOT  SHOW ACTIVITY AFTER           
         MVI   COLSTRT,X'FF'            END  DATE                               
*                                                                               
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK,F'1'        NEXT MONTH               
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)         PACKED                   
         CH    R6,=H'8'            6TH  MONTH ?                                 
         BNE   PR1H                NO,  CONTINUE                                
         MVC   SVEND1,WORK+6       SAVE END  OF   1ST  SIX  MONTHS              
         CLC   SVEND1,DATE         OR   END  DATE                               
         BNH   PR1H                WHICHEVER IS   LOWER                         
         MVC   SVEND1,DATE                                                      
*                                                                               
PR1H     DS    0H                                                               
         BCT   R6,PR1B                                                          
*                                                                               
*                                  **** END  DATE QUARTER            18         
*                                  **** FOR  ONE  QUARTER   MAX                 
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLEND,DATE         END  DATE                                    
         MVC   COLSTRT,DATE        FIND QUARTER   THIS YEAR                     
         MVI   BYTE,C'1'                                                        
         CLI   DATE+1,X'04'        IN   1ST  QUARTER ?                          
         BL    PR2                 YES, PROCESS   1ST  QUARTER                  
         MVI   BYTE,C'2'                                                        
         CLI   DATE+1,X'07'        IN   2ND  QUARTER ?                          
         BL    PR4                 YES, PROCESS   2ND  QUARTER                  
         MVI   BYTE,C'3'                                                        
         CLI   DATE+1,X'10'        IN   3RD  QUARTER ?                          
         BL    PR6                 YES, PROCESS   3RD  QUARTER                  
         MVI   BYTE,C'4'           MUST BE   4TH  QUARTER                       
         MVI   COLSTRT+1,X'10'     MONTHS    10   THRU 12                       
         MVI   COLEND+1,X'12'                                                   
         B     PR8                                                              
*                                                                               
PR2      MVI   COLSTRT+1,X'01'     1ST  QUARTER                                 
         MVI   COLEND+1,X'03'      MONTHS    1    THRU 3                        
         B     PR8                                                              
*                                                                               
PR4      MVI   COLSTRT+1,X'04'     2ND  QUARTER                                 
         MVI   COLEND+1,X'06'      MONTHS    4    THRU 6                        
         B     PR8                                                              
*                                                                               
PR6      MVI   COLSTRT+1,X'07'     3RD  QUARTER                                 
         MVI   COLEND+1,X'09'      MONTHS    7    THRU 9                        
*                                                                               
PR8      MVC   WORD,COLSTRT        SAVE START     AND  END  FOR  LATER          
*                                                                               
         MVC   COLEND+1(1),DATE+1  LIMIT     END  QTR  BY   END  DATE           
*                                                                               
*                                  **** END  DATE QUARTER   LAST YEAR           
*                                  **** FOR  ONE  QUARTER            19         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   WORK(2),WORD        GET  END  DATE QTR  START                    
         MVI   WORK+2,X'01'        DAY  01                                      
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)         CHARACTER DATE           
         GOTO1 ADDAY,DMCB,(C'Y',WORK+3),WORK+9,F'-1'   MINUS     1 YEAR         
         GOTO1 DATCON,DMCB,(0,WORK+9),(1,WORK)         PACKED    DATE           
         MVC   COLSTRT,WORK                                                     
*                                                                               
         MVC   WORK(2),WORD+2      GET  END  DATE QTR  END                      
         MVI   WORK+2,X'01'        DAY  01                                      
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)         CHARACTER DATE           
         GOTO1 ADDAY,DMCB,(C'Y',WORK+3),WORK+9,F'-1'   MINUS     1 YEAR         
         GOTO1 DATCON,DMCB,(0,WORK+9),(1,WORK)         PACKED    DATE           
         MVC   COLEND,WORK                                                      
*                                                                               
*                                  **** END  DATE PREVIOUS  QUARTER  20         
*                                  **** FOR  ONE  QUARTER                       
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   WORK(2),WORD        GET  END  DATE QTR  START                    
         MVI   WORK+2,X'01'        DAY  01                                      
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)         CHARACTER DATE           
         GOTO1 ADDAY,DMCB,(C'M',WORK+3),WORK+9,F'-3'   MINUS     3 MON          
         GOTO1 DATCON,DMCB,(0,WORK+9),(1,WORK)         PACKED    DATE           
         MVC   COLSTRT,WORK                                                     
         MVC   WORK(2),WORD+2      GET  END  DATE QTR  END                      
*        MVI   WORK+2,X'01'        DAY  01                                      
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)         CHARACTER DATE           
         GOTO1 ADDAY,DMCB,(C'M',WORK+3),WORK+9,F'-3'   MINUS     3 MON          
         GOTO1 DATCON,DMCB,(0,WORK+9),(1,WORK)         PACKED    DATE           
         MVC   COLEND,WORK                                                      
*                                                                               
*                                  **** BUDGET    FOR  END  DATE QTR            
*                                  **** FOR  ONE  QUARTER            21         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT(4),WORD                                                  
*                                                                               
*                                  **** %    ACT  VS   BUDGET        22         
*                                  **** END  DATE QUARTER                       
*                                  **** FOR  ONE  QUARTER                       
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT(4),WORD                                                  
*                                                                               
*                                  **** START     DATE TO   END  DATE           
*                                  **** FOR  ONE  YEAR MAX           23         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT(4),STEND                                                 
*                                                                               
*                                  **** START     DATE TO   END  DATE           
*                                  **** PREVIOUS  YEAR                          
*                                  **** FOR  ONE  YEAR MAX           24         
         LA    R7,L'COLTAB(,R7)                                                 
*                                                      START     DATE           
         GOTO1 ADDAY,DMCB,(C'Y',SDATEC),WORK,F'-1'     MINUS     1 YEAR         
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)         PACKED    DATE           
         MVC   COLSTRT,WORK+6                                                   
*                                                                               
*                                                      END       DATE           
         GOTO1 ADDAY,DMCB,(C'Y',EDATEC),WORK,F'-1'     MINUS     1 YEAR         
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)         PACKED    DATE           
         MVC   COLEND,WORK+6                                                    
*                                                                               
*                                  **** YTD  BUDGET                             
*                                  **** START     DATE TO   END  DATE           
*                                  **** FOR  ONE  YEAR  MAX          25         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT(4),STEND                                                 
*                                                                               
*                                  **** YTD  %    ACT  VS   BUDGET              
*                                  **** START     DATE TO   END  DATE           
*                                  **** FOR  ONE  YEAR MAX           26         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT(4),STEND                                                 
*                                                                               
*                                  **** YEAR PRIOR     TO   END  DATE           
*                                  **** FOR  ONE  YEAR               27         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLEND,DATE                                                      
*                                                      END       DATE           
         GOTO1 ADDAY,DMCB,(C'M',EDATEC),WORK,F'-11'    MINUS     11 MON         
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)         PACKED    DATE           
         MVC   COLSTRT,WORK+6                                                   
*                                                                               
         MVC   WORD,COLSTRT        SAVE 12   MONTHS    THRU END  DATE           
*                                                                               
*                                  **** YEAR BEFORE    THE                      
*                                  **** YEAR PRIOR     TO   END  DATE           
*                                  **** FOR  ONE  YEAR               28         
         LA    R7,L'COLTAB(,R7)                                                 
*                                                      END       DATE           
         GOTO1 ADDAY,DMCB,(C'Y',EDATEC),WORK,F'-1'     MINUS     1  YR          
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)         PACKED    DATE           
         MVC   COLEND,WORK+6                                                    
*                                                                               
*                                                      END  DATE - 1 YR         
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,F'-11'    MINUS     11 MON         
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK+12)      PACKED    DATE           
         MVC   COLSTRT,WORK+12                                                  
*                                                                               
*                                  **** TWICE                                   
*                                  **** YEAR PRIOR     TO   END  DATE           
*                                  **** FOR  ONE  YEAR            29,30         
         LA    R4,2                TWICE                                        
*                                                                               
PR14     LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT(4),WORD                                                  
         BCT   R4,PR14                                                          
*                                                                               
*                                  **** MOVING    ANNUAL    TOTAL               
*                                  **** FOR  LAST SIX  MONTHS                   
*                                  **** TO   END  DATE            31-36         
*                                                      END       DATE           
         GOTO1 ADDAY,DMCB,(C'M',EDATEC),WORK,F'-16'    MINUS     16 MON         
*                                                      END       DATE           
         GOTO1 ADDAY,DMCB,(C'M',EDATEC),WORK+6,F'-5'   MINUS      5 MON         
         LA    R6,6                FOR  SIX  MONTH                              
*                                                                               
PR18     LA    R7,L'COLTAB(,R7)                                                 
         GOTO1 DATCON,DMCB,(0,WORK+6),(6,WORK+12)      TO   DATE MMM/YY         
         MVC   COLHEAD2+3(3),WORK+12                   SAVE MMM                 
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+18)        FROM DATE PACKED         
         MVC   COLSTRT,WORK+18                         SAVE FROM DATE           
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK+21)      TO   DATE PACKED         
         MVC   COLEND,WORK+21                          SAVE TO   DATE           
*                                                                               
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK,F'1'        NEXT FROM MONTH          
         GOTO1 ADDAY,DMCB,(C'M',WORK+6),WORK+6,F'1'    NEXT TO   MONTH          
         BCT   R6,PR18                                                          
*                                                                               
*                                  **** START     DATE TO   END DATE            
*                                  **** BUT  YEAR PRIOR              37         
*                                  **** FOR  ONE  YEAR MAX                      
         LA    R7,L'COLTAB(,R7)                                                 
*                                                      START     DATE           
         GOTO1 ADDAY,DMCB,(C'Y',SDATEC),WORK,F'-1'     MINUS     1 YEAR         
*                                                      END       DATE           
         GOTO1 ADDAY,DMCB,(C'Y',EDATEC),WORK+6,F'-1'   MINUS     1 YEAR         
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+18)        FROM DATE PACKED         
         MVC   COLSTRT,WORK+18                         SAVE FROM DATE           
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK+21)      TO   DATE PACKED         
         MVC   COLEND,WORK+21                          SAVE TO   DATE           
*                                                                               
*                                  **** START     DATE TO                       
*                                  **** START     DATE PLUS FIVE MONTH          
*                                  **** FOR  SIX  MONTHS    MAX      38         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT,SDATE                                                    
         MVC   COLEND,SVEND1                                                    
*                                                                               
*                                  **** START     DATE TO                       
*                                  **** START     DATE PLUS FIVE MONTH          
*                                  **** BUT  PRIOR     YEAR                     
*                                  **** FOR  SIX  MONTHS    MAX      39         
         LA    R7,L'COLTAB(,R7)                                                 
*                                                      FROM COL  37             
         MVC   COLSTRT,WORK+18                         SAVE FROM DATE           
         MVC   WORK(2),SVEND1                          TO   DATE                
         MVI   WORK+2,X'01'                            DAY  01                  
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+6)         CHAR TO   DATE           
         GOTO1 ADDAY,DMCB,(C'Y',WORK+6),WORK+6,F'-1'   MINUS     1 YEAR         
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK)         TO   DATE PACKED         
         MVC   COLEND,WORK                             SAVE TO   DATE           
*                                                                               
*                                  **** 12   MONTH     BUDGET                   
*                                  **** FROM START     DATE          40         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT,SDATE                           START     DATE           
         GOTO1 ADDAY,DMCB,(C'M',SDATEC),WORK,F'11'     PLUS 11   MONTHS         
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)         TO   DATE PACKED         
         MVC   COLEND,WORK+6                           SAVE TO   DATE           
*                                                                               
*                                  **** YEAR TO   DATE                          
*                                  **** START     DATE TO   END  DATE           
*                                  **** FOR  ONE  YEAR MAX           41         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT(4),STEND                                                 
*                                                                               
*                                  **** YEAR TO   DATE THIS YEAR                
*                                  **** START     DATE TO   END  DATE           
*                                  **** FOR  ONE  YEAR MAX           42         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT(4),STEND                                                 
*                                                                               
*                                  **** YEAR TO   DATE THIS YEAR                
*                                  **** START     DATE TO   END  DATE           
*                                  **** FOR  ONE  YEAR MAX           43         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT(4),STEND                                                 
*                                                                               
*                                  **** YEAR TO   DATE                          
*                                  **** START     DATE TO   END  DATE           
*                                  **** FOR  ONE  YEAR MAX           44         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT(4),STEND                                                 
*                                                                               
*                                  **** YEAR TO   DATE                          
*                                  **** START     DATE TO   END  DATE           
*                                  **** FOR  ONE  YEAR MAX           45         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLSTRT(4),STEND                                                 
*                                                                               
*                                  **** QUARTERS  STARTING  FROM                
*                                  **** FROM START     DATE                     
*                                  **** FOUR QUARTERS  MAX        46-49         
*        BUILD QUARTERLY DATES                                                  
*                                                                               
         XC    QTRS,QTRS                                                        
         MVC   WORK(6),SDATEC      START     DATE CHARACTER                     
         MVC   WORK+6(2),SDATE     START     DATE PACKED                        
         LA    R2,QTRS                                                          
         LA    R0,4                                                             
*                                                                               
PR19C    MVC   0(2,R2),WORK+6      START     MONTH     OF   QUARTER             
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+9,F'2'      END  MON  OF QTR         
         GOTO1 DATCON,DMCB,(0,WORK+9),(1,WORK+6)       END  MON  PACKED         
         MVC   2(2,R2),WORK+6                          SAVE END  MONTH          
         CLC   2(2,R2),STEND+2     END  MON  >=   END  DATE ?                   
         BNL   PR19F               YES, DONE                                    
         GOTO1 ADDAY,DMCB,(C'M',WORK+9),WORK,F'1'      NEXT QTR  START          
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)         START     PACKED         
         LA    R2,4(,R2)                               NEXT QTR  FIELD          
         BCT   R0,PR19C                                SAVE NEXT QTR            
*                                                                               
PR19F    MVC   2(2,R2),STEND+2     LAST MONTH     =    END  DATE                
         L     R1,=A(SELECTF)                                                   
         A     R1,RELO                                                          
         XC    0(8,R1),0(R1)       BUILD     SOFT COLHEADS                      
         LA    R3,QTRS                                                          
         LA    R0,46                                                            
*                                                                               
PR19G    OC    0(4,R3),0(R3)       END  OF   REQUESTED QUARTERS ?               
         BZ    PR19H               YES, SKIP                                    
         LA    R7,L'COLTAB(,R7)    QUARTERLY COLUMN                             
         MVC   COLSTRT(4),0(R3)    SAVE START     AND  END  DATES               
         STC   R0,0(,R1)           SAVE COLUMN    NUMBER                        
         LA    R1,1(,R1)           UPDATE    FOR  NEXT COLUMN                   
         AH    R0,=H'1'            "                                            
         CH    R0,=H'50'           FINISHED  FOUR COLUMNS ?                     
         BE    PR19H               YES, FINISHED  QUARTERS                      
         LA    R3,4(,R3)           TRY  NEXT QUARTER                            
         B     PR19G                                                            
*                                                                               
*                                  **** YEAR TO   DATE                          
*                                  **** START     DATE TO   END  DATE           
*                                  **** FOR  ONE  YEAR MAX           50         
PR19H    MVI   0(R1),50            LAST COLUMN    IS   YTD                      
         L     R7,=A(COL50)                                                     
         A     R7,RELO                                                          
         MVC   COLSTRT(4),STEND    SAVE START     AND  END  DATES               
*                                                                               
*        ****  AGENCY OPTION TO REPLACE SOME LINE DESCRIPTIONS                  
*                                                                               
         USING REPTABD,R3                                                       
*                                                                               
         L     R3,ADREPTAB                                                      
*                                                                               
REP1     CLI   0(R3),X'FF'                                                      
         BE    REP9                                                             
         CLC   RLAPLHA,ALPHAID                                                  
         BE    REP3                                                             
*                                                                               
REP2     LA    R3,REPLEN(,R3)                                                   
         B     REP1                                                             
*                                                                               
         USING LINTABD,R7                                                       
*                                                                               
REP3     SR    R7,R7                                                            
         ICM   R7,7,RLINADD                                                     
         MVC   LINHEAD,RLINHEAD                                                 
         B     REP2                                                             
*                                                                               
         DROP  R3,R7               KEEP IT   CLEAN                              
*                                                                               
         USING COLTABD,R7                                                       
*                                                                               
*                                  **** END  DATE                               
*                                  **** FOR  ONE  MONTH              51         
REP9     L     R7,=A(COL51)                                                     
         A     R7,RELO                                                          
         MVC   COLSTRT,DATE                                                     
         MVC   COLEND,DATE                                                      
*                                                                               
*                                  **** LAST QUARTER                            
*                                  **** FOR  END  DATE                          
*                                  **** FOR  ONE  QUARTER   MAX      52         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLEND,DATE         STOP      QTR  AT   END  DATE                
         MVC   COLSTRT,DATE        START     QTR  AT   END  DATE                
         ZAP   DUB,=P'0'           CALCULATE WHICH     QUARTER                  
         MVC   DUB+6(1),DATE+1     END  DATE MONTH     X    TEN                 
         SRP   DUB,63,0            END  DATE MONTH                              
         SP    DUB,=P'1'           MINUS     ONE  MONTH                         
         DP    DUB+3(5),=PL3'3'    ((END MONTH)/  3),  I.E. 0-3                 
         ZAP   DUB,DUB(5)          USE  ALL  OF   DUB                           
         MP    DUB,=P'3'           TIMES     3,   I.E. 0,3,6,9                  
         AP    DUB,=P'1'           ADD  ONE  TO   GET  CORRECT   QTR            
*                                       I.E. MON= JAN, APR, JUL, OCT            
         SRP   DUB,1,0             MONTH     X    TEN                           
         MVC   COLSTRT+1(1),DUB+6  INSERT    MONTH     WITHOUT   SIGN           
*                                                                               
*                                  **** LAST YEAR'S                             
*                                  **** LAST QUARTER                            
*                                  **** FOR  END  DATE                          
*                                  **** FOR  ONE  QUARTER   MAX      53         
         LA    R7,L'COLTAB(,R7)                                                 
         MVC   COLEND,DATE                                                      
         MVC   COLSTRT,DATE                                                     
         MVC   COLSTRT+1(1),DUB+6  SAME QUARTER   AS   COL. 52                  
         GOTO1 ADDAY,DMCB,(C'Y',EDATEC),WORK,F'-1'     LAST YEAR CHAR           
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)         LAST YEAR PACKED         
         MVC   COLSTRT(1),WORK+6                       SAVE YEAR START          
         MVC   COLEND(1),WORK+6                        SAVE YEAR END            
         XMOD1 1                   RETURN                                       
*                                                                               
         DROP  R7                  KEEP IT   CLEAN                              
*                                                                               
         LTORG                                                                  
*                                                                               
SDATEC   DS    CL6                 START DATE CHARACTER                         
EDATEC   DS    CL6                 END   DATE CHARACTER                         
STEND    DS    PL4                 START DATE AND  END  DATE PACKED             
SVEND1   DS    PL2                 END   OF   1ST  SIX  MONTHS    OR            
*                                        END  DATE WHICHEVER IS   LOWER         
QTRS     DS    PL16                FOUR  QTRS START/END DATES  YYMMYYMM         
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*                                                                               
*--------------------------------------------------------------------*          
         DS    0D                                                               
TAXIT    NMOD1 0,*TAXIT*                                                        
         L     RC,0(,R1)                                                        
         MVC   PLUS8,PROFIT                                                     
         LA    RE,PROFIT           POINT TO CORRECT COLUMN TO SEE               
         CLI   BUFKEY,X'05'        IF WE MADE PROFIT OR LOSS THIS               
         BL    TX2                 PERIOD FOR THIS CLIENT                       
         CLI   BUFKEY,X'08'                                                     
         BE    TX2                                                              
         CLI   BUFKEY,10                                                        
         BE    TX3                                                              
         LA    RE,PROFIT+48                                                     
*                                                                               
TX2      CP    0(8,RE),=P'0'                                                    
         BNH   TX80                                                             
*                                                                               
TX3      L     R6,MYWORD           A(SELECTAB)                                  
         LA    R2,PLUS8                                                         
         LA    RF,8                                                             
*                                                                               
TX10     DS    0H                                                               
         SR    R1,R1               GO FROM SELECTAB ENTRY TO COLTAB             
         IC    R1,0(,R6)                                                        
         BCTR  R1,0                                                             
         MH    R1,=H'25'                                                        
         L     R7,ACOLTAB                                                       
         AR    R7,R1                                                            
         CLI   0(R7),C'A'          AND SEE IF WE SHOULD PRINT                   
         BE    TX12                                                             
         CLI   0(R7),C'S'                                                       
         BNE   TX40                                                             
*                                                                               
TX12     ZIC   R1,PROGPROF+15                                                   
         CVD   R1,DUB                                                           
         ZAP   DIV,DUB                                                          
         MP    DIV,0(8,R2)                                                      
         DP    DIV,=P'100'                                                      
         SP    0(8,R2),DIV(12)     SUBTRACT TAX FROM PROFIT                     
         B     TX50                                                             
*                                                                               
TX40     ZAP   0(8,R2),=P'0'                                                    
         B     TX50                                                             
*                                                                               
TX50     LA    R2,8(,R2)                                                        
         LA    R6,1(,R6)                                                        
         BCT   RF,TX10                                                          
*                                                                               
TX80     LA    RE,P+30                                                          
         LA    R2,PLUS8                                                         
         LA    RF,8                                                             
*                                                                               
TX82     ZAP   DUB,0(8,R2)                                                      
         CP    DUB,=P'0'                                                        
         BE    TX84                                                             
         CLI   BUFKEY,10                                                        
         BNE   *+12                                                             
         CH    RF,=H'6'                                                         
         BE    TX83                THIRD COL OF QJBO IS PERCENT                 
         CLI   BUFKEY,X'08'                                                     
         BNE   TX82A                                                            
         CH    RF,=H'7'            SECOND COL OF BBDO                           
         BE    TX83                REPORT IS A FUNNY                            
*                                                                               
TX82A    AP    DUB,=P'50'                                                       
         CP    DUB,=P'0'                                                        
         BNL   *+10                                                             
         SP    DUB,=P'100'                                                      
         DP    DUB,=P'100'                                                      
         ZAP   DOUBLE,DUB(6)                                                    
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   TX82F                                                            
         CP    DOUBLE(8),=P'0'                                                  
         BNL   TX82F                                                            
         ST    R2,FULL                                                          
         LR    R2,RE                                                            
         ZAP   DUB3,DOUBLE(8)                                                   
         ST    RE,FULL2                                                         
         EDIT  (P8,DUB3),(9,0(R2)),BRACKET=YES                                  
         L     RE,FULL2                                                         
         L     R2,FULL                                                          
         B     TX84                                                             
*                                                                               
TX82F    EDIT  (P8,DOUBLE),(10,(RE)),MINUS=YES                                  
         B     TX84                                                             
*                                                                               
TX83     ZAP   DIV,DUB                                                          
         MP    DIV,=P'10000'                                                    
         CP    TOTREV,=P'0'                                                     
         BNH   TX84                                                             
         DP    DIV,TOTREV+2(6)                                                  
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   TX83F                                                            
         CP    DIV(8),=P'0'                                                     
         BNL   TX83F                                                            
         ST    R2,FULL                                                          
         LR    R2,RE                                                            
         ZAP   DUB3,DIV(8)                                                      
         ST    RE,FULL2                                                         
         EDIT  (P8,DUB3),(9,0(R2)),BRACKET=YES                                  
         L     RE,FULL2                                                         
         L     R2,FULL                                                          
         B     TX84                                                             
*                                                                               
TX83F    EDIT  (P8,DIV),(10,(RE)),2,MINUS=YES                                   
*                                                                               
TX84     LA    R2,8(,R2)                                                        
         LA    RE,10(,RE)                                                       
         BCT   RF,TX82                                                          
*                                                                               
         CLC   P+30(80),SPACES                                                  
         BE    TAXXIT                                                           
         MVC   P+1(15),=C'POST TAX PROFIT'                                      
         CLI   QOPT1,C'E'                                                       
         BNE   *+10                                                             
         MVC   P+1(15),=C'NET PROFIT/LOSS'                                      
         GOTO1 =A(MYREPORT),DMCB,(RC),(RA),RR=RELO                              
*        B     TAXXIT                                                           
*                                                                               
TAXXIT   XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        FIND BUDGET ELEMENTS AND SAVE                                          
*--------------------------------------------------------------------*          
         DS    0D                                                               
BUDGET   NMOD1 0,*BUDGET*                                                       
         L     RC,0(,R1)                                                        
         LA    RF,MAXCOLS                                                       
         LA    RE,PLUS                                                          
*                                                                               
B01A     ZAP   0(6,RE),=P'0'                                                    
         LA    RE,6(,RE)                                                        
         BCT   RF,B01A                                                          
         L     R3,ABUDC                                                         
*                                                                               
         USING BUDACCD,R3                                                       
*                                                                               
         MVC   BUSERKEY(15),BUDKEY                                              
         MVI   BUCMND,AMNTGET                                                   
         MVC   SAVEKEY1,SPACES                                                  
         MVC   SAVEKEY1(L'KEY),KEY                                              
*                                                                               
B01      GOTO1 BUDACC,DMCB,ABUDC                                                
*                                  ************** WARNING *************         
*                                  * THIS PROGRAM MAY LOOP BECAUSE OF *         
*                                  * AN ERROR IN ACBUDACC SINCE       *         
*                                  * BULSTKEY IS DEFINED ONE BYTE TOO *         
*                                  * SHORT IN ACBUDACCD !             *         
*                                  *                                  *         
*                                  * ACBUDACCD HAS NOT YET BEEN       *         
*                                  * UPDATED TO ACCOUNT FOR THE       *         
*                                  *   BUCKET TYPE FIELD              *         
*                                  * (SEE BUDKBTYP IN ACGENFILE).     *         
*                                  ************************************         
         CLI   BUMODE,FINISHED                                                  
         BE    BEND                                                             
         CLI   BUMODE,DMGRERR                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BUSERKEY(15),BULSTKEY+1                                          
         BNE   BEND                                                             
         MVI   BUCMND,AMNTNEXT                                                  
         CLI   BUMODE,PROCAMNT                                                  
         BNE   B01                                                              
         LA    R4,BUAMTREC         POINT TO BUDGET AMOUNT RECORD                
*                                                                               
         USING ACBTKEY,R4                                                       
*                                                                               
         CLC   ACBTKCON,SPACES     IGNORE IF 'ALL'                              
         BE    B01                                                              
         CLI   QSRTAREA,X'40'                                                   
         BE    B02                                                              
         CLI   QSRTAREA,X'00'                                                   
         BE    B02                                                              
         CLC   QSRTAREA(1),ACBTKBNO+1                                           
         BNE   B01                                                              
*                                                                               
B02      MVC   BUDCON,ACBTKCON+2   SAVE LEDGER/ACCT FROM BUDGET CONTRA          
*                                                                               
         CLI   PROGPROF+14,C'O'      COMBINE 15 AND 16 AS OVERHEAD              
         BNE   B02A                                                             
         CLI   BUDCON,C'5'        COMPRESS 15 AND 16 FOR NEW COST               
         BNE   B02A                                                             
         MVI   BUDCON+1,C'A'                                                    
         B     B02B                                                             
*                                                                               
B02A     CLI   BUDCON,C'6'        COMPRESS 16 FOR NEW COST                      
         BNE   *+8                                                              
         MVI   BUDCON+1,C'A'                                                    
*                                                                               
B02B     MVC   BUDCSV,BUDCON                                                    
         AH    R4,DATADISP                                                      
*                                                                               
B03      CLI   0(R4),0                                                          
         BE    B01                                                              
         CLI   0(R4),X'1D'                                                      
         BE    B07                                                              
*                                                                               
B05      ZIC   RF,1(,R4)                                                        
         AR    R4,RF                                                            
         B     B03                                                              
*                                                                               
         USING ACBAD,R4                                                         
*                                                                               
B07      ZAP   DUB,ACBABUDG        SAVE BUDGET VALUES                           
         MVC   BDATE,ACBAMNTH                                                   
*                                                                               
B09      L     R6,ADLINTAB                                                      
*                                                                               
         USING LINTABD,R6                                                       
*                                                                               
B11      CLC   LINKEY,BUDCON       MATCH TABLE LEDGER/ACCT WITH BUDGET          
         BE    B13                 CONTRA-ACCOUNT                               
         LA    R6,L'LINTAB(,R6)                                                 
         CLI   0(R6),X'FF'         NO MATCH - GET ANOTHER RECORD                
         BNE   B11                                                              
         L     R6,ADLINTAB                                                      
         B     B01                                                              
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
B13      ST    R6,SVADDR           SAVE TABLE ADDRESS FOR TOTAL ACCUMS          
         L     R5,=A(COLTAB)                                                    
*                                                                               
         MVC   BUFKEY+1(3),LINID                                                
         LA    RF,PLUS                                                          
*                                                                               
B15      CLI   0(R5),X'FF'                                                      
         BE    B30                                                              
         CLI   0(R5),C'B'          BUDGET COLUMN                                
         BE    B19                                                              
*                                                                               
B17      LA    RF,6(,RF)                                                        
*                                                                               
B18      LA    R5,L'COLTAB(,R5)                                                 
         B     B15                                                              
*                                                                               
B19      CLC   BDATE,COLSTRT                                                    
         BL    B17                                                              
         CLC   BDATE,COLEND                                                     
         BH    B17                                                              
         ZAP   0(6,RF),DUB                                                      
         B     B17                                                              
*                                                                               
B30      MVI   WORK+4,C' '                                                      
         MVC   WORK(4),MENUSV                                                   
*                                                                               
B32      MVC   BUFKEY(1),WORK                                                   
         MVC   BUFACCS,=8PL8'0'                                                 
         SR    R7,R7                                                            
         IC    R7,WORK                                                          
         BCTR  R7,R0                                                            
         MH    R7,=H'8'                                                         
         L     RF,=A(SELECTAB)                                                  
         AR    R7,RF                                                            
         LA    RF,8                                                             
         LA    R5,BUFACCS          BUILD 8X8 LINE                               
*                                                                               
B34      LA    RE,PLUS             FROM 44X6 LINE                               
         SR    R1,R1                                                            
         IC    R1,0(,R7)                                                        
         LTR   R1,R1                                                            
         BZ    B35                                                              
         BCTR  R1,R0                                                            
         MH    R1,=H'6'                                                         
         AR    RE,R1                                                            
         ZAP   0(8,R5),0(6,RE)                                                  
         LA    R7,1(,R7)                                                        
         LA    R5,8(,R5)                                                        
         BCT   RF,B34                                                           
*                                                                               
B35      CLC   BUFACCS,=8PL8'0'                                                 
         BE    B36                                                              
         MVI   MYBYTE,0                                                         
         CLI   BUFKEY,11                                                        
         BNE   *+8                                                              
         BAS   RE,ROUNDA           TYPE 11 REPORT F                             
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFLINE                              
         CLI   GRPMODE,C'Y'                     IF PART OF GROUP                
         BNE   B36                                                              
         GOTO1 BINADD,DMCB,BUFLINE,ADGRPTOT,(RC) ADD TO GROUP TABLE             
*                                                                               
B36      MVC   WORK(4),WORK+1                                                   
         CLI   WORK,C' '                                                        
         BNE   B32                                                              
*                                                                               
         CLC   BUDCON+1(12),SPACES ADD TO TOTAL LINE ALSO                       
         BNE   *+12                                                             
         ST    R6,FULL             SAVE ADDRESS OF TABLE ENTRY                  
         B     B40                                                              
         MVC   BUDCON+1(12),SPACES                                              
*                                                                               
B38      LA    R6,L'LINTAB(,R6)    FIND TOTAL LINE FOR THIS BUDGET              
         CLI   0(R6),X'FF'         IF WE CAN                                    
         BNE   *+12                                                             
         L     R6,SVADDR           POINT BACK TO TABLE ENTRY                    
         B     B42                                                              
         CLC   LINKEY,BUDCON                                                    
         BNE   B38                                                              
         MVC   BUFKEY+1(3),LINID   NEW LINE ID                                  
         B     B30                                                              
*                                                                               
B40      DS    0H                                                               
         ST    R6,FULL                                                          
         SR    RF,RF                                                            
         LA    RF,L'LINTAB(,RF)                                                 
         SR    R6,RF               POINT TO PREVIOUS LINE                       
B42      MVC   BUDCON,BUDCSV                                                    
         MVC   INSTRUCS,SPACES                                                  
         MVC   INSTRUCS(4),LINPARM+2                                            
         CLI   INSTRUCS,C' '                                                    
         BE    B98                                                              
         MVC   PLUS8,BUFACCS                                                    
         MVC   MINUS8,BUFACCS                                                   
         LA    RF,8                                                             
         LA    RE,MINUS8                                                        
*                                                                               
B50      MP    0(8,RE),=P'-1'                                                   
         LA    RE,8(,RE)                                                        
         BCT   RF,B50                                                           
         L     R6,FULL                                                          
*                                                                               
B52      MVC   BYTE,INSTRUCS                                                    
         OI    BYTE,X'40'                                                       
*                                                                               
B60      CLC   BYTE,LINID                                                       
         BE    B70                                                              
         LA    R6,L'LINTAB(,R6)                                                 
         CLI   0(R6),X'FF'                                                      
         BNE   B60                                                              
         DC    H'0'                                                             
*                                                                               
B70      MVC   BUFKEY+1(3),LINID                                                
         LA    RF,PLUS8                                                         
         TM    INSTRUCS,X'40'                                                   
         BO    *+8                                                              
         LA    RF,MINUS8                                                        
         MVC   BUFACCS,0(RF)                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFLINE                              
         CLI   GRPMODE,C'Y'                     IF PART OF GROUP                
         BNE   B71                                                              
         GOTO1 BINADD,DMCB,BUFLINE,ADGRPTOT,(RC) ADD TO GROUP TABLE             
*                                                                               
B71      MVC   INSTRUCS(5),INSTRUCS+1                                           
         MVI   INSTRUCS+5,C' '                                                  
         CLI   INSTRUCS,C' '       LAST OF UP TO SIX LINE INSTRUCTIONS          
         BE    B98                                                              
         L     R6,FULL                                                          
         B     B52                                                              
*                                                                               
B98      DS    0H                                                               
         LA    RF,MAXCOLS                                                       
         LA    RE,PLUS                                                          
*                                                                               
B100     ZAP   0(6,RE),=P'0'       ZERO PLUS AFTER EACH BUDGET                  
         LA    RE,6(,RE)                                                        
         BCT   RF,B100                                                          
         B     B05                 GET NEXT BUDGET ELEMENT                      
*                                                                               
         DROP  R6                                                               
         DROP  R5                                                               
*                                                                               
BEND     DS    0H                                                               
         CLI   MODE,PROCACC        EXIT IF AT BOTTOM LEVEL                      
         BE    BENDX               ELSE ADD UP AND CLEAR BOTTOM LEVEL           
         GOTO1 BUFFALO,PLIST,=C'ADD',ADBUFC,1,2,3,4,(X'80',5)                   
         CLI   HILEV,2             IF 2 LEVEL LEDGER - CLEAR BOTTOM             
         BE    BEND10                                                           
         CLI   HILEV,3             IF 3 LEVEL - CLEAR 1 OR 1 AND 2              
         BNE   BEND2                                                            
         CLI   MODE,LEVBFRST       1 IF LEV B                                   
         BE    BEND10                                                           
         CLI   MODE,LEVAFRST                                                    
         BNE   BENDX                                                            
         B     BEND20              1 AND 2 IF LEV A                             
*                                                                               
BEND2    CLI   MODE,LEVCFRST       IF 4 LEVEL LEDGER - CLEAR                    
         BE    BEND10              1 IF LEVEL C                                 
         CLI   MODE,LEVBFRST                                                    
         BE    BEND20               1 AND 2 IF LEVEL B                          
         B     BEND30              1 2 AND 3 IF LEVEL A                         
*                                                                               
BEND10   GOTO1 (RF),(R1),=C'CLEAR',,(X'80',1)                                   
         B     BENDX                                                            
*                                                                               
BEND20   GOTO1 (RF),(R1),=C'CLEAR',,1,(X'80',2)                                 
         B     BENDX                                                            
*                                                                               
BEND30   GOTO1 (RF),(R1),=C'CLEAR',,1,2,(X'80',3)                               
         B     BENDX                                                            
*                                                                               
BENDX    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',SAVEKEY1,IOA                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XMOD1 1                                                                
*                                                                               
         DROP  R3,R4               KEEP IT CLEAN                                
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        LOCAL ROUND ROUTINE                                                    
*---------------------------------------------------------------------*         
ROUNDA   NTR1                                                                   
         LA    R1,BUFACCS                                                       
         LA    R0,8                                                             
*                                                                               
ROUNDA1  SRP   0(8,R1),60,5                                                     
         LA    R1,8(,R1)                                                        
         BCT   R0,ROUNDA1                                                       
*                                                                               
         XIT1  ,                                                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        PREPARE DATA FOR DOWNLOADING                                           
*--------------------------------------------------------------------*          
*                                                                               
         USING DLCBD,R5                                                         
         USING COLTABD,R7                                                       
*                                                                               
         DS    0D                                                               
DLOAD    NMOD1 0,**DLOD**                                                       
         LR    RC,R1               RESET W/S POINTER                            
         LA    R5,DLBUFF                                                        
         CLI   DLSTAT,0            0 = DO HEADLINE                              
         BNE   DLOAD20                                                          
         LA    R2,4                                                             
         L     R3,ADLDGHIR                                                      
         LA    R4,ACHRDESA-ACHEIRD(,R3)                                         
         LA    R0,2                                                             
*                                                                               
DLOAD05  MVC   DLCBFLD(15),0(R4)   USE HEIRARCHY NAMES                          
         CLC   DLCBFLD,SPACES                                                   
         BE    DLOAD08                                                          
         MVC   DLCBFLD+16(4),=C'CODE'                                           
         CH    R0,=H'1'                                                         
         BH    *+10                                                             
         MVC   DLCBFLD+16(4),=C'NAME'                                           
         GOTO1 SQUASHER,DMCB,DLCBFLD,20                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,(R5)                                                    
         BCT   R0,DLOAD05                                                       
         LA    R4,16(R4)                                                        
         LA    R0,2                                                             
         BCT   R2,DLOAD05                                                       
         LA    R0,2                                                             
*                                                                               
DLOAD08  MVC   DLCBFLD(19),=C'CONTRA ACCOUNT CODE'                              
         CH    R0,=H'1'                                                         
         BH    *+10                                                             
         MVC   DLCBFLD+15(4),=C'NAME'                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,(R5)                                                    
         BCT   R0,DLOAD08                                                       
         LA    R2,8                MAX COLS                                     
*                                                                               
DLOAD10  SR    R3,R3                                                            
         ICM   R3,1,0(R6)                                                       
         BZ    DLOADX                                                           
         BCTR  R3,0                                                             
         MH    R3,=Y(L'COLTAB)                                                  
         L     R7,ACOLTAB                                                       
         AR    R7,R3                                                            
         MVC   DLCBFLD(10),COLHEAD1                                             
         MVC   DLCBFLD+10(10),COLHEAD2                                          
         GOTO1 SQUASHER,DMCB,DLCBFLD,20                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,(R5)                                                    
         LA    R6,1(,R6)                                                        
         BCT   R2,DLOAD10                                                       
         MVI   DLSTAT,3            SET FOR E-O-LINE                             
         B     DLOAD40                                                          
*                                                                               
         USING LINTABD,R6                                                       
*                                                                               
DLOAD20  CLI   MODE,ACCLAST                                                     
         BE    DLOAD20A                                                         
         CLI   QOPT7,C'M'          MULTI-LEVEL TOTALS                           
         BNE   DLOAD50                                                          
*                                                                               
DLOAD20A CLI   DLSTAT,1            1 = DO LINE NAME                             
         BNE   DLOAD30                                                          
         GOTO1 ACCDIV,DMCB,ADLDGHIR,ADACC,LEVSAVE                               
         LA    R4,LEVSAVE                                                       
         LA    R3,ADLVANAM                                                      
         MVC   MYLEV,HILEV                                                      
         CLI   MYLEV,1                                                          
         BE    DLOAD20D                                                         
         CLI   MODE,LEVALAST                                                    
         BNE   DLOAD20B                                                         
         MVI   MYLEV,1                                                          
         B     DLOAD20D                                                         
*                                                                               
DLOAD20B CLI   MODE,LEVBLAST                                                    
         BNE   DLOAD20C                                                         
         MVI   MYLEV,2                                                          
         B     DLOAD20D                                                         
*                                                                               
DLOAD20C CLI   MODE,LEVCLAST                                                    
         BNE   DLOAD20D                                                         
         MVI   MYLEV,3                                                          
*                                                                               
DLOAD20D ZIC   R2,MYLEV                                                         
         MVI   FILLSW,C'N'                                                      
         CLC   MYLEV,HILEV                                                      
         BE    DLOAD25                                                          
         MVI   FILLSW,C'Y'                                                      
         ZIC   R9,HILEV                                                         
         SR    R9,R2               FILLER LOOP COUNT                            
         SLL   R9,1                DOUBLED FOR CODE & NAME                      
*                                                                               
DLOAD25  L     R8,0(,R3)           A(LEV NAME)                                  
         MVC   DLCBFLD(12),1(R4)   ACC. CODE                                    
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,(R5)                                                    
         ZIC   R1,1(,R8)           L' ACCOUNT NAME                              
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),2(R8)                                                 
         GOTO1 SQUASHER,DMCB,DLCBFLD,40                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,(R5)                                                    
         LA    R4,13(,R4)                                                       
         LA    R3,20(,R3)                                                       
         BCT   R2,DLOAD25                                                       
         CLI   FILLSW,C'N'                                                      
         BE    DLOAD28                                                          
*                                                                               
DLOAD25A MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBFLD,C' '                                                     
         GOTO1 DOWNLOAD,(R5)                                                    
         BCT   R9,DLOAD25A                                                      
*                                                                               
DLOAD28  MVI   DLCBFLD,C'1'                                                     
         MVC   DLCBFLD+1(L'LINKEY),LINKEY                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,(R5)                                                    
         MVC   DLCBFLD(L'LINHEAD),LINHEAD                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,(R5)                                                    
         B     DLOADX                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
DLOAD30  CLI   DLSTAT,2            2 = DO NUMBER (PL6 IN DUB2)                  
         BNE   DLOAD40                                                          
         EDIT  (P6,DUB2),(10,DLCBFLD),FLOAT=-,ALIGN=LEFT                        
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBNUM                                                  
         GOTO1 DOWNLOAD,(R5)                                                    
         B     DLOADX                                                           
*                                                                               
DLOAD40  CLI   DLSTAT,3            3 = E-O-LINE                                 
         BNE   DLOAD50                                                          
         MVI   DLCBACT,DLCBEOL                                                  
         GOTO1 DOWNLOAD,(R5)                                                    
         B     DLOADX                                                           
*                                                                               
DLOAD50  CLI   DLSTAT,4            4 = E-O-REPORT                               
         BNE   DLOADX                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 DOWNLOAD,(R5)                                                    
         B     DLOADX                                                           
*                                                                               
DLOADX   XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R5,R7                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        ROUTINE TO ADD TO A BINSRCH TABLE                                      
*        PARAM1              A(RECORD TO BE ADDED)                              
*        PARAM2              A(BINSRCH PARAMS)                                  
*--------------------------------------------------------------------*          
*                                                                               
         ENTRY BINADDC                                                          
*                                                                               
         USING BIND,R5                                                          
*                                                                               
         DS    0D                                                               
BINADDC  NMOD1 0,*BINADD*                                                       
         L     R5,4(,R1)           BINSRCH PARAMETERS                           
         L     R3,0(,R1)           A(RECORD)                                    
         L     RC,8(,R1)                                                        
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    BINXIT              NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         ZIC   R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         ZIC   R7,BINNUMB          NUMBER OF BUCKETS                            
         TM    BINSTAT,X'80'                                                    
         BO    BINBIN              DATA IS BINARY                               
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(,R4)                                                        
         LA    R3,8(,R3)                                                        
         BCT   R7,*-14                                                          
         B     BINXIT                                                           
*                                                                               
BINBIN   L     RE,0(,R3)                                                        
         L     RF,0(,R4)                                                        
         AR    RF,RE                                                            
         ST    RF,0(,R4)                                                        
         LA    R3,4(,R3)                                                        
         LA    R4,4(,R4)                                                        
         BCT   R7,BINBIN                                                        
*                                                                               
BINXIT   XMOD1 1                                                                
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         LTORG                                                                  
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        TABLE FOR REPORT LINES                                                 
*--------------------------------------------------------------------*          
*                                                                               
         ENTRY LINTABC                                                          
*                                                                               
LINTABC  DS    0D                                                               
LINTAB   DS    0CL38                                                            
         DC    C'A1A',CL29' ',CL6'AC'             1=BILLINGS                    
         DC    C'A1B',CL29' ',CL6'AC'                                           
         DC    C'A1C',CL29' ',CL6'AC'                                           
         DC    C'A1D',CL29' ',CL6'AC'                                           
         DC    C'A1E',CL29' ',CL6'AC'                                           
         DC    C'A1F',CL29' ',CL6'AC'                                           
         DC    C'A1G',CL29' ',CL6'AC'                                           
         DC    C'A1H',CL29' ',CL6'AC'                                           
         DC    C'A1I',CL29' ',CL6'AC'                                           
         DC    C'A1J',CL29' ',CL6'AC'                                           
         DC    C'A1K',CL29' ',CL6'AC'                                           
         DC    C'A1L',CL29' ',CL6'AC'                                           
         DC    C'A1M',CL29' ',CL6'AC'                                           
         DC    C'A1N',CL29' ',CL6'AC'                                           
         DC    C'A1O',CL29' ',CL6'AC'                                           
         DC    C'A1P',CL29' ',CL6'AC'                                           
         DC    C'A1Q',CL29' ',CL6'AC'                                           
         DC    C'A1R',CL29' ',CL6'AC'                                           
         DC    C'A1S',CL29' ',CL6'AC'                                           
         DC    C'A1T',CL29' ',CL6'AC'                                           
         DC    C'A1U',CL29' ',CL6'AC'                                           
         DC    C'A1V',CL29' ',CL6'AC'                                           
         DC    C'A1W',CL29' ',CL6'AC'                                           
         DC    C'A1X',CL29' ',CL6'AC'                                           
         DC    C'A1Y',CL29' ',CL6'AC'                                           
         DC    C'A1Z',CL29' ',CL6'AC'                                           
         DC    C'A10',CL29' ',CL6'AC'                                           
         DC    C'A11',CL29' ',CL6'AC'                                           
         DC    C'A12',CL29' ',CL6'AC'                                           
         DC    C'A13',CL29' ',CL6'AC'                                           
         DC    C'A14',CL29' ',CL6'AC'                                           
         DC    C'A15',CL29' ',CL6'AC'                                           
         DC    C'A16',CL29' ',CL6'AC'                                           
         DC    C'A17',CL29' ',CL6'AC'                                           
         DC    C'A18',CL29' ',CL6'AC'                                           
         DC    C'A19',CL29' ',CL6'AC'                                           
XREPLC1  DC    C'C1 ',CL29'  ** TOTAL BILLING **',CL6' '                        
*                                                                               
         DC    C'E2A',CL29' ',C'EGSY  '            2=INCOME                     
         DC    C'E2B',CL29' ',C'EGSY  '                                         
         DC    C'E2C',CL29' ',C'EGSY  '                                         
         DC    C'E2D',CL29' ',C'EGSY  '                                         
         DC    C'E2E',CL29' ',C'EGSY  '                                         
         DC    C'E2F',CL29' ',C'EGSY  '                                         
         DC    C'E2G',CL29' ',C'EGSY  '                                         
         DC    C'E2H',CL29' ',C'EGSY  '                                         
         DC    C'E2I',CL29' ',C'EGSY  '                                         
         DC    C'E2J',CL29' ',C'EGSY  '                                         
         DC    C'E2K',CL29' ',C'EGSY  '                                         
         DC    C'E2L',CL29' ',C'EGSY  '                                         
         DC    C'E2M',CL29' ',C'EGSY  '                                         
         DC    C'E2N',CL29' ',C'EGSY  '                                         
         DC    C'E2O',CL29' ',C'EGSY  '                                         
         DC    C'E2P',CL29' ',C'EGSY  '                                         
         DC    C'E2Q',CL29' ',C'EGSY  '                                         
         DC    C'E2R',CL29' ',C'EGSY  '                                         
         DC    C'E2S',CL29' ',C'EGSY  '                                         
         DC    C'E2T',CL29' ',C'EGSY  '                                         
         DC    C'E2U',CL29' ',C'EGSY  '                                         
         DC    C'E2V',CL29' ',C'EGSY  '                                         
         DC    C'E2W',CL29' ',C'EGSY  '                                         
         DC    C'E2X',CL29' ',C'EGSY  '                                         
         DC    C'E2Y',CL29' ',C'EGSY  '                                         
         DC    C'E2Z',CL29' ',C'EGSY  '                                         
         DC    C'E20',CL29' ',C'EGSY  '                                         
         DC    C'E21',CL29' ',C'EGSY  '                                         
         DC    C'E22',CL29' ',C'EGSY  '                                         
         DC    C'E23',CL29' ',C'EGSY  '                                         
         DC    C'E24',CL29' ',C'EGSY  '                                         
         DC    C'E25',CL29' ',C'EGSY  '                                         
         DC    C'E26',CL29' ',C'EGSY  '                                         
         DC    C'E27',CL29' ',C'EGSY  '                                         
         DC    C'E28',CL29' ',C'EGSY  '                                         
         DC    C'E29',CL29' ',C'EGSY  '                                         
*&&US*&& DC    C'G2 ',CL29'  ** GROSS INCOME **',CL6' '                         
*&&UK*&& DC    C'G2 ',CL29'  ** TOTAL REVENUE **',CL6' '                        
*                                                                               
         DC    C'I3A',CL29' ',C'IKR',X'A2E7A8'  3=EXPENSES                      
         DC    C'I3B',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3C',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3D',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3E',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3F',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3G',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3H',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3I',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3J',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3K',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3L',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3M',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3N',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3O',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3P',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3Q',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3R',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3S',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3T',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3U',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3V',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3W',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3X',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3Y',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I3Z',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I30',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I31',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I32',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I33',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I34',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I35',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I36',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I37',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I38',CL29' ',C'IKR',X'A2E7A8'                                  
         DC    C'I39',CL29' ',C'IKR',X'A2E7A8'                                  
*&&US                                                                           
XREPLK3  DC    C'K3 ',CL29'  ** TOTAL DIR EXPENSE **',CL6' '                    
*&&                                                                             
*&&UK*&& DC    C'K3 ',CL29'  ** TOTAL EXPENSES ** ',CL6' '                      
*                                                                               
         DC    C'L4A',CL29' ',C'LMR',X'A2E7A8'      4=TIME COSTS                
         DC    C'L4B',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4C',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4D',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4E',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4F',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4G',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4H',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4I',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4J',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4K',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4L',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4M',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4N',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4O',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4P',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4Q',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4R',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4S',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4T',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4U',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4V',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4W',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4X',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4Y',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L4Z',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L40',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L41',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L42',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L43',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L44',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L45',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L46',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L47',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L48',CL29' ',C'LMR',X'A2E7A8'                                  
         DC    C'L49',CL29' ',C'LMR',X'A2E7A8'                                  
*                                                                               
XREPLM4  DC    C'M4 ',CL29'  ** TOTAL TIME COSTS **',CL6' '                     
*                                                                               
         DC    C'N7A',CL29' ',C'NOR',X'A2E7A8'    7=DIRECTLY RELATED            
         DC    C'N7B',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7C',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7D',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7E',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7F',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7G',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7H',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7I',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7J',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7K',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7L',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7M',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7N',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7O',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7P',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7Q',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7R',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7S',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7T',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7U',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7V',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7W',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7X',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7Y',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N7Z',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N70',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N71',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N72',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N73',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N74',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N75',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N76',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N77',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N78',CL29' ',C'NOR',X'A2E7A8'                                  
         DC    C'N79',CL29' ',C'NOR',X'A2E7A8'                                  
*&&UK                                                                           
         DC    C'O7 ',CL29'  ** TOTAL RELATED EXPENSES',CL6' '                  
         DC    C'R  ',CL29'TOTAL TIME AND EXPENSES',CL6' '                      
*&&                                                                             
*&&US                                                                           
         DC    C'O7 ',CL29'  ** TOTAL RELATED EXPENSES',CL6' '                  
XREPLR   DC    C'R  ',CL29'TOTAL DIR TIME AND EXPENSE',CL6' '                   
*&&                                                                             
XREPLS61 DC    C'S61',CL29'  ** DIRECT MARGIN **',CL6' '                        
*                                                                               
         DC    C'U5A',CL29' ',C'UW',X'E7A84040'   5=OVERHEAD                    
         DC    C'U5B',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5C',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5D',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5E',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5F',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5G',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5H',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5I',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5J',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5K',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5L',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5M',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5N',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5O',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5P',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5Q',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5R',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5S',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5T',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5U',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5V',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5W',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5X',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5Y',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U5Z',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U50',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U51',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U52',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U53',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U54',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U55',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U56',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U57',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U58',CL29' ',C'UW',X'E7A84040'                                 
         DC    C'U59',CL29' ',C'UW',X'E7A84040'                                 
*                                                                               
         DC    C'V6A',CL29'OVERHEAD',C'VW',X'E7A84040'   6=OVERHEAD             
         DC    C'V6B',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6C',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6D',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6E',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6F',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6G',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6H',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6I',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6J',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6K',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6L',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6M',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6N',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6O',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6P',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6Q',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6R',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6S',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6T',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6U',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6V',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6W',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6X',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6Y',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V6Z',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V60',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V61',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V62',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V63',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V64',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V65',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V66',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V67',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V68',CL29' ',C'VW',X'E7A84040'                                 
         DC    C'V69',CL29' ',C'VW',X'E7A84040'                                 
*&&US                                                                           
XREPLW5  DC    C'W5 ',CL29'  ** OPERATING EXPENSE **',CL6' '                    
XREPLX   DC    C'X  ',CL29'TOTAL DIRECT/INDIRECT EXP',CL6' '                    
XREPLY62 DC    C'Y62',CL29'PROFIT CONTRIBUTION',CL6' '                          
*&&                                                                             
*&&UK                                                                           
         DC    C'W5 ',CL29'  ** TOTAL OVERHEADS **',CL6' '                      
         DC    C'X  ',CL29'TOTAL EXPENSE/OVERHEADS',CL6' '                      
         DC    C'Y62',CL29'NET PROFIT / LOSS',CL6' '                            
*&&                                                                             
         DC    X'FF'                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        TABLE OF REPORT LINE REPLACEMENTS                                      
*        FORMAT IS CO/ADDRESS OF LINTAB/REPLACE DATA                            
*--------------------------------------------------------------------*          
*                                                                               
         ENTRY REPCS                                                            
*                                                                               
REPCS    DS    0D                                                               
*&&US                                                                           
REPTAB   DS    0H                                                               
         DC    C'TB',AL3(XREPLK3),CL29'TOTAL DIRECT CLIENT EXPENSE'             
         DC    C'TB',AL3(XREPLM4),CL29'  ** TOTAL ADVERTISING SVCS'             
         DC    C'TB',AL3(XREPLR),CL29'TOTAL DIRECT EXPENSE'                     
         DC    C'TB',AL3(XREPLY62),CL29'NET OPERATING INCOME'                   
         DC    C'QJ',AL3(XREPLY62),CL29'PROFIT/LOSS BEFORE TAXES'               
         DC    C'SC',AL3(XREPLR),CL29'TOTAL DIRECT EXPENSES'                    
         DC    C'SC',AL3(XREPLY62),CL29'PROFIT/LOSS BEFORE TAXES'               
         DC    C'SC',AL3(XREPLS61),CL29'OPERATING PROFIT'                       
         DC    C'SC',AL3(XREPLX),CL29' '                                        
         DC    C'BS',AL3(XREPLX),CL29'  ** TOTAL EXPENSES **'                   
         DC    C'BD',AL3(XREPLC1),CL29'  ** TOTAL CAP SALES **'                 
         DC    C'BD',AL3(XREPLS61),CL29'  ** PROFIT BEFORE OVERHEAD**'          
         DC    C'BD',AL3(XREPLW5),CL29'  ** TOTAL OVERHEAD **'                  
         DC    C'BD',AL3(XREPLX),CL29'  ** TOTAL DIRECT/OVERHEAD **'            
         DC    X'FF'                                                            
*&&                                                                             
*&&UK                                                                           
REPTAB   DS    0H                                                               
         DC    X'FF'                                                            
*&&                                                                             
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*                                                                               
*--------------------------------------------------------------------*          
         ENTRY GRPTOT                                                           
GRPTOT   DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(L'BUFLINE)      TABLE LENGTH                                 
         DC    AL4(BUFACCS-BUFKEY) DISP. TO KEY KEY LENGTH                      
         DC    F'200'              MAX. NUMBER OF RECORDS                       
         DC    AL1(L'BUFACCS/8)    NUMBER OF BUCKETS                            
         DC    AL1(BUFACCS-BUFKEY) DISP TO BUCKETS                              
         DC    AL2(0)                                                           
         DS    (200*L'BUFLINE)C    TABLE                                        
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*                                                                               
*--------------------------------------------------------------------*          
         ENTRY BUDC                                                             
BUDC     DS    0D                                                               
         DC    1500X'00'                                                        
*                                                                               
         BUFF  LINES=001,ROWS=5,COLUMNS=8,FLAVOR=PACKED,KEYLIST=(4,A)           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        DSECT FOR SPACEND                                                      
*--------------------------------------------------------------------*          
AC9A02D  DSECT                                                                  
RELO     DS    F                                                                
SVADDR   DS    F                                                                
APGM     DS    A                   A(AC9A02)                                    
AWRKD    DS    A                   A(WORKD)                                     
*                                                                               
ATYPES   DS    0A                  A AND V TYPES                                
ADBUFC   DS    A                   A(BUFFALOC)                                  
ADREPTAB DS    A                   A(REPCS)                                     
ADLINTAB DS    A                   A(LINTABC)                                   
ABUDC    DS    A                   A(BUDC)                                      
ADGRPTOT DS    A                   A(GRPTOT)                                    
BINADD   DS    A                   A(BINADDC)                                   
BUDACC   DS    V                   V(BUDACC)                                    
ACCDIV   DS    V                   V(ACCDIV)                                    
SQUASHER DS    V                   V(SQUASHER)                                  
DOWNLOAD DS    V                   V(DLFLD)                                     
ACOLTAB  DS    A                   A(COLTAB)                                    
ADLOAD   DS    A                   A(DLOAD)                                     
ATAXIT   DS    A                   A(TAXIT)                                     
*                                                                               
DUB2     DS    D                   EXTRA DUB                                    
DUB3     DS    D                   EXTRA DUB                                    
PLIST    DS    8F                                                               
MYWORD   DS    F                                                                
FULL2    DS    F                                                                
NUMSAVE  DS    F                                                                
*                                                                               
LSTBINRC DS    A                   A(LAST BINSRCH RECORD)                       
*                                                                               
SDATE    DS    PL2                 START DATE PACKED                            
         DS    PL1                 START DATE DAY  01                           
DATE     DS    PL2                 END   DATE PACKED                            
         DS    PL1                 END   DATE DAY  01                           
*                                                                               
BDATE    DS    PL2                 BUDGET AMOUNT ELEMENT YYMM                   
*                                                                               
GRPMODE  DS    CL1                                                              
PLUS     DS    (MAXCOLS)PL6                                                     
MINUS    DS    (MAXCOLS)PL6                                                     
*                                  **** KEEP TOGETHER START                     
BUFLINE  DS    0CL68                                                            
BUFKEY   DS    CL4                                                              
BUFACCS  DS    CL64                8 X PL8                                      
*                                  **** KEEP TOGETHER END                       
PLUS8    DS    CL64                                                             
MINUS8   DS    CL64                                                             
INSTRUCS DS    CL6                                                              
MENUSV   DS    CL4                                                              
FIVE     DS    CL5                                                              
DIV      DS    CL14                WORK SPACE FOR DIVISION                      
HDAT     DS    CL23                DATE FOR HEADLINE                            
MYBYTE   DS    CL1                 SWITCH FOR HEADLINE PRINTING                 
COMPBYTE DS    CL1                                                              
BUDCON   DS    CL13                                                             
BUDCSV   DS    CL13                                                             
INCOME   DS    CL64                                                             
PROFIT   DS    CL64                                                             
REQACTIV DS    CL1                                                              
HILEV    DS    CL1                                                              
THISLEV  DS    CL1                                                              
BNUMBER  DS    PL3                                                              
CNUMBER  DS    PL3                                                              
DNUMBER  DS    PL3                                                              
LOWNO    DS    PL1                                                              
TOTREV   DS    PL8                                                              
TOTREV2  DS    PL8                                                              
SAVEH1   DS    CL80                                                             
SAVEH2   DS    CL80                                                             
SAVEH3   DS    CL80                                                             
SAVEH4   DS    CL80                                                             
SAVEH5   DS    CL132                                                            
SAVEH6   DS    CL132                                                            
PSV      DS    CL132                                                            
LEVSAVE  DS    CL52                                                             
AGYTOTS  DS    6PL8                                                             
*                                  **** KEEP TOGETHER START                     
SUMREC   DS    0CL106                                                           
SUMNUM   DS    CL12                                                             
SUMNAME  DS    CL36                                                             
SUMACCS  DS    6PL8                                                             
*                                  **** KEEP TOGETHER END                       
MYBOXSW  DS    CL1                                                              
MYROW    DS    CL100                                                            
MYCOL    DS    CL132                                                            
*SVMYROW DS    CL132                                                            
BUDNAME  DS    CL10                                                             
BUDKEY   DS    CL15                                                             
SAVEKEY1 DS    CL42                                                             
DLBUFF   DS    CL80                                                             
DLPLINE  DS    CL132                                                            
DLSTAT   DS    C                                                                
MYLEV    DS    C                   DOWNLOAD COLUMN COUNT                        
FILLSW   DS    C                   FILLER LOOP COUNT                            
METHOD   DS    CL1                 NEW COST METHOD OF ALLOCATION                
IOA      DS    1050C                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        DSECT FOR A LINE OF THE COLUMN TABLE                                   
*--------------------------------------------------------------------*          
COLTABD  DSECT                                                                  
COLTYPE  DS    CL1                                                              
COLSTRT  DS    PL2                 YYMM (PACKED)                                
COLEND   DS    PL2                 YYMM (PACKED)                                
COLHEAD1 DS    CL10                COLUMN HEADINGS                              
COLHEAD2 DS    CL10                                                             
COLLEN   EQU   *-COLTABD                                                        
*                                                                               
*                                  COLTYPE VALUES ARE -                         
*                                  A=REGULAR                                    
*                                  B=BUDGET                                     
*                                  P=PERCENT OF BUDGET                          
*                                  S=PERCENT OF REVENUE                         
*                                  V=ACTUAL LESS BUDGET                         
*                                                                               
*              DSECT FOR A LINE OF THE LINE TABLE                               
*                                                                               
LINTABD  DSECT                                                                  
LINID    DS    CL1                 COLLATING CODE                               
LINKEY   DS    CL2                 LEDGER/ACCT FOR COSTING POSTINGS             
LINHEAD  DS    CL29                NAME OF LINE IN REPORT                       
LINPARM  DS    CL5                 6X1B GIVES CODE OF LINE FOR ADDITION         
*                                  OR SUBTRACTION (IF X'40' BIT OFF)            
*                                                                               
*              DSECT FOR RPT LINE DATA OVERRIDE                                 
*                                                                               
REPTABD  DSECT                                                                  
RLAPLHA  DS    CL2                 ALPHA FOR THIS COMPANY                       
RLINADD  DS    AL3                 ADDRESS OF LINE TO REPLACE                   
RLINHEAD DS    CL29                REPLACE LINE DATA WITH THIS                  
REPLEN   EQU   *-REPTABD                                                        
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        DSECT FOR THE BINSRCH LIST                                             
*--------------------------------------------------------------------*          
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINSTAT  DS    CL1                 X'80' BINARY DATA                            
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                                                             
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        INCLUDED DSECTS                                                        
*--------------------------------------------------------------------*          
*ACBUDACCD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACBUDACCD                                                      
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDREPXTRAD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*DDREPMASTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
*DDREMOTED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078ACREP9A02S05/01/02'                                      
         END                                                                    
