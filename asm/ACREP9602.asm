*          DATA SET ACREP9602  AT LEVEL 014 AS OF 07/10/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044165.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE AC9602A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE ACCDIV                                                                 
         EJECT                                                                  
* PROFILE OPTIONS                                                               
         SPACE 2                                                                
* 1      REVERSE PRINTING ORDER OF 13/14                                        
* 2      USE LEDGER NAMES NOT TABLE NAMES FOR TOTAL LINES                       
* 3      SKIP DIRECT MARGIN LINE(PRE OVERHEAD PROFIT)                           
* 4      SKIP TOTAL OVERHEAD LINE                                               
* 5      SKIP TOTAL TIME+EXPENSE LINE                                           
* 6      CHICKEN TRACK TOTAL EXPENSE LINE                                       
* 7      CHICKEN TRACK TIME+EXPENSE LINE                                        
* 8      PRINT SPECIAL SUMMARY                                                  
* 9      PRINT TOTAL LINES ONLY                                                 
* 10     CALCULATE BILLINGS FROM INCOME(CAPITALISE)                             
* 11     DO NOT PRINT DETAILS OF BILLING/INCOME                                 
* 12     NO OF LOW LEVEL ACCOUNTS FOR HIGH LEVEL TO PRINT                       
* 13     SUPPRESS 13 DETAILS                                                    
* 14     SUPPRESS 14 DETAILS                                                    
* 15     SUPPRESS 15 DETAILS                                                    
* 16     TAX RATE                                                               
         TITLE 'CLIENT PROFITABILITY REPORT'                                    
AC9602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC9602,RR=R5                                                 
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         USING AC9602+4096,R8                                                   
         LA    RC,2048(R8)                                                      
         LA    RC,2048(RC)                                                      
         USING AC9602+8192,RC                                                   
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    R9,SPACEND                                                       
         USING AC9602D,R9                                                       
         ST    R5,RELO                                                          
         EJECT                                                                  
*              INITIAL ROUTINES                                                 
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   PR01                                                             
         L     R7,=A(BUFFALOC)                                                  
         A     R7,RELO                                                          
         ST    R7,ADBUFC                                                        
         GOTO1 BUFFALO,DMCB,=C'SET',(R7)                                        
         L     R7,=A(LINTABC)                                                   
         A     R7,RELO                                                          
         ST    R7,ADLINTAB                                                      
         BAS   RE,BUDNAME          FILL IN LINTAB NAMES                         
         CLI   PROGPROF,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,INVERT           REVERSE PRINTING ORDER OF 13/14              
         CLI   PROGPROF+1,C'Y'                                                  
         BNE   XIT                                                              
         BAS   RE,REPLACE          USE LEDGER NAMES FOR TOTAL LINES             
         B     XIT                                                              
         EJECT                                                                  
PR01     DS    0H                                                               
         CLI   MODE,REQFRST                                                     
         BNE   PR20                                                             
         MVC   AGYTOTS(48),=8PL8'0'                                             
         MVI   REQACTIV,X'FF'      FOR REQUEST ACTIVITY                         
         MVC   QSTART+4(2),=C'01'                                               
         MVC   QEND+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,QEND),(1,DATE)                                    
         GOTO1 (RF),(R1),,(9,HDAT)                                              
         GOTO1 (RF),(R1),(0,QSTART),(1,SDATE)                                   
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   GRPMODE,C'N'                                                     
         ZAP   BNUMBER,=P'0'                                                    
         ZAP   CNUMBER,=P'0'                                                    
*&&UK*&& ZAP   LOWNO,=P'1'         DEFAULT VALUES FOR LOW LEVEL                 
*&&US*&& ZAP   LOWNO,=P'0'         NUMBER TEST                                  
         CLI   PROGPROF+11,0       ELSE USE PROFILE VALUE                       
         BE    PR01A                                                            
         ZIC   RE,PROGPROF+11                                                   
         CVD   RE,DUB                                                           
         ZAP   LOWNO,DUB                                                        
PR01A    DS    0H                                                               
         CLI   QOPT1,C' '          ESTABLISH A DEFAULT                          
         BNE   *+8                                                              
         MVI   QOPT1,C'1'                                                       
         LA    R7,MENUS                                                         
PR02     CLI   0(R7),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   QOPT1,0(R7)                                                      
         BE    *+12                                                             
         LA    R7,5(R7)                                                         
         B     PR02                                                             
         SPACE 1                                                                
         MVC   MENUSV,1(R7)        SAVE MENU FOR THIS REQUEST                   
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT3,C'Y'          SPROG FOR INCOME REPORT                      
         BNE   *+8                                                              
         MVI   RCSUBPRG,1                                                       
         L     R1,ADWORK                                                        
*&&DO                                                                           
         POINTS (R1)                                                            
         CHECK (R1)                                                             
*&&                                                                             
*&&OS                                                                           
         LR    R7,R1                                                            
         POINT (R7),TTRFIRST                                                    
*&&                                                                             
         SPACE 3                                                                
         LA    R7,COLTAB           FILL IN THE MONTHS FOR             1         
         USING COLTABD,R7          YEARLY, QUARTERLY ETC FIELDS                 
         SR    R3,R3               IN THE COLUMN TABLE                          
         XC    HALF,HALF                                                        
         ZAP   DUB,=P'0'                                                        
         SPACE 1                                                                
         MVC   COLSTRT,DATE        MONTH THIS YEAR                              
         MVC   COLEND,DATE                                                      
         MVC   STEND(2),SDATE      START/END IN ONE FIELD                       
         MVC   STEND+2(2),DATE                                                  
         SPACE 1                                                                
         LA    R7,L'COLTAB(R7)     MONTH LAST YEAR                    2         
         MVC   COLSTRT,DATE                                                     
         MVC   DUB+5(2),COLSTRT                                                 
         SP    DUB,=P'1000'        REDUCE YEAR BY ONE                           
         MVC   COLSTRT(1),DUB+5                                                 
         MVC   COLEND,COLSTRT                                                   
         SPACE 1                                                                
         LA    R7,L'COLTAB(R7)     LAST MONTH                         3         
         MVC   COLSTRT,DATE                                                     
         CLI   COLSTRT+1,X'01'     JAN/DEC PROBLEM                              
         BNE   PR1                                                              
         SPACE 1                                                                
         MVC   DUB+5(2),COLSTRT                                                 
         SP    DUB,=P'1000'        REDUCE YEAR BY ONE                           
         MVC   COLSTRT(1),DUB+5                                                 
         MVI   COLSTRT+1,X'12'                                                  
         B     PR1A                                                             
         SPACE 1                                                                
PR1      MVC   DUB+5(2),COLSTRT                                                 
         SP    DUB,=P'10'                                                       
         MVC   COLSTRT(2),DUB+5                                                 
PR1A     MVC   COLEND,COLSTRT                                                   
         SPACE 1                                                                
         LA    R7,L'COLTAB(R7)     BUDGET THIS MONTH                  4         
         MVC   COLSTRT,DATE                                                     
         MVC   COLEND,DATE                                                      
         SPACE 1                                                                
         LA    R7,L'COLTAB(R7)     % ACT VS BUDGET THIS MONTH         5         
         MVC   COLSTRT,DATE                                                     
         MVC   COLEND,DATE                                                      
         SPACE 2                                                                
         LA    R6,12                                               6-17         
         MVC   DUB+5(2),SDATE                                                   
         SR    RE,RE                                                            
         IC    RE,SDATE+1          MONTH FROM START DATE                        
         CH    RE,=H'10'                                                        
         BL    *+8                                                              
         SH    RE,=H'6'                                                         
         BCTR  RE,R0                                                            
         MH    RE,=H'3'                                                         
         LA    RE,MONTHS(RE)                                                    
         SPACE 1                                                                
PR1B     LA    R7,L'COLTAB(R7)                                                  
         MVC   COLSTRT,DUB+5                                                    
         MVC   COLEND,DUB+5                                                     
         MVC   COLHEAD1+3(3),0(RE)                                              
         AP    DUB,=P'10'                                                       
         LA    RE,3(RE)                                                         
         CLI   DUB+6,X'13'         DID WE JUST DO DEC                           
         BNE   PR1D                                                             
         LA    RE,MONTHS                                                        
         MVI   DUB+6,X'01'         JAN                                          
         AP    DUB,=P'1000'        ADD 1 TO YEAR                                
PR1D     CLC   COLSTRT,DATE                                                     
         BNH   *+8                                                              
         MVI   COLSTRT,X'FF'       DONT SHOW ACTIVITY AFTER END DATE            
         CH    R6,=H'8'                                                         
         BNE   PR1H                                                             
         MVC   SVEND1,DUB+5        SAVE END OF FIRST 6 MONTHS                   
         CLC   SVEND1,DATE         OR END DATE                                  
         BNH   *+10                WHICHEVER IS LOWER                           
         MVC   SVEND1,DATE                                                      
PR1H     DS    0H                                                               
         BCT   R6,PR1B                                                          
         SPACE 2                                                                
         LA    R7,L'COLTAB(R7)                                       18         
         MVC   COLEND,DATE                                                      
         MVC   COLSTRT,DATE        QUARTER THIS YEAR                            
         MVI   BYTE,C'1'                                                        
         CLI   DATE+1,X'04'        IN 1ST QUARTER?                              
         BL    PR2                 YES - BRANCH                                 
         MVI   BYTE,C'2'                                                        
         CLI   DATE+1,X'07'        2ND QUARTER ?                                
         BL    PR4                                                              
         MVI   BYTE,C'3'                                                        
         CLI   DATE+1,X'10'        THIRD QUARTER?                               
         BL    PR6                                                              
         MVI   BYTE,C'4'           MUST BE 4TH QUARTER                          
         MVI   COLSTRT+1,X'10'                                                  
         MVI   COLEND+1,X'12'                                                   
         B     PR8                                                              
         SPACE 1                                                                
PR2      MVI   COLSTRT+1,X'01'                                                  
         MVI   COLEND+1,X'03'                                                   
         B     PR8                                                              
PR4      MVI   COLSTRT+1,X'04'                                                  
         MVI   COLEND+1,X'06'                                                   
         B     PR8                                                              
         SPACE 1                                                                
PR6      MVI   COLSTRT+1,X'07'                                                  
         MVI   COLEND+1,X'09'                                                   
         SPACE 2                                                                
PR8      MVC   WORD,COLSTRT        SAVE START & END FOR LATER        19         
         MVC   COLEND+1(1),DATE+1  IF IN JAN-DON'T WANT FEB & MAR.ETC           
         LA    R7,L'COLTAB(R7)     QUARTER LAST YEAR                            
         MVC   DUB+3(4),WORD                                                    
         SP    DUB,=P'10001000'    START YR - 1,AND END YR - 1                  
         MVC   COLSTRT(4),DUB+3                                                 
         SPACE 2                                                                
         LA    R7,L'COLTAB(R7)     LAST QUARTER                      20         
         MVC   COLSTRT(4),WORD                                                  
         CLI   BYTE,C'1'           IF IN 1ST QUARTER - MUST SUBTRACT            
         BE    PR10                1 FROM YEAR AS WELL                          
         CLI   BYTE,C'4'           4TH QTR - IF YES,CAN'T JUST                  
         BE    PR9A                SUBTRACT                                     
         IC    R3,COLSTRT+1                                                     
         SH    R3,=H'3'                                                         
         STC   R3,COLSTRT+1                                                     
         AH    R3,=H'2'                                                         
         STC   R3,COLEND+1                                                      
         B     PR12                                                             
         SPACE 1                                                                
PR9A     MVI   COLSTRT+1,X'07'                                                  
         MVI   COLEND+1,X'09'                                                   
         B     PR12                                                             
         SPACE 1                                                                
PR10     MVI   COLSTRT+1,X'10'                                                  
         MVI   COLEND+1,X'12'                                                   
         ZAP   DUB,=P'0'                                                        
         MVC   DUB+5(1),COLSTRT                                                 
         SP    DUB,=P'1000'        SUBTRACT 1 FROM YEAR                         
         MVC   COLSTRT(1),DUB+5                                                 
         MVC   COLEND(1),DUB+5                                                  
         SPACE 2                                                                
PR12     LA    R7,L'COLTAB(R7)     BUDGET THIS QTR                   21         
         MVC   COLSTRT(4),WORD                                                  
         SPACE 2                                                                
         LA    R7,L'COLTAB(R7)     % ACT VS BUDGET (THIS QTR)        22         
         MVC   COLSTRT(4),WORD                                                  
         SPACE 2                                                                
         LA    R7,L'COLTAB(R7)     YTD THIS YR                       23         
         MVC   COLSTRT(4),STEND                                                 
         SPACE 2                                                                
         LA    R7,L'COLTAB(R7)     YTD LAST YEAR                     24         
         MVC   DUB+3(4),STEND                                                   
         SP    DUB,=P'10001000'                                                 
         MVC   COLSTRT(4),DUB+3                                                 
         SPACE 2                                                                
         LA    R7,L'COLTAB(R7)     YTD BUDGET                        25         
         MVC   COLSTRT(4),STEND                                                 
         SPACE 2                                                                
         LA    R7,L'COLTAB(R7)     YTD % ACT VS BUDGET               26         
         MVC   COLSTRT(4),STEND                                                 
         SPACE 2                                                                
         LA    R7,L'COLTAB(R7)     12 MONTH                          27         
         MVC   COLEND,DATE                                                      
         MVC   COLSTRT,DATE                                                     
         ZAP   DUB,=P'0'                                                        
         CLI   COLSTRT+1,X'12'     DECEMBER?                                    
         BE    PR13A               YES-BRANCH                                   
         MVC   DUB+5(2),COLSTRT                                                 
         SP    DUB,=P'1000'        REDUCE YR BY ONE                             
         AP    DUB,=P'10'          12 MONTHS IS JULY-JUN - ADD 1 TO MTH         
         MVC   COLSTRT,DUB+5                                                    
         B     PR13B                                                            
PR13A    MVI   COLSTRT+1,X'01'     JAN TO DEC IS 12 MONTHS                      
PR13B    MVC   WORD,COLSTRT                                                     
         SPACE 1                                                                
         LA    R7,L'COLTAB(R7)     12 MONTH LAST YEAR                28         
         MVC   DUB+3(4),WORD                                                    
         SP    DUB,=P'10001000'                                                 
         MVC   COLSTRT(4),DUB+3                                                 
         SPACE 1                                                                
         LA    R4,2                12 MONTH (X2)                  29,30         
PR14     LA    R7,L'COLTAB(R7)                                                  
         MVC   COLSTRT(4),WORD                                                  
         BCT   R4,PR14                                                          
         SPACE 2                                                                
         ZAP   DUB,=P'0'           MOVING ANNUAL TOTAL FOR LAST 6 MTHS          
         MVC   DUB+5(2),DATE       THIS MONTH                                   
         CLI   DATE+1,X'05'        MAY OR EARLIER?                              
         BNH   PR16                YES-BRANCH                                   
         SP    DUB,=P'50'          POINT BACK 6(?) MONTHS                       
         B     PR17                                                             
         SPACE 1                                                                
PR16     AP    DUB,=P'70'          FORWARD 6(?) MONTHS            31-36         
         SP    DUB,=P'1000'        AND BACK 1 YEAR                              
         SPACE 1                                                                
PR17     ZAP   DOUBLE,DUB+6(2)     MONTH ONLY                                   
         DP    DOUBLE,=P'10'       MONTH IS X10                                 
         MVC   DIV(6),DOUBLE                                                    
         ZAP   DOUBLE,DIV(6)       LOSE THE REMAINDER                           
         CVB   R6,DOUBLE                                                        
         BCTR  R6,R0                                                            
         MH    R6,=H'3'                                                         
         LA    R4,MONTHS(R6)       POINT TO NAME OF START MONTH                 
         MVC   DOUBLE,DUB                                                       
         CLI   DOUBLE+6,X'12'      DEC?                                         
         BE    PR17A               YES-BRANCH                                   
         AP    DOUBLE,=P'10'       NO - BACK 1 YR & FORWARD 1 MONTH             
         SP    DOUBLE,=P'1000'                                                  
         B     *+8                                                              
PR17A    MVI   DOUBLE+6,X'01'      MAKE EQUAL TO JAN-DEC                        
         SPACE 2                                                                
         LA    R6,6                                                             
PR18     LA    R7,L'COLTAB(R7)                                                  
         MVC   COLHEAD2+3(3),0(R4) NAME OF MONTH                                
         MVC   COLSTRT,DOUBLE+5                                                 
         MVC   COLEND,DUB+5                                                     
         AP    DUB,=P'10'          BUMP BY 1 MONTH                              
         AP    DOUBLE,=P'10'                                                    
         LA    R4,3(R4)                                                         
         CLI   DUB+6,X'13'         HAVE WE JUST DONE DEC?                       
         BE    PR18A               THEN ADJUST LEADING YEAR/MONTH               
         CLI   DOUBLE+6,X'13'      OR TRAILING YEAR/MONTH                       
         BE    PR18B                                                            
         B     PR19                                                             
         SPACE 1                                                                
PR18A    MVI   DUB+6,X'01'         MAKE MONTH JAN                               
         AP    DUB,=P'1000'        BUMP YEAR BY ONE                             
         LA    R4,MONTHS           BACK TO JAN                                  
         B     PR19                                                             
         SPACE 1                                                                
PR18B    MVI   DOUBLE+6,X'01'                                                   
         AP    DOUBLE,=P'1000'                                                  
         SPACE 1                                                                
PR19     BCT   R6,PR18                                                          
         SPACE 2                                                                
         LA    R7,L'COLTAB(R7)     LAST YEAR                         37         
         ZAP   DUB,=P'0'                                                        
         MVC   DUB+5(2),SDATE                                                   
         SP    DUB,=P'1000'        BACK 1 YEAR                                  
         MVC   COLSTRT,DUB+5                                                    
         ZAP   DUB,=P'0'                                                        
         MVC   DUB+5(2),DATE                                                    
         SP    DUB,=P'1000'                                                     
         MVC   COLEND,DUB+5                                                     
         SPACE 2                                                                
         LA    R7,L'COLTAB(R7)     FIRST 6 MONTHS TOTAL              38         
         MVC   COLSTRT,SDATE                                                    
         MVC   COLEND,SVEND1                                                    
         SPACE 1                                                                
         LA    R7,L'COLTAB(R7)     SAME 6 LAST YEAR                  39         
         ZAP   DUB,=P'0'                                                        
         MVC   DUB+5(2),SDATE                                                   
         SP    DUB,=P'1000'                                                     
         MVC   COLSTRT,DUB+5                                                    
         MVC   DUB+5(2),SVEND1                                                  
         SP    DUB,=P'1000'                                                     
         MVC   COLEND,DUB+5                                                     
         SPACE 1                                                                
         LA    R7,L'COLTAB(R7)     12 MONTH BUDGET (FROM START OF YEAR)         
         MVC   COLSTRT,SDATE                                         40         
         MVC   COLEND,SDATE                                                     
         MVI   COLEND+1,X'12'                                                   
         CLI   SDATE+1,X'01'       ASSUME JAN START                             
         BE    PR19A                                                            
         MVC   DUB+5(2),SDATE                                                   
         AP    DUB,=P'990'         FORWARD 1 YR AND BACK 1 MONTH                
         MVC   COLEND,DUB+5                                                     
         SPACE 1                                                                
PR19A    LA    R7,L'COLTAB(R7)     YEAR-TO-DATE                      41         
         MVC   COLSTRT(4),STEND                                                 
         SPACE 2                                                                
         LA    R7,L'COLTAB(R7)     YTD THIS YR                       42         
         MVC   COLSTRT(4),STEND                                                 
         SPACE 2                                                                
         LA    R7,L'COLTAB(R7)     YTD THIS YR                       43         
         MVC   COLSTRT(4),STEND                                                 
         B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
*              LEDGFRST - GET NUMBER OF LEVELS                                  
         SPACE 2                                                                
PR20     CLI   MODE,LEDGFRST                                                    
         BNE   PR30                                                             
         L     R4,ADLDGHIR                                                      
         USING ACHEIRD,R4                                                       
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
         EJECT                                                                  
*              ROUTINE FOR ACCFRST                                              
         SPACE 2                                                                
PR30     CLI   MODE,PROCACC                                                     
         BNE   PR40                                                             
         MVI   MYBYTE,X'FF'                                                     
         LA    R6,PLUS                                                          
         LA    R7,MINUS                                                         
         LA    RF,43                                                            
PR32     ZAP   0(6,R6),=P'0'                                                    
         ZAP   0(6,R7),=P'0'                                                    
         LA    R6,6(R6)                                                         
         LA    R7,6(R7)                                                         
         BCT   RF,PR32                                                          
         MVI   FORCEHED,C'Y'       NEW PAGE PER CLIENT                          
         CLI   HILEV,1                                                          
         BNE   PR34                                                             
         L     R4,ADACCSTA         TEST FOR START OF SUB-GROUP                  
         USING ACSTATD,R4                                                       
         CLI   ACSTCOST,C'('                                                    
         BNE   PR34                                                             
         MVI   GRPMODE,C'Y'                                                     
PR34     BAS   RE,BUDGET           SAVE BUDGETS                                 
         ZAP   TOTREV,=P'0'                                                     
         DROP  R4                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR PROCHIST                                             
         SPACE 2                                                                
PR40     CLI   MODE,PROCHIST       DO WE HAVE AHISTORY RECORD                   
         BNE   PR80                NO, SKIP THIS                                
         L     R4,ADSUBAC          YES, GET SUB-ACCT ADDRESS                    
         USING TRSUBHD,R4                                                       
         L     R5,ADTRANS                                                       
         CLI   0(R5),X'45'                                                      
         BNE   XIT                                                              
         CLI   BUCKTYPE,C' '       WANT ONLY REGULAR BUCKETS                    
         BNE   XIT                                                              
         CLI   TRSBACNT+2,C'P'     TRANSLATE LEDGER P / EXPENSES                
         BNE   PR42                TO LEDGER 3 / EXPENSES                       
         MVC   WORK(1),TRSBACNT+5  DEAL WITH 2/3 LONG DEPTS                     
         CLI   TRSBACNT+6,C' '                                                  
         BE    PR41                                                             
         MVC   WORK(1),TRSBACNT+6                                               
PR41     MVC   TRSBACNT+3(12),SPACES                                            
         MVC   TRSBACNT+3(1),WORK                                               
         MVI   TRSBACNT+2,C'3'                                                  
         SPACE 1                                                                
PR42     MVI   BYTE,X'FF'                                                       
         CLI   QOPT3,C'Y'          INCOME REPORT OPTION                         
         BNE   PR43                                                             
         CLI   TRSBACNT+2,C'2'                                                  
         BH    XIT                                                              
         B     PR43A                                                            
PR43     DS    0H                                                               
         CLI   PROGPROF+9,C'Y'     PROFILE OPTION TO CALCULATE BILLINGS         
         BNE   PR43A               FROM INCOME (CAPITALISE)                     
         CLI   TRSBACNT+2,C'1'                                                  
         BE    XIT                                                              
         SPACE 1                                                                
         USING TRHISTD,R5                                                       
PR43A    ZAP   DUB,TRHSCR          FOR BILLING/COMMISSIONS WE WANT              
         CLI   TRSBACNT+2,C'2'     DEBITS / ALL OTHERS-CREDITS                  
         BH    PR44                                                             
         ZAP   DUB,TRHSDR                                                       
PR44     ZAP   DOUBLE,=P'0'                                                     
         CP    DUB,=P'0'           ZERO? - DON'T BOTHER                         
         BE    XIT                                                              
         SP    DOUBLE,DUB          MINUS AMOUNT                                 
         LA    R6,COLTAB                                                        
         USING COLTABD,R6                                                       
         LA    R3,PLUS                                                          
         LA    R7,MINUS                                                         
PR46     CLC   COLSTRT,TRHSYEAR                                                 
         BH    PR48                                                             
         CLC   COLEND,TRHSYEAR                                                  
         BL    PR48                                                             
         CLI   COLTYPE,C'B'        IGNORE BUDGET COLUMNS                        
         BE    PR48                                                             
         SPACE 2                                                                
         ZAP   0(6,R3),DUB         ADD TO CORRECT PLACE IN                      
         ZAP   0(6,R7),DOUBLE      A PLUS & MINUS LINE                          
         MVI   BYTE,0              GOT ONE                                      
         MVI   MYBYTE,0                                                         
PR48     LA    R3,6(R3)                                                         
         LA    R7,6(R7)                                                         
         LA    R6,L'COLTAB(R6)                                                  
         CLI   0(R6),X'FF'         END OF COLTAB?                               
         BNE   PR46                                                             
         SPACE 1                                                                
         CLI   BYTE,0              DIDN'T GET A MATCH ON                        
         BNE   XIT                 THIS HISTORY ELEMENT                         
         DROP  R6                                                               
PR50     MVC   WORK(4),MENUSV                                                   
PR52     SR    R6,R6                                                            
         IC    R6,WORK             REPORT NUMBER                                
         BCTR  R6,R0                                                            
         MH    R6,=H'8'                                                         
         LA    R7,SELECTAB(R6)                                                  
         LA    RF,8                                                             
         LA    R5,PLUS8            BUILD 2 8-BYTE LINES                         
         LA    R6,MINUS8           FROM 2 43-BYTE LINES                         
         MVC   PLUS8,=8PL8'0'                                                   
         MVC   MINUS8,=8PL8'0'                                                  
PR54     LA    R2,PLUS                                                          
         LA    R3,MINUS                                                         
         SR    R1,R1                                                            
         IC    R1,0(R7)                                                         
         LTR   R1,R1               CAN FINISH AT LESS THAN 8                    
         BZ    PR55                                                             
         BCTR  R1,R0                                                            
         MH    R1,=H'6'                                                         
         AR    R2,R1                                                            
         AR    R3,R1                                                            
         ZAP   0(8,R5),0(6,R2)                                                  
         ZAP   0(8,R6),0(6,R3)                                                  
         LA    R7,1(R7)                                                         
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         BCT   RF,PR54                                                          
         SPACE 2                                                                
PR55     L     R6,ADLINTAB                                                      
         USING LINTABD,R6                                                       
PR56     CLI   0(R6),X'FF'                                                      
         BE    XIT                                                              
         CLC   LINKEY,TRSBACNT+2                                                
         BE    *+12                                                             
         LA    R6,L'LINTAB(R6)                                                  
         B     PR56                                                             
         SPACE 1                                                                
         MVC   HALF,=H'6'          USE HALF AS COUNTER                          
         MVC   INSTRUCS,LINPARM                                                 
PR58     MVC   BUFKEY+1(3),LINID   BUILD A BUFFALO RECORD                       
         MVC   BUFKEY(1),WORK      REPORT NO.                                   
         LA    RF,PLUS8                                                         
         TM    INSTRUCS,X'40'                                                   
         BO    *+8                                                              
         LA    RF,MINUS8                                                        
         MVC   BUFACCS,0(RF)                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFLINE                              
         CLI   PROGPROF+9,C'Y'     CALCULATE BILLING FIGURES                    
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
PR58A    ZAP   DIV,0(8,RE)         INCOME OF 15 BECOMES BILLING OF 100          
         MP    DIV,=P'100'                                                      
         DP    DIV,=P'15'                                                       
         ZAP   0(8,RE),DIV(12)                                                  
         LA    RE,8(RE)                                                         
         BCT   RF,PR58A                                                         
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFLINE                              
         MVC   BUFKEY+1(2),SVBUFKY                                              
         MVC   BUFACCS,SVBUFACS                                                 
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
PR60     CLI   0(R6),X'FF'         TRY TO FIND NEXT LINE                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE,LINID                                                       
         BE    PR58                                                             
         LA    R6,L'LINTAB(R6)                                                  
         B     PR60                                                             
         SPACE 1                                                                
PR62     MVC   WORK(3),WORK+1      ANY MORE IN MENU/REPORT LIST                 
         MVI   WORK+3,C' '                                                      
         CLI   WORK,C' '                                                        
         BNE   PR52                                                             
         LA    R6,PLUS                                                          
         LA    R7,MINUS                                                         
         LA    RF,43                                                            
PR64     ZAP   0(6,R6),=P'0'                                                    
         ZAP   0(6,R7),=P'0'                                                    
         LA    R6,6(R6)                                                         
         LA    R7,6(R7)                                                         
         BCT   RF,PR64                                                          
         B     XIT                                                              
         DROP  R4                                                               
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE FOR SBACFRST                                             
         SPACE 2                                                                
PR80     CLI   MODE,SBACFRST                                                    
         BNE   PR90                                                             
*&&US*&& B     XIT                                                              
         L     R4,ADSUBAC          MOVE CONTRA-A/C NAME                         
         USING TRSUBHD,R4          INTO TABLE                                   
         MVC   WORK(2),TRSBACNT+2                                               
         CLI   TRSBACNT+2,C'P'                                                  
         BNE   PR81                                                             
         MVI   WORK,C'3'                                                        
         MVC   WORK+1(1),TRSBACNT+5                                             
         CLI   TRSBACNT+6,C' '                                                  
         BE    PR81                                                             
         MVC   WORK+1(1),TRSBACNT+6   2/3 LONG DEPTS                            
PR81     DS    0H                                                               
         L     R6,ADLINTAB                                                      
         USING LINTABD,R6                                                       
         CLI   TRSBACNT+3,C' '     IGNORE FUNNIES                               
         BE    XIT                                                              
         SPACE 1                                                                
PR82     CLI   LINID,X'FF'                                                      
         BE    XIT                                                              
         CLC   LINKEY,WORK                                                      
         BE    PR84                                                             
         LA    R6,L'LINTAB(R6)                                                  
         B     PR82                                                             
         SPACE 1                                                                
PR84     SR    R3,R3                                                            
         MVC   LINHEAD,SPACES                                                   
         IC    R3,TRSBLEN                                                       
         SH    R3,=H'17'                                                        
         GOTO1 CHOPPER,DMCB,((R3),TRSBNAME),(27,LINHEAD),1                      
         SPACE 1                                                                
         B     XIT                                                              
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              ACCLAST AND SOME OTHER LAST TIMES                                
         SPACE 2                                                                
PR90     CLI   MODE,ACCLAST                                                     
         BNE   PR94                                                             
         CLI   MYBYTE,X'FF'                                                     
         BE    PR92                                                             
         MVI   REQACTIV,0                                                       
         LA    R2,3                                                             
         CLI   HILEV,3                                                          
         BL    *+8                                                              
         LA    R2,4                                                             
         GOTO1 BUFFALO,DMCB,=C'ADD',ADBUFC,1,(X'80',(R2))                       
         MVI   THISLEV,1                                                        
         BAS   RE,LEVEL                                                         
         CLI   HILEV,3                                                          
         BNE   *+10                                                             
         AP    CNUMBER,=P'1'                                                    
         CLI   HILEV,2                                                          
         BNE   PR92                                                             
         AP    BNUMBER,=P'1'                                                    
         SPACE 1                                                                
PR92     L     R4,ADACCSTA         TEST END OF GROUP                            
         USING ACSTATD,R4                                                       
         CLI   GRPMODE,C'Y'                                                     
         BNE   XIT                                                              
         CLI   ACSTCOST,C')'                                                    
         BNE   XIT                                                              
         MVI   THISLEV,2                                                        
         BAS   RE,LEVEL                                                         
         MVI   GRPMODE,C'N'                                                     
         B     XIT                                                              
         SPACE 2                                                                
PR94     CLI   MODE,LEVALAST                                                    
         BNE   PR96                                                             
         MVC   THISLEV,HILEV                                                    
         BAS   RE,LEVEL                                                         
         ZAP   BNUMBER,=P'0'                                                    
         B     XIT                                                              
         SPACE 2                                                                
PR96     CLI   MODE,LEVBLAST                                                    
         BNE   PR100                                                            
         ZIC   R1,HILEV                                                         
         BCTR  R1,0                                                             
         STC   R1,THISLEV                                                       
         BAS   RE,LEVEL                                                         
         AP    BNUMBER,=P'1'                                                    
         ZAP   CNUMBER,=P'0'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              GENERAL ROUTINE FOR END-OF-LEVELS                                
         SPACE 2                                                                
LEVEL    NTR1                                                                   
         SR    R2,R2                                                            
         IC    R2,THISLEV                                                       
         CLI   THISLEV,1                                                        
         BNE   *+12                                                             
         CLI   GRPMODE,C'Y'                                                     
         BE    *+14                                                             
         CLC   THISLEV,HILEV                                                    
         BNL   TEALL                                                            
         LA    R3,1(R2)                                                         
         GOTO1 BUFFALO,DMCB,=C'ADD',ADBUFC,(R2),(X'80',(R3))                    
         SPACE 1                                                                
TEALL    DS    0H                                                               
         MVC   FIVE(4),MENUSV                                                   
         MVI   FIVE+4,C' '                                                      
         CLI   QOPT2,C'S'                                                       
         BE    TEA40                                                            
         CLI   GRPMODE,C'Y'                                                     
         BE    TEA20                                                            
         CLI   MODE,LEVALAST                                                    
         BNE   TEA10                                                            
         CLI   QOPT2,C' '                                                       
         BNE   TEA20                                                            
         CP    BNUMBER,LOWNO                                                    
         BH    TEA20                                                            
         B     TEA40                                                            
         SPACE 1                                                                
TEA10    CLI   MODE,LEVBLAST                                                    
         BNE   TEA12                                                            
         CLI   QOPT2,C'1'                                                       
         BE    TEA40                                                            
         CP    CNUMBER,=P'0'                                                    
         BH    TEA20                                                            
         B     TEA40                                                            
         SPACE 1                                                                
TEA12    CLI   QOPT2,C' '          PROCACC                                      
         BE    TEA20                                                            
         CLI   HILEV,1                                                          
         BE    TEA20                                                            
         CLI   HILEV,2             ONLY WANT 3RD LEVEL IF                       
         BNE   TEA40               QOPT2 IS BLANK                               
         CLI   QOPT2,C'1'                                                       
         BE    TEA40                                                            
         SPACE 1                                                                
TEA20    BAS   RE,FORMAT                                                        
         MVC   FIVE(4),FIVE+1                                                   
         CLI   FIVE,C' '                                                        
         BNE   TEA20                                                            
TEA40    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(0,ADBUFC),(X'80',(R2))                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR REQLAST                                              
         SPACE 2                                                                
PR100    CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         MVC   SAVEH1,SPACES                                                    
         MVC   SAVEH1(12),=CL12'REQUEST'                                        
         MVC   SAVEH1+12(6),=C'TOTALS'                                          
         CLI   REQACTIV,X'FF'      NO ACTIVITY ON REQUEST                       
         BE    PR104                                                            
         LA    R2,3                                                             
         CLI   HILEV,3                                                          
         BL    *+8                                                              
         LA    R2,4                                                             
         MVC   FIVE(4),MENUSV                                                   
         MVI   FIVE+4,C' '                                                      
PR102    BAS   RE,FORMAT                                                        
         MVC   FIVE(4),FIVE+1                                                   
         CLI   FIVE,C' '                                                        
         BE    PR104                                                            
         MVI   FORCEHED,C'Y'                                                    
         B     PR102                                                            
PR104    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFC                                    
         CLI   QOPT2,C'S'                                                       
         BE    XIT                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         L     R1,ADWORK                                                        
         MVC   SUMREC(2),=C'/*'                                                 
         LA    R5,SUMREC                                                        
*&&DO                                                                           
         WRITE (R1),SQ,(R5)                                                     
         CHECK (R1)                                                             
*&&                                                                             
*&&OS                                                                           
         LR    R2,R1                                                            
         WRITE DECB0001,SF,(R2),(R5)                                            
         CHECK DECB0001                CHECK                                    
*&&                                                                             
         MVI   COMPBYTE,0          PERCENT MODE                                 
         BAS   RE,COMPAREM                                                      
         CLI   PROGPROF+7,C'Y'                                                  
         BNE   XIT                                                              
         MVI   COMPBYTE,1          MONEY MODE                                   
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,COMPAREM                                                      
XIT      XMOD1 1                                                                
         EJECT                                                                  
*              FIND BUDGET ELEMENTS AND SAVE                                    
         SPACE 2                                                                
BUDGET   NTR1                                                                   
         USING LINTABD,R6                                                       
         L     R4,ADACC                                                         
         AH    R4,DATADISP         FIND SOME BUDGET ELEMENTS                    
B2       CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),X'34'                                                      
         BE    B6                                                               
B4       SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     B2                                                               
         SPACE 2                                                                
         USING ACBUDGD,R4                                                       
B6       L     R6,ADLINTAB                                                      
B7       CLC   LINKEY,ACBDSBAC+2   MATCH LEDGER/ACCOUNT WITH                    
         BE    B8                  BUDGET CONTRA-ACCOUNT                        
         LA    R6,L'LINTAB(R6)                                                  
         CLI   0(R6),X'FF'         IF WE DIDN'T FIND A MATCH - GO               
         BNE   B7                  AND GET ANOTHER BUDGET ELEMENT               
         L     R6,ADLINTAB                                                      
         B     B4                                                               
         SPACE 1                                                                
         USING COLTABD,R3                                                       
B8       MVC   BUFKEY+1(3),LINID                                                
         LA    R5,PLUS                                                          
         LA    R3,COLTAB                                                        
B10      CLI   0(R3),X'FF'                                                      
         BE    B30                                                              
         CLI   0(R3),C'B'                                                       
         BNE   B24                                                              
         SPACE 1                                                                
         CLC   COLSTRT,ACBDSTRT    TRY TO FIND IF THERE IS ANY                  
         BL    B12                 OVERLAP ON BUDGET & PERIOD DATES             
         CLC   COLSTRT,ACBDEND                                                  
         BH    B24                 NO OVERLAP ON START DATES                    
         B     B14                                                              
B12      CLC   COLEND,ACBDSTRT                                                  
         BL    B24                                                              
         SPACE 1                                                                
B14      DC    0H'0'                                                            
         ZAP   DOUBLE,ACBDBUDG                                                  
         MP    DOUBLE,=P'10'                                                    
         ZAP   DUB(6),DOUBLE                                                    
         CLC   ACBDEND,COLEND      IF BUDGET FINISHES BEFORE END DATE           
         BNH   B22                 GIVE IT WHOLE OF BUDGET                      
         SPACE 1                                                                
         LA    RE,12               FIND LENGTH OF BUDGET                        
         CLC   ACBDSTRT(1),ACBDEND                                              
         BNE   *+6                                                              
         SR    RE,RE                                                            
         IC    RF,ACBDSTRT+1                                                    
         CH    RF,=H'16'                                                        
         BL    *+8                                                              
         SH    RF,=H'6'                                                         
         BCTR  RF,0                                                             
         SR    RE,RF                                                            
         IC    RF,ACBDEND+1                                                     
         CH    RF,=H'16'                                                        
         BL    *+8                                                              
         SH    RF,=H'6'                                                         
         AR    RE,RF                                                            
         CH    RE,=H'16'                                                        
         BL    *+8                                                              
         SH    RE,=H'6'                                                         
         CVD   RE,DOUBLE                                                        
         ZAP   DIV,DUB(6)                                                       
         DP    DIV,DOUBLE+6(2)                                                  
         ZAP   DUB(6),DIV(12)      ONE MONTH'S BUDGET                           
         MVI   THREE+2,X'0C'       FIND NO OF MONTHS OVERLAP                    
         MVC   THREE(2),COLSTRT                                                 
         CLC   COLSTRT,ACBDSTRT                                                 
         BH    *+10                                                             
         MVC   THREE(2),ACBDSTRT   FIND HIGHER OF TWO START DATES               
         MVI   FULL+2,X'0C'                                                     
         MVC   FULL(2),COLEND                                                   
         CLC   COLEND,ACBDEND                                                   
         BL    *+10                                                             
         MVC   FULL(2),ACBDEND     AND LOWER OF TWO END DATES                   
         ZAP   HALF,=P'1'                                                       
*                                                                               
B16      CLC   THREE(2),FULL       BUMP LOW DATE UNTIL EQUAL TO HIGH            
         BE    B20                                                              
         CLI   THREE+1,X'12'                                                    
         BNE   B18                                                              
         MVI   THREE+1,X'01'                                                    
         AP    THREE,=P'1000'      NEXT YEAR                                    
         B     B19                                                              
B18      AP    THREE,=P'10'                                                     
B19      AP    HALF,=P'1'                                                       
         B     B16                                                              
*                                                                               
B20      ZAP   DIV,DUB(6)          MULTIPLY ONE MONTHS BUDGET BY                
         MP    DIV,HALF            NO OF MONTHS OF OVERLAP                      
         ZAP   DUB(6),DIV                                                       
         SPACE 1                                                                
B22      ZAP   DIV,DUB(6)                                                       
         DP    DIV,=P'10'                                                       
         ZAP   0(6,R5),DIV(12)                                                  
B24      LA    R5,6(R5)                                                         
         LA    R3,L'COLTAB(R3)                                                  
         B     B10                                                              
         SPACE 2                                                                
B30      MVI   WORK+4,C' '                                                      
         MVC   WORK(4),MENUSV                                                   
B32      MVC   BUFKEY(1),WORK                                                   
         MVC   BUFACCS,=8PL8'0'                                                 
         SR    R7,R7                                                            
         IC    R7,WORK                                                          
         BCTR  R7,R0                                                            
         MH    R7,=H'8'                                                         
         LA    R7,SELECTAB(R7)                                                  
         LA    RF,8                                                             
         LA    R5,BUFACCS          BUILD 8X8 LINE                               
B34      LA    R3,PLUS             FROM 43X6 LINE                               
         SR    R1,R1                                                            
         IC    R1,0(R7)                                                         
         LTR   R1,R1                                                            
         BZ    B35                                                              
         BCTR  R1,R0                                                            
         MH    R1,=H'6'                                                         
         AR    R3,R1                                                            
         ZAP   0(8,R5),0(6,R3)                                                  
         LA    R7,1(R7)                                                         
         LA    R5,8(R5)                                                         
         BCT   RF,B34                                                           
         SPACE 1                                                                
B35      CLC   BUFACCS,=8PL8'0'                                                 
         BE    B36                                                              
         MVI   MYBYTE,0                                                         
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFLINE                              
B36      MVC   WORK(4),WORK+1                                                   
         CLI   WORK,C' '                                                        
         BNE   B32                                                              
         SPACE 1                                                                
         CLC   ACBDSBAC+3(12),SPACES    ADD TO TOTAL LINE ALSO                  
         BE    B40                                                              
         MVC   ACBDSBAC+3(12),SPACES                                            
B38      LA    R6,L'LINTAB(R6)     FIND TOTAL LINE FOR THIS BUDGET              
         CLI   0(R6),X'FF'         IF WE CAN                                    
         BE    B40                                                              
         CLC   LINKEY,ACBDSBAC+2                                                
         BNE   B38                                                              
         MVC   BUFKEY+1(3),LINID   NEW LINE ID                                  
         B     B30                                                              
         SPACE 1                                                                
B40      DS    0H                                                               
*&&US                                                                           
         CLI   BUFKEY+1,C'Q'                                                    
         BNE   *+12                                                             
         MVI   BUFKEY+1,C'X'                                                    
         B     B30                                                              
         CLI   BUFKEY+2,C'3'       ADD BUDGETS FOR EXPENSES/O'HEADS             
         BL    B41                 TO TOTAL LINE FOR EXPENSES/O'HEADS           
         CLI   BUFKEY+2,C'5'                                                    
         BH    B41                                                              
         MVC   BYTE,BUFKEY+2                                                    
         MVC   BUFKEY+1(3),SPACES                                               
         MVI   BUFKEY+1,C'Q'                                                    
         CLI   BYTE,C'3'                                                        
         BE    B30                                                              
         CLI   BYTE,C'4'                                                        
         BE    B30                                                              
         MVI   BUFKEY+1,C'X'                                                    
         B     B30                                                              
B41      DS    0H                                                               
*&&                                                                             
*&&UK                                                                           
         ST    R6,FULL                                                          
         SR    RF,RF                                                            
         LA    RF,L'LINTAB(RF)                                                  
         SR    R6,RF               POINT TO PREVIOUS LINE                       
         MVC   INSTRUCS,SPACES                                                  
         MVC   INSTRUCS(4),LINPARM+2                                            
         CLI   INSTRUCS,C' '                                                    
         BE    B98                                                              
         MVC   PLUS8,BUFACCS                                                    
         MVC   MINUS8,BUFACCS                                                   
         LA    RF,8                                                             
         LA    RE,MINUS8                                                        
B50      MP    0(8,RE),=P'-1'                                                   
         LA    RE,8(RE)                                                         
         BCT   RF,B50                                                           
         L     R6,FULL                                                          
B52      MVC   BYTE,INSTRUCS                                                    
         OI    BYTE,X'40'                                                       
B60      CLC   BYTE,LINID                                                       
         BE    B70                                                              
         LA    R6,L'LINTAB(R6)                                                  
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
         MVC   INSTRUCS(5),INSTRUCS+1                                           
         MVI   INSTRUCS+5,C' '                                                  
         CLI   INSTRUCS,C' '       LAST OF UP TO SIX LINE INSTRUCTIONS          
         BE    B98                                                              
         L     R6,FULL                                                          
         B     B52                                                              
B98      DS    0H                                                               
*&&                                                                             
         LA    RF,43                                                            
         LA    R3,PLUS                                                          
B100     ZAP   0(6,R3),=P'0'       ZERO PLUS AFTER EACH BUDGET                  
         LA    R3,6(R3)                                                         
         BCT   RF,B100                                                          
         B     B4                                                               
         DROP  R6                                                               
         DROP  R4                                                               
         DROP  R3                                                               
         EJECT                                                                  
*              PRINT THE TABLES                                                 
         SPACE 2                                                                
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
         LA    R7,COLTAB                                                        
         USING COLTABD,R7                                                       
         ST    R2,WORD                                                          
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFLINE,(R2)                        
         TM    DMCB+8,X'80'        EOF                                          
         BO    XIT                                                              
         CLC   FIVE(1),BUFKEY                                                   
         BNE   XIT                                                              
         CLI   QOPT3,C'Y'          INCOME REPORT OPTION                         
         BNE   F1                                                               
         CLI   BUFKEY+1,C'G'                                                    
         BH    FENDEND                                                          
F1       DS    0H                                                               
         SPACE 1                                                                
         LA    RF,8                                                             
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   SAVEH5,SPACES                                                    
         MVC   SAVEH6,SPACES                                                    
         LA    R1,SAVEH5+31                                                     
         LA    R2,SAVEH6+31                                                     
F2       SR    R3,R3                                                            
         IC    R3,0(R6)                                                         
         LTR   R3,R3                                                            
         BZ    F3                                                               
         BCTR  R3,R0                                                            
         LA    R5,L'COLTAB                                                      
         STH   R5,HALF                                                          
         MH    R3,HALF                                                          
         LA    R7,COLTAB(R3)                                                    
         MVC   0(10,R1),COLHEAD1                                                
         MVC   0(10,R2),COLHEAD2                                                
         LA    R1,10(R1)                                                        
         LA    R2,10(R2)                                                        
         LA    R7,COLTAB                                                        
         LA    R6,1(R6)                                                         
         BCT   RF,F2                                                            
         SPACE 1                                                                
F3       MVI   SPACING,2                                                        
         GOTO1 MYREPORT                                                         
         L     R6,ADLINTAB                                                      
         USING LINTABD,R6                                                       
         SPACE 2                                                                
F4       CLC   FIVE(1),BUFKEY      SAME REPORT                                  
         BNE   FEND                                                             
         JIF   BUFKEY+1,=,C'C',OR,C'G',OR,C'K',OR,C'M',OR,C'S',OR,     X        
               C'O',OR,C'Y',F4A,JUMP=N                                          
         B     PR4C                                                             
F4A      DS    0H                  ONLY WANT CERTAIN TOTAL LINES                
         LA    RE,SUMACCS                                                       
         CLI   MODE,REQLAST                                                     
         BNE   *+8                                                              
         LA    RE,AGYTOTS                                                       
         CLI   BUFKEY+1,C'S'                                                    
         BL    F4C                                                              
         LA    RF,5                                                             
         BE    F4E                                                              
         LA    RF,6                                                             
         B     F4E                                                              
F4C      DS    0H                                                               
         PACK  DUB,BUFKEY+2(1)                                                  
         CVB   RF,DUB                                                           
F4E      BCTR  RF,0                                                             
         MH    RF,=H'8'                                                         
         AR    RE,RF               POINT TO CORRECT SUMMARY COUNTER             
         LA    RF,BUFACCS          POINT TO CORRECT BUFFALO COUNTER             
         CLI   BUFKEY,4                                                         
         BE    PR4A                                                             
         LA    RF,8(RF)                                                         
         CLI   BUFKEY,2                                                         
         BE    PR4A                                                             
         LA    RF,8(RF)                                                         
         CLI   BUFKEY,3                                                         
         BE    PR4A                                                             
         LA    RF,8(RF)                                                         
         CLI   BUFKEY,8                                                         
         BE    PR4A                                                             
         LA    RF,8(RF)                                                         
         CLI   BUFKEY,1                                                         
         BE    PR4A                                                             
         LA    RF,16(RF)                                                        
PR4A     DS    0H                                                               
         ZAP   0(8,RE),0(8,RF)                                                  
PR4C     DS    0H                                                               
         CLI   PROGPROF+8,C'Y'     PROFILE OPTION TO SKIP ALL BUT               
         BNE   F4G                 TOTAL LINES                                  
         CLI   BUFKEY+3,C' '                                                    
         BE    F4G                                                              
         CLI   BUFKEY+1,C'S'                                                    
         BE    F4G                                                              
         CLI   BUFKEY+1,C'Y'                                                    
         BNE   F40                                                              
F4G      DS    0H                                                               
         CLI   PROGPROF+12,C'Y'    OPTION TO SKIP C/A 13 DETAILS                
         BNE   *+12                                                             
         CLI   BUFKEY+1,C'I'                                                    
         BE    F40                                                              
         CLI   PROGPROF+13,C'Y'    OPTION TO SKIP C/A 14 DETAILS                
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
         CLI   BUFKEY+1,C'Q'                                                    
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
F6       CLC   LINID(3),BUFKEY+1                                                
         BE    F8                                                               
         LA    R6,L'LINTAB(R6)                                                  
         B     F5                                                               
F8       MVC   P+1(27),LINHEAD                                                  
         LA    R2,P+30                                                          
         LA    R7,COLTAB                                                        
         L     R3,MYWORD           ADDRESS OF COLUMN LIST                       
         LA    RF,8                                                             
         LA    R5,BUFACCS                                                       
F10      LA    R1,L'COLTAB                                                      
         STH   R1,HALF                                                          
         IC    R1,0(R3)                                                         
         BCTR  R1,R0                                                            
         MH    R1,HALF                                                          
         LA    R7,COLTAB(R1)                                                    
         CLI   BUFKEY,3                                                         
         BNE   *+12                                                             
         BAS   RE,TYPE3FIX                                                      
         B     F22                                                              
         CLI   COLTYPE,C'P'        PERCENT CALCULATION REQUIRED                 
         BNE   F20                                                              
         SPACE 1                                                                
         ZAP   DIV,0(8,R5)                                                      
         MP    DIV,=P'10000'                                                    
         CP    DUB,=P'0'           DUB HAS BUDGET FROM PREV. CALCN              
         BE    F22                                                              
         DP    DIV,DUB+2(6)                                                     
         AP    DIV(8),=P'50'                                                    
         DP    DIV(8),=P'100'                                                   
         EDIT  (P6,DIV),(9,(R2)),MINUS=YES                                      
         CP    DUB,=P'0'                                                        
         BNE   F22                                                              
         MVC   3(7,R2),SPACES                                                   
         B     F22                                                              
         SPACE 1                                                                
F20      ZAP   DUB2,0(8,R5)                                                     
         AP    DUB2,=P'50'                                                      
         CP    DUB2,=P'0'          MINUS ROUNDING                               
         BNL   *+10                                                             
         SP    DUB2,=P'100'                                                     
         DP    DUB2,=P'100'        LOSE PENNIES                                 
         EDIT  (P6,DUB2),(10,(R2)),MINUS=YES                                    
         ZAP   DUB,0(8,R5)         RESTORE WHOLE VALUE FOR BUDGET               
F22      LA    R7,COLTAB                                                        
         LA    R5,8(R5)            ACCUMS                                       
         LA    R3,1(R3)            LINE LIST                                    
         LA    R2,10(R2)           PRINT LINE                                   
         BCT   RF,F10                                                           
         SPACE 1                                                                
         CLC   P+31(100),SPACES    NOW THE FANCY PRINTING                       
         BE    F40                                                              
         MVI   DIV,0               USE AS SPACING SWITCH                        
         MVC   P+1(27),LINHEAD                                                  
         CLI   PROGPROF+8,C'Y'     NO CHICKEN TRACKS IF COMPRESSING             
         BE    F36                                                              
         JIF   BUFKEY+1,=,C'G',OR,C'S',OR,C'Y',F34,JUMP=N                       
         CLI   BUFKEY+1,C'K'                                                    
         BE    F24                                                              
         CLI   BUFKEY+1,C'O'                                                    
         BNE   F26                                                              
F24      CLI   PROGPROF+5,C'Y'     PROFILE OPTION FOR FANCY                     
         BE    F34                 PRINTING ON TOTAL EXPENSES                   
F26      CLI   BUFKEY+1,C'Q'                                                    
         BNE   F36                                                              
         CLI   PROGPROF+6,C'Y'     FANCY PRINTING ON TIME+EXPENSE LINE          
         BE    F34                                                              
         B     F36                                                              
         SPACE 1                                                                
F34      MVC   PSECOND,P           PUT DATA ON SECOND PRINT LINE                
         MVI   DIV,X'FF'                                                        
         MVI   P+1,C'-'                                                         
*&&UK*&& MVC   P+2(109),P+1        PROPOGATE UNDERLINING                        
*&&US                                                                           
         MVC   P+2(69),P+1                                                      
         CLI   BUFKEY,2                                                         
         BE    *+10                                                             
         MVC   P+71(40),P+70                                                    
         CLI   BUFKEY,3                                                         
         BNE   *+10                                                             
         MVC   P+90(21),SPACES                                                  
*&&                                                                             
         MVC   PTHIRD,P            UNDERNEATH DATA ALSO                         
F36      CLI   P+3,C'*'            DO WE WANT TO SPACE                          
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         GOTO1 MYREPORT                                                         
         SPACE 2                                                                
F40      L     R2,WORD                                                          
         MVC   P,SPACES                                                         
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFLINE,(R2)                         
         TM    DMCB+8,X'80'                                                     
         BO    FEND                                                             
         CLI   BUFKEY+1,C'G'                                                    
         BNE   F42                                                              
         MVC   INCOME,BUFACCS                                                   
         B     F44                                                              
F42      CLI   BUFKEY+1,C'Y'       NET PROFIT AND LOSS                          
         BNE   F44                                                              
         MVC   PROFIT,BUFACCS                                                   
F44      DS    0H                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   F46                                                              
         CLI   BUFKEY+1,C'G'                                                    
         BH    FENDEND                                                          
*&&UK                                                                           
         B     F4                                                               
*&&                                                                             
F46      CLI   PROGPROF,C'Y'       IF ORDER IS INVERTED - WE MUST               
         BNE   F4                  START AGAIN AT FRONT OF TABLE                
         L     R6,ADLINTAB                                                      
         B     F4                                                               
         SPACE 2                                                                
FEND     DS    0H                                                               
         CLI   PROGPROF+15,0                                                    
         BE    *+8                                                              
         BAS   RE,TAXIT            CALCULATE POST TAX PROFIT                    
*                                  NOW DO PERCENTAGE LINE                       
         L     R6,MYWORD           A(SELECTAB)                                  
         LA    R2,P+31                                                          
         LA    R4,INCOME                                                        
         LA    R5,PROFIT                                                        
         LA    RF,8                                                             
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
         SPACE 1                                                                
FEND2    DS    0H                                                               
         SR    R1,R1               GO FROM SELECTAB ENTRY TO COLTAB             
         IC    R1,0(R6)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'25'                                                        
         LA    R7,COLTAB(R1)                                                    
         CLI   0(R7),C'P'          AND SEE IF WE SHOULD PRINT PERCENT           
         BE    FEND4                                                            
         SPACE 2                                                                
         CP    0(8,R4),=P'0'                                                    
         BNH   FEND4                                                            
         CP    0(8,R5),=P'0'       TRY TO AVOID NEGATIVE PCT                    
         BL    FEND4                                                            
         CVB   R1,0(R5)                                                         
         CVB   RE,0(R4)                                                         
         M     R0,=F'10000'        CALCULATE PCT=PROFITX100/INCOME              
         DR    R0,RE                                                            
         LH    RE,=H'50'                                                        
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         LCR   RE,RE                                                            
         AR    R1,RE                                                            
         LR    R0,R1                                                            
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(9,(R2)),MINUS=YES                                          
         SPACE 1                                                                
FEND4    LA    R6,1(R6)                                                         
         LA    R2,10(R2)                                                        
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   RF,FEND2                                                         
         SPACE 2                                                                
FEND10   DS    0H                                                               
         CLC   P+31(80),SPACES     ANY NUMBERS TO PRINT                         
         BE    FENDEND                                                          
         MVI   SPACING,2                                                        
*&&US*&& MVC   P+1(17),=C'PROFIT PERCENTAGE'                                    
*&&UK*&& MVC   P+1(25),=C'PROFIT/REVENUE PERCENTAGE'                            
         GOTO1 MYREPORT                                                         
         SPACE 2                                                                
FENDEND  CLI   MODE,REQLAST                                                     
         BE    XIT                                                              
         CLI   QOPT2,C'S'                                                       
         BE    XIT                                                              
         CLC   SUMACCS(40),=8PL8'0'                                             
         BE    XIT                                                              
         L     R1,ADWORK                                                        
         LA    R5,SUMREC                                                        
*&&DO                                                                           
         WRITE (R1),SQ,(R5)                                                     
         CHECK (R1)                                                             
*&&                                                                             
*&&OS                                                                           
         LR    R2,R1                                                            
         WRITE DECB0002,SF,(R2),(R5)                                            
         CHECK DECB0002                CHECK                                    
*&&                                                                             
         B     XIT                                                              
         EJECT                                                                  
*              PRODUCE NEW SUMMARY                                              
         SPACE 2                                                                
COMPAREM NTR1                                                                   
         LA    RF,6                                                             
         LA    RE,HEAD7                                                         
COMP1    MVC   0(L'P,RE),SPACES                                                 
         LA    RE,L'P(RE)                                                       
         BCT   RF,COMP1                                                         
         LA    RF,3                                                             
         LA    RE,P                                                             
COMP1A   MVC   0(L'P,RE),SPACES                                                 
         LA    RE,L'P(RE)                                                       
         BCT   RF,COMP1A                                                        
         L     R1,ADWORK                                                        
*&&DO                                                                           
         POINTS (R1)                                                            
         CHECK (R1)                                                             
*&&                                                                             
*&&OS                                                                           
         LR    R2,R1                                                            
         POINT (R2),TTRFIRST                                                    
         CHECK DECB0001                CHECK                                    
*&&                                                                             
         MVC   HEAD5+104(6),HDAT                                                
         MVC   P+1(14),=C'COMPANY TOTALS'                                       
         LA    RE,AGYTOTS                                                       
         LA    R2,P+44                                                          
         LA    RF,6                                                             
COMP2    ZAP   DUB2,0(8,RE)                                                     
         AP    DUB2,=P'50'                                                      
         CP    DUB2,=P'0'          MINUS ROUNDING                               
         BNL   *+10                                                             
         SP    DUB2,=P'100'                                                     
         DP    DUB2,=P'100'        LOSE PENNIES                                 
         EDIT  (P6,DUB2),(11,(R2)),MINUS=YES                                    
         LA    R2,11(R2)                                                        
         LA    RE,8(RE)                                                         
         BCT   RF,COMP2                                                         
         GOTO1 ACREPORT                                                         
         CLI   COMPBYTE,1          MONEY MODE                                   
         BE    COMP6                                                            
         SPACE 1                                                                
         MVC   P+9(3),=C'PCT'                                                   
         LA    R2,P+51                                                          
         LA    RF,6                                                             
COMP4    MVC   0(3,R2),=C'100'                                                  
         LA    R2,11(R2)                                                        
         BCT   RF,COMP4                                                         
COMP6    GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         SPACE 2                                                                
         MVC   P+1(17),=C'ACCOUNT CODE/NAME'                                    
         MVI   PSECOND+1,C'-'                                                   
         MVC   PSECOND+2(16),PSECOND+1                                          
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
COMP10   DS    0H                                                               
         L     R1,ADWORK                                                        
         LA    R5,SUMREC                                                        
*&&DO                                                                           
         READ  (R1),SQ,(R5)                                                     
         CHECK (R1)                                                             
*&&                                                                             
*&&OS                                                                           
         LR    R2,R1                                                            
         READ  DECB0003,SF,(R2),(R5)                                            
         CHECK DECB0003                CHECK                                    
*&&                                                                             
         CLC   SUMREC(2),=C'/*'                                                 
         BE    COMP20                                                           
         MVC   P+1(12),SUMNUM                                                   
         GOTO1 CHOPPER,DMCB,(36,SUMNAME),(30,P+14),(C'P',2)                     
         LA    RF,6                                                             
         LA    R2,P+50                                                          
         CLI   COMPBYTE,0                                                       
         BE    *+8                                                              
         LA    R2,P+44                                                          
         LA    R4,AGYTOTS                                                       
         LA    R5,SUMACCS                                                       
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
         EDIT  (P6,DUB2),(11,(R2)),MINUS=YES                                    
         B     COMP14                                                           
         SPACE 1                                                                
COMP13   DS    0H                  FORMAT FOR PERCENT MODE                      
         ZAP   DIV,0(8,R5)                                                      
         MP    DIV,=P'10000'                                                    
         DP    DIV,2(6,R4)                                                      
         AP    DIV(8),=P'50'                                                    
         CP    DIV(8),=P'0'                                                     
         BNL   *+10                                                             
         SP    DIV(8),=P'100'                                                   
         DP    DIV(8),=P'100'                                                   
         EDIT  (P6,DIV),(5,(R2)),MINUS=YES                                      
COMP14   LA    R2,11(R2)                                                        
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   RF,COMP12                                                        
         MVC   HEAD5+104(6),HDAT                                                
         CLC   P+50(60),SPACES                                                  
         BNE   COMP16                                                           
         MVC   P+1(50),SPACES                                                   
         B     COMP10                                                           
COMP16   DS    0H                                                               
         GOTO1 ACREPORT                                                         
         B     COMP10                                                           
         SPACE 2                                                                
COMP20   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
TAXIT    NTR1                                                                   
         MVC   PLUS8,PROFIT                                                     
         LA    RE,PROFIT           POINT TO CORRECT COLUMN TO SEE               
         CLI   BUFKEY,X'05'        IF WE MADE PROFIT OR LOSS THIS               
         BL    TX2                 PERIOD FOR THIS CLIENT                       
         CLI   BUFKEY,X'08'                                                     
         BE    TX2                                                              
         LA    RE,PROFIT+48                                                     
*                                                                               
TX2      CP    0(8,RE),=P'0'                                                    
         BNH   TX80                                                             
         L     R6,MYWORD           A(SELECTAB)                                  
         LA    R2,PLUS8                                                         
         LA    RF,8                                                             
TX10     DS    0H                                                               
         SR    R1,R1               GO FROM SELECTAB ENTRY TO COLTAB             
         IC    R1,0(R6)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'25'                                                        
         LA    R7,COLTAB(R1)                                                    
         CLI   0(R7),C'A'          AND SEE IF WE SHOULD PRINT                   
         BNE   TX40                                                             
         ZIC   R1,PROGPROF+15                                                   
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
TX50     LA    R2,8(R2)                                                         
         LA    R6,1(R6)                                                         
         BCT   RF,TX10                                                          
*                                                                               
TX80     LA    RE,P+30                                                          
         LA    R2,PLUS8                                                         
         LA    RF,8                                                             
         MVC   P+1(15),=C'POST TAX PROFIT'                                      
TX82     ZAP   DUB,0(8,R2)                                                      
         CP    DUB,=P'0'                                                        
         BE    TX84                                                             
         AP    DUB,=P'50'                                                       
         CP    DUB,=P'0'                                                        
         BNL   *+10                                                             
         SP    DUB,=P'100'                                                      
         DP    DUB,=P'100'                                                      
         ZAP   DOUBLE,DUB(6)                                                    
         EDIT  (P8,DOUBLE),(10,(RE)),MINUS=YES                                  
TX84     LA    R2,8(R2)                                                         
         LA    RE,10(RE)                                                        
         BCT   RF,TX82                                                          
         GOTO1 MYREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              DO FUNNY BUSINESS FOR REPORT 3                                   
         SPACE 2                                                                
TYPE3FIX NTR1                                                                   
         CLI   COLTYPE,C'S'        REVENUE PCT COLUMN                           
         BNE   FIX10                                                            
         CLI   BUFKEY+1,C'G'                                                    
         BNE   FIX2                                                             
         ZAP   TOTREV,0(8,R5)      TOTAL REVENUE                                
         B     XIT                                                              
         SPACE 1                                                                
FIX2     CLI   BUFKEY+1,C'E'       NO PERCENTAGE FOR BILLING/INCOME             
         BNH   XIT                                                              
FIX4     CP    TOTREV,=P'0'                                                     
         BNH   XIT                                                              
         ZAP   DIV,0(8,R5)                                                      
         MP    DIV,=P'10000'                                                    
         DP    DIV,TOTREV+2(6)                                                  
         AP    DIV(8),=P'50'                                                    
         DP    DIV(8),=P'100'                                                   
         EDIT  (P6,DIV),(10,(R2)),MINUS=YES                                     
         CP    DUB,=P'0'                                                        
         BNE   XIT                                                              
         MVC   3(7,R2),SPACES                                                   
         B     XIT                                                              
         SPACE 2                                                                
FIX10    ZAP   DUB2,0(8,R5)                                                     
         CLI   COLTYPE,C'V'        VARIANCE COLUMN                              
         BNE   FIX20                                                            
         LR    R1,R5                                                            
         SH    R1,=H'8'            ACTUAL LESS BUDGET                           
         SP    DUB2,0(8,R1)                                                     
         SPACE 1                                                                
FIX20    AP    DUB2,=P'50'                                                      
         CP    DUB2,=P'0'          MINUS ROUNDING                               
         BNL   *+10                                                             
         SP    DUB2,=P'100'                                                     
         DP    DUB2,=P'100'        LOSE PENNIES                                 
         EDIT  (P6,DUB2),(10,(R2)),MINUS=YES                                    
         B     XIT                                                              
         EJECT                                                                  
*              CHANGE PRINTING ORDER OF 13/14 POSTINGS                          
         SPACE 2                                                                
INVERT   NTR1                                                                   
         L     R6,ADLINTAB                                                      
         USING LINTABD,R6                                                       
INV2     CLI   LINID,X'FF'                                                      
         BE    XIT                                                              
         CLI   LINID,C'I'                                                       
         BE    INV6                                                             
         CLI   LINID,C'K'                                                       
         BE    INV8                                                             
INV4     LA    R6,L'LINTAB(R6)                                                  
         B     INV2                                                             
         SPACE 1                                                                
INV6     MVI   LINID,C'N'                                                       
         MVC   LINPARM(2),=C'NO'                                                
         B     INV4                                                             
         SPACE 1                                                                
INV8     MVI   LINID,C'O'                                                       
         B     INV4                                                             
         EJECT                                                                  
*              REPLACE TABLE NAMES WITH LEDGER NAMES                            
         SPACE 2                                                                
REPLACE  NTR1                                                                   
         L     R6,ADLINTAB                                                      
         USING LINTABD,R6                                                       
         MVC   SAVEH5(L'KEY),KEY   FOR RE-POSITIONING                           
         L     R2,ADCOMP           NOW SET UP KEY                               
         MVC   IOA(1),0(R2)        COMPANY                                      
         MVI   IOA+1,C'1'          UNIT                                         
         MVC   IOA+2(44),SPACES                                                 
         SPACE 1                                                                
REPL2    CLI   LINID,X'FF'                                                      
         BE    REPL20                                                           
         CLI   LINKEY+1,C' '       FIND A LEDGER VALUE                          
         BNE   REPL4                                                            
         CLI   LINKEY,C' '                                                      
         BNE   REPL6                                                            
REPL4    LA    R6,L'LINTAB(R6)                                                  
         B     REPL2                                                            
         SPACE 2                                                                
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
REPL10   CLI   0(R5),0                                                          
         BE    REPL4                                                            
         CLI   0(R5),X'20'                                                      
         BE    REPL12                                                           
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     REPL10                                                           
         SPACE 1                                                                
         USING ACNAMED,R5                                                       
REPL12   ZIC   R7,ACNMLEN                                                       
         SH    R7,=H'2'                                                         
         MVC   LINHEAD+5(22),SPACES                                             
         GOTO1 CHOPPER,DMCB,((R7),ACNMNAME),(19,LINHEAD+5),1                    
         LA    R7,LINHEAD+24                                                    
         CLI   0(R7),C' '                                                       
         BNE   *+8                                                              
         BCT   R7,*-8                                                           
         MVC   2(2,R7),=C'**'                                                   
         B     REPL4                                                            
         SPACE 2                                                                
REPL20   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',SAVEH5,IOA                       
         B     XIT                                                              
         EJECT                                                                  
*              FILL LINE-TABLE NAME FIELDS FROM FILE                            
         SPACE 2                                                                
BUDNAME  NTR1                                                                   
         L     R6,ADLINTAB                                                      
         USING LINTABD,R6                                                       
         MVC   SAVEH5(L'KEY),KEY   FOR RE-POSITIONING                           
         L     R2,ADCOMP           NOW SET UP KEY                               
         MVC   IOA(1),0(R2)        COMPANY                                      
         MVI   IOA+1,C'1'          UNIT                                         
         MVC   IOA+2(44),SPACES                                                 
BUDN2    CLI   LINID,X'FF'                                                      
         BE    BUDN20                                                           
         CLI   LINKEY+1,C' '       FIND A LINE FOR AN ACCOUNT                   
         BNE   BUDN6                                                            
BUDN4    LA    R6,L'LINTAB(R6)                                                  
         B     BUDN2                                                            
         SPACE 1                                                                
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
BUDN10   CLI   0(R5),0                                                          
         BE    BUDN4                                                            
         CLI   0(R5),X'20'                                                      
         BE    BUDN12                                                           
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     BUDN10                                                           
         SPACE 1                                                                
         USING ACNAMED,R5                                                       
BUDN12   ZIC   R7,ACNMLEN                                                       
         SH    R7,=H'2'                                                         
         MVC   LINHEAD,SPACES                                                   
         GOTO1 CHOPPER,DMCB,((R7),ACNMNAME),(27,LINHEAD),1                      
         B     BUDN4                                                            
         SPACE 1                                                                
BUDN20   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',SAVEH5,IOA                       
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE CONTROL FOR TWO PAGE PROBLEM                            
         SPACE 2                                                                
MYREPORT NTR1                                                                   
         CLI   MODE,REQLAST                                                     
         BE    MYRPT20                                                          
         L     R4,ADLDGHIR                                                      
         USING ACHEIRD,R4                                                       
         GOTO1 =V(ACCDIV),DMCB,(R4),ADACC,LEVSAVE,RR=RELO                       
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
         SPACE 2                                                                
MYRPT2   DS    0H                                                               
         L     R7,0(RE)                                                         
         MVC   0(16,R6),0(R2)      NAME OF LEVEL                                
         MVC   0(12,R4),0(R3)                                                   
         MVI   16(R6),C' '                                                      
         MVC   17(12,R6),0(R3)     CODE                                         
         MVC   30(36,R6),SPACES                                                 
         ZIC   RF,1(R7)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   30(0,R6),2(R7)      NAME                                         
         MVC   SUMNAME,SPACES                                                   
         MVC   SUMNAME,30(R6)                                                   
         LA    R2,16(R2)                                                        
         LA    R3,13(R3)                                                        
         LA    R4,12(R4)                                                        
         LA    R6,80(R6)                                                        
         LA    RE,20(RE)                                                        
         BCT   R5,MYRPT2                                                        
         GOTO1 =V(SQUASHER),DMCB,WORK,60,RR=RELO                                
         MVC   SUMNUM,WORK                                                      
         SPACE 3                                                                
MYRPT20  LA    R5,3                                                             
         LA    R6,SAVEH1                                                        
MYRPT22  DS    0H                                                               
         GOTO1 =V(SQUASHER),DMCB,0(R6),80,RR=RELO                               
         LA    R6,80(R6)                                                        
         BCT   R5,MYRPT22                                                       
         SPACE 2                                                                
         LA    R5,3                                                             
         LA    R2,HEAD5+1                                                       
         LA    R6,SAVEH1                                                        
MYRPT30  CLC   0(80,R6),SPACES                                                  
         BE    MYRPT32                                                          
         MVC   0(80,R2),0(R6)                                                   
         MVC   0(80,R6),SPACES                                                  
         LA    R2,132(R2)                                                       
         LA    R6,80(R6)                                                        
         BCT   R5,MYRPT30                                                       
         SPACE 2                                                                
MYRPT32  LA    R2,HEAD7                                                         
         CH    R5,=H'2'                                                         
         BE    MYRPT34                                                          
         LA    R2,132(R2)                                                       
         CH    R5,=H'1'                                                         
         BE    MYRPT34                                                          
         LA    R2,132(R2)                                                       
MYRPT34  MVC   0(132,R2),SAVEH5                                                 
         MVC   132(132,R2),SAVEH6                                               
         MVC   HEAD5+104(6),HDAT                                                
         CLI   QOPT4,C'Y'          SKIP IF THEY ONLY WANT SUMMARY               
         BE    XIT                                                              
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              TABLE FOR COLUMNS OF REPORT                                      
         SPACE 2                                                                
COLTAB   DS    0CL25                                                            
         DC    C'A',XL4'00',CL10'  MONTH',CL10'THIS YEAR'          1            
         DC    C'A',XL4'00',CL10'  MONTH',CL10'LAST YEAR'          2            
         DC    C'A',XL4'00',CL10'  LAST',CL10'  MONTH'             3            
         DC    C'B',XL4'00',CL10'BUDGET',CL10'FOR MONTH'           4            
         DC    C'P',XL4'00',CL10'PCT ACTUAL',CL10'VS BUDGET'        5           
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
         DC    C'P',XL4'00',CL10'PCT ACTUAL',CL10'VS BUDGET'       22           
         DC    C'A',XL4'00',CL10'   YTD',CL10'THIS YEAR'           23           
         DC    C'A',XL4'00',CL10'   YTD',CL10'LAST YEAR'           24           
         DC    C'B',XL4'00',CL10'   YTD',CL10'  BUDGET'            25           
         DC    C'P',XL4'00',CL10'PCT ACTUAL',CL10'VS BUDGET'       26           
         DC    C'A',XL4'00',CL10'12 MONTHS',CL10'THIS YEAR'        27           
         DC    C'A',XL4'00',CL10'12 MONTHS',CL10'LAST YEAR'        28           
         DC    C'B',XL4'00',CL10'12 MONTHS',CL10'  BUDGET'         29           
         DC    C'P',XL4'00',CL10'PCT ACTUAL',CL10'VS BUDGET'       30           
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
         DC    C'P',XL4'00',CL10'PCT ACTUAL',CL10'VS BUDGET'       41           
         DC    C'S',XL4'00',CL10'YTD AS PC',CL10'OF INCOME'        42           
         DC    C'V',XL4'00',CL10'YTD BUDGET',CL10' VARIANCE '      43           
         DC    X'FF'                                                            
         EJECT                                                                  
*              REPORT MENUS AND COLUMN LISTS                                    
         SPACE 2                                                                
MENUS    DC    C'1',AL1(1),CL3' '                                               
         DC    C'2',AL1(2),CL3' '                                               
         DC    C'3',AL1(3),CL3' '                                               
         DC    C'4',AL1(4),CL3' '                                               
         DC    C'5',AL1(5),CL3' '                                               
         DC    C'6',AL1(6),CL3' '                                               
         DC    C'7',AL1(7),CL3' '                                               
         DC    C'8',AL1(8),CL3' '                                               
         DC    C'A',AL1(5,6,7),C' '     MULTI-REPORT MENUS                      
         DC    C'B',AL1(5,7),CL2' '                                             
         DC    C'C',AL1(6,7),CL2' '                                             
         DC    C'D',AL1(5,6),CL2' '                                             
         DC    X'FF'                                                            
         SPACE 3                                                                
SELECTAB DC    AL1(1,2,18,19,23,24,27,28)    NO 1      DEFAULT                  
         DC    AL1(1,23,40,41,0,0,0,0)       NO 2      DFS FORMAT               
         DC    AL1(1,42,23,25,43,40,0,0)     NO 3      FCB FORMAT               
         DC    AL1(1,4,5,23,25,26,0,0)       NO 4                               
         DC    AL1(6,7,8,9,10,11,38,39)      NO 5      1ST HALF YR              
         DC    AL1(12,13,14,15,16,17,23,37)  NO 6      2ND HALF YR              
         DC    AL1(31,32,33,34,35,36,23,37)  NO 7      MOVING ANN. TOT          
         DC    AL1(23,25,26,0,0,0,0,0)       NO 8      BBDO FORMAT              
         SPACE 1                                                                
TTRFIRST DC    A(1)                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         BUFF  LINES=200,ROWS=4,COLUMNS=8,FLAVOR=PACKED,KEYLIST=(4,A)           
         EJECT                                                                  
*              DSECT FOR SPACEND                                                
         SPACE 2                                                                
AC9602D  DSECT                                                                  
RELO     DS    F                                                                
DUB2     DS    D                   EXTRA DUB                                    
MYWORD   DS    F                                                                
ADLINTAB DS    A                                                                
ADBUFC   DS    A                                                                
DATE     DS    CL2                 FOR THE MONTH OF ...                         
         DS    CL1                 FOR THE UNUSED DAY PART OF DATE              
GRPMODE  DS    CL1                                                              
PLUS     DS    43PL6                                                            
MINUS    DS    43PL6                                                            
BUFLINE  DS    0CL68                                                            
BUFKEY   DS    CL4                                                              
BUFACCS  DS    CL64                8 X PL8                                      
PLUS8    DS    CL64                                                             
MINUS8   DS    CL64                                                             
SVBUFACS DS    CL64                                                             
INSTRUCS DS    CL6                                                              
MENUSV   DS    CL4                                                              
SVBUFKY  DS    CL2                                                              
FIVE     DS    CL5                                                              
DIV      DS    CL14                WORK SPACE FOR DIVISION                      
HDAT     DS    CL6                 DATE FOR HEADLINE                            
MYBYTE   DS    CL1                 SWITCH FOR HEADLINE PRINTING                 
COMPBYTE DS    CL1                                                              
SDATE    DS    CL2                                                              
         DS    CL1                                                              
SVEND1   DS    CL2                                                              
STEND    DS    CL4                                                              
INCOME   DS    CL64                                                             
PROFIT   DS    CL64                                                             
REQACTIV DS    CL1                                                              
HILEV    DS    CL1                                                              
THISLEV  DS    CL1                                                              
BNUMBER  DS    PL3                                                              
CNUMBER  DS    PL3                                                              
LOWNO    DS    PL1                                                              
TOTREV   DS    PL8                                                              
SAVEH1   DS    CL80                                                             
SAVEH2   DS    CL80                                                             
SAVEH3   DS    CL80                                                             
SAVEH5   DS    CL132                                                            
SAVEH6   DS    CL132                                                            
LEVSAVE  DS    CL52                                                             
AGYTOTS  DS    6PL8                                                             
SUMREC   DS    0CL106                                                           
SUMNUM   DS    CL12                                                             
SUMNAME  DS    CL36                                                             
SUMACCS  DS    6PL8                                                             
IOA      DS    1050C                                                            
         EJECT                                                                  
*              DSECT FOR A LINE OF THE COLUMN TABLE                             
         SPACE 2                                                                
COLTABD  DSECT                                                                  
COLTYPE  DS    CL1                                                              
COLSTRT  DS    CL2                 YYMM (PACKED)                                
COLEND   DS    CL2                 YYMM                                         
COLHEAD1 DS    CL10                COLUMN HEADINGS                              
COLHEAD2 DS    CL10                                                             
         SPACE 3                                                                
*                                  COLTYPE VALUES ARE -                         
*                                  A=REGULAR                                    
*                                  B=BUDGET                                     
*                                  P=PERCENT OF BUDGET                          
*                                  S=PERCENT OF REVENUE                         
*                                  V=ACTUAL LESS BUDGET                         
         SPACE 3                                                                
*              DSECT FOR A LINE OF THE LINE TABLE                               
         SPACE 2                                                                
LINTABD  DSECT                                                                  
LINID    DS    CL1                 COLLATING CODE                               
LINKEY   DS    CL2                 LEDGER/ACCT FOR COSTING POSTINGS             
LINHEAD  DS    CL27                NAME OF LINE IN REPORT                       
LINPARM  DS    CL5                 6X1B GIVES CODE OF LINE FOR ADDITION         
*                                  OR SUBTRACTION (IF X'40' BIT OFF)            
         EJECT                                                                  
*              TABLE FOR REPORT LINES                                           
         SPACE 2                                                                
LINTABC  CSECT                                                                  
LINTAB   DS    0CL36                                                            
         DC    C'A1A',CL27' ',CL6'AC'             1=BILLINGS                    
         DC    C'A1B',CL27' ',CL6'AC'                                           
         DC    C'A1C',CL27' ',CL6'AC'                                           
         DC    C'A1D',CL27' ',CL6'AC'                                           
         DC    C'A1E',CL27' ',CL6'AC'                                           
         DC    C'A1F',CL27' ',CL6'AC'                                           
         DC    C'A1G',CL27' ',CL6'AC'                                           
         DC    C'A1H',CL27' ',CL6'AC'                                           
         DC    C'A1I',CL27' ',CL6'AC'                                           
         DC    C'A1J',CL27' ',CL6'AC'                                           
         DC    C'A1K',CL27' ',CL6'AC'                                           
         DC    C'A1L',CL27' ',CL6'AC'                                           
         DC    C'A1M',CL27' ',CL6'AC'                                           
         DC    C'A1N',CL27' ',CL6'AC'                                           
         DC    C'A1O',CL27' ',CL6'AC'                                           
         DC    C'A1P',CL27' ',CL6'AC'                                           
         DC    C'A1Q',CL27' ',CL6'AC'                                           
         DC    C'A1R',CL27' ',CL6'AC'                                           
         DC    C'A1S',CL27' ',CL6'AC'                                           
         DC    C'A1T',CL27' ',CL6'AC'                                           
         DC    C'A1U',CL27' ',CL6'AC'                                           
         DC    C'A1V',CL27' ',CL6'AC'                                           
         DC    C'A1W',CL27' ',CL6'AC'                                           
         DC    C'A1X',CL27' ',CL6'AC'                                           
         DC    C'A1Y',CL27' ',CL6'AC'                                           
         DC    C'A1Z',CL27' ',CL6'AC'                                           
         DC    C'A10',CL27' ',CL6'AC'                                           
         DC    C'A11',CL27' ',CL6'AC'                                           
         DC    C'A12',CL27' ',CL6'AC'                                           
         DC    C'A13',CL27' ',CL6'AC'                                           
         DC    C'A14',CL27' ',CL6'AC'                                           
         DC    C'A15',CL27' ',CL6'AC'                                           
         DC    C'A16',CL27' ',CL6'AC'                                           
         DC    C'A17',CL27' ',CL6'AC'                                           
         DC    C'A18',CL27' ',CL6'AC'                                           
         DC    C'A19',CL27' ',CL6'AC'                                           
         DC    C'C1 ',CL27'  ** TOTAL BILLING **',CL6' '                        
         DC    C'E2A',CL27' ',C'EGSY  '            2=INCOME                     
         DC    C'E2B',CL27' ',C'EGSY  '                                         
         DC    C'E2C',CL27' ',C'EGSY  '                                         
         DC    C'E2D',CL27' ',C'EGSY  '                                         
         DC    C'E2E',CL27' ',C'EGSY  '                                         
         DC    C'E2F',CL27' ',C'EGSY  '                                         
         DC    C'E2G',CL27' ',C'EGSY  '                                         
         DC    C'E2H',CL27' ',C'EGSY  '                                         
         DC    C'E2I',CL27' ',C'EGSY  '                                         
         DC    C'E2J',CL27' ',C'EGSY  '                                         
         DC    C'E2K',CL27' ',C'EGSY  '                                         
         DC    C'E2L',CL27' ',C'EGSY  '                                         
         DC    C'E2M',CL27' ',C'EGSY  '                                         
         DC    C'E2N',CL27' ',C'EGSY  '                                         
         DC    C'E2O',CL27' ',C'EGSY  '                                         
         DC    C'E2P',CL27' ',C'EGSY  '                                         
         DC    C'E2Q',CL27' ',C'EGSY  '                                         
         DC    C'E2R',CL27' ',C'EGSY  '                                         
         DC    C'E2S',CL27' ',C'EGSY  '                                         
         DC    C'E2T',CL27' ',C'EGSY  '                                         
         DC    C'E2U',CL27' ',C'EGSY  '                                         
         DC    C'E2V',CL27' ',C'EGSY  '                                         
         DC    C'E2W',CL27' ',C'EGSY  '                                         
         DC    C'E2X',CL27' ',C'EGSY  '                                         
         DC    C'E2Y',CL27' ',C'EGSY  '                                         
         DC    C'E2Z',CL27' ',C'EGSY  '                                         
         DC    C'E20',CL27' ',C'EGSY  '                                         
         DC    C'E21',CL27' ',C'EGSY  '                                         
         DC    C'E22',CL27' ',C'EGSY  '                                         
         DC    C'E23',CL27' ',C'EGSY  '                                         
         DC    C'E24',CL27' ',C'EGSY  '                                         
         DC    C'E25',CL27' ',C'EGSY  '                                         
         DC    C'E26',CL27' ',C'EGSY  '                                         
         DC    C'E27',CL27' ',C'EGSY  '                                         
         DC    C'E28',CL27' ',C'EGSY  '                                         
         DC    C'E29',CL27' ',C'EGSY  '                                         
*&&US*&& DC    C'G2 ',CL27'  ** GROSS INCOME **',CL6' '                         
*&&UK*&& DC    C'G2 ',CL27'  ** TOTAL REVENUE **',CL6' '                        
         DC    C'I3A',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3B',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3C',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3D',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3E',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3F',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3G',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3H',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3I',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3J',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3K',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3L',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3M',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3N',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3O',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3P',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3Q',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3R',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3S',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3T',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3U',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3V',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3W',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3X',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3Y',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I3Z',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I30',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I31',CL27' ',C'IKQ',X'A2E7A8'      3=EXPENSES                  
         DC    C'I32',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I33',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I34',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I35',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I36',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I37',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I38',CL27' ',C'IKQ',X'A2E7A8'                                  
         DC    C'I39',CL27' ',C'IKQ',X'A2E7A8'                                  
*&&US*&& DC    C'K3 ',CL27'  ** TOTAL DIR EXPENSE **',CL6' '                    
*&&UK*&& DC    C'K3 ',CL27'  ** TOTAL EXPENSES ** ',CL6' '                      
         DC    C'L4A',CL27' ',C'LMQ',X'A2E7A8'      4=TIME COSTS                
         DC    C'L4B',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4C',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4D',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4E',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4F',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4G',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4H',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4I',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4J',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4K',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4L',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4M',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4N',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4O',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4P',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4Q',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4R',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4S',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4T',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4U',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4V',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4W',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4X',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4Y',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L4Z',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L40',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L41',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L42',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L43',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L44',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L45',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L46',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L47',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L48',CL27' ',C'LMQ',X'A2E7A8'                                  
         DC    C'L49',CL27' ',C'LMQ',X'A2E7A8'                                  
*&&UK                                                                           
         DC    C'M4 ',CL27'  ** TOTAL TIME COSTS **',CL6' '                     
         DC    C'Q  ',CL27'TOTAL TIME AND EXPENSES',CL6' '                      
*&&                                                                             
*&&US                                                                           
         DC    C'M4 ',CL27'  ** TOTAL DIR TIME **',CL6' '                       
         DC    C'Q  ',CL27'TOTAL DIR TIME AND EXPENSE',CL6' '                   
*&&                                                                             
         DC    C'S61',CL27'  ** DIRECT MARGIN **',CL6' '                        
         DC    C'U5A',CL27' ',C'UW',X'E7A84040'                                 
         DC    C'U5O',CL27' ',C'UW',X'E7A84040'                                 
         DC    C'U50',CL27' ',C'UW',X'E7A84040'                                 
         DC    C'U51',CL27' ',C'UW',X'E7A84040'     5=OVERHEADS                 
         DC    C'U52',CL27' ',C'UW',X'E7A84040'                                 
         DC    C'U53',CL27' ',C'UW',X'E7A84040'                                 
         DC    C'U54',CL27' ',C'UW',X'E7A84040'                                 
         DC    C'U55',CL27' ',C'UW',X'E7A84040'                                 
         DC    C'U56',CL27' ',C'UW',X'E7A84040'                                 
         DC    C'U57',CL27' ',C'UW',X'E7A84040'                                 
         DC    C'U58',CL27' ',C'UW',X'E7A84040'                                 
         DC    C'U59',CL27' ',C'UW',X'E7A84040'                                 
*&&US                                                                           
         DC    C'W5 ',CL27'  ** OPERATING EXPENSE **',CL6' '                    
         DC    C'X  ',CL27'TOTAL DIRECT/INDIRECT EXP',CL6' '                    
         DC    C'Y62',CL27'PROFIT CONTRIBUTION',CL6' '                          
*&&                                                                             
*&&UK                                                                           
         DC    C'W5 ',CL27'  ** TOTAL OVERHEADS **',CL6' '                      
         DC    C'X  ',CL27'TOTAL EXPENSE/OVERHEADS',CL6' '                      
         DC    C'Y62',CL27'NET PROFIT / LOSS',CL6' '                            
*&&                                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACREP9602 07/10/14'                                      
         END                                                                    
