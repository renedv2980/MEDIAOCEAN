*          DATA SET APGHFIDMG  AT LEVEL 007 AS OF 05/01/02                      
*                                                                               
*PHASE ACHFIDMG,+0                                                              
         TITLE 'APG HOOK FOR DOREMUS REVENUE ANALYSIS REPORT'                   
ACHFIDMG CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8,RR=R5                                              
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R6,HOOKACOL         ADDR OF APG COL INFO                         
                                                                                
*------------------------------------------------------------------*            
*   APG HOOK CODE FOR DOREMUS REVENUE ANALYSIS REPORT ACREPFIDMF                
*------------------------------------------------------------------*            
         CLI   FRSTPASS,X'00'                                                   
         BNE   HOOK00              BRANCH IF NOT FIRST TIME                     
         CLI   HOOKNUM,9                                                        
         BE    HKSRT0                                                           
                                                                                
HKINIT0  LA    RE,ASAVE            START OF PRG WORK AREA                       
         LA    RF,CURREV                                                        
         SR    RF,RE                                                            
         EX    RF,HKREINIT                                                      
         B     *+10                                                             
HKREINIT XC    0(0,RE),0(RE)       CLEAR FOR MULTIPLE REQUESTS                  
                                                                                
         MVC   QSTART+4(2),=C'01'                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(1,WORK) CONVERT STRT-END                 
         MVC   MYSTART(2),WORK                                                  
         MVC   QEND+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,QEND),(1,WORK) CONVERT STRT-END                   
         MVC   MYEND(2),WORK                                                    
         MVC   QSTART+4(2),SPACES                                               
         MVC   QEND+4(2),SPACES                                                 
                                                                                
         ZIC   RE,MYEND+1          PUT END MONTH OF RQST IN RE                  
         CH    RE,=H'9'                                                         
         BNH   HKINIT2                                                          
         LA    RF,10                                                            
         CLI   MYEND+1,16          OCT                                          
         BE    HKINIT1                                                          
         LA    RF,11                                                            
         CLI   MYEND+1,17          NOV                                          
         BE    HKINIT1                                                          
         LA    RF,12               DEC                                          
HKINIT1  LR    RE,RF                                                            
                                                                                
HKINIT2  STC   RE,ENDMNTH                                                       
         LA    R3,COMPTAB                                                       
HKINIT3  CLC   0(1,R3),ENDMNTH                                                  
         BE    HKINIT4                                                          
         LA    R3,3(R3)                                                         
         B     HKINIT3                                                          
HKINIT4  ZIC   R5,1(R3)             ANNUAL CALC BYTE                            
         ZIC   RF,2(R3)             TRGT TO ACHV BYTE                           
*--------------------------------------------------------------------*          
*        SCAN APG COMPILE AREA FOR COLCOMPS-ADJUST DIVISION VALUES TO           
*        COMPLY WITH REQUEST RANGE FOR CURRENT REPORT.                          
*--------------------------------------------------------------------*          
         L     R2,AUSER            A(COMPILED APG INSTRUCTIONS)                 
HKINIT4A CLI   0(R2),X'29'         EL CODE FOR COLCOMP                          
         BE    HKINIT5                                                          
         CLI   0(R2),X'FF'         END OF COMPILED DATA                         
         BE    HOOK00                                                           
HKINIT4B ZIC   R1,1(R2)                                                         
         AR    R2,R1               NEXT EL                                      
         B     HKINIT4A                                                         
                                                                                
HKINIT5  LA    R1,4(R2)            R1=APG ARITHMETIC CODE                       
         CLI   0(R1),1             IF = IT'S THE COLCOMP FOR YTD PRINT          
         BE    HKINIT4B            NO ADJUSTMENT NEEDED                         
         CLI   0(R1),4             COLCOMP FOR ANNUAL                           
         BNE   HKINIT6             NO                                           
         STC   R5,1(R1)            YES, R5=# OF MNTS RQSTED TO DIV BY           
         B     HKINIT4B                                                         
HKINIT6  STC   RF,3(R1)            RF=#OF MONTHS BEYOND END TO DIV BY           
         B     HKINIT4B                                                         
*-------------------------------------------------------------------*           
* BYTE 1-JAN-DEC                                                                
* BYTE 2-NUMBER OF MONTHS WITHIN RQST-FOR ANNUAL CALC                           
* BYTE 3-NUMBER OF MONTHS OUTSIDE OF RQST-FOR TRGT TO ACHV CALC                 
*-------------------------------------------------------------------*           
COMPTAB  DC    X'01',X'1F',X'29'                                                
         DC    X'02',X'20',X'28'                                                
         DC    X'03',X'21',X'27'   BYTES 2/3 ARE THE APG EQUIVALENTS            
         DC    X'04',X'22',X'26'   OF 0-12 DECIMAL.                             
         DC    X'05',X'23',X'25'                                                
         DC    X'06',X'24',X'24'                                                
         DC    X'07',X'25',X'23'                                                
         DC    X'08',X'26',X'22'                                                
         DC    X'09',X'27',X'21'                                                
         DC    X'0A',X'28',X'20'                                                
         DC    X'0B',X'29',X'1F'                                                
         DC    X'0C',X'2A',X'1E'                                                
         EJECT                                                                  
*--------------------------------------------------------------------*          
* CHANGE COLUMNS FROM CURRENT YEAR TO PRIOR YEAR                                
*--------------------------------------------------------------------*          
HOOK00   CLI   HOOKNUM,X'03'                                                    
         BNE   HOOK10              COULD BE 3 OR 5                              
                                                                                
         CLI   CURRACC+7,C'2'                                                   
         BE    XIT                 FOR BUDGET IT'S SET                          
         MVI   5(R6),X'19'         REPLACE BUD1 WITH BAL0 F/CUR, PRIOR          
         CLI   CURRACC+7,C'1'      FOR CURRENT IT'S SET AT YTD                  
         BE    XIT                                                              
         MVI   2(R6),X'06'         SET PRIOR FOR -YTD                           
         B     XIT                                                              
                                                                                
HOOK10   CLI   HOOKNUM,3                                                        
         BNE   HOOKSORT                                                         
                                                                                
         CLI   CURRACC+7,C'1'    1 SIGNIFIES CURRENT YEAR                       
         BNE   HOOKA                                                            
         MVI   2(R6),X'04'       X'04' FOR FISCAL YEAR NUMBERS                  
         MVI   5(R6),X'19'       X'19' FOR BAL0 REPLACE BUD KEY                 
         B     XIT                                                              
                                                                                
HOOKA    CLI   CURRACC+7,C'3'    2 SIGNIFIES PRIOR                              
         BNE   XIT               MUST BE BUD OR HIGHER LEV                      
         MVI   2(R6),X'08'       X'08' FOR PRIOR YEAR NUMBERS                   
         MVI   5(R6),X'19'       X'19' FOR BAL0 REPLACE BUD KEY                 
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        EDIT REQUEST FOR EXPENSE DETAIL REPORT. BUILD AND PUT                  
*        TO MERGER REQUIRED LEVEL TOTALS.                                       
*--------------------------------------------------------------------*          
HOOKSORT CLI   HOOKNUM,8           HOOK FOR APG EXP DETAIL                      
         BNE   HKSRT0                                                           
         L     R4,HOOKAREC         A(SORTREC)                                   
         USING RECSORT2,R4                                                      
         L     R5,MERGER           A(MERGER)                                    
                                                                                
         CLI   SRPT,2                                                           
         BNE   HK0                                                              
         CLC   QSELECT(2),SPACES                                                
         BNE   HK0                                                              
         MVI   SOFFICE,X'90'                                                    
         MVC   SNAM1,=CL36'COMPANY TOTALS'                                      
                                                                                
HK0      LA    R1,SRECLN2                                                       
         LR    RE,R4               SAVE CURRENT SORT RECORD                     
         L     RF,ASVSORT                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
                                                                                
HK07     CLI   SRTROW6,C'1'        CURRENT YEAR                                 
         BNE   HK10                                                             
         MVI   SRTROW6,C'4'        CURRENT YEAR ACT TOT                         
         B     HK50                                                             
HK10     CLI   SRTROW6,C'2'        BUDGET                                       
         BNE   HK20                                                             
         MVI   SRTROW6,C'5'        BUDGET ACT TOT                               
         B     HK50                                                             
HK20     CLI   SRTROW6,C'3'        PRIOR YEAR                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SRTROW6,C'6'        PRIOR YEAR ACT TOT                           
                                                                                
HK50     LA    R2,3                                                             
         MVC   SRTROW5(14),SPACES                                               
         MVI   SRTROW5,X'FB'       MAKE ACCOUNT ROW SAME                        
         MVC   SNAM5,=CL36'* ACCOUNT TOTAL *'                                   
                                                                                
         BAS   R7,RECCHK           PREVENT PUT OF ZERO AMOUNT RECORDS           
         BZ    HK55                                                             
         SR    R2,R2                                                            
         B     HK60                                                             
HK55     BAS   R7,PUTSORT                                                       
         BCTR  R2,0                                                             
         CH    R2,=H'2'                                                         
         BL    HK60                                                             
                                                                                
HK60     LA    R1,SRECLN2          RESTORE ORIGINAL SORT REC                    
         LR    RF,R4                                                            
         L     RE,ASVSORT                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
         CLI   QSELECT+1,C' '                                                   
         BE    *+8                                                              
         MVI   SOFFICE,X'90'       FOR GROUP REQUEST SAME OFFICE                
         LTR   R2,R2                                                            
         BZ    XIT               RETURN TO APGUTILS TO PUT ORIGINAL REC         
                                                                                
         MVI   SRTROW2,C'9'        BUILD EXP TOT REC                            
         MVI   SRTROW3,C'9'                                                     
         MVI   SRTROW4,C'9'                                                     
         MVC   SRTROW5(14),SPACES                                               
         MVI   SRTROW5,X'FD'                                                    
         MVC   SNAM5,=CL36'* EXPENSE TOTAL *'                                   
         MVC   SNAM2,=CL36'FILLER'                                              
         MVC   SNAM3,=CL36'FILLER'                                              
         MVC   SNAM4,=CL36'FILLER'                                              
         B     HK55                                                             
         EJECT                                                                  
                                                                                
RECCHK   LA    RE,SAMT1            AVOID PROC OF SORT RECS-NO AMOUNTS-          
         LA    RF,18               MORE THAN 1 TIME.                            
RECC10   CP    0(8,RE),=P'0'                                                    
         BNE   RECC20              FOUND VAL CONTINUED PROCESSING               
         LA    RE,8(RE)                                                         
         BCT   RF,RECC10                                                        
                                                                                
         LA    R1,KEYHOLD                                                       
         LA    R6,SOFFICE          BUILD ACCOUNT KEY                            
         LA    R0,6                                                             
RECC13   MVC   0(1,R1),0(R6)                                                    
         LA    R6,16(R6)           NEXT ROW                                     
         LA    R1,1(R1)                                                         
         BCT   R0,RECC13                                                        
                                                                                
         L     R1,AACCTAB                                                       
RECC14   CLC   0(6,R1),KEYHOLD                                                  
         BE    RECC19              ALREADY IN TABLE OK TO BYPASS                
         CLI   0(R1),X'FF'                                                      
         BE    RECC15              END OF TABLE                                 
         LA    R1,6(R1)                                                         
         B     RECC14                                                           
                                                                                
RECC15   MVC   0(6,R1),KEYHOLD     ADD KEY TO TABLE AND WRITE SORTREC           
         MVI   6(R1),X'FF'                                                      
         B     RECC20                                                           
                                                                                
RECC19   LA    R0,1                SET CC NE T0 EXCLUDE RECORD                  
         B     *+6                                                              
RECC20   SR    R0,R0               SET CC INDICATING NONZERO FOUND              
         LTR   R0,R0                                                            
         BR    R7                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        CALCULATE CURRENT ANNUAL AND TRGT TO ACHV FOR EXPENSE DETAIL           
*        REPORT CLEAR CURRENT MONTHLY BUCKETS BEYOND END MONTH                  
*--------------------------------------------------------------------*          
HKSRT0   MVC   HKNO,HOOKNUM                                                     
         CLI   HOOKNUM,9           SORTOUT-CALC CURRENT ANNUAL                  
         BNE   HK5OR7                                                           
         MVI   FRSTPASS,0          REINIT FOR MULTIPLE REQUESTS                 
         L     RE,HOOKAREC         A(SORTREC)                                   
         L     R4,ASVSORT          USE THIS AREA TO HOLD CUR SORT REC           
         USING RECSORT2,R4                                                      
         LR    RF,R4                                                            
         LA    R1,SRECLN2                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
                                                                                
         CLI   SRTROW6,C'4'        CURRENT YEAR TOTAL                           
         BE    HK79                                                             
         CLI   SRTROW6,C'1'        CURRENT YEAR                                 
         BNE   HK100                                                            
                                                                                
*        PROCESS CURRENT YEAR FOR REQUESTED RANGE                               
                                                                                
HK79     ZIC   RE,ENDMNTH          HEX VAL OF END MONTH(JAN=01)                 
         LR    RF,RE               CLEAR SORT BKTS BEYOND END MNTH              
         MH    RF,=H'8'                                                         
         LA    R1,SAMT1                                                         
         AR    R1,RF               =A(1ST BKT TO CLEAR)                         
         LA    RF,12                                                            
         SR    RF,RE               =NUMBER OF BKTS TO CLEAR                     
         LTR   RF,RF                                                            
         BZ    HK90                12 MONTH REQUEST                             
HK80     ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   RF,HK80                                                          
                                                                                
HK90     ZIC   R1,ENDMNTH                                                       
         CVD   R1,DUB              CONVERT END MNTH TO PACKED                   
         ZAP   WORK(8),SAMT13      YTD                                          
         MP    WORK(8),=P'10'                                                   
         DP    WORK(8),DUB+6(2)    YTD/MNTHS OF REQUEST                         
         SRP   WORK(6),64-1,5                                                   
         ZAP   DUB(8),WORK(6)                                                   
         MP    DUB(8),=P'12'       RQST AVG. * 12 MONTHS=ANNUAL                 
         ZAP   SAMT14,DUB(8)                                                    
         B     HKS01                                                            
                                                                                
HK100    CLI   SRTROW6,C'2'        BUDGET                                       
         BE    HK101                                                            
         CLI   SRTROW6,C'5'        BUDGET TOTAL                                 
         BNE   HKS01                                                            
                                                                                
HK101    SP    SAMT18,SAMT17       TRGT TO ACHV,BUD-BAL0                        
         ZAP   WORK(8),SAMT18                                                   
         ZIC   R1,ENDMNTH                                                       
         LA    RE,12                                                            
         SR    RE,R1               12 MONTHS MINUS MONTHS WITHIN RQST           
         CVD   RE,DUB                                                           
         MP    WORK(8),=P'10'                                                   
         DP    WORK(8),DUB+6(2)    DIV BY MNTHS BEYOND ENDRQST MNTH             
         SRP   WORK(6),64-1,5                                                   
         ZAP   SAMT18,WORK(6)                                                   
                                                                                
HKS01    L     RF,HOOKAREC         A(SORTREC)                                   
         L     RE,ASVSORT          RESTORE CUR SORT REC                         
         LA    R1,SRECLN2                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
         SR    R0,R0                                                            
         B     XIT                                                              
         EJECT                                                                  
HK5OR7   CLI   HKNO,5              HOOK CALL PRIOR TO 'PUT' MERGER CALL         
         BE    HKSRT00             HOOK FOR APG DETAIL(REPORT 1)                
                                                                                
         DROP  R4                                                               
                                                                                
HKSRT00  L     R4,HOOKAREC         A(SORTREC)                                   
         USING RECSORTD,R4                                                      
         L     R5,MERGER           A(MERGER)                                    
         L     R3,ASRVDPT          A(TABLE OF SERV DEPT CODES)                  
         USING SRVDPTD,R3                                                       
         L     R2,AOFFCDS          A(TABLE OFFICE CODES)                        
         USING OFFCDSD,R2                                                       
*--------------------------------------------------------------------*          
*        PROCESS REPORT(S) FOR REQUESTED RANGE                                  
*--------------------------------------------------------------------*          
                                                                                
HKSRT12  CLI   SROW5,C'1'          IF CURRENT AMOUNT REC                        
         BNE   HKSRT12B                                                         
         ZIC   RE,ENDMNTH          HEX VAL OF END MONTH(JAN=01)                 
         LR    RF,RE               CLEAR SORT BKTS BEYOND END MNTH              
         MH    RF,=H'8'                                                         
         LA    R1,SAMNT1                                                        
         AR    R1,RF               =A(1ST BKT TO CLEAR)                         
         LA    RF,12                                                            
         SR    RF,RE               =NUMBER OF BKTS TO CLEAR                     
         LTR   RF,RF                                                            
         BZ    HKSRT12B            12 MONTH REQUEST                             
HKSRT12A ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   RF,HKSRT12A                                                      
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ADJUST SPECIAL COLUMNS-YTD, ANNUALIZED, MNTHL TRGT TO BDGT             
*--------------------------------------------------------------------*          
HKSRT12B ZAP   DUB,=P'0'                                                        
         LA    R6,SAMNT1                                                        
         LA    R7,MONLIST+4        USE DATES HERE TO CNTRL ADD LOOPS            
HKSRT221 CLC   0(2,R7),MYSTART     ADVANCE TO MYSTART DATE                      
         BE    HKSRT222                                                         
         LA    R7,6(R7)                                                         
         B     HKSRT221                                                         
HKSRT222 CLC   0(2,R7),MYEND                                                    
         BH    HKSRT224                                                         
         AP    DUB,0(8,R6)         ACCUM YTD TOT IN DUB                         
         LA    R6,8(R6)            NXT SORT REC BKT                             
         LA    R7,6(R7)            NXT MONLIST YM                               
         B     HKSRT222                                                         
HKSRT224 ZAP   SAMNT13,DUB         YTD TOT PCT VALS FOR CURR,BUD,PRIOR          
                                                                                
         CLI   SROW5,C'1'          IS IT A CURRENT AMOUNT RECORD                
         BNE   HKSRT225                                                         
         ZAP   SAMNT14,DUB         YTD TOT FOR ANNUAL                           
         B     HKSRT229                                                         
                                                                                
HKSRT225 CLI   SROW5,C'2'          IS IT A BUDGET AMOUNT RECORD                 
         BNE   HSRT228A                                                         
         ZAP   DUB,=P'0'                                                        
         LA    R6,SAMNT1                                                        
         LA    RE,12                                                            
HKSRT226 AP    DUB,0(8,R6)         ACCUM FISCAL                                 
         LA    R6,8(R6)                                                         
         BCT   RE,HKSRT226                                                      
         ZAP   SAMNT15,DUB         FISCAL ACCUM FOR BUD ANNUAL                  
         ZAP   SAMNT18,DUB         FISCAL ACCUM FOR TRGT TO ACHIEVE             
         B     HKSRT229                                                         
                                                                                
HSRT228A CLI   SROW5,C'3'          PRIOR AMOUNT REC                             
         BE    *+6                                                              
         DC    H'0'                WHAT IS THIS                                 
         ZAP   DUB,=P'0'                                                        
         LA    R6,SAMNT1                                                        
         LA    RE,12                                                            
HSRT228D AP    DUB,0(8,R6)         ACCUM -FISCAL                                
         LA    R6,8(R6)                                                         
         BCT   RE,HSRT228D                                                      
         ZAP   SAMNT16,DUB         -FISCAL ACCUM FOR BUD ANNUAL                 
                                                                                
*--------------------------------------------------------------------*          
*        FINAL TOT REC IS OPERATING INCOME. NEED TO REVERSE AMOUNT              
*        VALUES AS THEY ARE BEING SUBTRACTED VIA APG LOGIC.                     
*--------------------------------------------------------------------*          
HKSRT229 B     *+6                                                              
HKSRT231 B     *+6                                                              
                                                                                
         LA    R6,SAMNT1                                                        
         LA    RF,18               18 BKTS(COLUMNS IN APG CODE)                 
HKSRT250 MP    0(8,R6),=P'-1'                                                   
         LA    R6,8(R6)                                                         
         BCT   RF,HKSRT250                                                      
                                                                                
HKSRT260 LA    R1,TOTLEN(R1)       NEXT TOT ENTRY                               
         ST    R1,REGSAVE1                                                      
         ST    RE,REGSAVE2                                                      
                                                                                
*        THIS CALL PUTS THE TOTAL RECS                                          
                                                                                
         BAS   R7,PUTSORT                                                       
                                                                                
         L     R1,REGSAVE1                                                      
         L     RE,REGSAVE2                                                      
         BCT   RE,HKSRT231                                                      
                                                                                
HKSRT273 L     RE,ASVSORT          RESTORE ORIGINAL SORT REC AMOUNTS            
         LR    RF,R4                                                            
         LA    R1,SRECLN                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
XIT      XIT1                                                                   
         EJECT                                                                  
*--------------------------------------------------------------------*          
* DIVIDE SAVED EXPENSES BY SAVED REVENUE PLACE IN CURRENT RATIO REC             
*--------------------------------------------------------------------*          
HKCALC13 NTR1                                                                   
         LA    R1,13               FOR CALC OF 12 MONTHS + YTD BKTS             
         LA    R6,SAMNT1           A(1ST OPERATING RATIO BKT)                   
                                                                                
HKCALC20 ZAP   WORK(8),=P'0'                                                    
         CP    0(8,R3),=P'0'       CHECK FOR 0 REV                              
         BE    HKCALC50                                                         
         SRP   0(8,R2),64-2,5      ROUND PENNIES OF EXP AND REV SAVES           
         SRP   0(8,R3),64-2,5      LEAVE XTRA PLACE IN EXP FOR RNDING           
                                                                                
         ZAP   WORK(16),0(8,R2)    EXP TO DIVIDEND FLD                          
         MP    WORK(16),=P'1000'                                                
         DP    WORK(16),0(8,R3)    EXP/REV                                      
         SRP   WORK(8),64-1,5                                                   
         MP    WORK(8),=P'100'     FOR APGREPS ROUND OF PENNIES                 
         ZAP   0(8,R6),WORK(8)     QUOTIENT TO SAMNT(OPER. RATIO REC)           
                                                                                
HKCALC50 LA    R6,8(R6)            NEXT RATIO AMNT FLD                          
         LA    R2,8(R2)            NEXT EXP SAVE                                
         LA    R3,8(R3)            NEXT REV SAVE                                
         BCT   R1,HKCALC20                                                      
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ACCUMALATE REVENUE AND EXPENSE TOTALS                                  
*--------------------------------------------------------------------*          
HKRATADD LA    R2,SAMNT1                                                        
         LA    R6,13               12 MONTHS, AND YTD                           
HKRAT100 AP    0(8,R3),0(8,R2)                                                  
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R6,HKRAT100                                                      
         BR    R7                                                               
*--------------------------------------------------------------------*          
*        CALCULATE ANNUAL COLUMN FOR PRIOR AND BUDGET LINES                     
*--------------------------------------------------------------------*          
HKANNUAL LA    RF,12                                                            
         ZAP   WORK(16),=P'0'      ACCUM EXP                                    
         ZAP   WORK+16(8),=P'0'    ACCUM REV                                    
HKANN10  AP    WORK(16),0(8,R2)                                                 
         AP    WORK+16(8),0(8,R3)                                               
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   RF,HKANN10                                                       
         MP    WORK(16),=P'1000'   EXP TOT * 1000                               
         CP    WORK+16(8),=P'0'                                                 
         BNE   HKANN20                                                          
         ZAP   WORK(8),=P'0'                                                    
         B     HKANN30                                                          
HKANN20  DP    WORK(16),WORK+16(8) EXP/REV                                      
         SRP   WORK(8),64-1,5                                                   
         MP    WORK(8),=P'100'     FOR APGREPS ROUND OF PENNIES                 
HKANN30  ZAP   0(8,R6),WORK(8)     ANSWER TO ANNUAL BUCKET                      
         BR    R7                                                               
*--------------------------------------------------------------------*          
*        GOTO1 MERGER                                                           
*--------------------------------------------------------------------*          
PUTSORT  GOTO1 (R5),DMCB,=C'PUT',(R4)                                           
         BR    R7                                                               
         EJECT                                                                  
                                                                                
AMNTCHK  LA    RE,SAMNT1           AVOID PROC OF SORT RECS-NO AMOUNTS-          
         LA    RF,18               MORE THAN 1 TIME.                            
AMNT10   CP    0(8,RE),=P'0'                                                    
         BNE   AMNT20              FOUND VAL CONTINUED PROCESSING               
         LA    RE,8(RE)                                                         
         BCT   RF,AMNT10                                                        
                                                                                
         LA    R1,KEYHOLD                                                       
         MVC   0(1,R1),SRPRT       REPORT NUMBER                                
         LA    R1,1(R1)                                                         
         LA    R6,SOFFCD           BUILD ACCOUNT KEY                            
         LA    R0,5                                                             
AMNT13   MVC   0(1,R1),0(R6)                                                    
         LA    R6,16(R6)           NEXT ROW                                     
         LA    R1,1(R1)                                                         
         BCT   R0,AMNT13                                                        
                                                                                
         L     R1,AACCTAB                                                       
AMNT14   CLC   0(6,R1),KEYHOLD                                                  
         BE    AMNT19              ALREADY IN TABLE OK TO BYPASS                
         CLI   0(R1),X'FF'                                                      
         BE    AMNT15              END OF TABLE                                 
         LA    R1,6(R1)                                                         
         B     AMNT14                                                           
                                                                                
AMNT15   MVC   0(6,R1),KEYHOLD     ADD KEY TO TABLE AND WRITE SORTREC           
         MVI   6(R1),X'FF'                                                      
         B     AMNT20                                                           
                                                                                
AMNT19   LA    R0,1                SET CC NE T0 EXCLUDE RECORD                  
         B     *+6                                                              
AMNT20   SR    R0,R0               SET CC INDICATING NONZERO FOUND              
         LTR   R0,R0                                                            
         BR    R7                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        LITERAL POOL                                                           
*--------------------------------------------------------------------*          
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        LOCAL WORKING STORAGE                                                  
*--------------------------------------------------------------------*          
ASAVE    DS    F                   GENERAL SAVE FLD                             
MYSTART  DS    PL2                 START YYMM                                   
MYEND    DS    PL2                 END YYMM                                     
MYKYSV   DS    CL49                SAVED BDG KEY                                
BDGLEN   DS    F                   LENGTH OF BDGT ENTRY                         
SVCD     DS    C                   SAVE FOR CURRENT OFFICE                      
SVAPGKY  DS    CL49                SAVED APG CURRENT KEY                        
RATIOSW  DS    C                   INDICATES 1ST PASS-BLD RATIO DUMMIES         
ABDGTBL  DS    F                   A(BUDGET TABLE)                              
ASVSORT  DS    F                   A(SAVE AREA)                                 
AOFFCDS  DS    F                   A(OFF. CODES-NAMES)                          
AACCTAB  DS    A                   A(ACCTAB)                                    
ASRVDPT  DS    A                   A(SERV. DEPT. CODES-NAMES)                   
AGROUPS  DS    A                   GROUPS OF BRANCHES FOR A REQUEST             
CORATIO  DS    C                   OPER. RATIO FOR CO. TOTS                     
HKNO     DS    C                   SAVED HOOK NUMBER                            
REGSAVE1 DS    F                   GENERAL SAVES                                
REGSAVE2 DS    F                                                                
KEYHOLD  DS    CL6                                                              
ENDMNTH  DS    C                   END MONTH HEX VAL                            
TBLDUN   DS    C                   GROUP RQST TBL CONVERTED INDICATOR           
MNTHCNT  DS    C                   COUNT OF MONTHS FOR RQST                     
FRSTPASS DS    C                                                                
AGRP     DS    F                   A(REQUESTED GROUP)                           
DTL      DS    C                   REGULATE SERV DEPT PGS & ALLOCATION          
TOT      DS    C                                                                
SRVRQST  DS    C                   GROUP OR SPECIFIC SERVICE DEPT.              
WASGRP   DS    C                   GRP RQST INDICATOR                           
SVBAL0   DS    PL8                 SAVED YTD TOT TRGT TO ACHV CALC              
*                                                                               
CURREV   DS    13PL8               CUR REV AMNT SAVES-12 MNTS, YTD              
BDGREV   DS    13PL8               BDG REV-12 MNTHS, YTD                        
PRIREV   DS    13PL8               PRIOR REV-12 MNTHS, YTD                      
*                                                                               
REVTOEXP EQU   *-CURREV                                                         
*                                                                               
CUREXP   DS    13PL8               CUR EXP AMNT SAVES-12 MNTS, YTD              
BDGEXP   DS    13PL8               BDG EXP-12 MNTHS, YTD                        
PRIEXP   DS    13PL8               PRIOR EXP-12 MNTHS, YTD                      
*                                                                               
SVBAL01  DS    PL8                 SAVED ACCUMS FOR TGRT ACHV CALC              
SVBAL02  DS    PL8                                                              
*                                                                               
         EJECT                                                                  
TOTRECS  DC    CL3'291',CL36'SERVICE DEPARTMENT TOTALS' SEE TOTRECD             
         DC    CL3'33X',CL36'OPERATING EXPENSE TOTALS'                          
         DC    CL3'41X',CL36'OPERATING INCOME'                                  
*                                                                               
RATIOVAL DC    C'1',CL36'CURRENT MONTH' SEE RVALD                               
         DC    C'2',CL36'BUDGET'                                                
         DC    C'3',CL36'PRIOR YEAR'                                            
         DC    X'FF'                                                            
*                                                                               
BDGTBL   DS    150CL250            SEE BDGTBLD                                  
BTBLEN   EQU   *-BDGTBL            ENTRY FOR EACH OFF-SRV COMP. +6              
         DC    X'FF'                                                            
SVSORT   DS    CL460               SAVED SORT REC                               
SVSORTLN EQU   *-SVSORT            EQUALS SIZE OF RECSORT2(RPRT 3 REC)          
*                                                                               
ACCTAB   DS    6000CL6                                                          
         EJECT                                                                  
*                                                                               
RECSORTD DSECT COVERS SORT RECORD PASSED FROM ACAPGUTILS FOR RPRTS 1, 2         
SRPRT    DS    C                   REPORT NUMBER                                
         DS    C                                                                
SOFFCD   DS    C                   ROW 1 OFFICE CODE                            
         DS    CL15                                                             
ROWTOROW EQU   *-SOFFCD                                                         
SROW2    DS    C                   ROW 2 ACCT CODE FROM SUPER LEDGER            
         DS    CL15                                                             
SROW3    DS    C                   ROW 3 ACCT CODE                              
         DS    CL15                                                             
SROW4    DS    C                   ROW 4 ACCT CODE                              
         DS    CL15                                                             
SROW5    DS    C                   ROW 5 ACCT CODE                              
         DS    CL17                                                             
SNM1     DS    CL36                OFFICE CODE NAME                             
NMTONM   EQU   *-SNM1                                                           
SNM2     DS    CL36                ROW 2 NAME                                   
SNM3     DS    CL36                ROW 3 NAME                                   
SNM4     DS    CL36                ROW 4 NAME                                   
SNM5     DS    CL36                ROW 5 NAME                                   
TOAMNT   EQU   *-RECSORTD                                                       
SAMNT1   DS    PL8   JAN           18 PACKED BKTS(DICTATED BY APG CODE)         
SAMNT2   DS    PL8   FEB                                                        
SAMNT3   DS    PL8   MAR                                                        
SAMNT4   DS    PL8   APR                                                        
SAMNT5   DS    PL8   MAY                                                        
SAMNT6   DS    PL8   JUN                                                        
SAMNT7   DS    PL8   JUL                                                        
SAMNT8   DS    PL8   AUG                                                        
SAMNT9   DS    PL8   SEP                                                        
SAMNT10  DS    PL8   OCT                                                        
SAMNT11  DS    PL8   NOV                                                        
SAMNT12  DS    PL8   DEC                                                        
SAMNT13  DS    PL8   YTD TOT-CURRENT, BUD, OR PRIOR                             
SAMNT14  DS    PL8   YTD TOT-CURRENT                                            
SAMNT15  DS    PL8   FISCAL-BUD                                                 
SAMNT16  DS    PL8   -FISCAL-PRIOR                                              
SAMNT17  DS    PL8   YTD TOT-BUD                                                
SAMNT18  DS    PL8   FISCAL-BUD                                                 
SRECLN   EQU   *-RECSORTD                                                       
*                                                                               
RECSORT2 DSECT COVERS SORT RECORD PASSED FROM ACAPGUTILS FOR REPORT 3           
SRPT     DS    C                   REPORT NUMBER                                
         DS    C                                                                
SOFFICE  DS    C                   ROW 1 OFFICE CODE                            
         DS    CL15                                                             
SRTROW2  DS    C                   ROW 2 ACCT CODE FROM SUPER LEDGER            
         DS    CL15                                                             
SRTROW3  DS    C                   ROW 3 ACCT CODE                              
         DS    CL15                                                             
SRTROW4  DS    C                   ROW 4 ACCT CODE                              
         DS    CL15                                                             
SRTROW5  DS    C                   ROW 5 ACCT CODE                              
         DS    CL15                                                             
SRTROW6  DS    C                   ROW 6 ACCT CODE                              
         DS    CL17                                                             
SNAM1    DS    CL36                OFFICE CODE NAME                             
SNAM2    DS    CL36                ROW 2 NAME                                   
SNAM3    DS    CL36                ROW 3 NAME                                   
SNAM4    DS    CL36                ROW 4 NAME                                   
SNAM5    DS    CL36                ROW 5 ACCOUNT NAME                           
SNAM6    DS    CL36                ROW 6 NAME                                   
SAMT1    DS    PL8   JAN           18 PACKED BKTS(DICTATED BY APG CODE)         
SAMT2    DS    PL8   FEB                                                        
SAMT3    DS    PL8   MAR                                                        
SAMT4    DS    PL8   APR                                                        
SAMT5    DS    PL8   MAY                                                        
SAMT6    DS    PL8   JUN                                                        
SAMT7    DS    PL8   JUL                                                        
SAMT8    DS    PL8   AUG                                                        
SAMT9    DS    PL8   SEP                                                        
SAMT10   DS    PL8   OCT                                                        
SAMT11   DS    PL8   NOV                                                        
SAMT12   DS    PL8   DEC                                                        
SAMT13   DS    PL8   YTD TOT-CURRENT, BUD, OR PRIOR                             
SAMT14   DS    PL8   YTD TOT-CURRENT                                            
SAMT15   DS    PL8   FISCAL-BUD                                                 
SAMT16   DS    PL8   -FISCAL-PRIOR                                              
SAMT17   DS    PL8   YTD TOT-BUD                                                
SAMT18   DS    PL8   FISCAL-BUD                                                 
SRECLN2  EQU   *-RECSORT2                                                       
*                                                                               
*                                                                               
SRVDPTD  DSECT COVERS SRVDPT(SERVICE DEPARTMENT TABLE)                          
SRVCD    DS    C                   SERV. DEPT. CODE                             
SACCT1   DS    C              SERV. DEPT. ACCT. CODE-FROM SUPER LEDGER          
SACCT2   DS    C                                                                
SACCT3   DS    C                                                                
SRVNAME  DS    CL36                DEPT NAME                                    
SRVLEN   EQU   *-SRVDPTD                                                        
*                                                                               
*                                                                               
OFFCDSD  DSECT COVERS OFFICE TABLE                                              
OFFCD    DS    C                   OFFICE CODE                                  
OFFNAME  DS    CL36                OFFICE NAME                                  
OFFLEN   EQU   *-OFFCDSD                                                        
*                                                                               
*                                                                               
BDGTBLD  DSECT COVERS BUDGET TABLE                                              
BDGOFF   DS    C                   OFFICE                                       
BDGSRV   DS    C                   SERVICE DEPARTMENT                           
BDGDAT   DS    PL2                 YYMM                                         
BDGAMNT  DS    PL8                 AMOUNT                                       
* 24 DT/AMNT PAIRS FOR EACH OFF-SRV COMBINATION                                 
*                                                                               
TOTRECD  DSECT COVERS TOTREC TABLE                                              
TACCT1   DS    C                   SORT (ROW) KEY ACCT                          
TACCT2   DS    C                                                                
TACCT3   DS    C                                                                
TOTNAME  DS    CL36                TOT LEVEL NAME                               
TOTLEN   EQU   *-TOTRECD                                                        
*                                                                               
RVALD    DSECT COVERS DEFINED VALUES FOR OPER. RATIO REC                        
RVALCD   DS    C                                                                
RVALNM   DS    CL36                                                             
RVALLN   EQU   *-RVALD                                                          
*                                                                               
         EJECT                                                                  
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007APGHFIDMG 05/01/02'                                      
         END                                                                    
