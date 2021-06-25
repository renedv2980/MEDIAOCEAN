*          DATA SET ACINQ03    AT LEVEL 017 AS OF 05/01/02                      
*PHASE T60603A,*,NOAUTO                                                         
         TITLE 'ACCOUNT ENQUIRY MK2 - MONTH - T60603'                           
T60603   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR TYPE 'MONTH' IN ACCOUNT ENQUIRY PROGRAM               
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60603)                                               
         DC    A(FILTABLE-T60603)                                               
         DC    A(KNTRYPNT-T60603)                                               
         DC    A(FNTRYPNT-T60603)                                               
         DC    A(DNTRYPNT-T60603)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS+1)                                              
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS+2)                                              
         DC    AL2(EDITACC-GWS)                                                 
*                                                                               
         DC    CL10'THE REST'                                                   
         DC    C' '                                                             
         DC    C'V'                                                             
         DC    X'00'                                                            
         DC    AL1(15)                                                          
         DC    AL1(27)                                                          
         DC    AL2(SPACES-GWS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'START'                                                      
         DC    CL2'ST'                                                          
         DC    X'24'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(AHST-GWS)                                                    
         DC    AL1(TRHSYEAR-TRHISTD)                                            
         DC    AL1(5)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITSTRT-GWS)                                                
*                                                                               
         DC    CL10'END'                                                        
         DC    CL2'EN'                                                          
         DC    X'24'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(AHST-GWS)                                                    
         DC    AL1(TRHSYEAR-TRHISTD)                                            
         DC    AL1(5)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITEND-GWS)                                                 
*                                                                               
         DC    CL10'BUCKTYPE'                                                   
         DC    CL2'BT'                                                          
         DC    X'18'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYREF-ACKEYD)                                             
         DC    AL1(2)                                                           
         DC    AL2(EDITOPT1-GWS)   SET OPTIONS X'01' BIT                        
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'WORKCODE'                                                   
         DC    CL2'WC'                                                          
         DC    X'18'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYWRK-ACKEYD)                                             
         DC    AL1(2)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
KNTRYPNT DS    0H                                                               
FNTRYPNT DS    0H                                                               
         EJECT                                                                  
*              MAIN PROCESS                                                     
         SPACE 1                                                                
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ3**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60603,RB                                                        
         USING T606TWA,RA                                                       
         L     R8,ALOCAL                                                        
         USING LOCALD,R8                                                        
         L     R7,ATIA             R7 = A(TABLE ENTRY)                          
         USING TABLED,R7                                                        
         SPACE 1                                                                
         CLI   LASTKMK,0           CONTINUATION OR PREVIOUS SCREEN              
         BNE   TDISP                                                            
         SPACE 1                                                                
TFIRST   CLI   VIRGIN,C'2'         FIRST REC - INITIALISE TABLE & COUNT         
         BE    TUPDATE                                                          
         XC    SVDATE,SVDATE                                                    
         MVI   TABLE,X'FF'                                                      
         MVC   RECNUM,=H'1'                                                     
         XC    COUNT,COUNT                                                      
         ZAP   DEBIT,=P'0'                                                      
         ZAP   CREDIT,=P'0'                                                     
         MVI   STATUS,0                                                         
         MVI   VIRGIN,C'2'                                                      
         B     TUPDATE                                                          
         EJECT                                                                  
*              UPDATE TABLE OF YEAR/MONTH/DEBITS/CREDITS                        
         SPACE 1                                                                
TUPDATE  L     R9,AIO                                                           
         USING ACKEYD,R9                                                        
         CLI   ACKEYACC,RUNLAST    NO MORE RECORDS TO READ                      
         BE    TDISP                                                            
         SPACE 1                                                                
T1       TM    OPTIONS,X'01'       IF WE ARE NOT FILTERING ON BUCKTYPE          
         BO    T2                                                               
         CLI   ACKEYREF,C' '       BUCKTYPE MUST BE SPACE                       
         BNE   TNEXT                                                            
         SPACE 1                                                                
T2       DS    0H                  EACH REC - UPDATE TABLE FROM ELEMS.          
         MVI   ELCODE,X'55'        FIRST GET 55 EL.                             
         BAS   RE,GETEL                                                         
         BE    T3B                                                              
T2B      MVI   ELCODE,X'45'        AND THEN 45'S                                
         L     R9,AIO              RESTORE R9                                   
         USING TRHISTD,R9                                                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
T3       BAS   RE,NEXTEL                                                        
         BNE   TNEXT                                                            
T3B      ST    R9,AHST                                                          
         CLC   ELCODE,LASTEL                                                    
         BE    *+8                 DON'T FIX IF SAME AS LAST                    
         BAS   RE,CHADSP     FIDDLE WITH DISPLACEMENT FOR DATE CHECKING         
         CLI   FILTAB,X'FF'         DON'T GO TO FILTER ROUTINE                  
         BE    T3B1                 IF NO FILTERS INPUT                         
         MVI   DMCB,0              ELEMENT FILTERING                            
         L     RF,AFILTER                                                       
         BASR  RE,RF                                                            
         BZ    T3X                                                              
T3B1     MVC   DATE,TRHSYEAR                                                    
         CLI   ELCODE,X'55'                                                     
         BNE   *+10                                                             
         MVC   DATE,ACPBKHI-ACPBKD(R9) USE END DATE FROM 55                     
         LH    R4,RECNUM           NUMBER SO FAR                                
         L     R7,ATIA             A(TABLE)                                     
T3C      CLC   TABDATE,DATE                                                     
         BE    T3E                 FOUND BUCKET FOR THIS MONTH                  
         CLI   0(R7),X'FF'         IS IT END OF TABLE                           
         BE    T3D                                                              
         LA    R7,TABLEN(R7)                                                    
         BCT   R4,T3C                                                           
*                                                                               
T3D      LH    R4,RECNUM                                                        
         CH    R4,=H'121'                                                       
         BE    TNEXT               TABLE FULL                                   
         AH    R4,=H'1'            UPDATE COUNT                                 
         STH   R4,RECNUM                                                        
         MVC   TABDATE,DATE        SET UP NEW ENTRY                             
         MVI   TABSTAT,0                                                        
         ZAP   TABDR,=P'0'                                                      
         ZAP   TABCR,=P'0'                                                      
         MVI   TABLEN(R7),X'FF'        NEW END OF TABLE                         
*                                                                               
T3E      CLI   ELCODE,X'55'                                                     
         BE    T3F                                                              
         AP    TABDR,TRHSDR                                                     
         AP    TABCR,TRHSCR                                                     
         B     T3X                                                              
         SPACE 1                                                                
         USING ACPBKD,R9                                                        
T3F      AP    TABDR,ACPBKDR                                                    
         AP    TABCR,ACPBKCR                                                    
         OI    TABSTAT,X'80'       SET STATUS BIT                               
         CLC   SVDATE,ACPBKHI                                                   
         BH    *+10                                                             
         MVC   SVDATE,ACPBKHI      AND SAVE LASTEST END DATE                    
         SPACE 1                                                                
T3X      CLI   ELCODE,X'55'                                                     
         BE    T2B                                                              
         B     T3                                                               
         EJECT                                                                  
*              SET UP DISPLAY SCREEN FROM TABLE                                 
         SPACE 1                                                                
TDISP    CLI   TABLE,X'FF'                                                      
         BE    TXIT                                                             
         MVI   VIRGIN,C'H'                                                      
         CLI   LINE+1,3                                                         
         BE    T32                                                              
         LA    R6,INFDAT2H                                                      
         GOTO1 EDITACNM            SET UP ACCOUNT & CONTRA NAMES                
         MVC   INFDAT3,HEADING     AND HEADING                                  
         OI    INFDAT3H+6,X'80'                                                 
         MVI   LINE+1,3                                                         
T32      MVI   LASTKMK,0                                                        
         LH    R5,COUNT            R5 = COUNT OF TABLE ENTRIES DISPLAYD         
         LA    R6,INFDAT4H         R6 = A(LINE HEADER)                          
         USING LINED,R6                                                         
         ZAP   DEBIT,=P'0'                                                      
         ZAP   CREDIT,=P'0'                                                     
         CLC   INFKEY(4),=C'PREV'  ADJUST COUNT IN R5 FOR PREVIOUS              
         BNE   T4                                                               
         SR    R2,R2                                                            
         LR    R3,R5                                                            
         D     R2,=F'12'                                                        
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         LA    R2,12                                                            
         LA    R2,12(R2)                                                        
         SR    R5,R2                                                            
         B     T4                                                               
HEADING  DC    CL39'MONTH                   DEBITS'                             
         DC    CL39'        CREDITS             DIFFERENCE'                     
         SPACE 1                                                                
T4       LTR   R5,R5               IF THIS IS A CONTINUATION SCREEN             
         BZ    T6                  DISPLAY B/F TOTALS                           
         LA    R3,1                                                             
         LR    R4,R3                                                            
T5       AP    DEBIT,TABDR                                                      
         AP    CREDIT,TABCR                                                     
         LA    R7,TABLEN(R7)                                                    
         BXLE  R3,R4,T5                                                         
         MVC   LINEDATA(3),=C'B/F'                                              
         LA    R4,DEBIT                                                         
         BAS   RE,DISPVALS                                                      
         MVI   SVDATE+1,0     ENSURE NO SAVED DATE (ONLY OK 1ST SCRN)           
         B     T6A                                                              
         SPACE 1                                                                
T6       LH    R4,RECNUM           FIRST TIME SORT THE TABLE                    
         GOTO1 VQSORT,DMCB,(0,ATIA),(R4),19,2,0                                 
         CLI   SVDATE,0            WERE THERE ANY 55'S                          
         BE    *+8                                                              
         BAS   RE,ADJTAB           YES, ADJUST TABLE                            
*                                                                               
T6A      LA    R6,LINELEN(R6)      DISPLAY A MONTH'S TOTALS                     
T6B      OC    TABDATE,TABDATE     SKIP THIS ENTRY IF NO DATE                   
         BNZ   *+16                                                             
         LA    R7,TABLEN(R7)       TRY NEXT                                     
         LA    R5,1(R5)            BUMP TABLE ENTRY COUNT                       
         B     T6B                                                              
         CLI   TABLE,X'FF'                                                      
         BE    TOTALS                                                           
         CLC   LINE,=H'15'                                                      
         BE    TFULL                                                            
         MVC   WORK(2),TABDATE                                                  
         MVI   WORK+2,1                                                         
         GOTO1 VDATCON,DMCB,(1,WORK),(9,LINEDATA)                               
         CLI   SVDATE+1,0          WAS THERE A SAVE DATE                        
         BE    *+14                                                             
         MVC   LINEDATA+6(8),=C' && PRIOR' PRINT FIRST TIME ONLY                
         MVI   SVDATE+1,0                                                       
         LA    R4,TABDR                                                         
         BAS   RE,DISPVALS                                                      
         AP    DEBIT,TABDR                                                      
         AP    CREDIT,TABCR                                                     
         LA    R5,1(R5)            BUMP COUNT                                   
         LA    R7,TABLEN(R7)            TABLE                                   
         LH    R1,LINE                  SCREEN LINE                             
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         B     T6A                                                              
         EJECT                                                                  
*              ROUTINE TO ADJUST DISPLACEMENT TO DATE FILTERS                   
         SPACE 2                                                                
         USING FTBD,RF                                                          
CHADSP   DS    0H                                                               
         MVC   LASTEL,0(R9)                                                     
         LA    RF,FILTAB                                                        
CHAD2    CLI   FTBELMNT,X'FF'      END OF TABLE                                 
         BER   RE                                                               
         TM    FTBSTAT,X'24'       IS THIS A DATE FILTER                        
         BO    *+12                                                             
CHAD4    LA    RF,FTBTBLEN(RF)                                                  
         B     CHAD2                                                            
         MVI   FTBDISP,TRHSYEAR-TRHISTD  DEFAULT IS 45 EL.                      
         CLI   0(R9),X'55'                                                      
         BNE   CHAD4                                                            
         MVI   FTBDISP,ACPBKHI-ACPBKD    ELSE USE END DATE FROM 55 EL.          
         B     CHAD4                                                            
         SPACE 2                                                                
*              ROUTINE TO ADJUST TABLE ENTRIES WHEN 55 ELS. PRESENT             
         SPACE 2                                                                
ADJTAB   NTR1                                                                   
         MVC   WORK(16),=2PL8'0'                                                
ADJ2     CLI   TABLE,X'FF'         END OF TABLE                                 
         BE    ADJX                                                             
         CLC   TABDATE,SVDATE      IF TABLE ENTRY LESS THAN SAVED DATE          
         BE    ADJX                                                             
         AP    WORK(8),TABDR       ADD TO ACCUMS.                               
         AP    WORK+8(8),TABCR                                                  
         XC    TABDATE,TABDATE     CLEAR KEY                                    
         MVC   TABDR(16),=2PL8'0'  AND ACCUMS                                   
         LA    R7,TABLEN(R7)       AND SKIP TO NEXT                             
         B     ADJ2                                                             
ADJX     AP    TABDR,WORK(8)       WHEN LAST IS FOUND                           
         AP    TABCR,WORK+8(8)     ADD TO ITS ACCUMS                            
         MVI   SVDATE,0            AND CLEAR SAVED YEAR                         
         B     TXIT                                                             
         EJECT                                                                  
*              HANDLE SCREEN FULL AND END CONDITIONS                            
         SPACE 1                                                                
TFULL    MVC   LINEDATA(3),=C'C/F' IF SCREEN FULL AND MORE TO COME              
         LA    R4,DEBIT            DISPLAY C/F TOTALS                           
         BAS   RE,DISPVALS                                                      
         OI    LASTKMK,X'80'       CONTINUATION BIT                             
T8       AP    DEBIT,TABDR         CALCULATE GRAND TOTALS                       
         AP    CREDIT,TABCR                                                     
         LA    R7,TABLEN(R7)                                                    
         CLI   TABLE,X'FF'                                                      
         BNE   T8                                                               
         SPACE 1                                                                
TOTALS   STH   R5,COUNT            DISPLAY GRAND TOTALS                         
         C     R5,=F'12'                                                        
         BNH   *+8                                                              
         OI    LASTKMK,X'40'       PREVIOUS SCREEN MARKER                       
         LA    R6,2*LINELEN(R6)                                                 
         MVC   LINEDATA(5),=C'TOTAL'                                            
         LA    R4,DEBIT                                                         
         BAS   RE,DISPVALS                                                      
         CLI   LASTKMK,0           COULD THERE BE A 'PREV' OR 'NEXT'            
         BE    TXIT                ENQUIRY                                      
         GOTO1 AWRITIA             IF SO SAVE TABLE                             
         MVI   LINE+1,3                                                         
         TM    LASTKMK,X'80'                                                    
         BZ    TXIT                                                             
         LNR   RF,RF               CC = NEQ FOR SCREEN FULL                     
         B     TXIT                                                             
         SPACE 1                                                                
TNEXT    TM    OPTIONS,X'01'       IF NO BUCKTYPE FILTER                        
         BO    TNEXT1              FORCE READ HIGH FOR NEXT CONTRA              
         MVI   KEY+32,X'FF'                                                     
         MVI   KEY+41,X'3F'                                                     
         MVI   DMARK,1                                                          
TNEXT1   LTR   RB,RB               CC = POS FOR NEXT RECORD PLEASE              
         SPACE 1                                                                
TXIT     XIT1                      CC = EQU FOR END                             
         EJECT                                                                  
*              DISPLAY A LINE OF VALUES                                         
*              ON ENTRY R4 = A(DEBIT/CREDIT PAIR OF PL8 VALUE FIELDS)           
*                       R6 = A(LINE HEADER) COVERED BY DSECT LINED              
         SPACE 1                                                                
DISPVALS OI    LINEHDR+6,X'80'                                                  
         LA    RF,LINEDATA-2       RF=NOTIONAL COLUMN ZERO ON SCREEN            
         ZAP   DUB,0(8,R4)                                                      
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),DUB                                                     
         MVC   19(14,RF),WORK+4                                                 
         ZAP   DUB,8(8,R4)                                                      
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),DUB                                                     
         MVC   43(14,RF),WORK+4                                                 
         ZAP   WORK(8),0(8,R4)                                                  
         SP    WORK(8),8(8,R4)                                                  
         ZAP   DUB,WORK(8)                                                      
         MVC   WORK+20(18),=X'40202020202020202020202020214B202060'             
         ED    WORK+20(18),DUB                                                  
         MVC   66(14,RF),WORK+24                                                
         BR    RE                                                               
         EJECT                                                                  
         GETEL R9,DATADISP,ELCODE                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
LOCALD   DSECT                                                                  
RECNUM   DS    H         X         NUMBER OF TABLE ENTRIES SO FAR               
SVDATE   DS    CL2       P         LATEST END MONTH FROM 55                     
DATE     DS    CL2       P         YEAR & MONTH                                 
STATUS   DS    CL1                                                              
DEBIT    DS    PL8       P         DEBIT VALUE OR TOTAL                         
CREDIT   DS    PL8       P         CREDIT VALUE OR TOTAL                        
LASTEL   DS    CL1                                                              
         SPACE 1                                                                
*              DSECT TO COVER A TABLE ENTRY IN TIA                              
TABLED   DSECT                                                                  
TABLE    DS    0C                                                               
TABDATE  DS    CL2       P         YEAR & MONTH                                 
TABSTAT  DS    CL1       X         X'80' = ENTRY FROM 55 ELEMENT                
TABDR    DS    PL8       P         DEBIT                                        
TABCR    DS    PL8       P         CREDIT                                       
TABLEN   EQU   *-TABLED                                                         
         SPACE 1                                                                
*                                                                               
*              NESTED INCLUDE FOR ACINQDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACINQ03   05/01/02'                                      
         END                                                                    
