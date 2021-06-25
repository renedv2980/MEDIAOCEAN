*          DATA SET ACINQ09    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T60609A                                                                  
         TITLE 'ACCOUNT ENQUIRY MK2 - STATEMENT - T60609'                       
T60609   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR TYPE 'ST' IN ACCOUNT ENQUIRY PROGRAM                  
*              THIS TYPE GIVES STATEMENTS BY CONTRA-ACCOUNT FOR LEDGERS         
*              OTHER THAN THE PRODUCTION LEDGER (FOR WHICH USE 'DS' OR          
*              'JS').                                                           
*              PRESENT BALANCE AND ACCOUNT TOTAL FIGURES ARE TAKEN FROM         
*              THE BALANCE ELEMENT RATHER THAN ACCUMULATED FROM                 
*              TRANSACTIONS AS PER 'DS' AND 'JS'.                               
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60609)                                               
         DC    A(FILTABLE-T60609)                                               
         DC    A(KNTRYPNT-T60609)                                               
         DC    A(FNTRYPNT-T60609)                                               
         DC    A(DNTRYPNT-T60609)                                               
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
FILTABLE DC    CL10'END'                                                        
         DC    CL2'EN'                                                          
         DC    X'49'               DISABLED FROM NORMAL FILTERING               
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(EDITOPT1-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'MOS'                                                        
         DC    CL2'MO'                                                          
         DC    X'21'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSBTCH-TRANSD)                                             
         DC    AL1(2)                                                           
         DC    AL2(EDITOPT1-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'START'                                                      
         DC    CL2'ST'                                                          
         DC    X'51'               DISABLED FROM NORMAL FILTERING               
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(EDITOPT1-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
*              HANDLE CONTRA-ACCOUNT FILTER                                     
*              (CALLED BY FILTER ROUTINE IN ROOT)                               
         SPACE 1                                                                
FNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ9**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60609,RB                                                        
         USING T606TWA,RA                                                       
         LA    R2,WORK             SET UP DUMMY COMPARANDS                      
         BCTR  R3,0                R3 = LENGTH MINUS 1                          
         EX    R3,VCONTMV1         EQUATE COMPARANDS                            
         LA    R4,1                R4 = DISPLACEMENT INTO CA/C                  
         L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         CLC   ACKEYCON(3),SPACES                                               
         BNE   *+8                                                              
         LA    R4,3                                                             
         LA    R7,ACKEYCON(R4)                                                  
         EX    R3,VCONTCLC                                                      
         BL    VCONTNO                                                          
         SPACE 1                                                                
         CLC   SAVECACN,SPACES     SAVE NAME IF AVAILABLE                       
         BNE   VCONTX                                                           
         LA    R9,L'ACKEYCON                                                    
         SR    R9,R4                                                            
         BCTR  R9,0                                                             
         LA    R7,ACKEYCON(R4)                                                  
         EX    R9,VCONTCLC         DOES THE CA/C READ MATCH THE FILTER          
         BNE   VCONTX              IN FULL                                      
         CLI   ACRECORD,X'43'      IF SO IS THIS A CA/C HEADER                  
         BNE   VCONTX                                                           
         LA    R7,ACRECORD         IF SO SAVE THE NAME                          
         USING TRSUBHD,R7                                                       
         IC    R4,TRSBLEN                                                       
         SH    R4,=H'18'                                                        
         EX    R4,VCONTMV4                                                      
         B     VCONTX                                                           
         SPACE 1                                                                
VCONTNO  MVI   WORK,0              UNEQUATE COMPARANDS                          
         MVC   0(25,R7),SPACES     RESET KEY TO READ HIGH FOR THIS              
         EX    R3,VCONTMV5         CONTRA NEXT TIME                             
         MVC   KEY(L'ACCKEY),ACKEYD                                             
         MVI   KEY+L'ACCKEY-1,0                                                 
         MVI   DMARK,1                                                          
VCONTX   LA    R3,1(R3)                                                         
         LA    R5,INFCAC                                                        
         XIT1  REGS=(R2,R5)                                                     
         SPACE 1                                                                
VCONTMV1 MVC   WORK(0),INFCAC                                                   
VCONTMV4 MVC   SAVECACN(0),TRSBNAME                                             
VCONTCLC CLC   0(0,R7),INFCAC                                                   
VCONTMV5 MVC   0(0,R7),INFCAC                                                   
         EJECT                                                                  
*              MAIN PROCESS FOR A RECORD                                        
         SPACE 1                                                                
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ9**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60609,RB                                                        
         USING T606TWA,RA                                                       
         L     R8,ALOCAL                                                        
         USING LOCALD,R8                                                        
         SPACE 1                                                                
T0       CLI   VIRGIN,1            FIRST TIME THROUGH                           
         BH    T1                                                               
         ZAP   DEBIT,=P'0'         CLEAR CONTRA TOTALS                          
         ZAP   CREDIT,=P'0'                                                     
         ZAP   TOTDR,=P'0'         AND SCREEN TOTALS                            
         ZAP   TOTCR,=P'0'                                                      
         MVI   VIRGIN,2                                                         
         SPACE 1                                                                
         CLI   LASTKMK,0           SAVE KEY AND BALANCE ELEMENT IF NOT          
         BNE   T1                  CONTINUATION                                 
         L     RE,AIO                                                           
         MVC   SAVEKEY,0(RE)                                                    
         LA    R5,SAVBALEL                                                      
         USING ACBALD,R5                                                        
         MVI   ACBLEL,0            BUILD SAVE ELEMENT                           
         ZAP   ACBLFRWD,=P'0'                                                   
         ZAP   ACBLDR,=P'0'                                                     
         ZAP   ACBLCR,=P'0'                                                     
         TM    OPTIONS,1           IF WE'RE DATE-FILTERING THE SAVBALEL         
         BO    TNEXT               AREA IS USED FOR ACROSS SCRN A/G TOT         
         ICM   R9,15,ABAL                                                       
         BZ    TNEXT                                                            
         MVC   SAVBALEL,0(R9)                                                   
         B     TNEXT                                                            
         SPACE 1                                                                
T1       L     R9,AIO                                                           
         USING ACKEYD,R9                                                        
         CLI   ACKEYACC,RUNLAST                                                 
         BE    T3                                                               
         EJECT                                                                  
*              PROCESS A SUBA/C HEADER OR TRANSACTION                           
         SPACE 1                                                                
T2       OC    ACDTPEEL,ACDTPEEL   SKIP PEELED                                  
         BNZ   TNEXT                                                            
         MVI   DMCB,0                                                           
         L     RF,AFILTER                                                       
         BASR  RE,RF               HAVE WE REACHED THE START CONTRA             
         BZ    TNEXT               NO                                           
         ICM   R7,15,ASUB          IS THIS A SUBA/C HEADER                      
         BZ    T7                                                               
         SPACE 1                                                                
T3       CLI   VIRGIN,C'H'         DISPLAY HEADINGS                             
         BE    T4                                                               
         MVI   VIRGIN,C'H'                                                      
         MVI   LASTKMK,0                                                        
         CLI   LINE+1,4                                                         
         BE    T4                                                               
         LA    R6,INFDAT2H                                                      
         GOTO1 EDITACNM                                                         
         MVC   INFDAT3,HEADING                                                  
         OI    INFDAT3H+6,X'80'                                                 
         MVI   LINE+1,4                                                         
         CLI   SAVBALEL,0          AND BAL F/WRD                                
         BE    T4                                                               
         LA    R5,SAVBALEL                                                      
         USING ACBALD,R5                                                        
         MVC   INFDAT4+15(15),=C'BALANCE FORWARD'                               
         LA    RF,INFDAT4+31                                                    
         EDIT  ACBLFRWD,(13,0(RF)),2,MINUS=YES                                  
         OI    INFDAT4H+6,X'80'                                                 
         B     T4                                                               
HEADING  DC    CL39'CONTRA-ACCOUNT'                                             
         DC    CL39'                   DEBITS      CREDITS'                     
         SPACE 1                                                                
*                                                                               
T4       CLI   LINE+1,16           CHECK FOR SCREEN FULL - WE NEED ROOM         
         BNH   T5                  FOR THIS CONTRA + OTHERS + TOTAL             
         MVC   ACKEYD(L'ACCKEY),SAVEKEY  REVERT TO PREVIOUS CONTRA              
         MVC   ACKEYCON,CONCODE                                                 
         OI    LASTKMK,X'80'                                                    
         B     TEND1                                                            
         SPACE 1                                                                
T5       CLC   CONCODE,ACKEYCON    TEST SAME CONTRA (NEW FILE)                  
         BE    T7                                                               
         CP    DEBIT,=P'0'         ANYTHING FOR LAST CONTRA                     
         BNE   T55                                                              
         CP    CREDIT,=P'0'                                                     
         BE    T6                                                               
T55      BAS   RE,DISPLINE         DISPLAY LAST CONTRA DETAILS                  
         USING TRSUBHD,R7                                                       
         SPACE 1                                                                
T6       CLI   ACKEYACC,RUNLAST                                                 
         BE    TEND                                                             
         MVC   SAVEKEY,0(R9)       SAVE TRANSACTION KEY                         
         MVC   CONCODE,ACKEYCON    STORE CODE AND NAME FOR THIS ONE             
         MVC   CONNAME,SPACES                                                   
         ZAP   DEBIT,=P'0'         CLEAR CONTRA TOTALS                          
         ZAP   CREDIT,=P'0'                                                     
         ZIC   R3,TRSBLEN                                                       
         SH    R3,=H'18'                                                        
         BM    TNEXT                                                            
         EX    R3,*+8                                                           
         B     TNEXT                                                            
         MVC   CONNAME(0),TRSBNAME                                              
         SPACE 1                                                                
T7       ICM   R7,15,ATRN          HANDLE TRANSACTION                           
         BZ    TNEXT                                                            
         USING TRANSD,R7                                                        
         TM    OPTIONS,1           DATE-FILTERING                               
         BNO   *+12                                                             
         BAS   RE,DATFILT                                                       
         BZ    TNEXT                                                            
         LA    R5,DEBIT                                                         
         TM    TRNSSTAT,X'80'                                                   
         BO    *+8                                                              
         LA    R5,CREDIT                                                        
         AP    0(8,R5),TRNSAMNT    ADD VALUE TO CONTRA DR OR CR TOTAL           
         B     TNEXT                                                            
         EJECT                                                                  
*              ACTIONS AT END AND SCREEN FULL                                   
         SPACE 1                                                                
TEND     CLI   VIRGIN,C'H'         ANYTHING DISPLAYED                           
         BNE   TEND3                                                            
         SPACE 1                                                                
TEND1    LA    R5,SAVBALEL         AT END OR SCREEN FULL                        
         MVC   CONCODE(51),SPACES                                               
         TM    OPTIONS,1           IF DATE-FILTERING,DISPLAY DIFFERENCE         
         BNO   TEND1A              AND TOTAL DR & CR (NO BALANCE)               
         AP    ACBLDR,TOTDR                                                     
         AP    ACBLCR,TOTCR                                                     
         TM    LASTKMK,X'80'                                                    
         BO    TFULL                                                            
         MVC   CONNAME(14),=C'A/C DIFFERENCE'                                   
         ZAP   TOTDR,ACBLDR                                                     
         SP    TOTDR,ACBLCR                                                     
         ZAP   DEBIT,ACBLDR                                                     
         ZAP   CREDIT,ACBLCR                                                    
         B     TEND2A                                                           
         SPACE 1                                                                
TEND1A   ZAP   DEBIT,ACBLDR    DISPLAY OTHERS = TOTAL - THIS SCREEN             
         ZAP   CREDIT,ACBLCR                                                    
         SP    DEBIT,TOTDR                                                      
         SP    CREDIT,TOTCR                                                     
         CP    DEBIT,=P'0'                                                      
         BNE   *+14                                                             
         CP    CREDIT,=P'0'                                                     
         BE    TEND2                                                            
         MVC   CONNAME(6),=C'OTHERS'                                            
         BAS   RE,DISPLINE                                                      
TEND2    ZAP   DEBIT,TOTDR         AND PRESENT BALANCE/TOTAL                    
         ZAP   CREDIT,TOTCR                                                     
         AP    TOTDR,ACBLFRWD                                                   
         SP    TOTDR,TOTCR                                                      
         MVC   CONNAME(15),=C'PRESENT BALANCE'                                  
TEND2A   LA    RF,CONNAME+16                                                    
         EDIT  TOTDR,(13,0(RF)),2,MINUS=YES                                     
         MVC   CONNAME+31(5),=C'TOTAL'                                          
         BAS   RE,DISPLINE                                                      
         TM    LASTKMK,X'80'                                                    
         BO    TFULL                                                            
         SPACE 1                                                                
TEND3    SR    RF,RF               CC =  EQU FOR END                            
         B     TXIT                                                             
TFULL    LNR   RF,RF               CC =  NEG FOR SCREEN FULL                    
         MVI   LINE+1,4                                                         
         B     TXIT                                                             
TNEXT    LTR   RB,RB               CC =  POS FOR NEXT RECORD PLEASE             
TXIT     XIT1                                                                   
         EJECT                                                                  
*              APPLY DATE FILTER(S) TO A TRANSACTION                            
*              ON EXIT CC = EQU IF TRANSACTION NOT REQUIRED                     
         SPACE 1                                                                
DATFILT  NTR1                                                                   
         LA    R6,FILTAB                                                        
         USING FTBD,R6                                                          
         SR    RF,RF                                                            
         SPACE 1                                                                
DAT2     CLI   FTBELMNT,X'FF'                                                   
         BE    DATKEEP                                                          
         TM    FTBSTAT,X'60'       A DATE FILTER                                
         BZ    DATBUMP                                                          
         TM    FTBSTAT,X'20'       MOS (MONTH/YEAR)                             
         BO    DAT3                                                             
         MVC   THISBR1,DBRANCH     BNL                                          
         TM    FTBSTAT,X'08'       END DATE                                     
         BNO   *+8                                                              
         MVI   THISBR1+1,X'D0'     BNH                                          
         CLI   FTBSIGN,C'N'        NEGATIVE                                     
         BE    *+8                                                              
         XI    THISBR1+1,X'F0'     SWITCH BNL,BNH TO BL,BH                      
         CLC   ACKEYDTE,FTBVAL                                                  
         EX    RF,THISBR1                                                       
         B     DATBUMP                                                          
         SPACE 1                                                                
DAT3     TM    OPTIONS,MOSRANGE    SINGLE MOS                                   
         BO    DAT4                                                             
         MVC   THISBR1,DBRANCH1                                                 
         CLI   FTBSIGN,C'P'                                                     
         BE    *+8                                                              
         XI    THISBR1+1,X'F0'     SWITCH BNE & BE                              
         CLC   TRNSBTCH(2),FTBVAL                                               
         EX    RF,THISBR1                                                       
         B     DATBUMP                                                          
         SPACE 1                                                                
DAT4     GOTO1 CONVMOS,DMCB,TRANSD,WORK                                         
         SR    RF,RF                                                            
         MVC   THISBR1,DBRANCH     MOS RANGE                                    
         MVC   THISBR2,DBRANCH                                                  
         MVI   THISBR1+1,X'40'     BL SKIP                                      
         MVI   THISBR2+1,X'20'     BH SKIP                                      
         CLI   FTBSIGN,C'P'                                                     
         BE    DAT5                                                             
         MVC   THISBR1,DBRANCH2    BL OK                                        
         MVI   THISBR2+1,X'D0'     BNH SKIP                                     
DAT5     CLC   WORK(2),FTBVAL+0                                                 
         EX    RF,THISBR1                                                       
         CLC   WORK(2),FTBVAL+2                                                 
         EX    RF,THISBR2                                                       
         SPACE 1                                                                
DATBUMP  LA    R6,FTBTBLEN(R6)                                                  
         B     DAT2                                                             
         SPACE 1                                                                
DATSKIP  SR    RB,RB                                                            
DATKEEP  LTR   RB,RB                                                            
         XIT1                                                                   
         SPACE 1                                                                
DBRANCH  BNL   DATSKIP                                                          
DBRANCH1 BNE   DATSKIP                                                          
DBRANCH2 BL    DATBUMP                                                          
         DROP  R6                                                               
         EJECT                                                                  
*              SET UP A DISPLAY LINE FOR A CONTRA, INCREMENTS   SCREEN          
*              TOTALS AND UPDATES LINE NUMBER                                   
*              ON ENTRY CONCODE(63) = CONTRA CODE/NAME/DEBITS/CREDITS           
*                       LINE        = SCREEN LINE NUMBER                        
*              ON EXIT  LINE        = UPDATED LINE NUMBER                       
         SPACE 1                                                                
DISPLINE NTR1                                                                   
         LH    R6,LINE             FIND A(SCREEN LINE HEADER)                   
         LR    R7,R6                                                            
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         SPACE 1                                                                
DISP1    LA    R3,13               LEFT-ALIGN CONTRA CODE                       
         LA    R4,CONCODE+1                                                     
         CLI   0(R4),C' '                                                       
         BH    DISP2                                                            
         LA    R4,1(R4)                                                         
         BCT   R3,*-12                                                          
         B     DISP3                                                            
DISP2    EX    R3,*+8                                                           
         B     DISP3                                                            
         MVC   LINEDATA(0),0(R4)                                                
         SPACE 1                                                                
DISP3    MVC   LINEDATA+15(36),CONNAME                                          
         LA    RF,LINEDATA-2       RF=NOTIONAL COLUMN ZERO ON SCREEN            
         EDIT  (P8,DEBIT),(13,54(RF)),2,MINUS=YES                               
         EDIT  (P8,CREDIT),(13,67(RF)),2,MINUS=YES                              
         OI    LINEHDR+6,X'80'                                                  
         SPACE 1                                                                
DISP4    AP    TOTDR,DEBIT         INCREMENT TOTALS                             
         AP    TOTCR,CREDIT                                                     
         LA    R7,1(R7)            UPDATE LINE                                  
         STH   R7,LINE                                                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
LOCALD   DSECT                                                                  
CONCODE  DS    CL15      C         CONTRA-ACCOUNT CODE                          
CONNAME  DS    CL36      C         CONTRA-ACCOUNT NAME                          
DEBIT    DS    PL8       P         TOTAL DEBITS FOR CONTRA                      
CREDIT   DS    PL8       P         TOTAL CREDITS FOR CONTRA                     
TOTDR    DS    PL8       P         TOTAL DEBITS FOR SCREEN                      
TOTCR    DS    PL8       P         TOTAL CREDITS FOR SCREEN                     
THISBR1  DS    F                   EXECUTED BRANCHES IN DATFILT                 
THISBR2  DS    F                                                                
MOSRANGE EQU   X'02'                                                            
         SPACE 1                                                                
*                                                                               
*              NESTED INCLUDE FOR ACGENBOTH AND ACINQDSECT                      
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
SAVEKEY  DS    CL42      C         SAVED ACCOUNT KEY                            
SAVBALEL DS    CL26      V         SAVED BALANCE ELEMENT                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACINQ09   05/01/02'                                      
         END                                                                    
