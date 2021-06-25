*          DATA SET ACINQ07    AT LEVEL 021 AS OF 05/01/02                      
*PHASE T60607A,*,NOAUTO                                                         
         TITLE 'ACCOUNT ENQUIRY MK2 - RECEIVABLE - T60607'                      
T60607   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR TYPE 'RECEIVABLE' IN ACCOUNT ENQUIRY PROGRAM          
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60607)                                               
         DC    A(FILTABLE-T60607)                                               
         DC    A(KNTRYPNT-T60607)                                               
         DC    A(FNTRYPNT-T60607)                                               
         DC    A(DNTRYPNT-T60607)                                               
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
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(RECVUNIT-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(RECVLEDG-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(EDITACC-GWS)                                                 
*                                                                               
         DC    CL10'SPACES'        ACKEYWRK & 1ST 3 BYTES OF ACKEYCON           
         DC    C' '                MUST BE SPACES                               
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(15)                                                          
         DC    AL1(5)                                                           
         DC    AL2(SPACES-GWS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'THE REST'                                                   
         DC    C' '                                                             
         DC    C'V'                                                             
         DC    X'00'                                                            
         DC    AL1(20)                                                          
         DC    AL1(22)                                                          
         DC    AL2(SPACES-GWS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'AGEING'                                                     
         DC    CL2'AG'                                                          
         DC    X'01'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(3)                                                           
         DC    X'FF00'                                                          
         DC    AL2(0)                                                           
*                                                                               
*&&US                                                                           
         DC    CL10'DUE'                                                        
         DC    CL2'DU'                                                          
         DC    X'01'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(3)                                                           
         DC    X'FF00'                                                          
         DC    AL2(0)                                                           
*&&                                                                             
*                                                                               
         DC    CL10'DATE'                                                       
         DC    CL2'DA'                                                          
         DC    X'58'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'END'                                                        
         DC    CL2'EN'                                                          
         DC    X'48'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    X'FF00'             SAVE END DATE AS YYMMDD FOR AGEING           
         DC    AL2(EDITEND-GWS)                                                 
*                                                                               
*&&US                                                                           
         DC    CL10'GROSS'         INCLUDE CASH DISCOUNT FOR COMPLETELY         
         DC    CL2'GR'             UNPAID BILLS                                 
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'08'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*&&                                                                             
*                                                                               
         DC    CL10'MONTH'                                                      
         DC    CL2'MO'                                                          
         DC    X'28'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(5)                                                           
         DC    X'FF00'                                                          
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'REFERENCE'                                                  
         DC    CL2'RE'                                                          
         DC    X'18'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYREF-ACKEYD)                                             
         DC    AL1(L'ACKEYREF)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'START'                                                      
         DC    CL2'ST'                                                          
         DC    X'50'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITSTRT-GWS)                                                
*                                                                               
         DC    CL10'SUMMARY'                                                    
         DC    CL2'SU'                                                          
         DC    X'00'                                                            
         DC    CL8'CLIENT'                                                      
         DC    X'04'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    CL10'SUMMARY'                                                    
         DC    CL2'SU'                                                          
         DC    X'00'                                                            
         DC    CL8'SOURCE'                                                      
         DC    X'10'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
*              CODE TO HANDLE AGEING/END/MONTH OPTIONS AT VALIDATION            
*              AGEING OPTION                                                    
*              CHECK THAT VALUE IS VALID NUMERIC 1-999, STORE NEGATIVE          
*              OF VALUE MINUS 1 IN DAYS AND SET AGEING OPTIONS BIT              
*              END OPTION                                                       
*              SAVE FILTER END DATE AS YYMMDD IN ENDDATE AND SET                
*              OPTIONS X'20' BIT TO INDICATE THIS HAS BEEN DONE                 
*              MONTH OPTION                                                     
*              SAVE LAST DAY OF SPECIFIED MONTH AS YYMMDD IN ENDDATE            
*              AND AS PACKED DATE IN FILTAB ENTRY (INPUT IS BATCH HDR           
*              MOS FORMAT). SET MONTH OPTIONS BIT.                              
         SPACE 1                                                                
FNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ7**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60607,RB                                                        
         L     R8,ALOCAL                                                        
         USING FILTERSD,R4                                                      
         USING LOCALD,R8                                                        
         USING FTBD,R6             R6 = A(FILTER TABLE ENTRY) WHICH             
*                                       CONTAINS PACKED DATE IN FTBVAL          
         SPACE 1                                                                
F01      CLI   FILSHTKW,C'A'       AGEING OPTION                                
         BNE   F10                                                              
         TM    OPTIONS,MONTH+DUE                                                
         BNZ   FERRINC             INCOMPATIBLE                                 
         TM    3(R3),X'80'         R3 = A(SCANBLCK ENTRY) - NUMERIC TST         
         BO    *+12                                                             
         MVI   ERROR,NOTNUMRC                                                   
         B     FERRXIT                                                          
         ICM   R1,15,8(R3)                                                      
         BP    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     FERRXIT                                                          
         BCTR  R1,0                                                             
         LNR   R1,R1                                                            
         ST    R1,DAYS                                                          
         OI    OPTIONS,AGEING                                                   
         B     FXIT                                                             
         SPACE 1                                                                
F10      CLI   FILSHTKW,C'D'       DUE DATE AGEING                              
         BNE   F20                                                              
         TM    OPTIONS,MONTH+AGEING                                             
         BNZ   FERRINC             INCOMPATIBLE                                 
         TM    3(R3),X'80'         R3 = A(SCANBLCK ENTRY) - NUMERIC TST         
         BO    *+12                                                             
         MVI   ERROR,NOTNUMRC                                                   
         B     FERRXIT                                                          
         ICM   R1,15,8(R3)                                                      
         BP    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     FERRXIT                                                          
         BCTR  R1,0                                                             
         LNR   R1,R1                                                            
         ST    R1,DAYS                                                          
         OI    OPTIONS,DUE                                                      
         B     FXIT                                                             
         SPACE 1                                                                
F20      CLI   FILSHTKW,C'E'       END OPTION                                   
         BNE   F30                                                              
         CLI   FTBSIGN,C'N'        NEGATIVE                                     
         BE    FXIT                                                             
         TM    OPTIONS,MONTH                                                    
         BO    FERRINC             INCOMPATIBLE                                 
         GOTO1 VDATCON,DMCB,(1,FTBVAL),(0,ENDDATE)                              
         OI    OPTIONS,ENDAGE                                                   
         B     FXIT                                                             
         SPACE 1                                                                
F30      DS    0H                  MONTH OPTION                                 
         CLI   FTBSIGN,C'N'                                                     
         BE    FXIT                                                             
         TM    OPTIONS,AGEING+ENDAGE+DUE                                        
         BNZ   FERRINC             INCOMPATIBLE                                 
         GOTO1 VDATVAL,DMCB,(2,22(R3)),WORK                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,1                                 
         MVC   WORK+10(2),=C'01'   LAST DAY OF M = 1ST DAY OF NEXT - 1          
         LA    RF,1                                                             
         LNR   RF,RF                                                            
         GOTO1 VADDAY,DMCB,WORK+6,ENDDATE,(RF)                                  
         GOTO1 VDATCON,DMCB,(0,ENDDATE),(1,FTBVAL)                              
         MVI   FTBLEN,3                                                         
         OI    OPTIONS,MONTH                                                    
         SPACE 1                                                                
FXIT     MVI   DMCB,0                                                           
         B     *+12                                                             
FERRINC  MVI   ERROR,82            INCOMPATIBLE OPTIONS                         
FERRXIT  MVI   DMCB,1                                                           
         XIT1                                                                   
         DROP  R4,R6                                                            
         EJECT                                                                  
*              MAIN PROCESS                                                     
         SPACE 1                                                                
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ7**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60607,RB                                                        
         USING T606TWA,RA                                                       
         L     R8,ALOCAL                                                        
         USING LOCALD,R8                                                        
         L     R9,AIO                                                           
         USING ACKEYD,R9                                                        
         USING LINED,R6                                                         
         SPACE 1                                                                
T00      CLI   ACKEYACC,RUNLAST    FILTER TRANSACTIONS IF NOT RUNLAST           
         BE    T10                                                              
         CLI   ACKEYDTE,C' '                                                    
         BE    TNEXT                                                            
         SPACE 1                                                                
T10      CLI   VIRGIN,1            FIRST TIME ACTIONS                           
         BNE   T20                                                              
         ZAP   CASHDISC,=P'0'                                                   
         MVI   PARTPAID,C' '                                                    
         BAS   RE,AGEINIT                                                       
         MVI   VIRGIN,2                                                         
         CLI   LASTKMK,0           ACTIONS IF NOT CONTINUATION                  
         BNE   T20                                                              
         MVC   SAVEKEY,ACKEYACC                                                 
         MVC   SAVELAST,ACKEYCON+3                                              
         LA    R7,BILLTOTS                                                      
         USING ANYVALD,R7                                                       
         LA    R3,3                                                             
T15      LA    R5,ANYAGE1                                                       
         LA    R2,4                                                             
         ZAP   0(6,R5),=P'0'                                                    
         LA    R5,6(R5)                                                         
         BCT   R2,*-10                                                          
         MVI   ANYMARK,0                                                        
         LA    R7,ANYLEN(R7)                                                    
         BCT   R3,T15                                                           
         SPACE 1                                                                
T20      CLI   ACKEYACC,RUNLAST    IF ITS RUNLAST FORCE ALL CNTRL BRKS          
         BNE   T30                                                              
         MVI   ACKEYCON,X'FF'                                                   
         MVC   ACKEYCON+1(23),ACKEYCON                                          
         EJECT                                                                  
*              BILL NUMBER CONTROL BREAK                                        
         SPACE 1                                                                
T30      CLC   SAVESRCE,ACKEYCON+3                                              
         BNE   *+14                                                             
         CLC   SAVEBILL,ACKEYREF                                                
         BE    T70                                                              
         OC    SAVEBILL,SAVEBILL   ALREADY HANDLED                              
         BZ    T40                                                              
         CP    BILLTOTL,=P'0'                                                   
         BE    T38                                                              
         SPACE 1                                                                
T31      TM    OPTIONS,GROSS       IF GROSS OPTION IN USE AND NO PART           
         BNO   T32                 PAYMENT ADD CASH DISCOUNT                    
         CLI   PARTPAID,C'*'                                                    
         BE    T32                                                              
         AP    BILLTOTL,CASHDISC                                                
         SPACE 1                                                                
T32      LA    R7,BILLTOTS         PUT TOTAL O/S IN APPROPRIATE AGEING          
         LA    R5,AGETABLE         BUCKET                                       
         USING AGED,R5                                                          
         LA    R6,SAVEDATE                                                      
         OC    SAVEDUE,SAVEDUE                                                  
         BZ    T33                                                              
         LA    R6,SAVEDUE          USE DUE DATE IF I HAVE IT                    
T33      CLC   0(3,R6),AGEMIN                                                   
         BNL   T34                                                              
         LA    R7,6(R7)                                                         
         LA    R5,AGELEN(R5)                                                    
         B     T33                                                              
T34      MVC   0(6,R7),BILLTOTL                                                 
         MVI   BILLMARK,DISPLAY                                                 
         SPACE 1                                                                
T36      MVI   CALLTYPE,BILL       SET UP BILL DISPLAY LINE                     
         BAS   RE,DISPVALS                                                      
         BZ    T38                                                              
         MVC   LINEDATA(12),SAVESRCE                                            
         MVC   LINEDATA+13(6),SAVEBILL                                          
         LA    R7,SAVEDATE                                                      
         TM    OPTIONS,DUE                                                      
         BZ    T37                                                              
         MVI   LINEDATA+28,C'B'    INDICATE BILL DATE IF NO DUE DATE            
         OC    SAVEDUE,SAVEDUE     IF DUE DATE                                  
         BZ    T37                                                              
         LA    R7,SAVEDUE          USE IT                                       
         MVI   LINEDATA+28,C' '    AND LEAVE INDICATOR BLANK                    
T37      GOTO1 VDATCON,DMCB,(1,0(R7)),(8,LINEDATA+20)                           
*&&UK*&& OI    LINEDATA+20,X'F0'                                                
         MVC   LINEDATA+77(1),PARTPAID                                          
         SPACE 1                                                                
T38      ZAP   CASHDISC,=P'0'      CLEAR BILL-LEVEL DATA                        
         MVI   PARTPAID,0                                                       
         XC    SAVEBILL,SAVEBILL                                                
         CLI   LINE+1,18                                                        
         BH    TFULL                                                            
         B     T40                                                              
         EJECT                                                                  
*              BILLING SOURCE CONTROL BREAK                                     
         SPACE 1                                                                
T40      CLC   SAVESRCE,ACKEYCON+3                                              
         BE    T60                                                              
         OC    SAVESRCE,SAVESRCE   ALREADY HANDLED                              
         BZ    T50                                                              
         MVI   CALLTYPE,SOURCE                                                  
         BAS   RE,DISPVALS                                                      
         BZ    T45                                                              
         MVC   LINEDATA(12),SAVESRCE                                            
         MVC   LINEDATA+13(13),=C'SOURCE TOTALS'                                
         SPACE 1                                                                
T45      XC    SAVESRCE,SAVESRCE                                                
         CLI   LINE+1,18                                                        
         BH    TFULL                                                            
         B     T50                                                              
         EJECT                                                                  
*              ACCOUNT CONTROL BREAK                                            
         SPACE 1                                                                
T50      CLI   ACKEYACC,RUNLAST                                                 
         BNE   T60                                                              
         MVI   CALLTYPE,ACCOUNT                                                 
         BAS   RE,DISPVALS                                                      
         BZ    TEND                                                             
         MVC   LINEDATA+13(14),=C'ACCOUNT TOTALS'                               
         B     TEND                                                             
         EJECT                                                                  
*              PROCESS A TRANSACTION                                            
         SPACE 1                                                                
T60      MVC   SAVELAST,ACKEYCON+3 RESET SAVED KEY IF CB HAS OCCURRED           
         SPACE 1                                                                
T70      XC    SAVEDUE,SAVEDUE                                                  
         OC    ACDTPEEL,ACDTPEEL   SKIP PEELED                                  
         BNZ   TNEXT                                                            
         ICM   R9,15,ATRN                                                       
         USING TRANSD,R9                                                        
         TM    TRNSSTAT,X'80'      DEBIT                                        
         BNO   T75                                                              
         AP    BILLTOTL,TRNSAMNT                                                
         TM    OPTIONS,DUE         NO DUE DATE OPTION                           
         BZ    T72                                                              
         ICM   R7,15,ADUE                                                       
         BZ    T72                                                              
         USING TRDUED,R7                                                        
         GOTO1 VDATCON,DMCB,(2,TRDUDATE),(1,SAVEDUE)                            
T72      TM    OPTIONS,GROSS       CASH DISCOUNT REQUIRED                       
         BNO   TNEXT                                                            
         ICM   R7,15,ACSHD                                                      
         BZ    TNEXT                                                            
         USING TRCASHD,R7                                                       
         CLI   TRCSTYPE,C'D'                                                    
         BNE   TNEXT                                                            
         AP    CASHDISC,TRCSAMNT                                                
         B     TNEXT                                                            
         SPACE 1                                                                
T75      SP    BILLTOTL,TRNSAMNT                                                
         MVI   PARTPAID,C'*'                                                    
         B     TNEXT                                                            
         SPACE 1                                                                
TFULL    LNR   RF,RF               CC = NEG FOR SCREEN FULL                     
         MVI   LINE+1,5                                                         
         B     TXIT                                                             
TEND     SR    RB,RB               CC = EQU FOR END                             
TNEXT    LTR   RB,RB               CC = POS FOR NEXT RECORD PLEASE              
TXIT     XIT1                                                                   
         EJECT                                                                  
*              INITIALISE AGEING PARAMETERS                                     
*              ON ENTRY OPTIONS  = X'80' - AGEING INTERVAL IS STORED            
*                                          IN DAYS (SET BY AG=N OPTION)         
*                                  X'20' - ENDDATE CONTAINS FINAL DATE          
*                                          SO DONT USE TODAY'S DATE             
*              ON EXIT  AGEDISP  = DISPLACEMENT INTO RECORD OF AGEING           
*                                  COMPARAND                                    
*                       AGEXLEN  = EXECUTE LENGTH FOR AGE COMPARISONS           
*                       AGETABLE = 3 ENTRY TABLE CONTAINING MAXIMUM,            
*                                  MINIMUM AND HEADLINES PER AGE GROUP          
         SPACE 1                                                                
AGEINIT  NTR1                                                                   
         TM    OPTIONS,AGEING+DUE                                               
         BNZ   AG20                        SET BY AGEING OPTION                 
         LA    R1,29                       DEFAULTS TO 30                       
         LNR   R1,R1                                                            
         ST    R1,DAYS                                                          
         SPACE 1                                                                
AG20     TM    OPTIONS,ENDAGE+MONTH GET FINAL DATE INTO ENDDATE                 
         BNZ   AG30                IT IS ALREADY                                
         GOTO1 VDATCON,DMCB,(5,0),(0,ENDDATE)                                   
         SPACE 1                                                                
AG30     LA    R5,AGETABLE         SET UP AGETABLE                              
         USING AGED,R5                                                          
         LA    R3,3                COUNT                                        
         B     AG32                                                             
         SPACE 1                                                                
AG31     LA    RF,1                                                             
         LNR   RF,RF                                                            
         GOTO1 VADDAY,DMCB,ENDDATE,STADATE,(RF)                                 
         MVC   ENDDATE,STADATE                                                  
AG32     MVC   AGED(AGELEN),SPACES                                              
         BAS   RE,AGECONV          CONVERT ENDDATE TO PACKED & DDMMMYY          
         MVC   AGEMAX,WORK                                                      
         MVC   AGEHD2,WORK+3                                                    
         CH    R3,=H'1'                                                         
         BNE   AG35                                                             
         MVC   AGEHD1,AGEHD2       SHUFFLE HEADS FOR LAST AGE GROUP             
         MVC   AGEHEAD2,=C'AND PRIOR'                                           
         XC    AGEMIN,AGEMIN                                                    
         B     AGXIT                                                            
AG35     TM    OPTIONS,MONTH                                                    
         BNO   AG36                                                             
         MVC   AGEHD1,AGEHD2                                                    
         MVC   AGEHD2+2(6),=6C'-'                                               
         MVC   ENDDATE+4(2),=C'01'                                              
         B     AG38                                                             
AG36     L     R2,DAYS                                                          
         GOTO1 VADDAY,DMCB,ENDDATE,STADATE,(R2)                                 
         MVC   ENDDATE,STADATE                                                  
AG38     BAS   RE,AGECONV                                                       
         MVC   AGEMIN,WORK                                                      
         TM    OPTIONS,MONTH                                                    
         BO    *+10                                                             
         MVC   AGEHD1,WORK+3                                                    
         LA    R5,AGELEN(R5)                                                    
         BCT   R3,AG31                                                          
AGXIT    XIT1                                                                   
         SPACE 3                                                                
AGECONV  NTR1                      CONVERT ENDDATE TO PACKED IN WORK            
         GOTO1 VDATCON,DMCB,(0,ENDDATE),(1,WORK)                                
         MVC   WORK+3(9),SPACES                                                 
         LA    RE,WORK+3           AND TO DDMMMYY IN WORK+3                     
         MVI   HALF,8                                                           
         TM    OPTIONS,MONTH                                                    
         BNO   *+12                                                             
         LA    RE,WORK+5           OR  FOR MONTH OPTION,MMM YY                  
         MVI   HALF,6                                                           
         ST    RE,DMCB+4                                                        
         MVC   DMCB+4(1),HALF                                                   
         BASR  RE,RF                                                            
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*              DISPLAY A LINE OF VALUES IF REQUIRED AND ROLL TOTALS             
*              ON ENTRY CALLTYPE = BILL/SOURCE/ACCOUNT (EQUATED VALUES)         
*              ON EXIT  LINE     = UPDATED LINE NUMBER                          
*                       CC       = EQU IF NO LINE DISPLAYED                     
         SPACE 1                                                                
DISPVALS NTR1                                                                   
         LA    R7,BILLTOTS         SET UP VALUE POINTER AND OPTION              
         USING ANYVALD,R7          CHECK BITS (IN R1) FOR THIS CALLTYPE         
         LA    R1,CLIORSRC                                                      
         CLI   CALLTYPE,BILL                                                    
         BE    DV20                                                             
         LA    R7,SRCETOTS                                                      
         LA    R1,CLISUMRY                                                      
         CLI   CALLTYPE,SOURCE                                                  
         BE    DV20                                                             
         LA    R7,ACCTTOTS                                                      
         SR    R1,R1                                                            
         SPACE 1                                                                
DV20     CLI   ANYMARK,DISPLAY     ANYTHING TO DISPLAY AT THIS LEVEL            
         BE    *+10                                                             
         SR    RF,RF               CLEAR RF FOR NO DISPLAY                      
         B     DVXIT                                                            
         EX    R1,*+8              ARE WE ONLY DISPLAYING A HIGHER              
         B     *+8                 LEVEL (THAN THIS) SUMMARY                    
         TM    OPTIONS,0                                                        
         BZ    DV30                IF SO                                        
         SR    RF,RF               CLEAR RF FOR NO DISPLAY                      
         B     DV52                AND BRANCH TO ROLL TOTALS                    
         SPACE 1                                                                
DV30     CLI   VIRGIN,C'H'         FIRST TIME SET UP HEADINGS                   
         BE    DV31                                                             
         MVI   VIRGIN,C'H'                                                      
         CLI   LINE+1,4                                                         
         BE    *+8                                                              
         BAS   RE,HEADINGS                                                      
         SPACE 1                                                                
DV31     LH    R6,LINE                                                          
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         OI    LINEHDR+6,X'80'                                                  
         SPACE 1                                                                
         LA    R4,ANYAGE1          DISPLAY VALUES                               
         LA    R5,LINEDATA+29                                                   
         LA    R3,4                                                             
DV32     EDIT  (P6,0(R4)),(12,0(R5)),2,MINUS=YES                                
         CH    R3,=H'1'                                                         
         BE    DV40                                                             
         CLC   7(4,R5),=C' .00'    SUPPRESSING ZEROS IN AGE TOTALS              
         BNE   *+10                                                             
         MVC   8(3,R5),SPACES                                                   
         LA    R4,6(R4)                                                         
         LA    R5,12(R5)                                                        
         BCT   R3,DV32                                                          
         SPACE 1                                                                
DV40     LH    R1,LINE             INCREMENT LINE                               
         LA    R1,1(R1)                                                         
         CLI   CALLTYPE,SOURCE     SPACE LINE AFTER SOURCE TOTAL                
         BNE   DV48                                                             
         L     R9,AIO                                                           
         USING ACKEYD,R9                                                        
         CLI   ACKEYACC,RUNLAST    IF RUNLAST & STILL ROOM FOR A/C TOT          
         BNE   DV45                                                             
         CLI   LINE+1,17                                                        
         BL    DV47                                                             
         B     DV48                                                             
DV45     TM    OPTIONS,SRCSUMRY    OR IF NOT RUNLAST AND NOT SOURCE             
         BO    DV48                SUMMARY                                      
DV47     LA    R1,1(R1)                                                         
DV48     STH   R1,LINE                                                          
         SPACE 1                                                                
DV50     CLI   CALLTYPE,ACCOUNT    ROLL TOTALS AND ANYMARK                      
         BE    DVXIT                                                            
DV52     LA    R3,4                                                             
         MVI   ANYMARK+ANYLEN,DISPLAY                                           
         MVI   ANYMARK,0                                                        
DV54     AP    ANYAGE1+ANYLEN(6),ANYAGE1                                        
         ZAP   ANYAGE1,=P'0'                                                    
         LA    R7,6(R7)                                                         
         BCT   R3,DV54                                                          
         SPACE 1                                                                
DVXIT    LTR   RF,RF               CC = EQU FOR NO DISPLAY                      
         XIT1  REGS=(R6)                                                        
         DROP  R7                                                               
         EJECT                                                                  
*              SET UP PAGE HEADINGS                                             
         SPACE 1                                                                
HEADINGS NTR1                                                                   
HE20     LA    R5,WORK             SET UP OFFICE (IF ANY) AND ACCOUNT           
         MVC   WORK(90),SPACES                                                  
         CLI   SAVEHIER+1,C'O'     IS THERE AN OFFICE                           
         BNE   HE25                                                             
         ZIC   R1,SAVEHIER                                                      
         LA    R1,2(R1)                                                         
         MVC   KEYB(L'ACCKEY),SPACES                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEYB(0),SAVEKEY                                                  
         GOTO1 AREADB                                                           
         BZ    HE25                                                             
         L     R9,AIOB                                                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNZ   HE25                                                             
         USING ACNAMED,R9                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         BM    HE25                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+7(0),ACNMNAME                                               
         MVC   WORK(7),=C'OFFICE='                                              
         MVI   WORK+44,C'/'                                                     
         LA    R5,WORK+46                                                       
HE25     MVC   0(8,R5),=C'ACCOUNT='                                             
         MVC   8(36,R5),SAVEACCN                                                
         CLI   WORK,C'O'                                                        
         BNE   HE26                                                             
         GOTO1 VSQASHER,DMCB,WORK,90                                            
HE26     MVC   INFDAT2,WORK                                                     
         OI    INFDAT2H+6,X'80'                                                 
         SPACE 1                                                                
HE30     MVC   INFDAT3,HEADING1    SET UP HEADLINES                             
         OI    INFDAT3H+6,X'80'                                                 
         MVC   INFDAT4,HEADING2                                                 
         OI    INFDAT4H+6,X'80'                                                 
         TM    OPTIONS,DUE                                                      
         BZ    *+10                                                             
         MVC   INFDAT3+20(4),=C'DUE '                                           
         LA    R6,INFDAT3+32                                                    
         LA    R5,AGETABLE         INCLUDING AGEING HEADLINES                   
         USING AGED,R5                                                          
         LA    R3,3                                                             
         B     HE34                                                             
HE32     TM    OPTIONS,MONTH                                                    
         BO    *+8                                                              
         MVI   8(R6),C'-'                                                       
         LA    R5,AGELEN(R5)                                                    
         LA    R6,12(R6)                                                        
HE34     MVC   0(9,R6),AGEHEAD1                                                 
         MVC   86(9,R6),AGEHEAD2                                                
         BCT   R3,HE32                                                          
         SPACE 1                                                                
HE36     TM    OPTIONS,CLIORSRC    EXCLUDING BILL-LEVEL HEADS IF                
         BZ    HE40                SUMMARY OPTION IN USE                        
         MVC   INFDAT3+13(11),SPACES                                            
         MVC   INFDAT4+13(11),SPACES                                            
         MVI   INFDAT3+77,C' '                                                  
         MVI   INFDAT4+77,C' '                                                  
         SPACE 1                                                                
HE40     MVI   LINE+1,5                                                         
         XIT1                                                                   
         DROP  R5                                                               
         SPACE 1                                                                
HEADING1 DC    CL39'BILLING      BILL   BILL'                                   
         DC    CL39'                                TOTAL P'                    
HEADING2 DC    CL39'SOURCE       NUMBER DATE'                                   
         DC    CL39'                                ----- P'                    
         EJECT                                                                  
         GETEL R9,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
LOCALD   DSECT                                                                  
ENDDATE  DS    CL6       C         FILTER END OR TODAY AS YYMMDD                
STADATE  DS    CL6       C         WORK                                         
DAYS     DS    F         B         NEGATIVE AGEING INTERVAL MINUS 1             
PARTPAID DS    C         C         *=PARTPAID (THERE ARE CR TRANSACTNS)         
CASHDISC DS    PL6       P         CASH DISCOUNT TOTAL FOR BILL                 
CALLTYPE DS    C         C         PASSED TO PRINTIT ROUTINE (B/S/A)            
AGETABLE DS    3CL24     V         AGE CATEGORY TABLE                           
SAVEDUE  DS    CL3       P         DUE DATE                                     
         SPACE 3                                                                
*              DSECT TO COVER AN AGE CATEGORY TABLE ENTRY                       
         SPACE 1                                                                
AGED     DSECT                                                                  
AGEMIN   DS    CL3       V         MIN DATE FOR AGE CATEGORY                    
AGEMAX   DS    CL3       V         MAX DATE                                     
AGEHEAD1 DS    0CL9      C         1ST HEADLINE FOR AGE CATEGORY                
*&&US                                                                           
AGEHD1   DS    CL9       C                                                      
*&&                                                                             
*&&UK                                                                           
         DS    C                                                                
AGEHD1   DS    CL8       C                                                      
*&&                                                                             
AGEHEAD2 DS    0CL9      C         2ND HEADLINE                                 
*&&US                                                                           
AGEHD2   DS    CL9       C                                                      
*&&                                                                             
*&&UK                                                                           
         DS    C                                                                
AGEHD2   DS    CL8                                                              
*&&                                                                             
AGELEN   EQU   *-AGED                                                           
         SPACE 3                                                                
*              DSECT TO COVER A LEVEL OF VALUE TOTALS                           
         SPACE 1                                                                
ANYVALD  DSECT                                                                  
ANYAGE1  DS    PL6       P         MOST RECENT AGE CATEGORY TOTAL               
ANYAGE2  DS    PL6       P         NEXT MOST RECENT                             
ANYAGE3  DS    PL6       P         THE REMAINDER                                
ANYTOTL  DS    PL6       P         TOTAL AMOUNT OUTSTANDING                     
ANYMARK  DS    C         C         D=SOMETHING TO DISPLAY                       
ANYLEN   EQU   *-ANYVALD                                                        
         SPACE 3                                                                
*              EQUATED VALUES                                                   
         SPACE 1                                                                
BILL     EQU   C'B'                                                             
SOURCE   EQU   C'S'                                                             
ACCOUNT  EQU   C'A'                                                             
DISPLAY  EQU   C'D'                                                             
AGEING   EQU   X'80'                                                            
MONTH    EQU   X'40'                                                            
ENDAGE   EQU   X'20'                                                            
SRCSUMRY EQU   X'10'                                                            
GROSS    EQU   X'08'                                                            
CLISUMRY EQU   X'04'                                                            
DUE      EQU   X'02'                                                            
CLIORSRC EQU   X'14'                                                            
         EJECT                                                                  
*                                                                               
*              NESTED INCLUDE FOR ACINQDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
SAVEKEY  DS    CL15      V         ACCOUNT KEY                                  
SAVELAST DS    0CL21     V         LAST SOURCE/DATE/BILL NUMBER                 
*                                  KEY ELEMENT CONTAINS NULLS IF                
*                                  CONTROL BREAK AT THIS LEVEL HAS BEEN         
*                                  HANDLED ALREADY (PRINTED ETC)                
SAVESRCE DS    CL12      C                                                      
SAVEDATE DS    CL3       P                                                      
SAVEBILL DS    CL6       C                                                      
         SPACE 1                                                                
BILLTOTS DS    0CL24     P         BILL NUMBER TOTALS (SEE ANYVALD)             
BILLAGE1 DS    PL6       P         MOST RECENT AGR CATEGORY TOTAL               
BILLAGE2 DS    PL6       P         NEXT MOST RECENT                             
BILLAGE3 DS    PL6       P         THE REMAINDER                                
BILLTOTL DS    PL6       P         TOTAL AMOUNT OUTSTANDING                     
BILLMARK DS    C         C         D=SOMETHING TO DISPLAY                       
         SPACE 1                                                                
SRCETOTS DS    4PL6      P         SOURCE TOTALS AS FOR BILL NUMBER             
SRCEMARK DS    C         C         D=SOMETHING TO DISPLAY                       
         SPACE 1                                                                
ACCTTOTS DS    4PL6      P         ACCOUNT TOTALS AS FOR SOURCE                 
ACCTMARK DS    C         C         D=SOMETHING TO DISPLAY                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACINQ07   05/01/02'                                      
         END                                                                    
