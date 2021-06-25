*          DATA SET ACINQ11    AT LEVEL 023 AS OF 05/01/02                      
*PHASE T60611A,*,NOAUTO                                                         
         TITLE 'ACCOUNT ENQUIRY - JOB BILLING ALLOCATION - T60611'              
T60611   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR TYPE 'JOB BILL ALLOC' IN ACCT ENQUIRY PROGRAM         
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60611)                                               
         DC    A(FILTABLE-T60611)                                               
         DC    A(KNTRYPNT-T60611)                                               
         DC    A(FNTRYPNT-T60611)                                               
         DC    A(DNTRYPNT-T60611)                                               
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
         DC    AL2(PRODUNIT-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(PRODLEDG-GWS)                                                
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
FILTABLE DC    CL10'AMOUNT'                                                     
         DC    CL2'AM'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSAMNT-TRANSD)                                             
         DC    AL1(10)                                                          
         DC    AL2(EDITCASH-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'BATCH'                                                      
         DC    CL2'BA'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSBTCH-TRANSD)                                             
         DC    AL1(L'TRNSBTCH)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'BILLED'                                                     
         DC    CL2'BI'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'01'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACDTUSED-ACKEYD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    X'FF00'             OMIT WC99                                    
*                                                                               
         DC    CL10'CREDIT'                                                     
         DC    CL2'CR'                                                          
         DC    X'02'                                                            
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DATE'                                                       
         DC    CL2'DA'                                                          
         DC    X'40'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DEBIT'                                                      
         DC    CL2'DR'                                                          
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DDS'                                                        
         DC    CL2'DD'                                                          
         DC    X'80'                                                            
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
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
         DC    AL2(0)                                                           
         DC    AL2(EDITEND-GWS)                                                 
*                                                                               
         DC    CL10'MOS'                                                        
         DC    CL2'MO'                                                          
         DC    X'20'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSBTCH-TRANSD)                                             
         DC    AL1(2)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ORDER'                                                      
         DC    CL2'OR'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AXNO-GWS)                                                    
         DC    AL1(ACNO-ACNOD)                                                  
         DC    AL1(6)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'PEELED'                                                     
         DC    CL2'PE'                                                          
         DC    X'80'                                                            
         DC    CL8'YES'                                                         
         DC    X'20'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
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
         DC    CL10'URGENT'                                                     
         DC    CL2'UR'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'40'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'WORKCODE'                                                   
         DC    CL2'WC'                                                          
         DC    X'18'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYWRK-ACKEYD)                                             
         DC    AL1(L'ACKEYWRK)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'AUTHORISE'                                                  
         DC    CL2'AU'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'08'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'BILLNO'                                                     
         DC    CL2'BN'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(6)                                                           
         DC    AL2(0)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
*              BILLED FILTER MUST ALWAYS DROP WC99 TRANSACTIONS                 
*                                                                               
FNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ11*                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
*                                                                               
         L     RB,APHASE                                                        
         USING T60611,RB                                                        
         USING T606TWA,RA                                                       
*                                                                               
         L     R7,AIO                                                           
         USING ACKEYD,R7                                                        
         USING FTBD,R6                                                          
*                                                                               
         L     R4,ALOCAL                                                        
         USING LOCALD,R4                                                        
*                                                                               
         C     R2,ATRN             R2 POINTING AT TRANS EL                      
         BE    F20                 MUST BE DOING BILL NO FILTER                 
*                                                                               
         LA    R2,WORK                                                          
         MVI   WORK,0              SET NOT-BILLED                               
*                                                                               
         CLC   ACKEYWRK,=C'99'                                                  
         BE    F10                                                              
*                                                                               
         LA    R3,ACKEYD                                                        
         AH    R3,DATADISP                                                      
         USING TRANSD,R3                                                        
*                                                                               
         CLI   0(R3),X'44'                                                      
         BNE   FXIT                                                             
         ZAP   TRNSAVE,TRNSAMNT                                                 
         ZAP   BILLTOT,=P'0'       CLEAR BILLED TOTAL                           
*                                                                               
         GOTO1 PRORATA,DMCB,(X'80',(R7)),0,ACOMFACS,0,PROBLK,0                  
*                                                                               
         USING PRORATAD,RF                                                      
         LA    RF,PROBLK                                                        
         ZAP   BILLTOT,PA$NETBL    ADD NET BILLED                               
         AP    BILLTOT,PA$WOFAM    AND AMOUNT WRITTEN OFF                       
*                                                                               
         CP    BILLTOT,=P'0'       ANYTHING BILLED?                             
         BE    FXIT                NO                                           
         CP    BILLTOT,TRNSAVE                                                  
         BNE   FXIT                NO EQUAL MEANS PART BILLED                   
         MVI   WORK,1              BILLED                                       
         B     FXIT                                                             
*                                                                               
F10      CLI   FTBSIGN,C'P'        INVERT FOR WRK CODE 99                       
         BE    *+8                                                              
         MVI   WORK,1                                                           
*                                                                               
FXIT     XIT1  REGS=(R2)                                                        
         DROP  RF                                                               
*                                                                               
F20      DS    0H                                                               
         LA    R3,ACKEYD                                                        
         AH    R3,DATADISP                                                      
         CLI   0(R3),X'44'                                                      
         BNE   FNOX                                                             
*                                                                               
         GOTO1 PRORATA,DMCB,(X'C0',(R7)),0,ACOMFACS,0,PROBLK,PROLST             
*                                                                               
         USING PRORATAD,R3                                                      
         LA    R3,PROLST                                                        
*                                                                               
         USING PTAELD,R3                                                        
         CLI   0(R3),PTAELQ                                                     
         B     F22+8                                                            
*                                                                               
F22      MVI   ELCODE,PTAELQ       LOOK FOR THE RIGHT ELEMENT                   
         BAS   RE,NEXTEL                                                        
         BNE   FNOX                                                             
*                                                                               
         TM    PTASTAT1,PTASPEND   SKIP PENDING                                 
         BO    F22                                                              
         CLI   PTATYPE,PTATRAL     ACCEPT BILLING                               
         BE    F26                                                              
         CLI   PTATYPE,PTATWOF     WRITE-OFFS                                   
         BE    F24                                                              
         CLI   PTATYPE,PTATWOFR    AND RECOVERIES                               
         BNE   F22                                                              
*                                                                               
F24      CLC   PTAWREF,FTBVAL      LOOK FOR MATCHING WRITE-FF/RECOVERY          
         BE    FYEX                                                             
         B     F22                                                              
*                                                                               
F26      CLC   PTARBLNO,FTBVAL     LOOK FOR MATCHING BILL                       
         BNE   F22                                                              
*                                                                               
FYEX     L     R5,ATRN             WANT IT-SO TELL FILTER RTN IN ROOT           
         B     *+8                                                              
*                                                                               
FNOX     LA    R5,=F'0'            DON'T WANT IT                                
         XIT1  REGS=(R5)                                                        
         DROP  R3,R4,R6,R7                                                      
         EJECT                                                                  
*              MAIN PROCESS                                                     
*                                                                               
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ11*                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60611,RB                                                        
         USING T606TWA,RA                                                       
         L     R7,ALOCAL                                                        
         USING LOCALD,R7                                                        
         L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         SPACE 1                                                                
         CLI   ACKEYACC,RUNLAST    END                                          
         BE    T01                                                              
         CLI   ACKEYDTE,C' '       MUST BE TRANSACTION                          
         BE    TNEXT                                                            
         SPACE 1                                                                
T00A     TM    OPTIONS,MOSRANGE    HANDLE MOS RANGE FILTER                      
         BZ    T01                                                              
         LA    R6,FILTAB                                                        
         USING FTBD,R6                                                          
         TM    FTBSTAT,5           DISABLED+SUBRECORD = MUST BE MOSRNGE         
         BO    *+12                IN WHICH CASE FTBVAL(12),FTBVAL+2(2)         
         LA    R6,FTBTBLEN(R6)     CONTAIN LOW & HIGH MOS CODES                 
         B     *-12                                                             
         ICM   R4,15,ATRN                                                       
         BZ    TNEXT                                                            
         USING TRANSD,R4                                                        
         GOTO1 CONVMOS,DMCB,TRANSD,WORK                                         
         CLC   WORK(2),FTBVAL+0                                                 
         BL    TNEXT                                                            
         CLC   WORK(2),FTBVAL+2                                                 
         BH    TNEXT                                                            
         DROP  R4,R6                                                            
         SPACE 1                                                                
T01      CLI   VIRGIN,C'H'                                                      
         BE    T1                                                               
         EJECT                                                                  
*              FIRST TIME ACTIONS                                               
*                                                                               
         MVI   VIRGIN,C'H'                                                      
         CLI   LASTKMK,0           FIRST TIME THROUGH SAVE WORKCODE             
         BNE   T05                 AND CLEAR WORKCODE AND ACCOUNT TOTS          
         CLI   ACKEYACC,RUNLAST                                                 
         BE    TEND10                                                           
         MVC   SAVEWC,ACKEYWRK                                                  
         LA    R1,4                                                             
         LA    R2,TOTALS                                                        
         ZAP   0(6,R2),=P'0'                                                    
         LA    R2,6(R2)                                                         
         BCT   R1,*-10                                                          
         MVI   INVTIND,C'N'        INVOICE TOTAL INDICATOR                      
*                                                                               
T05      CLI   LINE+1,2                                                         
         BE    T06                                                              
         LA    R6,INFDAT2H                                                      
         GOTO1 EDITACNM            SET UP ACCOUNT & CONTRA NAMES                
*                                                                               
T06      MVC   INFDAT3,HEADING                                                  
         OI    INFDAT3H+6,X'80'                                                 
         MVC   INFDAT4,HEADING2                                                 
         OI    INFDAT4H+6,X'80'                                                 
         MVI   LINE+1,4                                                         
         B     T2                                                               
*                                                                               
HEADING  DC    CL41'WC CONTRA       DATE/LAST  REF    BATCH/'                   
         DC    CL37'  ALLOCATED        NET     BALANCE'                         
HEADING2 DC    CL41'-- ------       BILL DATE  ---   BILL NO'                   
         DC    CL37'    AMOUNT       AMOUNT    -------'                         
         EJECT                                                                  
*              EACH TIME ACTIONS                                                
*                                                                               
T1       CLI   LINE+1,18           CHECK FOR SCREEN FULL                        
         BH    TFULL                                                            
         TM    OPTIONS,DDSONLY                                                  
         BNO   *+12                                                             
         CLI   LINE+1,18                                                        
         BNL   TFULL                                                            
*                                                                               
T2       LH    R6,LINE             SET UP DISPLAY FOR A RECORD                  
         LR    R9,R6               R9 = LINE NUMBER                             
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         CLI   ACKEYACC,RUNLAST                                                 
         BE    TEND                                                             
*                                                                               
         CLC   ACKEYWRK,SAVEWC     WORKCODE CONTROL BREAK                       
         BE    T27                                                              
         OC    SAVEWC,SAVEWC       JUST DONE WC TOTALS IE TOP OF PAGE           
         BZ    *+8                                                              
         BAS   RE,WORKTOTS                                                      
         XC    SAVEWC,SAVEWC                                                    
         CLC   ACKEYWRK,=C'99'     DO INVOICE TOTALS JUST BEFORE BILLS          
         BNE   T26                                                              
         CLI   LINE+1,18                                                        
         BH    TFULL                                                            
         LH    R6,LINE             BUMP LINE POINTER                            
         LR    R9,R6               R9 = LINE NUMBER                             
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         BAS   RE,INVTOTS                                                       
T26      MVC   SAVEWC,ACKEYWRK                                                  
         B     T1                                                               
*                                                                               
T27      L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         SPACE 1                                                                
         TM    OPTIONS,PEELED      SKIP PEELED UNLESS PEELED=YES                
         BZ    T2A                 IN WHICH CASE SHOW ONLY PEELED               
         OC    ACDTPEEL,ACDTPEEL                                                
         BZ    TNEXT                                                            
         B     *+14                                                             
*                                                                               
T2A      OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   TNEXT                                                            
         ICM   R5,15,ATRN                                                       
         BZ    TNEXT                                                            
*                                                                               
         USING TRANSD,R5                                                        
T50      MVC   LINEDATA+34(6),TRNSBTCH                                          
         LA    R8,LINEDATA+55                                                   
*                                                                               
         CLC   SAVEWC,=C'**'       UNMATCHED ORDER WORKCODE - SHOW              
         BNE   T52                 O/S EST AMT IN BRACKETS                      
         ICM   RF,15,AOAM                                                       
         BZ    T5A                                                              
*                                                                               
         USING ACOAMTD,RF                                                       
         SR    RE,RE                                                            
T51      AP    TRNSAMNT,ACOAMT     EST                                          
         SP    TRNSAMNT,ACOAIVAL   MINUS INVOICED TO DATE                       
         IC    RE,ACOALEN                                                       
         AR    RF,RE                                                            
         CLI   0(RF),X'68'                                                      
         BE    T51                                                              
         SH    R8,=H'2'                                                         
         EDIT  TRNSAMNT,(12,0(R8)),2,MINUS=YES,BRACKET=YES                      
         B     T54                                                              
         DROP  RF                                                               
*                                                                               
T52      ZAP   TRNSAVE,TRNSAMNT                                                 
T54      AP    WRKAMNT,TRNSAMNT                                                 
*                                                                               
T5A      L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         MVC   LINEDATA(2),ACKEYWRK                                             
         LA    R3,13                                                            
         LA    R4,ACKEYCON+1                                                    
*                                                                               
T6       CLI   0(R4),C' '                                                       
         BH    T7                                                               
         LA    R4,1(R4)                                                         
         BCT   R3,T6                                                            
*                                                                               
T7       EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LINEDATA+3(0),0(R4)                                              
         GOTO1 VDATCON,DMCB,(1,ACKEYDTE),(8,LINEDATA+18)                        
*&&UK*&& OI    LINEDATA+18,X'F0'                                                
         MVC   LINEDATA+27(6),ACKEYREF                                          
*                                                                               
         CLC   SAVEWC,=C'**'       ORDER                                        
         BE    T92                                                              
         ZAP   TRNALLOC,=P'0'                                                   
         ZAP   BILLTOT,=P'0'                                                    
*                                                                               
         MVC   WORK(8),SPACES                                                   
         ZAP   DUB,TRNSAVE         IF ALL BILLED USE THIS                       
*                                                                               
         GOTO1 PRORATA,DMCB,(X'C0',(R5)),0,ACOMFACS,0,PROBLK,PROLST             
*                                                                               
         USING PTAELD,R3                                                        
         LA    R3,PROLST                                                        
         CLI   0(R3),PTAELQ                                                     
         B     T72+8                                                            
*                                                                               
T72      MVI   ELCODE,PTAELQ       LOOK FOR THE RIGHT ELEMENT                   
         BAS   RE,NEXTEL                                                        
         BNE   T78                                                              
*                                                                               
         TM    PTASTAT1,PTASPEND   SKIP PENDING                                 
         BO    T72                                                              
         CLI   PTATYPE,PTATRAL     ACCEPT BILLING                               
         BE    T76                                                              
         CLI   PTATYPE,PTATWOF     WRITE-OFFS                                   
         BE    T74                                                              
         CLI   PTATYPE,PTATWOFR    AND RECOVERIES                               
         BNE   T74                                                              
*                                                                               
T74      AP    BILLTOT,PTANET      ADD AMOUNT TO BUCKET                         
         CLC   WORK(8),SPACES      ANY BILL NUMBER YET?                         
         BE    *+14                NO, MOVE ONE IN WITH DATE                    
         CLC   WORK+6(2),PTADATE   YES, IS DATE LATER THAN THIS?                
         BH    T72                 YES, LEAVE IT ALONE                          
         MVC   WORK(L'PTAWREF),PTAWREF                                          
         MVC   WORK+L'PTAWREF(L'PTADATE),PTADATE                                
         B     T72                                                              
*                                                                               
T76      AP    BILLTOT,PTANET      ADD AMOUNT TO BUCKET                         
         CLC   WORK(8),SPACES      ANY BILL NUMBER YET?                         
         BE    *+14                NO, MOVE ONE IN WITH DATE                    
         CLC   WORK+6(2),PTARDATE  YES, IS DATE LATER THAN THIS?                
         BH    T72                 YES, LEAVE IT ALONE                          
         MVC   WORK(L'PTARBLNO),PTARBLNO                                        
         MVC   WORK+L'PTARBLNO(L'PTARDATE),PTARDATE                             
         B     T72                                                              
*                                                                               
T78      CP    BILLTOT,=P'0'       ANY BILLING?                                 
         BE    T90                 NO                                           
         ZAP   TRNALLOC,BILLTOT                                                 
         AP    WRKALLOC,BILLTOT                                                 
*                                                                               
         MVC   LINEDATA+34(6),WORK                                              
         GOTO1 VDATCON,DMCB,(2,WORK+6),(8,LINEDATA+18)                          
*                                                                               
T90      LA    R1,TRNALLOC                                                      
         BAS   RE,FORMAT                                                        
         CLC   SAVEWC,=C'99'       NO BALANCE FOR BILLS                         
         BNE   T92                                                              
         MVC   LINEDATA+65(11),SPACES                                           
*                                                                               
T92      OI    LINEHDR+6,X'80'                                                  
         LA    R9,1(R9)                                                         
         STH   R9,LINE                                                          
         TM    OPTIONS,DDSONLY                                                  
         BNO   TNEXT                                                            
         BAS   RE,DDSXTRAS                                                      
         SPACE 1                                                                
TNEXT    LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
*              RUNLAST ACTIONS                                                  
*                                                                               
TEND     OC    SAVEWC,SAVEWC       HAVE FINAL WC TOTALS BEEN DISPLAYED          
         BZ    TEND0               IF NOT DISPLAY THEM                          
         BAS   RE,WORKTOTS                                                      
         XC    SAVEWC,SAVEWC                                                    
         CLI   LINE+1,18                                                        
         BH    TFULL                                                            
         LH    R6,LINE                                                          
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         SPACE 1                                                                
TEND0    CLI   INVTIND,C'Y'                                                     
         BE    TEND2                                                            
         CLI   LINE+1,18                                                        
         BH    TFULL                                                            
         LH    R6,LINE                                                          
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         BAS   RE,INVTOTS                                                       
         CLI   LINE+1,18                                                        
         BH    TFULL                                                            
         LH    R6,LINE                                                          
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         SPACE 1                                                                
TEND2    MVC   LINEDATA+3(13),=C'ACCOUNT TOTAL'                                 
         LA    R1,TOTALLOC                                                      
         BAS   RE,FORMAT                                                        
         OI    LINEHDR+6,X'80'                                                  
*                                                                               
TEND10   SR    RF,RF               SET CC TO EQU FOR END                        
         B     XIT                                                              
         SPACE 1                                                                
TFULL    LNR   RB,RB               SET CC TO NEGATIVE FOR SCREEN FULL           
         MVI   LINE+1,2            AND SAVE SOME HEADS                          
TXIT     XIT1                                                                   
         EJECT                                                                  
*              SET UP DISPLAY LINE FOR DDS=YES OPTION                           
         SPACE 1                                                                
DDSXTRAS NTR1                                                                   
         LH    R6,LINE             R6 = A(LINE HEADER) - WE ALREADY             
         MH    R6,=H'86'           KNOW THERE'S ROOM FOR IT                     
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         MVI   UNSCANBK,C' '                                                    
         MVC   UNSCANBK+1(199),UNSCANBK                                         
         SR    R3,R3               R3 = COUNT OF UNSCAN FIELDS                  
         LA    R4,UNSCANBK         R4 = A(UNSCAN BLOCK)                         
         L     R5,ATRN                                                          
         USING TRANSD,R5                                                        
         SPACE 1                                                                
DX1      CLI   TRNSTYPE,0          TYPE                                         
         BE    DX2                                                              
         MVC   0(2,R4),=C'TY'                                                   
         EDIT  TRNSTYPE,(2,10(R4)),FILL=0                                       
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
         SPACE 1                                                                
DX2      MVC   0(2,R4),=C'ST'      STATUS IN HEX                                
         GOTO1 VHEXOUT,DMCB,TRNSSTAT,10(R4),1,=C'MIX'                           
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
         SPACE 1                                                                
DX3      ICM   R5,15,AOTH          OTHERS                                       
         BZ    DX6                                                              
         USING ACOTHERD,R5                                                      
         CLC   ACOTNUM(L'ACOTNUM+L'ACOTPROF),SPACES                             
         BE    DX6                                                              
         MVC   0(2,R4),=C'OT'                                                   
         LA    R8,10(R4)                                                        
         CLC   ACOTNUM,SPACES                                                   
         BE    DX4                                                              
         MVC   0(L'ACOTNUM,R8),ACOTNUM                                          
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
         LA    R8,10(R8)                                                        
DX4      CLC   ACOTPROF,SPACES                                                  
         BE    DX6                                                              
         MVC   0(L'ACOTPROF,R8),ACOTPROF                                        
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
         SPACE 1                                                                
DX6      ICM   R5,15,ACSH          SUBSIDIARY CASH                              
         BZ    DX8                                                              
         USING TRCASHD,R5                                                       
         MVC   0(2,R4),=C'AM'                                                   
         EDIT  (P6,TRCSAMNT),(10,10(R4)),2,MINUS=YES,ALIGN=LEFT                 
         MVC   20(1,R4),TRCSTYPE                                                
         LA    R3,2(R3)                                                         
         LA    R4,2*L'UNSCANBK(R4)                                              
         SPACE 1                                                                
DX8      ICM   R5,15,ATRS          ACTIVITY DATE                                
         BZ    DX9                                                              
         USING TRSTATD,R5                                                       
         MVC   0(2,R4),=C'DA'                                                   
         GOTO1 VDATCON,DMCB,(2,TRSTDATE),(X'20',10(R4))                         
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
         SPACE 1                                                                
DX9      L     R5,AIO              USED DATE                                    
         USING ACKEYD,R5                                                        
         OC    ACDTUSED,ACDTUSED                                                
         BZ    DX10                                                             
         MVC   0(2,R4),=C'US'                                                   
         GOTO1 VDATCON,DMCB,(2,ACDTUSED),(X'20',10(R4))                         
         LA    R4,L'UNSCANBK(R4)                                                
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
DX10     OC    ACDTPEEL,ACDTPEEL   PEEL DATE                                    
         BZ    DX11                                                             
         MVC   0(2,R4),=C'PE'                                                   
         GOTO1 VDATCON,DMCB,(2,ACDTPEEL),(X'20',10(R4))                         
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
DX11     LTR   R3,R3               ANYTHING TO UNSCAN                           
         BZ    DXIT                                                             
         MVC   WORK,SPACES                                                      
         GOTO1 VUNSCAN,DMCB,((R3),UNSCANBK),(C'C',WORK)                         
         OI    LINEHDR+6,X'80'                                                  
         MVI   LINEDATA,C'('       PARENTHESES ROUND IT                         
         MVC   LINEDATA+1(77),WORK                                              
         LA    R4,LINEDATA+76                                                   
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         LH    R1,LINE             BUMP LINE NUMBER                             
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
DXIT     B     XIT                                                              
         EJECT                                                                  
*              DISPLAY WORKCODE TOTALS AND UPDATE ACCOUNT TOTALS                
*              ON ENTRY SAVEWC   = WORKCODE                                     
*                       R6       = A(LINE HEADER)                               
*              ON EXIT  LINE     = UPDATED LINE NUMBER                          
         SPACE 1                                                                
WORKTOTS NTR1                                                                   
         LH    R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         MVC   LINEDATA(2),SAVEWC                                               
         MVC   LINEDATA+3(14),=C'WORKCODE TOTAL'                                
         LA    RF,LINEDATA+53                                                   
WT2      CLC   SAVEWC,=C'**'       UNMATCHED ORDERS IN BRACKETS AND NOT         
         BNE   WT3                 INCLUDED IN ACCOUNT TOTAL                    
         SH    RF,=H'2'                                                         
         EDIT  WRKAMNT,(14,0(RF)),2,MINUS=YES,BRACKET=YES                       
         B     WT10                                                             
         SPACE 1                                                                
WT3      LA    R1,WRKALLOC                                                      
         BAS   RE,FORMAT                                                        
         CLC   SAVEWC,=C'99'       FOR BILLS SUBTRACT FROM BALANCE              
         BNE   WT4                 AND OMIT BALANCE COLUMN                      
         MVC   LINEDATA+65(11),SPACES                                           
         SP    TOTAMNT,WRKAMNT                                                  
         B     WT10                                                             
WT4      AP    TOTAMNT,WRKAMNT                                                  
         AP    TOTALLOC,WRKALLOC                                                
*                                                                               
WT10     OI    LINEHDR+6,X'80'                                                  
         ZAP   WRKAMNT,=P'0'                                                    
         ZAP   WRKALLOC,=P'0'                                                   
WTXIT    B     XIT                                                              
         EJECT                                                                  
*              DISPLAY INVOICE TOTALS                                           
*              ON ENTRY SAVEWC   = WORKCODE                                     
*                       R6       = A(LINE HEADER)                               
*              ON EXIT  LINE     = UPDATED LINE NUMBER                          
*                                                                               
INVTOTS  NTR1                                                                   
         LH    R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         MVC   LINEDATA(2),SAVEWC                                               
         MVC   LINEDATA+3(13),=C'INVOICE TOTAL'                                 
         LA    R1,TOTALLOC                                                      
         BAS   RE,FORMAT                                                        
         ZAP   TOTALLOC,=P'0'                                                   
         OI    LINEHDR+6,X'80'                                                  
         MVI   INVTIND,C'Y'        SET INDICATOR TO SAY WE DID IT               
         B     XIT                                                              
         EJECT                                                                  
*              CALCULATE BALANCE AND EDIT 3 OUTPUT FIELDS                       
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         LA    RF,LINEDATA+41      ALLOCATED AMOUNT                             
         CP    0(6,R1),=P'0'                                                    
         BE    FMT2                                                             
         EDIT  (P6,0(R1)),(12,0(RF)),2,MINUS=YES                                
FMT2     CP    6(6,R1),=P'0'                                                    
         BE    FMT4                                                             
         LA    RF,LINEDATA+53      NET                                          
         EDIT  (P6,6(R1)),(12,0(RF)),2,MINUS=YES                                
FMT4     ZAP   DUB1,6(6,R1)                                                     
         SP    DUB1,0(6,R1)                                                     
         CP    DUB1,=P'0'                                                       
         BE    XIT                                                              
         LA    RF,LINEDATA+65      BALANCE                                      
         EDIT  (P8,DUB1),(11,0(RF)),2,MINUS=YES                                 
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              LOCAL WS AND EQUATES                                             
         SPACE 1                                                                
LOCALD   DSECT                                                                  
DUB1     DS    D                                                                
CALLTYPE DS    C                                                                
TRNALLOC DS    PL6                                                              
TRNSAVE  DS    PL6                 TRANSACTION AMOUNT                           
BILLTOT  DS    PL8                 BILLED AMOUNT                                
*                                                                               
PEELED   EQU   X'20'               PEELED=YES OPTION IN USE                     
MOSRANGE EQU   X'02'                                                            
DETAIL   EQU   C'D'                                                             
WORKCODE EQU   C'W'                                                             
PROBLK   DS    CL(PR$LNQ)                                                       
PROLST   DS    CL2000                                                           
         EJECT                                                                  
*                                                                               
*              NESTED INCLUDE FOR ACINQDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
SAVEWC   DS    CL2       C         SAVED WORKCODE                               
TOTALS   DS    0C                  WORKCODE AND ACCOUNT TOTALS                  
TOTALLOC DS    PL6       P         TOTAL BILLED FOR ACCOUNT                     
TOTAMNT  DS    PL6       P         TOTAL TRNSAMNT FOR ACCOUNT                   
WRKALLOC DS    PL6       P         TOTAL BILLED FOR WORKCODE                    
WRKAMNT  DS    PL6       P         TOTAL TRNSAMNT FOR WORKCODE                  
INVTIND  DS    CL1                 INDICATOR FOR INVOICE TOTAL PRINTING         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACINQ11   05/01/02'                                      
         END                                                                    
