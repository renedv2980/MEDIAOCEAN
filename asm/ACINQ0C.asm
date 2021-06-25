*          DATA SET ACINQ0C    AT LEVEL 008 AS OF 10/28/96                      
*PHASE T6060CA,*,NOAUTO                                                         
         TITLE 'ACCOUNT ENQUIRY MK2 - ORDER DETAIL - T6060C'                    
T6060C   CSECT                                                                  
         PRINT NOGEN                                                            
*-------------------------------------------------------------                  
*        TABLE FOR TYPE 'OD' IN ACCOUNT ENQUIRY PROGRAM                         
*-------------------------------------------------------------                  
*                                                                               
         DC    A(KEYTABLE-T6060C)                                               
         DC    A(FILTABLE-T6060C)                                               
         DC    A(KNTRYPNT-T6060C)                                               
         DC    A(FNTRYPNT-T6060C)                                               
         DC    A(DNTRYPNT-T6060C)                                               
         DS    A                                                                
         DS    A                                                                
         EJECT                                                                  
*-------------------------------------------------------------                  
*        KEYS TABLE COVERED BY DSECT KEYTABD                                    
*-------------------------------------------------------------                  
*                                                                               
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
         DC    CL10'WORK CODE'     WORK CODE **                                 
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(15)                                                          
         DC    AL1(2)                                                           
         DC    AL2(WORK-GWS)                                                    
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'THE REST'                                                   
         DC    C' '                                                             
         DC    C'V'                                                             
         DC    X'00'                                                            
         DC    AL1(17)                                                          
         DC    AL1(25)                                                          
         DC    AL2(SPACES-GWS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        FILTER TABLE COVERED BY DSECT FILTERSD                                 
*-------------------------------------------------------------                  
*                                                                               
FILTABLE DC    CL10'DATE'                                                       
         DC    CL2'DA'                                                          
         DC    X'44'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DELETED'                                                    
         DC    CL2'DE'                                                          
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'00'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    CL10'END'                                                        
         DC    CL2'EN'                                                          
         DC    X'44'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITEND-GWS)                                                 
*                                                                               
         DC    CL10'ORDER'                                                      
         DC    CL2'OR'                                                          
         DC    X'04'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYREF-ACKEYD)                                             
         DC    AL1(L'ACKEYREF)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'REFERENCE'                                                  
         DC    CL2'RE'                                                          
         DC    X'04'                                                            
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
         DC    X'44'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITSTRT-GWS)                                                
*                                                                               
         DC    CL10'WORKCODE'                                                   
         DC    CL2'WC'                                                          
         DC    X'04'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AOAM-GWS)                                                    
         DC    AL1(ACOAWC-ACOAMTD)                                              
         DC    AL1(L'ACOAWC)                                                    
         DC    AL2(0)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
         EJECT                                                                  
*-------------------------------------------------------------                  
*        KNTRYPNT MODULE                                                        
*-------------------------------------------------------------                  
*                                                                               
KNTRYPNT DS    0D                                                               
         NMOD1 0,**INQC**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6060C,RB                                                        
         MVC   WORK(2),SPACES                                                   
         CLC   KEY+1(2),=C'SJ'     IF DEALING WITH A PRODUCTION ORDER           
         BNE   TXIT                                                             
         MVC   WORK(2),=2C'*'      FIX WORKCODE IN KEY TO **                    
         B     TXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------                  
*        FNTRYPNT MODULE                                                        
*-------------------------------------------------------------                  
*                                                                               
FNTRYPNT DS    0D                  HANDLE MULTIPLE WORKCODE FILTER              
         NMOD1 0,**INQC**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6060C,RB                                                        
         LA    R2,WORK                                                          
         SR    R0,R0                                                            
         L     RF,AOAM                                                          
*                                                                               
         USING ACOAMTD,RF                                                       
F2       CLI   ACOAEL,X'68'                                                     
         BNE   FXIT                                                             
         MVC   WORK(2),ACOAWC                                                   
         CLC   ACOAWC,0(R5)                                                     
         BE    FXIT                                                             
         IC    R0,ACOALEN                                                       
         AR    RF,R0                                                            
         B     F2                                                               
*                                                                               
FXIT     XIT1  REGS=(R2)                                                        
         DROP  RF                                                               
         EJECT                                                                  
*-------------------------------------------------------------                  
*        MAIN PROCESS                                                           
*-------------------------------------------------------------                  
*                                                                               
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQC**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6060C,RB                                                        
         USING T606TWA,RA                                                       
         L     R7,ALOCAL                                                        
         USING LOCALD,R7                                                        
         L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         CLI   ACKEYD,RUNLAST                                                   
         BE    T02                                                              
*                                                                               
T00      CLI   ACKEYDTE,C' '       SAVE NAME FROM CONTRA HEADER IN TWA          
         BNE   T01                                                              
         MVC   SUPPCODE,ACKEYCON                                                
         MVC   SUPPNAME,SPACES                                                  
         ICM   R4,15,ASUB                                                       
         BZ    TNEXT                                                            
         USING TRSUBHD,R4                                                       
         ZIC   R0,TRSBLEN                                                       
         SH    R0,=H'17'                                                        
         BNP   TNEXT                                                            
         GOTO1 VCHOPPER,DMCB,((R0),TRSBNAME),(20,SUPPNAME),3                    
         B     TNEXT                                                            
         DROP  R4                                                               
*                                                                               
T01      MVI   DMCB,0              APPLY FILTERS TO TRANSACTIONS                
         GOTO1 AFILTER                                                          
         BZ    TNEXT                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        FIRST TIME ACTIONS                                                     
*-------------------------------------------------------------                  
*                                                                               
T02      CLI   VIRGIN,C'H'                                                      
         BE    T1                                                               
         CLI   LASTKMK,0                                                        
         BNE   T05                                                              
         ZAP   ACESTT,=P'0'                                                     
         ZAP   ACNUMT,=P'0'                                                     
         ZAP   ACACTT,=P'0'                                                     
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
         CLI   LASTKMK,0                                                        
         BNE   *+12                                                             
         CLI   ACKEYACC,RUNLAST                                                 
         BE    TEND1                                                            
         MVI   VIRGIN,C'H'                                                      
         B     T1                                                               
*                                                                               
HEADING  DC    CL39'----SUPPLIER-----                 ---OR'                    
         DC    CL39'DER---     WC     ORDER      -INVOICED-'                    
HEADING2 DC    CL39'CODE         NAME                 NUMBE'                    
         DC    CL39'R DATE     --    AMOUNT      AMOUNT   #'                    
         EJECT                                                                  
*-------------------------------------------------------------                  
*        EACH TIME ACTIONS                                                      
*-------------------------------------------------------------                  
*                                                                               
T1       CLI   LINE+1,18           CHECK FOR SCREEN FULL                        
         BH    TFULL                                                            
*                                                                               
T2       LH    R6,LINE             SET UP DISPLAY FOR A RECORD                  
         LR    R9,R6               R9 = LINE NUMBER                             
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         CLI   ACKEYACC,RUNLAST                                                 
         BE    TEND                                                             
         ICM   R5,15,ATRN                                                       
         BZ    TNEXT                                                            
*                                                                               
         USING TRANSD,R5                                                        
T2B      L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         CLI   DELETED,C'Y'        ONLY DELETED TRANSACTIONS                    
         BNE   T3                                                               
         TM    ACSTATUS,X'A0'                                                   
         BZ    TNEXT                                                            
*                                                                               
T3       MVI   BLOCK,C' '          BUILD A DISPLAY OF 1-9 LINES IN              
         MVC   BLOCK+1(240),BLOCK  BLOCK                                        
         MVC   BLOCK+241(240),BLOCK+240                                         
         MVC   BLOCK+481(240),BLOCK+480                                         
         MVC   BLOCK+721(80),BLOCK+720                                          
         MVC   BLOCK(12),ACKEYCON+3   CONTRA CODE                               
         CLC   SUPPCODE,ACKEYCON   DO WE HAVE CONTRA NAME IN TWA                
         BNE   T31                 NO                                           
         MVC   BLOCK+13(L'SUPPN1),SUPPN1                                        
         MVC   BLOCK+80+13(L'SUPPN1),SUPPN2                                     
         MVC   BLOCK+160+13(L'SUPPN1),SUPPN3                                    
*                                                                               
T31      MVC   BLOCK+34(6),TRNSREF                                              
         GOTO1 VDATCON,DMCB,(1,TRNSDATE),(8,BLOCK+41)                           
*&&UK*&& OI    BLOCK+41,X'F0'                                                   
*                                                                               
T4       ICM   R4,15,AOAM          LINE PER WORKCODE                            
         BZ    TNEXT                                                            
         USING ACOAMTD,R4                                                       
         LA    R3,BLOCK                                                         
         MVI   BLKLINES,C'N'                                                    
         ZAP   ORESTT,=P'0'                                                     
         ZAP   ORACTT,=P'0'                                                     
         ZAP   ORNUMT,ACOAINUM     NUMBER OF INVOICES                           
         B     T42                                                              
*                                                                               
T41      DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'68'         ORDER ELEMENT                                
         BNE   T44                                                              
         MVI   BLKLINES,C'Y'       SET MORE THAN ONE LINE SWITCH                
*                                                                               
T42      DS    0H                                                               
         AP    ORESTT,ACOAMT       ORDER AMOUNT TOTAL                           
         AP    ORACTT,ACOAIVAL     AMOUNT INVOICED TO DATE                      
*                                                                               
         MVC   50(2,R3),ACOAWC                                                  
         EDIT  (P6,ACOAMT),(10,53(R3)),2,MINUS=YES                              
         CP    ACOAIVAL,=P'0'                                                   
         BE    T43                                                              
         EDIT  (P6,ACOAIVAL),(10,65(R3)),2,MINUS=YES                            
*                                                                               
T43      LA    R3,80(R3)           BUMP TO NEXT LINE                            
         B     T41                                                              
*                                                                               
T44      DS    0H                                                               
         EDIT  (P6,ORNUMT),(2,BLOCK+76)                                         
         CLI   BLKLINES,C'N'       NO TOTAL LINE IF ONLY ONE LINE               
         BE    T45                                                              
         MVC   34(11,R3),=C'ORDER TOTAL'                                        
         EDIT  (P6,ORESTT),(12,51(R3)),2,MINUS=YES                              
         EDIT  (P6,ORACTT),(12,63(R3)),2,MINUS=YES                              
         EDIT  (P6,ORNUMT),(2,76(R3))                                           
         MVC   BLOCK+76(2),SPACES                                               
*                                                                               
T45      LA    R3,BLOCK+80         CHECK FOR ROOM ON SCREEN                     
         SR    R1,R1                                                            
*                                                                               
T46      CLC   0(80,R3),SPACES                                                  
         BZ    T47                                                              
         LA    R3,80(R3)                                                        
         LA    R1,1(R1)                                                         
         B     T46                                                              
*                                                                               
T47      AR    R1,R9                                                            
         CH    R1,=H'18'                                                        
         BH    TFULL                                                            
*                                                                               
T5       LA    R3,BLOCK            MOVE DISPLAY TO TWA                          
T51      CLC   0(80,R3),SPACES                                                  
         BZ    T6                                                               
         MVC   LINEDATA,0(R3)                                                   
         OI    LINEHDR+6,X'80'                                                  
         LA    R9,1(R9)                                                         
         LA    R6,86(R6)                                                        
         LA    R3,80(R3)                                                        
         B     T51                                                              
*                                                                               
T6       STH   R9,LINE             UPDATE A/C TOTS                              
         AP    ACESTT,ORESTT                                                    
         AP    ACNUMT,ORNUMT                                                    
         AP    ACACTT,ORACTT                                                    
*                                                                               
TNEXT    LTR   RB,RB                                                            
         B     TXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------                  
*        RUNLAST ACTIONS                                                        
*-------------------------------------------------------------                  
*                                                                               
TEND     MVC   LINEDATA+34(13),=C'ACCOUNT TOTAL'                                
         LA    RF,LINEDATA                                                      
         EDIT  (P6,ACESTT),(12,51(RF)),2,MINUS=YES                              
         EDIT  (P6,ACACTT),(12,63(RF)),2,MINUS=YES                              
         OI    LINEHDR+6,X'80'                                                  
*                                                                               
TEND1    SR    RF,RF               SET CC TO EQU FOR END                        
         B     TXIT                                                             
*                                                                               
TFULL    LNR   RB,RB               SET CC TO NEGATIVE FOR SCREEN FULL           
         MVI   LINE+1,2            AND SAVE SOME HEADS                          
TXIT     XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------                  
*        LITERALS                                                               
*-------------------------------------------------------------                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------                  
*        LOCAL WS AND EQUATES                                                   
*-------------------------------------------------------------                  
*                                                                               
LOCALD   DSECT                                                                  
ORESTT   DS    PL6                 ORDER TOTALS                                 
ORNUMT   DS    PL6                                                              
ORACTT   DS    PL6                                                              
BLKLINES DS    XL1                 NUMBER OF BLOCK LINES USED                   
BLOCK    DS    CL800                                                            
*                                                                               
*-------------------------------------------------------------                  
*        NESTED INCLUDE FOR ACINQDSECT                                          
*-------------------------------------------------------------                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
ACESTT   DS    PL6                 ACCOUNT TOTALS                               
ACNUMT   DS    PL6                                                              
ACACTT   DS    PL6                                                              
SUPPCODE DS    CL15                SUPPLIER CODE FOR SAVED NAME                 
SUPPNAME DS    0CL60               SAVED NAME CHOPPED TO 3 LINES                
SUPPN1   DS    CL20                                                             
SUPPN2   DS    CL20                                                             
SUPPN3   DS    CL20                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACINQ0C   10/28/96'                                      
         END                                                                    
