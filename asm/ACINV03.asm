*          DATA SET ACINV03    AT LEVEL 014 AS OF 05/01/02                      
*PHASE T61303A                                                                  
         TITLE 'ACINV03 - INVOICE MARKING - MANUAL CHEQUE'                      
ACINV03  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**INV3**,RA                                                    
         USING INVWORKD,R9                                                      
         USING TWAD,R8                                                          
         L     R7,ASAVE                                                         
         USING LOCALD,R7                                                        
         CLI   OVMODE,INIT                                                      
         BE    KEYVAL                                                           
         MVC   WORK(L'CHQPAY),CHQPAY                                            
         OC    WORK,SPACES                                                      
         CLC   LKEY,WORK           START AGAIN IF KEY HAS CHANGED               
         BE    *+12                                                             
         MVI   OVMODE,INIT                                                      
         B     KEYVAL                                                           
         CLI   OVMODE,MARK                                                      
         BE    UPDATE                                                           
         B     OKEND                                                            
         EJECT                                                                  
KEYVAL   GOTO1 AFVAL,CHQPAYH                                                    
         BZ    EXIT                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),FLD                                                    
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   EXIT                                                             
         MVC   LKEY,KEY+1                                                       
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   CHQPAYN,WORK                                                     
         OI    CHQPAYNH+6,X'80'                                                 
         GOTO1 AFVAL,CHQCHQH       VALIDATE AND SAVE CHEQUE NO                  
         BZ    EXIT                                                             
         MVC   CHNO,FLD                                                         
         GOTO1 AFVAL,CHQDTEH       DATE                                         
         BZ    EXIT                                                             
         MVI   FERN,INVDATE                                                     
         GOTO1 VDATVAL,DMCB,(0,FLD),WORK                                        
         OC    DMCB(4),DMCB                                                     
         BZ    EXIT                                                             
         GOTO1 VDATCON,DMCB,(0,WORK),(2,CHDTE)                                  
         GOTO1 (RF),(R1),,(1,DATE3)                                             
         CLI   ACTION,UNCHQ                                                     
         BE    KEYV2               SKIP IF UNCHEQUE                             
         GOTO1 AFVAL,CHQAMTH       AMOUNT                                       
         BZ    EXIT                                                             
         MVI   FERN,INVAMNT                                                     
         ZIC   RF,CHQAMTH+5                                                     
         GOTO1 VCASHVAL,DMCB,CHQAMT,(RF)                                        
         CLI   0(R1),X'FF'                                                      
         BE    EXIT                                                             
         L     RF,DMCB+4                                                        
         CVD   RF,DUB                                                           
         ZAP   CHAMT,DUB                                                        
         GOTO1 AFVAL,CHQBNKH       AND BANK ACCOUNT                             
         BZ    EXIT                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),BANKUL                                                  
         MVC   KEY+3(12),FLD                                                    
         CLI   FLD,C'*'            ALLOW BANK ACCOUNT TO BE ANYWHERE            
         BNE   KEYV1                                                            
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(12),FLD+1                                                  
KEYV1    GOTO1 AREAD,AIOAREA1                                                   
         BNE   EXIT                                                             
         MVC   CHBNK,KEY+1                                                      
         GOTO1 VDATCON,DMCB,(1,TODAYP),(2,TODAY2)                               
*                                                                               
         B     KEYV2               CODE BELOW SUSPENDED UNTIL                   
*                                  DISCUSSIONS WITH USERS(1/85)                 
*                                                                               
         MVC   KEY,SPACES          NOW SEE IF CHEQUE ACTUALLY POSTED            
         LA    R2,KEY                                                           
         USING ACKEYD,R2           READ IN BANK ACCOUNT                         
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(L'CHBNK),CHBNK                                        
         MVC   ACKEYCON,COMPANY    CONTRA PAYEE                                 
         MVC   ACKEYDTE,DATE3                                                   
         MVC   ACKEYREF,CHNO                                                    
         MVI   ACKEYSBR,0                                                       
         LA    RF,CHQCHQH                                                       
         ST    RF,FADR                                                          
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   EXIT                                                             
         L     R2,AIOAREA1                                                      
         LA    R3,ACRECORD                                                      
         USING TRANSD,R3                                                        
         CP    TRNSAMNT,CHAMT                                                   
         BE    KEYV2                                                            
         LA    RF,CHQAMTH                                                       
         ST    RF,FADR                                                          
         MVI   FERN,INVAMNT                                                     
         B     EXIT                                                             
KEYV2    MVC   KEY,SPACES                                                       
         MVC   KEY(15),COMPANY                                                  
         ZAP   MARKTOT,=P'0'                                                    
         MVC   CHQTOT,SPACES                                                    
         OI    CHQTOTH+6,X'80'                                                  
         XC    KEYLAST,KEYLAST                                                  
KEYV3    LA    RE,KEYTAB                                                        
         LA    RF,13*27                                                         
         XCEF                                                                   
         ZAP   LINES,=P'0'                                                      
         LA    R5,CHQFRSTH                                                      
         LA    R4,KEYTAB                                                        
         MVI   FLAG1,C'N'          ACTIVITY SWITCH                              
         GOTO1 AREAD,AIOAREA2                                                   
KEYV4    GOTO1 ASEQ,AIOAREA2                                                    
         L     R2,AIOAREA2                                                      
         CLC   COMPANY(15),0(R2)   SAME ACCOUNT                                 
         BNE   KEYV20                                                           
         USING ACKEYD,R2                                                        
         CLI   ACRECORD,X'44'                                                   
         BNE   KEYV4                                                            
         LA    R3,ACRECORD                                                      
KEYV6    CLI   0(R3),0             TEST FOR A X'64' ELEMENT                     
         BE    KEYV4                                                            
         CLI   0(R3),X'64'                                                      
         BE    KEYV8                                                            
         ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     KEYV6                                                            
*                                                                               
         USING TRMAPD,R3                                                        
KEYV8    CLI   ACTION,CHQ                                                       
         BNE   KEYV11                                                           
         OC    ACDTUSED,ACDTUSED   SKIP MARKED ITEMS FOR CHQ ACTION             
         BNZ   KEYV4                                                            
         B     KEYV12                                                           
KEYV11   OC    ACDTUSED,ACDTUSED   SKIP UNMARKED ITEMS FOR UNCHQ ACTION         
         BZ    KEYV4                                                            
         CLC   TRMAPNO,CHNO        AND TEST FOR NUMBER/DATE MATCH               
         BNE   KEYV4                                                            
         CLC   TRMAPDTE,CHDTE                                                   
         BNE   KEYV4                                                            
KEYV12   LA    R3,ACRECORD                                                      
         USING TRANSD,R3                                                        
         TM    TRNSSTAT,X'80'      CREDITS ONLY                                 
         BO    KEYV4                                                            
         CLI   INVREG,C'Y'         IF ON INVOICE REGISTER                       
         BNE   KEYV13                                                           
*&&UK                                                                           
         CLC   CHQPAY(2),=C'SF'    AND NOT MEDLINE                              
         BE    KEYV13                                                           
*&&                                                                             
         TM    TRNSSTAT,X'08'      SKIP UNAUTHED                                
         BZ    KEYV4                                                            
KEYV13   CLI   INVFILTH+5,0        ANY FILTERS                                  
         BE    KEYV14                                                           
         GOTO1 ATRNSFLT,AIOAREA2                                                
         BNE   KEYV4                                                            
KEYV14   BAS   RE,FORMAT                                                        
         MVI   FLAG1,C'Y'                                                       
         ZIC   RF,0(R5)                                                         
         AR    R5,RF                                                            
         MVI   8(R5),C' '          BLANK INPUT FIELD                            
         OI    6(R5),X'80'                                                      
         IC    RF,0(R5)                                                         
         AR    R5,RF               POINT TO NEXT OUTPUT LINE                    
         LA    R4,L'KEYTAB(R4)                                                  
         AP    LINES,=P'1'                                                      
         CP    LINES,SCRLINE                                                    
         BNE   KEYV4                                                            
         MVC   KEYLAST,ACKEYWRK                                                 
         MVC   MSG(29),=C'MARK THESE - MORE WILL FOLLOW'                        
         MVI   OVMODE,MARK                                                      
         LA    RE,CHQFMKH                                                       
         ST    RE,FADR             FOR CURSOR POSITION                          
         OI    CHQFMKH+1,X'01'                                                  
         B     EXIT                                                             
         SPACE 2                                                                
KEYV20   CLI   FLAG1,C'Y'          ACTIVITY SWITCH                              
         BNE   KEYV22                                                           
         MVI   OVMODE,MARK                                                      
         MVC   MSG,=CL60'THESE TRANSACTIONS AVAILABLE FOR MARKING'              
         XC    KEYLAST,KEYLAST                                                  
         LA    RE,CHQFMKH                                                       
         ST    RE,FADR                                                          
         B     KEYV24                                                           
KEYV22   MVI   OVMODE,INIT                                                      
         OC    KEYLAST,KEYLAST     IF NONE ON A CONTINUATION SCREEN             
         BNZ   UPD60               THEN SHOW TOTALS                             
         MVC   MSG(23),=C'NO TRANSACTIONS TO MARK'                              
         LA    RE,INVACTH                                                       
         ST    RE,FADR                                                          
KEYV24   CLI   0(R5),0                                                          
         BE    EXIT                                                             
         CLI   0(R5),9             TAB FIELD                                    
         BE    EXIT                                                             
         XC    8(L'CHQFRST,R5),8(R5)                                            
         OI    6(R5),X'80'                                                      
         ZIC   RF,0(R5)            CLEAR REST OF SCREEN                         
         AR    R5,RF                                                            
         MVI   8(R5),0                                                          
         OI    6(R5),X'80'                                                      
         IC    RF,0(R5)                                                         
         AR    R5,RF                                                            
         B     KEYV24                                                           
         EJECT                                                                  
FORMAT   NTR1                                                                   
         L     R2,AIOAREA2         ADDRESS OF RECORD                            
         LA    R6,8(R5)                                                         
         USING LINED,R6                                                         
         MVC   LNSUP,ACKEYCON+1                                                 
         MVC   LNINV,TRNSREF                                                    
         GOTO1 VDATCON,DMCB,(1,TRNSDATE),(8,LNDTE)                              
         EDIT  TRNSAMNT,(10,LNAMT),2,MINUS=YES                                  
         USING KEYTABD,R4                                                       
         MVC   KEYDTE,TRNSDATE                                                  
         MVC   KEYSUP,ACKEYCON                                                  
         MVC   KEYWC,SPACES                                                     
         MVC   KEYREF,ACKEYREF                                                  
         MVC   KEYSBRF,ACKEYSBR                                                 
*                                                                               
         OI    6(R5),X'80'         TRANSMIT                                     
         B     EXIT                                                             
         EJECT                                                                  
UPDATE   DS    0H                                                               
         LA    R5,CHQFMKH                                                       
         LA    R4,KEYTAB                                                        
UPD2     CLI   8(R5),C'Y'          IF Y IS INPUT MARK TRANSACTION               
         BE    UPD4                                                             
         CLI   8(R5),C' '          BLANK OR Y ONLY                              
         BE    UPD10                                                            
         CLI   8(R5),0                                                          
         BE    UPD10                                                            
         MVI   FERN,INVALID                                                     
         ST    R5,FADR                                                          
         B     EXIT                                                             
UPD4     DS    0H                                                               
         BAS   RE,RDMK                                                          
UPD10    LA    R4,L'KEYTAB(R4)                                                  
         ZIC   RF,0(R5)                                                         
         AR    R5,RF               SKIP TO NEXT INPUT FIELD                     
         CLI   0(R5),9                                                          
         BE    UPD50               TAB FIELD                                    
         OC    8(L'CHQFRST,R5),8(R5)                                            
         BE    UPD50                                                            
         IC    RF,0(R5)                                                         
         AR    R5,RF                                                            
         CLI   0(R5),0                                                          
         BE    UPD50                                                            
         B     UPD2                                                             
UPD50    MVI   OVMODE,INIT                                                      
         OC    KEYLAST,KEYLAST                                                  
         BZ    UPD60                                                            
         MVC   KEY(15),COMPANY     IF CONTINUING - START FROM SAVED KEY         
         MVC   KEY+15(L'KEYLAST),KEYLAST                                        
         MVI   OVMODE,INIT                                                      
         B     KEYV3                                                            
         SPACE 1                                                                
UPD60    MVC   CHQTOT,SPACES                                                    
         MVC   CHQTOT(18),=C'TOTAL VALUE MARKED'                                
         LA    RF,CHQTOT+19                                                     
         CLI   ACTION,CHQ                                                       
         BE    UPD62                                                            
         MVC   CHQTOT+12(8),=C'UNMARKED'                                        
         LA    RF,CHQTOT+21                                                     
UPD62    EDIT  MARKTOT,(11,0(RF)),2,MINUS=YES                                   
         OI    CHQTOTH+6,X'80'                                                  
         B     OKEND                                                            
         EJECT                                                                  
RDMK     NTR1                      ROUTINE TO FIND TRANSACTION                  
         USING KEYTABD,R4          AND SET DATA IN X'64' EL                     
         LA    R2,KEY                                                           
         USING ACKEYD,R2                                                        
         MVC   ACKEYACC,COMPANY    BUILD KEY FOR SUPPLIER TRANS.                
         MVC   ACKEYDTE,KEYDTE                                                  
         MVC   ACKEYREF,KEYREF                                                  
         MVC   ACKEYSBR,KEYSBRF                                                 
         MVC   ACKEYWRK,KEYWC                                                   
         MVC   ACKEYCON,KEYSUP                                                  
         GOTO1 AREADL,AIOAREA2                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
         USING ACKEYD,R2                                                        
         LA    R3,ACRECORD                                                      
         USING TRANSD,R3                                                        
         AP    MARKTOT,TRNSAMNT                                                 
RDMK2    CLI   0(R3),0             FIND 64 EL                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'64'                                                      
         BE    RDMK4                                                            
         ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     RDMK2                                                            
         USING TRMAPD,R3                                                        
RDMK4    CLI   ACTION,CHQ                                                       
         BE    RDMK6                                                            
         XC    ACDTUSED,ACDTUSED   UNCHQ - TURN OFF CHEQUE NO ETC               
         MVC   TRMAPNO,SPACES                                                   
         ZAP   TRMAPAMT,=P'0'                                                   
         MVC   TRMAPBNK,SPACES                                                  
         B     RDMK8                                                            
*                                                                               
RDMK6    DS    0H                  CHQ - SAVE CHEQUE NO ETC                     
         MVC   ACDTUSED,TODAY2                                                  
         MVC   TRMAPNO,CHNO                                                     
         ZAP   TRMAPAMT,CHAMT                                                   
         MVC   TRMAPDTE,CHDTE                                                   
         MVC   TRMAPBNK,CHBNK                                                   
*                                                                               
RDMK8    DS    0H                                                               
         GOTO1 AWRITE,AIOAREA2                                                  
         B     EXIT                                                             
         EJECT                                                                  
* EXIT BACK TO ROOT                                                             
*                                                                               
OKEND    DS    0H                                                               
         CLI   FERN,OK                                                          
         BNE   *+10                                                             
         MVC   MSG(29),=C'ACTION COMPLETED - ENTER NEXT'                        
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACINVDSECT                                                     
         ORG   INVTABH                                                          
       ++INCLUDE ACINVFBD                                                       
         ORG   TWAD+1800                                                        
PROGPROF DS    CL16                                                             
KEYTAB   DS    13CL32              SAVED KEY VALUES OF SCREEN DISPLAY           
KEYLAST  DS    CL27                                                             
CHNO     DS    CL6                                                              
CHDTE    DS    XL2                                                              
CHAMT    DS    PL6                                                              
CHBNK    DS    CL14                                                             
TODAY2   DS    XL2                                                              
DATE3    DS    XL3                                                              
MARKTOT  DS    PL6                                                              
         EJECT                                                                  
         SPACE 2                                                                
LOCALD   DSECT                                                                  
LINES    DS    PL2                                                              
         SPACE 2                                                                
LINED    DSECT                     FOR SCREEN DISPLAY LINE                      
LNSUP    DS    CL14                                                             
         DS    CL3                                                              
LNINV    DS    CL6                                                              
         DS    CL1                                                              
LNDTE    DS    CL9                                                              
         DS    CL1                                                              
LNAMT    DS    CL10                                                             
         SPACE 2                                                                
KEYTABD  DSECT                     FOR LINE IN KEYTAB                           
KEYWC    DS    CL2                                                              
KEYSUP   DS    CL15                                                             
KEYDTE   DS    CL3                                                              
KEYREF   DS    CL6                                                              
KEYSBRF  DS    CL1                                                              
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACINV03   05/01/02'                                      
         END                                                                    
