*          DATA SET ACINV02    AT LEVEL 007 AS OF 05/01/02                      
*PHASE T61302A,*                                                                
         TITLE 'ACINV02 - INVOICE MARKING - HOLD'                               
ACINV02  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**INV2**,RA                                                    
         USING INVWORKD,R9                                                      
         USING TWAD,R8                                                          
         L     R7,ASAVE                                                         
         USING LOCALD,R7                                                        
         CLI   OVMODE,INIT                                                      
         BE    KEYVAL                                                           
         MVC   WORK(2),PRODUL                                                   
         MVC   WORK+2(12),HLDJOB                                                
         OC    WORK,SPACES                                                      
         CLC   LKEY,WORK           START AGAIN IF KEY HAS CHANGED               
         BE    *+12                                                             
         MVI   OVMODE,INIT                                                      
         B     KEYVAL                                                           
         CLI   OVMODE,MARK                                                      
         BE    UPDATE                                                           
         B     OKEND                                                            
         EJECT                                                                  
KEYVAL   MVI   NOCVB,C'Y'                                                       
         GOTO1 AFVAL,HLDJOBH                                                    
         BZ    EXIT                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),PRODUL                                                  
         MVC   KEY+3(12),FLD                                                    
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   EXIT                                                             
         MVC   LKEY,KEY+1                                                       
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   HLDJOBN,WORK                                                     
         OI    HLDJOBNH+6,X'80'                                                 
*                                                                               
KEYV2    XC    KEYLAST,KEYLAST                                                  
         MVI   FLAG1,C'N'          ACTIVITY SWITCH                              
         LA    RE,KEYTAB                                                        
         LA    RF,13*27                                                         
         XCEF                                                                   
         ZAP   LINES,=P'0'                                                      
         LA    R5,HLDFRSTH                                                      
         LA    R4,KEYTAB                                                        
KEYV10   GOTO1 ASEQ,AIOAREA2                                                    
         L     R2,AIOAREA2                                                      
         CLC   COMPANY(15),0(R2)   SAME ACCOUNT                                 
         BNE   KEYV20                                                           
         USING ACKEYD,R2                                                        
         CLI   ACRECORD,X'44'                                                   
         BNE   KEYV10                                                           
         CLC   ACKEYWRK,=C'**'     SKIP ORDERS                                  
         BE    KEYV10                                                           
         OC    ACDTUSED,ACDTUSED   SKIP BILLED ITEMS                            
         BNZ   KEYV10                                                           
         LA    R3,ACRECORD                                                      
         USING TRANSD,R3                                                        
         TM    TRNSSTAT,X'80'      DEBITS ONLY                                  
         BZ    KEYV10                                                           
*        CLI   INVFILTH+5,0        ANY FILTERS                                  
*        BE    KEYV11                                                           
         GOTO1 ATRNSFLT,AIOAREA2                                                
         BNE   KEYV10                                                           
KEYV11   CLI   ACTION,HOLD                                                      
         BE    KEYV12                                                           
         TM    TRNSSTAT,X'04'      UNHOLD - WANT HOLD ONLY                      
         BZ    KEYV10                                                           
         B     KEYV14                                                           
KEYV12   TM    TRNSSTAT,X'04'      AND VICE VERSA                               
         BO    KEYV10                                                           
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
         BNE   KEYV10                                                           
         MVC   KEYLAST,ACKEYWRK                                                 
         MVC   MSG(29),=C'MARK THESE - MORE WILL FOLLOW'                        
         MVI   OVMODE,MARK                                                      
         LA    RE,HLDFMKH                                                       
         ST    RE,FADR             FOR CURSOR POSITION                          
         OI    HLDFMKH+1,X'01'                                                  
         B     EXIT                                                             
         SPACE 2                                                                
KEYV20   CLI   FLAG1,C'Y'          ACTIVITY SWITCH                              
         BNE   KEYV22                                                           
         MVI   OVMODE,MARK                                                      
         MVC   MSG,=CL60'THESE TRANSACTIONS AVAILABLE FOR MARKING'              
         LA    RE,HLDFMKH                                                       
         ST    RE,FADR                                                          
         B     KEYV24                                                           
KEYV22   MVI   OVMODE,INIT                                                      
         MVC   MSG(23),=C'NO TRANSACTIONS TO MARK'                              
         LA    RE,INVACTH                                                       
         ST    RE,FADR                                                          
KEYV24   CLI   0(R5),0                                                          
         BE    EXIT                                                             
         CLI   0(R5),9             TAB FIELD                                    
         BE    EXIT                                                             
         XC    8(L'HLDFRST,R5),8(R5)                                            
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
         MVC   LNWC,ACKEYWRK                                                    
         GOTO1 VDATCON,DMCB,(1,TRNSDATE),(8,LNDTE)                              
         EDIT  TRNSAMNT,(10,LNAMT),2,MINUS=YES                                  
         USING KEYTABD,R4                                                       
         MVC   KEYDTE,TRNSDATE                                                  
         MVC   KEYSUP,ACKEYCON+1                                                
         MVC   KEYWC,ACKEYWRK                                                   
         MVC   KEYREF,ACKEYREF                                                  
         MVC   KEYSBRF,ACKEYSBR                                                 
*                                                                               
         OI    6(R5),X'80'         TRANSMIT                                     
         B     EXIT                                                             
         EJECT                                                                  
UPDATE   DS    0H                                                               
         LA    R5,HLDFMKH                                                       
         LA    R4,KEYTAB                                                        
UPD2     CLI   8(R5),C'Y'          IF Y IS INPUT MARK 2 TRANSACTIONS            
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
         CLI   0(R5),0                                                          
         BE    UPD50               END OF SCREEN                                
         OC    8(L'HLDFRST,R5),8(R5)                                            
         BE    UPD50                                                            
         IC    RF,0(R5)                                                         
         AR    R5,RF                                                            
         CLI   0(R5),0                                                          
         BE    UPD50                                                            
         B     UPD2                                                             
UPD50    MVI   OVMODE,INIT                                                      
         OC    KEYLAST,KEYLAST                                                  
         BZ    OKEND                                                            
CONTINUE MVC   KEY(15),COMPANY     IF CONTINUING - START FROM SAVED KEY         
         MVC   KEY+15(L'KEYLAST),KEYLAST                                        
         MVI   OVMODE,INIT                                                      
         GOTO1 AREAD,AIOAREA2                                                   
         B     KEYV2                                                            
         EJECT                                                                  
RDMK     NTR1                      ROUTINE TO FIND TRANSACTIONS                 
         USING KEYTABD,R4          AND SET HOLD BIT ON OR OFF                   
         LA    R2,KEY                                                           
         USING ACKEYD,R2                                                        
         MVC   ACKEYACC,COMPANY    BUILD KEY FOR JOB TRANSACTION                
         MVC   ACKEYDTE,KEYDTE                                                  
         MVC   ACKEYREF,KEYREF                                                  
         MVC   ACKEYSBR,KEYSBRF                                                 
         MVC   ACKEYACC+1(2),PRODUL                                             
         MVC   ACKEYWRK,KEYWC                                                   
         MVC   ACKEYCON(1),COMPANY                                              
         MVC   ACKEYCON+1(14),KEYSUP                                            
         GOTO1 AREADL,AIOAREA2                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
         USING ACKEYD,R2                                                        
         LA    R3,ACRECORD                                                      
         USING TRANSD,R3                                                        
         CLI   ACTION,HOLD                                                      
         BE    RDMK6                                                            
         NI    TRNSSTAT,X'FB'      UNHOLD - TURN OFF X'04' BIT                  
         B     RDMK8                                                            
*                                                                               
RDMK6    OI    TRNSSTAT,X'04'      HOLD - TURN ON X'04' BIT                     
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
         SPACE 3                                                                
       ++INCLUDE ACINVDSECT                                                     
         ORG   INVTABH                                                          
       ++INCLUDE ACINVFDD                                                       
         ORG   TWAD+1800                                                        
PROGPROF DS    CL16                                                             
KEYTAB   DS    13CL32              SAVED KEY VALUES OF SCREEN DISPLAY           
KEYLAST  DS    CL27                                                             
         EJECT                                                                  
         SPACE 2                                                                
LOCALD   DSECT                                                                  
LINES    DS    PL2                                                              
         SPACE 2                                                                
LINED    DSECT                     FOR SCREEN DISPLAY LINE                      
LNWC     DS    CL2                                                              
         DS    CL2                                                              
LNSUP    DS    CL14                                                             
         DS    CL1                                                              
LNINV    DS    CL6                                                              
         DS    CL1                                                              
LNDTE    DS    CL9                                                              
         DS    CL1                                                              
LNAMT    DS    CL10                                                             
         SPACE 2                                                                
KEYTABD  DSECT                     FOR LINE IN KEYTAB                           
KEYWC    DS    CL2                                                              
KEYSUP   DS    CL14                                                             
KEYDTE   DS    CL3                                                              
KEYREF   DS    CL6                                                              
KEYSBRF  DS    CL1                                                              
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACINV02   05/01/02'                                      
         END                                                                    
