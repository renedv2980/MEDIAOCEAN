*          DATA SET ACINV01    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T61301A                                                                  
         TITLE 'ACINV01 - INVOICE MARKING - AUTHORISE'                          
ACINV01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**INV1**,RA                                                    
         USING INVWORKD,R9                                                      
         USING TWAD,R8                                                          
         L     R7,ASAVE                                                         
         USING LOCALD,R7                                                        
         CLI   OVMODE,INIT                                                      
         BE    KEYVAL                                                           
         MVC   WORK(14),AUTSUP                                                  
         OC    WORK,SPACES                                                      
         CLC   LKEY,WORK           START AGAIN IF KEY HAS CHANGED               
         BE    *+12                                                             
         MVI   OVMODE,INIT                                                      
         B     KEYVAL                                                           
         CLI   OVMODE,MARK                                                      
         BE    UPDATE                                                           
         B     OKEND                                                            
         EJECT                                                                  
KEYVAL   GOTO1 AFVAL,AUTSUPH                                                    
         BZ    EXIT                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),FLD                                                    
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   EXIT                                                             
         MVC   LKEY,KEY+1                                                       
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   AUTSUPN,WORK                                                     
         OI    AUTSUPNH+6,X'80'                                                 
*                                                                               
KEYV2    XC    KEYLAST,KEYLAST                                                  
         MVI   FLAG1,C'N'          ACTIVITY SWITCH                              
         LA    RE,KEYTAB                                                        
         LA    RF,13*27                                                         
         XCEF                                                                   
         ZAP   LINES,=P'0'                                                      
         LA    R5,AUTFRSTH                                                      
         LA    R4,KEYTAB                                                        
KEYV10   GOTO1 ASEQ,AIOAREA2                                                    
         L     R2,AIOAREA2                                                      
         CLC   COMPANY(15),0(R2)   SAME ACCOUNT                                 
         BNE   KEYV20                                                           
         USING ACKEYD,R2                                                        
         CLI   ACRECORD,X'44'                                                   
         BNE   KEYV10                                                           
         CLC   ACKEYCON+1(2),=C'SE'  WANT JOB AND EXPENSE CONTRAS               
         BE    *+14                                                             
         CLC   ACKEYCON+1(2),PRODUL                                             
         BNE   KEYV10                                                           
         LA    R3,ACRECORD                                                      
         USING TRANSD,R3                                                        
         TM    TRNSSTAT,X'80'      CREDITS ONLY                                 
         BO    KEYV10                                                           
         CLI   INVFILTH+5,0        ANY FILTERS                                  
         BE    KEYV11                                                           
         GOTO1 ATRNSFLT,AIOAREA2                                                
         BNE   KEYV10                                                           
KEYV11   CLI   ACTION,AUTH                                                      
         BE    KEYV12                                                           
         TM    TRNSSTAT,X'08'      UNAUTH - WANT ONLY AUTH'D                    
         BZ    KEYV10                                                           
         B     KEYV14                                                           
KEYV12   TM    TRNSSTAT,X'08'      AND VICE VERSA                               
         BO    KEYV10                                                           
KEYV14   MVI   FLAG,C'N'           SWITCH FOR 4F ELEMENT                        
         BAS   RE,FORMAT                                                        
         CLI   FLAG,C'N'                                                        
         BNE   *+14                IF NO 4F EL CLEAR SCREEN -GET NEXT           
         MVC   8(L'AUTFRST,R5),SPACES                                           
         B     KEYV10                                                           
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
         LA    RE,AUTFMKH                                                       
         ST    RE,FADR             FOR CURSOR POSITION                          
         OI    AUTFMKH+1,X'01'                                                  
         B     EXIT                                                             
         SPACE 2                                                                
KEYV20   CLI   FLAG1,C'Y'          ACTIVITY SWITCH                              
         BNE   KEYV22                                                           
         MVI   OVMODE,MARK                                                      
         MVC   MSG,=CL60'THESE TRANSACTIONS AVAILABLE FOR MARKING'              
         LA    RE,AUTFMKH                                                       
         ST    RE,FADR                                                          
         B     KEYV24                                                           
KEYV22   MVI   OVMODE,INIT                                                      
         MVC   MSG(23),=C'NO TRANSACTIONS TO MARK'                              
         LA    RE,AUTSUPH                                                       
         ST    RE,FADR                                                          
KEYV24   CLI   0(R5),0                                                          
         BE    EXIT                                                             
         CLI   0(R5),9             TAB FIELD                                    
         BE    EXIT                                                             
         XC    8(L'AUTFRST,R5),8(R5)                                            
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
         MVC   LNJOB,ACKEYCON+3    DEFAULT                                      
         MVC   LNINV,TRNSREF                                                    
         GOTO1 VDATCON,DMCB,(1,TRNSDATE),(8,LNDTE)                              
         EDIT  TRNSAMNT,(10,LNAMT),2,MINUS=YES                                  
         USING KEYTABD,R4                                                       
         MVC   KEYDTE,TRNSDATE                                                  
         MVC   KEYREF,TRNSREF                                                   
         MVC   KEYSBRF,TRNSSBRF                                                 
         MVC   LNSUB,SPACES                                                     
FMT2     ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         CLI   0(R3),0                                                          
         BE    EXIT                                                             
         CLI   0(R3),X'23'                                                      
         BNE   FMT4                                                             
         USING ACOTHERD,R3                                                      
         MVC   LNSUB,ACOTNUM                                                    
         B     FMT2                                                             
FMT4     CLI   0(R3),X'4F'                                                      
         BNE   FMT2                                                             
         USING TRCPJD,R3           CLI/PROD/JOB EL                              
         CLI   TRCPTYPE,C'E'       EXPENSE ACCT                                 
         BNE   FMT6                                                             
         MVC   KEYWC,SPACES                                                     
         MVC   LNWC,SPACES                                                      
         MVC   WORK(12),TRCPACCT   EXPENSE ACCOUNT                              
         B     FMT10                                                            
*                                                                               
FMT6     CLI   TRCPWC,C'A'                                                      
         BL    EXIT                                                             
         CLI   TRCPWC+1,C'A'                                                    
         BL    EXIT                                                             
         MVC   KEYWC,TRCPWC                                                     
         MVC   LNWC,TRCPWC                                                      
         MVC   WORK,SPACES                                                      
         MVC   WORK(6),TRCPCLI                                                  
         ZIC   RE,PRODHEIR                                                      
         LA    RE,WORK(RE)         BUILD UP CLI/PROD/JOB IN WORK                
         MVC   0(6,RE),TRCPPROD                                                 
         ZIC   RE,PRODHEIR+1                                                    
         LA    RE,WORK(RE)                                                      
         MVC   0(6,RE),TRCPJOB                                                  
FMT10    MVC   LNJOB,WORK                                                       
         MVC   KEYJOB,WORK                                                      
         MVI   FLAG,C'Y'                                                        
*                                                                               
         OI    6(R5),X'80'         TRANSMIT                                     
         B     EXIT                                                             
         EJECT                                                                  
UPDATE   DS    0H                                                               
         LA    R5,AUTFMKH                                                       
         LA    R4,KEYTAB                                                        
UPD2     CLI   8(R5),C'Y'          IF Y IS INPUT MARK 2 TRANSACTIONS            
         BE    UPD4                                                             
         CLI   8(R5),C' '                                                       
         BE    UPD10                                                            
         CLI   8(R5),0                                                          
         BE    UPD10                                                            
         MVI   FERN,INVALID        BLANK OR Y ONLY                              
         ST    R5,FADR                                                          
         B     EXIT                                                             
UPD4     DS    0H                                                               
         BAS   RE,RDMK                                                          
UPD10    LA    R4,L'KEYTAB(R4)                                                  
         ZIC   RF,0(R5)                                                         
         AR    R5,RF               SKIP TO NEXT INPUT FIELD                     
         CLI   0(R5),0                                                          
         BE    UPD50               END OF SCREEN                                
         OC    8(L'AUTFRST,R5),8(R5)                                            
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
RDMK     NTR1                      ROUTINE TO FIND 2 TRANSACTIONS               
         USING KEYTABD,R4          AND SET AUTHORISED BIT ON OR OFF             
         LA    R2,KEY                                                           
         USING ACKEYD,R2                                                        
         MVC   ACKEYACC,COMPANY    BUILD KEY FOR SUPPLIER TRANSACTION           
         MVC   ACKEYWRK,SPACES                                                  
         MVC   ACKEYCON(1),COMPANY                                              
         MVC   ACKEYCON+1(2),PRODUL                                             
         MVC   ACKEYCON+3(12),=CL12'999'                                        
         MVC   ACKEYDTE,KEYDTE                                                  
         MVC   ACKEYREF,KEYREF                                                  
         MVC   ACKEYSBR,KEYSBRF                                                 
         GOTO1 AREADL,AIOAREA2                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
         USING ACKEYD,R2                                                        
         LA    R3,ACRECORD                                                      
         USING TRANSD,R3                                                        
         MVC   THISBTCH,TRNSBTCH                                                
         CLI   ACTION,AUTH                                                      
         BE    RDMK2                                                            
         NI    TRNSSTAT,X'F7'      UNAUTH - TURN OFF X'08'BIT                   
         B     RDMK4                                                            
*                                                                               
RDMK2    OI    TRNSSTAT,X'08'                                                   
RDMK4    DS    0H                                                               
         GOTO1 AWRITE,AIOAREA2                                                  
         LA    R2,KEY                                                           
         MVC   ACKEYACC+1(2),PRODUL                                             
         MVC   ACKEYACC+3(12),KEYJOB                                            
         MVC   ACKEYWRK,KEYWC                                                   
         CLC   KEYWC,SPACES                                                     
         BNE   *+10                                                             
         MVC   ACKEYACC+1(2),=C'SE'                                             
         MVC   ACKEYCON,COMPANY                                                 
         MVC   MKYSAVE,KEY                                                      
         GOTO1 AREADL,AIOAREA2                                                  
         BNE   RDMK4A              NOT FOUND                                    
         L     R2,AIOAREA2         IF FOUND TEST RIGHT BATCH                    
         LA    R3,ACRECORD                                                      
         CLC   THISBTCH,TRNSBTCH                                                
         BE    RDMK5               ASSUME WE HAVE THE RIGHT POSTING             
RDMK4A   LA    R5,10                                                            
         LA    R2,KEY                                                           
         MVC   KEY,MKYSAVE         OTHERWISE SET SUBREF BACK TO ZERO            
         MVI   ACKEYSBR,0          AND TRY 10 TIMES                             
         MVI   FLAG,0                                                           
*                                                                               
RDMK4B   GOTO1 ARDHIL,AIOAREA2                                                  
         L     R2,AIOAREA2                                                      
         CLC   KEYSAVE,0(R2)                                                    
         BE    RDMK4E                                                           
RDMK4C   MVC   KEY,MKYSAVE                                                      
         ZIC   RF,FLAG             IF NO GOOD BUMP SUBREF BY 2                  
         LA    RF,2(RF)            AND TRY AGAIN                                
         STC   RF,FLAG                                                          
         LA    R2,KEY                                                           
         STC   RF,ACKEYSBR                                                      
         BCT   R5,RDMK4B                                                        
         B     EXIT                                                             
*                                                                               
RDMK4E   L     R2,AIOAREA2                                                      
         LA    R3,ACRECORD                                                      
         USING TRANSD,R3                                                        
         CLC   THISBTCH,TRNSBTCH   TEST THIS RECORD FOR BATCH REF               
         BNE   RDMK4C              MATCH                                        
         CLI   ACTION,AUTH         CHECK RECORD WITH ACTION                     
         BE    RDMK4G                                                           
         TM    TRNSSTAT,X'08'      UNAUTH AND BIT OFF - NO GOOD                 
         BZ    RDMK4C              GET NEXT                                     
         B     RDMK5                                                            
RDMK4G   TM    TRNSSTAT,X'08'      AUTH AND BIT ON - NO GOOD                    
         BO    RDMK4C              GET NEXT                                     
         B     RDMK5                                                            
*                                                                               
RDMK5    L     R2,AIOAREA2                                                      
         USING ACKEYD,R2                                                        
         LA    R3,ACRECORD                                                      
         USING TRANSD,R3                                                        
         CLI   ACTION,AUTH                                                      
         BE    RDMK6                                                            
         NI    TRNSSTAT,X'F7'      UNAUTH - TURN OFF X'08'BIT                   
         B     RDMK8                                                            
*                                                                               
RDMK6    OI    TRNSSTAT,X'08'      AUTH - TURN ON X'08' BIT                     
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
       ++INCLUDE ACINVFED                                                       
         ORG   TWAD+1800                                                        
PROGPROF DS    CL16                                                             
KEYTAB   DS    13CL32              SAVED KEY VALUES OF SCREEN DISPLAY           
KEYLAST  DS    CL27                                                             
         EJECT                                                                  
         SPACE 2                                                                
LOCALD   DSECT                                                                  
LINES    DS    PL2                                                              
MKYSAVE  DS    CL42                                                             
THISBTCH DS    CL6                                                              
         SPACE 2                                                                
LINED    DSECT                     FOR SCREEN DISPLAY LINE                      
LNJOB    DS    CL12                                                             
         DS    CL1                                                              
LNWC     DS    CL2                                                              
         DS    CL1                                                              
LNINV    DS    CL6                                                              
         DS    CL1                                                              
LNDTE    DS    CL9                                                              
         DS    CL4                                                              
LNSUB    DS    CL6                                                              
         DS    CL4                                                              
LNAMT    DS    CL10                                                             
         SPACE 2                                                                
KEYTABD  DSECT                     FOR LINE IN KEYTAB                           
KEYWC    DS    CL2                                                              
KEYJOB   DS    CL12                                                             
KEYDTE   DS    CL3                                                              
KEYREF   DS    CL6                                                              
KEYSBRF  DS    CL1                                                              
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACINV01   05/01/02'                                      
         END                                                                    
