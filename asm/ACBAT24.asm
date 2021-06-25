*          DATA SET ACBAT24    AT LEVEL 019 AS OF 05/01/02                      
*PHASE T61B24A                                                                  
         TITLE 'ADVANCE PAYMENT'                                                
T61B24   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**BAT24                                             
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         EJECT                                                                  
*              READ & SAVE ACCOUNTS                                             
         SPACE 2                                                                
         MVC   DOC,SPACES                                                       
         ZAP   CRAMT,=P'0'                                                      
         LA    R2,ADVDOCH                                                       
         BAS   RE,ANY                                                           
         ZIC   RE,ADVDOCH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DOC(0),ADVDOC                                                    
*                                                                               
         LA    R2,ADVDATH                                                       
         MVI   ERRNUM,13                                                        
         CLI   5(R2),0                                                          
         BNE   AP2                                                              
         BAS   RE,GETODAY                                                       
         B     AP4                                                              
AP2      GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
AP4      GOTO1 DATCON,DMCB,(0,WORK),(1,DATE)                                    
         GOTO1 DATECHK,DMCB,DATE                                                
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVI   ERRNUM,25                                                        
         LA    R2,ADVCHAMH                                                      
         BAS   RE,ANY                                                           
         ZIC   R3,5(R2)                                                         
         GOTO1 AMTVAL,DMCB,8(R2),(R3)                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     RF,DMCB+4                                                        
         LA    RF,0(RF)                                                         
         ZAP   CHAMT,0(8,RF)                                                    
         ZAP   TRANSAMT,0(8,RF)                                                 
         LA    R2,ADVCRAMH                                                      
         CLI   5(R2),0                                                          
         BE    AP10                                                             
         ZIC   R3,5(R2)                                                         
         GOTO1 AMTVAL,DMCB,8(R2),(R3)                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     RF,DMCB+4                                                        
         LA    RF,0(RF)                                                         
         ZAP   CRAMT,0(8,RF)                                                    
         B     AP10                                                             
         EJECT                                                                  
AP10     LA    R2,ADVBNKH          VALIDATE BANK ACCOUNT                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         LA    RF,COMPEL                                                        
         USING ACCOMPD,RF                                                       
         MVC   KEY+1(2),ACMPBANK                                                
         SR    R6,R6               NO PROFILES                                  
         MVI   ERRNUM,X'FF'                                                     
         BAS   RE,ACCVAL                                                        
         BNE   ERROR                                                            
         MVC   BANKNUM,ACCTNUM                                                  
         MVC   BANKNAME,ACCTNAME                                                
         TM    ADVBNKH+4,X'20'                                                  
         BO    AP20                                                             
         OI    ADVBNKH+4,X'20'                                                  
         MVC   ADVBNKN,ACCTNAME                                                 
         OI    ADVBNKNH+6,X'80'                                                 
         SPACE 2                                                                
AP20     LA    R2,ADVPAYH          VALIDATE PAYEE ACCOUNT                       
         MVC   KEY+1(14),SPACES                                                 
         BAS   RE,ACCVAL                                                        
         BNE   ERROR                                                            
         MVI   ERRNUM,9            INVALID LEDGER                               
         LA    RF,ULTAB                                                         
AP22     CLI   0(RF),X'FF'                                                      
         BE    ERROR                                                            
         CLC   ADVPAY(2),0(RF)                                                  
         BE    AP24                                                             
         LA    RF,L'ULTAB(RF)                                                   
         B     AP22                                                             
AP24     MVC   PAYEE,ACCTNUM                                                    
         MVC   PAYEENM,ACCTNAME                                                 
         TM    ADVPAYH,X'20'                                                    
         BO    AP30                                                             
         MVC   ADVPAYN,ACCTNAME                                                 
         OI    ADVPAYH+4,X'20'                                                  
         OI    ADVPAYNH+6,X'80'                                                 
         SPACE 2                                                                
AP30     LA    R2,ADVCRH                                                        
         CP    CRAMT,=P'0'         CREDIT ACCOUNT (OPTIONAL)                    
         BNE   AP32                                                             
         MVI   ERRNUM,2                                                         
         CLI   ADVCRH+5,0          IF NO AMOUNT - CANT HAVE ACCOUNT             
         BNE   ERROR                                                            
         B     AP50                                                             
*                                                                               
AP32     BAS   RE,ANY              IF AMOUNT - MUST HAVE ACCOUNT                
         MVI   ERRNUM,X'FF'                                                     
         MVC   KEY+1(14),SPACES                                                 
         BAS   RE,ACCVAL                                                        
         BNE   ERROR                                                            
         MVI   ERRNUM,18                                                        
         CLI   ACCTNUM+1,C'S'                                                   
         BE    AP34                                                             
         CLI   ACCTNUM+1,C'G'                                                   
         BNE   ERROR                                                            
AP34     MVC   CREDNUM,ACCTNUM                                                  
         MVC   CREDNAME,ACCTNAME                                                
         TM    ADVCRH+4,X'20'                                                   
         BO    AP50                                                             
         OI    ADVCRH+4,X'20'                                                   
         MVC   ADVCRN,ACCTNAME                                                  
         OI    ADVCRNH+6,X'80'                                                  
         B     AP50                                                             
         SPACE 2                                                                
AP50     LA    R2,ADVOFFH                                                       
         MVC   OFFICE,SPACES                                                    
*&&UK                                                                           
         TM    COMPSTAT,X'20'      OFFICE AGENCY                                
         BZ    AP50A                                                            
         BAS   RE,ANY              TEST FOR REQD INPUT                          
AP50A    CLI   TWAACCS,C'*'        AND IF OFFICE LOGON                          
         BNE   AP50C                                                            
         CLC   ADVOFF,TWAACCS+1    THAT THE INPUT IS CORRECT                    
         BE    AP50C                                                            
         MVI   ERRNUM,SECLOCK                                                   
         B     ERROR                                                            
*&&                                                                             
AP50C    CLI   5(R2),0                                                          
         BE    AP51                                                             
         OC    ADVOFF,SPACES                                                    
         GOTO1 AVALOFFC,DMCB,ADVOFF                                             
         CLI   ERRNUM,OK                                                        
         BNE   ERROR                                                            
         MVC   OFFICE,ADVOFF                                                    
         B     AP51                                                             
         EJECT                                                                  
AP51     LA    R7,IOAREA+2         GENERATE POSTINGS                            
         USING DLDESCD,R7                                                       
         MVI   DLDSEL,X'64'        DESCRIPTION ELEMENT                          
         MVC   DLDSREF,DOC                                                      
         MVC   DLDSDATE,DATE                                                    
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
*&&UK*&& OI    DLDSSTAT,X'08'      AUTHORISE                                    
         XC    DLDSNARR,DLDSNARR                                                
         LA    R3,DLDSNARR                                                      
         LA    R2,ADVNARH                                                       
         BAS   RE,NARRSCAN                                                      
         LA    R3,DLDSNARR                                                      
         SR    R3,R7                                                            
         LA    R3,0(R6,R3)                                                      
         STH   R3,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         EJECT                                                                  
         SPACE 2                                                                
         AR    R7,R3               POSTING ELEMENTS                             
         USING DLPOSTD,R7                                                       
         MVC   DLPSEL(2),=X'6A71'  ALL CREDITS                                  
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,CHAMT                                                   
         SP    DLPSAMNT,CRAMT                                                   
         MVC   DLPSANAL,OFFICE                                                  
         MVC   DLPSCRAC,BANKNUM    PLUS CREDIT BANK                             
         MVC   DLPSCRNM,BANKNAME                                                
         MVC   DLPSDBAC,PAYEE                                                   
         MVC   DLPSDBNM,PAYEENM                                                 
         SPACE 1                                                                
         LR    R6,R7                                                            
         ZIC   RE,DLPSLEN                                                       
         AR    R7,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R6)                                                    
         MVC   DLPSCRAC,DLPSDBAC                                                
         MVC   DLPSCRNM,DLPSDBNM                                                
         MVC   DLPSDBAC,BANKNUM                                                 
         MVC   DLPSDBNM,BANKNAME                                                
         ZAP   DLPSAMNT,CHAMT                                                   
         MP    DLPSAMNT,=P'-1'     MINUS CREDIT PAYEE                           
         SPACE 1                                                                
         CP    CRAMT,=P'0'                                                      
         BE    AP60                                                             
         LR    R6,R7                                                            
         ZIC   RE,DLPSLEN                                                       
         AR    R7,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R6)                                                    
         MVC   DLPSCRAC,CREDNUM                                                 
         MVC   DLPSCRNM,CREDNAME                                                
         MVC   DLPSDBAC,PAYEE                                                   
         MVC   DLPSDBNM,PAYEENM                                                 
         ZAP   DLPSAMNT,CRAMT      PLUS CREDIT CREDIT ACC(OPTIONAL)             
         CLC   DLPSCRAC+1(2),=C'SE'                                             
         BNE   AP60                                                             
         MP    DLPSAMNT,=P'-1'     IF POSTING IS TO SE                          
         MVI   DLPSEL,X'69'        MAKE INTO A MINUS DEBIT                      
         MVC   DLPSDBAC,CREDNUM                                                 
         MVC   DLPSDBNM,CREDNAME                                                
         MVC   DLPSCRAC,PAYEE                                                   
         MVC   DLPSCRNM,PAYEENM                                                 
         SPACE 1                                                                
AP60     ZIC   RE,DLPSLEN                                                       
         AR    R7,RE                                                            
         MVI   0(R7),0             END OF RECORD                                
         LA    R3,IOAREA-1                                                      
         SR    R7,R3                                                            
         STH   R7,HALF                                                          
         MVC   IOAREA(2),HALF      FINAL LENGTH                                 
         BAS   RE,PUTDAY                                                        
         SPACE 3                                                                
         XC    WORK(20),WORK                                                    
         MVC   WORK(6),DOC         REFERENCE                                    
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         BAS   RE,ADTWA1                                                        
         SPACE 1                                                                
         LA    R2,ADVDOCH                                                       
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO VALIDATE AN ACCOUNT                                                
*                                                                               
ACCVAL   STM   R0,RF,SAVEREG2                                                   
         LA    RF,KEY+1                                                         
         CLI   KEY+1,C' '                                                       
         BE    *+8                                                              
         LA    RF,KEY+3            WE NEED U/L FORM SCREEN                      
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'      BALANCE ELEMNT                               
         BZ    ACCVXT                                                           
         TM    ACCTSTAT,X'30'      LOCKED OR CLOSED                             
         BNZ   ACCVXT                                                           
         MVI   ERRNUM,X'FF'                                                     
ACCVXT   LM    R0,RF,SAVEREG2                                                   
         CLI   ERRNUM,X'FF'        RETURN WITH CC=NE IF PROBLEM                 
         BR    RE                                                               
         SPACE 2                                                                
ULTAB    DS    0CL2                                                             
*&&UK                                                                           
         DC    C'SFSVSXST'                                                      
*&&                                                                             
*&&US                                                                           
         DC    C'SSSTSPSQSVSWSX'                                                
*&&                                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
       ++INCLUDE ACBATDSECT                                                     
       ++INCLUDE ACBATD7D                                                       
         EJECT                                                                  
PROGD    DSECT                                                                  
SAVEREG2 DS    16F                                                              
CHAMT    DS    PL6                                                              
CRAMT    DS    PL6                                                              
DOC      DS    CL6                                                              
DATE     DS    PL3                                                              
BANKNUM  DS    CL15                                                             
BANKNAME DS    CL36                                                             
PAYEE    DS    CL15                                                             
PAYEENM  DS    CL36                                                             
CREDNUM  DS    CL15                                                             
CREDNAME DS    CL36                                                             
OFFICE   DS    CL2                                                              
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACGENDAY                                                               
*        DDFLDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACBAT24   05/01/02'                                      
         END                                                                    
