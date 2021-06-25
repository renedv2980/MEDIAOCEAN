*          DATA SET SPREPFXX5  AT LEVEL 003 AS OF 02/13/95                      
*          DATA SET SPREPFXX3  AT LEVEL 038 AS OF 02/13/95                      
*PHASE SPFX02B                                                                  
         TITLE 'SPFX02 - BAD CANADIAN BUYS'                                     
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX       XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(3),1(R6)                                                   
         GOTO1 HIGH                                                             
         B     FX4                                                              
*                                                                               
FX2      GOTO1 SEQ                                                              
*                                                                               
FX4      CLC   KEY(5),KEYSAVE                                                   
         BNE   FX10                                                             
*                                                                               
         CLC   KEY+9(2),=X'0340'                                                
         BNE   FX2                                                              
         GOTO1 HEXOUT,DMCB,KEY,P+10,13,=C'TOG'                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         MVC   KEY+9(2),=X'0285'                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   *+6                                                              
         DC    H'0'                DUPLICATE KEY !!!                            
         GOTO1 ADD                                                              
*                                                                               
         MVC   P(8),=C'ADD BILL'                                                
         GOTO1 HEXOUT,DMCB,(R6),P+10,13,=C'TOG'                                 
         GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'        DELETE BILL                                  
         GOTO1 WRITE                                                            
*                                                                               
         MVC   P(8),=C'DEL BILL'                                                
         GOTO1 HEXOUT,DMCB,KEY,P+10,13,=C'TOG'                                  
         GOTO1 REPORT                                                           
         B     FX2                                                              
*                                                                               
FX10     XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(3),1(R6)        MOVE A-M/CLT                                 
         MVI   KEY+3,X'FF'         SET POL                                      
         GOTO1 HIGH                                                             
         B     FX22                                                             
*                                                                               
FX20     GOTO1 SEQ                                                              
*                                                                               
FX22     CLC   KEY(4),KEYSAVE      TEST A-M/CLT/PRD                             
         BE    FX25                                                             
         MVI   MODE,CLTLAST                                                     
         B     EXIT                                                             
*                                                                               
FX25     CLC   KEY+6(2),=X'0340'   TEST BCI                                     
         BNE   FX20                                                             
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+10,13,=C'TOG'                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   MYKEY,KEY           SAVE CURRENT KEY                             
         MVC   KEY+6(2),=X'0285'   SET CORRECT STATION                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FX30                                                             
         GOTO1 HEXOUT,DMCB,KEY,P+15,13,=C'TOG'                                  
         MVC   P(11),=C'*** DUP KEY'                                            
         GOTO1 REPORT                                                           
         B     FX20                                                             
*                                                                               
FX30     DS    0H                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         OI    15(R6),X'80'                                                     
         GOTO1 PUT                                                              
         MVC   P(7),=C'PUT BUY'                                                 
         GOTO1 HEXOUT,DMCB,(R6),P+10,24,=C'TOG'                                 
         GOTO1 REPORT                                                           
*                                                                               
         NI    15(R6),X'7F'        RESET DELETED BIT                            
         MVC   6(2,R6),=X'0285'    FIX THE KEY                                  
         ST    R6,AREC                                                          
         GOTO1 ADD                 ADD NEW RECORD                               
*                                                                               
         MVC   P(7),=C'ADD BUY'                                                 
         GOTO1 HEXOUT,DMCB,(R6),P+10,24,=C'TOG'                                 
         GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'        DELETE THE OLD BUY                           
         GOTO1 WRITE                                                            
*                                                                               
         MVC   P(7),=C'WRT DIR'                                                 
         GOTO1 HEXOUT,DMCB,KEY,P+10,24,=C'TOG'                                  
         GOTO1 REPORT                                                           
         B     FX20                                                             
*                                                                               
MYKEY    DS    XL24                                                             
*                                                                               
*                                                                               
*                                                                               
FXEND    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
ELCODE   DS    X                                                                
                                                                                
*                                                                               
AGENCYD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPFXX5 02/13/95'                                      
         END                                                                    
