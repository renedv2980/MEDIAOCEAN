*          DATA SET SPREPFX02  AT LEVEL 028 AS OF 09/28/99                      
*PHASE SPFX02M                                                                  
         TITLE 'SPFX02 - DELETE BAD PIGGYBACK GOALS'                            
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
* REQFRST                                                                       
*                                                                               
FX10     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),=X'02C8A8C5'     GOALS/OU/C/KGF                           
         GOTO1 HIGH                                                             
         B     FX14                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX14     CLC   KEY(4),KEYSAVE                                                   
         BNE   FXX                                                              
         CLI   KEY+12,0            TEST PIGGYBACK                               
         BE    FX12                NO                                           
         CLC   KEY+9(1),KEY+10     TEST PRDLEN=TOTLEN                           
         BE    FX15                                                             
         CLI   KEY+9,0             PRD SLN 0 IS TERRIBLE                        
         BNE   FX12                                                             
* DELETE GOAL RECORD                                                            
FX15     MVC   KEYSAVE,KEY         SAVE CURRENT KEY                             
*                                                                               
         OI    KEY+13,X'80'                                                     
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX16                                                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',KEY,KEY                        
*                                                                               
FX16     MVC   P(7),=C'DELETED'                                                 
         GOTO1 HEXOUT,DMCB,KEY,P+8,18,=C'TOG'                                   
         GOTO1 REPORT                                                           
         B     FX12                                                             
*                                                                               
         MVC   AREC,ADGOAL                                                      
         GOTO1 GET                                                              
*                                                                               
         L     R6,AREC                                                          
         MVI   9(R6),30            SET SLN                                      
         MVI   10(R6),30           SET TLN                                      
         GOTO1 ADD                                                              
*                                                                               
         MVC   P+46(5),=C'ADDED'                                                
         GOTO1 HEXOUT,DMCB,(R6),P+52,18,=C'TOG'                                 
         GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE PREVIOUS KEY                         
         GOTO1 HIGH                RESTORE DIR FOR SEQ                          
         B     FX12                                                             
*                                                                               
FX90     MVI   FORCEHED,C'Y'                                                    
         MVC   P(17),=C'RECORDS CHANGED ='                                      
         EDIT  (P8,RECTOT),(7,P+18)                                             
         GOTO1 REPORT                                                           
*                                                                               
FXX      GOTO1 AENDREQ                                                          
         B     EXIT                                                             
         EJECT                                                                  
FIX      LR    R0,RE                                                            
         GOTO1 HIGH                                                             
         B     FIX4                                                             
*                                                                               
FIX2     GOTO1 SEQ                                                              
*                                                                               
FIX4     CLC   KEY(5),KEYSAVE                                                   
         BNE   FIXX                                                             
         CLC   KEY+5(1),4(R2)                                                   
         BH    FIXX                                                             
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
         AP    RECTOT,=P'1'                                                     
         GOTO1 PRNTBL,DMCB,0,KEY,C'DUMP',18,=C'1D'                              
         B     FIX2                                                             
*                                                                               
FIXX     LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* STORAGE                                                                       
*                                                                               
         DS    0D                                                               
RECTOT   DS    PL8                                                              
*                                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028SPREPFX02 09/28/99'                                      
         END                                                                    
