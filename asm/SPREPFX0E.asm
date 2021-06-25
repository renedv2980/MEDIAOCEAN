*          DATA SET SPREPFX0E  AT LEVEL 005 AS OF 05/12/98                      
*          DATA SET SPREPFXC   AT LEVEL 015 AS OF 09/21/93                      
*PHASE SPFX02M                                                                  
         TITLE 'SPFX02 - COUNT 0D7A RECORDS'                                    
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02                                                         
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX2                                                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
FX2      ZAP   RECCNT,=P'0'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D7A'                                                  
         GOTO1 HIGH                                                             
         B     FX6                                                              
*                                                                               
FX4      GOTO1 SEQ                                                              
*                                                                               
FX6      CLC   KEY(2),KEYSAVE                                                   
         BNE   FX7                                                              
         TM    KEY+9,X'F0'         TEST CABLE                                   
         BNO   FX4                                                              
         CLC   =X'FFFFFF',KEY+9    SKIP GRANT SPECIALS                          
         BE    FX4                                                              
         AP    RECCNT,=P'1'                                                     
         BAS   RE,PRTIT                                                         
         B     FX4                                                              
*                                                                               
FX7      MVI   MODE,REQLAST                                                     
         MVC   P(14),=C'NUMBER OF RECS'                                         
         EDIT  RECCNT,(12,P+17)                                                 
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
PRTIT    LR    R0,RE                                                            
         GOTO1 HEXOUT,DMCB,KEY,P,13,=C'TOG'                                     
         GOTO1 REPORT                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
RECCNT   DS    PL6                                                              
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPREPFX0E 05/12/98'                                      
         END                                                                    
