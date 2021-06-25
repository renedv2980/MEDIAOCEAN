*          DATA SET SPREPFXBW  AT LEVEL 010 AS OF 08/29/00                      
*PHASE SPFX02A                                                                  
         TITLE 'SPFX02 - CONVERT SJR''S WORKSHEET WORK RECORDS'                 
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
         MVI   FORCEHED,C'Y'                                                    
         ZAP   RECTOT,=P'0'                                                     
         ZAP   RECCHA,=P'0'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(3),=X'0D68C1'                                                
         GOTO1 HIGH                                                             
         B     FX14                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX14     CLC   KEY(3),KEYSAVE                                                   
         BNE   FX30                                                             
         AP    RECTOT,=P'1'                                                     
         L     R2,ADBUY                                                         
         ST    R2,AREC                                                          
         GOTO1 GET                                                              
         USING BWDRECD,R2                                                       
         MVI   PRTSW,C'N'                                                       
         CP    RECTOT,=P'20'                                                    
         BH    FX15                                                             
         MVI   PRTSW,C'Y'                                                       
         SR    R5,R5                                                            
         ICM   R5,3,13(R2)                                                      
         GOTO1 PRNTBL,DMCB,(20,RECBEF),(R2),C'DUMP',(R5),=C'2D'                 
         GOTO1 REPORT                                                           
*                                                                               
FX15     LA    R4,BWDEL                                                         
         SR    R0,R0                                                            
         SR    R5,R5                                                            
*                                                                               
FX16     CLI   0(R4),0                                                          
         BE    FX20                                                             
         CLI   0(R4),1                                                          
         BNE   FX18                                                             
         USING BWDEL,R4                                                         
         MVC   ELEM(77),BWDSTA+5                                                
         MVC   BWDSTA+5(3),=X'404040'                                           
         MVC   BWDSTA+8(77),ELEM                                                
         LA    R5,1                                                             
*                                                                               
FX18     IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     FX16                                                             
*                                                                               
FX20     LTR   R5,R5                                                            
         BNZ   FX22                                                             
         SR    R5,R5                                                            
         ICM   R5,3,13(R2)                                                      
         GOTO1 PRNTBL,DMCB,(15,RECNOC),(R2),C'DUMP',(R5),=C'2D'                 
         GOTO1 REPORT                                                           
         B     FX12                                                             
*                                                                               
FX22     AP    RECCHA,=P'1'                                                     
         GOTO1 PUT                                                              
         CLI   PRTSW,C'Y'                                                       
         BNE   FX12                                                             
         SR    R5,R5                                                            
         ICM   R5,3,13(R2)                                                      
         GOTO1 PRNTBL,DMCB,(19,RECAFT),(R2),C'DUMP',(R5),=C'2D'                 
         GOTO1 REPORT                                                           
         B     FX12                                                             
*                                                                               
FX30     MVI   FORCEHED,C'Y'                                                    
         MVC   P(17),=C'RECORDS READ    ='                                      
         EDIT  (P8,RECTOT),(7,P+18)                                             
         GOTO1 REPORT                                                           
         MVC   P(17),=C'RECORDS CHANGED ='                                      
         EDIT  (P8,RECCHA),(7,P+18)                                             
         GOTO1 REPORT                                                           
*                                                                               
FXX      GOTO1 AENDREQ                                                          
         B     EXIT                                                             
         EJECT                                                                  
* STORAGE                                                                       
*                                                                               
         DS    0D                                                               
PRTSW    DS    CL1                                                              
RECTOT   DS    PL8                                                              
RECCHA   DS    PL8                                                              
ELEM     DS    CL256                                                            
RECBEF   DC    CL20'RECORD BEFORE CHANGE'                                       
RECAFT   DC    CL19'RECORD AFTER CHANGE'                                        
RECNOC   DC    CL15'** NO CHANGE **'                                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPNWSDTL                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREPFXBW 08/29/00'                                      
         END                                                                    
