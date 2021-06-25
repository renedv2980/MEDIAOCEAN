*          DATA SET SPREPFXGM  AT LEVEL 056 AS OF 06/05/98                      
*PHASE SPFX022                                                                  
         TITLE 'SPFX02 - LOOK FOR BAD CPP GUIDE RECORDS'                        
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX50                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         EJECT                                                                  
* CLTFRST                                                                       
*                                                                               
FX50     DS    0H                                                               
         XC    KEY,KEY                                                          
         L     R2,ADCLT                                                         
         MVI   KEY,X'02'                                                        
         MVC   KEY+1(3),1(R2)        A/M,CLT                                    
         MVI   KEY+4,X'FF'                                                      
         GOTO1 HIGH                                                             
         B     FX55                                                             
*                                                                               
FX52     GOTO1 SEQ                                                              
*                                                                               
FX55     CLC   KEY(5),KEYSAVE      SAME TYP/AM/CLT/POL                          
         BNE   FX100                                                            
*                                                                               
FX57     GOTO1 GETGOAL                                                          
*                                                                               
         L     R6,ADGOAL                                                        
         USING GOALRECD,R6                                                      
*                                                                               
         LA    R6,GDELEM                                                        
         SR    R7,R7                                                            
*                                                                               
FX60     SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    FX64                                                             
         CLI   0(R6),X'21'                                                      
         BNE   FX60                                                             
         LA    R7,1(R7)                                                         
         B     FX60                                                             
*                                                                               
FX64     CHI   R7,51                                                            
         BH    FX52                                                             
         BAS   RE,PRTGL                                                         
         B     FX52                                                             
*                                                                               
FX100    GOTO1 AENDREQ                                                          
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTGL    NTR1                                                                   
*                                                                               
         L     R6,ADGOAL                                                        
         USING GOALRECD,R6                                                      
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PAGY(2),GAGYALPH                                                 
         GOTO1 CLUNPK,DMCB,GKEYCLT,PCLT                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,GKEYMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMKT,DUB                                                         
*                                                                               
         ZIC   R0,GKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PEST,WORK+7                                                      
*                                                                               
         MVC   PLIN(1),GKEYDPT                                                  
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         AP    BUYFIX,=P'1'                                                     
*                                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
*                                                                               
         DS    0D                                                               
ELEM     DS    CL256                                                            
ELCODE   DS    X                                                                
CHG      DS    CL1                                                              
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL26                                                            
INBUYS   DC    PL6'0',CL20'RECORDS IN'                                          
BUYFIX   DC    PL6'0',CL20'BUYS FIXED'                                          
BUCKTABX EQU   *-BUCKTAB                                                        
         SPACE 2                                                                
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
*                                                                               
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056SPREPFXGM 06/05/98'                                      
         END                                                                    
