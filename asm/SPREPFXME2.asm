*          DATA SET SPREPFXME2 AT LEVEL 020 AS OF 09/20/00                      
*          DATA SET SPREPFX02Y AT LEVEL 037 AS OF 09/14/00                      
*PHASE SPFX02Y                                                                  
         TITLE 'SPFX02 - DELETE GOAL RECORDS FOR WINY'                          
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX10                                                             
         CLI   MODE,CLTFRST                                                     
         BE    FX20                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
FX10     MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'      SET TO IGNORE 'RECORD IS DELETED'            
         XC    MYKEY,MYKEY                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
FX20     XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVI   KEY,X'02'                                                        
         MVC   KEY+1(3),1(R6)      A-M/CLT                                      
         GOTO1 HIGH                                                             
         B     FX24                                                             
*                                                                               
FX22     GOTO1 SEQ                                                              
*                                                                               
FX24     CLI   KEY+4,X'FF'         TEST PRD POL (CPP GUIDE)                     
         BE    FX100               YES - DONE                                   
*                                                                               
         CLC   KEY(4),KEYSAVE      SAME TY/A-M/CLT                              
         BNE   FX100               NO - EXIT                                    
*                                                                               
         CLI   KEY+7,4             TEST ESTIMATE 4                              
         BNE   FX22                                                             
*                                                                               
         BAS   RE,PRTKEY                                                        
*                                                                               
         OI    KEY+13,X'80'        DELETE THE DIRECTORY                         
         GOTO1 WRITE                                                            
*                                                                               
         GOTO1 GETGOAL                                                          
         L     R6,ADGOAL                                                        
         USING GOALRECD,R6                                                      
         OI    GOALREC+15,X'80'                                                 
         GOTO1 PUTGOAL                                                          
         DROP  R6                  FUCKHEAD                                     
         B     FX22                                                             
*                                                                               
FX100    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* S/R TO PRINT BUY RECORD KEYS                                                  
*                                                                               
PRTKEY   NTR1                                                                   
*                                                                               
         LA    R6,KEY                                                           
         USING GOALRECD,R6                                                      
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PAGY(2),AGY                                                      
*                                                                               
         MVC   PMED,QMED                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,GKEYCLT,PCLT                                         
*                                                                               
         L     RE,ADCLT                                                         
         LA    RE,(CLIST-CLTHDRD)(RE)                                           
PRTK2    CLC   KEY+4(1),3(RE)      MATCH PRD                                    
         BE    PRTK4                                                            
         AHI   RE,4                                                             
         CLI   0(RE),C'A'                                                       
         BNL   PRTK2                                                            
         LA    RE,=C'***'                                                       
*                                                                               
PRTK4    MVC   PPRD,0(RE)                                                       
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
         UNPK  PEST,DUB                                                         
*                                                                               
         MVC   PLIN(1),GKEYDPT                                                  
*                                                                               
         SR    R0,R0                                                            
         IC    R0,GKEYSLN                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN+1(2),DUB                                                    
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,PKEY,13,=C'TOG'                                  
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
ELCODE   DS    X                                                                
FLAG     DS    X                                                                
         DS    0D                                                               
         DC    CL8'*NETSEQ*'                                                    
NETSEQ   DS    X                                                                
NETEBC   DS    CL8                                                              
         DS    0D                                                               
         DC    CL8'**MYKEY*'                                                    
MYKEY    DS    XL13                                                             
         DS    0D                                                               
         DC    CL8'*LASTPRT'                                                    
LASTPRT  DS    XL13                                                             
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
LASTNET  DC    XL3'00'                                                          
*                                                                               
         DS    0D                                                               
         DC    CL8'*ESTTAB*'                                                    
TAB      DS    256XL256                                                         
TABX     EQU   *                                                                
                                                                                
*                                                                               
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PKEY     DS    CL30                BUY LINE KEY                                 
         DS    CL1                                                              
PCOM     DS    CL10                                                             
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPREPFXME209/20/00'                                      
         END                                                                    
