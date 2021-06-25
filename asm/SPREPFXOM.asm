*          DATA SET SPREPFXOM  AT LEVEL 026 AS OF 08/20/98                      
*          DATA SET SPREPFXCSO AT LEVEL 045 AS OF 06/05/98                      
*PHASE SPFX02R                                                                  
         TITLE 'SPFX02 - FIX BUY DEMOS FOR OMNY/R/GET/NSP/63'                   
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
         CLI   MODE,PROCBUY                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX10     L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2             TEST DEMO ELEMENT PRESENT                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   BYTE,0              SET 'NOT FIXED' FLAG                         
*                                                                               
         LA    R1,24(R6)                                                        
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         AHI   R0,-24                                                           
         SRL   R0,3                                                             
*                                                                               
FX12     CLI   2(R1),X'94'         TEST AD2554                                  
         BNE   FX14                                                             
         MVI   2(R1),X'8F'         SET TO AD1854                                
         MVI   BYTE,1              SET UPDATED FLAG                             
*                                                                               
FX14     LA    R1,8(R1)            NEXT DEMO                                    
         BCT   R0,FX12                                                          
*                                                                               
         CLI   BYTE,1                                                           
         BNE   *+10                                                             
         MVC   P+PCOM-PLINED(10),=CL10'NOT FIXED'                               
*                                                                               
         GOTO1 PUTBUY                                                           
         BAS   RE,PRTBUY                                                        
         B     EXIT                                                             
*                                                                               
FX50     DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PAGY(2),BUYALPHA                                                 
*                                                                               
         MVC   PMED,QMED                                                        
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
         CLI   WORK+4,C' '                                                      
         BNE   *+10                                                             
         MVC   PSTA+5(2),=C'TV'                                                 
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,PKEY,13,=C'TOG'                                  
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         BC    0,EXIT                                                           
         OI    *-3,X'F0'                                                        
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'BUYREC',(R6),C'DUMP',(R0),=C'1D00'                
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
ELCODE   DS    X                                                                
FLAG     DS    X                                                                
         DS    0D                                                               
         DC    C'DEMOELEM'                                                      
DEMOELEM DS    XL256                                                            
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL1                                                              
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
         DS    CL1                                                              
PLINOLD  DS    CL3                                                              
         DS    CL1                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
         DS    CL1                                                              
PCOM     DS    CL10                                                             
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPREPFXOM 08/20/98'                                      
         END                                                                    
