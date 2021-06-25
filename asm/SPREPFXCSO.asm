*          DATA SET SPREPFXCSO AT LEVEL 052 AS OF 03/26/99                      
*PHASE SPFX02K                                                                  
         TITLE 'SPFX02 - INSERT MISSING ELKNER DEMO ELEMENTS'                   
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
         BE    FX2                                                              
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX2      DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         LA    R6,BDELEM                                                        
         MVI   ELCODE,X'0C'                                                     
FX4      BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         TM    6(R6),X'80'                                                      
         BZ    FX4                                                              
         SR    R7,R7                                                            
         IC    R7,1(R6)                                                         
         AR    R7,R6               POINT TO NEXT ELEMENT                        
         CLI   0(R7),X'0C'                                                      
         BNE   FX4                                                              
         TM    6(R7),X'80'                                                      
         BZ    FX4                                                              
* PRINT IT OUT                                                                  
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
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
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
         EJECT                                                                  
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
         DS    CL1                                                              
PLINOLD  DS    CL3                                                              
         DS    CL1                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052SPREPFXCSO03/26/99'                                      
         END                                                                    
