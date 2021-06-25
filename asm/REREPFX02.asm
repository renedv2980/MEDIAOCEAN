*          DATA SET REREPFX02  AT LEVEL 006 AS OF 08/31/00                      
*          DATA SET REREPFX02  AT LEVEL 005 AS OF 08/11/99                      
*PHASE REFX02A                                                                  
         TITLE 'REFX02 - FIND ENQUEUE FAILURE'                                  
REFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,REFX02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
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
         MVI   RCTRACE,C'Y'                                                     
         XC    KEY,KEY                                                          
         MVI   KEY+26,1                                                         
         GOTO1 HIGH                                                             
         B     FX14                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX14     GOTO1 GREC                                                             
         GOTO1 PREC                                                             
         DCHO                                                                   
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
* LITERAL POOL                                                                  
         LTORG                                                                  
*                                                                               
COMMAND  DS    CL8                                                              
AIOAREA  DC    A(AIOB)                                                          
         DS    0L                                                               
         DC    CL16'HEREHEREHEREHERE'                                           
AIOB     DS    4000C                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006REREPFX02 08/31/00'                                      
         END                                                                    
