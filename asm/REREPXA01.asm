*          DATA SET REREPXA01  AT LEVEL 030 AS OF 03/21/00                      
*PHASE REXA01A,*                                                                
         TITLE 'SPECS FOR RTS REP SPOT AUDIT EXTRACT'                           
*                                                                               
*- REREPXA01 -- PHASE REXA01 -- SPECS                                           
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REXA01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 1                                                                
         ASPEC H1,02,REP                                                        
         ASPEC H1,40,C'RTS REP FILE EXTRACT'                                    
         ASPEC H1,90,RENUM                                                      
         ASPEC H1,110,PAGE                                                      
         ASPEC H2,02,REQUESTOR                                                  
         ASPEC H2,90,RUN                                                        
         SPROG 1                                                                
*                              1         2         3         4                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030REREPXA01 03/21/00'                                      
         END                                                                    
