*          DATA SET REREPBU01  AT LEVEL 026 AS OF 08/03/99                      
*PHASE RETS01A,*                                                                
         TITLE 'SPECS FOR REP TO SPOT TRANSFER COMPARE'                         
*                                                                               
*- REREPTS01 -- PHASE RETS01 -- SPECS                                           
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
RETS01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 1                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'RTS BUYLINE COMPARE'                                     
         ASPEC H1,90,PAGE                                                       
         SPROG 1                                                                
*                              1         2         3         4                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026REREPBU01 08/03/99'                                      
         END                                                                    
