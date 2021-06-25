*          DATA SET REREPID01  AT LEVEL 008 AS OF 09/12/89                      
*PHASE REID01A,*                                                                
         TITLE 'SPECS FOR INTEREP SUBSIDIARY DELETE'                            
REID01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'INTEREP SUBSIDIARY DELETE'                               
         SPROG 1                                                                
         ASPEC H3,16,C'RECORD TYPE'                                             
         ASPEC H3,33,C'PURGE COUNTS BY REP CODE AND RECORD TYPE'                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008REREPID01 09/12/89'                                      
         END                                                                    
