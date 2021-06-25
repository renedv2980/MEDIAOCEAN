*          DATA SET REREPAC01T AT LEVEL 022 AS OF 05/15/97                      
*PHASE REAC01T,*                                                                
         TITLE 'SPECS FOR SELTEL TV DATA CONVERSION'                            
*                                                                               
*- REREPAC01 -- PHASE REAC01 -- SPECS MODULE FOR KATZ TV ACTUALIZER             
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REAC01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC UPDATE,REPFIL                                                    
         FSPEC UPDATE,REPDIR                                                    
         SPROG 0,1                                                              
         ASPEC H1,02,RUN                                                        
         ASPEC H1,36,C'KATZ TV ACTUALIZER'                                      
         ASPEC H1,60,PAGE                                                       
         ASPEC H3,02,C'REP'                                                     
         ASPEC H3,08,C'STA'                                                     
         ASPEC H3,18,C'OFF'                                                     
         ASPEC H3,38,C'CONTRACT'                                                
         ASPEC H3,54,C'ACTUAL'                                                  
         ASPEC H3,65,C'HOUSE ACCT'                                              
         SPROG 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022REREPAC01T05/15/97'                                      
         END                                                                    
