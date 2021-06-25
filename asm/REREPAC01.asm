*          DATA SET REREPAC01  AT LEVEL 021 AS OF 08/08/96                      
*PHASE REAC01A,*                                                                
         TITLE 'SPECS FOR KATZ DATA CONVERSION'                                 
*                                                                               
*- REREPAC01 -- PHASE REAC01 -- SPECS MODULE FOR KATZ ACTUALIZER                
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
         ASPEC H1,36,C'KATZ ACTUALIZER'                                         
         ASPEC H1,60,PAGE                                                       
         ASPEC H3,02,C'REP'                                                     
         ASPEC H3,08,C'STA'                                                     
         ASPEC H3,18,C'OFF'                                                     
         ASPEC H3,30,C'CONTRACT'                                                
         ASPEC H3,45,C'ACTUAL'                                                  
         ASPEC H3,56,C'HOUSE ACCT'                                              
         SPROG 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021REREPAC01 08/08/96'                                      
         END                                                                    
