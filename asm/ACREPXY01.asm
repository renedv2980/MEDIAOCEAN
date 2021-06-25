*          DATA SET ACREPXY01  AT LEVEL 013 AS OF 08/16/00                      
*PHASE ACXY01A                                                                  
         TITLE 'SPECS FOR RS PROFILE FIX'                                       
ACXY01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H1,36,C'MISSING 1R ACCOUNTS'                                     
         ACDEF H2,36,C'-----------------'                                       
         ACDEF H3,2,C'ACCOUNT'                                                  
         ACDEF H3,17,C'CONTRA'                                                  
         ACDEF H3,34,C'DATE'                                                    
         ACDEF H3,44,C'HOURS'                                                   
         ACDEF H3,52,C'TYPE OF TIME'                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPXY01 08/16/00'                                      
         END                                                                    
