*          DATA SET ACREPX301  AT LEVEL 009 AS OF 07/07/07                      
*PHASE ACX301A                                                                  
         TITLE 'FIX TIME TOTAL RECORDS'                                         
ACX301   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
*                                                                               
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG 0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREPX301 07/07/07'                                      
         END                                                                    
