*          DATA SET ACREP8801  AT LEVEL 002 AS OF 08/16/00                      
*PHASE AC8801A                                                                  
         TITLE 'ACREP8802 PRINT SPEC'                                           
AC8801   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         ACDEF MODE,PROCLEV                                                     
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,44,C'CLIENT EXPENSE ANALYSIS'                                 
         ACDEF H2,44,23C'-'                                                     
         ACDEF H1,80,C'REPORT AC88'                                             
         ACDEF H1,98,PAGE                                                       
         SPACE 1                                                                
         ACDEF H3,2,COMPANY                                                     
         ACDEF H3,80,C'REQUEST PERIOD'                                          
         SPACE 1                                                                
         ACDEF H5,80,REQUESTOR                                                  
         SPACE 1                                                                
         ACDEF SPROG,0                                                          
         ACDEF H5,2,C'EXPENSE TYPE'                                             
         SPACE 1                                                                
         ACDEF SPROG,1                                                          
         ACDEF H5,2,C'REQUEST SUMMARY'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREP8801 08/16/00'                                      
         END                                                                    
