*          DATA SET ACREP7201  AT LEVEL 009 AS OF 08/16/00                      
*PHASE AC7201A                                                                  
AC7201   CSECT                                                                  
         TITLE 'AC7201 SPECS FOR LIST OF ACCOUNT RULES'                         
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF MODE,PROCLEV                                                     
         ACDEF SPROG,0                                                          
         ACDEF H1,51,C'LIST OF ACCOUNT RULES'                                   
         ACDEF H1,2,RUN                                                         
         ACDEF H1,81,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H2,51,21C'-'                                                     
         ACDEF H3,2,COMPANY                                                     
         ACDEF H3,81,REQUESTOR                                                  
         ACDEF H6,2,C'-------ACCOUNT-------'                                    
         ACDEF H6,56,C'----CODES---   ---RATES---'                              
         ACDEF H7,2,C'CODE             NAME'                                    
         ACDEF H7,56,C'MEDIA   WORK   COMMN   TAX'                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREP7201 08/16/00'                                      
         END                                                                    
