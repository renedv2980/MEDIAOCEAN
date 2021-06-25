*          DATA SET ACREPI301  AT LEVEL 004 AS OF 08/17/00                      
*PHASE ACI301A                                                                  
         TITLE 'ACI301 - PRODUCTION INTERFACE SPECS'                            
ACI301   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF SPROG,0,1,2                                                      
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF MODE,PROCLEV                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,76,REPORT                                                     
         ACDEF H1,90,PAGE                                                       
         ACDEF H3,2,COMPANY                                                     
         SPACE 1                                                                
         ACDEF SPROG,0,1                                                        
         ACDEF H7,3,C'YEAR'                                                     
         ACDEF H8,3,C'----'                                                     
         ACDEF H7,12,C'BUDGET CODE'                                             
         ACDEF H8,12,11C'-'                                                     
         ACDEF H7,36,C'JOB NUMBER'                                              
         ACDEF H8,36,C'----------'                                              
         ACDEF H7,55,C'INVOICE NO.'                                             
         ACDEF H8,55,C'-----------'                                             
         ACDEF H7,81,C'AMOUNT'                                                  
         ACDEF H8,81,6C'-'                                                      
         ACDEF SPROG,0,1,2                                                      
         ACDEF H6,2,95C'-'                                                      
         ACDEF SPROG,2                                                          
         ACDEF H4,40,C'* BUDGET SUMMARY *'                                      
         ACDEF H7,26,C'AMOUNT'                                                  
         ACDEF H8,26,C'------'                                                  
         ACDEF H7,2,C'BUDGET CODE'                                              
         ACDEF H8,2,C'-----------'                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPI301 08/17/00'                                      
         END                                                                    
