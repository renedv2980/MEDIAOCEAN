*          DATA SET ACREP7101  AT LEVEL 003 AS OF 08/16/00                      
*PHASE AC7101A                                                                  
         TITLE 'SPECS FOR PRODUCTION PROFILE LISTING'                           
AC7101   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF MODE,PROCLEV                                                     
         ACDEF F1,2,REQDETS                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,47,C'PRODUCTION PROFILE LISTING'                              
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H2,47,26C'-'                                                     
         ACDEF H4,2,COMPANY                                                     
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H4,86,REQUESTOR                                                  
         ACDEF H10,2,C'RECORD   ACCOUNT CODE AND NAME'                          
         ACDEF H10,38,C'PROFILE INFORMATION'                                    
         ACDEF H11,2,C' TYPE    ---------------------'                          
         ACDEF H11,38,19C'-'                                                    
         ACDEF H8,40,C'CLIENT'                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREP7101 08/16/00'                                      
         END                                                                    
