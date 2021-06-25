*          DATA SET ACREP7A01  AT LEVEL 011 AS OF 08/16/00                      
*PHASE AC7A01A                                                                  
         TITLE 'SPECS FOR NAME AND ADDRESS LISTING'                             
AC7A01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF RESET                                                            
                                                                                
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,86,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H9,50,C'NEW ACCOUNT NAME'                                        
         ACDEF H10,50,C'----------------'                                       
         ACDEF F1,2,REQDETS                                                     
                                                                                
         ACDEF SPROG,0                                                          
         ACDEF H1,40,C'CONVERSION LISTING - OLD ACCOUNT ORDER'                  
         ACDEF H2,40,C'--------------------------------------'                  
         ACDEF H9,10,C'OLD ACCOUNT'                                             
         ACDEF H10,10,C'-----------'                                            
         ACDEF H9,30,C'NEW ACCOUNT'                                             
         ACDEF H10,30,C'-----------'                                            
                                                                                
         ACDEF SPROG,1                                                          
         ACDEF H1,40,C'CONVERSION LISTING - NEW ACCOUNT ORDER'                  
         ACDEF H2,40,C'--------------------------------------'                  
         ACDEF H9,10,C'NEW ACCOUNT'                                             
         ACDEF H10,10,C'-----------'                                            
         ACDEF H9,30,C'OLD ACCOUNT'                                             
         ACDEF H10,30,C'-----------'                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREP7A01 08/16/00'                                      
         END                                                                    
