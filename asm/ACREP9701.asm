*          DATA SET ACREP9701  AT LEVEL 011 AS OF 01/05/99                      
*PHASE AC9701A,*                                                                
         TITLE 'SPECS FOR ACCOUNT DELETER'                                      
AC9701   CSECT                                                                  
         ACDEF READ,ACCOUNTS                                                    
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF RESET                                                            
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,44,C'LIST OF DELETED ACCOUNTS'                                
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H2,44,24C'-'                                                     
         ACDEF H4,2,COMPANY                                                     
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H4,74,REQUESTOR                                                  
         ACDEF H5,74,PERIOD                                                     
         ACDEF SPROG,1                                                          
         ACDEF H6,74,C'*** ONLY ACCOUNTS THAT   ***'                            
         ACDEF H7,74,C'*** HAVE NEVER BEEN USED ***'                            
         ACDEF SPROG,0,1                                                        
         ACDEF H10,21,C'ACCOUNT NUMBER      ACCOUNT NAME'                       
         ACDEF H11,21,C'--------------      ------------'                       
         ACDEF H10,79,C'LAST ACTIVE DATE'                                       
         ACDEF H11,79,C'----------------'                                       
         ACDEF F1,2,REQDETS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREP9701 01/05/99'                                      
         END                                                                    
