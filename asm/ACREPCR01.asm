*          DATA SET ACREPCR01  AT LEVEL 003 AS OF 01/30/02                      
*PHASE ACCR01A,*                                                                
ACCR01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF RESET                                                            
         ACDEF WIDTH,198                                                        
*                                                                               
         ACDEF SPROG,0,1,2                                                      
         ACDEF H1,2,RUN                                                         
         ACDEF H1,48,C'Cash Reconciliation'                                     
         ACDEF H2,48,C'-------------------'                                     
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H2,86,COMPANY                                                    
         ACDEF H3,86,REQUESTOR                                                  
         ACDEF H2,2,C'BANK           :'                                         
         ACDEF H3,2,C'ACCOUNT #      :'                                         
         ACDEF H4,2,C'STATEMENT DATE :'                                         
*                                                                               
         ACDEF SPROG,1,2                                                        
         ACDEF H6,3,C'Check #'                                                  
         ACDEF H6,14,C'Check Date'                                              
         ACDEF H6,29,C'Check Amount'                                            
         ACDEF H6,50,C'Deposit Amount'                                          
         ACDEF H6,71,C'DDS Cash Account'                                        
         ACDEF H6,88,C'Contra Account'                                          
         ACDEF H6,110,C'Cleared Date'                                           
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H6,125,C'Errors'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPCR01 01/30/02'                                      
         END                                                                    
