*          DATA SET ACREPDT01  AT LEVEL 003 AS OF 07/05/07                      
*PHASE ACDT01A                                                                  
         TITLE 'FIND AND OPTIONALLY DELETE DRAFT TRANSACTIONS'                  
ACDT01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'EXPIRED DRAFT TRANSACTIONS'                              
         ACDEF H2,38,C'--------------------------'                              
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H9,2,C'ACCOUNT'                                                  
         ACDEF H10,2,C'-------'                                                 
         ACDEF H9,19,C'WC'                                                      
         ACDEF H10,19,C'--'                                                     
         ACDEF H9,24,C'CONTRA'                                                  
         ACDEF H10,24,C'------'                                                 
         ACDEF H9,40,C'REFER'                                                   
         ACDEF H10,40,C'-----'                                                  
         ACDEF H9,48,C' DATE '                                                  
         ACDEF H10,48,C'------'                                                 
         ACDEF H9,56,C'OF'                                                      
         ACDEF H10,56,C'--'                                                     
         ACDEF H9,60,C'BTCH'                                                    
         ACDEF H10,60,C'----'                                                   
         ACDEF H9,66,C'TY'                                                      
         ACDEF H10,66,C'--'                                                     
         ACDEF H9,73,C'DEBIT AMOUNT'                                            
         ACDEF H10,73,C'------------'                                           
         ACDEF H9,89,C'CREDIT AMOUNT'                                           
         ACDEF H10,89,C'-------------'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPDT01 07/05/07'                                      
         END                                                                    
