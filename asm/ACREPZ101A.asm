*          DATA SET ACREPZ101A AT LEVEL 015 AS OF 10/29/99                      
*PHASE ACZ101A,+0                                                               
         TITLE 'FIX TRANSACTION OFFICE'                                         
ACZ101   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'FIX TRANSACTION EL'                                      
         ACDEF H2,38,C'------------------'                                      
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H9,2,C'ID'                                                       
         ACDEF H10,2,C'--'                                                      
         ACDEF H9,7,C'ACCOUNT'                                                  
         ACDEF H10,7,C'-------'                                                 
         ACDEF H9,19,C'W/C KEY'                                                 
         ACDEF H10,19,C'-------'                                                
         ACDEF H9,29,C'W/C 44'                                                  
         ACDEF H10,29,C'------'                                                 
         ACDEF H9,39,C'NEW W/C 44'                                              
         ACDEF H10,39,C'----------'                                             
         ACDEF H9,55,C'CONTRA (ULA)'                                            
         ACDEF H10,55,C'------------'                                           
         ACDEF H9,75,C'DATE'                                                    
         ACDEF H10,75,C'----'                                                   
         ACDEF H9,85,C'REF'                                                     
         ACDEF H10,85,C'---'                                                    
         ACDEF H9,95,C'TYPE'                                                    
         ACDEF H10,95,C'----'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACREPZ101A10/29/99'                                      
         END                                                                    
