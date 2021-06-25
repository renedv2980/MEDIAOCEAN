*          DATA SET ACREPZ701  AT LEVEL 008 AS OF 08/16/00                      
*PHASE ACZ701A                                                                  
         TITLE 'UNDELETE ACCOUNTS'                                              
ACZ701   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'UNDELETED ACCOUNTS'                                      
         ACDEF H2,38,C'------------------'                                      
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H9,2,C'ACCOUNT'                                                  
         ACDEF H10,2,C'-------'                                                 
         ACDEF H9,18,C'CONTRA'                                                  
         ACDEF H10,18,C'------'                                                 
         ACDEF H9,34,C'REFER'                                                   
         ACDEF H10,34,C'-----'                                                  
         ACDEF H9,43,C'DATE'                                                    
         ACDEF H10,43,C'----'                                                   
         ACDEF H9,50,C'OFF'                                                     
         ACDEF H10,50,C'---'                                                    
         ACDEF H9,55,C'BATCH'                                                   
         ACDEF H10,55,C'-----'                                                  
         ACDEF H9,62,C'TYPE'                                                    
         ACDEF H10,62,C'----'                                                   
         ACDEF H9,68,C'     DEBITS  '                                           
         ACDEF H10,68,C'------------'                                           
         ACDEF H9,91,C'    CREDITS  '                                           
         ACDEF H10,91,C'------------'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPZ701 08/16/00'                                      
         END                                                                    
