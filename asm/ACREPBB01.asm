*          DATA SET ACREPBB01  AT LEVEL 002 AS OF 07/09/07                      
*PHASE ACBB01A,+0                                                               
         TITLE 'SPECS FOR BALANCE BUCKET REPORT'                                
ACBB01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         ACDEF UPDATE,ACCFIL                                                    
                                                                                
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,112,REPORT                                                    
         ACDEF H1,132,PAGE                                                      
         ACDEF H2,2,MOSFILT                                                     
         ACDEF H2,112,REQUESTOR                                                 
         ACDEF H3,2,COMPANY                                                     
         ACDEF H4,2,UNIT                                                        
         ACDEF H5,2,LEDGER                                                      
                                                                                
         ACDEF SPROG,0,1                                                        
         ACDEF H6,2,C'ACCOUNT'                                                  
         ACDEF H7,2,C'*ERROR*'                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPBB01 07/09/07'                                      
         END                                                                    
