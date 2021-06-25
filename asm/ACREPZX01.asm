*          DATA SET ACREPZX01  AT LEVEL 014 AS OF 12/19/11                      
*PHASE ACZX01A                                                                  
         TITLE 'CLEARANCE UPDATE FIX'                                           
                                                                                
ACZX01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'CLEARANCE FIX REPORT'                                    
         ACDEF H2,38,C'--------------------'                                    
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
*                                                                               
         ACDEF H09,002,C'ACCOUNT'                                               
         ACDEF H10,002,C'------------'                                          
         ACDEF H09,015,C'OFC'                                                   
         ACDEF H10,015,C'---'                                                   
         ACDEF H09,019,C'CONTRA'                                                
         ACDEF H10,019,C'--------------'                                        
         ACDEF H09,034,C'  DATE '                                               
         ACDEF H10,034,C'--------'                                              
         ACDEF H09,43,C'REFER '                                                 
         ACDEF H10,43,C'------'                                                 
         ACDEF H09,50,C'SUB'                                                    
         ACDEF H10,50,C'---'                                                    
         ACDEF H09,54,C'SEQ'                                                    
         ACDEF H10,54,C'---'                                                    
         ACDEF H09,58,C'CLI'                                                    
         ACDEF H10,58,C'---'                                                    
         ACDEF H09,62,C'PRO'                                                    
         ACDEF H10,62,C'---'                                                    
         ACDEF H09,66,C'PR2'                                                    
         ACDEF H10,66,C'---'                                                    
         ACDEF H09,70,C'INVOICE'                                                
         ACDEF H10,70,C'-------'                                                
         ACDEF H09,85,C'EST #'                                                  
         ACDEF H10,85,C'-----'                                                  
         ACDEF H09,92,C'MT#'                                                    
         ACDEF H10,92,C'---'                                                    
         ACDEF H09,96,C'   AMOUNT   '                                           
         ACDEF H10,96,C'------------'                                           
         ACDEF H09,109,C'CLR DATE'                                              
         ACDEF H10,109,C'--------'                                              
         ACDEF H09,118,C'PRINT PUB'                                             
         ACDEF H10,118,C'---------'                                             
         ACDEF H09,131,C'CHECK '                                                
         ACDEF H10,131,C'------'                                                
         ACDEF H09,138,C'CHK DATE'                                              
         ACDEF H10,138,C'--------'                                              
         ACDEF H09,147,C'PD DATE'                                               
         ACDEF H10,147,C'-------'                                               
         ACDEF H09,156,C'REC DATE'                                              
         ACDEF H10,156,C'--------'                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACREPZX01 12/19/11'                                      
         END                                                                    
