*          DATA SET ACREPXG01  AT LEVEL 004 AS OF 08/16/00                      
*PHASE ACXG01A,+0                                                               
         TITLE 'SPECS FOR OFFICE CHANGE FIX'                                    
ACXG01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF GETOPT,NO                                                        
         ACDEF UPDATE,ACCFILE                                                   
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H1,38,C'CHANGE CONTRA NAMES'                                     
         ACDEF H2,38,C'-------------------'                                     
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
*                                                                               
         ACDEF H09,2,C'RECORD'                                                  
         ACDEF H10,2,C'------'                                                  
         ACDEF H09,9,C'ACTION'                                                  
         ACDEF H10,9,C'------'                                                  
*                                                                               
         ACDEF H09,16,C'ACCOUNT CODE'                                           
         ACDEF H10,16,C'------------'                                           
         ACDEF H09,31,C'OFFICE'                                                 
         ACDEF H10,31,C'------'                                                 
         ACDEF H09,38,C'CONTRA CODE'                                            
         ACDEF H10,38,C'-----------'                                            
         ACDEF H09,53,C'OLD CONTRA NAME'                                        
         ACDEF H10,53,C'---------------'                                        
         ACDEF H09,90,C'NEW CONTRA NAME'                                        
         ACDEF H10,90,C'---------------'                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPXG01 08/16/00'                                      
         END                                                                    
