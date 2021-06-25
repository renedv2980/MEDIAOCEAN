*          DATA SET ACREPZ401  AT LEVEL 010 AS OF 08/16/00                      
*PHASE ACZ401A                                                                  
ACZ401   TITLE 'CHANGE ACCOUNT'                                                 
ACZ401   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF GETOPT,N                                                         
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H1,50,C'ACCOUNT CHANGE'                                          
         ACDEF H2,50,C'--------------'                                          
         ACDEF H4,2,UNIT                                                        
         ACDEF H5,2,LEDGER                                                      
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREPZ401 08/16/00'                                      
         END                                                                    
