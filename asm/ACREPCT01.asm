*          DATA SET ACREPCT01  AT LEVEL 006 AS OF 09/17/97                      
*PHASE ACCT01A,+0                                                               
         TITLE 'ACCT - OM TIME SHEET - SPECS'                                   
ACCT01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF READ,TIME                                                        
         ACDEF RESET                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPCT01 09/17/97'                                      
         END                                                                    
