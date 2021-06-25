*          DATA SET ACREPC301  AT LEVEL 004 AS OF 04/09/97                      
*PHASE ACC301A,+0                                                               
         TITLE 'ACC3 - TIME SHEET - SPECS'                                      
ACC301   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF READ,TIME                                                        
         ACDEF MODE,PROCLEV                                                     
         ACDEF MAXLINES,60                                                      
         ACDEF RESET                                                            
         SPACE 1                                                                
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,61,C'TIME SHEET'                                              
         ACDEF H2,61,10C'-'                                                     
         ACDEF H1,108,REPORT                                                    
         ACDEF H1,122,PAGE                                                      
         ACDEF H3,2,C'COMPANY'                                                  
         ACDEF H4,108,C'FOR THE PERIOD'                                         
         ACDEF H10,2,C'CLIENT/NON-CLIENT NAME'                                  
         ACDEF H10,28,C'CODE'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPC301 04/09/97'                                      
         END                                                                    
