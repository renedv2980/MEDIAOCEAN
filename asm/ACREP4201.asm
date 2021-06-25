*          DATA SET ACREP4201  AT LEVEL 011 AS OF 01/24/95                      
*PHASE AC4201A,+0                                                               
         TITLE 'SPECS FOR RECEIVABLE BALANCE REMOVAL'                           
AC4201   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF UPDATE,ACCFILE                                                   
         ACDEF RESET                                                            
*                                                                               
         ACDEF H1,2,RUN                                                         
         ACDEF H1,40,C'BALANCE REMOVAL'                                         
         ACDEF H1,76,REPORT                                                     
         ACDEF H1,91,PAGE                                                       
         ACDEF H2,40,15C'-'                                                     
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,76,REQUESTOR                                                  
         ACDEF H6,32,C'ACCOUNT NO'                                              
         ACDEF H7,32,C'----------'                                              
         ACDEF H6,56,C'BALANCE'                                                 
         ACDEF H7,53,C'BROUGHT FORWARD'                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREP4201 01/24/95'                                      
         END                                                                    
