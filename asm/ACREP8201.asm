*          DATA SET ACREP8201  AT LEVEL 004 AS OF 07/25/89                      
*PHASE AC8201A,+0                                                               
         TITLE 'SPECS FOR RECEIVABLE STATEMENT'                                 
AC8201   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF SET,ACCTBAL                                                      
*                                                                               
         ACDEF SPROG,0,1,2                                                      
         ACDEF H1,1,SPACES                                                      
         ACDEF H4,90,PAGE                                                       
         ACDEF H4,100,REPORT                                                    
         ACDEF H5,1,C' '                                                        
         ACDEF H6,1,C' '                                                        
         ACDEF H7,1,C' '                                                        
         ACDEF H8,1,C' '                                                        
         ACDEF H9,1,C' '                                                        
*                                                                               
         ACDEF SPROG,0,1,2                                                      
         ACDEF H1,2,36C' '                                                      
         ACDEF H1,2,RUN                                                         
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H2,43,C'ADVANCES STATEMENT'                                      
         ACDEF H3,43,18C'-'                                                     
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H2,43,C'ADVANCES SUMMARY'                                        
         ACDEF H3,43,16C'-'                                                     
         ACDEF H10,2,C'ACCOUNT AND NAME'                                        
         ACDEF H11,2,23C'-'                                                     
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H2,43,C'CONTRA ACCOUNT SUMMARY'                                  
         ACDEF H3,43,22C'-'                                                     
         ACDEF H10,2,C'CONTRA ACCOUNT'                                          
         ACDEF H11,2,14C'-'                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREP8201 07/25/89'                                      
         END                                                                    
