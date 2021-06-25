*          DATA SET ACREP8301  AT LEVEL 013 AS OF 11/15/89                      
*PHASE AC8301A,+0                                                               
         TITLE 'SPECS FOR RECEIVABLE STATEMENT'                                 
AC8301   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
*                                                                               
         ACDEF SPROG,0,1,2,3                                                    
         ACDEF H1,1,SPACES                                                      
         ACDEF H4,90,PAGE                                                       
         ACDEF H4,100,REPORT                                                    
         ACDEF H5,1,C' '                                                        
         ACDEF H6,1,C' '                                                        
         ACDEF H7,1,C' '                                                        
         ACDEF H8,1,C' '                                                        
         ACDEF H9,1,C' '                                                        
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H2,43,C'RECEIVABLE STATEMENT'                                    
         ACDEF H3,43,20C'-'                                                     
         ACDEF H5,2,ACCADD                                                      
         ACDEF H10,2,C'BILLING SOURCE  BILL '                                   
         ACDEF H11,2,C'-------------- NUMBER'                                   
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H2,45,C'BILLING SOURCE SUMMARY'                                  
         ACDEF H3,45,22C'-'                                                     
         ACDEF H10,2,C'BILLING SOURCE'                                          
         ACDEF H11,2,14C'-'                                                     
*                                                                               
         ACDEF SPROG,2,3                                                        
         ACDEF H2,47,C'RECEIVABLE SUMMARY'                                      
         ACDEF H3,47,18C'-'                                                     
         ACDEF H4,2,36C' '                                                      
         ACDEF H4,2,RUN                                                         
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H10,2,C'ACCOUNT CODE AND NAME'                                   
         ACDEF H11,2,21C'-'                                                     
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H10,2,C'ACCOUNT/BILLING SOURCE'                                  
         ACDEF H11,2,22C'-'                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREP8301 11/15/89'                                      
         END                                                                    
