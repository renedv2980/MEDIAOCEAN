*          DATA SET ACREPC101  AT LEVEL 024 AS OF 04/10/91                      
*PHASE ACC101A,+0                                                               
         PRINT NOGEN                                                            
ACC11    CSECT                                                                  
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         SPACE 1                                                                
         ACDEF SPROG,0,1,2,4                                                    
         ACDEF F1,2,REQDETS                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,40,C'DETAIL UNDISBURSED ANALYSIS'                             
         ACDEF H1,80,REPORT                                                     
         ACDEF H1,98,PAGE                                                       
         ACDEF H2,40,27C'-'                                                     
         ACDEF H3,2,MOSFILT                                                     
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,80,REQUESTOR                                                  
         ACDEF H5,2,LEDGER                                                      
         ACDEF H5,80,C'END DATE'                                                
         SPACE 1                                                                
         ACDEF SPROG,0,2,4                                                      
         ACDEF H6,2,C'ACCOUNT'                                                  
         SPACE 1                                                                
         ACDEF SPROG,0                                                          
         ACDEF H11,2,C'CONTRA ACCOUNT'                                          
         ACDEF H11,19,C'----INVOICE----    BATCH'                               
         ACDEF H12,19,C'NUMBER AND DATE  REFERENCE'                             
         ACDEF H11,47,C'------------OTHER INFORMATION------------'              
         ACDEF H11,96,C'PAYABLE    AGE'                                         
         ACDEF H12,97,C'AMOUNT    ---'                                          
         ACDEF H12,2,14C'-'                                                     
         SPACE 1                                                                
         ACDEF SPROG,4                                                          
         ACDEF H11,2,C'CONTRA ACCOUNT'                                          
         ACDEF H11,23,C'INVOICE        BATCH'                                   
         ACDEF H12,19,C'NUMBER AND DATE  REFERENCE'                             
         ACDEF H11,59,C'OTHER INFORMATION'                                      
         ACDEF H11,96,C'PAYABLE    AGE'                                         
         ACDEF H12,97,C'AMOUNT'                                                 
         SPACE 1                                                                
         ACDEF SPROG,2                                                          
         ACDEF H11,63,C'ACCOUNT TOTAL'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACREPC101 04/10/91'                                      
         END                                                                    
