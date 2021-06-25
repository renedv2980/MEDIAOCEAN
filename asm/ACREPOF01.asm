*          DATA SET ACREPOF01  AT LEVEL 001 AS OF 12/20/02                      
*PHASE ACOF01A,+0                                                               
         TITLE 'ACOF - OFFICE LOCATOR'                                          
ACOF01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
*                                                                               
         SPROG 0,1,2,3                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,53,C'OFFICE CODE LOCATOR   '                                  
         ACDEF H2,53,C'----------------------'                                  
*        ACDEF H3,87,PERIOD                                                     
         ACDEF H4,2,COMPANY                                                     
         ACDEF H1,160,REPORT                                                    
         ACDEF H1,180,PAGE                                                      
         ACDEF H2,160,REQUESTOR                                                 
*                                                                               
         SPROG 1                                                                
         ACDEF H3,53,C'OFFICE ACCOUNT DETAIL PAGE'                              
         ACDEF H4,53,C'--------------------------'                              
         ACDEF H10,3,C'COMPANY'                                                 
         ACDEF H10,12,C'OFFICE'                                                 
         ACDEF H10,20,C'ACCOUNT'                                                
*                                                                               
         SPROG 2                                                                
         ACDEF H3,53,C'OFFICE SUMMARY'                                          
         ACDEF H4,53,C'--------------'                                          
         ACDEF H10,3,C'COMPANY'                                                 
         ACDEF H10,12,C'OFFICE'                                                 
         ACDEF H10,22,C'LIST'                                                   
         ACDEF H10,26,C'  ACCOUNTS'                                             
         ACDEF H10,42,C'SJ ACCOUNTS'                                            
         ACDEF H10,56,C'ACCOUNT TOTALS'                                         
*                                                                               
         SPROG 3                                                                
         ACDEF H3,53,C'OFFICE LIST SUMMARY'                                     
         ACDEF H4,53,C'-------------------'                                     
         ACDEF H10,3,C'COMPANY'                                                 
         ACDEF H10,12,C'OFFICE'                                                 
         ACDEF H10,20,C'IN LIST'                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPOF01 12/20/02'                                      
         END                                                                    
