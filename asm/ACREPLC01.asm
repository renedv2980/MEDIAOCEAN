*          DATA SET ACREPLC01  AT LEVEL 003 AS OF 08/17/00                      
*PHASE ACLC01A                                                                  
         TITLE 'SPECS FOR LEDGER COPY PROGRAM'                                  
ACLC01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'LEDGER COPY '                                            
         ACDEF H2,38,C'------------'                                            
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H9,2,C'ACCOUNT'                                                  
         ACDEF H10,2,C'-------'                                                 
         ACDEF H9,18,C'NAME'                                                    
         ACDEF H10,18,C'---'                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPLC01 08/17/00'                                      
         END                                                                    
