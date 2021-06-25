*          DATA SET ACREPXR01A AT LEVEL 007 AS OF 08/16/00                      
*PHASE ACXR01A                                                                  
         TITLE 'DELETE 1900 BUDGETS FROM MKDTO'                                 
ACXR01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'BUDGET UPLOAD DELETE'                                    
         ACDEF H2,38,C'--------------------'                                    
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPXR01A08/16/00'                                      
         END                                                                    
