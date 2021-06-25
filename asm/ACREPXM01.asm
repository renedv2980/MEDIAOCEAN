*          DATA SET ACREPXM01  AT LEVEL 015 AS OF 08/16/00                      
*PHASE ACXM01A,+0                                                               
         TITLE 'FIX/REMOVE ELEMENTS'                                            
ACXM01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C' ADD STATUS ELEMENT '                                    
         ACDEF H2,38,C'--------------------'                                    
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACREPXM01 08/16/00'                                      
         END                                                                    
