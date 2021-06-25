*          DATA SET ACREPX901  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACX901A                                                                  
         TITLE 'FIX/REMOVE ELEMENTS'                                            
ACX901   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'FIX ELEMENT FIELDS'                                      
         ACDEF H2,38,C'------------------'                                      
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H09,2,C'ACCOUNT'                                                 
         ACDEF H10,2,C'--------------'                                          
         ACDEF H09,17,C'OF'                                                     
         ACDEF H10,17,C'--'                                                     
         ACDEF H09,20,C'CONTRA'                                                 
         ACDEF H10,20,C'--------------'                                         
         ACDEF H09,35,C'DATE '                                                  
         ACDEF H10,35,C'--------'                                               
         ACDEF H09,44,C'REFER'                                                  
         ACDEF H10,44,C'------'                                                 
         ACDEF H09,51,C'SUB'                                                    
         ACDEF H10,51,C'---'                                                    
         ACDEF H09,57,C'     DEBITS  '                                          
         ACDEF H10,57,C'-------------'                                          
         ACDEF H09,71,C'    CREDITS  '                                          
         ACDEF H10,71,C'-------------'                                          
         ACDEF H09,86,C'OLD DATE'                                               
         ACDEF H10,86,C'--------'                                               
         ACDEF H09,95,C'NEW DATE'                                               
         ACDEF H10,95,C'--------'                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPX901 08/16/00'                                      
         END                                                                    
