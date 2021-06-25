*          DATA SET ACREP8701  AT LEVEL 019 AS OF 03/25/92                      
*PHASE AC8701A                                                                  
         TITLE 'SPECS FOR  HISTORICAL X-ANALYSIS'                               
AC8701   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         SPROG 0,1,2                                                            
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H4,2,COMPANY                                                     
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H8,13,ACCOUNT                                                    
         ACDEF H4,86,REQUESTOR                                                  
         ACDEF H5,86,PERIOD                                                     
         ACDEF H10,106,C'TOTAL'                                                 
         ACDEF H11,106,C'-----'                                                 
         ACDEF F1,2,REQDETS                                                     
         SPROG 0,1                                                              
         ACDEF H1,41,C'ACCOUNT HISTORICAL CROSS-ANALYSIS'                       
         ACDEF H2,41,C'---------------------------------'                       
         ACDEF H10,2,C'CONTRA-ACCOUNT CODE AND NAME'                            
         ACDEF H11,2,C'----------------------------'                            
         SPROG 1,2                                                              
         ACDEF H7,1,60C' '                                                      
         ACDEF H8,1,110C' '                                                     
         SPROG 2                                                                
         ACDEF H1,50,C'OFFICE RECAP'                                            
         ACDEF H2,50,C'------------'                                            
         ACDEF H10,2,C'OFFICE CODE AND NAME'                                    
         ACDEF H11,2,C'--------------------'                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACREP8701 03/25/92'                                      
         END                                                                    
