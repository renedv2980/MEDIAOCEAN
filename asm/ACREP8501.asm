*          DATA SET ACREP8501  AT LEVEL 003 AS OF 07/13/99                      
*PHASE AC8501A,+0                                                               
         TITLE 'SPECS FOR HISTORICAL ANALYSIS'                                  
AC8501   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         ACDEF READ,TRANSACTIONS                                                
         ACDEF GENBUCK,CONTRA                                                   
         SPROG 0,1                                                              
         ACDEF H1,47,C'HISTORICAL ANALYSIS'                                     
         ACDEF H2,47,19C'-'                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H4,2,COMPANY                                                     
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H4,86,REQUESTOR                                                  
         ACDEF H5,86,PERIOD                                                     
         ACDEF H10,106,C'TOTAL'                                                 
         ACDEF H11,106,C'-----'                                                 
         ACDEF F1,2,REQDETS                                                     
         SPROG 0                                                                
         ACDEF H10,2,C'ACCOUNT CODE AND NAME'                                   
         ACDEF H11,2,21C'-'                                                     
         SPROG 1                                                                
         ACDEF H4,50,C'OFFICE RECAP'                                            
         ACDEF H10,2,C'OFFICE CODE AND NAME'                                    
         ACDEF H11,2,C'--------------------'                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREP8501 07/13/99'                                      
         END                                                                    
