*          DATA SET ACREP8101  AT LEVEL 028 AS OF 01/29/99                      
*PHASE AC8101A                                                                  
         TITLE 'SPECS FOR CONSOLIDATED TRIAL BALANCE'                           
AC8101   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         ACDEF READ,TRANSACTIONS                                                
         ACDEF SET,ACCTBAL                                                      
         ACDEF GETOPT,NO                                                        
         ACDEF GENBUCK,OFFICE                                                   
         ACDEF SPROG,0,1,2,4,5                                                  
         ACDEF H1,2,RUN                                                         
         ACDEF H1,50,C'TRIAL BALANCE'                                           
         ACDEF H2,50,13C'-'                                                     
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H4,2,COMPANY                                                     
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H4,86,REQUESTOR                                                  
         ACDEF H8,2,C'ACCOUNT CODE    ACCOUNT NAME'                             
         ACDEF H9,2,C'------------    ------------'                             
         ACDEF F1,2,REQDETS                                                     
         SPACE 1                                                                
         ACDEF SPROG,1                                                          
         ACDEF H1,44,C'CONSOLIDATED TRIAL BALANCE'                              
         ACDEF H2,44,26C'-'                                                     
         SPACE 1                                                                
         ACDEF SPROG,4,5                                                        
         ACDEF H4,50,C'OFFICE RECAP'                                            
         ACDEF H8,2,C'OFFICE CODE     OFFICE NAME '                             
         ACDEF H9,2,C'-----------     ----------- '                             
         SPACE 1                                                                
         ACDEF SPROG,0,1,4                                                      
         ACDEF H8,53,C'BALANCE          DEBITS         CREDITS'                 
         ACDEF H9,53,C'FORWARD          ------         -------'                 
         ACDEF H8,101,C'PRESENT'                                                
         ACDEF H9,101,C'BALANCE'                                                
         SPACE 1                                                                
         ACDEF SPROG,2,5                                                        
         ACDEF H8,53,C'                 DEBITS         CREDITS'                 
         ACDEF H9,53,C'                 ------         -------'                 
         ACDEF H8,101,C'   NET '                                                
         ACDEF H9,101,C' CHANGE'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028ACREP8101 01/29/99'                                      
         END                                                                    
