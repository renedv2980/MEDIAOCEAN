*          DATA SET ACREP3801  AT LEVEL 005 AS OF 08/16/00                      
*PHASE AC3801A                                                                  
         TITLE 'SPECS FOR STATEMENT PRINTING'                                   
AC3801   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF MODE,PROCLEV                                                     
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5,6,7                                            
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H2,2,MOSFILT                                                     
         ACDEF H3,86,REQUESTOR                                                  
         ACDEF H4,3,COMPANY                                                     
         ACDEF F1,2,REQDETS                                                     
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,45,C'CONTRA-ACCOUNT STATEMENT'                                
         ACDEF H2,45,24C'-'                                                     
         ACDEF H10,2,C'ACCOUNT'                                                 
         ACDEF H10,25,C'------------TRANSACTION------------'                    
         ACDEF H10,80,C'DEBIT      CREDIT      BALANCE'                         
         ACDEF H11,2,7C'-'                                                      
         ACDEF H11,25,C'BATCH  DATE     REF. CODE NARRATIVE'                    
         ACDEF H11,80,C'-----      ------      -------'                         
         ACDEF SPROG,1                                                          
         ACDEF H10,80,C'           AMOUNT        TOTAL'                         
         ACDEF H11,80,C'           ------        -----'                         
         ACDEF SPROG,2,3                                                        
         ACDEF H1,46,C'CONTRA-ACCOUNT SUMMARY'                                  
         ACDEF H2,46,22C'-'                                                     
         ACDEF H10,2,C'C/ACCOUNT CODE AND NAME'                                 
         ACDEF H10,80,C'DEBIT      CREDIT      BALANCE'                         
         ACDEF H11,2,23C'-'                                                     
         ACDEF H11,80,C'-----      ------      -------'                         
         ACDEF SPROG,3                                                          
         ACDEF H10,80,C'           AMOUNT        TOTAL'                         
         ACDEF H11,80,C'           ------        -----'                         
         ACDEF SPROG,7                                                          
         ACDEF H1,46,C'CONTRA-ACCOUNT TOTALS'                                   
         ACDEF H10,25,C'TOTALS FOR REPORT'                                      
         ACDEF H11,25,C'-----------------'                                      
         ACDEF H10,80,C'DEBIT      CREDIT      BALANCE'                         
         ACDEF H11,80,C'-----      ------      -------'                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREP3801 08/16/00'                                      
         END                                                                    
