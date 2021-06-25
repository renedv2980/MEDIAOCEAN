*          DATA SET ACREPG101  AT LEVEL 006 AS OF 08/17/00                      
*PHASE ACG101A                                                                  
         SPACE 2                                                                
ACG101   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         ACDEF MODE,PROCLEV                                                     
         SPACE 1                                                                
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,80,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H3,2,COMPANY                                                     
         ACDEF H4,2,UNIT                                                        
         ACDEF H5,2,LEDGER                                                      
         ACDEF H1,44,C'GENERAL LEDGER SUMMARY'                                  
         ACDEF H2,44,C'----------------------'                                  
         SPACE 1                                                                
         ACDEF M2,2,C'ACCOUNT NAME AND NO.'                                     
         SPACE 1                                                                
         ACDEF SPROG,0                                                          
         ACDEF M1,34,C'MONTH          MONTH'                                    
         ACDEF M2,32,C'THIS YEAR      LAST YEAR'                                
         ACDEF M1,65,C'YTD            YTD'                                      
         ACDEF M2,62,C'THIS YEAR      LAST YEAR        VARIANCE'                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPG101 08/17/00'                                      
         END                                                                    
