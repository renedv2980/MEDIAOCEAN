*          DATA SET ACREP8901  AT LEVEL 008 AS OF 08/16/00                      
*PHASE AC8901A                                                                  
         TITLE 'SPECS FOR BUDGET COMPARISON REPORT'                             
         PRINT NOGEN                                                            
AC8901   CSECT                                                                  
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         ASPEC H1,2,RUN                                                         
         ASPEC H1,45,C'BUDGET COMPARISON REPORT'                                
         ASPEC H2,45,24C'-'                                                     
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H4,2,COMPANY                                                     
         ASPEC H5,2,UNIT                                                        
         ASPEC H6,2,LEDGER                                                      
         ASPEC H9,48,C'BUDGET'                                                  
         ASPEC H10,49,C'TYPE'                                                   
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC H5,86,PERIOD                                                     
         ASPEC H9,02,C'ACCOUNT                   CONTRA-ACCOUNT'                
         ASPEC H10,2,C'-------                   --------------'                
         ASPEC  H9,55,C'BUDGET PERIOD    BUDGET    ACTUAL   BALANCE'            
         ASPEC H10,55,C'-------------    ------    ------   -------'            
         ASPEC  H9,100,C'PERCENTAGE'                                            
         ASPEC H10,100,C'OF BUDGET'                                             
         ASPEC F1,2,REQDETS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREP8901 08/16/00'                                      
         END                                                                    
