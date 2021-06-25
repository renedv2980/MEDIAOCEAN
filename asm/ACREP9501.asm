*          DATA SET ACREP9501  AT LEVEL 005 AS OF 08/16/00                      
*PHASE AC9501A                                                                  
         PRINT NOGEN                                                            
         TITLE 'SPECS FOR PAYROLL COST ANALYSIS'                                
AC9501   CSECT                                                                  
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC F1,2,REQDETS                                                     
         SPROG 0                                                                
         ASPEC H8,35,C'-----MONTH-----     ----3 MONTH----'                     
         ASPEC H8,75,C'-----Y.T.D-----     ----12MONTH----'                     
         ASPEC H9,35,C'HOURS      COST     HOURS      COST'                     
         ASPEC H9,75,C'HOURS      COST     HOURS      COST'                     
         SPROG 1                                                                
         ASPEC H8,45,C'-----MONTH-----'                                         
         ASPEC H8,80,C'-----Y.T.D-----'                                         
         ASPEC H9,45,C'HOURS      COST'                                         
         ASPEC H9,80,C'HOURS      COST'                                         
         SPROG 0,1                                                              
         ASPEC H1,48,C'PAYROLL COST ANALYSIS'                                   
         ASPEC H2,48,21C'-'                                                     
         SPROG 2                                                                
         ASPEC H8,40,C' MONTH             3 MONTH'                              
         ASPEC H9,40,C' -----             -------'                              
         ASPEC H8,80,C' Y.T.D             12MONTH'                              
         ASPEC H9,80,C' -----             -------'                              
         ASPEC H1,44,C'DEPARTMENTAL EXPENSE ANALYSIS'                           
         ASPEC H2,44,29C'-'                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREP9501 08/16/00'                                      
         END                                                                    
