*          DATA SET ACREPFA01  AT LEVEL 003 AS OF 08/17/00                      
*PHASE ACFA01A                                                                  
         TITLE 'ACFA01 - SPECS FOR FEE ALLOCATION SYSTEM'                       
ACFA01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3,4                                                        
         ASPEC H1,2,RUN                                                         
         ASPEC H1,44,C'FEE ALLOCATION SYSTEM'                                   
         ASPEC H2,44,21C'-'                                                     
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H4,2,C'COMPANY'                                                  
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H5,85,C'FOR THE MONTH OF'                                        
         SPROG 1                                                                
         ASPEC H3,47,C'(RULE LISTING)'                                          
         ASPEC H5,2,C'CLIENT'                                                   
         ASPEC H10,2,C'DIVISION CODE AND NAME'                                  
         ASPEC H10,40,C'DEPT   SUB.   PERSON  NAME'                             
         ASPEC H11,40,C'CODE   DEPT    CODE       '                             
         ASPEC H10,93,C'RULE DETAILS'                                           
         SPROG 2                                                                
         ASPEC H3,44,C'(HOURLY DISTRIBUTION)'                                   
         ASPEC H5,2,C'OFF/DEPT'                                                 
         ASPEC H6,2,C'SUB-DEPT'                                                 
         ASPEC H10,2,C'PERSON CODE AND NAME'                                    
         ASPEC H10,48,C' CLIENT       CLIENT       CLIENT'                      
         ASPEC H11,48,C'DIVISION       TIME         COST '                      
         ASPEC H10,86,C'  OTHERS       OTHERS  '                                
         ASPEC H11,86,C'   TIME         COST   '                                
         SPROG 3                                                                
         ASPEC H3,46,C'(CLIENT REPORTS)'                                        
         ASPEC H5,2,C'CLIENT'                                                   
         ASPEC H6,2,C'DIVISION'                                                 
         ASPEC H6,85,C'YEAR COMMENCING'                                         
         ASPEC H10,2,C'DEPARTMENT/PERSON'                                       
         ASPEC H10,37,C'ANNUAL'                                                 
         ASPEC H11,37,C'SALARY'                                                 
         SPROG 3,4                                                              
         ASPEC H10,46,C'HOURS    SALARY  FEE CHARGING DETAILS'                  
         ASPEC H11,56,C'COST'                                                   
         ASPEC H10,94,C'FEES'                                                   
         ASPEC H10,105,C' FEES'                                                 
         ASPEC H11,105,C'Y.T.D'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPFA01 08/17/00'                                      
         END                                                                    
