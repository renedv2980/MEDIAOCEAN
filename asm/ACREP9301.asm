*          DATA SET ACREP9301  AT LEVEL 002 AS OF 01/21/92                      
*PHASE AC9301A                                                                  
         TITLE 'PAYROLL RATE LISTING'                                           
AC9301   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         SPACE 1                                                                
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,94,REPORT                                                     
         ASPEC H1,108,PAGE                                                      
         ASPEC H3,94,REQUESTOR                                                  
         ASPEC H8,2,C'STAFF CODE AND NAME'                                      
         ASPEC H9,2,19C'-'                                                      
         ASPEC F1,2,REQDETS                                                     
         RSPEC MAXLINES,55                                                      
*                                                                               
         SPROG 0                                                                
         ASPEC H1,45,C'PAYROLL RATE LISTING'                                    
         ASPEC H2,45,C'--------------------'                                    
         ASPEC H8,48,C'TYPE'                                                    
         ASPEC H9,48,C'----'                                                    
         ASPEC H8,55,C'EFFECTIVE'                                               
         ASPEC H9,55,C' DATE(S)'                                                
         ASPEC H8,73,C'BASIS'                                                   
         ASPEC H9,73,C'-----'                                                   
         ASPEC H8,88,C'   RATE   '                                              
         ASPEC H9,88,C'----------'                                              
*                                                                               
         SPROG 1                                                                
         ASPEC H1,55,C'PAYROLL COST LISTING'                                    
         ASPEC H2,55,C'--------------------'                                    
         ASPEC H8,43,C' SALARY'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREP9301 01/21/92'                                      
         END                                                                    
