*          DATA SET ACREPCH01  AT LEVEL 002 AS OF 10/05/92                      
*PHASE ACCH01A                                                                  
         TITLE 'SPECS COST RATES'                                               
ACCH01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF RESET                                                            
*                                                                               
         SPROG 0,1                                                              
         ACDEF H1,2,RUN                                                         
         ACDEF H4,2,COMPANY                                                     
         ACDEF H5,2,C'OFFICE'                                                   
         ACDEF H6,2,C'DEPT'                                                     
         ACDEF H7,2,C'SUB DEPT'                                                 
         ACDEF H1,57,C'COSTING HOURLY RATES'                                    
         ACDEF H2,57,C'--------------------'                                    
         ACDEF H1,96,REPORT                                                     
         ACDEF H2,96,REQUESTOR                                                  
         ACDEF H3,96,PERIOD                                                     
         ACDEF H1,110,PAGE                                                      
*                                                                               
         ACDEF H10,2,C'EMPLOYEE CODE/NAME'                                      
         ACDEF H11,2,C'------------------'                                      
         ACDEF H10,52,C'SALARY'                                                 
         ACDEF H11,52,C'------'                                                 
         ACDEF H10,60,C'BENEFIT'                                                
         ACDEF H11,60,C'-------'                                                
         ACDEF H10,69,C'PENSION'                                                
         ACDEF H11,69,C'-------'                                                
         ACDEF H10,116,C'TOTAL  '                                               
         ACDEF H11,116,C'-----  '                                               
*                                                                               
         SPROG 0                                                                
         ACDEF H10,43,C'METHOD'                                                 
         ACDEF H11,43,C'------'                                                 
*                                                                               
         SPROG 1                                                                
         ACDEF H5,96,C'ALLOCATION METHOD'                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPCH01 10/05/92'                                      
         END                                                                    
