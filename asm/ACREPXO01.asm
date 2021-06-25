*          DATA SET ACREPXO01  AT LEVEL 005 AS OF 08/16/00                      
*PHASE ACXO01A                                                                  
         TITLE 'COMPARE/FIX ORDERS'                                             
*                                                                               
ACXO01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ORDERS                                                      
         ACDEF RESET                                                            
         ACDEF GETOPT,N                                                         
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,35,C'ORDER MATCH REPORT'                                      
         ACDEF H2,35,C'------------------'                                      
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,C'COMPANY'                                                  
         ACDEF H4,83,REQUESTOR                                                  
*                                                                               
         ACDEF H09,2,C'ORDER'                                                   
         ACDEF H10,2,C'-----'                                                   
*                                                                               
         ACDEF H09,10,C'  DATE  '                                               
         ACDEF H10,10,C'--------'                                               
*                                                                               
         ACDEF H09,20,C'     JOB    '                                           
         ACDEF H10,20,C'------------'                                           
*                                                                               
         ACDEF H09,34,C'    ORDER STATUS    '                                   
         ACDEF H10,34,C'--------------------'                                   
*                                                                               
         ACDEF H09,56,C'     JOB  STATUS    '                                   
         ACDEF H10,56,C'--------------------'                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPXO01 08/16/00'                                      
         END                                                                    
