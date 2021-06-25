*          DATA SET ACREPZD01  AT LEVEL 051 AS OF 01/03/00                      
*PHASE ACZD01A,*                                                                
         TITLE 'ACZD01 - MONACC MODES AND WHAT THEY PASS'                       
ACZD01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         FSPEC READ,TRANS                                                       
         FSPEC READ,TIME                                                        
         ACDEF WIDTH,198                                                        
*                                                                               
         SPROG 0,1                                                              
         ACDEF H1,2,RUN                                                         
         ACDEF H1,88,REPORT                                                     
         ACDEF H1,102,PAGE                                                      
         ACDEF H3,88,REQUESTOR                                                  
         ACDEF H3,2,ORIGIN                                                      
*                                                                               
         SPROG 1                                                                
         ACDEF H1,45,C'CHECK B7 TRANSACTIONS'                                   
         ACDEF H2,45,C'---------------------'                                   
         ACDEF H8,02,C'ACCOUNT'                                                 
         ACDEF H9,02,C'-------'                                                 
         ACDEF H8,17,C'W/C'                                                     
         ACDEF H9,17,C'---'                                                     
         ACDEF H8,21,C'CONTRA'                                                  
         ACDEF H9,21,C'------'                                                  
         ACDEF H8,39,C'DATE    REF'                                             
         ACDEF H9,39,C'----    ---'                                             
         ACDEF H8,60,C'DESCRIPTION'                                             
         ACDEF H9,60,C'-----------'                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051ACREPZD01 01/03/00'                                      
         END                                                                    
