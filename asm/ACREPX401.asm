*          DATA SET ACREPX401  AT LEVEL 028 AS OF 02/08/01                      
*PHASE ACX401A                                                                  
         TITLE 'PUT PIDELS ON TEMPO XREF RECORDS'                               
ACX401   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0,1,2,3,4                                                  
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H8,2,C'PERSON CODE'                                              
         ACDEF H9,2,C'-----------'                                              
         ACDEF H8,22,C'PERSONAL ID#'                                            
         ACDEF H9,22,C'------------'                                            
         ACDEF H8,42,C'OLD PERSONAL ID#'                                        
         ACDEF H9,42,C'----------------'                                        
         ACDEF H8,80,C'  DISK ADDRESS  '                                        
         ACDEF H9,80,C'----------------'                                        
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H8,6,C'PERSON CODE'                                              
         ACDEF H9,6,C'-----------'                                              
         ACDEF H8,19,C'OFF/DPT/SUB'                                             
         ACDEF H9,19,C'-----------'                                             
         ACDEF H8,32,C'P ENDDATE'                                               
         ACDEF H9,32,C'---------'                                               
         ACDEF H8,43,C'OLD PID'                                                 
         ACDEF H9,43,C'-------'                                                 
         ACDEF H8,53,C'NEW PID'                                                 
         ACDEF H9,53,C'-------'                                                 
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H8,6,C'PERSON CODE'                                              
         ACDEF H9,6,C'-----------'                                              
         ACDEF H8,19,C'1R ACCOUNT'                                              
         ACDEF H9,19,C'----------'                                              
         ACDEF H8,38,C'OFFICE'                                                  
         ACDEF H9,38,C'------'                                                  
         ACDEF H8,45,C'CONTRA'                                                  
         ACDEF H9,45,C'------'                                                  
         ACDEF H8,64,C'P ENDDATE'                                               
         ACDEF H9,64,C'---------'                                               
         ACDEF H8,75,C'OLD PID'                                                 
         ACDEF H9,75,C'-------'                                                 
         ACDEF H8,85,C'NEW PID'                                                 
         ACDEF H9,85,C'-------'                                                 
*                                                                               
         ACDEF SPROG,4                                                          
         ACDEF H8,6,C'PERSON CODE'                                              
         ACDEF H9,6,C'-----------'                                              
         ACDEF H8,19,C'OLD PID'                                                 
         ACDEF H9,19,C'-------'                                                 
         ACDEF H8,29,C'NEW PID'                                                 
         ACDEF H9,29,C'-------'                                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028ACREPX401 02/08/01'                                      
         END                                                                    
