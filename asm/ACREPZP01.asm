*          DATA SET ACREPZP01  AT LEVEL 005 AS OF 08/16/00                      
*PHASE ACZP01A                                                                  
         TITLE 'CREATE PERSON/SALARY RECORDS'                                   
ACZP01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF READ,ACCOUNTS                                                    
         ACDEF UPDATE,ACCFIL                                                    
*        ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,135,REPORT                                                    
         ACDEF H1,150,PAGE                                                      
         ACDEF H2,2,COMPANY                                                     
         ACDEF H2,135,REQUESTOR                                                 
         ACDEF H3,135,MOSFILT                                                   
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,76,C'PERSON RECORD CREATION'                                  
         ACDEF H2,76,C'----------------------'                                  
         ACDEF H7,1,C'NEW ACCOUNT'                                              
         ACDEF H8,1,C'-----------'                                              
         ACDEF H7,16,C'OLD ACCOUNT'                                             
         ACDEF H8,16,C'-----------'                                             
         ACDEF H7,34,C'LAST NAME'                                               
         ACDEF H8,34,C'-------------------------------'                         
         ACDEF H7,67,C'FIRST NAME'                                              
         ACDEF H8,67,C'-------------------------------'                         
         ACDEF H6,104,C'  HIRE  '                                               
         ACDEF H7,104,C'  DATE  '                                               
         ACDEF H8,104,C'--------'                                               
         ACDEF H6,116,C'  TERM  '                                               
         ACDEF H7,116,C'  DATE  '                                               
         ACDEF H8,116,C'--------'                                               
         ACDEF H6,128,C'LOCATION'                                               
         ACDEF H7,128,C' START  '                                               
         ACDEF H8,128,C'--------'                                               
         ACDEF H6,138,C'LOCATION'                                               
         ACDEF H7,138,C'  END   '                                               
         ACDEF H8,138,C'--------'                                               
         ACDEF H7,150,C'ACTION'                                                 
         ACDEF H8,150,C'------'                                                 
         ACDEF H6,158,C' TSLOCK '                                               
         ACDEF H7,158,C'  DATE  '                                               
         ACDEF H8,158,C'--------'                                               
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,72,C'SALARY HISTORY RECORD CREATION'                          
         ACDEF H2,72,C'------------------------------'                          
         ACDEF H7,1,C'NEW ACCOUNT'                                              
         ACDEF H8,1,C'-----------'                                              
         ACDEF H7,27,C'SALARY DATE'                                             
         ACDEF H8,27,C'-----------'                                             
         ACDEF H7,49,C'TYPE'                                                    
         ACDEF H8,49,C'----'                                                    
         ACDEF H7,60,C'CHECK DATE'                                              
         ACDEF H8,60,C'----------'                                              
         ACDEF H7,88,C'AMOUNT'                                                  
         ACDEF H8,88,C'------'                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPZP01 08/16/00'                                      
         END                                                                    
