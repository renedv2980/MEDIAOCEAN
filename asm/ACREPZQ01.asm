*          DATA SET ACREPZQ01  AT LEVEL 013 AS OF 03/28/14                      
*PHASE ACZQ01A                                                                  
         TITLE 'FIX MOA IN TMS NON-BILL'                                        
ACZQ01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF MAXLINES,56                                                      
         ACDEF UPDATE,ACCFILE                                                   
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,1,RUN                                                         
         ACDEF H1,100,PAGE                                                      
         ACDEF H2,1,COMPANY                                                     
         ACDEF H2,100,C'REPORT ACZQ'                                            
         ACDEF H3,100,REQUESTOR                                                 
*                                                                               
         ACDEF H8,001,C'TYPE'                                                   
         ACDEF H8,007,C'LINE #'                                                 
         ACDEF H8,014,C'1R ACCOUNT'                                             
         ACDEF H8,030,C'CONTRA ACCOUNT'                                         
         ACDEF H8,046,C'P-END DATE'                                             
         ACDEF H8,064,C'HOURS'                                                  
         ACDEF H8,072,C'MOA'                                                    
         ACDEF H8,079,C'TIME TYPE'                                              
         ACDEF H8,096,C'OFF'                                                    
         ACDEF H8,100,C'NEW MOA'                                                
         ACDEF H8,110,C'MSGS'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPZQ01 03/28/14'                                      
         END                                                                    
