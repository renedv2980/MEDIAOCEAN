*          DATA SET ACREPAB01  AT LEVEL 002 AS OF 03/24/20                      
*PHASE ACAB01C,+0                                                               
         TITLE 'ACAB - AUTO APPROVE PAYABLES BY VENDOR'                         
ACAB01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         DC    AL1(10)                                                          
         DC    AL1(03)                                                          
         DC    AL1(22)             READ TRANSACTION DIRECTORY                   
*                                                                               
         SPROG 0,1,2,3,4,5                                                      
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,C'AUTO APPROVE BY VENDOR'                                  
         ACDEF H2,86,C'----------------------'                                  
         ACDEF H4,2,COMPANY                                                     
         ACDEF H1,160,REPORT                                                    
         ACDEF H1,180,PAGE                                                      
         ACDEF H2,160,REQUESTOR                                                 
*                                                                               
         SPROG 5                                                                
         ACDEF H8,2,C'**ERROR** INCONSISTENT WITH PROFILE'                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPAB01 03/24/20'                                      
         END                                                                    
