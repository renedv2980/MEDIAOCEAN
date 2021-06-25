*          DATA SET ACREPB501  AT LEVEL 001 AS OF 03/25/93                      
*PHASE ACB501A,+0                                                               
         TITLE 'WORKER/BUDGET REPORT'                                           
ACB501   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MAXLINES,56                                                      
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANS                                                       
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,1,RUN,WIDE=198                                                
         ACDEF H1,130,C'REPORT ACB5',WIDE=198                                   
         ACDEF H1,145,PAGE,WIDE=198                                             
         ACDEF H2,1,C'COMPANY',WIDE=198                                         
*                                                                               
*              QOPT1=W WORKER BUDGET UPLOAD REPORT                              
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,62,C'BUDGET UPDATES REPORT',WIDE=198                          
         ACDEF H2,62,C'---------------------',WIDE=198                          
         ACDEF H9,03,C'ACCOUNT',WIDE=198                                        
         ACDEF H9,48,C'CONTRA ACCOUNT',WIDE=198                                 
         ACDEF H9,94,C'DATE ',WIDE=198                                          
         ACDEF H9,104,C' AMOUNT ',WIDE=198                                      
         ACDEF H9,119,C'ACTION',WIDE=198                                        
         ACDEF H9,132,C'PREVIOUS',WIDE=198                                      
*                                                                               
*              QOPT1=W ERROR REPORT                                             
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,59,C'BUDGET UPDATES ERROR REPORT',WIDE=198                    
         ACDEF H2,59,C'---------------------------',WIDE=198                    
         ACDEF H9,03,C'ACCOUNT',WIDE=198                                        
         ACDEF H9,48,C'CONTRA ACCOUNT',WIDE=198                                 
         ACDEF H9,93,C'ERROR',WIDE=198                                          
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPB501 03/25/93'                                      
         END                                                                    
