*          DATA SET ACREPCU01  AT LEVEL 013 AS OF 05/25/94                      
*PHASE ACCU01A,+0                                                               
         TITLE 'SALARY REPORT'                                                  
ACCU01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MAXLINES,56                                                      
         ACDEF READ,ACCOUNTS                                                    
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15                      
         ACDEF H1,1,RUN,WIDE=198                                                
         ACDEF H1,115,PAGE,WIDE=198                                             
         ACDEF H2,1,C'COMPANY',WIDE=198                                         
         ACDEF H3,115,C'REPORT ACCU',WIDE=198                                   
         ACDEF H3,130,REQUESTOR,WIDE=198                                        
         ACDEF H4,130,PERIOD,WIDE=198                                           
*                                                                               
*              REGULAR REPORT                                                   
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5,6,7,8,9                                        
         ACDEF H1,62,C'SALARY HISTORY REPORT',WIDE=198                          
         ACDEF H2,62,C'---------------------',WIDE=198                          
         ACDEF H9,70,C'SALARY',WIDE=198                                         
*                                                                               
         ACDEF SPROG,0,2,4,6,8                                                  
         ACDEF H9,03,C'PERSON',WIDE=198                                         
         ACDEF H9,11,C'NAME (LAST, FIRST M.I.)',WIDE=198                        
         ACDEF H8,44,C'CHECK',WIDE=198                                          
         ACDEF H9,44,C'DATE ',WIDE=198                                          
         ACDEF H8,54,C'PAYROLL',WIDE=198                                        
         ACDEF H9,54,C' CODE  ',WIDE=198                                        
*                                                                               
         ACDEF SPROG,0,1,11                                                     
         ACDEF H9,86,C'PENSION',WIDE=198                                        
         ACDEF H9,103,C'BENEFIT',WIDE=198                                       
         ACDEF H9,119,C'INDIRECT',WIDE=198                                      
         ACDEF H9,139,C'TOTAL',WIDE=198                                         
*                                                                               
         ACDEF SPROG,2,3,12                                                     
         ACDEF H9,86,C'PENSION',WIDE=198                                        
         ACDEF H9,103,C'BENEFIT',WIDE=198                                       
         ACDEF H9,119,C'INDIRECT',WIDE=198                                      
         ACDEF H9,139,C'OTHER',WIDE=198                                         
         ACDEF H9,156,C'TOTAL',WIDE=198                                         
*                                                                               
         ACDEF SPROG,4,5,13                                                     
         ACDEF H9,86,C'PENSION',WIDE=198                                        
         ACDEF H9,103,C'BENEFIT',WIDE=198                                       
         ACDEF H9,122,C'TOTAL',WIDE=198                                         
*                                                                               
         ACDEF SPROG,6,7,14                                                     
         ACDEF H9,86,C'PENSION',WIDE=198                                        
         ACDEF H9,105,C'TOTAL',WIDE=198                                         
*                                                                               
         ACDEF SPROG,10                                                         
         ACDEF H1,62,C'PAYROLL RATE LISTING',WIDE=198                           
         ACDEF H2,62,C'--------------------',WIDE=198                           
         ACDEF H9,03,C'STAFF CODE AND NAME',WIDE=198                            
         ACDEF H9,44,C'TYPE',WIDE=198                                           
         ACDEF H8,54,C'EFFECTIVE',WIDE=198                                      
         ACDEF H9,54,C' DATE(S)',WIDE=198                                       
         ACDEF H9,70,C'BASIS',WIDE=198                                          
         ACDEF H9,90,C'RATE',WIDE=198                                           
*                                                                               
         ACDEF SPROG,11,12,13,14,15                                             
         ACDEF H1,62,C'OVERHEAD ACCT LISTING',WIDE=198                          
         ACDEF H2,62,C'---------------------',WIDE=198                          
         ACDEF H9,03,C'OVERHEAD ACCOUNT   ',WIDE=198                            
         ACDEF H9,70,C'SALARY',WIDE=198                                         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPCU01 05/25/94'                                      
         END                                                                    
