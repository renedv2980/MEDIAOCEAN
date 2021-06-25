*          DATA SET ACREPTS01  AT LEVEL 013 AS OF 01/19/01                      
*PHASE ACTS01A,+0                                                               
         TITLE 'ACTS - TIMESHEET SUMMARY REPORT'                                
ACTS01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         ACDEF READ,RECOVERY                                                    
*                                                                               
         SPROG 1                                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,55,C'TIMESHEET SUMMARY REPORT'                                
         ACDEF H2,55,C'------------------------'                                
*        ACDEF H4,2,COMPANY                                                     
         ACDEF H1,101,REPORT                                                    
         ACDEF H1,122,PAGE                                                      
         ACDEF H2,101,REQUESTOR                                                 
*                                                                               
         ACDEF H11,3,C'TMS'                                                     
         ACDEF H12,3,C'LNE#'                                                    
         ACDEF H11,8,C'TEMPO'                                                   
         ACDEF H12,8,C'LINE#'                                                   
         ACDEF H11,17,C'PID'                                                    
         ACDEF H12,15,C'NUMBER'                                                 
         ACDEF H10,23,C'ACTUAL'                                                 
         ACDEF H11,23,C'PER STR'                                                
         ACDEF H12,23,C'DATE'                                                   
         ACDEF H10,31,C'ACTUAL'                                                 
         ACDEF H11,31,C'PER END'                                                
         ACDEF H12,31,C'DATE'                                                   
         ACDEF H12,39,C'CLI'                                                    
         ACDEF H12,44,C'PRD'                                                    
         ACDEF H12,49,C'JOB'                                                    
         ACDEF H12,57,C'1N ACCOUNT'                                             
         ACDEF H11,71,C'TSK'                                                    
         ACDEF H12,71,C'CDE'                                                    
         ACDEF H11,75,C'TIM'                                                    
         ACDEF H12,75,C'TYP'                                                    
         ACDEF H12,80,C'RATE'                                                   
         ACDEF H11,89,C'INCOME/'                                                
         ACDEF H12,89,C'SUSPENSE'                                               
         ACDEF H12,103,C'OFF'                                                   
         ACDEF H12,110,C'HOURS'                                                 
         ACDEF H10,120,C'TYPE'                                                  
         ACDEF H11,121,C'OF'                                                    
         ACDEF H12,120,C'T/S'                                                   
         ACDEF H12,126,C'CONTRA ACC'                                            
         ACDEF H12,142,C'MOA'                                                   
         ACDEF H12,149,C'DUP'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPTS01 01/19/01'                                      
         END                                                                    
