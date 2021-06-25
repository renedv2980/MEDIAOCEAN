*          DATA SET ACREPZK01  AT LEVEL 049 AS OF 03/29/01                      
*PHASE ACZK01A,+0                                                               
         TITLE 'ACZK - TIMESHEET SUMMARY REPORT'                                
ACZK01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
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
         ACDEF H10,15,C'ORIG'                                                   
         ACDEF H11,15,C'TEMPO'                                                  
         ACDEF H12,15,C'LINE#'                                                  
         ACDEF H11,24,C'PID'                                                    
         ACDEF H12,22,C'NUMBER'                                                 
         ACDEF H10,30,C'ACTUAL'                                                 
         ACDEF H11,30,C'PER STR'                                                
         ACDEF H12,30,C'DATE'                                                   
         ACDEF H10,38,C'ACTUAL'                                                 
         ACDEF H11,38,C'PER END'                                                
         ACDEF H12,38,C'DATE'                                                   
         ACDEF H12,46,C'CLI'                                                    
         ACDEF H12,51,C'PRD'                                                    
         ACDEF H12,56,C'JOB'                                                    
         ACDEF H12,64,C'1N ACCOUNT'                                             
         ACDEF H11,78,C'TSK'                                                    
         ACDEF H12,78,C'CDE'                                                    
         ACDEF H11,82,C'TIM'                                                    
         ACDEF H12,82,C'TYP'                                                    
         ACDEF H12,87,C'RATE'                                                   
         ACDEF H11,96,C'INCOME/'                                                
         ACDEF H12,96,C'SUSPENSE'                                               
         ACDEF H12,110,C'OFF'                                                   
         ACDEF H12,117,C'HOURS'                                                 
         ACDEF H10,127,C'TYPE'                                                  
         ACDEF H11,128,C'OF'                                                    
         ACDEF H12,127,C'T/S'                                                   
         ACDEF H12,133,C'CONTRA ACC'                                            
         ACDEF H12,149,C'MOA'                                                   
         ACDEF H12,156,C'ERROR'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049ACREPZK01 03/29/01'                                      
         END                                                                    
