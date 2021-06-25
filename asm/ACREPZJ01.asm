*          DATA SET ACREPZJ01  AT LEVEL 002 AS OF 01/20/12                      
*PHASE ACZJ01B,+0                                                               
         TITLE 'ACZJ - COUNT TIMESHEET - SPECS'                                 
ACZJ01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
*                                                                               
         SPROG 1                                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,55,C'TIMESHEET COUNT REPORT'                                  
         ACDEF H2,55,C'----------------------'                                  
*        ACDEF H4,2,COMPANY                                                     
         ACDEF H1,101,REPORT                                                    
         ACDEF H1,122,PAGE                                                      
         ACDEF H2,101,REQUESTOR                                                 
*                                                                               
         ACDEF H11,3,C'TMS'                                                     
         ACDEF H12,3,C'LNE#'                                                    
         ACDEF H11,8,C'TEMPO'                                                   
         ACDEF H12,8,C'LINE#'                                                   
         ACDEF H11,14,C'PID'                                                    
         ACDEF H12,14,C'NUMBER'                                                 
         ACDEF H10,21,C'ACTVTY'                                                 
         ACDEF H11,21,C'DATE'                                                   
         ACDEF H10,28,C'ACTUAL'                                                 
*        ACDEF H11,28,C'PER STR'                                                
         ACDEF H12,28,C'DATE'                                                   
         ACDEF H10,36,C'ACTUAL'                                                 
*        ACDEF H11,36,C'PER END'                                                
         ACDEF H12,36,C'DATE'                                                   
         ACDEF H11,44,C'POSTING'                                                
         ACDEF H12,44,C'ACCOUNT'                                                
         ACDEF H11,60,C'REVERSAL'                                               
         ACDEF H12,60,C'NUMBER'                                                 
         ACDEF H11,75,C'TSK'                                                    
         ACDEF H12,75,C'CDE'                                                    
         ACDEF H11,79,C'TIM'                                                    
         ACDEF H12,79,C'TYP'                                                    
         ACDEF H12,83,C'RATE'                                                   
         ACDEF H11,93,C'INCOME/'                                                
         ACDEF H12,93,C'SUSPENSE'                                               
         ACDEF H12,107,C'OFF'                                                   
         ACDEF H12,111,C'HOURS - B'                                             
         ACDEF H12,123,C'HOURS - R'                                             
         ACDEF H12,135,C'HOURS - N'                                             
         ACDEF H12,147,C'HOURS - N/C'                                           
         ACDEF H10,159,C'TYPE'                                                  
         ACDEF H11,159,C'OF'                                                    
         ACDEF H12,159,C'T/S'                                                   
         ACDEF H12,166,C'CONTRA ACC'                                            
         ACDEF H12,180,C'MOA'                                                   
         ACDEF H12,189,C'DUP'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPZJ01 01/20/12'                                      
         END                                                                    
