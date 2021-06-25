*          DATA SET ACREPZT01  AT LEVEL 023 AS OF 02/18/20                      
*PHASE ACZT01B,+0                                                               
         TITLE 'TIME SHEET LINE NUM. PROBLEM FIX FOR TEMPO'                     
ACZT01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0,1,2,3,4                                                  
         ACDEF H1,2,RUN                                                         
         ACDEF H1,55,C'TIMESHEET/TEMPO LINE NO. ERR REPORT'                     
         ACDEF H2,55,C'-----------------------------------'                     
         ACDEF H3,56,PERIOD                                                     
         ACDEF H1,101,REPORT                                                    
         ACDEF H1,122,PAGE                                                      
         ACDEF H2,101,REQUESTOR                                                 
*                                                                               
         SPROG 0,1,3,4                                                          
         ACDEF H11,2,C'CODE'                                                    
         ACDEF H12,2,C'----'                                                    
         ACDEF H11,10,C'NAME'                                                   
         ACDEF H12,10,C'----'                                                   
         ACDEF H11,50,C'TS END DTE'                                             
         ACDEF H12,50,C'----------'                                             
         ACDEF H11,62,C'TS STT DTE'                                             
         ACDEF H12,62,C'----------'                                             
         ACDEF H11,73,C'TMS LIN#'                                               
         ACDEF H12,73,C'--------'                                               
         ACDEF H11,84,C'HOURS'                                                  
         ACDEF H12,84,C'-----'                                                  
         ACDEF H11,96,C'DAY 1'                                                  
         ACDEF H12,96,C'-----'                                                  
         ACDEF H11,106,C'DAY 2'                                                 
         ACDEF H12,106,C'-----'                                                 
         ACDEF H11,116,C'DAY 3'                                                 
         ACDEF H12,116,C'-----'                                                 
         ACDEF H11,126,C'DAY 4'                                                 
         ACDEF H12,126,C'-----'                                                 
         ACDEF H11,136,C'DAY 5'                                                 
         ACDEF H12,136,C'-----'                                                 
         ACDEF H11,146,C'DAY 6'                                                 
         ACDEF H12,146,C'-----'                                                 
         ACDEF H11,156,C'DAY 7'                                                 
         ACDEF H12,156,C'-----'                                                 
         ACDEF H11,166,C'DA   '                                                 
         ACDEF H12,166,C'-----'                                                 
*                                                                               
         SPROG 2                                                                
         ACDEF H11,2,C'CODE'                                                    
         ACDEF H12,2,C'----'                                                    
         ACDEF H11,14,C'ACCOUNT'                                                
         ACDEF H12,14,C'-------'                                                
         ACDEF H11,30,C'TS END DTE'                                             
         ACDEF H12,30,C'----------'                                             
         ACDEF H11,50,C'TMS LIN# (INPUT)'                                       
         ACDEF H12,50,C'----------------'                                       
         ACDEF H11,70,C'HOURS (INPUT)'                                          
         ACDEF H12,70,C'-------------'                                          
         ACDEF H11,90,C'TMS LIN# (TIMETIME)'                                    
         ACDEF H12,90,C'-------------------'                                    
         ACDEF H11,110,C'HOURS (TIMETIME)'                                      
         ACDEF H12,110,C'----------------'                                      
         ACDEF H11,130,C'DA   '                                                 
         ACDEF H12,130,C'-----'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACREPZT01 02/18/20'                                      
         END                                                                    
