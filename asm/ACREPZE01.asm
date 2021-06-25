*          DATA SET ACREPZE01  AT LEVEL 026 AS OF 11/26/00                      
*PHASE ACZE01A,+0                                                               
         TITLE 'ACZE - COUNT TIMESHEET - SPECS'                                 
ACZE01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TIME                                                        
*                                                                               
         SPROG 1,2,3                                                            
         ACDEF H1,2,RUN                                                         
         ACDEF H1,55,C'TIMESHEET COUNT REPORT'                                  
         ACDEF H2,55,C'----------------------'                                  
         ACDEF H3,56,PERIOD                                                     
         ACDEF H4,2,COMPANY                                                     
         ACDEF H1,101,REPORT                                                    
         ACDEF H1,122,PAGE                                                      
         ACDEF H2,101,REQUESTOR                                                 
*                                                                               
         SPROG 1                                                                
         ACDEF H11,1,C'D'                                                       
         ACDEF H12,1,C'--'                                                      
         ACDEF H11,5,C'#'                                                       
         ACDEF H12,5,C'-'                                                       
         ACDEF H11,7,C'CODE'                                                    
         ACDEF H12,7,C'----'                                                    
         ACDEF H11,18,C'NAME'                                                   
         ACDEF H12,18,C'----'                                                   
         ACDEF H11,44,C'PERIOD'                                                 
         ACDEF H12,44,C'------'                                                 
         ACDEF H11,53,C'TYP'                                                    
         ACDEF H12,53,C'---'                                                    
         ACDEF H11,57,C'MOA'                                                    
         ACDEF H12,57,C'---'                                                    
         ACDEF H11,65,C'HOURS'                                                  
         ACDEF H12,65,C'------'                                                 
         ACDEF H11,73,C'AMOUNT'                                                 
         ACDEF H12,73,C'--------'                                               
         ACDEF H11,84,C'TMS#'                                                   
         ACDEF H12,84,C'----'                                                   
         ACDEF H11,90,C'TEMPO#'                                                 
         ACDEF H12,90,C'------'                                                 
         ACDEF H11,99,C'CONTRA ACC'                                             
         ACDEF H12,99,C'--------------'                                         
         ACDEF H11,115,C'INCOME ACC'                                            
         ACDEF H12,115,C'--------------'                                        
         ACDEF H11,131,C'WC'                                                    
         ACDEF H12,131,C'--'                                                    
         ACDEF H11,134,C'OF'                                                    
         ACDEF H12,134,C'--'                                                    
         ACDEF H11,139,C'ACTIVITY'                                              
         ACDEF H12,139,C'--------'                                              
         ACDEF H11,149,C'MSG'                                                   
         ACDEF H12,149,C'----------------'                                      
         SPROG 2                                                                
         ACDEF H11,2,C'TOTALS FOR COMPANY'                                      
         ACDEF H12,2,C'------------------'                                      
         SPROG 3                                                                
         ACDEF H11,2,C'TOTALS FOR ACCFILE'                                      
         ACDEF H12,2,C'------------------'                                      
         SPROG 2,3                                                              
         ACDEF H11,40,C'B-TIME'                                                 
         ACDEF H12,40,C'------------'                                           
         ACDEF H11,54,C'N-TIME'                                                 
         ACDEF H12,54,C'------------'                                           
         ACDEF H11,68,C'R-TIME'                                                 
         ACDEF H12,68,C'------------'                                           
         ACDEF H11,82,C'C-TIME'                                                 
         ACDEF H12,82,C'------------'                                           
         ACDEF H11,96,C'TOTAL TIME'                                             
         ACDEF H12,96,C'------------'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026ACREPZE01 11/26/00'                                      
         END                                                                    
