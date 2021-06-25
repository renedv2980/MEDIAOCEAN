*          DATA SET ACREPZ301  AT LEVEL 008 AS OF 08/16/00                      
*PHASE ACZ301A                                                                  
         TITLE 'FIND MISSING 1R CONTRAS'                                        
ACZ301   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MAXLINES,56                                                      
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,1,RUN                                                         
         ACDEF H1,100,PAGE                                                      
         ACDEF H2,1,COMPANY                                                     
         ACDEF H2,100,C'REPORT AC32'                                            
         ACDEF H3,100,REQUESTOR                                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPZ301 08/16/00'                                      
         END                                                                    
