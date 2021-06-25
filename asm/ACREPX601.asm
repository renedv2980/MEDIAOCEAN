*          DATA SET ACREPX601  AT LEVEL 010 AS OF 07/07/07                      
*PHASE ACX601A                                                                  
         TITLE 'CHECK TIME TOTAL RECORDS'                                       
ACX601   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
*                                                                               
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG 0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREPX601 07/07/07'                                      
         END                                                                    
