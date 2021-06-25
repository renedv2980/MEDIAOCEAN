*          DATA SET ACREPCD01  AT LEVEL 008 AS OF 07/09/07                      
*PHASE ACCD01A,+0                                                               
         TITLE 'SPECS FOR COST DUMP RECOVERY'                                   
ACCD01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
                                                                                
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,83,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,83,REQUESTOR                                                  
         ASPEC H1,37,C'CA COST DUMP RECOVERY'                                   
         ASPEC H2,37,C'---------------------'                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPCD01 07/09/07'                                      
         END                                                                    
