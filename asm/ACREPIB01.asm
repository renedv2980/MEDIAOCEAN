*          DATA SET ACREPIB01  AT LEVEL 002 AS OF 08/17/00                      
*PHASE ACIB01A                                                                  
         TITLE 'SPECS FOR COKE BUDGET INTERFACE'                                
ACIB01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,50,C'COKE BUDGET INTERFACE'                                   
         ASPEC H1,81,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H2,50,21C'-'                                                     
         ASPEC H3,2,COMPANY                                                     
         ASPEC H8,3,C'ACN'                                                      
         ASPEC H8,7,C'AGCY'                                                     
         ASPEC H8,28,C'NAME'                                                    
         ASPEC H8,50,C'PD'                                                      
         ASPEC H8,53,C'MD'                                                      
         ASPEC H8,58,C'PREVIOUS'                                                
         ASPEC H8,74,C'INPUT'                                                   
         ASPEC H8,87,C'CURRENT'                                                 
         ASPEC H8,101,C'CHANGE'                                                 
         ASPEC H9,50,C'CD'                                                      
         ASPEC H9,53,C'CD'                                                      
         ASPEC H9,59,C'BUDGET'                                                  
         ASPEC H9,75,C'FILE'                                                    
         ASPEC H9,88,C'BUDGET'                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPIB01 08/17/00'                                      
         END                                                                    
