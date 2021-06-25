*          DATA SET ACREPB401  AT LEVEL 003 AS OF 08/16/00                      
*PHASE ACB401A,+0                                                               
         TITLE 'SPECS FOR BUDGET RECORD UTILITY'                                
ACB401   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         SPROG 0,1,2,3,4,6                                                      
         ASPEC H1,2,RUN                                                         
         ASPEC H1,46,C'BUDGET UPDATE'                                           
         ASPEC H1,83,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H2,46,13C'-'                                                     
         ASPEC H5,2,COMPANY                                                     
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPB401 08/16/00'                                      
         END                                                                    
