*          DATA SET ACREPZ201  AT LEVEL 007 AS OF 08/16/00                      
*PHASE ACZ201A                                                                  
         TITLE 'FIX MOA IN TRANSACTIONS'                                        
ACZ201   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MAXLINES,56                                                      
         ACDEF UPDATE,ACCFILE                                                   
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,1,RUN                                                         
         ACDEF H1,100,PAGE                                                      
         ACDEF H2,1,COMPANY                                                     
         ACDEF H2,100,C'REPORT ACZ2'                                            
         ACDEF H3,100,REQUESTOR                                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPZ201 08/16/00'                                      
         END                                                                    
