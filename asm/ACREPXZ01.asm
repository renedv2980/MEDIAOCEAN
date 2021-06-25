*          DATA SET ACREPXZ01  AT LEVEL 005 AS OF 07/07/07                      
*PHASE ACXZ01A                                                                  
         TITLE 'SCREEN FIELD REPORT'                                            
ACXZ01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'SCREEN FIELD REPORT'                                     
         ACDEF H2,38,C'-------------------'                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
*                                                                               
         ACDEF H09,2,C'TYPE'                                                    
         ACDEF H10,2,C'----'                                                    
         ACDEF H09,7,C'FIELD'                                                   
         ACDEF H10,7,C'-----'                                                   
         ACDEF H09,13,C'ATTRIBUTES'                                             
         ACDEF H10,13,C'----------'                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPXZ01 07/07/07'                                      
         END                                                                    
