*          DATA SET ACREPXF01  AT LEVEL 005 AS OF 08/16/00                      
*PHASE ACXF01A,+0                                                               
         TITLE 'MAKE SUBREFERENCE UPPER CASE'                                   
ACXF01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANS                                                       
         ACDEF UPDATE,ACCFILE                                                   
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPXF01 08/16/00'                                      
         END                                                                    
