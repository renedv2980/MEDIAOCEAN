*          DATA SET ACREPX701  AT LEVEL 006 AS OF 08/16/00                      
*PHASE ACX701A                                                                  
         TITLE 'FIX 51 ELEMENTS FOR 1R TRANS'                                   
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
**PAN#1  DC    CL21'006ACREPX701 08/16/00'                                      
         END                                                                    
