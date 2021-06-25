*          DATA SET ACREPX501  AT LEVEL 006 AS OF 08/16/00                      
*PHASE ACX501A                                                                  
         TITLE 'FIND SJ RCDS WITH BAD TYPE 48 ELEMENTS'                         
ACX501   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,ACCFILE                                                   
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H1,36,C'BADS 48 SCAN'                                            
         ACDEF H2,36,C'------------'                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPX501 08/16/00'                                      
         END                                                                    
