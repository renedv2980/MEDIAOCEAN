*          DATA SET NEMED54    AT LEVEL 003 AS OF 10/27/10                      
*PHASE T31E54A                                                                  
         TITLE 'T31E54 - SPECS FOR PACKAGE EVALUATION'                          
T31E54   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3                                                          
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H5,102,PAGE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003NEMED54   10/27/10'                                      
         END                                                                    
