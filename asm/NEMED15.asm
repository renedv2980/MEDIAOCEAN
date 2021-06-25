*          DATA SET NEMED15    AT LEVEL 007 AS OF 08/10/00                      
*PHASE T31E15A                                                                  
         TITLE 'T31E15 - SPECS FOR NETWORK CALENDAR'                            
T31E15   CSECT                                                                  
         SPROG 0,1                                                              
         WSPEC H1,3,C'NETWORK CALENDAR'                                         
         WSPEC H2,3,16C'-'                                                      
         WSPEC H1,129,AGYNAME                                                   
         WSPEC H2,129,AGYADD                                                    
         WSPEC H4,129,NETREP                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007NEMED15   08/10/00'                                      
         END                                                                    
