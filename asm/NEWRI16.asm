*          DATA SET NEWRI16    AT LEVEL 006 AS OF 08/10/00                      
*PHASE T32016A                                                                  
         TITLE 'T32016 - SPECS FOR OVERNIGHTS'                                  
T32016   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3                                                          
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,53,PERIOD                                                     
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,124,PAGE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEWRI16   08/10/00'                                      
         END                                                                    
