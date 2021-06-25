*          DATA SET NEMED56    AT LEVEL 005 AS OF 08/10/00                      
*PHASE T31E56A                                                                  
         TITLE 'T31E56 - SPECS FOR POST-BUY'                                    
*  SPROG 1 - POST-BUY EVALUATION                                                
*  SPROG 2 - POST-BUY SUMMARY                                                   
T31E56   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 1,2                                                              
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,75,NETREP                                                     
         SSPEC H5,75,C'NETWORK - '                                              
         SSPEC H5,102,PAGE                                                      
         SPROG 1                                                                
         SSPEC H1,44,C'POST-BUY EVALUATION'                                     
         SSPEC H2,44,C'-------------------'                                     
         SPROG 2                                                                
         SSPEC H1,46,C'POST-BUY SUMMARY'                                        
         SSPEC H2,46,C'----------------'                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEMED56   08/10/00'                                      
         END                                                                    
