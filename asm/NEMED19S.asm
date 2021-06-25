*          DATA SET NEMED19S   AT LEVEL 002 AS OF 08/10/00                      
*PHASE T31E19A                                                                  
         TITLE 'T31E09 - SPECS FOR FEED LIST'                                   
T31E09   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,1,C'CODE'                                                     
         SSPEC H4,77,NETREP                                                     
         SSPEC H5,102,PAGE                                                      
         SSPEC H1,47,C'MARKET LISTING'                                          
         SSPEC H2,47,14C'-'                                                     
         SSPEC H4,1,C'GROUP'                                                    
         SPROG 2                                                                
         SSPEC H5,1,C'CLIENT'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED19S  08/10/00'                                      
         END                                                                    
