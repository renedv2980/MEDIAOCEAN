*          DATA SET NEMED16    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T31E16A                                                                  
         TITLE 'T21D83 - SPECS FOR OVERNIGHTS'                                  
T31E16   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3                                                          
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,77,NETREP                                                     
         SSPEC H5,102,PAGE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED16   08/10/00'                                      
         END                                                                    
