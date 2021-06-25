*          DATA SET NEMED55    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T31E55A                                                                  
         TITLE 'T31E55 - SPECS FOR EVALUATION SUMMARY'                          
T31E55   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,41,C'NETWORK EVALUATION SUMMARY'                              
         SSPEC H2,41,26C'-'                                                     
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,76,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,76,NETREP                                                     
         SSPEC H6,76,C'ALL DAYPARTS'                                            
         SSPEC H6,100,PAGE                                                      
         SSPEC H10,1,C'NETWORK PACKAGE'                                         
         SSPEC H11,1,C'------- -------'                                         
         SSPEC H10,29,C'COST'                                                   
         SSPEC H11,28,C'UNITS'                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED55   08/10/00'                                      
         END                                                                    
