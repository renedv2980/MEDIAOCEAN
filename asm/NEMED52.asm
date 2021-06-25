*          DATA SET NEMED52    AT LEVEL 006 AS OF 08/10/00                      
*PHASE T31E52A                                                                  
         TITLE 'T31E52 - SPECS FOR HUTLIST'                                     
T31E52   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V'                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,51,C'MONTHLY HUT LISTING'                                     
         SSPEC H2,51,C'-------------------'                                     
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,1,C'SOURCE    NTI'                                            
         SSPEC H4,51,C'DAY=M-F   YEAR=1980'                                     
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,99,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEMED52   08/10/00'                                      
         END                                                                    
