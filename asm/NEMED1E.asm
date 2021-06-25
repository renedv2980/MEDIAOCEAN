*          DATA SET NEMED1E    AT LEVEL 006 AS OF 08/10/00                      
*PHASE T31E1EA                                                                  
         TITLE 'T31E1E - SPECS FOR NETWORK BILLING PREP '                       
T31E1E   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,45,C'BILLING PREPARATION REPORT'                              
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,45,C'--------------------------'                              
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,45,PERIOD                                                     
         SSPEC H4,100,NETREP                                                    
         SSPEC H5,100,PAGE                                                      
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'PRODUCT'                                                  
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEMED1E   08/10/00'                                      
         END                                                                    
