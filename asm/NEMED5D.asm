*          DATA SET NEMED5D    AT LEVEL 004 AS OF 08/10/00                      
*PHASE T31E5DA                                                                  
         TITLE 'T31E5D - SPECS FOR EQUALIZED COST UPDATE/REPORT'                
T31E5D   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,55,C'EQUALIZED COST REPORT'                                   
         SSPEC H1,96,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,55,C'---------------------'                                   
         SSPEC H3,52,PERIOD                                                     
         SSPEC H2,96,AGYADD                                                     
         SSPEC H3,1,C'CLIENT'                                                   
         SSPEC H4,1,C'PRODUCT'                                                  
         SSPEC H5,1,C'ESTIMATE'                                                 
         SSPEC H6,1,C'PACKAGE'                                                  
         SSPEC H4,96,RUN                                                        
         SSPEC H5,109,PAGE                                                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NEMED5D   08/10/00'                                      
         END                                                                    
