*          DATA SET NEMED51    AT LEVEL 005 AS OF 08/10/00                      
*PHASE T31E51A                                                                  
         TITLE 'T31E51 - SPECS FOR NETWORK UPDATE'                              
T31E51   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,45,C'NETWORK UPDATE REPORT'                                   
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,45,C'---------------------'                                   
         SSPEC H2,76,AGYADD                                                     
         SSPEC H3,76,RUN                                                        
         SSPEC H3,1,C'CLIENT'                                                   
         SSPEC H4,1,C'ESTIMATE'                                                 
         SSPEC H5,1,C'NETWORK'                                                  
         SSPEC H6,1,C'PACKAGE'                                                  
         SSPEC H5,90,PAGE                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEMED51   08/10/00'                                      
         END                                                                    
