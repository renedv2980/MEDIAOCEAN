*          DATA SET NEMED1F    AT LEVEL 005 AS OF 08/10/00                      
*PHASE T31E1FA                                                                  
         TITLE 'T31E1F - SPECS FOR NETWORK TEST REPORT (EGS)'                   
T31E1F   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,45,C'NETWORK EGS TEST REPORT'                                 
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,45,C'-----------------'                                       
         SSPEC H2,76,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,76,NETREP                                                     
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,41,C'NETWORK='                                                
         SSPEC H5,58,C'PACKAGE='                                                
         SSPEC H5,90,PAGE                                                       
         SSPEC H6,1,C'ESTIMATE'                                                 
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEMED1F   08/10/00'                                      
         END                                                                    
