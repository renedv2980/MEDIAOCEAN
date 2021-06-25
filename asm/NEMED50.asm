*          DATA SET NEMED50    AT LEVEL 015 AS OF 08/10/00                      
*PHASE T31E50A                                                                  
         TITLE 'T31E50 - SPECS FOR NETWORK UNIT LIST'                           
T31E50   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,45,C'NETWORK UNIT LIST'                                       
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,45,C'-----------------'                                       
         SSPEC H2,76,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,76,NETREP                                                     
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,41,C'NETWORK='                                                
         SSPEC H5,58,C'PACKAGE='                                                
         SSPEC H5,90,PAGE                                                       
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H6,76,C'STATUS='                                                 
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015NEMED50   08/10/00'                                      
         END                                                                    
