*          DATA SET NEWRIB0    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T320B0A                                                                  
         TITLE 'GENERIC SSPECS FOR NET DRIVE REPORTS'                           
T320B0   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,49,PERIOD                                                     
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,99,PAGE                                                       
         SSPEC H6,1,C'ESTIMATE'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEWRIB0   08/10/00'                                      
         END                                                                    
