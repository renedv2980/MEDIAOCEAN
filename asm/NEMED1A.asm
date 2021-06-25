*          DATA SET NEMED1A    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T31E1AA                                                                  
         TITLE 'T31E1A - SPECS FOR NET ARMY INTERFACE TAPE'                     
T31E1A   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,55,C'ARMED FORCES NETWORK INTERFACE TAPE'                     
         SSPEC H1,106,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,55,C'------------------------------------'                    
         SSPEC H2,106,AGYADD                                                    
         SSPEC H3,1,C'CLIENT'                                                   
         SSPEC H4,1,C'PRODUCT'                                                  
         SSPEC H5,1,C'ESTIMATE'                                                 
         SSPEC H5,106,PAGE                                                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED1A   08/10/00'                                      
         END                                                                    
