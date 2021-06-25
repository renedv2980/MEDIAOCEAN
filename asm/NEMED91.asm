*          DATA SET NEMED91    AT LEVEL 006 AS OF 08/10/00                      
*PHASE T31E91A                                                                  
         TITLE 'T31E91 - SPECS FOR NETWORK ACCOUNTING REPORT'                   
T31E91   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,41,C'NETWORK ACCOUNTING ESTIMATE'                             
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,41,C'---------------------------'                             
         SSPEC H2,76,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,76,NETREP                                                     
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,76,C'NETWORK - '                                              
         SSPEC H5,101,PAGE                                                      
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H6,76,C'DAYPART - '                                              
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEMED91   08/10/00'                                      
         END                                                                    
