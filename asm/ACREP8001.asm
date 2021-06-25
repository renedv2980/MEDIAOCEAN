*          DATA SET ACREP8001  AT LEVEL 008 AS OF 08/16/00                      
*PHASE AC8001A                                                                  
         TITLE 'SPECS FOR LIST REPORT'                                          
AC8001   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACLIST                                                      
         ASPEC H1,2,RUN                                                         
         ASPEC H1,50,C'LIST REPORT'                                             
         ASPEC H1,99,PAGE                                                       
         ASPEC H1,84,REPORT                                                     
         ASPEC H2,50,C'-----------'                                             
         ASPEC H3,84,REQUESTOR                                                  
         ASPEC H5,2,C'LIST CODE AND NAME'                                       
         ASPEC H6,2,C'------------------'                                       
         ASPEC H5,48,C'LIST TYPE'                                               
         ASPEC H6,48,C'---------'                                               
         ASPEC H5,67,C'EXPIRY DATE'                                             
         ASPEC H6,67,C'-----------'                                             
         ASPEC H5,80,C'LIST DATA'                                               
         ASPEC H6,80,C'---------'                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREP8001 08/16/00'                                      
         END                                                                    
