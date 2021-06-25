*          DATA SET ACREP6501  AT LEVEL 003 AS OF 08/16/00                      
*PHASE AC6501A                                                                  
         TITLE 'SPECS FOR COMMENT LISTING'                                      
AC6501   CSECT                                                                  
         FSPEC READ,COMMENTS                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,48,C'STANDARD COMMENT LIST'                                   
         ASPEC H2,48,21C'-'                                                     
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H3,86,REQUESTOR                                                  
         ASPEC H3,2,COMPANY                                                     
         ASPEC H5,2,C'COMMENT NUMBER'                                           
         ASPEC H6,2,C'--------------'                                           
         ASPEC H5,20,C'COMMENT DATA'                                            
         ASPEC H6,20,C'------------'                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREP6501 08/16/00'                                      
         END                                                                    
