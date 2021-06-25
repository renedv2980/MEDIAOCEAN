*          DATA SET CTREP6001  AT LEVEL 002 AS OF 08/22/00                      
*PHASE CT6001A                                                                  
         TITLE 'SPECS FOR JCL REPORT'                                           
CT6001   CSECT                                                                  
         FSPEC READ,JCL                                                         
         ASPEC H1,2,RUN                                                         
         ASPEC H1,31,C'JCL LISTING'                                             
         ASPEC H2,31,C'-----------'                                             
         ASPEC H1,55,REPORT                                                     
         ASPEC H1,70,PAGE                                                       
         ASPEC H4,55,REQUESTOR                                                  
         ASPEC H7,2,C'JCL BOOK      LINE    DESCRIPTION'                        
         ASPEC H8,2,C'--------     NUMBER   -----------'                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTREP6001 08/22/00'                                      
         END                                                                    
