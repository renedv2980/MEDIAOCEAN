*          DATA SET CTREP6401  AT LEVEL 002 AS OF 08/22/00                      
*PHASE CT6401A                                                                  
         TITLE 'SPECS FOR LIBRARY LIST'                                         
CT6401   CSECT                                                                  
         FSPEC READ,LIBRARY                                                     
         ASPEC H1,2,RUN                                                         
         ASPEC H1,31,C'LIBRARY LIST'                                            
         ASPEC H2,31,C'------------'                                            
         ASPEC H1,55,REPORT                                                     
         ASPEC H1,70,PAGE                                                       
         ASPEC H4,55,REQUESTOR                                                  
         ASPEC H7,2,C' LIBRARY      LINE    DESCRIPTION'                        
         ASPEC H8,2,C'BOOK NAME    NUMBER   -----------'                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTREP6401 08/22/00'                                      
         END                                                                    
