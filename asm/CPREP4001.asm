*          DATA SET CPREP4001  AT LEVEL 002 AS OF 09/01/00                      
*PHASE CP4001A                                                                  
         TITLE 'CP4001 - SPECS FOR CUSTOMISED REPORT'                           
         PRINT NOGEN                                                            
CP4001   CSECT                                                                  
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,77,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,77,AGYADD                                                     
         PSPEC H4,41,PERIOD                                                     
         PSPEC H4,77,REPORT                                                     
         PSPEC H4,103,PAGE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CPREP4001 09/01/00'                                      
         END                                                                    
