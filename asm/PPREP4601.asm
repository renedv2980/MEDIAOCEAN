*          DATA SET PPREP4601  AT LEVEL 009 AS OF 06/02/00                      
*PHASE PP4601A,+0                                                               
         TITLE 'SPECS FOR PUBLICATION SHEET'                                    
PP4601   CSECT                                                                  
         RSPEC REQUEST,NOREP                                                    
         SPACE 1                                                                
         FSPEC READ,PUBLICATIONS                                                
         PSPEC H1,2,MEDIA                                                       
         PSPEC H1,39,C'PUBLICATION INFORMATION SHEET'                           
         PSPEC H2,39,29C'-'                                                     
         PSPEC H1,77,AGYNAME                                                    
         PSPEC H2,2,C'PUBLICATION'                                              
         PSPEC H2,14,PUBNUM                                                     
         PSPEC H2,77,AGYADD                                                     
         PSPEC H4,2,PUBNAME                                                     
         PSPEC H4,77,RUN                                                        
         PSPEC H5,2,ZONENAME                                                    
         PSPEC H5,77,REPORT                                                     
         PSPEC H5,97,PAGE                                                       
         PSPEC H6,2,PUBADD                                                      
         PSPEC H6,77,REQUESTOR                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009PPREP4601 06/02/00'                                      
         END                                                                    
