*          DATA SET PPREP4401  AT LEVEL 002 AS OF 08/09/00                      
*PHASE PP4401A                                                                  
         TITLE 'PP4401 - PRINT LIST BUY SPECS'                                  
PP4401   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         FSPEC READ,PUBS                                                        
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PRTFILE                                                   
*                                                                               
         SPACE 3                                                                
         SPROG 0,10,20                                                          
*                                                                               
         PSPEC H1,58,C'PRINTPAK LIST BUY'                                       
         PSPEC H2,58,C'-----------------'                                       
*                                                                               
         PSPEC H1,1,REQUESTOR                                                   
         PSPEC H3,1,CLIENT                                                      
         PSPEC H4,1,PRODUCT                                                     
         PSPEC H5,1,ESTIMATE                                                    
*                                                                               
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H5,98,RUN                                                        
         PSPEC H6,98,PAGE                                                       
         SPACE 3                                                                
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREP4401 08/09/00'                                      
         END                                                                    
