*          DATA SET PPREPWE01  AT LEVEL 009 AS OF 08/09/00                      
*PHASE PPWE01A                                                                  
         TITLE 'PPWE01 - WEC/CAMBELL REP CONVERSION'                            
PPWE01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PUBFILE                                                   
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PUBDIR                                                    
*                                                                               
         PSPEC H1,51,C'WEC/CAMBELL REP CONVERSION'                              
         PSPEC H2,51,C'--------------------------'                              
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009PPREPWE01 08/09/00'                                      
         END                                                                    
