*          DATA SET SPREPTS01  AT LEVEL 003 AS OF 08/29/00                      
*PHASE SPTS01A                                                                  
SPTS01   TITLE 'CTA BUCKET CREATE - SPECS'                                      
SPTS01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC USE,SP0003                                                       
         SSPEC H1,55,C'CTA BUCKET CREATE'                                       
         SSPEC H2,55,C'-----------------'                                       
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H3,51,PERIOD                                                     
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,100,PAGE                                                      
         SSPEC H3,111,REPORT                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPTS01 08/29/00'                                      
         END                                                                    
