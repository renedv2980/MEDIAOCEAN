*          DATA SET PPREPX101  AT LEVEL 008 AS OF 08/09/00                      
*PHASE PPX101A                                                                  
         TITLE 'PP0201 - PRTFIX SPECS'                                          
PP0201   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PUBFILE                                                   
*                                                                               
         PSPEC H1,48,C'PRINTPAK CONTRACT PURGE LISTING'                         
         PSPEC H2,48,C'-------------------------------'                         
         PSPEC H5,24,C'CON'                                                     
         PSPEC H6,1,C'CLT   PUB              NUM.  PERIOD'                      
         PSPEC H7,1,C'---   ---------------  ----  -------------------'         
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008PPREPX101 08/09/00'                                      
         END                                                                    
