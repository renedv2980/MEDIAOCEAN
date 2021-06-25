*          DATA SET PPREP9601  AT LEVEL 039 AS OF 08/09/00                      
*PHASE PP9601A                                                                  
         TITLE 'PP9601 - LISTER SPECS'                                          
PP9601   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BUYS                                                        
*                                                                               
*        PSPEC H1,52,C'PRINTPAK P&G ESTIMATE LISTING'                           
         PSPEC H3,49,PERIOD                                                     
         PSPEC H2,52,C'-----------------------------'                           
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H1,1,MEDIA                                                       
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H3,1,C'CLIENT'                                                   
         PSPEC H5,98,RUN                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039PPREP9601 08/09/00'                                      
         END                                                                    
