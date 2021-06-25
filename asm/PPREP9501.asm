*          DATA SET PPREP9501  AT LEVEL 003 AS OF 08/09/00                      
*PHASE PP9501A                                                                  
         TITLE 'PP9501 - RJR INTERFACE TAPES - SPECS'                           
         PRINT NOGEN                                                            
PP9501   CSECT                                                                  
         SPACE 2                                                                
         FSPEC READ,BUYS                                                        
*                                                                               
         PSPEC H1,1,C'PRINTPAK - RJR TAPE CREATION'                             
         PSPEC H2,1,C'----------------------------'                             
         PSPEC H1,32,REPORT                                                     
         PSPEC H2,32,RUN                                                        
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPREP9501 08/09/00'                                      
         END                                                                    
