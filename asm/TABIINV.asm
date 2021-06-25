*          DATA SET TABIINV    AT LEVEL 013 AS OF 12/09/13                      
*PHASE TABIINVA,*                                                               
*                                                                               
         TITLE 'TABIINV - INVOICE SORT TABLE FOR BILLING (TAREP13)'             
SRTINVT  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        INVOICE SORT TABLE                                                     
*                                                                               
         ORG   *+(20000*SRTILEN)                                                
*                                                                               
*TABIDSECT                                                                      
*TABILLD                                                                        
*TAGENFILE                                                                      
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TABIDSECT                                                      
       ++INCLUDE TABILLD                                                        
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013TABIINV   12/09/13'                                      
         END                                                                    
