*          DATA SET SPREPZR01  AT LEVEL 002 AS OF 05/30/12                      
*PHASE SPZR01A,+0,NOAUTO                                                        
         TITLE 'SPZR01 - EASI - REP INVOICE CONVERTER'                          
         PRINT NOGEN                                                            
SPZ501   CSECT                                                                  
         SPACE 2                                                                
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
*        FSPEC UPDATE,TRAFFIC                                                   
         FSPEC USE,SP0003                                                       
*                                                                               
         PSPEC H1,11,REQUESTOR                                                  
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,98,REPORT                                                     
         PSPEC H3,122,PAGE                                                      
*                                                                               
         PSPEC H1,52,C'EASI - INVOICE CONVERSION REPORT'                        
         PSPEC H2,52,C'--------------------------------'                        
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPZR01 05/30/12'                                      
         END                                                                    
