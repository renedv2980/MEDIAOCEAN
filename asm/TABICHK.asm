*          DATA SET TABICHK    AT LEVEL 027 AS OF 12/09/13                      
*PHASE TABICHKA,*                                                               
*                                                                               
         TITLE 'TABICHK - CHECK TABLE FOR BILLING (TABILL)'                     
CHKTABT  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        CHECK TABLE                                                            
*                                                                               
         ORG   *+(TABCHKLN*TABLEN)                                              
*                                                                               
*TABIDSECT                                                                      
*TABILLD                                                                        
*DDSPOOLD                                                                       
*TAGENFILE                                                                      
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TABIDSECT                                                      
       ++INCLUDE TABILLD                                                        
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027TABICHK   12/09/13'                                      
         END                                                                    
