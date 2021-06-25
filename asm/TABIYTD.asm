*          DATA SET TABIYTD    AT LEVEL 016 AS OF 12/09/13                      
*PHASE TABIYTDA                                                                 
*                                                                               
         TITLE 'TABIYTD - TABLE OF YTD OF ALL PERFORMERS (TAREP13)'             
YTDTAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        YTD TABLE OF ALL PERFORMERS                                            
*                                                                               
         ORG   *+(MAXYTD*TYTDLEN)                                               
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
**PAN#1  DC    CL21'016TABIYTD   12/09/13'                                      
         END                                                                    
