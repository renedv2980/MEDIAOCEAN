*          DATA SET REWCF10    AT LEVEL 012 AS OF 05/04/06                      
*PHASE T83A10C                                                                  
         TITLE 'REWCF10(T83A10) - REP WEB CONFIRM BOX ROUTINES'                 
       ++INCLUDE REWCFUTIL                                                      
*                                                                               
* REWCFWRK                                                                      
* REGENSAL2                                                                     
* REGENAGY2                                                                     
* REGENIBKL                                                                     
* REGENDSP                                                                      
* REGENDCT                                                                      
         PRINT OFF                                                              
       ++INCLUDE REWCFWRK                                                       
       ++INCLUDE REGENSAL2                                                      
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REGENIBKL                                                      
       ++INCLUDE REGENDSP                                                       
       ++INCLUDE REGENDCT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012REWCF10   05/04/06'                                      
         END                                                                    
