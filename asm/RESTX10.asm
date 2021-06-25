*          DATA SET RESTX10    AT LEVEL 013 AS OF 03/07/06                      
*PHASE T83D10A                                                                  
         TITLE 'RESTX10(T83D10) - REP WEB CONFIRM BOX ROUTINES'                 
       ++INCLUDE RESTXUTIL                                                      
*                                                                               
* RESTXWRK                                                                      
* REGENSAL2                                                                     
* REGENAGY2                                                                     
* REGENIBKL                                                                     
* REGENDSP                                                                      
* REGENDCT                                                                      
         PRINT OFF                                                              
       ++INCLUDE RESTXWRK                                                       
       ++INCLUDE REGENSAL2                                                      
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REGENIBKL                                                      
       ++INCLUDE REGENDSP                                                       
       ++INCLUDE REGENDCT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013RESTX10   03/07/06'                                      
         END                                                                    
