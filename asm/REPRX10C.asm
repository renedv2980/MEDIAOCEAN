*          DATA SET REPRX10C   AT LEVEL 022 AS OF 12/09/99                      
*PHASE T81A10A                                                                  
         TITLE 'REPRP10(T81A10) - REP PC SELLERS BLACK BOX ROUTINES'            
*                                                                               
       ++INCLUDE REPRPUTIL                                                      
*                                                                               
* REPRPWORKD                                                                    
* REGENSAL2                                                                     
* REGENAGY2                                                                     
* REGENIBKL                                                                     
* REGENDSP                                                                      
* REGENDCT                                                                      
         PRINT OFF                                                              
       ++INCLUDE REPRPWORKD                                                     
       ++INCLUDE REGENSAL2                                                      
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REGENIBKL                                                      
       ++INCLUDE REGENDSP                                                       
       ++INCLUDE REGENDCT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022REPRX10C  12/09/99'                                      
         END                                                                    
