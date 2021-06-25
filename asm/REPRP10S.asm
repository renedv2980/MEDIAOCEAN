*          DATA SET REPRP10S   AT LEVEL 022 AS OF 11/29/00                      
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
**PAN#1  DC    CL21'022REPRP10S  11/29/00'                                      
         END                                                                    
