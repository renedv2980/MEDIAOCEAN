*          DATA SET REPRP10    AT LEVEL 032 AS OF 10/16/08                      
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
       ++INCLUDE REPRPWORKD                                                     
       ++INCLUDE REGENSAL2                                                      
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REGENIBKL                                                      
       ++INCLUDE REGENDSP                                                       
       ++INCLUDE REGENDCT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032REPRP10   10/16/08'                                      
         END                                                                    
