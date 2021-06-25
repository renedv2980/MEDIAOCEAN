*          DATA SET REEZP10    AT LEVEL 028 AS OF 10/16/08                      
*PHASE T82A10A                                                                  
         TITLE 'REEZP10(T82A10) - REP EZ-POST BLACK BOX ROUTINES'               
***********************************************************************         
* HISTORY OF CHANGES:                                                 *         
* 05/16/2007  BU     URLSTORE INCREASED TO 80 CHARS:  REASSEMBLY      *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
       ++INCLUDE REPRPUTIL                                                      
*                                                                               
* REEZPWORKD                                                                    
* REGENSAL2                                                                     
* REGENAGY2                                                                     
* REGENIBKL                                                                     
* REGENDSP                                                                      
* REGENDCT                                                                      
         PRINT ON                                                               
       ++INCLUDE REEZPWORKD                                                     
       ++INCLUDE REGENSAL2                                                      
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REGENIBKL                                                      
       ++INCLUDE REGENDSP                                                       
       ++INCLUDE REGENDCT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028REEZP10   10/16/08'                                      
         END                                                                    
