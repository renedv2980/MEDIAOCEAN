*          DATA SET PPINS12    AT LEVEL 040 AS OF 06/30/08                      
*PHASE T41F12B                                                                  
*INCLUDE PPFMTBUY                                                               
*INCLUDE PPBYOUT                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
*                                                                               
         TITLE 'T41F12 - FMTBUY ROUTINES'                                       
*                                                                               
T41F12   CSECT                                                                  
         DC    A(*)                                                             
         DC    V(FMTBUY)                                                        
         DC    V(PPBYOUT)                                                       
         DC    V(CHOPPER)                                                       
         DC    V(SQUASHER)                                                      
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040PPINS12   06/30/08'                                      
         END                                                                    
