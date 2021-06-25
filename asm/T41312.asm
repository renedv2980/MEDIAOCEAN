*          DATA SET T41312     AT LEVEL 023 AS OF 06/05/12                      
*PHASE T41312A                                                                  
*INCLUDE NLFMTBUY                                                               
*INCLUDE PPBYOUT                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
         TITLE 'T41312 - FMTBUY ROUTINES'                                       
         EJECT                                                                  
T41312   CSECT                                                                  
         DC    A(*)                                                             
         DC    V(FMTBUY)                                                        
         DC    V(PPBYOUT)                                                       
         DC    V(CHOPPER)                                                       
         DC    V(SQUASHER)                                                      
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023T41312    06/05/12'                                      
         END                                                                    
