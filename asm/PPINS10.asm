*          DATA SET PPINS10    AT LEVEL 049 AS OF 07/17/18                      
*PHASE T41F10A                                                                  
*INCLUDE PPPRCINS                                                               
*INCLUDE PPIORPRT                                                               
*                                                                               
         TITLE 'T41F10- INS ORDERS- PINSOR,IOPRNT'                              
*                                                                               
T41F10   CSECT                                                                  
         SPACE 2                                                                
         DC    A(*)                                                             
         DC    V(PINSOR)                                                        
         DC    V(IOPRNT)                                                        
         DC    X'FF'                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049PPINS10   07/17/18'                                      
         END                                                                    
