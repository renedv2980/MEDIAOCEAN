*          DATA SET T41310     AT LEVEL 026 AS OF 07/17/18                      
*PHASE T41310A                                                                  
*INCLUDE NLPINSOR                                                               
*INCLUDE NLIOPRNT                                                               
         TITLE 'T41310- INS ORDERS- PINSOR,IOPRNT'                              
T41310   CSECT                                                                  
         SPACE 2                                                                
         DC    A(*)                                                             
         DC    V(PINSOR)                                                        
         DC    V(IOPRNT)                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026T41310    07/17/18'                                      
         END                                                                    
