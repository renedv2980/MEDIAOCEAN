*          DATA SET PPINS11    AT LEVEL 020 AS OF 02/01/10                      
*PHASE T41F11A                                                                  
*INCLUDE PPSTDCOM                                                               
*INCLUDE PPCOMLIN                                                               
*INCLUDE PPMATCOM                                                               
*INCLUDE PPBLDREV                                                               
         TITLE 'T41F11- INS ORDERS- STDCOM,COMLIN,MATCOM,BLDREV'                
T41F11   CSECT                                                                  
         SPACE 3                                                                
         DC    A(*)                                                             
         DC    V(STDCOM)                                                        
         DC    V(COMLIN)                                                        
         DC    V(MATCOM)                                                        
         DC    V(BLDREV)                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020PPINS11   02/01/10'                                      
         END                                                                    
