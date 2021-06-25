*          DATA SET T41311     AT LEVEL 015 AS OF 10/20/09                      
*PHASE T41311A                                                                  
*INCLUDE NLSTDCOM                                                               
*INCLUDE NLCOMLIN                                                               
*INCLUDE NLMATCOM                                                               
*INCLUDE NLBLDREV                                                               
         TITLE 'T41311- INS ORDERS- FMTBUY,STDCOM,COMLIN,MATCOM'                
T41311   CSECT                                                                  
         SPACE 3                                                                
         DC    A(*)                                                             
         DC    V(STDCOM)                                                        
         DC    V(COMLIN)                                                        
         DC    V(MATCOM)                                                        
         DC    V(BLDREV)                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015T41311    10/20/09'                                      
         END                                                                    
