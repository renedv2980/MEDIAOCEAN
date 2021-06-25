*          DATA SET NEMEDB7    AT LEVEL 003 AS OF 08/10/00                      
*PHASE T31EB7A                                                                  
         TITLE 'T31EB7 - BUFFER FOR DRIVER APPLICS'                             
GLOBALS  CSECT                                                                  
         DC    C'*GLOBAL*'                                                      
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    F'40000'            THIS MAPS TO GLSIZE                          
         DC    40000X'00'                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003NEMEDB7   08/10/00'                                      
         END                                                                    
