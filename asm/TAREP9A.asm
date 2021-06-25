*          DATA SET TAREP9A    AT LEVEL 001 AS OF 08/10/00                      
*PHASE T7039AA                                                                  
         TITLE 'T7039A - BUFFER FOR DRIVER APPLICS'                             
GLOBALS  CSECT                                                                  
         DC    C'*GLOBAL*'                                                      
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    F'40000'            THIS MAPS TO GLSIZE                          
         DC    40000X'00'                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TAREP9A   08/10/00'                                      
         END                                                                    
