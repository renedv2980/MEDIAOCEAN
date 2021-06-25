*          DATA SET TAGEN9F    AT LEVEL 005 AS OF 08/10/00                      
*PHASE T7029FA                                                                  
         TITLE 'T7029F - BUFFER FOR DRIVER APPLICS'                             
GLOBALS  CSECT                                                                  
         DC    C'*GLOBAL*'                                                      
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    F'80000'            THIS MAPS TO GLSIZE                          
         DC    80000X'00'                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005TAGEN9F   08/10/00'                                      
         END                                                                    
