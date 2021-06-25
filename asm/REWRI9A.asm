*          DATA SET REWRI9A    AT LEVEL 002 AS OF 08/31/00                      
*          DATA SET REWRI9A    AT LEVEL 001 AS OF 05/23/90                      
*PHASE T8219AA                                                                  
         TITLE 'T8219A - BUFFER FOR DRIVER APPLICS'                             
GLOBALS  CSECT                                                                  
         DC    C'*GLOBAL*'                                                      
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    F'40000'            THIS MAPS TO GLSIZE                          
         DC    40000X'00'                                                       
         SPACE                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002REWRI9A   08/31/00'                                      
         END                                                                    
