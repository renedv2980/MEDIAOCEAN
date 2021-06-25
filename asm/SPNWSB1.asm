*          DATA SET SPNWSB1    AT LEVEL 003 AS OF 11/20/89                      
*PHASE T207B1,*                                                                 
         TITLE 'T207B1 - BUFFER FOR BWS DRIVER APPLICATIONS'                    
GLOBALS  CSECT                                                                  
         DC    C'*GLOBAL*'                                                      
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    F'40000'            THIS MAPS TO GLSIZE                          
         DC    40000X'00'                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPNWSB1   11/20/89'                                      
         END                                                                    
