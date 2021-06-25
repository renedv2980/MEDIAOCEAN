*          DATA SET NEWRIB1    AT LEVEL 006 AS OF 02/08/07                      
*PHASE T320B1A                                                                  
         TITLE 'T320B1 - BUFFER FOR DRIVER APPLICS'                             
GLOBALS  CSECT                                                                  
         DC    C'*GLOBAL*'                                                      
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    F'40000'            THIS MAPS TO GLSIZE                          
         DC    40000X'00'                                                       
         SPACE 1                                                                
         DC    C'*NAMPOOL'         POOL FOR ACCOUNT NAMES                       
         DC    A(0)                NUMBER OF ITEM                               
         DC    A(400)              MAX NUMBER OF ITEMS                          
         DC    A(40)               WIDTH OF EACH ITEM                           
         DC    16000X'00'                                                       
         SPACE 1                                                                
         DC    C'**COST**'         POOL FOR BRAND COST PERCENTS                 
******   DC    F'1020'                                                          
******   DC    1020X'00'                                                        
         DC    F'3500'          (ALPHA PROD+CL4) X 500                          
         DC    3500X'00'                                                        
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEWRIB1   02/08/07'                                      
         END                                                                    
