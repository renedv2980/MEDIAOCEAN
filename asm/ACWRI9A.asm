*          DATA SET ACWRI9A    AT LEVEL 012 AS OF 08/10/00                      
*          DATA SET ACWRIB1    AT LEVEL 010 AS OF 05/10/88                      
*PHASE T6149AA                                                                  
         TITLE 'T6149A - BUFFER FOR DRIVER APPLICS'                             
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
         DC    A(51)               WIDTH OF EACH ITEM                           
         DC    20400X'00'                                                       
         SPACE 1                                                                
         DC    C'*LDGPOOL'         POOL FOR LEDGER HEIRARCHIES                  
         DC    A(0)                NUMBER OF ITEM                               
         DC    A(100)              MAX NUMBER OF ITEMS                          
         DC    A(67)               WIDTH OF EACH ITEM                           
         DC    6700X'00'                                                        
         SPACE 1                                                                
         DC    C'*OGROUP*'         OFFICE GROUP LIST                            
         DC    A(400)              LENGTH OF BUFFER                             
         DC    400X'00'                                                         
         SPACE 1                                                                
         DC    C'*GGROUP*'         MEDIA GROUP LIST                             
         DC    A(400)              LENGTH OF BUFFER                             
         DC    400X'00'                                                         
         SPACE 1                                                                
         DC    C'*WGROUP*'         WORK CODE LIST                               
         DC    A(3000)             LENGTH OF BUFFER                             
         DC    3000X'00'                                                        
         SPACE 1                                                                
         DC    C'**BUFF**'         OFF LINE BUFFER                              
         DC    20000X'00'          20K                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACWRI9A   08/10/00'                                      
         END                                                                    
