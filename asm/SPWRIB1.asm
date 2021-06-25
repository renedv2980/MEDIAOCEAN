*          DATA SET SPWRIB1    AT LEVEL 082 AS OF 08/11/00                      
*PHASE T204B1A                                                                  
         TITLE 'T204B1 - BUFFER FOR SPOT WRITER APPLICATIONS'                   
GLOBALS  CSECT                                                                  
         DC    C'*PRDBUF*'         PRODUCT BUFFER                               
         DC    (256*PRDBUFFL)X'00'                                              
         SPACE 1                                                                
         DC    C'*ESTTAB*'         PRODUCT/ESTIMATE TABLE                       
         DC    (256*256)X'00'                                                   
         SPACE 1                                                                
         DC    C'*USNTAB*'                                                      
         DC    F'0'                NUMBER OF TABLE ENTRIES                      
         DC    F'200'              MAXIMUM TABLE ENTRIES                        
         DC    (200*7)X'00'                                                     
         SPACE 1                                                                
         DC    C'*DPTTABS'         DAYPART TABLES                               
         DC    (36*180)X'00'                                                    
         SPACE 1                                                                
         DC    C'*MKTWGT*'         MARKET WEIGHT TABLE                          
         DC    (MWTENTS*MWTABL)X'00'                                            
         SPACE 1                                                                
         DC    C'*MKTLST*'         MARKET LIST                                  
         DC    (MLNMKTS*MKTLSTL)X'00'                                           
         SPACE 1                                                                
         DC    C'*ESTBUF*'         ESTIMATE BUFFER                              
         DC    (256*ESTBUFFL)X'00'                                              
         SPACE 2                                                                
* SPWRIWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'082SPWRIB1   08/11/00'                                      
         END                                                                    
