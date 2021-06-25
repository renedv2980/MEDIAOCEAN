*          DATA SET SPWRIB2    AT LEVEL 087 AS OF 12/15/04                      
*PHASE T204B2A,*                                                                
         TITLE 'T204B2 - BUFFER FOR SPOT WRITER APPLICATIONS'                   
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
* DDSPLWORKD                                                                    
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087SPWRIB2   12/15/04'                                      
         END                                                                    
