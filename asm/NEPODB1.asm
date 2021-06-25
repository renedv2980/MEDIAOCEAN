*          DATA SET NEPODB1    AT LEVEL 002 AS OF 05/31/89                      
*PHASE T325B1,*                                                                 
         TITLE 'T325B1 - BUFFER FOR research writer application'                
GLOBALS  CSECT                                                                  
         DC    C'*GLOBAL*'                                                      
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    F'40000'            THIS MAPS TO GLSIZE                          
         DC    40000X'00'                                                       
         SPACE 1                                                                
******** DC    C'*NAMPOOL'         POOL FOR SYSTEM NAMES                        
******** DC    A(0)                A(NEXT AVAILABLE ENTRY)                      
******** DC    A(64000)            A(LENGTH OF AVAILABLE SPACE)                 
******** DC    64000X'00'                                                       
         SPACE 1                                                                
         DC    C'*PRDBUF*'         PRODUCT BUFFER                               
         DC    (256*PRDBUFFL)X'00'                                              
         SPACE 1                                                                
         DC    C'*MKTGRP*'         MARKET GROUP TABLE                           
         DC    (10000*2)X'00'                                                   
         SPACE 1                                                                
         DC    C'*MKTGR2*'         MARKET GROUP TABLE 2                         
         DC    (10000*2)X'00'                                                   
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
         SPACE 1                                                                
         DC    C'*STABUF*'         STATION BUFFER                               
         DC    F'0'                RECORDS SO FAR                               
         DC    F'1000'             MAX RECORDS                                  
         DC    (1000*STABUFFL)X'00'                                             
**                                                                              
**       NOTE: 44K NOW AVAILABLE                                                
**       -----------------------                                                
**                                                                              
*****    SPACE 1                                                                
*****    DC    C'**BUFF**'         OFF LINE BUFFER                              
*****    DC    F'24000'            AVAILABLE SPACE                              
*****    DC    24000X'00'          24K                                          
         SPACE 2                                                                
* SPWRIWORKD                                                                    
       ++INCLUDE SPWRIWORKD                                                     
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEPODB1   05/31/89'                                      
         END                                                                    
