*          DATA SET REFAMO     AT LEVEL 014 AS OF 03/22/16                      
*PHASE REFAMOA                                                                  
RCVPTAB  CSECT                                                                  
         DC    A(RCVPTABP-RCVPTAB)                                              
         DC    A(RCVPTABT-RCVPTAB)                                              
                                                                                
RCVPTABP DS    0D                  PRODUCTION REP SYSTEM - ALPHA ORDER          
*        DC    CL2'AM'             Removed Mar, 22,2016                         
         DC    CL2'BL'                                                          
         DC    CL2'BM'             Dec/09                                       
         DC    CL2'B1'                                                          
*        DC    CL2'CQ'             Removed Mar, 22,2016                         
         DC    CL2'F8'                                                          
         DC    CL2'F9'                                                          
         DC    CL2'K8'                                                          
*        DC    CL2'MR'             Removed Mar, 22,2016                         
         DC    CL2'MS'                                                          
         DC    CL2'PV'                                                          
         DC    CL2'P9'                                                          
         DC    CL2'QT'                                                          
         DC    CL2'SJ'                                                          
*        DC    CL2'SZ'             Removed Mar, 22,2016                         
*        DC    CL2'TV'             Removed Mar, 22,2016                         
         DC    CL2'UV'                                                          
         DC    CL2'UY'                                                          
         DC    CL2'U1'                                                          
         DC    CL2'U2'                                                          
*        DC    CL2'8K'             Removed Mar, 22,2016                         
         DC    X'FFFF'                                                          
                                                                                
RCVPTABT DS    0D                  TEST REP SYSTEM - SORTED BY AGENCY           
         DC    CL2'B3'                                                          
         DC    CL2'B4'                                                          
         DC    CL2'KB'                                                          
         DC    CL2'QT'                                                          
         DC    CL2'6U'             RUSNY                                        
         DC    X'FFFF'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014REFAMO    03/22/16'                                      
         END                                                                    
