*          DATA SET SRFAD01    AT LEVEL 002 AS OF 08/22/00                      
*          DATA SET SRFAD01    AT LEVEL 009 AS OF 09/21/90                      
*PHASE T15EA1A                                                                  
T15E01C  CSECT                                                                  
         DC    X'FFFFFF',C'FAADRREC  '                                          
         DC    X'0000',C'ADRRECD D',X'00',X'20'                                 
         DC    C'***** DATA RECORDER RECORD *****'                              
         DC    X'0000',C'ADRREC  S',X'05',C'0CL38',X'00'                        
         DC    X'0000',C'ADRLINE S',X'03',C'CL4',X'07',C'LINE ID'               
         DC    X'0004',C'ADRADDR S',X'03',C'CL4',X'10'                          
         DC    C'TERMINAL ADDRESS'                                              
         DC    X'0008',C'ADROVSYSS',X'01',C'C',X'1C'                            
         DC    C'SYS NUMBER FOR OVERLAY CALLS'                                  
         DC    X'0009',C'ADRSYSNOS',X'01',C'C',X'0D'                            
         DC    C'SYSTEM NUMBER'                                                 
         DC    X'FFFF00'                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRFAD01   08/22/00'                                      
         END                                                                    
