*          DATA SET SRFAD02    AT LEVEL 002 AS OF 08/22/00                      
*          DATA SET SRFAD01    AT LEVEL 009 AS OF 09/21/90                      
*PHASE T15EA2A                                                                  
T15E01C  CSECT                                                                  
         DC    X'000A',C'ADRPRGNOS',X'01',C'C',X'0E'                            
         DC    C'PROGRAM NUMBER'                                                
         DC    X'000B',C'ADRTASK S',X'01',C'C',X'12'                            
         DC    C'PROCESSING TASK ID'                                            
         DC    X'000C',C'ADRSIN  S',X'01',C'F',X'13'                            
         DC    C'SYSTEM INPUT NUMBER'                                           
         DC    X'0010',C'ADRSTTM S',X'01',C'F',X'1A'                            
         DC    C'PROCESSING START TIME (TU)'                                    
         DC    X'0014',C'ADRNDTM S',X'01',C'F',X'18'                            
         DC    C'PROCESSING END TIME (TU)'                                      
         DC    X'0018',C'ADRCPUTMS',X'01',C'F',X'15'                            
         DC    C'ELAPSED CPU TIME (TU)'                                         
         DC    X'001C',C'ADRQTM  S',X'01',C'H',X'1D'                            
         DC    C'ELAPSED INPUT QUEUE TIME (TU)'                                 
         DC    X'001E',C'ADRMSGI S',X'01',C'H',X'14'                            
         DC    C'INPUT MESSAGE LENGTH'                                          
         DC    X'0020',C'ADRMSGO S',X'01',C'H',X'15'                            
         DC    C'OUTPUT MESSAGE LENGTH'                                         
         DC    X'0022',C'ADRIOCNTS',X'01',C'H',X'0B',C'I/O COUNTER'             
         DC    X'0024',C'ADROVCNTS',X'01',C'H',X'14'                            
         DC    C'CALL OVERLAY COUNTER'                                          
         DC    X'FFFFFF',C'FAATC     '                                          
         DC    X'0000',C'ATCD    D',X'00',X'21'                                 
         DC    C'***** ATTACHED TASK CONTROL *****'                             
         DC    X'FFFF00'                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRFAD02   08/22/00'                                      
         END                                                                    
