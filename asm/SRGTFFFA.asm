*PHASE T166FFA                                                                  
SRGTFF   CSECT                                                                  
**       DC    XL(8)'5600000100000000'             78 BYTE UPROT FLD            
**       DC    CL(78)' '                                                        
         DC    XL(8)'0D08000100000000'             TSO LIKE READY FLD           
**       DC    XL(8)'0D08005100000000'             TSO LIKE READY FLD           
         DC    CL(5)'READY'                                                     
         DC    XL(8)'FF00000800000000'             247 BYTE UPROT FLD           
**       DC    XL(8)'FF00005800000000'             247 BYTE UPROT FLD           
         DC    CL(247)' '                                                       
         DC    XL(8)'5620032100000000'             78 BYTE PROT FLD             
         DC    CL(78)'GETFILE: IND$FILE INTERFACE'                              
         DC    X'00'                                                            
         END                                                                    
