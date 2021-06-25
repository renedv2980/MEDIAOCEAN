*          DATA SET FAMSGBUFF  AT LEVEL 002 AS OF 08/22/00                      
*PHASE T00AA0A                                                                  
         TITLE 'GETTXT CORE BUFFER FOR FREQUENTLY USED MESSAGES'                
MSGBUFF  CSECT                                                                  
BASE     DS    0C                                                               
         SPACE 2                                                                
         DC    CL7'MSGBUFF'                                                     
         DC    CL1'+'              SET TO '-' TO DISABLE                        
         DC    F'0'                NUMBER OF GETTXT CALLS                       
         DC    F'0'                PREV NUMBER OF GETTXT CALLS                  
         DC    F'0'                NUMBER RETRIEVED FROM MSGBUFF                
         DC    H'0'                NUMBER OF INDEX ENTRIES                      
         DC    H'300'              MAX NUMBER OF INDEX ENTRIES                  
         DC    X'0'                CUT OFF THRESHOLD                            
         DC    X'0'                SPARE                                        
         DC    X'0'                SPARE                                        
         DC    X'0'                SPARE                                        
         DC    AL4(0)              A(BUFFER END)                                
         DC    AL4(0)              A(NEXT FREE ENTRY)                           
         SPACE 2                                                                
INDEX    DC    (MAXNDX)XL8'00'     INDEX ENTRIES                                
         SPACE 1                                                                
BUFFER   DC    (1024*MAXBUFL)C' '  BUFFER AREA                                  
         SPACE 2                                                                
END      DC    CL8'MSGBUFFX'                                                    
         EJECT                                                                  
       ++INCLUDE FAMSGBUFFD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002FAMSGBUFF 08/22/00'                                      
         END                                                                    
