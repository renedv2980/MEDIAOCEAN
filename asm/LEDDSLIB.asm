*          DATA SET LEDDSLIB   AT LEVEL 006 AS OF 10/25/99                      
*PHASE LEDDSLIB                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'LOAD MODULE TO BE LOADED BY LE MAIN ROUTINE'                    
LEDDSLIB CSECT                                                                  
         NMOD1 0,LEDDS                                                          
         ENTRY SSB                                                              
         ENTRY MASTC                                                            
         L     R4,4(RD)                                                         
         XC    0(4,R4),0(R4)                                                    
         L     R3,DATCON                                                        
         XC    22(4,R3),22(R3)                                                  
         GOTO1 DATCON,(R1)                                                      
         XMOD1 1                                                                
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
DATCON   DC    V(DATCON)                                                        
         SPACE 1                                                                
         DS    0D                                                               
SSB      DC    XL255'00'                                                        
         DS    0D                                                               
MASTC    DC    XL255'00'                                                        
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006LEDDSLIB  10/25/99'                                      
         END   LEDDSLIB                                                         
