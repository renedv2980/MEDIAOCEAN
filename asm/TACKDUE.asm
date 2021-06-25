*          DATA SET TACKDUE    AT LEVEL 005 AS OF 07/14/14                      
*PHASE TACKDUEA                                                                 
*                                                                               
         TITLE 'TACKDUE - DUETAB FOR TALENT CHECK PROGRAM'                      
DUETAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF DUE COMPANY ELEMENTS                                          
*                                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(DUELEN)         RECORD LENGTH                                
         DC    AL4(DUELKEY)        DISP. TO KEY / KEY LENGTH                    
         DC    AL4(MAXDUES)        MAX. NUMBER OF RECORDS                       
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    X'80'               BINARY                                       
         DC    AL1(0)                                                           
         DC    (MAXDUES*DUELEN)X'00'  THE TABLE                                 
*                                                                               
*TACKREPD                                                                       
*TAGENFILE                                                                      
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005TACKDUE   07/14/14'                                      
         END                                                                    
