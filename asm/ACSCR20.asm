*          DATA SET ACSCR20    AT LEVEL 006 AS OF 01/29/97                      
*PHASE T60C20A,+0                                                               
         TITLE 'AREA TO LOAD APG PROGRAMS OR JUST A LARGE WORK AREA'            
T60C20   CSECT                                                                  
BIGBLOCK DS    XL(20*K)            20K WORK AREA                                
BIGEND   EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE ACSCRWRK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACSCR20   01/29/97'                                      
         END                                                                    
