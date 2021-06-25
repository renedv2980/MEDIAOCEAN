*          DATA SET TACKAYF    AT LEVEL 006 AS OF 03/02/01                      
*PHASE TACKAYFA                                                                 
*                                                                               
         TITLE 'TACKACC - AYFTAB FOR TALENT CHECK PROGRAM'                      
AYFTAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        VARIABLE LENGTH TABLE OF AGENCY FLISTS                                 
*                                                                               
         DC    (FLISTABL)X'00'  THE TABLE                                       
         DC    X'FF'                                                            
*                                                                               
*TACKREPD                                                                       
*TAGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006TACKAYF   03/02/01'                                      
         END                                                                    
