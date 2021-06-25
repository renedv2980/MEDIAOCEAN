*          DATA SET TACKCLF    AT LEVEL 007 AS OF 03/02/01                      
*PHASE TACKCLFA                                                                 
*                                                                               
         TITLE 'TACKACC - CLFTAB FOR TALENT CHECK PROGRAM'                      
CLFTAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        VARABILE LENGTH TABLE OF CLIENT FLISTS                                 
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
**PAN#1  DC    CL21'007TACKCLF   03/02/01'                                      
         END                                                                    
