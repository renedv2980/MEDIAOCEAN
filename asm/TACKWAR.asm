*          DATA SET TACKWAR    AT LEVEL 002 AS OF 03/02/01                      
*PHASE TACKWARA                                                                 
*                                                                               
         TITLE 'TACKWAR - WARTAB FOR TALENT CHECK PROGRAM'                      
WARTAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF WARNING CHECKS                                                
*                                                                               
         ORG   *+MAXWARS*WARLEN+1                                               
*                                                                               
*TACKREPD                                                                       
*TAGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TACKWAR   03/02/01'                                      
         END                                                                    
