*          DATA SET TACKERI    AT LEVEL 002 AS OF 03/02/01                      
*PHASE TACKERIA                                                                 
*                                                                               
         TITLE 'TACKERI - ERITAB FOR TALENT CHECK PROGRAM'                      
ERITAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF ERROR INVOICES                                                
*                                                                               
         ORG   *+MAXERIS*ERILEN+1                                               
*                                                                               
*TACKREPD                                                                       
*TAGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TACKERI   03/02/01'                                      
         END                                                                    
