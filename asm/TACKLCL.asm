*          DATA SET TACKLCL    AT LEVEL 002 AS OF 03/02/01                      
*PHASE TACKLCLA                                                                 
*                                                                               
         TITLE 'TACKLCL - LCLTAB FOR TALENT CHECK PROGRAM'                      
LCLTAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF UNION LOCALS WE DON'T SEND CHECKS TO DIRECTLY                 
*                                                                               
         ORG   *+MAXLCLS*6+1                                                    
*                                                                               
*TACKREPD                                                                       
*TAGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TACKLCL   03/02/01'                                      
         END                                                                    
