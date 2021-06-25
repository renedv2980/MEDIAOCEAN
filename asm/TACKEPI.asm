*          DATA SET TACKEPI    AT LEVEL 002 AS OF 03/02/01                      
*PHASE TACKEPIA                                                                 
*                                                                               
         TITLE 'TACKEPI - EPITAB FOR TALENT CHECK PROGRAM'                      
EPITAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF EPISODES FOR SOAP PAYMENTS                                    
*                                                                               
         ORG   *+MAXEPIS*EPILEN+1                                               
*                                                                               
*TACKREPD                                                                       
*TAGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TACKEPI   03/02/01'                                      
         END                                                                    
