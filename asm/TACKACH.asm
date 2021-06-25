*          DATA SET TACKACH    AT LEVEL 001 AS OF 01/02/15                      
*PHASE TACKACHA                                                                 
*                                                                               
         TITLE 'TACKACH - ACHTAB FOR TALENT CHECK PROGRAM'                      
ACHTAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF ACH POSTING ACCOUNTS                                          
*                                                                               
         ORG   *+MAXACHS*ACHLEN+1                                               
*                                                                               
*TACKREPD                                                                       
*TAGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TACKACH   01/02/15'                                      
         END                                                                    
