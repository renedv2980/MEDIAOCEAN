*          DATA SET ACREPFU01  AT LEVEL 002 AS OF 08/17/00                      
*PHASE ACFU01A                                                                  
         TITLE 'ACFU01 - SPECS FOR FILE UPLOAD PROGRAM'                         
ACFU01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF GETOPT,N                                                         
         ACDEF WIDTH,198                                                        
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,143,C'FILE UPLOAD'                                            
         ACDEF H2,143,PAGE                                                      
*                                                                               
         ACDEF H6,002,C'ACCOUNT CODE '                                          
         ACDEF H6,017,C'ACCOUNT NAME'                                           
         ACDEF H6,054,C'ADDRESS'                                                
         ACDEF H5,081,C'FILTR'                                                  
         ACDEF H6,081,C'12345'                                                  
         ACDEF H6,087,C'P'                                                      
         ACDEF H6,089,C'OF'                                                     
         ACDEF H6,092,C'OPTIONS'                                                
         ACDEF H6,143,C'ERRORS'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPFU01 08/17/00'                                      
         END                                                                    
