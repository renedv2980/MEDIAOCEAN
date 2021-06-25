*          DATA SET ACREPMA01  AT LEVEL 007 AS OF 08/17/00                      
*PHASE ACMA01A                                                                  
ACMA01   TITLE '- SPECS FOR MARKER TURN AROUND REPORT'                          
ACMA01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,RECOVER                                                     
         ACDEF NOSUM                                                            
*&&UK*&& ACDEF MAXLINES,60                                                      
*&&US*&& ACDEF MAXLINES,58                                                      
         ACDEF SPROG,0,1,2,3,4,5                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,90,REPORT                                                     
         ACDEF H1,104,PAGE                                                      
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPMA01 08/17/00'                                      
         END                                                                    
