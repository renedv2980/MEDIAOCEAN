*          DATA SET ACREPFE01  AT LEVEL 001 AS OF 06/25/18                      
*PHASE ACFE01A                                                                  
ACFE01   TITLE 'SPECS FOR FILE ACTIVITY REPORT'                                 
ACFE01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,RECOVER,DELETES                                             
         ACDEF NOSUM                                                            
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,50,AC#DFAS,40,L                                               
         ACDEF H2,50,AC#DFAS,40,LU                                              
         ACDEF H1,98,REPORT                                                     
         ACDEF H1,123,PAGE                                                      
         ACDEF H2,2,COMPANY                                                     
         ACDEF H2,100,REQUESTOR                                                 
*                                                                               
         ACDEF SPROG,0                                                          
*                                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPFE01 06/25/18'                                      
         END                                                                    
