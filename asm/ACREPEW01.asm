*          DATA SET ACREPEW01  AT LEVEL 002 AS OF 01/06/17                      
*PHASE ACEW01A                                                                  
         TITLE '- SPECS FOR JOB EMAIL ALERTS FOR AURA ESTIMATES'                
*                                                                               
ACEW01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF NOREP                                                            
         ACDEF NOSUM                                                            
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN      RUN ON [DATE] AT [TIME]                            
         ACDEF H1,98,REPORT  REPORT ACNN                                        
         ACDEF H1,123,PAGE   PAGE N(NNN)                                        
                                                                                
         ACDEF SPROG,0                                                          
         ACDEF H5,60,AC#ERRRP,20,C                                              
         ACDEF H6,60,AC#ERRRP,20,CU                                             
                                                                                
         ACDEF SPROG,1                                                          
         ACDEF H5,50,AC#JEMAL,30,C                                              
         ACDEF H6,50,AC#JEMAL,30,CU                                             
         ACDEF H10,2,AC#RSPID,8,L                                               
         ACDEF H11,2,AC#RSPID,8,LU                                              
         ACDEF H10,11,AC#CFNAM,15,L                                             
         ACDEF H11,11,AC#CFNAM,15,LU                                            
         ACDEF H10,27,AC#CLNAM,20,L                                             
         ACDEF H11,27,AC#CLNAM,20,LU                                            
         ACDEF H10,50,AC#INACT,8,L                                              
         ACDEF H11,50,AC#INACT,8,LU                                             
         ACDEF H10,62,AC#PCTES,10,L                                             
         ACDEF H11,62,AC#PCTES,10,LU                                            
                                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPEW01 01/06/17'                                      
         END                                                                    
