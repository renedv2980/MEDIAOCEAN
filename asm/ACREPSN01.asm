*          DATA SET ACREPSN01  AT LEVEL 001 AS OF 11/15/12                      
*PHASE ACSN01A                                                                  
         TITLE '- SPECS FOR NEW INTERFACE TO SORT/MERGE'                        
ACSN01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF NOSUM                                                            
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,81,REPORT                                                     
         ACDEF H1,40,AC#ACISM,40,C                                              
         ACDEF H2,40,AC#ACISM,40,CU                                             
         ACDEF H1,99,PAGE                                                       
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,40,AC#WKPAS,40,C                                              
         ACDEF H2,40,AC#WKPAS,40,CU                                             
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPSN01 11/15/12'                                      
         END                                                                    
