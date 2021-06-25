*          DATA SET ACREPRM01  AT LEVEL 003 AS OF 12/05/19                      
*PHASE ACRM01A                                                                  
ACRM01   TITLE '- SPECS FOR REMINDER NOTICES BY EMAIL'                          
* SUB-PROGRAM 0 - REMINDER NOTICES                                              
*                                                                               
ACRM01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF NOREP                                                            
         ACDEF NOSUM                                                            
*                                                                               
         ACDEF SPROG,0,1,2                                                      
         ACDEF H1,2,RUN      RUN ON [DATE] AT [TIME]                            
         ACDEF H1,98,REPORT  REPORT ACNN                                        
         ACDEF H1,123,PAGE   PAGE N(NNN)                                        
                                                                                
         ACDEF SPROG,1                                                          
         ACDEF H5,50,AC#TSOVD,30,C                                              
         ACDEF H6,50,AC#TSOVD,30,CU                                             
         ACDEF H10,2,AC#RSPID,8,L                                               
         ACDEF H11,2,AC#RSPID,8,LU                                              
         ACDEF H10,11,AC#CFNAM,15,L                                             
         ACDEF H11,11,AC#CFNAM,15,LU                                            
         ACDEF H10,27,AC#CLNAM,20,L                                             
         ACDEF H11,27,AC#CLNAM,20,LU                                            
         ACDEF H10,57,AC#NMTSA,10,L                                             
         ACDEF H11,57,AC#NMTSA,10,LU                                            
         ACDEF H10,68,AC#ETSDT,15,L                                             
         ACDEF H11,68,AC#ETSDT,15,LU                                            
         ACDEF H10,85,AC#CMTS,10,L                                              
         ACDEF H11,85,AC#CMTS,10,LU                                             
                                                                                
         ACDEF SPROG,2                                                          
         ACDEF H5,50,C'LOCKED PID REPORT'                                       
         ACDEF H6,50,C'-----------------'                                       
         ACDEF H10,2,AC#RSPID,8,L                                               
         ACDEF H11,2,AC#RSPID,8,LU                                              
         ACDEF H10,11,AC#CFNAM,15,L                                             
         ACDEF H11,11,AC#CFNAM,15,LU                                            
         ACDEF H10,27,AC#CLNAM,20,L                                             
         ACDEF H11,27,AC#CLNAM,20,LU                                            
         ACDEF H10,69,C'LATEST TS'                                              
         ACDEF H11,69,C'---------'                                              
         ACDEF H10,85,AC#CMTS,10,L                                              
         ACDEF H11,85,AC#CMTS,10,LU                                             
*                                        DOWNLOADABLE REPORT                    
         ACDEF SPROG,4                                                          
                                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPRM01 12/05/19'                                      
         END                                                                    
