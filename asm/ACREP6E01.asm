*          DATA SET ACREP6E01  AT LEVEL 004 AS OF 03/20/07                      
*PHASE AC6E01A                                                                  
         TITLE 'SPECS: MCS ESTIMATE LIST/DETAILS'                               
AC6E01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MAXLINES,56                                                      
         ACDEF READ,ACCOUNTS                                                    
*                                                                               
         ACDEF SPROG,1,2,3                                                      
         ACDEF H3,2,RUN                                                         
         ACDEF H3,101,REPORT                                                    
         ACDEF H3,115,PAGE                                                      
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,101,REQUESTOR                                                 
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H5,2,AC#CLINT,7                                                  
         ACDEF H6,2,AC#PRO,7                                                    
         ACDEF H7,2,AC#JOB,7                                                    
         ACDEF H9,2,AC#NUM,4,L                                                  
         ACDEF H9,7,AC#ESTNO,7,L                                                
         ACDEF H9,15,AC#DATE,9,L                                                
         ACDEF H9,25,AC#STT,3,L                                                 
         ACDEF H9,29,AC#SCM,9,L                                                 
         ACDEF H9,39,AC#MEDC,2,L                                                
         ACDEF H9,43,AC#AMT,11,R                                                
         ACDEF H9,57,AC#FRMAT,9,L                                               
         ACDEF H9,67,AC#DESC,20,L                                               
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H7,2,AC#ESTNO,7,L                                                
         ACDEF H7,10,AC#DATE,9,L                                                
         ACDEF H7,20,AC#STT,3,L                                                 
         ACDEF H7,24,AC#OFF,3,L                                                 
         ACDEF H7,28,AC#CLI,6,L                                                 
         ACDEF H7,35,AC#PRO,3,L                                                 
         ACDEF H7,39,AC#JOB,7,L                                                 
         ACDEF H7,47,AC#NUM,4,L                                                 
         ACDEF H7,52,AC#SCM,9,L                                                 
         ACDEF H7,62,AC#AMT,11,R                                                
         ACDEF H7,76,AC#FRMAT,9,L                                               
         ACDEF H7,86,AC#DESC,20,L                                               
*                                                                               
         ACDEF SPROG,1,2,3                                                      
         ACDEF H1,50,AC#ESTL,15,L                                               
         ACDEF H2,50,AC#ESTL,15,LU                                              
*                                                                               
         ACDEF SPROG,0                                                          
*                                                                               
* ACDDEQUS                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREP6E01 03/20/07'                                      
         END                                                                    
