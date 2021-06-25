*          DATA SET ACREPEM01  AT LEVEL 004 AS OF 11/17/17                      
*PHASE ACEM01A                                                                  
ACEM01   TITLE '- SPECS FOR APPROVAL NOTICES BY EMAIL'                          
* SUB-PROGRAM 0 - APPROVAL NOTICES                                              
*                                                                               
ACEM01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF NOREP                                                            
         ACDEF NOSUM                                                            
         ACDEF MAXLINES,46                                                      
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5,6,7,8                                          
         ACDEF H1,2,RUN      RUN ON [DATE] AT [TIME]                            
         ACDEF H1,98,REPORT  REPORT ACNN                                        
                                                                                
         ACDEF SPROG,0                                                          
         ACDEF H5,60,AC#ERRRP,20,C                                              
         ACDEF H6,60,AC#ERRRP,20,CU                                             
                                                                                
         ACDEF SPROG,1                TIMESHEETS                                
         ACDEF H5,50,AC#ANBE,30,C                                               
         ACDEF H6,50,AC#ANBE,30,CU                                              
         ACDEF H8,2,AC#RSPID,8,L                                                
         ACDEF H8,11,AC#CFNAM,15,L                                              
         ACDEF H8,27,AC#CLNAM,20,L                                              
         ACDEF H8,58,AC#NMTSA,15,L                                              
         ACDEF H8,74,AC#ETSDT,12,L                                              
         ACDEF H8,87,AC#CMTS,10,L                                               
                                                                                
         ACDEF SPROG,2                EXPENSES                                  
         ACDEF H5,50,AC#ANBE,30,C                                               
         ACDEF H6,50,AC#ANBE,30,CU                                              
         ACDEF H8,2,AC#RSPID,8,L                                                
         ACDEF H8,11,AC#CFNAM,15,L                                              
         ACDEF H8,27,AC#CLNAM,20,L                                              
         ACDEF H8,58,AC#NMEXP,15,L                                              
         ACDEF H8,74,AC#EEXDT,12,L                                              
         ACDEF H8,87,AC#CMTS,10,L                                               
                                                                                
         ACDEF SPROG,3                ORDERS                                    
         ACDEF H5,50,AC#ANBE,30,C                                               
         ACDEF H6,50,AC#ANBE,30,CU                                              
         ACDEF H8,2,AC#RSPID,8,L                                                
         ACDEF H8,11,AC#CFNAM,15,L                                              
         ACDEF H8,27,AC#CLNAM,20,L                                              
         ACDEF H8,58,AC#NUMOR,15,L                                              
         ACDEF H8,74,AC#EORD,12,L                                               
         ACDEF H8,87,AC#CMTS,10,L                                               
                                                                                
         ACDEF SPROG,4                JOBS                                      
         ACDEF H5,50,AC#ANBE,30,C                                               
         ACDEF H6,50,AC#ANBE,30,CU                                              
         ACDEF H8,2,AC#RSPID,8,L                                                
         ACDEF H8,11,AC#CFNAM,15,L                                              
         ACDEF H8,27,AC#CLNAM,20,L                                              
         ACDEF H8,58,AC#NMJOB,15,L                                              
         ACDEF H8,74,AC#EJOB,15,L                                               
         ACDEF H8,87,AC#CMTS,10,L                                               
                                                                                
         ACDEF SPROG,5                ESTIMATES                                 
         ACDEF H5,50,AC#ANBE,30,C                                               
         ACDEF H6,50,AC#ANBE,30,CU                                              
         ACDEF H8,2,AC#RSPID,8,L                                                
         ACDEF H8,11,AC#CFNAM,15,L                                              
         ACDEF H8,27,AC#CLNAM,20,L                                              
         ACDEF H8,58,AC#NUMES,15,L                                              
         ACDEF H8,74,AC#EEST,12,L                                               
         ACDEF H8,87,AC#CMTS,10,L                                               
*                                                                               
         ACDEF SPROG,6                INVOICES                                  
         ACDEF H5,50,AC#ANBE,30,C                                               
         ACDEF H6,50,AC#ANBE,30,CU                                              
         ACDEF H8,2,AC#RSPID,8,L                                                
         ACDEF H8,11,AC#CFNAM,15,L                                              
         ACDEF H8,27,AC#CLNAM,20,L                                              
         ACDEF H8,58,AC#NUMIS,15,L                                              
         ACDEF H8,74,AC#EINV,10,L                                               
         ACDEF H8,87,AC#CMTS,10,L                                               
                                                                                
         ACDEF SPROG,7                DETAILED REPORT                           
         ACDEF H5,50,AC#ARPBP,46,C                                              
         ACDEF H6,50,AC#ARPBP,46,CU                                             
                                                                                
         ACDEF SPROG,8                                                          
         ACDEF H5,50,C'LOCKED PID REPORT'                                       
         ACDEF H6,50,C'-----------------'                                       
         ACDEF H10,2,AC#RSPID,8,L                                               
         ACDEF H11,2,AC#RSPID,8,LU                                              
         ACDEF H10,11,AC#CFNAM,15,L                                             
         ACDEF H11,11,AC#CFNAM,15,LU                                            
         ACDEF H10,27,AC#CLNAM,20,L                                             
         ACDEF H11,27,AC#CLNAM,20,LU                                            
         ACDEF H10,85,AC#CMTS,10,L                                              
         ACDEF H11,85,AC#CMTS,10,LU                                             
                                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPEM01 11/17/17'                                      
         END                                                                    
