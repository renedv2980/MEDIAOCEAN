*          DATA SET ACREPTD01  AT LEVEL 004 AS OF 05/26/10                      
*PHASE ACTD01A                                                                  
         TITLE 'SPECS FOR TIMESHEET AUDIT REPORT'                               
* SUB-PROGRAM 0 - DOWNLOAD                                                      
* SUB-PROGRAM 1 - NON-DOWNLOAD                                                  
ACTD01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MAXLINES,56                                                      
         ACDEF WIDTH,132                                                        
         ACDEF READ,ACCOUNTS                                                    
         ACDEF GETOPT,N                                                         
                                                                                
         ACDEF SPROG,1                                                          
         ACDEF WIDTH,198                                                        
         ACDEF H1,88,C'TIMESHEET AUDIT REPORT'                                  
         ACDEF H2,88,C'----------------------'                                  
         ACDEF H3,2,RUN                                                         
         ACDEF H3,158,REPORT                                                    
         ACDEF H3,175,PAGE                                                      
         ACDEF H4,2,COMPANY                                                     
         ACDEF H5,2,AC#PRSN,6                                                   
         ACDEF H6,2,AC#DATE,4                                                   
         ACDEF H9,7,C' ----------------FROM---------------- '                   
         ACDEF H9,45,C' -----------------TO------------------ '                 
         ACDEF H10,2,AC#ROW,3                                                   
         ACDEF H10,7,AC#TYPE,3                                                  
         ACDEF H10,11,AC#ACC,7                                                  
         ACDEF H10,27,AC#WC,2                                                   
         ACDEF H10,30,AC#HOURS,5                                                
         ACDEF H10,38,AC#MOA,3                                                  
         ACDEF H10,46,AC#TYPE,3                                                 
         ACDEF H10,51,AC#ACC,7                                                  
         ACDEF H10,67,AC#WC,2                                                   
         ACDEF H10,70,AC#HOURS,5                                                
         ACDEF H10,78,AC#MOA,3                                                  
         ACDEF H10,86,AC#STT,6                                                  
         ACDEF H10,104,AC#TSACT,16                                              
         ACDEF H10,123,AC#TLACT,16                                              
         ACDEF H10,159,AC#RSPID,3                                               
         ACDEF H10,169,AC#CPY,8                                                 
         ACDEF H10,179,AC#ACTYD,7                                               
         ACDEF F1,2,REQDETS                                                     
*                                                                               
         ACDEF SPROG,0                                                          
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPTD01 05/26/10'                                      
         END                                                                    
