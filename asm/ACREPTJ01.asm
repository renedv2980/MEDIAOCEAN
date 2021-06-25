*          DATA SET ACREPTJ01  AT LEVEL 012 AS OF 08/16/00                      
*PHASE ACTJ01A,*                                                                
ACTI01   TITLE '- SPECS FOR TIMESHEET TURN AROUND REPORT'                       
ACTI01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF READ,RECOVER                                                     
         ACDEF NOSUM                                                            
         ACDEF MAXLINES,58                                                      
         ACDEF RESET                                                            
                                                                                
         ACDEF SPROG,0,1,2,3,4,5                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,68,AC#TJACT,12,L                                              
         ACDEF H2,68,AC#TJACT,12,LU                                             
         ACDEF H1,135,REPORT                                                    
*                                                                               
         ACDEF H2,2,AC#PERD,6,L                                                 
         ACDEF H2,135,PAGE                                                      
*                                                                               
         ACDEF H3,2,AC#PRSN,6,L                                                 
*                                                                               
         ACDEF H4,2,AC#OFFC,6,L                                                 
*                                                                               
         ACDEF H5,2,AC#DPT,4,L                                                  
*                                                                               
         ACDEF H6,2,AC#SUBDP,8,L                                                
*                                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACREPTJ01 08/16/00'                                      
         END                                                                    
