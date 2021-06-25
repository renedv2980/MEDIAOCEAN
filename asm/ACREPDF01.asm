*          DATA SET ACREPDF01  AT LEVEL 005 AS OF 08/16/00                      
*PHASE ACDF01A,*                                                                
ACDF01   TITLE '- TRANSACTION COUNTER AND ERROR REPORT'                         
ACDF01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF READ,TRANSACTIONS                                                
         ACDEF UPDATE,ACCFIL                                                    
*&&UK*&& ACDEF MAXLINES,60                                                      
*&&US*&& ACDEF MAXLINES,58                                                      
         ACDEF SPROG,0,1,2,3,4,5                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,90,REPORT                                                     
         ACDEF H1,104,PAGE                                                      
         ACDEF H1,52,C'TRANSACTION COUNTER'                                     
         ACDEF H2,52,C'-------------------'                                     
         SPACE 1                                                                
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H4,2,C'ACCOUNT CODE'                                             
         ACDEF H5,2,C'------------'                                             
         ACDEF H4,19,C'ACCOUNT NAME'                                            
         ACDEF H5,19,C'------------'                                            
         ACDEF H4,58,C'A/C REC COUNT'                                           
         ACDEF H5,58,C'-------------'                                           
         ACDEF H4,74,C'ACTUAL COUNT'                                            
         ACDEF H5,74,C'------------'                                            
         ACDEF H4,90,C'COMMENTS'                                                
         ACDEF H5,90,C'--------'                                                
*                                                                               
*                                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPDF01 08/16/00'                                      
         END                                                                    
