*          DATA SET ACREPDA01  AT LEVEL 007 AS OF 06/10/03                      
*PHASE ACDA01A,*                                                                
ACDA01   TITLE 'SPECS FOR FILE ACTIVITY REPORT'                                 
ACDA01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,RECOVER,DELETES                                             
         ACDEF NOSUM                                                            
*&&UK*&& ACDEF MAXLINES,60                                                      
*&&US*&& ACDEF MAXLINES,57                                                      
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
*                                                                               
         ACDEF H1,37,AC#RCVRC,28,C                                              
         ACDEF H2,37,AC#RCVRC,28,CU                                             
         ACDEF H8,2,AC#RECTY,20,L                                               
         ACDEF H9,2,AC#RECTY,20,LU                                              
         ACDEF H8,23,AC#ADDS,10,R                                               
         ACDEF H9,23,AC#ADDS,10,RU                                              
         ACDEF H8,35,AC#COPYS,10,R                                              
         ACDEF H9,35,AC#COPYS,10,RU                                             
         ACDEF H8,47,AC#CHNGS,10,R                                              
         ACDEF H9,47,AC#CHNGS,10,RU                                             
         ACDEF H8,59,AC#PTR,10,R                                                
         ACDEF H9,59,AC#COPYS,10,R                                              
         ACDEF H8,71,AC#PTR,10,R                                                
         ACDEF H9,71,AC#CHNGS,10,R                                              
         ACDEF H8,83,AC#OTHRS,10,R                                              
         ACDEF H9,83,AC#OTHRS,10,RU                                             
         ACDEF H8,95,AC#TOTAL,10,R                                              
         ACDEF H9,95,AC#TOTAL,10,RU                                             
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,REPORT                                                     
         ACDEF H1,124,PAGE                                                      
*                                                                               
         ACDEF H1,40,AC#DFACT,55,C                                              
         ACDEF H2,40,AC#DFACT,55,CU                                             
         ACDEF H6,2,AC#TIME,5,L                                                 
         ACDEF H7,2,AC#TIME,5,LU                                                
         ACDEF H6,8,AC#TML,8,L                                                  
         ACDEF H7,8,C'/'                                                        
         ACDEF H7,9,AC#PRSN,7,L                                                 
         ACDEF H6,17,AC#RECTY,20,L                                              
         ACDEF H7,17,AC#RECTY,20,LU                                             
         ACDEF H6,38,AC#RECKY,18,L                                              
         ACDEF H7,38,AC#RECKY,18,LU                                             
         ACDEF H6,57,AC#ACT,14,L                                                
         ACDEF H7,57,AC#ACT,14,LU                                               
         ACDEF H6,72,AC#DTANM,20,L                                              
         ACDEF H7,72,AC#DTANM,20,LU                                             
         ACDEF H6,93,AC#DTAVL,39,L                                              
         ACDEF H7,93,AC#DTAVL,39,LU                                             
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,REPORT                                                     
         ACDEF H1,124,PAGE                                                      
*                                                                               
         ACDEF H1,43,AC#DFAS,48,C                                               
         ACDEF H2,43,AC#DFAS,48,CU                                              
         ACDEF H8,35,AC#RECTY,20,L                                              
         ACDEF H9,35,AC#RECTY,20,LU                                             
         ACDEF H8,56,AC#ADDS,19,R                                               
         ACDEF H9,56,AC#ADDS,19,RU                                              
         ACDEF H8,77,AC#AMDS,19,R                                               
         ACDEF H9,77,AC#AMDS,19,RU                                              
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,REPORT                                                     
         ACDEF H1,124,PAGE                                                      
*                                                                               
         ACDEF H1,40,AC#BRCDS,55,C                                              
         ACDEF H2,40,AC#BRCDS,55,CU                                             
*                                                                               
         ACDEF SPROG,4                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPDA01 06/10/03'                                      
         END                                                                    
