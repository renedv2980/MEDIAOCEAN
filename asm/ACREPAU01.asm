*          DATA SET ACREPAU01  AT LEVEL 003 AS OF 08/17/00                      
*PHASE ACAU01A                                                                  
ACAU01   TITLE '- SPECS FOR ACCOUNT UPDATE'                                     
ACAU01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF NOSUM                                                            
         ACDEF SPROG,0,1,2,3                                                    
         ACDEF H1,2,RUN                                                         
         ACDEF H1,81,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,44,AC#ANPTY,30,C                                              
         ACDEF H2,44,AC#ANPTY,30,CU                                             
         ACDEF H7,2,AC#UNTNM,27,L                                               
         ACDEF H8,2,AC#UNTNM,27,LU                                              
         ACDEF H7,30,AC#LGRNM,36,L                                              
         ACDEF H8,30,AC#LGRNM,36,LU                                             
         ACDEF H7,67,AC#DRS,13,R                                                
         ACDEF H8,67,AC#DRS,13,RU                                               
         ACDEF H7,82,AC#CRS,13,R                                                
         ACDEF H8,82,AC#CRS,13,RU                                               
         ACDEF H7,97,AC#BAL,13,R                                                
         ACDEF H8,97,AC#BAL,13,RU                                               
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,46,C'List of Missing Postings'                                
         ACDEF H2,46,24X'BF'                                                    
         ACDEF H7,2,C'CC Account        Of Contra-Account'                      
         ACDEF H7,38,C'Batch    Date   Refnum'                                  
         ACDEF H7,68,C'Debits        Credits  Error'                            
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H1,40,AC#LSTMP,40,C                                              
         ACDEF H2,40,AC#LSTMP,40,CU                                             
         ACDEF H7,2,AC#ACC,15,L                                                 
         ACDEF H8,2,AC#ACC,15,LU                                                
         ACDEF H7,17,AC#CTR,15,L                                                
         ACDEF H8,17,AC#CTR,15,LU                                               
         ACDEF H7,32,AC#CODES,5,C                                               
         ACDEF H8,32,AC#CODES,5,CU                                              
         ACDEF H7,38,AC#DATE,8,C                                                
         ACDEF H8,38,AC#DATE,8,CU                                               
         ACDEF H7,47,AC#DRS,13,R                                                
         ACDEF H8,47,AC#DRS,13,RU                                               
         ACDEF H7,62,AC#CRS,13,R                                                
         ACDEF H8,62,AC#CRS,13,RU                                               
         ACDEF H7,77,AC#REF,8,C                                                 
         ACDEF H8,77,AC#REF,8,CU                                                
         ACDEF H7,86,AC#NRTV,20,L                                               
         ACDEF H8,86,AC#NRTV,20,LU                                              
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPAU01 08/17/00'                                      
         END                                                                    
