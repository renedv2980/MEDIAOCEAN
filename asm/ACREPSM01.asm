*          DATA SET ACREPSM01  AT LEVEL 003 AS OF 11/30/12                      
*PHASE ACSM01A                                                                  
ACSM01   TITLE '- SPECS FOR SORT MERGE AND WORKER MAINTENANCE'                  
ACSM01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF NOSUM                                                            
         ACDEF SPROG,0,1,2,3,4                                                  
         ACDEF H1,2,RUN                                                         
         ACDEF H1,81,REPORT                                                     
         ACDEF H1,40,AC#SMIF,40,C                                               
         ACDEF H2,40,AC#SMIF,40,CU                                              
         ACDEF H1,99,PAGE                                                       
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF M1,2,AC#DSCIF,30,L                                               
         ACDEF M2,2,AC#DSCIF,30,LU                                              
         ACDEF M1,71,AC#DRS,12,R                                                
         ACDEF M2,71,AC#DRS,12,RU                                               
         ACDEF M1,86,AC#CRS,12,R                                                
         ACDEF M2,86,AC#CRS,12,RU                                               
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H4,2,AC#DDSSU,30,L                                               
         ACDEF H5,2,AC#DDSSU,30,LU                                              
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H4,2,AC#CPYAN,30,L                                               
         ACDEF H5,2,AC#CPYAN,30,LU                                              
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H1,40,AC#FCTKS,40,C                                              
         ACDEF H2,40,AC#FCTKS,40,CU                                             
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H1,40,AC#FCTUS,40,C                                              
         ACDEF H2,40,AC#FCTUS,40,CU                                             
*                                                                               
         ACDEF SPROG,4                                                          
         ACDEF H1,40,AC#FCTHS,40,C                                              
         ACDEF H2,40,AC#FCTHS,40,CU                                             
*                                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPSM01 11/30/12'                                      
         END                                                                    
