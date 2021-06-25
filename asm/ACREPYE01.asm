*          DATA SET ACREPYE01  AT LEVEL 016 AS OF 08/17/00                      
*PHASE ACYE01A                                                                  
ACYE01   TITLE 'SPECS FOR FILE ACTIVITY REPORT'                                 
ACYE01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,RECOVER                                                     
         ACDEF NOSUM                                                            
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H1,40,C'Recovery record counts'                                  
         ACDEF H2,40,C'----------------------'                                  
         ACDEF M1,4,C'Record type'                                              
         ACDEF M1,44,C'Input'                                                   
         ACDEF M1,60,C'Purged'                                                  
         ACDEF M1,77,C'Output'                                                  
         ACDEF SPROG,0                                                          
         ACDEF H5,4,C'Company'                                                  
         ACDEF SPROG,1                                                          
         ACDEF H5,4,C'File totals'                                              
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACREPYE01 08/17/00'                                      
         END                                                                    
