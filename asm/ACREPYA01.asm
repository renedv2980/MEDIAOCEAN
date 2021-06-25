*          DATA SET ACREPYA01  AT LEVEL 003 AS OF 08/17/00                      
*PHASE ACYA01A                                                                  
ACYA01   TITLE '- LIST COMPANY RECORDS'                                         
ACYA01   CSECT                                                                  
         PRINT NOGEN                                                            
                                                                                
         ACDEF SPROG,1,2,3,4,5,6,7,8,9,10                                       
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H3,83,REQUESTOR                                                  
                                                                                
         ACDEF SPROG,1                                                          
         ACDEF H1,45,C'Company analysis'                                        
         ACDEF H2,45,C'----------------'                                        
         ACDEF H5,2,C'HC'                                                       
         ACDEF H5,5,C'AG'                                                       
         ACDEF H5,8,C'Company name'                                             
         ACDEF H5,45,C'User-ID'                                                 
         ACDEF H5,56,C'Profile values'                                          
                                                                                
         ACDEF SPROG,2                                                          
         ACDEF H1,45,C'Company data analysis'                                   
         ACDEF H2,45,C'---------------------'                                   
         ACDEF H5,2,C'DataName'                                                 
         ACDEF H5,11,C'N/Dfl'                                                   
         ACDEF H5,17,C'Y/Set'                                                   
         ACDEF H5,23,C'Comment'                                                 
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPYA01 08/17/00'                                      
         END                                                                    
