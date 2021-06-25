*          DATA SET ACREPD801  AT LEVEL 009 AS OF 02/25/92                      
*PHASE ACD801A,+0                                                               
         TITLE 'SPECS FOR ADJUSTMENT RATE REPORT'                               
ACD801   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC MAXLINES,53                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC F1,2,REQDETS                                                     
         ASPEC H1,2,RUN                                                         
         ASPEC H3,2,C'COMPANY '                                                 
         ASPEC H4,2,C'OFFICE'                                                   
         ASPEC H5,2,C'CLIENT'                                                   
         ASPEC H6,2,C'PRODUCT'                                                  
         ASPEC H7,2,C'JOB'                                                      
         ASPEC H9,2,C'OFFICE'                                                   
         ASPEC H9,19,C'DEPARTMENT'                                              
         ASPEC H9,36,C'SUB-DEPT'                                                
         ASPEC H9,52,C'STAFF'                                                   
         ASPEC H9,73,C'TASK'                                                    
         ASPEC H9,89,C' EFF/DATE  ADJUST %  EFF/DATE  ADJUST %'                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREPD801 02/25/92'                                      
         END                                                                    
