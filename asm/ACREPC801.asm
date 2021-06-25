*          DATA SET ACREPC801  AT LEVEL 013 AS OF 01/29/92                      
*PHASE ACC801A,+0                                                               
         TITLE 'SPECS FOR CHARGE RATE REPORT'                                   
ACC801   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC MAXLINES,53                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC F1,2,REQDETS                                                     
         ASPEC H1,2,RUN                                                         
         ASPEC H2,2,C'COMPANY '                                                 
         ASPEC H3,2,C'OFFICE'                                                   
         ASPEC H4,2,C'DEPARTMENT'                                               
         ASPEC H5,2,C'SUB-DEPT'                                                 
         ASPEC H6,2,C'STAFF'                                                    
         ASPEC H8,2,C'TASK                  OFFICE'                             
         ASPEC H8,45,C'CLIENT                 PRODUCT'                          
         ASPEC H8,89,C' EFF/DATE   RATE     EFF/DATE   RATE'                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPC801 01/29/92'                                      
         END                                                                    
