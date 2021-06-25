*          DATA SET ACREPC201  AT LEVEL 010 AS OF 08/16/00                      
*PHASE ACC201A,+0                                                               
         TITLE 'SPECS FOR TIME SHEET EDIT'                                      
         PRINT NOGEN                                                            
ACC201   CSECT                                                                  
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,48,C'TIME SHEET EDIT'                                         
         ASPEC H2,48,C'---------------'                                         
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H1,47,C'PROJECT TIME EDIT'                                       
         ASPEC H2,47,C'-----------------'                                       
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H1,47,C'MISSING SALARY EDIT'                                     
         ASPEC H2,47,C'-------------------'                                     
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,81,REPORT                                                     
         ASPEC H1,95,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,81,C'PERIOD'                                                  
         ASPEC H5,2,C'OFFICE'                                                   
         ASPEC H6,2,C'DEPT'                                                     
         ASPEC H5,81,REQUESTOR                                                  
         ASPEC H9,2,C'PERSON/TIME CATEGORY'                                     
         ASPEC H10,2,20C'-'                                                     
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,48,C'SUBSEQUENT HIRES'                                        
         ASPEC H1,81,REPORT                                                     
         ASPEC H1,95,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H2,48,16C'-'                                                     
         ASPEC H4,81,C'PERIOD'                                                  
         ASPEC H5,81,REQUESTOR                                                  
         ASPEC H7,2,C'OFFICE/DEPT./SUB-DEPT./STAFF NO.'                         
         ASPEC H7,36,C'NAME'                                                    
         ASPEC H7,74,C'HIRE DATE'                                               
         ASPEC H8,2,32C'-'                                                      
         ASPEC H8,36,4C'-'                                                      
         ASPEC H8,74,9C'-'                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREPC201 08/16/00'                                      
         END                                                                    
