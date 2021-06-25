*          DATA SET PPREP5601  AT LEVEL 003 AS OF 07/18/16                      
*PHASE PP5601A                                                                  
         TITLE 'ERR MSG AND AGENCY HDR PRINT'                                   
PP5601   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,1,2,3,4                                                        
         PSPEC H1,1,REQUESTOR                                                   
         PSPEC H1,48,C'SFM REPORT'                                              
         PSPEC H2,48,10C'-'                                                     
         PSPEC H1,93,RUN                                                        
         PSPEC H2,93,REPORT                                                     
         PSPEC H2,118,PAGE                                                      
         PSPEC H3,1,C'AGENCY HEADERS'                                           
         SPROG 1                                                                
         PSPEC H5,2,C'AGY'                                                      
         PSPEC H5,43,C'ABBV'                                                    
         PSPEC H5,51,C'EQU TO'                                                  
         PSPEC H5,116,C'DDS BILLING'                                            
         PSPEC H6,3,C'ID'                                                       
         PSPEC H6,7,C'NAME AND ADDRESS'                                         
         PSPEC H6,43,C'NAME'                                                    
         PSPEC H6,52,C'AGY'                                                     
         PSPEC H6,80,C'000000000111111111122222222223'                          
         PSPEC H6,115,C'SKIP  PRD DTL'                                          
         PSPEC H7,2,C'---'                                                      
         PSPEC H7,7,16C'-'                                                      
         PSPEC H7,43,4C'-'                                                      
         PSPEC H7,51,6C'-'                                                      
         PSPEC H7,80,C'123456789012345678901234567890'                          
         PSPEC H7,115,C'----  -------'                                          
         SPROG 2                                                                
         PSPEC H5,38,C'MEDIA COUNTERS SUMMARY'                                  
         PSPEC H6,38,C'----------------------'                                  
         PSPEC H8,02,C'MEDIA   COUNT'                                           
         PSPEC H9,02,C'-----   -----'                                           
         SPROG 3,4                                                              
         PSPEC H5,41,C'PROFILE ANALYSIS'                                        
         PSPEC H6,41,C'----------------'                                        
         SPROG 3                                                                
         PSPEC H8,02,C'CHAR    (01)    (02)    (03)    (04)    (05)   '         
         PSPEC H9,02,C'----   ------  ------  ------  ------  ------  '         
         PSPEC H8,49,C' (06)    (07)    (08)    (09)    (10)   '                
         PSPEC H9,49,C'------  ------  ------  ------  ------  '                
         PSPEC H8,89,C' (11)    (12)    (13)    (14)    (15)   '                
         PSPEC H9,89,C'------  ------  ------  ------  ------  '                
         SPROG 4                                                                
         PSPEC H8,02,C'CHAR    (16)    (17)    (18)    (19)    (20)   '         
         PSPEC H9,02,C'----   ------  ------  ------  ------  ------  '         
         PSPEC H8,49,C' (21)    (22)    (23)    (24)    (25)   '                
         PSPEC H9,49,C'------  ------  ------  ------  ------  '                
         PSPEC H8,89,C' (26)    (27)    (28)    (29)    (30)   '                
         PSPEC H9,89,C'------  ------  ------  ------  ------  '                
         SPROG 5                                                                
         PSPEC H5,33,C'OTHER CHARACTERS USED IN PROFILE'                        
         PSPEC H6,33,C'--------------------------------'                        
         PSPEC H8,02,C'HEX REPRESENTATION OF THE CHARACTER(S)'                  
         PSPEC H9,02,C'--------------------------------------'                  
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPREP5601 07/18/16'                                      
         END                                                                    
