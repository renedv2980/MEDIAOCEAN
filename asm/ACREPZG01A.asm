*          DATA SET ACREPZG01A AT LEVEL 007 AS OF 03/08/00                      
*PHASE ACZG01A,+0                                                               
         TITLE 'FILE MERGE'                                                     
ACZG01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,070,C'FILE MERGE'                                             
         ACDEF H2,143,PAGE                                                      
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H5,004,C'MERGED'                                                 
         ACDEF H6,003,C'CLI'                                                    
         ACDEF H7,003,C'CODE'                                                   
         ACDEF H6,008,C'PRD'                                                    
         ACDEF H7,008,C'CODE'                                                   
         ACDEF H5,014,C'AGENCY'                                                 
         ACDEF H6,013,C'CLI'                                                    
         ACDEF H7,013,C'CODE'                                                   
         ACDEF H6,018,C'PRD'                                                    
         ACDEF H7,018,C'CODE'                                                   
         ACDEF H7,023,C'NAME'                                                   
         ACDEF H6,048,C'AGY'                                                    
         ACDEF H7,048,C'CODE'                                                   
         ACDEF H7,053,C'AGENCY NAME'                                            
         ACDEF H7,091,C'SYS'                                                    
         ACDEF H7,096,C'SR-RECEIVABLE'                                          
         ACDEF H7,110,C'1C-COSTING'                                             
         ACDEF H7,124,C'RESULT'                                                 
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H5,003,C'AGN'                                                    
         ACDEF H6,003,C'CLI'                                                    
         ACDEF H7,003,C'CODE'                                                   
         ACDEF H5,008,C'NEW'                                                    
         ACDEF H6,008,C'CLI'                                                    
         ACDEF H7,008,C'CODE'                                                   
         ACDEF H7,014,C'NAME'                                                   
*        ACDEF H7,042,C'OM'                                                     
*        ACDEF H7,054,C'JW'                                                     
*        ACDEF H7,066,C'DW'                                                     
*        ACDEF H7,077,C'DF'                                                     
*        ACDEF H7,089,C'DT'                                                     
*        ACDEF H7,101,C'SF'                                                     
         ACDEF H7,111,C'RESULT'                                                 
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H5,003,C'AGN'                                                    
         ACDEF H6,003,C'CLI'                                                    
         ACDEF H7,003,C'CODE'                                                   
         ACDEF H5,008,C'NEW'                                                    
         ACDEF H6,008,C'CLI'                                                    
         ACDEF H7,008,C'CODE'                                                   
         ACDEF H7,014,C'NAME'                                                   
         ACDEF H6,038,C'AGN'                                                    
         ACDEF H7,038,C'CODE'                                                   
         ACDEF H7,049,C'ACC'                                                    
         ACDEF H7,066,C'SPOT'                                                   
         ACDEF H7,083,C'NET'                                                    
         ACDEF H7,100,C'PRINT'                                                  
         ACDEF H7,112,C'RESULT'                                                 
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H7,003,C'U/L'                                                    
         ACDEF H7,010,C'TYPE'                                                   
         ACDEF H7,016,C'OFFICE'                                                 
         ACDEF H6,023,C'CLI'                                                    
         ACDEF H7,023,C'CODE'                                                   
         ACDEF H6,031,C'PRD'                                                    
         ACDEF H7,031,C'CODE'                                                   
         ACDEF H7,041,C'NAME'                                                   
*                                                                               
         ACDEF SPROG,4                                                          
         ACDEF H5,003,C'CLIENT'                                                 
         ACDEF H6,004,C'CODE'                                                   
         ACDEF H5,011,C'CLIENT'                                                 
         ACDEF H6,012,C'NAME'                                                   
         ACDEF H5,036,C'SOURCE'                                                 
         ACDEF H6,036,C'AGENCY'                                                 
*                                                                               
         ACDEF SPROG,5                                                          
         ACDEF H5,006,C'SJ TOTAL'                                               
         ACDEF H5,021,C'SR TOTAL'                                               
         ACDEF H5,036,C'1C TOTAL'                                               
         ACDEF H5,051,C'CT TOTAL'                                               
         ACDEF H5,066,C'GRAND TOTAL'                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPZG01A03/08/00'                                      
         END                                                                    
