*          DATA SET ACREPPM01  AT LEVEL 003 AS OF 08/17/00                      
*PHASE ACPM01A                                                                  
         TITLE 'SPECS FOR PHILIP MORRIS - BATES ESTIMATE INTERFACE'             
ACPM01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
*                                                                               
         SPROG 0,1                                                              
         FSPEC READ,ACCOUNTS                                                    
         ACDEF H1,052,C'PHILIP MORRIS ESTIMATE TAPE'                            
         ACDEF H2,052,28C'-'                                                    
         ACDEF H1,002,RUN                                                       
         ACDEF H1,101,REPORT                                                    
         ACDEF H1,117,PAGE                                                      
         ACDEF H3,002,COMPANY                                                   
         ACDEF H4,002,C'CLIENT'                                                 
         ACDEF H5,002,C'PRODUCT'                                                
         ACDEF H6,055,PERIOD                                                    
         ACDEF H8,002,C'JOB'                                                    
         ACDEF H9,002,C'CODE'                                                   
         ACDEF H10,002,C'----'                                                  
         ACDEF H8,009,C'ESTIMATE'                                               
         ACDEF H9,009,C'NUMBER'                                                 
         ACDEF H10,009,C'--------'                                              
         ACDEF H8,022,C'REVISION'                                               
         ACDEF H9,022,C'NUMBER'                                                 
         ACDEF H10,022,C'--------'                                              
         ACDEF H8,032,C'WORK'                                                   
         ACDEF H9,032,C'CODE'                                                   
         ACDEF H10,032,C'----'                                                  
         SPROG 0                                                                
         ACDEF H8,056,C'COMMISSION'                                             
         ACDEF H9,056,C'AMOUNT'                                                 
         ACDEF H10,056,C'----------'                                            
         ACDEF H8,068,C'NON-COMMISSION'                                         
         ACDEF H9,068,C'AMOUNT'                                                 
         ACDEF H10,068,C'--------------'                                        
         ACDEF H8,084,C'AGENCY'                                                 
         ACDEF H9,084,C'COMMISSION'                                             
         ACDEF H10,084,C'----------'                                            
         ACDEF H8,097,C'ESTIMATE'                                               
         ACDEF H9,097,C'DATE'                                                   
         ACDEF H10,097,C'--------'                                              
         ACDEF H8,106,C'ESTIMATE'                                               
         ACDEF H9,106,C'START'                                                  
         ACDEF H10,106,C'--------'                                              
         ACDEF H8,115,C'ESTIMATE'                                               
         ACDEF H9,115,C'CLOSE'                                                  
         ACDEF H10,115,C'--------'                                              
         ACDEF H8,125,C'FINAL'                                                  
         ACDEF H10,125,C'-----'                                                 
*                                                                               
         SPROG 1                                                                
         ACDEF H8,039,C'ESTIMATE TOTAL'                                         
         ACDEF H9,039,C'AMOUNT'                                                 
         ACDEF H10,039,C'--------------'                                        
         ACDEF H8,056,C'COMMISSION'                                             
         ACDEF H9,056,C'AMOUNT'                                                 
         ACDEF H10,056,C'----------'                                            
         ACDEF H8,068,C'NON-COMMISSION'                                         
         ACDEF H9,068,C'AMOUNT'                                                 
         ACDEF H10,068,C'--------------'                                        
         ACDEF H8,084,C'AGENCY'                                                 
         ACDEF H9,084,C'COMMISSION'                                             
         ACDEF H10,084,C'----------'                                            
         ACDEF H8,097,C'ESTIMATE'                                               
         ACDEF H9,097,C'DATE'                                                   
         ACDEF H10,097,C'--------'                                              
         ACDEF H8,106,C'ESTIMATE'                                               
         ACDEF H9,106,C'START'                                                  
         ACDEF H10,106,C'--------'                                              
         ACDEF H8,115,C'ESTIMATE'                                               
         ACDEF H9,115,C'CLOSE'                                                  
         ACDEF H10,115,C'--------'                                              
         ACDEF H8,125,C'FINAL'                                                  
         ACDEF H10,125,C'-----'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPPM01 08/17/00'                                      
         END                                                                    
