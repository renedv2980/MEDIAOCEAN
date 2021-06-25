*          DATA SET ACREPIG01  AT LEVEL 002 AS OF 11/01/04                      
*PHASE ACIG01A                                                                  
         TITLE 'COLGATE BILLING INTERFACE TAPE'                                 
ACIG01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         SPROG 0,1                                                              
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PRORATA,BILL                                                     
         ACDEF RESET                                                            
*                                                                               
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,87,REPORT                                                     
         ASPEC H1,102,PAGE                                                      
         ASPEC H1,58,C'COLGATE INTERFACE'                                       
         ASPEC H2,58,C'-----------------'                                       
         ASPEC H3,2,COMPANY                                                     
         ASPEC H3,87,PERIOD                                                     
*                                                                               
         SPROG 0                                                                
         ASPEC H8,3,C'  ACCOUNT/'                                               
         ASPEC H9,3,C'  VENDOR NAME'                                            
         ASPEC H8,24,C'USER FIELD'                                              
         ASPEC H8,50,C'BILL'                                                    
         ASPEC H9,50,C'NUMBER'                                                  
         ASPEC H8,60,C'BILL'                                                    
         ASPEC H9,60,C'DATE'                                                    
         ASPEC H8,69,C'WORK'                                                    
         ASPEC H9,69,C'CODE'                                                    
         ASPEC H8,74,C'NET'                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPIG01 11/01/04'                                      
         END                                                                    
