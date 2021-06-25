*          DATA SET ACREPIT01  AT LEVEL 031 AS OF 08/17/00                      
*PHASE ACIT01A                                                                  
         TITLE 'CME/TEXACO BILLING INTERFACE TAPE'                              
ACIT01   CSECT                                                                  
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
         ASPEC H1,58,C'TEXACO INTERFACE'                                        
         ASPEC H2,58,C'----------------'                                        
         ASPEC H3,2,COMPANY                                                     
         ASPEC H3,87,PERIOD                                                     
*                                                                               
         SPROG 0,2                                                              
         ASPEC H8,2,C'  ACCOUNT/'                                               
         ASPEC H9,2,C'  VENDOR NAME'                                            
         ASPEC H8,23,C'USER FIELD'                                              
         ASPEC H8,49,C'BILL'                                                    
         ASPEC H9,49,C'NUMBER'                                                  
         ASPEC H8,56,C'BILL'                                                    
         ASPEC H9,56,C'DATE'                                                    
*        ASPEC H8,55,C'DUE '                                                    
*        ASPEC H9,55,C'DATE'                                                    
         ASPEC H8,65,C'WORK'                                                    
         ASPEC H9,65,C'CODE'                                                    
         ASPEC H8,70,C'NET'                                                     
         ASPEC H8,86,C'COMMISSION'                                              
         ASPEC H8,103,C'GROSS    '                                              
*                                                                               
         SPROG 2                                                                
         ASPEC H8,103,C'CASH DISC'                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACREPIT01 08/17/00'                                      
         END                                                                    
