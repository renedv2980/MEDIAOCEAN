*          DATA SET ACREPBT01  AT LEVEL 026 AS OF 09/19/96                      
*PHASE ACBT01A,*                                                                
         TITLE 'BANKERS TRUST BILLING INTERFACE TAPE'                           
ACBT01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         SPROG 0,1                                                              
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PRORATA,BILL                                                     
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,87,REPORT                                                     
         ASPEC H1,102,PAGE                                                      
         ASPEC H1,54,C'BANKERS TRUST INTERFACE'                                 
         ASPEC H2,54,C'-----------------------'                                 
         ASPEC H3,2,COMPANY                                                     
         ASPEC H3,55,C'RUN MONTH:'                                              
         ASPEC H5,87,PERIOD                                                     
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H5,1,C'CONTACT NAME:'                                            
         SPROG 0,2                                                              
         ASPEC H7,2,C'INVOICE NUMBER'                                           
         ASPEC H7,17,C'INVOICE DATE'                                            
         ASPEC H7,30,C'DESCRIPTION'                                             
         ASPEC H7,71,C'EXPENSE CODE'                                            
         ASPEC H7,84,C'ACCOUNT CODE'                                            
         ASPEC H7,97,C'    NET    '                                             
         ASPEC H7,109,C'    TAX    '                                            
         ASPEC H7,121,C'TOTAL COST '                                            
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026ACREPBT01 09/19/96'                                      
         END                                                                    
