*          DATA SET ACREPIR01  AT LEVEL 007 AS OF 08/17/00                      
*PHASE ACIR01A                                                                  
         TITLE 'R. J. REYNOLDS - BILLING: NEW, REVISED AND CLOSED ESTIMA        
               ATES'                                                            
ACIR01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF RESET                                                            
         ACDEF PRORATA,BILL                                                     
         SPACE 1                                                                
         SPROG 0,1,2,3,4,5,6,7,8                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,87,REPORT                                                     
         ASPEC H1,102,PAGE                                                      
         ASPEC H3,87,PERIOD                                                     
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H3,02,C'NEW ESTIMATES'                                           
         ASPEC H4,02,13C'-'                                                     
         ASPEC H5,02,C' JOB NUMBER '                                            
         ASPEC H6,02,12C'-'                                                     
         ASPEC H5,16,C'ESTIMATE NO.'                                            
         ASPEC H6,16,12C'-'                                                     
         ASPEC H5,30,C'ESTIMATE TOTAL'                                          
         ASPEC H6,30,14C'-'                                                     
         ASPEC H5,46,C'DESCRIPTION'                                             
         ASPEC H6,46,30C'-'                                                     
         ASPEC H5,78,C'PROMOTION NO'                                            
         ASPEC H6,78,12C'-'                                                     
         ASPEC H5,92,C'PPQT'                                                    
         ASPEC H6,92,4C'-'                                                      
         ASPEC H5,100,C'     ERROR MESSAGES     '                               
         ASPEC H6,100,24C'-'                                                    
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H3,02,C'REVISED ESTIMATES'                                       
         ASPEC H4,02,17C'-'                                                     
         ASPEC H5,02,C' JOB NUMBER '                                            
         ASPEC H6,02,12C'-'                                                     
         ASPEC H5,16,C'ESTIMATE NO.'                                            
         ASPEC H6,16,12C'-'                                                     
         ASPEC H5,30,C'ESTIMATE TOTAL'                                          
         ASPEC H6,30,14C'-'                                                     
         ASPEC H5,78,C'REV'                                                     
         ASPEC H6,78,3C'-'                                                      
         ASPEC H5,100,C'     ERROR MESSAGES     '                               
         ASPEC H6,100,24C'-'                                                    
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H3,02,C'BILLING'                                                 
         ASPEC H4,02,07C'-'                                                     
         ASPEC H5,02,C' JOB NUMBER '                                            
         ASPEC H6,02,12C'-'                                                     
         ASPEC H5,16,C'ESTIMATE NO.'                                            
         ASPEC H6,16,12C'-'                                                     
         ASPEC H5,30,C'BILLING TOTAL'                                           
         ASPEC H6,30,13C'-'                                                     
         ASPEC H5,78,C'PAYDATE'                                                 
         ASPEC H6,78,7C'-'                                                      
         ASPEC H5,90,C'INVOICE'                                                 
         ASPEC H6,90,7C'-'                                                      
         ASPEC H5,100,C'     ERROR MESSAGES     '                               
         ASPEC H6,100,24C'-'                                                    
         SPACE 1                                                                
         SPROG 4                                                                
         ASPEC H3,02,C'CLOSED ESTIMATES'                                        
         ASPEC H4,02,16C'-'                                                     
         ASPEC H5,02,C' JOB NUMBER '                                            
         ASPEC H6,02,12C'-'                                                     
         ASPEC H5,16,C'ESTIMATE NO.'                                            
         ASPEC H6,16,12C'-'                                                     
         ASPEC H5,100,C'     ERROR MESSAGES     '                               
         ASPEC H6,100,24C'-'                                                    
         SPACE 1                                                                
         SPROG 5                                                                
         ASPEC H3,02,C'NEW ESTIMATE DETAIL'                                     
         ASPEC H4,02,19C'-'                                                     
         SPACE 1                                                                
         SPROG 6                                                                
         ASPEC H3,02,C'REVISED ESTIMATE DETAIL'                                 
         ASPEC H4,02,23C'-'                                                     
         SPACE 1                                                                
         SPROG 7                                                                
         ASPEC H3,02,C'BILLING DETAIL'                                          
         ASPEC H4,02,14C'-'                                                     
         SPACE 1                                                                
         SPROG 5,6,7                                                            
         ASPEC H5,02,C' JOB NUMBER '                                            
         ASPEC H6,02,12C'-'                                                     
         ASPEC H5,16,C'ESTIMATE NO.'                                            
         ASPEC H6,16,12C'-'                                                     
         ASPEC H5,30,C'WKCD'                                                    
         ASPEC H6,30,4C'-'                                                      
         ASPEC H5,36,C'      AMOUNT'                                            
         ASPEC H6,36,13C'-'                                                     
         ASPEC H5,100,C'     ERROR MESSAGES     '                               
         ASPEC H6,100,24C'-'                                                    
         SPACE 1                                                                
         SPROG 8                                                                
         ASPEC H3,02,C'COMMENTS'                                                
         ASPEC H4,02,8C'-'                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPIR01 08/17/00'                                      
         END                                                                    
