*          DATA SET ACREPIC01  AT LEVEL 007 AS OF 08/17/00                      
*PHASE ACIC01A                                                                  
         TITLE 'ACIC01 - SPECS FOR - COKE EXPENDITURE INTERFACE'                
ACIC01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         RSPEC MAXLINES,54                                                      
         SPROG 0,1,2,3,4,5,6                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,80,REPORT                                                     
         ASPEC H1,98,PAGE                                                       
         ASPEC H2,80,REQUESTOR                                                  
         ASPEC H5,80,C'*'          AGENCY,BOTTLER,ETC. BOUGHT                   
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H3,2,C'INVOICE NUMBER    '                                       
         ASPEC H4,2,C'TRANSMISSION MONTH'                                       
         SPACE 1                                                                
         SPROG 0,1,2,5,6                                                        
         ASPEC H4,80,C'PERIOD'                                                  
         SPACE 1                                                                
         SPROG 0,1,3,4,5,6                                                      
         ASPEC H7,9,C'PRODUCT             MEDIA'                                
         ASPEC H7,80,C'EST  NET LESS CD    COMMISSION  GROSS LESS CD'           
         SPACE 1                                                                
         SPROG 0,3,5                                                            
**       ASPEC H5,2,C'BOTTLER           '                                       
         ASPEC H7,47,C'VEHICLE'                                                 
         ASPEC H7,62,C'MARKET'                                                  
         ASPEC H7,69,C'CAT'                                                     
         ASPEC H7,73,C'MONTH'                                                   
         SPACE 1                                                                
         SPROG 0,5                                                              
         ASPEC H1,44,C'COCA COLA EXPENDITURES '                                 
         ASPEC H2,44,C'---------------------- '                                 
         ASPEC H3,44,C'                       '                                 
         ASPEC H4,44,C'    BILLING REPORT     '                                 
         ASPEC H5,44,C'                       '                                 
         SPACE 1                                                                
         SPROG 3,4                                                              
         ASPEC H1,44,C'UNAPPROVED ITEMS REPORT'                                 
         ASPEC H2,44,C'-----------------------'                                 
         ASPEC H3,44,C'                       '                                 
         ASPEC H5,44,C'                       '                                 
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H4,44,C'                       '                                 
         SPACE 1                                                                
         SPROG 4                                                                
         ASPEC H4,44,C'    - SUMMARY -        '                                 
         SPACE 1                                                                
         SPROG 6                                                                
         ASPEC H1,44,C'    * SUMMARY *        '                                 
         ASPEC H3,44,C'                       '                                 
         ASPEC H4,44,C'                       '                                 
         ASPEC H5,44,C'                       '                                 
         SPACE 1                                                                
         SPROG 1,2                                                              
         ASPEC H1,44,C'    * INVOICE *        '                                 
         ASPEC H3,44,C'MCCANN ERICKSON, INC.  '                                 
         ASPEC H4,44,C'485 LEXINGTON AVENUE   '                                 
         ASPEC H5,44,C'NEW YORK, N. Y.  10017 '                                 
         SPACE 1                                                                
         SPROG 1,2,6                                                            
         ASPEC H2,44,C'                       '                                 
         ASPEC H5,2,C'                  '                                       
         ASPEC H7,47,C'                    '                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPIC01 08/17/00'                                      
         END                                                                    
