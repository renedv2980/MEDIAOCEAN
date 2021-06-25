*          DATA SET ACREP8401  AT LEVEL 006 AS OF 04/15/99                      
*PHASE AC8401A,+0                                                               
         TITLE 'SPECS FOR OPEN ITEM TRIAL BALANCE'                              
AC8401   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         SPROG 0,1                                                              
         ASPEC H2,2,RUN                                                         
         ASPEC H2,44,C'OPEN ITEM TRIAL BALANCE'                                 
         ASPEC H2,86,REPORT                                                     
         ASPEC H2,100,PAGE                                                      
         ASPEC H3,44,23C'-'                                                     
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC H5,2,C'UNIT'                                                     
         ASPEC H5,86,C'MONTH OF'                                                
         ASPEC H6,2,C'LEDGER'                                                   
         ASPEC H8,2,C'ACCOUNT CODE  ACCOUNT NAME'                               
         ASPEC H9,2,C'------------  ------------'                               
         ASPEC H8,46,C'OPENING'                                                 
         ASPEC H9,46,C'BALANCE'                                                 
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H8,61,C'DEBITS       CREDITS       CLOSING'                      
         ASPEC H9,61,C'------       -------       BALANCE'                      
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H8,60,C'BILLING        CASH     ADJUSTMENTS'                     
         ASPEC H9,60,C'-------      RECEIPTS   -----------'                     
         ASPEC H8,102,C'CLOSING'                                                
         ASPEC H9,102,C'BALANCE'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREP8401 04/15/99'                                      
         END                                                                    
