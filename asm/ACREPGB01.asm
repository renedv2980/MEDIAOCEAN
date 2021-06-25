*          DATA SET ACREPGB01  AT LEVEL 002 AS OF 03/12/20                      
*PHASE ACGB01C,*                                                                
         TITLE 'Specs for General Ledger bucket report'                         
ACGB01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF RESET                                                            
         ACDEF WIDTH,132                                                        
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5,6                                              
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H2,2,MOSFILT                                                     
         ACDEF H2,86,REQUESTOR                                                  
         ACDEF H4,2,COMPANY                                                     
         ACDEF F1,2,REQDETS                                                     
*                                                                               
         ACDEF H1,53,C'GL Balance check report'                                 
         ACDEF H6,2,C'Account'                                                  
         ACDEF H7,2,C'-------'                                                  
         ACDEF H6,17,C'MOA'                                                     
         ACDEF H7,17,C'---'                                                     
         ACDEF H6,24,C'OF'                                                      
         ACDEF H7,24,C'--'                                                      
         ACDEF H6,27,C'Sub-account'                                             
         ACDEF H7,27,C'-----------'                                             
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H5,46,C'------------Special--------------'                       
         ACDEF H6,46,C'------------Records--------------'                       
         ACDEF H7,46,C'       Debits       Credits      '                       
         ACDEF H5,82,C'-------------Unit G--------------'                       
         ACDEF H6,82,C'----------Transactions-----------'                       
         ACDEF H7,82,C'       Debits       Credits      '                       
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H5,46,C'-------------Unit S--------------'                       
         ACDEF H6,46,C'----------Transactions-----------'                       
         ACDEF H7,46,C'       Debits       Credits      '                       
         ACDEF H5,82,C'-------------Unit G--------------'                       
         ACDEF H6,82,C'----------Transactions-----------'                       
         ACDEF H7,82,C'       Debits       Credits      '                       
         ACDEF H5,116,C'------------------'                                     
         ACDEF H6,116,C'------------------'                                     
         ACDEF H7,116,C'Ref      DA#      '                                     
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H5,46,C'-------------Unit S--------------'                       
         ACDEF H6,46,C'-------------Balance-------------'                       
         ACDEF H7,46,C'       Debits       Credits      '                       
         ACDEF H5,82,C'-------------Unit G--------------'                       
         ACDEF H6,82,C'-------------Balance-------------'                       
         ACDEF H7,82,C'       Debits       Credits      '                       
*                                                                               
         ACDEF SPROG,4                                                          
         ACDEF H5,46,C'-------------Special-------------'                       
         ACDEF H6,46,C'-------------Records-------------'                       
         ACDEF H7,46,C'       Debits       Credits      '                       
         ACDEF H5,82,C'----------Unit S and G-----------'                       
         ACDEF H6,82,C'----------Transactions-----------'                       
         ACDEF H7,82,C'       Debits       Credits      '                       
         ACDEF H5,116,C'------------------'                                     
         ACDEF H6,116,C'------------------'                                     
         ACDEF H7,116,C'         DA#      '                                     
*                                                                               
         ACDEF SPROG,5                                                          
         ACDEF H6,44,C'Oth detail'                                              
         ACDEF H7,44,C'----------'                                              
         ACDEF H5,55,C'-------------Special-------------'                       
         ACDEF H6,55,C'-------------Records-------------'                       
         ACDEF H7,55,C'       Debits       Credits      '                       
         ACDEF H5,91,C'----------Unit S and G-----------'                       
         ACDEF H6,91,C'----------Transactions-----------'                       
         ACDEF H7,91,C'       Debits       Credits      '                       
         ACDEF H5,124,C' --------'                                              
         ACDEF H6,124,C' --------'                                              
         ACDEF H7,124,C'   DA#   '                                              
*                                                                               
         ACDEF SPROG,6                                                          
         ACDEF H5,46,C'-----------Unit G GLU------------'                       
         ACDEF H6,46,C'----------Transactions-----------'                       
         ACDEF H7,46,C'       Debits       Credits      '                       
         ACDEF H5,82,C'-----------Unit S GLU------------'                       
         ACDEF H6,82,C'----------Transactions-----------'                       
         ACDEF H7,82,C'       Debits       Credits      '                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPGB01 03/12/20'                                      
         END                                                                    
