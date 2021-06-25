*          DATA SET ACREP5001  AT LEVEL 008 AS OF 08/16/00                      
*PHASE AC5001A                                                                  
AC5001   TITLE '- SPECS FOR VOID CHEQUE REPORT'                                 
AC5001   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF GETOPT,N                                                         
         ACDEF H1,2,RUN                                                         
         ACDEF H1,84,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H3,2,COMPANY                                                     
         ACDEF H3,84,REQUESTOR                                                  
*&&US                                                                           
         ACDEF H1,46,C'VOID CHECK REPORT'                                       
         ACDEF H2,46,17X'BF'                                                    
         ACDEF H8,59,AC#CHK2,5,L                                                
         ACDEF H8,69,AC#CHK2,5,L                                                
         ACDEF H8,82,AC#CHK2,5,L                                                
*&&                                                                             
*&&UK                                                                           
         ACDEF H1,46,C'VOID PAYMENT REPORT'                                     
         ACDEF H2,46,19X'BF'                                                    
*        ACDEF H8,59,AC#CHK,6,L                                                 
*        ACDEF H8,69,AC#CHK,6,L                                                 
*        ACDEF H8,82,AC#CHK,6,L                                                 
*&&                                                                             
         ACDEF H4,2,LEDGER                                                      
         ACDEF H8,2,AC#PAYAA,22,L                                               
         ACDEF H9,2,AC#PAYAA,22,LU                                              
         ACDEF H9,59,AC#REF,6,R                                                 
         ACDEF H9,69,AC#DATE,5,R                                                
         ACDEF H9,82,AC#AMT,6,R                                                 
         ACDEF LANG,3                                                           
         ACDEF H1,40,C'LISTE STORNIERTER SCHECKS'                               
         ACDEF H2,40,25X'BF'                                                    
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREP5001 08/16/00'                                      
         END                                                                    
