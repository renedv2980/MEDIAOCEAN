*          DATA SET ACREP9801  AT LEVEL 032 AS OF 02/27/09                      
*PHASE AC9801A,+0                                                               
AC9801   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF GETOPT,NO                                                        
         ACDEF RESET                                                            
         SPACE 1                                                                
         SPROG 0,1,2,3                                                          
         ASPEC H1,2,RUN                                                         
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,86,REQUESTOR                                                  
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,45,C'TRANSACTION PEEL-OFF REPORT'                             
         ASPEC H2,45,27C'-'                                                     
         ASPEC H5,2,UNIT                                                        
         ASPEC H6,2,LEDGER                                                      
         ASPEC H8,2,C'ACCOUNT NUMBER    ACCOUNT NAME'                           
         ASPEC H9,2,C'--------------    ------------'                           
         ASPEC H8,53,C'BALANCE B/F          DELETED          DELETED'           
         ASPEC H9,53,C'-----------           DEBITS          CREDITS'           
         ASPEC H8,104,C'BALANCE C/F      UNPEELED'                              
         ASPEC H9,104,C'-----------      --------'                              
         ASPEC F1,2,REQDETS                                                     
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H1,45,C'PEELED TRANSACTIONS'                                     
         ASPEC H2,45,C'-------------------'                                     
*&&US*&& ASPEC H5,2,UNIT                                                        
*&&US*&& ASPEC H6,2,LEDGER                                                      
*&&UK*&& ASPEC H5,2,C'LEDGER'                                                   
*&&UK*&& ASPEC H6,2,C'TRIGGER ON'                                               
         ASPEC H10,2,C'ACCOUNT NO.  CONTRA A/C'                                 
         ASPEC H11,2,C'-----------  ----------'                                 
         ASPEC H10,30,C'REF NO'                                                 
         ASPEC H11,30,C'------'                                                 
         ASPEC H10,37,C'DATE     BATCH'                                         
         ASPEC H11,37,C'----      REF'                                          
         ASPEC H10,53,C'OFFC'                                                   
         ASPEC H11,53,C'----'                                                   
         ASPEC H10,58,C'NARRATIVE'                                              
         ASPEC H11,58,C'---------'                                              
         ASPEC H10,92,C'DEBIT          CREDIT'                                  
         ASPEC H11,92,C'-----          ------'                                  
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H1,45,C'CLOSED JOB REPORT'                                       
         ASPEC H2,45,C'-----------------'                                       
         ASPEC H5,2,UNIT                                                        
         ASPEC H6,2,LEDGER                                                      
         ASPEC H8,2,C'CLI/PRD/JOB       JOB NAME'                               
         ASPEC H9,2,C'-----------       --------'                               
         ASPEC H8,51,C'      DEBITS TO     CREDITS TO    LAST   CLOSED'         
         ASPEC H9,51,C'     BE DELETED     BE DELETED  ACTIVITY  DATE '         
         ASPEC F1,2,REQDETS                                                     
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H1,45,C'TRANSACTION UNPEEL   REPORT'                             
         ASPEC H2,45,27C'-'                                                     
         ASPEC H5,2,UNIT                                                        
         ASPEC H6,2,LEDGER                                                      
         ASPEC H8,2,C'ACCOUNT NUMBER    ACCOUNT NAME'                           
         ASPEC H9,2,C'--------------    ------------'                           
         ASPEC H8,53,C'BALANCE B/F         RESTORED         RESTORED'           
         ASPEC H9,53,C'-----------          DEBITS           CREDITS'           
         ASPEC H8,104,C'BALANCE C/F'                                            
         ASPEC H9,104,C'-----------'                                            
         ASPEC F1,2,REQDETS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032ACREP9801 02/27/09'                                      
         END                                                                    
