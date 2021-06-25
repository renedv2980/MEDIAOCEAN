*          DATA SET PPREPA801  AT LEVEL 056 AS OF 12/13/00                      
*PHASE PPA801A,+0                                                               
         TITLE 'PPA801 - PRINTPAK TRIAL BALANCE HEADLINES'                      
PPA801   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBLICATIONS                                                 
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,1,3,4,5,6,7,8,9,10,11,12,13,14,15,20,21,23,24,25,26,27X        
               ,28,29,30,31,32,33,34,35,60,80                                   
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H1,52,C'BILLING/CLEARANCE REPORT'                                
         PSPEC H1,100,AGYNAME                                                   
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,52,C'------------------------'                                
         PSPEC H2,100,AGYADD                                                    
         PSPEC H4,100,RUN                                                       
         PSPEC H5,100,REPORT                                                    
         PSPEC H5,125,PAGE                                                      
         SPROG 1,3,4,21,23,24                                                   
         PSPEC H4,1,CLIENT                                                      
         PSPEC H7,79,C'DATE'                                                    
         PSPEC H8,24,C'BAL FORWARD       BILLINGS  INVOICE    CLEARANCEX        
               S     CLEARED  REP  BALANCE OUT'                                 
         PSPEC H8,110,C'BILLINGS   CLEARANCES'                                  
         PSPEC H9,24,11C'-'                                                     
         PSPEC H9,42,8C'-'                                                      
         PSPEC H9,52,7C'-'                                                      
         PSPEC H9,63,10C'-'                                                     
         PSPEC H9,78,7C'-'                                                      
         PSPEC H9,87,3C'-'                                                      
         PSPEC H9,92,11C'-'                                                     
         PSPEC H9,110,8C'-'                                                     
         PSPEC H9,121,10C'-'                                                    
         SPROG 1,3,4,21,23,24                                                   
         PSPEC H8,1,C'PUBLICATION'                                              
         PSPEC H9,1,C'-----------'                                              
         SPROG 15,35                                                            
         PSPEC H8,1,C'PRODUCT'                                                  
         PSPEC H9,1,C'-------'                                                  
         PSPEC H4,1,CLIENT                                                      
         PSPEC H8,24,C'BAL FORWARD       BILLINGS             CLEARANCEX        
               S                   BALANCE OUT'                                 
         PSPEC H8,110,C'BILLINGS   CLEARANCES'                                  
         PSPEC H9,24,11C'-'                                                     
         PSPEC H9,42,8C'-'                                                      
         PSPEC H9,63,10C'-'                                                     
         PSPEC H9,92,11C'-'                                                     
         PSPEC H9,110,8C'-'                                                     
         PSPEC H9,121,10C'-'                                                    
         SPROG 1,3,4,15                                                         
         PSPEC H6,24,C'* * * * * * * * * * * * * * * * * * *'                   
         PSPEC H6,62,C'N E T'                                                   
         PSPEC H6,68,C'* * * * * * * * * * * * * * * * * *'                     
         PSPEC H7,110,C'* * * G R O S S * * *'                                  
         SPROG 21,23,24,35                                                      
         PSPEC H6,25,C'* * * * * * * * * * * * * * * *'                         
         PSPEC H6,57,C'N E T / N E T'                                           
         PSPEC H6,71,C'* * * * * * * * * * * * * * * * *'                       
         PSPEC H7,109,C'* * G R O S S - C D * *'                                
         SPROG 4,24                                                             
         PSPEC H8,18,C'EST'                                                     
         PSPEC H9,18,C'---'                                                     
         SPROG 5,6,7,8,9,11,12,13,14,25,26,27,28,29,31,32,33,34                 
         PSPEC H9,20,C'BALANCE FORWARD'                                         
         PSPEC H9,44,C'BILLINGS'                                                
         PSPEC H9,60,C'CLEARANCES'                                              
         PSPEC H9,75,C'BALANCE OUT'                                             
         PSPEC H9,96,C'BILLINGS'                                                
         PSPEC H9,111,C'CLEARANCES'                                             
         PSPEC H10,20,15C'-'                                                    
         PSPEC H10,44,8C'-'                                                     
         PSPEC H10,60,10C'-'                                                    
         PSPEC H10,75,11C'-'                                                    
         PSPEC H10,96,8C'-'                                                     
         PSPEC H10,111,10C'-'                                                   
         SPROG 5,6,7,8,9,11,12,13,14                                            
         PSPEC H8,20,C'* * * * * * * * * * * * * * *'                           
         PSPEC H8,50,C'N E T '                                                  
         PSPEC H8,56,C'* * * * * * * * * * * * * * *'                           
         PSPEC H8,96,C'* * * * G R O S S * * * *'                               
         SPROG 25,26,27,28,29,31,32,33,34                                       
         PSPEC H8,20,C'* * * * * * * * * * * * *'                               
         PSPEC H8,46,C'N E T / N E T'                                           
         PSPEC H8,60,C'* * * * * * * * * * * * *'                               
         PSPEC H8,96,C'* * G R O S S  -  C D * *'                               
         SPROG 5,6,7,8,10,25,26,27,28,30,60,80                                  
         PSPEC H4,1,CLIENT                                                      
         SPROG 5,6,25,26                                                        
         PSPEC H6,1,C'** PRODUCT TOTALS **'                                     
         SPROG 6,26                                                             
         PSPEC H9,14,C'EST'                                                     
         PSPEC H10,14,C'---'                                                    
         SPROG 7,8,27,28                                                        
         PSPEC H6,1,C'** CLIENT TOTALS **'                                      
         SPROG 8,28                                                             
         PSPEC H9,14,C'EST'                                                     
         PSPEC H10,14,C'---'                                                    
         SPROG 9,29                                                             
         PSPEC H6,1,C'** MEDIA TOTALS **'                                       
         PSPEC H9,1,C'CLT'                                                      
         PSPEC H10,1,C'---'                                                     
         SPROG 11,31                                                            
         PSPEC H6,1,C'** OFFICE TOTALS **'                                      
         PSPEC H9,1,C'CLT'                                                      
         PSPEC H10,1,C'---'                                                     
         SPROG 12,14,32,34                                                      
         PSPEC H6,1,C'** OFFICE LIST TOTALS **'                                 
         SPROG 12,32                                                            
         PSPEC H9,1,C'OFF'                                                      
         PSPEC H10,1,C'---'                                                     
         SPROG 14,34                                                            
         PSPEC H9,1,C'CLT'                                                      
         PSPEC H10,1,C'---'                                                     
         SPROG 13,33                                                            
         PSPEC H6,1,C'** MEDIA TOTALS **'                                       
         PSPEC H9,1,C'OFF'                                                      
         PSPEC H10,1,C'---'                                                     
         SPROG 10,30,60,80                                                      
         PSPEC H6,1,C'CURRENT INVOICES'                                         
         PSPEC H8,15,C'MTH OF'                                                  
         PSPEC H9,75,C'CASH DISC     BILL AMOUNT'                               
         PSPEC H10,75,C'---------     -----------'                              
         SPROG 10,60                                                            
         PSPEC H9,4,C'PRD  EST  SERVICE  TYPE  INVOICE             NET X        
                         GROSS'                                                 
         PSPEC H10,4,C'---  ---  -------  ----  -------             ---X        
                          -----'                                                
         SPROG 30,80                                                            
         PSPEC H9,4,C'PRD  EST  SERVICE  TYPE  INVOICE         NET/NET X        
                      GROSS-CD'                                                 
         PSPEC H10,4,C'---  ---  -------  ----  -------         -------X        
                       --------'                                                
         SPROG 60,80                                                            
         PSPEC H9,113,C'GST'                                                    
         PSPEC H10,113,C'---'                                                   
         PSPEC H9,129,C'PST'                                                    
         PSPEC H10,129,C'---'                                                   
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'0102040F1A1B130000'                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056PPREPA801 12/13/00'                                      
         END                                                                    
