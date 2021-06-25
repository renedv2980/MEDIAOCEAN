*          DATA SET SPREPA801  AT LEVEL 039 AS OF 07/11/03                      
*PHASE SPA801A,+0                                                               
         TITLE 'SPA801 - SPOT TRIAL BALANCE HEADLINES'                          
SPA801   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,1,3,4,5,6,7,8,9,10,11,12,13,14,15,50                           
*   NOTE - SPORG 15 IS SPECIAL FOR THE PRODUCT SUMMARY FORMAT                   
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,55,C'BILLING/CLEARANCE REPORT'                                
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,55,C'------------------------'                                
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,PAGE                                                      
         SSPEC H4,111,REPORT                                                    
         SPROG 1,3,4,15                                                         
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,1,PGROUP                                                      
         SSPEC H6,24,C'* * * * * * * * * * * * * * * * *'                       
         SSPEC H6,58,C'N E T'                                                   
         SSPEC H6,64,C'* * * * * * * * * * * * * * * * * *'                     
         SSPEC H7,107,C'* * * * G R O S S * * *'                                
         SPROG 15                                                               
         SSPEC H8,1,C'PRODUCT'                                                  
         SSPEC H9,1,C'-------'                                                  
         SSPEC H8,24,C'BAL FORWARD       BILLINGS             CLEARANCEX        
               S             BALANCE OUT'                                       
         SSPEC H8,107,C'BILLINGS      CLEARANCES'                               
         SSPEC H9,1,C'-------'                                                  
         SSPEC H9,24,11C'-'                                                     
         SSPEC H9,42,8C'-'                                                      
         SSPEC H9,63,10C'-'                                                     
         SSPEC H9,86,11C'-'                                                     
         SSPEC H9,107,8C'-'                                                     
         SSPEC H9,121,10C'-'                                                    
         SPROG 1,3,4                                                            
         SSPEC H7,79,C'DATE'                                                    
*                         ABOVE IS TOP PART OF "DATE CLEARED"                   
*                         WASN'T NEEDED FOR SPROG 15                            
*                                                                               
         SSPEC H8,1,C'STATION'                                                  
         SSPEC H8,24,C'BAL FORWARD       BILLINGS  INVOICE    CLEARANCEX        
               S     CLEARED   BALANCE OUT'                                     
         SSPEC H8,107,C'BILLINGS      CLEARANCES'                               
         SSPEC H9,1,C'-------'                                                  
         SSPEC H9,24,11C'-'                                                     
         SSPEC H9,42,8C'-'                                                      
         SSPEC H9,52,7C'-'                                                      
         SSPEC H9,63,10C'-'                                                     
         SSPEC H9,78,7C'-'                                                      
         SSPEC H9,88,11C'-'                                                     
         SSPEC H9,107,8C'-'                                                     
         SSPEC H9,121,10C'-'                                                    
         SPROG 3,4                                                              
         SSPEC H5,1,PRODUCT                                                     
         SPROG 4                                                                
         SSPEC H8,18,C'EST'                                                     
         SSPEC H9,18,C'---'                                                     
         SPROG 5,6,7,8,9,11,12,13,14                                            
         SSPEC H8,20,C'* * * * * * * * * * * * * * *'                           
         SSPEC H8,50,C'N E T '                                                  
         SSPEC H8,56,C'* * * * * * * * * * * * * * *'                           
         SSPEC H8,96,C'* * * * G R O S S * * * *'                               
         SSPEC H9,20,C'BALANCE FORWARD'                                         
         SSPEC H9,44,C'BILLINGS'                                                
         SSPEC H9,60,C'CLEARANCES'                                              
         SSPEC H9,75,C'BALANCE OUT'                                             
         SSPEC H9,96,C'BILLINGS'                                                
         SSPEC H9,111,C'CLEARANCES'                                             
         SSPEC H10,20,15C'-'                                                    
         SSPEC H10,44,8C'-'                                                     
         SSPEC H10,60,10C'-'                                                    
         SSPEC H10,75,11C'-'                                                    
         SSPEC H10,96,8C'-'                                                     
         SSPEC H10,111,10C'-'                                                   
         SPROG 5,6,7,8                                                          
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,1,PGROUP                                                      
         SPROG 5,6                                                              
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,C'** PRODUCT TOTALS **'                                     
         SPROG 6                                                                
         SSPEC H9,14,C'EST'                                                     
         SSPEC H10,14,C'---'                                                    
         SPROG 7,8                                                              
         SSPEC H6,1,C'** CLIENT TOTALS **'                                      
         SPROG 8                                                                
         SSPEC H9,14,C'EST'                                                     
         SSPEC H10,14,C'---'                                                    
         SPROG 9                                                                
         SSPEC H6,1,C'** MEDIA TOTALS **'                                       
         SSPEC H9,1,C'CLT'                                                      
         SSPEC H10,1,C'---'                                                     
         SPROG 11                                                               
         SSPEC H6,1,C'** OFFICE TOTALS **'                                      
         SSPEC H9,1,C'CLT'                                                      
         SSPEC H10,1,C'---'                                                     
         SPROG 12,14                                                            
         SSPEC H6,1,C'** OFFICE LIST TOTALS **'                                 
         SPROG 12                                                               
         SSPEC H9,1,C'OFF'                                                      
         SSPEC H10,1,C'---'                                                     
         SPROG 14                                                               
         SSPEC H9,1,C'CLT'                                                      
         SSPEC H10,1,C'---'                                                     
         SPROG 13                                                               
         SSPEC H6,1,C'** MEDIA TOTALS **'                                       
         SSPEC H9,1,C'OFF'                                                      
         SSPEC H10,1,C'---'                                                     
         SPROG 10,50                                                            
         SSPEC H3,1,CLIENT                                                      
         SSPEC H3,1,PGROUP                                                      
         SSPEC H6,1,C'CURRENT INVOICES'                                         
*                                                                               
*        NEXT 2 SPECS ARE JUST TO CLEAR OUT PRODUCT NAME                        
*        AND ESTIMATE STUFF THE THE PGROUP SSPEC DOES                           
*                                                                               
         SSPEC H6,17,C'                           '                             
         SSPEC H7,1,C'                                     '                    
         SSPEC H8,15,C'MTH OF'                                                  
         SSPEC H9,4,C'PRD  EST  SERVICE  TYPE  INVOICE             NET X        
                         GROSS     BILL AMOUNT'                                 
         SSPEC H10,4,C'---  ---  -------  ----  -------             ---X        
                          -----     -----------'                                
         SPROG 50         '                                                     
         SSPEC H9,96,C'GST'                                                     
         SSPEC H10,96,C'---'                                                    
         SSPEC H9,111,C'PST'                                                    
         SSPEC H10,111,C'---'                                                   
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    CL25'5006ACURRENT MONTH'                                         
         DC    CL25'6201AOPTION 1'                                              
         DC    CL25'6301AOPTION 2'                                              
         DC    CL25'6401AOPTION 3'                                              
         DC    CL25'6501AOPTION 4'                                              
         DC    CL25'6601AOPTION 5'                                              
         DC    CL25'6701AOPTION 6'                                              
         DC    CL25'6801AOPTION 7'                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039SPREPA801 07/11/03'                                      
         END                                                                    
