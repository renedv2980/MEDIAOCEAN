*          DATA SET PPREPSN01  AT LEVEL 006 AS OF 01/25/05                      
*PHASE PPSN01A,+0                                                               
         TITLE 'PPSN01 - SONY INTERFACES - HEADLINES'                           
PPSN01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         FSPEC GET,PUBS                                                         
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,20                                                          
         SSPEC H1,49,CL23'SONY INTERFACE'                                       
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,49,14C'-'                                                     
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,100,RUN                                                       
         SSPEC H4,100,REPORT                                                    
         SSPEC H4,125,PAGE                                                      
         SPROG 0                                                                
         SSPEC H6,20,C'RECORD TYPE              TOTAL'                          
         SSPEC H7,20,C'-----------              -----'                          
         SPROG 10                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,CLIENT                                                      
         SSPEC H3,45,PERIOD                                                     
         SSPEC H6,1,C'INVOICES'                                                 
         SSPEC H8,15,C'MTH OF'                                                  
         SSPEC H9,75,C'CASH DISC     BILL AMOUNT'                               
         SSPEC H10,75,C'---------     -----------'                              
         SSPEC H9,1,C'PRD EST/REG  SERVICE  TYPE  INVOICE           GROX        
               SS          NET-CD'                                              
         SSPEC H10,1,C'--- -------  -------  ----  -------           --X        
               ---          ------'                                             
         SSPEC H9,102,C'INV DATE'                                               
         SSPEC H9,112,C'DUE DATE'                                               
         SSPEC H10,102,C'--------'                                              
         SSPEC H10,112,C'--------'                                              
         SPROG 20                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,CLIENT                                                      
         SSPEC H3,45,PERIOD                                                     
         SSPEC H4,1,C'BILLED ESTIMATES'                                         
         SSPEC H7,2,C'PRD'                                                      
         SSPEC H7,7,C'EST/REG'                                                  
         SSPEC H7,16,C'START'                                                   
         SSPEC H7,28,C'END'                                                     
         SSPEC H7,42,C'DESCRIPTION'                                             
         SSPEC H8,2,C'---'                                                      
         SSPEC H8,7,C'-------'                                                  
         SSPEC H8,16,C'-----'                                                   
         SSPEC H8,28,C'---'                                                     
         SSPEC H8,41,C'-----------'                                             
         DC    X'00'                                                            
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPREPSN01 01/25/05'                                      
         END                                                                    
