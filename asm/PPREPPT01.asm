*          DATA SET PPREPPT01  AT LEVEL 002 AS OF 06/23/04                      
*PHASE PPPT01A,+0                                                               
         TITLE 'PPPT01 - PHILIP MORR S INTERFACE-HEADLINES'                     
PPPT01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC READ,BILLS                                                       
         FSPEC GET,PUBS                                                         
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,20                                                          
         SSPEC H1,49,CL23'PHILIP MORRIS INTERFACE'                              
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,49,23C'-'                                                     
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
         SSPEC H3,49,PERIOD                                                     
         SSPEC H6,1,C'CURRENT INVOICES'                                         
         SSPEC H8,15,C'MTH OF'                                                  
         SSPEC H9,75,C'CASH DISC     BILL AMOUNT'                               
         SSPEC H10,75,C'---------     -----------'                              
         SSPEC H9,4,C'PRD  EST  SERVICE  TYPE  INVOICE           GROSS X        
                      GROSS-CD'                                                 
         SSPEC H10,4,C'---  ---  -------  ----  -------           -----X        
                       --------'                                                
         SSPEC H9,102,C'INV DATE'                                               
         SSPEC H9,112,C'DUE DATE'                                               
         SSPEC H10,102,C'--------'                                              
         SSPEC H10,112,C'--------'                                              
         SPROG 20                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,CLIENT                                                      
         SSPEC H6,1,C'ESTIMATE/PUBLICATION RECAP'                               
         SSPEC H8,3,C'PRD'                                                      
         SSPEC H8,8,C'EST'                                                      
         SSPEC H8,15,C'PUBLICATION'                                             
         SSPEC H8,37,C'MTH/YEAR'                                                
         SSPEC H8,56,C'DISCOUNT'                                                
         SSPEC H8,71,C'AMOUNT DUE'                                              
         SSPEC H8,88,C'COMMISSION'                                              
         SSPEC H9,3,C'---'                                                      
         SSPEC H9,8,C'---'                                                      
         SSPEC H9,15,C'-----------'                                             
         SSPEC H9,37,C'--------'                                                
         SSPEC H9,56,C'--------'                                                
         SSPEC H9,71,C'----------'                                              
         SSPEC H9,88,C'----------'                                              
         DC    X'00'                                                            
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREPPT01 06/23/04'                                      
         END                                                                    
