*          DATA SET ACREPAB01O AT LEVEL 001 AS OF 12/06/16                      
*PHASE ACAB01A,+0                                                               
         TITLE 'SPECS FOR BX - IBM EDI TRANSMISSION'                            
ACAB01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PRORATA,BILL                                                     
*                                                                               
         RSPEC MAXLINES,54                                                      
         ACDEF RESET                                                            
*                                                                               
         SPROG 0                                                                
         ACDEF H5,12,C'VENDOR #:'                                               
         ACDEF H12,23,C'Bill No.'                                               
         ACDEF H12,33,C'Run Date'                                               
         ACDEF H12,46,C'Material'                                               
         ACDEF H12,56,C'Description'                                            
         ACDEF H12,76,C'Order Qty'                                              
         ACDEF H12,90,C'Unit'                                                   
*                                                                               
         SPROG 0,1                                                              
         ACDEF H1,100,PAGE                                                      
         ACDEF H1,2,RUN                                                         
         ACDEF H1,56,C'EDI REPORT'                                              
*                                                                               
         ACDEF H2,2,C'.'                                                        
*                                                                               
         ACDEF H3,2,C'Bill Run Date:'                                           
         ACDEF H3,48,C'Contact Person:'                                         
         ACDEF H3,86,C'Telephone:'                                              
*                                                                               
         ACDEF H4,2,C'.'                                                        
*                                                                               
         ACDEF H11,2,C'-'                                                       
*                                                                               
         ACDEF H12,104,C'Net Value'                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPAB01O12/06/16'                                      
         END                                                                    
