*          DATA SET PPREPST01  AT LEVEL 018 AS OF 12/13/89                      
*PHASE PPPM01A,+0                                                               
         TITLE 'PPPM01 - PHILIP MORRIS EXTRACT - HEADLINES'                     
PPPM01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBS                                                         
         RSPEC LINEUP PATTERN                                                   
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,20                                                          
         PSPEC H1,49,CL23'PHILIP MORRIS INTERFACE'                              
         PSPEC H1,100,AGYNAME                                                   
         PSPEC H2,49,23C'-'                                                     
         PSPEC H2,100,AGYADD                                                    
         PSPEC H3,50,PERIOD                                                     
         PSPEC H3,100,RUN                                                       
         PSPEC H4,100,REPORT                                                    
         PSPEC H4,125,PAGE                                                      
         SPROG 0                                                                
         PSPEC H6,8,C'RECORD TYPE            TOTAL'                             
         PSPEC H7,8,C'-----------            -----'                             
         SPROG 10                                                               
         PSPEC H1,1,MEDIA                                                       
         PSPEC H2,1,CLIENT                                                      
         PSPEC H6,1,C'CURRENT INVOICES'                                         
         PSPEC H8,15,C'MTH OF'                                                  
         PSPEC H9,75,C'CASH DISC     BILL AMOUNT'                               
         PSPEC H10,75,C'---------     -----------'                              
         PSPEC H9,4,C'PRD  EST  SERVICE  TYPE  INVOICE           GROSS X        
                      GROSS-CD'                                                 
         PSPEC H10,4,C'---  ---  -------  ----  -------           -----X        
                       --------'                                                
         PSPEC H9,102,C'INV DATE'                                               
         PSPEC H9,112,C'DUE DATE'                                               
         PSPEC H10,102,C'--------'                                              
         PSPEC H10,112,C'--------'                                              
         SPROG 20                                                               
         PSPEC H1,1,MEDIA                                                       
         PSPEC H2,1,CLIENT                                                      
         PSPEC H6,1,C'EST/PUBLICATION RECAP'                                    
         PSPEC H7,66,C'***** FILE TOTAL *****'                                  
         PSPEC H7,101,C'****** RUN TOTAL *****'                                 
         PSPEC H8,3,C'EST'                                                      
         PSPEC H8,9,C'PUBLICATION'                                              
         PSPEC H8,66,C'GROSS'                                                   
         PSPEC H8,80,C'GROSS-CD'                                                
         PSPEC H8,101,C'GROSS'                                                  
         PSPEC H8,115,C'GROSS-CD'                                               
         PSPEC H9,3,C'---'                                                      
         PSPEC H9,9,C'-----------'                                              
         PSPEC H9,66,C'-----'                                                   
         PSPEC H9,80,C'--------'                                                
         PSPEC H9,101,C'-----'                                                  
         PSPEC H9,115,C'--------'                                               
         DC    X'00'                                                            
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018PPREPST01 12/13/89'                                      
         END                                                                    
