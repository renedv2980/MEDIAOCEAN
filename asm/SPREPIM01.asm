*          DATA SET SPREPIM01  AT LEVEL 014 AS OF 08/29/00                      
*PHASE SPI301A                                                                  
         TITLE 'SPREPI301 - INVOICE CHECKING SPECS'                             
SPI301   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,PACKAGES                                                    
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         SPROG 0,THRU,4                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,48,C'MSSD/MKGD INVOICE CHECKING REPORT'                       
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,48,C'---------------------------------'                       
         SSPEC H2,97,AGYADD                                                     
         SSPEC H3,97,PAGE                                                       
         SSPEC H3,48,PERIOD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H5,1,CLIENT                                                      
         SSPEC H6,1,PRODUCT                                                     
         SSPEC H7,1,ESTIMATE                                                    
         SSPEC H7,97,MARKET                                                     
         SSPEC H10,1,C'---------------------------------'                       
         SSPEC H10,34,C'O R D E R E D'                                          
         SSPEC H10,47,C'-----------------------------------'                    
         SSPEC H10,83,C'---------'                                              
         SSPEC H10,92,C'MISSED OR MAKEGOOD REFERENCE--------'                   
         SSPEC H11,1,C'DATE(S)'                                                 
         SSPEC H12,1,C'-------'                                                 
         SSPEC H11,14,C'DAYS'                                                   
         SSPEC H12,14,C'----'                                                   
         SSPEC H11,25,C'TIMES'                                                  
         SSPEC H12,25,C'-----'                                                  
         SSPEC H11,38,C'LEN PRODUCT'                                            
         SSPEC H12,38,C'--- -------'                                            
         SSPEC H11,57,C'COST   EST-LIN'                                         
         SSPEC H12,57,C'----   -------'                                         
         SSPEC H11,72,C'PROGRAM'                                                
         SSPEC H12,72,C'-------'                                                
         SSPEC H11,83,C'TYPE  DATE(S)'                                          
         SSPEC H12,83,C'----  -------'                                          
         SSPEC H11,103,C'DAYS   TIME(S)'                                        
         SSPEC H12,103,C'----   -------'                                        
         SSPEC H11,122,C'EST-LIN'                                               
         SSPEC H12,122,C'-------'                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPIM01 08/29/00'                                      
         END                                                                    
