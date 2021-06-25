*          DATA SET SPREPMS01  AT LEVEL 024 AS OF 08/29/00                      
*PHASE SPMS01A                                                                  
         TITLE 'STATION CALL-LETTER CHANGE PROGRAM - PRINT SPECS'               
SPMS01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         SPROG 1,2                                                              
         SSPEC H1,3,MEDIA                                                       
         SSPEC H1,46,C'SPECIAL CANADA STATION FIX'                              
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,3,REQUESTOR                                                   
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,3,PAGE                                                        
         SSPEC H4,98,REPORT                                                     
         SPACE 1                                                                
         SPROG 1                                                                
         SSPEC H4,15,C'BUYS'                                                    
         SSPEC H7,46,C'PRODUCT'                                                 
         SSPEC H7,63,C'ESTIMATE'                                                
         SSPEC H7,81,C'OLD LINE NUMBER'                                         
         SSPEC H7,106,C'NEW LINE NUMBER'                                        
         SSPEC H8,46,C'-------'                                                 
         SSPEC H8,63,C'--------'                                                
         SSPEC H8,81,C'--- ---- ------'                                         
         SSPEC H8,106,C'--- ---- ------'                                        
         SPACE 1                                                                
         SPROG 2                                                                
         SSPEC H4,15,C'INVOICES'                                                
         SSPEC H7,21,C'DATE'                                                    
         SSPEC H8,21,C'----'                                                    
         SSPEC H7,35,C'SEQ. NO.'                                                
         SSPEC H8,35,C'---- ---'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SPREPMS01 08/29/00'                                      
         END                                                                    
