*          DATA SET PPREP2001  AT LEVEL 017 AS OF 08/09/00                      
*PHASE PP2001A                                                                  
PP2001   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC GET,DIVISION                                                     
         FSPEC GET,DISTRICTS                                                    
         FSPEC GET,REGIONS                                                      
         FSPEC GET,PUB                                                          
         FSPEC GET,REP                                                          
         RSPEC DOUBLE,SPACING                                                   
         RSPEC REQUEST,NOREP                                                    
         SPROG 0,2                                                              
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H1,42,C'INVOICE CHECKING LIST'                                   
         PSPEC H1,75,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,42,21C'-'                                                     
         PSPEC H2,75,AGYADD                                                     
         PSPEC H3,1,CLIENT                                                      
         PSPEC H3,42,PERIOD                                                     
         PSPEC H4,1,SPACES                                                      
         PSPEC H4,75,RUN                                                        
         PSPEC H5,1,PRODUCT                                                     
         PSPEC H6,1,C'PUB'                                                      
         PSPEC H7,1,PAYEE                                                       
         PSPEC H7,75,REPORT                                                     
         PSPEC H7,97,PAGE                                                       
         PSPEC H8,1,C'REP'                                                      
         PSPEC H8,9,REPNAME                                                     
         PSPEC H8,75,C'***UNPAID ITEMS ONLY***'                                 
         PSPEC H11,1,C'INSERT'                                                  
         PSPEC H11,35,SPACES                                                    
         PSPEC H11,77,C'GROSS'                                                  
         PSPEC H12,2,C'DATE        EST'                                         
         PSPEC H12,59,C'GROSS COST    LESS COMM    CASH DISC'                   
         PSPEC H12,97,C'NET PAYABLE'                                            
         PSPEC H13,1,C'------       ---'                                        
         PSPEC H13,59,C'----------    ---------    ---------'                   
         PSPEC H13,97,C'-----------'                                            
         SPROG 0                                                                
         PSPEC H12,18,C'SPACE      RATE     PREMIUM'                            
         PSPEC H13,18,C'-----      ----     -------'                            
         SPROG 2                                                                
         PSPEC H11,38,C'PAYABLE'                                                
         PSPEC H12,18,C'SPACE                 DATE'                             
         PSPEC H13,18,C'-----               --------'                           
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017PPREP2001 08/09/00'                                      
         END                                                                    
