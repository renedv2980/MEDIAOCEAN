*          DATA SET PPREP7901  AT LEVEL 011 AS OF 08/07/02                      
*PHASE PP7901A,+0                                                               
         TITLE 'PP7901 - PRINTPAK SHIPPING LIST'                                
PP7901   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC UPDATE,PRTFILE                                                   
         SPROG 0,2,3,4,31,41                                                    
         PSPEC H1,2,MEDIA                                                       
         PSPEC H2,2,REQUESTOR                                                   
         PSPEC H1,95,AGYNAME                                                    
         PSPEC H2,95,AGYADD                                                     
         PSPEC H4,95,REPORT                                                     
         PSPEC H5,95,RUN                                                        
         PSPEC H6,95,PAGE                                                       
         PSPEC H4,2,CLIENT                                                      
         PSPEC H5,2,PRODUCT                                                     
         PSPEC H1,52,C'SHIPPING INSTRUCTIONS'                                   
         PSPEC H2,52,C'---------------------'                                   
         SPROG 2,3,4,31,41                                                      
         PSPEC H1,52,C'    SHIPPING LIST    '                                   
         PSPEC H2,52,C'    -------------    '                                   
         PSPEC H4,52,PERIOD                                                     
         SPROG 2,3,4                                                            
         PSPEC H13,4,C'SHIP TO'                                                 
         PSPEC H14,4,C'-------'                                                 
         PSPEC H13,40,C'VENDOR NO.'                                             
         PSPEC H14,40,C'----------'                                             
         PSPEC H12,63,C'INSERTION'                                              
         PSPEC H13,63,C'   DATE  '                                              
         PSPEC H14,63,C'---------'                                              
         PSPEC H12,95,C'DATE'                                                   
         PSPEC H12,104,C'HOW'                                                   
         PSPEC H13,93,C'SHIPPED  SHIPPED  CHARGES'                              
         PSPEC H14,93,C'-------  -------  -------'                              
         SPROG 31,41                                                            
         PSPEC H12,4,C'SHIP TO'                                                 
         PSPEC H13,4,C'-------'                                                 
         PSPEC H12,40,C'VENDOR NO.'                                             
         PSPEC H13,40,C'----------'                                             
         PSPEC H11,63,C'INSERTION'                                              
         PSPEC H12,63,C'   DATE  '                                              
         PSPEC H13,63,C'---------'                                              
         PSPEC H11,95,C'DATE'                                                   
         PSPEC H11,104,C'HOW'                                                   
         PSPEC H12,93,C'SHIPPED  SHIPPED  CHARGES'                              
         PSPEC H13,93,C'-------  -------  -------'                              
         SPROG 3,4                                                              
         PSPEC H12,40,C'MARKET/'                                                
         SPROG 31,41                                                            
         PSPEC H11,40,C'MARKET/'                                                
         SPROG 4                                                                
         PSPEC H12,63,C' POSTING '                                              
         PSPEC H14,63,C' ------- '                                              
         PSPEC H12,80,C'NUMBER'                                                 
         PSPEC H13,78,C'OF POSTERS'                                             
         PSPEC H14,78,C'----------'                                             
         SPROG 41                                                               
         PSPEC H11,63,C' POSTING '                                              
         PSPEC H13,63,C' ------- '                                              
         PSPEC H11,80,C'NUMBER'                                                 
         PSPEC H12,78,C'OF POSTERS'                                             
         PSPEC H13,78,C'----------'                                             
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'0102041F150C0D0F131400'                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011PPREP7901 08/07/02'                                      
         END                                                                    
