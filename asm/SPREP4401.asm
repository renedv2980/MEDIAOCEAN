*          DATA SET SPREP4401  AT LEVEL 019 AS OF 02/26/99                      
*PHASE SP4401A,+0                                                               
         TITLE 'SP4401 - NETWORK/SHOW/SPILL HEADLINES'                          
SP4401   CSECT                                                                  
         FSPEC USE,SP4403                                                       
         SPROG 1,2,3                                                            
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,PAGE                                                      
         SSPEC H4,111,REPORT                                                    
         SPROG 1                                                                
         SSPEC H1,59,C'NETWORK LISTING'                                         
         SSPEC H2,59,C'------- -------'                                         
         SSPEC H7,26,C'OFFSET'                                                  
         SSPEC H8,28,C'IN'                                                      
         SSPEC H8,40,C'COST'                                                    
         SSPEC H9,2,C'STATION     REGIONS      MINS.     PERCENTAGE'            
         SSPEC H10,2,C'-------     -------     ------     ----------'           
         SPROG 2                                                                
         SSPEC H1,59,C'PROGRAM LISTING'                                         
         SSPEC H2,59,C'------- -------'                                         
         SSPEC H4,2,C'NETWORK'                                                  
         SSPEC H6,2,C'PROGRAM'                                                  
         SSPEC H8,2,C'STATION EXCEPTIONS'                                       
         SSPEC H10,13,C'RELATIVE'                                               
         SSPEC H11,2,C'STATION      DAY       TIME    DAYPART'                  
         SSPEC H12,2,C'-------    --------    ----    -------'                  
         SPROG 3                                                                
         SSPEC H1,56,C'SPILL MARKET LISTING'                                    
         SSPEC H2,56,C'----- ------ -------'                                    
         SSPEC H4,2,C'RATING SERVICE'                                           
         SSPEC H7,90,C'OFFSET    RATING'                                        
         SSPEC H8,92,C'IN       SRVC'                                           
         SSPEC H9,2,C'STATION   CLT'                                            
         SSPEC H9,19,C'MARKET'                                                  
         SSPEC H9,55,C'SPILL MARKET'                                            
         SSPEC H9,91,C'MINS.    MARKET'                                         
         SSPEC H10,2,C'-------   ---'                                           
         SSPEC H10,19,C'--------------------------------'                       
         SSPEC H10,55,C'--------------------------------'                       
         SSPEC H10,91,C'-----    ------'                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPREP4401 02/26/99'                                      
         END                                                                    
