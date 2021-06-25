*          DATA SET NEMED58    AT LEVEL 019 AS OF 08/10/00                      
*PHASE T31E58A                                                                  
         TITLE 'T31E58 - SPECS FOR LATEST ESTIMATED DEMOS'                      
T31E58   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3                                                          
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,96,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,96,AGYADD                                                     
         SSPEC H3,53,PERIOD                                                     
         SSPEC H3,96,RUN                                                        
         SSPEC H4,109,PAGE                                                      
         SPROG 1,2                                                              
         SSPEC H1,55,C'LATEST ESTIMATED DEMOS'                                  
         SSPEC H2,55,C'----------------------'                                  
         SSPEC H3,1,C'CLIENT'                                                   
         SSPEC H8,1,C'CLT'                                                      
         SSPEC H8,5,C'PRD'                                                      
         SSPEC H8,9,C'EST'                                                      
         SSPEC H8,13,C'NTWK'                                                    
         SSPEC H8,19,C'PKG'                                                     
         SSPEC H8,24,C'PROG'                                                    
         SSPEC H8,32,C'DATE'                                                    
         SPROG 1                                                                
         SSPEC H6,39,C'-------------OLD EST DEMOS-----------------'             
         SSPEC H6,84,C'-------------NEW EST DEMOS-----------------'             
         SPROG 2                                                                
         SSPEC H6,39,C'-------------OLD EST DEMOS-----------------'             
         SSPEC H6,84,C'-------------NEW ACT DEMOS-----------------'             
         SPROG 3                                                                
         SSPEC H1,56,C'PROGRAM NTI SEEDED'                                      
         SSPEC H2,56,C'------------------'                                      
         SSPEC H8,41,C'CLT'                                                     
         SSPEC H8,46,C'EST'                                                     
         SSPEC H8,52,C'NTWK'                                                    
         SSPEC H8,58,C'PKG'                                                     
         SSPEC H8,63,C'PROG'                                                    
         SSPEC H8,71,C'DATE'                                                    
         SSPEC H8,82,C'OLD '                                                    
         SSPEC H8,90,C'NEW '                                                    
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019NEMED58   08/10/00'                                      
         END                                                                    
