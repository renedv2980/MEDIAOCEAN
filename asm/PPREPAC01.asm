*          DATA SET PPREPAC01  AT LEVEL 003 AS OF 07/18/16                      
*PHASE PPAC01A,+0                                                               
         TITLE 'PPAC01 - PRINT ADVERTISER CONTRACT SPECS'                       
PPAC01   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC MAXLINES,75                                                      
         RSPEC REQUEST,NOREP                                                    
         FSPEC GET,CONTRACTS                                                    
         SPROG 2                                                                
         PSPEC H1,21,C'CONTRACT REGISTER'                                       
         SPROG 3                                                                
         PSPEC H1,21,C'SPACE RESERVATIONS'                                      
         SPROG 4                                                                
         PSPEC H1,21,C'CONTRACT'                                                
         SPROG 2,3,4                                                            
         PSPEC H1,45,REPORT                                                     
         PSPEC H2,45,AGYNAME                                                    
         PSPEC H3,2,PAGE                                                        
         PSPEC H3,45,AGYADD                                                     
         PSPEC H4,45,RUN                                                        
         PSPEC H6,2,C'CLIENT   PUBLICATION         CONTRACT   START DATX        
               E    END DATE       AGY FILTER'                                  
         PSPEC H7,2,C'------   -----------         --------   ---------X        
               -    --------       ----------'                                  
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'01020407090C0D0F131400'                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPREPAC01 07/18/16'                                      
         END                                                                    
