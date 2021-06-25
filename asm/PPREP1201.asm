*          DATA SET PPREP1201  AT LEVEL 026 AS OF 10/30/17                      
*PHASE PP1201A,+0                                                               
         TITLE 'PP1201 - PRINT CONTRACT SPECS'                                  
PP1201   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC MAXLINES,75                                                      
         RSPEC REQUEST,NOREP                                                    
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC GET,CONTRACTS                                                    
         SPROG 2                                                                
         PSPEC H1,21,C'CONTRACT REGISTER'                                       
         SPROG 12                                                               
         PSPEC H1,21,C'CONTRACT T/A REGISTER'                                   
         SPROG 3                                                                
         PSPEC H1,21,C'SPACE RESERVATIONS'                                      
         SPROG 4                                                                
         PSPEC H1,21,C'CONTRACTS'                                               
         SPROG 2,3,4,12                                                         
         PSPEC H1,45,REPORT                                                     
         PSPEC H2,45,AGYNAME                                                    
         PSPEC H3,2,PAGE                                                        
         PSPEC H3,45,AGYADD                                                     
         PSPEC H4,45,RUN                                                        
         PSPEC H6,2,C'CLIENT   PRD   EST   PUBLICATION        '                 
         PSPEC H6,43,C'CONTRACT   START DATE    END DATE'                       
         PSPEC H7,2,C'------   ---   ---   -----------        '                 
         PSPEC H7,43,C'--------   ----------    --------'                       
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'01020407090C0D0F131400'                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026PPREP1201 10/30/17'                                      
         END                                                                    
