*          DATA SET PPREP0501  AT LEVEL 009 AS OF 08/09/00                      
*PHASE PP0501A                                                                  
         TITLE 'PP0501 - DDS BILLING - SPECS'                                   
PP0501   CSECT                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         SPACE 2                                                                
         RSPEC REQUEST,NOREP                                                    
         RSPEC MAXLINES,53                                                      
         SPACE 2                                                                
         PSPEC H1,5,AGYNAME                                                     
         PSPEC H2,5,AGYADD                                                      
         SPACE 2                                                                
         PSPEC H1,54,C'DONOVAN DATA SYSTEMS, INC.'                              
         PSPEC H2,54,C'115 W 18TH ST NY,NY  10011'                              
         PSPEC H4,54,REPORT                                                     
         PSPEC H5,54,RUN                                                        
         PSPEC H8,54,PAGE                                                       
         SPACE 2                                                                
         PSPEC H11,58,C'BILLED'                                                 
         PSPEC H12,58,C'AMOUNT'                                                 
         SPACE 2                                                                
         PSPEC H11,74,C'AMOUNT'                                                 
         PSPEC H12,74,C'  DUE '                                                 
         SPACE 2                                                                
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009PPREP0501 08/09/00'                                      
         END                                                                    
