*          DATA SET SPREPSA01  AT LEVEL 011 AS OF 08/07/17                      
*PHASE SPSA01B                                                                  
         TITLE 'SPSA01 - SPOT SAP EXTRACT - SPECS'                              
SPFX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         SSPEC H1,43,C'SPOT SAP BILLING EXTRACT'                                
         SSPEC H2,43,C'------------------------'                                
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H1,80,AGYNAME                                                    
         SSPEC H2,80,AGYADD                                                     
*                                                                               
         SSPEC M1,01,C'AG M OF CLT PRD STATION   EST YS MS TYPE BILLNUMX        
                   BILL GROSS       BILL NET  '                                 
         SSPEC M2,01,C'-- - -- --- --- --------  --- -- -- ---- -------X        
                  -------------  -------------'                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPREPSA01 08/07/17'                                      
         END                                                                    
