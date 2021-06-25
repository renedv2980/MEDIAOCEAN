*          DATA SET SPREPNX01  AT LEVEL 006 AS OF 06/06/18                      
*          DATA SET SPREPSA01  AT LEVEL 010 AS OF 10/20/15                      
*PHASE SPNX01B                                                                  
         TITLE 'SPNX01 - SPOT NETWORK SORT -  SPECS'                            
SPNX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         SSPEC H1,44,C'NETWORK RECOVERY SORT'                                   
         SSPEC H2,44,C'---------------------'                                   
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H1,80,AGYNAME                                                    
         SSPEC H2,80,AGYADD                                                     
*                                                                               
         SSPEC M1,01,C'SY AG MD CLT PRD EST BILLDATE YS MS INVOICE'             
         SSPEC M2,01,C'-- -- -- --- --- --- -------- -- -- -------'             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPNX01 06/06/18'                                      
         END                                                                    
