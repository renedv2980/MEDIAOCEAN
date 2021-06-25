*          DATA SET NENETXA01  AT LEVEL 006 AS OF 08/10/00                      
*PHASE SPNX01A                                                                  
         TITLE 'SPNX01 - NETWORK ATT FILE EXTRACT'                              
SPNX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H1,49,C'NETWORK ATT FILE EXTRACT'                                
         SSPEC H2,49,C'------------------------'                                
         SSPEC H1,94,AGYNAME                                                    
         SSPEC H2,94,AGYADD                                                     
         SSPEC H4,34,C' ASSIGNED          ACTUAL        INTEG'                  
         SSPEC H5,34,C' --------          ----------    ----------'             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NENETXA01 08/10/00'                                      
         END                                                                    
