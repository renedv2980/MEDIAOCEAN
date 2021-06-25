*          DATA SET SPREPCT01  AT LEVEL 006 AS OF 08/29/00                      
*PHASE SPCT01A                                                                  
         TITLE 'SPCT01 - CCUSA - TRAFFIC COMMERCIAL RECORD TRANSFER'            
SPCT01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H1,46,C'CCUSA TRAFFIC COMMERCIAL RECORD TRANSFER'                
         SSPEC H2,46,C'----------------------------------------'                
         SSPEC H1,94,AGYNAME                                                    
         SSPEC H2,94,AGYADD                                                     
         SSPEC H4,1,C'COMMERCIAL  PRODUCTS'                                     
         SSPEC H5,1,C'----------  --------'                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPCT01 08/29/00'                                      
         END                                                                    
