*          DATA SET SPREPXA01  AT LEVEL 003 AS OF 08/29/00                      
*          DATA SET SPREPXA01  AT LEVEL 004 AS OF 11/05/85                      
*PHASE SPXA01A                                                                  
         TITLE 'SPXA01 - ATT FILE EXTRACT'                                      
SPXA01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H1,57,C'ATT FILE EXTRACT'                                        
         SSPEC H2,57,C'----------------'                                        
         SSPEC H1,94,AGYNAME                                                    
         SSPEC H2,94,AGYADD                                                     
         SSPEC H4,34,C' GROSS ORDERED    GROSS PAID      NET PAID'              
         SSPEC H5,34,C' -------------    ----------      --------'              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPXA01 08/29/00'                                      
         END                                                                    
