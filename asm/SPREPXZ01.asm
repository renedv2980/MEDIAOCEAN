*          DATA SET SPREPXZ01  AT LEVEL 002 AS OF 08/29/00                      
*PHASE SPXZ01A                                                                  
         TITLE 'SPXZ01 - PEPSI FILE EXTRACT'                                    
SPXZ01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H1,56,C'PEPSI FILE EXTRACT'                                      
         SSPEC H2,56,C'------------------'                                      
         SSPEC H1,94,AGYNAME                                                    
         SSPEC H2,94,AGYADD                                                     
         SSPEC H4,34,C' GROSS ORDERED    GROSS PAID      NET PAID'              
         SSPEC H5,34,C' -------------    ----------      --------'              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPXZ01 08/29/00'                                      
         END                                                                    
