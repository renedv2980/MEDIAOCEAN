*          DATA SET SPREPFE01  AT LEVEL 002 AS OF 08/29/00                      
*PHASE SPFE01A                                                                  
         TITLE 'SPONSOR TEST SPECS'                                             
SP0101   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         PSPEC H1,2,REPORT                                                      
         PSPEC H1,40,C'STATION BUCKET FIX'                                      
         PSPEC H2,40,18C'-'                                                     
         PSPEC H1,80,PAGE                                                       
         PSPEC H1,100,REQUESTOR                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPFE01 08/29/00'                                      
         END                                                                    
