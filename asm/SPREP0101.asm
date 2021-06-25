*          DATA SET SPREP0101  AT LEVEL 012 AS OF 08/29/00                      
*PHASE SP0101A                                                                  
         TITLE 'SPONSOR TEST SPECS'                                             
SP0101   CSECT                                                                  
         FSPEC USE,SP0003                                                       
*        FSPEC READ,BUYS                                                        
*        FSPEC UPDATE,SPTFILE                                                   
*        FSPEC UPDATE,SPTDIR                                                    
*        FSPEC READ,GOALS                                                       
*        FSPEC GET,MARKET                                                       
*        FSPEC GET,STATION                                                      
         SPROG 0,1,2,3                                                          
         PSPEC H1,2,REPORT                                                      
         PSPEC H1,40,C'SPONSOR TEST PROGRAM'                                    
         PSPEC H2,40,20C'-'                                                     
         PSPEC H1,80,PAGE                                                       
         PSPEC H1,100,REQUESTOR                                                 
         PSPEC H6,2,C'REQUEST DETAILS'                                          
         PSPEC H7,2,15C'-'                                                      
         SPROG 1,THRU,3                                                         
         PSPEC H5,2,C'SELECTED 1-3'                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPREP0101 08/29/00'                                      
         END                                                                    
