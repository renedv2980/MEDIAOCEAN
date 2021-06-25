*          DATA SET SPREPAX01  AT LEVEL 011 AS OF 10/16/06                      
*PHASE SPAX01A                                                                  
         TITLE 'SPAX01 - SPOT CLEARNACE UPDATE'                                 
SPAX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
*                                                                               
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H1,57,C'SPOT/NET CLEARANCE UPDATE'                               
         SSPEC H2,57,C'-------------------------'                               
         SSPEC H4,2,C' ALPHA ID    MEDIA   CLIENT   MARKET   STATION'           
         SSPEC H4,50,C'  DATE    CHECK NUMBER   SEQ NUM   BANK CLEARED'         
         SSPEC H5,2,C' --------    -----   ------   ------   -------'           
         SSPEC H5,50,C'  ----    ------------   -------   ------------'         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPREPAX01 10/16/06'                                      
         END                                                                    
