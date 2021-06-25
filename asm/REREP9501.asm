*          DATA SET REREP9501  AT LEVEL 029 AS OF 04/23/01                      
*PHASE RE9501A                                                                  
RE9501   CSECT                                                                  
         FSPEC GET,AGENCY                                                       
         FSPEC GET,ADVERTISER                                                   
         FSPEC GET,PRODUCT                                                      
         FSPEC READ,CONTRACTS                                                   
*        FSPEC UPDATE,REPFIL                                                    
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,056,C'SPOT COUNT BY ORDER/MONTH'                             
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H03,001,C'GROUP        OFFICE        STATION/MARKET'             
         ASPEC H03,049,C'S/P AGENCY               ADVERTISER'                   
         ASPEC H03,083,C'            CONTRACT'                                  
         ASPEC H04,001,C'PRODUCT              1ST BUY  MONTH   SPOTS '          
         ASPEC H04,044,C' ESTIMATE        BILLING'                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029REREP9501 04/23/01'                                      
         END                                                                    
