*          DATA SET REREP7401  AT LEVEL 009 AS OF 02/12/97                      
*PHASE RE7401A,+0                                                               
         TITLE 'SPECS FOR PRODUCT LISTING'                                      
RE7401   CSECT                                                                  
*        PRINT NOGEN                                                            
         FSPEC READ,PRODUCTS                                                    
         FSPEC GET,ADVERTISER                                                   
*                                                                               
*                                                                               
         SPROG 1                                                                
         ASPEC H1,02,REP1                                                       
         ASPEC H1,49,C'PRODUCT LISTING'                                         
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H2,2,RUN                                                         
         ASPEC H2,49,15C'-'                                                     
         ASPEC H2,88,REQUESTOR                                                  
         ASPEC H4,2,C'ADVERTISER'                                               
         ASPEC H7,02,C'CODE PRODUCT NAME         CLASS  '    AGENCY'            
         ASPEC H7,39,C'CODE PRODUCT NAME         CLASS  '    ------'            
         ASPEC H7,76,C'CODE PRODUCT NAME         CLASS  '                       
         ASPEC H8,02,C'---- ------------        CATEGORY'                       
         ASPEC H8,39,C'---- ------------        CATEGORY'                       
         ASPEC H8,76,C'---- ------------        CATEGORY'                       
*                                                                               
* DETAIL REPORT                                                                 
*                                                                               
         SPROG 3                                                                
         ASPEC H1,2,REP1                                                        
         ASPEC H1,45,C'DETAIL PRODUCT LISTING'                                  
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H2,2,RUN                                                         
         ASPEC H2,45,22C'-'                                                     
         ASPEC H2,88,REQUESTOR                                                  
         ASPEC H4,2,C'ADVERTISER'                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009REREP7401 02/12/97'                                      
         END                                                                    
