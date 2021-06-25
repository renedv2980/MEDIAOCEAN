*          DATA SET CTREP3001  AT LEVEL 005 AS OF 08/22/00                      
*PHASE CT3001A                                                                  
         TITLE 'SPECS FOR CPP XTRACT REPORT'                                    
         PRINT NOGEN                                                            
CT3001   CSECT                                                                  
         ASPEC H1,2,RUN                                                         
         ASPEC H1,41,C'CPP EXTRACT RULES'                                       
         ASPEC H2,41,C'-----------------'                                       
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,C'AGENCY'                                                   
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H8,2,C'CLIENT  PRODUCT  ESTIMATE  START    END'                  
         ASPEC H9,2,C' CODE    CODE     CODE     DATE     DATE'                 
         ASPEC H8,48,C'DEMO LIST'                                               
         ASPEC H9,48,C'---------'                                               
         ASPEC H8,72,C'PROGTYP EQUIVALENCE'                                     
         ASPEC H9,72,C'-------------------'                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTREP3001 08/22/00'                                      
         END                                                                    
