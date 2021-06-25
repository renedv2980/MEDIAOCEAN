*          DATA SET REREPBB01  AT LEVEL 027 AS OF 08/31/00                      
*          DATA SET REREPBB01  AT LEVEL 026 AS OF 08/24/99                      
*PHASE REBB01A                                                                  
         TITLE 'SPECS FOR NBC BACK BILLING UPLOAD'                              
*                                                                               
*- REREPBB01 -- PHASE REBB01 -- SPECS NBC BACK BILLING UPLOAD                   
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REBB01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 1                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'NBC BACK BILLING UPLOAD'                                 
         ASPEC H1,90,PAGE                                                       
         SPROG 1                                                                
         ASPEC H3,01,C'UPDATE'                                                  
         ASPEC H3,20,C'AGENCY'                                                  
         ASPEC H3,32,C'AGENCY'                                                  
         ASPEC H4,01,C'ACTION'                                                  
         ASPEC H4,20,C'CODE--'                                                  
         ASPEC H4,32,C'NAME-----------------------------'                       
*                              1         2         3         4                  
         SPROG 2                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'NBC BACK BILLING UPLOAD'                                 
         ASPEC H1,90,PAGE                                                       
         SPROG 2                                                                
         ASPEC H3,01,C'UPDATE'                                                  
         ASPEC H3,25,C'ADVERTISER'                                              
         ASPEC H3,40,C'ADVERTISER'                                              
         ASPEC H4,01,C'ACTION'                                                  
         ASPEC H4,25,C'CODE--'                                                  
         ASPEC H4,40,C'NAME----------------'                                    
*                              1         2         3         4                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027REREPBB01 08/31/00'                                      
         END                                                                    
