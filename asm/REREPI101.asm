*          DATA SET REREPI101  AT LEVEL 014 AS OF 11/03/95                      
*PHASE REI101A,*                                                                
         TITLE 'SPECS FOR INTEREP COMPANY SPLIT'                                
*                                                                               
*- REREPI101 -- PHASE REI101 -- SPECS MODULE FOR COMPANY SPLIT                  
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REI101   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
         ASPEC H1,002,REP                                                       
         ASPEC H1,33,C'CABALLERO/RADIO MARKETING SPLIT JOB'                     
         ASPEC H1,100,RENUM                                                     
         ASPEC H1,120,PAGE                                                      
         ASPEC H2,002,REQUESTOR                                                 
         ASPEC H2,100,RUN                                                       
         ASPEC H5,01,C'ACTION'                                                  
         ASPEC H5,15,C'CONTRACT#'                                               
         ASPEC H5,30,C'CREATED:'                                                
         ASPEC H5,40,C'FLT START'                                               
         ASPEC H5,50,C'FLT END  '                                               
         ASPEC H6,01,C'------'                                                  
         ASPEC H6,15,C'---------'                                               
         ASPEC H6,30,C'--------'                                                
         ASPEC H6,40,C'---------'                                               
         ASPEC H6,50,C'---------'                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014REREPI101 11/03/95'                                      
         END                                                                    
