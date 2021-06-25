*          DATA SET REREPI201  AT LEVEL 015 AS OF 01/06/96                      
*PHASE REI201A,*                                                                
         TITLE 'SPECS FOR KATZ STATION MOVER: ORDERS/BUYS'                      
*                                                                               
*- REREPI201 -- PHASE REI201 -- SPECS MODULE FOR STATION MOVER                  
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REI201   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
         ASPEC H1,002,REP                                                       
         ASPEC H1,35,C'KATZ STATION MOVER: ORDERS/BUYS'                         
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
**PAN#1  DC    CL21'015REREPI201 01/06/96'                                      
         END                                                                    
