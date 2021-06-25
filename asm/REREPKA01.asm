*          DATA SET REREPKA01  AT LEVEL 022 AS OF 07/16/07                      
*PHASE REKA01A,*                                                                
         TITLE 'SPECS FOR KATZ TAPE ACTUALIZATION'                              
*                                                                               
*- REREPKA01 -- PHASE REKA01 -- SPECS MODULE FOR KATZ TAPE ACTUALIZING          
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  JUL16/07 (BU ) --- OPEN FIL/DIR FOR UPDATE                                   
*                                                                               
REKA01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC UPDATE,REPFIL                                                    
         FSPEC UPDATE,REPDIR                                                    
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'KATZ TAPE ACTUALIZATION'                                 
         ASPEC H1,90,PAGE                                                       
         SPROG 0,1                                                              
         ASPEC H2,33,C'ADD-LINE DEFAULT SETTINGS AND ERRORS'                    
*                              1         2         3         4                  
*                      2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.         
         ASPEC H4,02,C'COMP STATION CONTRACT  AGENCY  ADVERTISER '              
         ASPEC H4,44,C'PRODUCT      CTGY S/P CON FLIGHT DATES  AMOUNT'          
         ASPEC H5,02,C' ANY'                                                    
         ASPEC H5,44,C'                      TYP'                               
         SPROG 2                                                                
         ASPEC H2,36,C'STATION CLOSE-DATE NOTIFICATION'                         
*                              1         2         3         4                  
*                      .2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8         
         ASPEC H4,01,C'STATION  COMPANY   NEW       OLD       COMMENT'          
         ASPEC H5,01,C'                   CLOSE     CLOSE'                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022REREPKA01 07/16/07'                                      
         END                                                                    
