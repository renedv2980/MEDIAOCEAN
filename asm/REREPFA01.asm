*          DATA SET REREPFA01  AT LEVEL 001 AS OF 04/09/12                      
*PHASE REFA01A                                                                  
         TITLE 'SPECS FOR FOX TAPE ACTUALIZATION'                               
*                                                                               
*- REREPFA01 -- PHASE REFA01 -- SPECS MODULE FOR FOX TAPE ACTUALIZING           
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  JUL16/07 (BU ) --- OPEN FIL/DIR FOR UPDATE                                   
*                                                                               
*  MAR15/12 (SMY) --- COPIED FROM REREPKA01 FOR FOX TAPE                        
*                                                                               
REFA01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC UPDATE,REPFIL                                                    
         FSPEC UPDATE,REPDIR                                                    
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'REPPAK TAPE ACTUALIZATION REPORT'                        
         ASPEC H1,90,PAGE                                                       
*SMY*    SPROG 0,1                                                              
         SPROG 2                                                                
         ASPEC H2,36,C'STATION CLOSE-DATE NOTIFICATION'                         
*                              1         2         3         4                  
*                      .2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8         
         ASPEC H4,01,C'STATION  COMPANY   NEW       OLD       COMMENT'          
         ASPEC H5,01,C'                   CLOSE     CLOSE'                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001REREPFA01 04/09/12'                                      
         END                                                                    
