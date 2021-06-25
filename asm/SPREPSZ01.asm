*          DATA SET SPREPSZ01  AT LEVEL 002 AS OF 12/05/07                      
*PHASE SPSZ01A                                                                  
         TITLE 'SPSZ01 - SPOTPAK MKT PURGE PROGRAM - SPECS'                     
SPSZ01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,STATION                                                   
         SSPEC H1,55,C'MARKET PURGE PROGRAM'                                    
         SSPEC H2,55,C'--------------------'                                    
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPSZ01 12/05/07'                                      
         END                                                                    
