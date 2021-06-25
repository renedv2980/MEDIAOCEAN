*          DATA SET SPREPDZ01  AT LEVEL 001 AS OF 01/10/06                      
*PHASE SPDZ01A                                                                  
         TITLE 'SPDZ01 - SPOTPAK DLUELEM FIX - SPECS'                           
SPDZ01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,55,C'DEMO LOOKUP DATA FIX'                                    
         SSPEC H2,55,C'--------------------'                                    
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPDZ01 01/10/06'                                      
         END                                                                    
