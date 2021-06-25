*          DATA SET REREP1601  AT LEVEL 004 AS OF 07/30/07                      
*                                                                               
*PHASE RE1601A,*                                                                
         TITLE 'RE1601 - REREP1601 - SPECS FOR CLOSE OUT REPORT'                
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP1601 --- SPECS FOR CLOSE OUT REPORT                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* SEP/89 (MRR)   --- INITIAL RELEASE                                *           
*                                                                   *           
* JUL30/07 (BU ) --- OPEN FOR UPDATE                                *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE1601   CSECT                                                                  
         FSPEC READ,CONTRACTS                                                   
         FSPEC UPDATE,REPFIL                                                    
         FSPEC UPDATE,REPDIR                                                    
         RSPEC MAXLINES,55                                                      
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'CLOSE OUT REPORT'                                      
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,058,16C'-'                                                   
         ASPEC H02,100,RUN                                                      
         ASPEC H05,001,C'STATION / MARKET'                                      
         ASPEC H06,003,C' CONTRACT DETAILS/MONTHLY DOLLARS'                     
         ASPEC H07,003,128C'-'                                                  
         ASPEC F01,002,REQDETS                                                  
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004REREP1601 07/30/07'                                      
         END                                                                    
