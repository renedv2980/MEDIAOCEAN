*          DATA SET REREP0101  AT LEVEL 004 AS OF 08/31/00                      
*          DATA SET REREP0101  AT LEVEL 003 AS OF 03/05/96                      
*PHASE RE0101A                                                                  
         TITLE 'REREP0101 - TEST FOR MEL'                                       
RE0101   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
**       FSPEC UPDATE,REPFIL                                                    
         ASPEC H01,002,REP                                                      
         ASPEC H01,056,C'YET ANOTHER TESTING PROGRAM'                           
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004REREP0101 08/31/00'                                      
         END                                                                    
