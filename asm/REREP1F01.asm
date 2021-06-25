*          DATA SET REREP1F01  AT LEVEL 054 AS OF 07/02/07                      
*PHASE RE1F01A,                                                                 
         TITLE 'REREP1F01 - BUDGET PROJECTION SPECS'                            
**********************************************************************          
*                                                                    *          
*        REREP1F01 --- REPPACK BUDGET REPORT HEADLINE SPEC           *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* AUG31/92 (BU ) --- INITIAL ENTRY                                   *          
*                                                                    *          
* DEC30/96 (BU ) --- KILL 'READ,CONTRACTS'                           *          
*                                                                    *          
* JUL02/07 (BU ) --- PERMIT UPDATE OF REPDIR                         *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
RE1F01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC UPDATE,REPFIL                                                    
         FSPEC UPDATE,REPDIR                                                    
         SPROG 0,3,4,5                                                          
         ASPEC H01,002,REP                                                      
         ASPEC H01,056,C'ADVERTISER CATEGORY FIXER'                             
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         SPACE 1                                                                
* > > > > > > > > >  > > END OF REREP1F01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054REREP1F01 07/02/07'                                      
         END                                                                    
