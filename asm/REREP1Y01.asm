*          DATA SET REREP1Y01  AT LEVEL 058 AS OF 11/18/98                      
*PHASE RE1Y01A,                                                                 
         TITLE 'REREP1Y01 - GENERAL CONTRACT FIXER '                            
**********************************************************************          
*                                                                    *          
*        REREP1Y01 --- REPPACK MAKEGOOD S/P TEAM INITIALIZER         *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* NOV18/98 (BU ) --- INITIAL ENTRY                                   *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
RE1Y01   CSECT                                                                  
         PRINT NOGEN                                                            
*        FSPEC READ,CONTRACTS                                                   
*        FSPEC UPDATE,REPFIL                                                    
         SPROG 0,3,4,5                                                          
         ASPEC H01,002,REP                                                      
         ASPEC H01,056,C'MAKEGOOD DATE INSERTION  '                             
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         SPACE 1                                                                
* > > > > > > > > >  > > END OF REREP1Y01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058REREP1Y01 11/18/98'                                      
         END                                                                    
