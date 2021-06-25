*          DATA SET REREPFX01  AT LEVEL 002 AS OF 08/31/00                      
*          DATA SET REREPFX01  AT LEVEL 001 AS OF 08/11/99                      
*          DATA SET REREP1H01  AT LEVEL 058 AS OF 04/04/99                      
*PHASE REFX01A                                                                  
         TITLE 'REREP1H01 - GENERAL CONTRACT FIXER '                            
**********************************************************************          
*                                                                    *          
*        REREP1H01 --- REPPACK GENERAL CONTRACT FIXER                *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* FEB01/93 (BU ) --- INITIAL ENTRY:  WOTV/WOOD FIX                   *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
REFX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC UPDATE,REPFIL                                                    
         SPROG 0,3,4,5                                                          
         ASPEC H01,002,REP                                                      
         ASPEC H01,056,C'GENERAL REPFILE SCANNER  '                             
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         SPACE 1                                                                
* > > > > > > > > >  > > END OF REREP1H01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002REREPFX01 08/31/00'                                      
         END                                                                    
