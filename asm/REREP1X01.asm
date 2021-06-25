*          DATA SET REREP1X01  AT LEVEL 055 AS OF 11/24/93                      
*PHASE RE1X01A,                                                                 
         TITLE 'REREP1X01 - GENERAL CONTRACT FIXER '                            
**********************************************************************          
*                                                                    *          
*        REREP1X01 --- REPPACK GENERAL CONTRACT FIXER                *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* FEB01/93 (BU ) --- INITIAL ENTRY:  WOTV/WOOD FIX                   *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
RE1X01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,056,C'OFFICE COMMENT ANALYZER  '                             
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H04,001,C'OF CO REP   FLIGHT'                                    
         ASPEC H04,033,C'CONTRACT'                                              
         ASPEC H04,046,C'COMMENT: 1ST LINE'                                     
         SPACE 1                                                                
         ASPEC H05,001,C'FC DE'                                                 
         SPACE 1                                                                
* > > > > > > > > >  > > END OF REREP1X01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055REREP1X01 11/24/93'                                      
         END                                                                    
