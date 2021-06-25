*          DATA SET REREPCM01  AT LEVEL 033 AS OF 09/21/95                      
*PHASE RECM01A,                                                                 
         TITLE 'REREPCM01 - SACP SWEEP'                                         
**********************************************************************          
*                                                                    *          
*        REREPCM01 --- SACP SWEEP                                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* MAR07/95 (BU ) --- INITIAL ENTRY: CLONED FROM REREP1B01            *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
RECM01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,053,C'SACP SWEEP              '                              
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         SPACE 1                                                                
*                                                                               
* > > > > > > > > >  > > END OF REREPCM01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033REREPCM01 09/21/95'                                      
         END                                                                    
