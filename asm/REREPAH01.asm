*          DATA SET REREPAH01  AT LEVEL 066 AS OF 01/19/01                      
*PHASE REAH01A,                                                                 
         TITLE 'REREPAH01 - ALLHIST REPORT SPECS'                               
**********************************************************************          
*                                                                    *          
*        REREPAH01 --- REPPACK ALLHIST REPORT HDLINE SPEC            *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* JAN18/01 (BU ) --- INITIAL ENTRY                                   *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
REAH01   CSECT                                                                  
         PRINT NOGEN                                                            
*--->    FSPEC READ,CONTRACTS                                                   
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'ALLHIST  REPORT'                                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H04,002,C'ORIGINAL REP'                                          
         ASPEC H04,030,C'ORIGINAL'                                              
         ASPEC H05,030,C'CONTRACT'                                              
         ASPEC H04,050,C'NEW     '                                              
         ASPEC H05,050,C'CONTRACT'                                              
*                                                                               
* > > > > > > > > >  > > END OF REREPAH01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066REREPAH01 01/19/01'                                      
         END                                                                    
