*          DATA SET REREP1G01  AT LEVEL 069 AS OF 10/06/94                      
*PHASE RE1G01A,                                                                 
         TITLE 'REREP1G01 - OFFICE BUDGET REPORT SPECS'                         
**********************************************************************          
*                                                                    *          
*        REREP1G01 --- REPPACK OFFICE BUDGET REPORT SPECS            *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* NOV06/92 (BU ) --- INITIAL ENTRY                                   *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
RE1G01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,053,C'INTERNAL BUDGET REPORT'                                
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,054,PERIOD                                                   
         ASPEC H02,100,RUN                                                      
         ASPEC H05,038,C'CURRENT      CURRENT     % TO          '               
         ASPEC H05,076,C' CURRENT       % TO          PRIOR     '               
         ASPEC H05,115,C'   % TO      '                                         
         ASPEC H06,038,C'BILLING     BILL POOL    BILL POOL     '               
         ASPEC H06,076,C'  BUDGET       BUDGET        FINAL     '               
         ASPEC H06,115,C'   FINAL     '                                         
*                                                                               
* > > > > > > > > >  > > END OF REREP1G01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069REREP1G01 10/06/94'                                      
         END                                                                    
