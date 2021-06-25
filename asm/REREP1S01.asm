*          DATA SET REREP1S01  AT LEVEL 057 AS OF 05/06/98                      
*PHASE RE1S01A,                                                                 
         TITLE 'REREP1S01 - STATION JOIN/ACTIVE REPORT'                         
**********************************************************************          
*                                                                    *          
*        REREP1S01 --- STATION JOIN/ACTIVE REPORT'                   *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* APR29/98 (BU ) --- INITIAL ENTRY                                   *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
RE1S01   CSECT                                                                  
         PRINT NOGEN                                                            
**       FSPEC READ,CONTRACTS                                                   
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,050,C'STATION JOIN DATE VS BILLING REPORT'                   
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
***>>>   ASPEC H03,054,PERIOD                                                   
         SPACE 1                                                                
         ASPEC H05,011,C'STATION'                                               
         ASPEC H05,021,C'  JOIN         LAST      LAST'                         
         SPACE 1                                                                
         ASPEC H06,021,C'  DATE         BOOK      BILL'                         
         SPACE 1                                                                
*                                                                               
         SPROG 1                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,050,C'STATION JOIN DATE NO BILLING REPORT'                   
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
***>>>   ASPEC H03,054,PERIOD                                                   
         SPACE 1                                                                
         ASPEC H05,011,C'STATION'                                               
***>>>   ASPEC H05,021,C'  JOIN         LAST      LAST'                         
         SPACE 1                                                                
***>>>   ASPEC H06,021,C'  DATE        BOOKED     BILL'                         
         SPACE 1                                                                
         ASPEC H05,021,C'  JOIN         LAST          '                         
         SPACE 1                                                                
         ASPEC H06,021,C'  DATE        BOOKED         '                         
         SPACE 1                                                                
*                                                                               
* > > > > > > > > >  > > END OF REREP1S01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057REREP1S01 05/06/98'                                      
         END                                                                    
