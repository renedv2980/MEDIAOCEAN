*          DATA SET REREP1H01  AT LEVEL 060 AS OF 05/10/07                      
*PHASE RE1H01C,                                                                 
         TITLE 'REREP1H01 - GENERAL CONTRACT FIXER '                            
**********************************************************************          
*                                                                    *          
*        REREP1H01 --- REPPACK GENERAL CONTRACT FIXER                *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* FEB01/93 (BU ) --- INITIAL ENTRY:  SELF-APPLIED MAKEGOODS          *          
* SEP19/06 (BU ) --- MERGED WITH DARE STATION-OFFERS                 *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
RE1H01   CSECT                                                                  
         PRINT NOGEN                                                            
*        FSPEC READ,CONTRACTS                                                   
*        FSPEC UPDATE,REPFIL                                                    
         SPROG 0                                                                
*                                                                               
         ASPEC H01,002,REP                                                      
         ASPEC H01,052,C'UNAPPROVED SELF-APPLIED MAKEGOODS'                     
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
*                                                                               
         ASPEC H04,002,C'SAL'                                                   
         ASPEC H04,008,C'SALESPERSON'                                           
         ASPEC H04,031,C'OFF'                                                   
         ASPEC H04,035,C'STATION'                                               
         ASPEC H04,044,C'CONTRACT#'                                             
         ASPEC H04,055,C'MISS'                                                  
         ASPEC H04,062,C'MKG'                                                   
         ASPEC H04,066,C'MAS'                                                   
         ASPEC H04,072,C'LN'                                                    
         ASPEC H04,078,C'SUB'                                                   
         ASPEC H04,082,C'MULTI'                                                 
         ASPEC H04,088,C'SENT'                                                  
         ASPEC H04,093,C'FLIGHT'                                                
*                                                                               
         ASPEC H05,002,C'CDE'                                                   
         ASPEC H05,008,C'NAME'                                                  
         ASPEC H05,031,C'ICE'                                                   
         ASPEC H05,055,C'LINE'                                                  
         ASPEC H05,062,C'OFR'                                                   
         ASPEC H05,066,C'LN#'                                                   
         ASPEC H05,072,C'#'                                                     
         ASPEC H05,078,C'REC'                                                   
         ASPEC H05,088,C'2STA'                                                  
*                                                                               
         SPROG 1                                                                
*                                                                               
         ASPEC H01,002,REP                                                      
         ASPEC H01,047,C'DARE STATION CREATED UNAPPROVED MAKEGOODS'             
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
*                                                                               
         ASPEC H04,002,C'SAL'                                                   
         ASPEC H04,008,C'SALESPERSON'                                           
         ASPEC H04,031,C'OFF'                                                   
         ASPEC H04,035,C'STATION'                                               
         ASPEC H04,044,C'CONTRACT#'                                             
***>>    ASPEC H04,055,C'MISS'                                                  
         ASPEC H04,062,C'MKG'                                                   
         ASPEC H04,066,C'MAS'                                                   
         ASPEC H04,072,C'LN'                                                    
         ASPEC H04,078,C'SUB'                                                   
         ASPEC H04,082,C'MULTI'                                                 
*                                                                               
         ASPEC H05,002,C'CDE'                                                   
         ASPEC H05,008,C'NAME'                                                  
         ASPEC H05,031,C'ICE'                                                   
***>>    ASPEC H05,055,C'LINE'                                                  
         ASPEC H05,062,C'OFR'                                                   
         ASPEC H05,066,C'LN#'                                                   
         ASPEC H05,072,C'#'                                                     
         ASPEC H05,078,C'REC'                                                   
*                                                                               
         SPACE 1                                                                
* > > > > > > > > >  > > END OF REREP1H01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060REREP1H01 05/10/07'                                      
         END                                                                    
