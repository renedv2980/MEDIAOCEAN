*          DATA SET REREP4701  AT LEVEL 028 AS OF 02/07/92                      
*PHASE RE4701A,*                                                                
         TITLE 'RE4701 - REREP4701 - SPECS FOR COMMISSION REPORT'               
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP4701 --- SPECS FOR COMMISSION REPORT                  *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUL17/89 (MRR) --- INITIAL RELEASE                                *           
*                                                                   *           
* JAN22/92 (MRR) --- CHANGE LAST MONTH COLUMN NAME TO ADJUSTMENT    *           
*                                                                   *           
* JAN29/92 (MRR) --- MOVE LITERALS FROM RE4702 TO HERE              *           
*                                                                   *           
* FEB06/92 (MRR) --- ADD TOTALS COL, CHANGE COL NAMES, MOVE STUFF   *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE4701   CSECT                                                                  
         FSPEC READ,CONTRACTS                                                   
         RSPEC MAXLINES,55                                                      
         ASPEC H01,002,REP                                                      
         ASPEC H01,053,C'MONTHLY COMMISSION REPORT'                             
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,053,C'-------------------------'                             
         ASPEC H02,100,RUN                                                      
         ASPEC H03,054,C'FOR THE MONTH OF'                                      
         ASPEC H04,043,C'FOR THE ACCOUNTING PERIOD OF'                          
         ASPEC H06,001,C'STATION'                                               
         ASPEC H07,004,C'OFFICE'                                                
         ASPEC H08,001,C'--------------------------'                            
         ASPEC H06,029,C'  MMM/YY AND PRIOR ADJ'                                
         ASPEC H07,029,C'   GROSS    COMMISSION'                                
         ASPEC H08,029,C'----------- -----------'                               
         ASPEC H06,053,C'-- MMM/YY ADJUSTMENT --'                               
         ASPEC H07,053,C'   GROSS    COMMISSION'                                
         ASPEC H08,053,C'----------- -----------'                               
         ASPEC H06,077,C'--- MMM/YY ESTIMATE ---'                               
         ASPEC H07,077,C'   GROSS    COMMISSION'                                
         ASPEC H08,077,C'----------- -----------'                               
         ASPEC H06,101,C'------- TOTALS --------'                               
         ASPEC H07,101,C'   GROSS    COMMISSION'                                
         ASPEC H08,101,C'----------- -----------'                               
         ASPEC F01,002,REQDETS                                                  
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028REREP4701 02/07/92'                                      
         END                                                                    
