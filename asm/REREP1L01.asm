*          DATA SET REREP1L01  AT LEVEL 030 AS OF 07/08/98                      
*PHASE RE1L01A,                                                                 
         TITLE 'REREP1Z01 - BUDGET/BILLING REVENUE PROJECTIONS'                 
**********************************************************************          
*                                                                    *          
*        REREP1Z01 --- REPPACK BUDGET/BILLING REVENUE PROJECTIONS    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* AUG16/90 (BU ) --- INITIAL ENTRY: CLONED FROM REREP1A01            *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
RE1L01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         SPROG 0,1,2,3,4,5,6,7,8,9,10                                           
         ASPEC H01,002,REP                                                      
*                        4     5         6         7                            
*                        5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9                    
         ASPEC H01,045,C'MONTHLY BUDGET/BILLING REVENUE '                       
         ASPEC H01,076,C'PROJECTION REPORT'                                     
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,054,PERIOD                                                   
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H04,004,C'GROUP'                                                 
         ASPEC H05,006,C'STATION'                                               
         ASPEC H06,008,C'OFFICE'                                                
         ASPEC H07,013,C'MONTH'                                                 
         SPACE 1                                                                
         SPROG 3,4,5,6,7,8                                                      
         ASPEC H05,006,C'OFFICE'                                                
         ASPEC H06,008,C'STATION'                                               
         ASPEC H07,013,C'MONTH'                                                 
*        SPACE 1                                                                
         SPROG 0,3,6                                                            
         ASPEC H06,048,C'BUDGET'                                                
         ASPEC H06,067,C'RATE'                                                  
         ASPEC H05,079,C'PROJ   '                                               
         ASPEC H06,079,C'REVENUE'                                               
         SPACE 1                                                                
         SPROG 1,4,7                                                            
         ASPEC H06,047,C'BILLING'                                               
         ASPEC H06,067,C'RATE'                                                  
         ASPEC H05,079,C'BILLING'                                               
         ASPEC H06,079,C'REVENUE'                                               
         SPACE 1                                                                
         SPROG 2,5,8                                                            
         ASPEC H06,048,C'BUDGET'                                                
         ASPEC H06,063,C'BILLING'                                               
         ASPEC H06,081,C' RATE '                                                
         ASPEC H05,098,C'PROJ'                                                  
         ASPEC H06,095,C'REVENUE'                                               
         ASPEC H05,111,C'BILLING'                                               
         ASPEC H06,111,C'REVENUE'                                               
         ASPEC H06,127,C'INDEX'                                                 
         SPACE 1                                                                
         SPROG 0,3,6                                                            
         ASPEC H03,055,C'BUDGET ALLOCATIONS ONLY'                               
         SPACE 1                                                                
         SPROG 1,4,7                                                            
         ASPEC H03,055,C'CURRENT BILLING ONLY'                                  
         SPACE 1                                                                
         SPROG 2,5,8                                                            
         ASPEC H03,058,C'BUDGET + BILLING'                                      
         SPACE 1                                                                
         SPROG 9,10                                                             
         ASPEC H03,058,C'EXCEPTION LISTING'                                     
         SPACE 1                                                                
         SPROG 9                                                                
         ASPEC H05,056,C'STATIONS WITH DEPART DATES'                            
         SPACE 1                                                                
         SPROG 10                                                               
         ASPEC H05,050,C'STATIONS USING DEFAULT COMMISSION RATE'                
*                                                                               
* > > > > > > > > >  > > END OF REREP1A01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030REREP1L01 07/08/98'                                      
         END                                                                    
