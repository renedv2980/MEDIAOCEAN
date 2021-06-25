*          DATA SET REREP1A01  AT LEVEL 048 AS OF 10/16/90                      
*PHASE RE1A01A,                                                                 
         TITLE 'REREP1A01 - DISCREPANCY REPORT SPECS'                           
**********************************************************************          
*                                                                    *          
*        REREP1A01 --- REPPACK BUDGET REPORT HEADLINE SPEC           *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* AUG16/90 (BU ) --- INITIAL ENTRY                                   *          
*                                                                    *          
* SEP11/90 (BU ) --- A FORMAT CHANGE IS BEING MADE FOR THE FIRST YEAR*          
*                    IN WHICH THIS PRODUCT IS USED.  TO RETURN TO THE*          
*                    EXPANDED FORMAT, REINSTATE THE LINES MARKED     *          
*                    '*--->' AND COMMENT OUT THE ACTIVE LINES, IF    *          
*                    ACTIVE LINES EXIST.                             *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
RE1A01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         SPROG 0,1,2,3,4,5,6,7,8                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,056,C'BUDGET PREPARATION REPORT'                             
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
*--->    ASPEC H03,054,PERIOD                                                   
         SPACE 1                                                                
*--->    ASPEC H07,050,C'      PRIOR'                                           
*--->    ASPEC H07,070,C'    CURRENT'                                           
         SPACE 1                                                                
*--->    ASPEC H08,050,C'      FINAL'                                           
*--->    ASPEC H08,070,C'    BILLING'                                           
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H04,004,C'GROUP'                                                 
         ASPEC H05,006,C'STATION'                                               
         ASPEC H07,018,C'OFFICE'                                                
         SPACE 1                                                                
         SPROG 3,4,5,6,7,8                                                      
         ASPEC H05,006,C'OFFICE'                                                
         ASPEC H07,018,C'STATION'                                               
         SPACE 1                                                                
         SPROG 0,1,3,4,6,7                                                      
         ASPEC H06,050,C'     BUDGET'                                           
*--->    ASPEC H06,090,C'     BUDGET'                                           
         SPACE 1                                                                
         SPROG 4                                                                
         ASPEC H06,073,C'REVISED'                                               
*--->    ASPEC H06,113,C'REVISED'                                               
         SPACE 1                                                                
         SPROG 2,5,8                                                            
         ASPEC H06,048,C'FINAL BUDGET'                                          
*--->    ASPEC H06,088,C'FINAL BUDGET'                                          
         SPACE 1                                                                
         SPROG 0,3,6                                                            
         ASPEC H02,058,C'BUDGET WORKSHEET'                                      
         SPACE 1                                                                
         SPROG 1,4,7                                                            
         ASPEC H02,055,C'BUDGET PRELIMINARIES'                                  
         SPACE 1                                                                
         SPROG 2,5,8                                                            
         ASPEC H02,059,C'BUDGET FINALS'                                         
         SPACE 1                                                                
         SPROG 3,4,5                                                            
         ASPEC F02,050,C'PREPARED BY: '                                         
         ASPEC F02,065,C'____________________'                                  
*                                                                               
* > > > > > > > > >  > > END OF REREP1A01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048REREP1A01 10/16/90'                                      
         END                                                                    
