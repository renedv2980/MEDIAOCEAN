*          DATA SET REREPNS01  AT LEVEL 001 AS OF 11/07/96                      
*          DATA SET REREPSW01  AT LEVEL 025 AS OF 08/12/96                      
*PHASE RENS01A,*                                                                
         TITLE 'SPECS FOR AVAIL FIX'                                            
*                                                                               
*- REREPNS01 -- PHASE RENS01 -- SPECS MODULE FOR AVAIL FIX                      
*                                                                               
*                                                                               
RENS01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'FILE SWITCH INFORMATION'                                 
         SPROG 1                                                                
         ASPEC H3,9,C'***RECORD CODES***'                                       
         ASPEC H3,63,C'***RECORD COUNTS***'                                     
         ASPEC H4,1,C'OLD-REP STA   COMP  OF SAL TM GP AGY    ADV'              
         ASPEC H5,1,C'NEW-REP STA   COMP  OF SAL TM GP AGY    ADV'              
         ASPEC H7,1,C'--- --- ----- ----- -- --- -- -- -----  ---'              
         ASPEC H4,46,C'DSP PP  START'                                           
         ASPEC H5,46,C'DSP PP  DATE'                                            
         ASPEC H7,46,C'--- --- --------'                                        
         ASPEC H4,63,C'STATION CONTRACT  INV      BUDGET'                       
         ASPEC H5,63,C'SDD     SALESMAN  AGENCY   ADV'                          
         ASPEC H6,63,C'DARE    POINT PR  EOP      STRATEG'                      
         ASPEC H7,63,C'------- --------  -------  -------'                      
         ASPEC H4,99,C'ATHENA  COMMISS.  BUY      AUR'                          
         ASPEC H5,99,C'PRODUCT PROPOSAL  SET      DEVSAL'                       
         ASPEC H6,99,C'DIR RES MAKEGOOD'                                        
         ASPEC H7,99,C'------- --------  -------  -------'                      
         SPROG 2                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,39,C'DI/HN FILE MERGE FACILITY'                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001REREPNS01 11/07/96'                                      
         END                                                                    
