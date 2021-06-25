*          DATA SET REREPB301  AT LEVEL 011 AS OF 08/31/00                      
*          DATA SET REREPB301  AT LEVEL 010 AS OF 10/20/95                      
*PHASE REB301A                                                                  
         TITLE 'SPECS FOR NEW REP FILE EXTRACT/CREATION'                        
*                                                                               
*- REREPB301 -- PHASE REB301 -- SPECS MODULE FOR REP EXTRACT/CREATION           
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  10/20/94 SKU NEW                                                             
*                                                                               
REB301   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'FILE EXTRACT INFORMATION'                                
         SPROG 1                                                                
         ASPEC H3,15,C'***RECORD CODES***'                                      
         ASPEC H3,60,C'***RECORD COUNTS***'                                     
         ASPEC H4,1,C'OLD-  REP STA   COMP  OFF SAL TM GRP AGY    ADV'          
         ASPEC H5,1,C'NEW-  REP STA   COMP  OFF SAL TM GRP AGY    ADV'          
         ASPEC H6,1,C'---   --- ----- ----- --- --- -- --- -----  ---'          
         ASPEC H4,50,C'STATION CONTRACT  INV      BUDGET'                       
         ASPEC H5,50,C'SDD     SALESMAN  AGENCY   ADV'                          
         ASPEC H6,50,C'------- --------  -------  -------'                      
         ASPEC H4,86,C'ATHENA  COMMISS.  BUY'                                   
         ASPEC H5,86,C'PRODUCT'                                                 
         ASPEC H6,86,C'------- --------  -------'                               
         SPROG 2                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,39,C'REP B3 FILE EXTRACT FACILITY'                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011REREPB301 08/31/00'                                      
         END                                                                    
