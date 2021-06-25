*          DATA SET REREPSW01  AT LEVEL 026 AS OF 06/16/98                      
*PHASE RESW01B,*                                                                
         TITLE 'SPECS FOR FILE DATA SWITCH'                                     
*                                                                               
*- REREPSW01 -- PHASE RESW01 -- SPECS MODULE FOR REP SWITCH                     
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  09/05/89  PJS  ADDED COMMISSION COLUMN                                       
*                                                                               
*  03/06/96  WSB  ADDED DARE, MAKEGOOD, SET, DEVSAL, PP, EOP, START             
*                 DATE, STRATEGY, DIRECT RESP                                   
*                                                                               
*  08/05/96  WSB  ADDED PROPOSAL                                                
*                                                                               
*  06/16/98  JRD  ADDED SELWIN RECORD                                           
*                                                                               
RESW01   CSECT                                                                  
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
         ASPEC H6,99,C'DIR RES MAKEGOOD  SELWIN'                                
         ASPEC H7,99,C'------- --------  -------  -------'                      
         SPROG 2                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,39,C'DI/HN FILE MERGE FACILITY'                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026REREPSW01 06/16/98'                                      
         END                                                                    
