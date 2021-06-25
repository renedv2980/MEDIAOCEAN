*          DATA SET SPREPX501  AT LEVEL 007 AS OF 08/29/00                      
*PHASE SPX501A                                                                  
         TITLE 'SPREPX501 - P&&G SPOT INFORMATION SYSTEM SPECS'                 
         PRINT NOGEN                                                            
SPX501   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC OPEN,DEMFILES                                                    
*                                                                               
         SPROG 0,THRU,6                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,48,C'P AND G SPOT INFORMATION SYSTEM TAPE'                    
         SSPEC H1,100,AGYNAME                                                   
*                                                                               
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,48,C'------------------------------------'                    
         SSPEC H2,100,AGYADD                                                    
*                                                                               
         SSPEC H3,50,PERIOD                                                     
*                                                                               
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,100,REPORT                                                    
         SSPEC H4,125,PAGE                                                      
*                                                                               
         SPROG 1                                                                
         SSPEC H6,1,C'TY  YEAR   QTR   PRD  MKT  PUR   LEN   AGY'               
         SSPEC H7,1,C'--  -----  ----  ---  ---  ----  ----  ----'              
         SSPEC H6,46,C'       DEMOS             COST      SVC  ERRORS'          
         SSPEC H7,46,C'--------------------  -----------  ---  ------'          
*                                                                               
         SPROG 2                                                                
         SSPEC H6,1,C'TY YEAR  QTR  PRD PURP LEN  DPT  STAT'                    
         SSPEC H7,1,C'-- ----- ---- --- ---- ---- ---- ----'                    
         SSPEC H6,39,C'AGY  SPOTS    COST     DEMO1    IMPS1'                   
         SSPEC H7,39,C'---- ----- ----------- ------ ---------'                 
         SSPEC H6,79,C'DEMO2    IMPS2     HOMES   RHOM SVC ERRORS'              
         SSPEC H7,79,C'------ --------- --------- ---- --- ------'              
*                                                                               
         SPROG 3                                                                
         SSPEC H6,1,C'TY PRD PURP LEN  DPT  STAT  DATE   TIME'                  
         SSPEC H7,1,C'-- --- ---- ---- ---- ---- ------ ------'                 
         SSPEC H6,42,C'   COST     DEMO1   IMPS1    DEMO2   IMPS2'              
         SSPEC H7,42,C'----------- ------ --------- ------ ---------'           
         SSPEC H6,88,C'  HOMES   RHOM   W18+      W1849   S ERRORS'             
         SSPEC H7,88,C'--------- ---- --------- --------- - ------'             
*                                                                               
         SPROG 4,5,6                                                            
         SSPEC H4,55,C'*** SUMMARY REPORT ***'                                  
*                                                                               
         SPROG 4                                                                
         SSPEC H6,50,C'PRD   EST PURP      DOLLARS  '                           
         SSPEC H7,50,C'---   --- ----    -----------'                           
*                                                                               
         SPROG 5                                                                
         SSPEC H6,36,C'PRD   EST PURP     SPOTS      COST   '                   
         SSPEC H7,36,C'---   --- ----    ------  -----------'                   
         SSPEC H6,73,C'    HOMES       RHOMES'                                  
         SSPEC H7,73,C'  ---------   ---------'                                 
*                                                                               
         SPROG 6                                                                
         SSPEC H6,25,C'PRD   EST PURP     SPOTS      COST   '                   
         SSPEC H7,25,C'---   --- ----    ------  -----------'                   
         SSPEC H6,62,C'    HOMES      RHOMES      W18+      W1849'              
         SSPEC H7,62,C'  ---------  ---------  ---------  ---------'            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPX501 08/29/00'                                      
         END                                                                    
