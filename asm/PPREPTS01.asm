*          DATA SET PPREPTS01  AT LEVEL 030 AS OF 07/18/16                      
*PHASE PPTS01A,+0                                                               
         TITLE 'PPTS01  TEARSHEET REPORT - HEADLINES'                           
PPTS01   CSECT                                                                  
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBS                                                         
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,1                                                              
         PSPEC H1,56,C'TEARSHEET REPORT'                                        
         PSPEC H2,56,C'----------------'                                        
         SPROG 10,11                                                            
         PSPEC H1,54,C'TEARSHEET TURNAROUND'                                    
         PSPEC H2,54,C'--------------------'                                    
         SPROG 0,1,10,11                                                        
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H3,53,PERIOD                                                     
         PSPEC H4,1,CLIENT                                                      
         PSPEC H6,1,ESTIMATE                                                    
         PSPEC H7,1,C'PUBLICATION'                                              
         PSPEC H7,14,PUBNUM                                                     
         PSPEC H8,1,PUBNAME                                                     
         PSPEC H8,22,PUBADD                                                     
         PSPEC H9,1,ZONENAME                                                    
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,RUN                                                        
         PSPEC H5,98,REPORT                                                     
         PSPEC H5,123,PAGE                                                      
         PSPEC H12,3,C'INSERT'                                                  
         PSPEC H13,4,C'DATE      ADCODE  SPACE'                                 
         PSPEC H14,3,C'------     ------  -----'                                
         PSPEC H11,39,C'-------------- EVALUATION ---------------'              
         PSPEC H12,39,C'       SPACE CAPTN POSTN DATE  ZONES REPO'              
         PSPEC H13,39,C'STATUS  OK?   OK?   OK?   OK?   OK?  QUAL'              
         PSPEC H14,39,C'------ ----- ----- ----- ----  ----- ----'              
         PSPEC H12,84,C'PAGE'                                                   
         PSPEC H13,82,C'NOTATION'                                               
         PSPEC H14,82,C'--------'                                               
         SPROG 0,10                                                             
         PSPEC H12,93,C'----------- LAST ACTIVITY ------------'                 
         PSPEC H13,93,C'DATE     BUYER EVALUATION DATA CHANGED'                 
         PSPEC H14,93,C'----     ----- -----------------------'                 
         SPROG 1,11                                                             
         PSPEC H12,93,C' LAST ACTIVITY'                                         
         PSPEC H13,93,C'DATE     BUYER'                                         
         PSPEC H14,93,C'----     -----'                                         
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'0102041F0708090A0B0C0D0E0F13191400'                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030PPREPTS01 07/18/16'                                      
         END                                                                    
