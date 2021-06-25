*          DATA SET PPREP4801  AT LEVEL 037 AS OF 03/03/00                      
*PHASE PP4801A,+0,NOAUTO                                                        
         TITLE 'SPECS FOR PUBLICATION LISTING PP48'                             
PP4801   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,PUBLICATIONS                                                
         FSPEC GET,DIVISIONS                                                    
         FSPEC GET,REGIONS                                                      
         FSPEC GET,DISTRICTS                                                    
         SPROG 0,1,4,5,6,7,9,15,21,23                                           
         PSPEC H1,2,MEDIA                                                       
         PSPEC H1,44,C'PUBLICATION LISTING'                                     
         PSPEC H2,44,19C'-'                                                     
         PSPEC H1,87,AGYNAME                                                    
         PSPEC H2,2,REQUESTOR                                                   
         PSPEC H2,87,AGYADD                                                     
         PSPEC H4,87,RUN                                                        
         PSPEC H5,87,REPORT                                                     
         PSPEC H5,107,PAGE                                                      
*                                                                               
         SPROG 2,3,8,10,11,12,13,14,16,17,20,22,24,25,26                        
         PSPEC H1,2,MEDIA                                                       
         PSPEC H1,50,C'PUBLICATION LISTING'                                     
         PSPEC H2,50,19C'-'                                                     
         PSPEC H1,102,AGYNAME                                                   
         PSPEC H2,2,REQUESTOR                                                   
         PSPEC H2,102,AGYADD                                                    
         PSPEC H4,102,RUN                                                       
         PSPEC H5,102,REPORT                                                    
         PSPEC H5,122,PAGE                                                      
*                                                                               
         SPROG 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,21,22,23,24         
         PSPEC H9,1,C'PUB CODE          PUBLICATION          CITY'              
         PSPEC H10,1,C'--------          -----------          ----'             
*                                                                               
         SPROG 25,26                                                            
         PSPEC H9,1,C'PUB CODE          PUBLICATION          CITY'              
         PSPEC H10,1,C'--------          -----------          ----'             
*                                                                               
         SPROG 2,3                                                              
         PSPEC H7,76,C'DEAD'                                                    
         PSPEC H8,76,C'LINE  CLOSING'                                           
         PSPEC H9,65,C'AC      CD DAYS  MO. DAY'                                
         PSPEC H10,64,C'----    --- ----  --- ---'                              
         PSPEC H8,112,C'KILL'                                                   
         PSPEC H9,112,C'DATE'                                                   
         PSPEC H10,110,C'--------'                                              
*                                                                               
         SPROG 2                                                                
         PSPEC H8,90,C'COL/ UNTS  BEST FOOD'                                    
         PSPEC H9,90,C'PAGE /COL    DAYS'                                       
         PSPEC H10,90,C'---- ----  --------'                                    
         PSPEC H8,119,C'RATE FULL PUB'                                          
         PSPEC H9,119,C'TYPE DEPTH CL'                                          
         PSPEC H10,119,C'---- ----- --'                                         
*                                                                               
*                                                                               
         SPROG 3                                                                
         PSPEC H8,90,C'PAYING    ON-SALE   '                                    
         PSPEC H9,90,C'MO. DAY   MO. DAY   '                                    
         PSPEC H10,90,C'--- ---   --- ---   '                                   
         PSPEC H8,121,C' PUB '                                                  
         PSPEC H9,121,C'CLASS'                                                  
         PSPEC H10,121,C'-----'                                                 
         PSPEC H9,127,C'FREQ'                                                   
         PSPEC H10,127,C'----'                                                  
*                                                                               
         SPROG 4                                                                
         PSPEC H8,81,C'CIRC    CIRC'                                            
         PSPEC H9,64,C'TOTAL CIRC       DATE    SRCE'                           
         PSPEC H10,64,C'----------     --------  ----'                          
*                                                                               
         SPROG 5                                                                
         PSPEC H8,81,C'CIRC    CIRC'                                            
         PSPEC H9,64,C'TOTAL CIRC       DATE    SRCE'                           
         PSPEC H10,64,C'----------     --------  ----'                          
*                                                                               
         SPROG 6                   CLT/DIV/REG/DST                              
         PSPEC H9,66,C'CLT  DIV  REG  DST  SHARE'                               
         PSPEC H10,66,C'---  ---  ---  ---  -----'                              
*                                                                               
         SPROG 7                   RATE                                         
         PSPEC H8,75,C'BASIC'                                                   
         PSPEC H8,97,C'DISCOUNT'                                                
         PSPEC H9,64,C'EFF.DATE    RATE                LEVEL RATE'              
         PSPEC H10,64,C'--------   -----                ----- ----'             
*                                                                               
         SPROG 11                  MAG RATES                                    
         PSPEC H9,73,C'SPACE DESCRIPTION'                                       
         PSPEC H10,73,C'-----------------'                                      
         PSPEC H9,108,C'EFF. DATE'                                              
         PSPEC H10,108,C'---------'                                             
         PSPEC H9,101,C'RATE'                                                   
         PSPEC H10,100,C'-----'                                                 
*                                                                               
         SPROG 15                                                               
         PSPEC H9,76,C'SPACE'                                                   
         PSPEC H10,76,C'-----'                                                  
         PSPEC H9,88,C'EQUIV. LINES'                                            
         PSPEC H10,88,C'------------'                                           
*                                                                               
         SPROG 8                   PREMIUMS                                     
         PSPEC H8,64,C'PRM              MIN.    MIN.   TYPE OF'                 
         PSPEC H9,64,C'TYP CLT EFF.DATE SIZE   CHARGE  CHARGE'                  
         PSPEC H10,64,C'--- --- -------- ----  -------- -------'                
         PSPEC H8,114,C'MAX AD SIZE'                                            
         PSPEC H9,105,C' CHARGE  APPLICABLE'                                    
         PSPEC H10,105,C'-------- -----------'                                  
*                                                                               
         SPROG 9                                                                
         PSPEC H9,65,C'GROUP MEMBERS'                                           
         PSPEC H10,65,C'-------------'                                          
*                                                                               
         SPROG 10                  REPS                                         
         PSPEC H9,64,C'STREET ADDRESS'                                          
         PSPEC H10,64,C'--------------'                                         
         PSPEC H8,95,C'----R E P S-----'                                        
         PSPEC H9,95,C' PAY  TRAF   CON  CLI'                                   
         PSPEC H10,95,C'----  ----  ----  ---'                                  
*                                                                               
         SPROG 12,13,14,22                                                      
         PSPEC H9,65,C'STREET ADDRESS'                                          
         PSPEC H10,65,C'--------------'                                         
         PSPEC H9,129,C'CLI'                                                    
         PSPEC H10,129,C'---'                                                   
*                                                                               
         SPROG 12                                                               
         PSPEC H9,98,C'PAYING ADDRESS'                                          
         PSPEC H10,98,C'--------------'                                         
*                                                                               
         SPROG 13                                                               
         PSPEC H9,98,C'TRAFFIC ADDRESS'                                         
         PSPEC H10,98,C'---------------'                                        
*                                                                               
         SPROG 14                                                               
         PSPEC H9,98,C'CONTRACT ADDRESS'                                        
         PSPEC H10,98,C'----------------'                                       
*                                                                               
         SPROG 22                                                               
         PSPEC H9,98,C'SHIPPING ADDRESS'                                        
         PSPEC H10,98,C'----------------'                                       
*                                                                               
         SPROG 16                                                               
         PSPEC H8,76,C'AD'                                                      
         PSPEC H9,66,C'DATE    NUMBER  SPACE'                                   
         PSPEC H9,99,C'RATE/COST  NOTES'                                        
         PSPEC H10,66,C'----    ------  -----'                                  
         PSPEC H10,99,C'---------  -----'                                       
*                                                                               
         SPROG 17                                                               
         PSPEC H9,73,C'CODE'                                                    
         PSPEC H10,73,C'----'                                                   
         PSPEC H9,79,C'DESCRIPTION'                                             
         PSPEC H10,79,C'-----------'                                            
         PSPEC H9,101,C'RATE'                                                   
         PSPEC H10,100,C'-----'                                                 
         PSPEC H9,110,C'EFF. DATE'                                              
         PSPEC H10,110,C'---------'                                             
*                                                                               
         SPROG 20                  PUB/CLT STANDARD COMMENTS                    
         PSPEC H9,64,C'STREET ADDRESS'                                          
         PSPEC H10,64,C'--------------'                                         
         PSPEC H8,95,C' -STANDARD COMMENTS-'                                    
         PSPEC H9,95,C' FIRST ON  SECOND ON'                                    
         PSPEC H10,95,C' ----- --  ------ --'                                   
         PSPEC H9,117,C'CLI'                                                    
         PSPEC H10,117,C'---'                                                   
*                                                                               
         SPROG 21                  ADV-AOR                                      
         PSPEC H9,65,C'ADV  PUBLICATION LINK'                                   
         PSPEC H10,65,C'---  ----------------'                                  
*                                                                               
         SPROG 23                  PAY CONTROL                                  
         PSPEC H7,64,C'      PAY IF    PAY IF             PAY'                  
         PSPEC H8,64,C'       CASH    TEARSHEET   PAY     VIA'                  
         PSPEC H9,64,C'CLI  RECEIVED  APPROVED  CONTROL  MATCH'                 
         PSPEC H10,64,C'---  --------  --------  -------  -----'                
*                                                                               
         SPROG 24                  PGA - PUB GROUP ASSIGNMENTS                  
         PSPEC H4,51,C'GROUP ASSIGNMENTS'                                       
         PSPEC H5,51,C'-----------------'                                       
*                                                                               
         SPROG 25                  PGA - PUB GROUP ASSIGNMENTS                  
         PSPEC H4,47,C'GROUP ASSIGNMENTS FOR ID ='                              
         PSPEC H5,47,C'----------------------------'                            
*                                                                               
         SPROG 24,25               PGA - PUB GROUP ASSIGNMENTS                  
         PSPEC H9,64,C'ID CODE  BREAK 1     NAME 1'                             
         PSPEC H10,64,C'-- ----  -------     ------'                            
         PSPEC H9,104,C'BREAK 2     NAME 2'                                     
         PSPEC H10,104,C'-------     ------'                                    
*                                                                               
         SPROG 26                  WWW WEB SITE                                 
         PSPEC H9,64,C'WEBSITE ADDRESS'                                         
         PSPEC H10,64,C'---------------'                                        
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037PPREP4801 03/03/00'                                      
         END                                                                    
