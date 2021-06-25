*          DATA SET SPREP4501  AT LEVEL 013 AS OF 11/15/90                      
*PHASE SP4501A,+0                                                               
        TITLE 'SP045-01 SPOT STATION ACTIVITY'                                  
SP4501   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SPRQ03                                                       
         RSPEC REQUEST,NOREP                                                    
         SPACE                                                                  
         SPROG 1,2,3                                                            
         SSPEC H1,1,MEDIA                                                       
         SSPEC H4,99,PAGE                                                       
         SSPEC H4,110,REPORT                                                    
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SPACE                                                                  
         SPROG 1,2                                                              
         SSPEC H1,55,C'STATION ACTIVITY'                                        
         SSPEC H2,55,C'----------------'                                        
         SPACE 3                                                                
         SPROG 1                                                                
         SPACE                                                                  
         SSPEC H7,1,C' '                                                        
         SSPEC H9,16,C'MEDIA'                                                   
         SSPEC H10,16,C'-----'                                                  
         SSPEC H9,40,C'NAME'                                                    
         SSPEC H10,40,C'----'                                                   
         SSPEC H9,63,C'ADDRESS'                                                 
         SSPEC H10,63,C'-------'                                                
         SSPEC H9,88,C'CITY'                                                    
         SSPEC H10,88,C'----'                                                   
         SSPEC H9,105,C'STATE   ZIP'                                            
         SSPEC H10,105,C'-----   ---'                                           
         SSPEC H9,120,C'POSTAL CODE'                                            
         SSPEC H10,120,C'-----------'                                           
         SPACE                                                                  
         SPROG 2                                                                
         SSPEC H9,16,C'MEDIA'                                                   
         SSPEC H10,16,C'-----'                                                  
         SSPEC H9,23,C'STATION'                                                 
         SSPEC H10,23,C'-------'                                                
         SSPEC H9,32,C'TYPE'                                                    
         SSPEC H10,32,C'----'                                                   
         SSPEC H9,42,C'NETWORK'                                                 
         SSPEC H10,42,C'-------'                                                
         SSPEC H9,55,C'CHANNEL'                                                 
         SSPEC H10,55,C'-------'                                                
         SSPEC H9,68,C'CLIENT'                                                  
         SSPEC H10,68,C'------'                                                 
         SSPEC H9,82,C'MARKET'                                                  
         SSPEC H10,82,C'------'                                                 
         SSPEC H9,96,C'REP(S)'                                                  
         SSPEC H10,96,C'------'                                                 
         SSPEC H9,110,C'GST CODE'                                               
         SSPEC H10,110,C'--------'                                              
         SPACE                                                                  
         SPROG 3                                                                
         SSPEC H1,55,C'MARKET ACTIVITY'                                         
         SSPEC H2,55,C'---------------'                                         
         SSPEC H2,1,REQUESTOR                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPREP4501 11/15/90'                                      
         END                                                                    
