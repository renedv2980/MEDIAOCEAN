*          DATA SET SPREPCD01  AT LEVEL 006 AS OF 12/11/01                      
*PHASE SPCD01A                                                                  
         TITLE 'SPCD - LOCKIN/GOAL STATUS'                                      
*                                                                               
**********************************************************************          
*                                                                               
*        NOTE- THIS IS A CLONE OF SPREPCC01                                     
*                                                                               
**********************************************************************          
*                                                                               
SPCD01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SPK403    (GOALS THEN BUYS)                                  
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
*                                                                               
         SSPEC H1,50,C'LOCKIN/GOAL STATUS'                                      
         SSPEC H2,50,C'------------------'                                      
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         SSPEC H7,1,C'M'                                                        
         SSPEC H7,4,C'CLT'                                                      
         SSPEC H7,9,C'PRD'                                                      
         SSPEC H7,14,C'EST'                                                     
         SSPEC H7,19,C'MARKET'                                                  
         SSPEC H7,26,C'STATION'                                                 
         SSPEC H7,35,C'WEEK'                                                    
         SSPEC H7,45,C'GOAL PTS'                                                
         SSPEC H7,55,C'GOAL DOL'                                                
         SSPEC H7,66,C'BUY PTS'                                                 
         SSPEC H7,76,C'BUY DOL'                                                 
         SSPEC H7,83,C'SPOTS'                                                   
         SSPEC H7,93,C'LOCKIN PTS'                                              
         SSPEC H7,106,C'LOCKIN DOL'                                             
         SSPEC H7,119,C'LOCKIN SPT'                                             
         SSPEC H8,1,C'-'                                                        
         SSPEC H8,4,C'---'                                                      
         SSPEC H8,9,C'---'                                                      
         SSPEC H8,14,C'---'                                                     
         SSPEC H8,19,C'------'                                                  
         SSPEC H8,26,C'------'                                                  
         SSPEC H8,35,C'-------'                                                 
         SSPEC H8,45,C'--------'                                                
         SSPEC H8,55,C'--------'                                                
         SSPEC H8,66,C'-------'                                                 
         SSPEC H8,76,C'-------'                                                 
         SSPEC H8,85,C'-----'                                                   
         SSPEC H8,93,C'----------'                                              
         SSPEC H8,106,C'----------'                                             
         SSPEC H8,119,C'----------'                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPCD01 12/11/01'                                      
         END                                                                    
