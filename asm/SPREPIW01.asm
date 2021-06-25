*          DATA SET SPREPIW01  AT LEVEL 010 AS OF 02/16/05                      
*PHASE SPIW01A                                                                  
         TITLE 'SPIW01 - AUTOPAY WORKER FILE ERRORS'                            
SPIW01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SPROG 1,2                                                              
*                                                                               
         SSPEC H1,38,C'AUTOPAY ERROR REPORT'                                    
         SSPEC H2,38,C'--------------------'                                    
*                                                                               
         SSPEC H2,2,C'CLIENT='                                                  
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         SPROG 1                                                                
*                                                                               
         SSPEC H7,10,C'MED'                                                     
         SSPEC H8,10,C'---'                                                     
         SSPEC H7,16,C'CLT'                                                     
         SSPEC H8,16,C'---'                                                     
         SSPEC H7,22,C'PRD'                                                     
         SSPEC H8,22,C'---'                                                     
         SSPEC H7,28,C'PTN'                                                     
         SSPEC H8,28,C'---'                                                     
         SSPEC H7,34,C'EST'                                                     
         SSPEC H8,34,C'---'                                                     
         SSPEC H7,40,C'STATION'                                                 
         SSPEC H8,40,C'-------'                                                 
         SSPEC H7,52,C'SREP'                                                    
         SSPEC H8,52,C'----'                                                    
         SSPEC H7,58,C'MONTH'                                                   
         SSPEC H8,58,C'------'                                                  
         SSPEC H7,66,C'INVOICE NUM'                                             
         SSPEC H8,66,C'------------'                                            
         SSPEC H7,80,C'ERROR'                                                   
         SSPEC H8,80,C'-------------------------------'                         
*                                                                               
         SPROG 2                                                                
*                                                                               
         SSPEC H7,2,C'MED'                                                      
         SSPEC H8,2,C'---'                                                      
         SSPEC H7,6,C'CLT'                                                      
         SSPEC H8,6,C'---'                                                      
         SSPEC H7,12,C'PRD'                                                     
         SSPEC H8,12,C'---'                                                     
         SSPEC H7,18,C'PTN'                                                     
         SSPEC H8,18,C'---'                                                     
         SSPEC H7,24,C'PAK'                                                     
         SSPEC H8,24,C'---'                                                     
         SSPEC H7,30,C'EST'                                                     
         SSPEC H8,30,C'---'                                                     
         SSPEC H7,36,C'STAT'                                                    
         SSPEC H8,36,C'----'                                                    
         SSPEC H7,42,C'SREP'                                                    
         SSPEC H8,42,C'----'                                                    
         SSPEC H7,48,C' MONTH'                                                  
         SSPEC H8,48,C'--------'                                                
         SSPEC H7,58,C'INVOICE'                                                 
         SSPEC H8,58,C'----------'                                              
         SSPEC H7,70,C'OPTIONS/ERRORS'                                          
         SSPEC H8,70,C'-------------------------------'                         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
 END                                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREPIW01 02/16/05'                                      
         END                                                                    
