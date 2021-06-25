*          DATA SET SPREPCF01  AT LEVEL 003 AS OF 08/29/00                      
*PHASE SPCF01A                                                                  
         TITLE 'SPCF01 - WORKER FILE BUYS IN ERROR'                             
SPCF01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
*                                                                               
         SSPEC H1,36,C'WORKER FILE BUYS IN ERROR'                               
         SSPEC H2,36,C'-------------------------'                               
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         SSPEC H7,1,C'SEQ NO'                                                   
         SSPEC H7,10,C'MED'                                                     
         SSPEC H7,16,C'CLT'                                                     
         SSPEC H7,22,C'PRD'                                                     
         SSPEC H7,28,C'EST'                                                     
         SSPEC H7,34,C'STATION'                                                 
         SSPEC H7,44,C'PERIOD'                                                  
         SSPEC H7,64,C'ROTATION'                                                
         SSPEC H7,74,C'TIME'                                                    
         SSPEC H7,88,C'DPART'                                                   
         SSPEC H7,96,C'LEN'                                                     
         SSPEC H7,101,C'PROGRAM'                                                
         SSPEC H7,129,C'COST'                                                   
         SSPEC H8,1,C'------'                                                   
         SSPEC H8,10,C'---'                                                     
         SSPEC H8,16,C'---'                                                     
         SSPEC H8,22,C'---'                                                     
         SSPEC H8,28,C'---'                                                     
         SSPEC H8,34,C'-------'                                                 
         SSPEC H8,44,C'------'                                                  
         SSPEC H8,64,C'--------'                                                
         SSPEC H8,74,C'----'                                                    
         SSPEC H8,88,C'-----'                                                   
         SSPEC H8,96,C'---'                                                     
         SSPEC H8,101,C'-------'                                                
         SSPEC H8,129,C'----'                                                   
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
 END                                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPCF01 08/29/00'                                      
         END                                                                    
