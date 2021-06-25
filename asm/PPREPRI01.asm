*          DATA SET PPREPRI01  AT LEVEL 020 AS OF 03/01/96                      
*PHASE PPRI01A,+0                                                               
         TITLE 'PP0701 - PRINTPAK I/O REVERSAL'                                 
PPRI01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC READ,ACTIVE                                                      
         FSPEC UPDATE,PRTFILE                                                   
         SPROG 0,5,10,15,20,30                                                  
         PSPEC H1,41,CL24'PRINTPAK I/O REVERSAL'                                
         PSPEC H2,41,C'---------------------'                                   
         PSPEC H1,2,MEDIA                                                       
         PSPEC H1,95,AGYNAME                                                    
         PSPEC H2,95,AGYADD                                                     
         PSPEC H4,95,REPORT                                                     
         PSPEC H4,116,PAGE                                                      
         PSPEC H5,95,RUN                                                        
         PSPEC H9,10,C'CLT'                                                     
         PSPEC H10,10,C'---'                                                    
         PSPEC H9,15,C'PRD'                                                     
         PSPEC H10,15,C'---'                                                    
         PSPEC H9,20,C'EST'                                                     
         PSPEC H10,20,C'---'                                                    
         PSPEC H9,25,C'DATE'                                                    
         PSPEC H10,25,C'----'                                                   
         PSPEC H9,35,C'PUB'                                                     
         PSPEC H10,35,C'---'                                                    
         PSPEC H9,62,C'I/O NO.'                                                 
         PSPEC H10,62,C'-------'                                                
         PSPEC H9,70,C'TYPE'                                                    
         PSPEC H10,70,C'----'                                                   
         PSPEC H9,75,C'I/O DATE'                                                
         PSPEC H10,75,C'--------'                                               
         PSPEC H9,85,C'SOURCE'                                                  
         PSPEC H10,85,C'------'                                                 
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'01020304050607080C0D131400'                                    
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020PPREPRI01 03/01/96'                                      
         END                                                                    
