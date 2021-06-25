*          DATA SET SPREPFX01B AT LEVEL 007 AS OF 02/04/93                      
*PHASE SPFX01B,+0                                                               
         TITLE 'SPFX01 - SPECS FOR BACKER TRAFFIC BUY ACT RECORD FIX'           
SPFX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,55,C'YET ANOTHER FILE FIX'                                    
         SSPEC H2,55,C'--------------------'                                    
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H4,2,C'AGY'                                                      
         SSPEC H4,6,C'M'                                                        
         SSPEC H4,8,C'CLT'                                                      
         SSPEC H4,12,C'MRKT'                                                    
         SSPEC H4,17,C'STATN'                                                   
         SSPEC H4,24,C'PRD'                                                     
         SSPEC H4,28,C'PTR'                                                     
         SSPEC H4,32,C'EST'                                                     
         SSPEC H4,36,C'LN1'                                                     
         SSPEC H4,40,C'LN2'                                                     
         SSPEC H4,44,C'ADD DATE'                                                
         SSPEC H4,53,C'CHG DATE'                                                
         SSPEC H4,62,C'DISK ADR'                                                
         SSPEC H4,71,C'A'                                                       
*****    FSPEC OPEN,DEMFILES                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPFX01B02/04/93'                                      
         END                                                                    
