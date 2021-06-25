*          DATA SET SPREPSX01  AT LEVEL 015 AS OF 11/16/06                      
*PHASE SPSX01T,*,NOAUTO                                                         
         TITLE 'MARKET FIX PROGRAM - PRINT SPECS'                               
SPSX01   CSECT                                                                  
**NOP    PRINT NOGEN                                                            
         FSPEC USE,SPSX03                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,STATION                                                   
         FSPEC UPDATE,TRAFFIC                                                   
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         SSPEC H1,3,MEDIA                                                       
         SSPEC H1,50,C'SPOTPAK MARKET FIX'                                      
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,3,REQUESTOR                                                   
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,44,C'STATION'                                                 
         SSPEC H3,60,C'HAS BEEN MOVED'                                          
         SSPEC H4,3,PAGE                                                        
         SSPEC H4,43,C'FROM -'                                                  
         SSPEC H4,55,C'-'                                                       
         SSPEC H4,98,REPORT                                                     
         SSPEC H5,43,C'TO   -'                                                  
         SSPEC H5,55,C'-'                                                       
         SPROG 0                                                                
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H7,46,C'PRODUCT'                                                 
         SSPEC H7,63,C'ESTIMATE'                                                
         SSPEC H7,81,C'OLD LINE NUMBER'                                         
         SSPEC H7,106,C'NEW LINE NUMBER'                                        
         SSPEC H8,11,C'------'                                                  
         SSPEC H8,46,C'-------'                                                 
         SSPEC H8,63,C'--------'                                                
         SSPEC H8,81,C'--- ---- ------'                                         
         SSPEC H8,106,C'--- ---- ------'                                        
         SPROG 1                                                                
         SSPEC H7,1,C'CLIENT'                                                   
         SSPEC H7,10,C'PROD'                                                    
         SSPEC H7,17,C'EST'                                                     
         SSPEC H7,24,C'ELEM'                                                    
         SSPEC H7,30,C'BILL'                                                    
         SSPEC H7,39,C'BILLDATE'                                                
         SSPEC H7,50,C'INVNO'                                                   
         SSPEC H7,61,C'GROSS AMT'                                               
         SSPEC H7,79,C'NET AMT'                                                 
         SSPEC H7,91,C'SPOTS'                                                   
         SSPEC H7,99,C'TAX AMT'                                                 
         SSPEC H8,1,C'------'                                                   
         SSPEC H8,10,C'---'                                                     
         SSPEC H8,17,C'----'                                                    
         SSPEC H8,24,C'----'                                                    
         SSPEC H8,30,C'PERIOD'                                                  
         SSPEC H8,39,C'--------'                                                
         SSPEC H8,50,C'-----'                                                   
         SSPEC H8,61,C'---------'                                               
         SSPEC H8,79,C'-------'                                                 
         SSPEC H8,91,C'-----'                                                   
         SSPEC H8,99,C'-------'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREPSX01 11/16/06'                                      
         END                                                                    
