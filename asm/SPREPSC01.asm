*          DATA SET SPREPSC01  AT LEVEL 018 AS OF 02/18/21                      
*PHASE SPSC01A,*,NOAUTO                                                         
         TITLE 'STATION CALL-LETTER CHANGE PROGRAM - PRINT SPECS'               
SPSC01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SPSC03                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,TRAFFIC                                                   
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
                                                                                
         SPROG 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22         
         SSPEC H1,3,MEDIA                                                       
         SSPEC H1,46,C'STATION CALL-LETTER CHANGE'                              
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,3,REQUESTOR                                                   
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,53,C'MARKET'                                                  
         SSPEC H4,3,PAGE                                                        
         SSPEC H4,46,C'OLD CALL LETTERS -'                                      
         SSPEC H4,98,REPORT                                                     
         SSPEC H5,46,C'NEW CALL LETTERS -'                                      
                                                                                
         SPROG 1                                                                
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H8,11,C'------'                                                  
         SSPEC H4,15,C'BUYS'                                                    
         SSPEC H7,46,C'PRODUCT'                                                 
         SSPEC H7,63,C'ESTIMATE'                                                
         SSPEC H7,81,C'OLD LINE NUMBER'                                         
         SSPEC H7,106,C'NEW LINE NUMBER'                                        
         SSPEC H8,46,C'-------'                                                 
         SSPEC H8,63,C'--------'                                                
         SSPEC H8,81,C'--- ---- ------'                                         
         SSPEC H8,106,C'--- ---- ------'                                        
                                                                                
         SPROG 2                                                                
         SSPEC H4,15,C'INVOICES'                                                
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H8,11,C'------'                                                  
         SSPEC H7,21,C'DATE'                                                    
         SSPEC H8,21,C'----'                                                    
         SSPEC H7,35,C'SEQ. NO.'                                                
         SSPEC H8,35,C'---- ---'                                                
                                                                                
         SPROG 3                                                                
         SSPEC H4,15,C'AUTHORIZATIONS'                                          
                                                                                
         SPROG 4                                                                
         SSPEC H4,15,C'DARE ORDERS'                                             
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H8,11,C'------'                                                  
         SSPEC H7,18,C'MKT  STA   EST    ORDER     ORD-TYPE '                   
         SSPEC H8,18,C'---- ----- ---    --------  ---------'                   
                                                                                
         SPROG 5                                                                
         SSPEC H4,15,C'SID / NSID RECORDS'                                      
                                                                                
         SPROG 6                                                                
         SSPEC H4,15,C'DESTINE RECORDS'                                         
         SSPEC H7,11,C'CLIENT    STA  '                                         
         SSPEC H8,11,C'------    -----'                                         
                                                                                
         SPROG 7                                                                
         SSPEC H4,15,C'LAST METHOD RECORDS'                                     
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H8,11,C'------'                                                  
         SSPEC H7,18,C'BUYER'                                                   
         SSPEC H8,18,C'-----'                                                   
                                                                                
         SPROG 8                                                                
         SSPEC H4,15,C'DARE BATCH'                                              
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H8,11,C'------'                                                  
         SSPEC H7,18,C'MKT  STA   EST'                                          
         SSPEC H8,18,C'---- ----- ---'                                          
                                                                                
         SPROG 9                                                                
         SSPEC H4,15,C'WILA PROFIT WITHIN'                                      
                                                                                
         SPROG 10                                                               
         SSPEC H4,15,C'STATION LOCK-IN'                                         
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H8,11,C'------'                                                  
         SSPEC H7,18,C'MKT  STA   EST DPT LEN LN2'                              
         SSPEC H8,18,C'---- ----- --- --- --- ---'                              
                                                                                
         SPROG 11                                                               
         SSPEC H4,15,C'DOUBLE BOOKING'                                          
                                                                                
         SPROG 12                                                               
         SSPEC H4,15,C'TRAFFIC INST RECAP'                                      
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H8,11,C'------'                                                  
         SSPEC H7,18,C'MKT  STA   COPY DPT'                                     
         SSPEC H8,18,C'---- ----- ---- ---'                                     
                                                                                
         SPROG 13                                                               
         SSPEC H4,15,C'TRAFFIC SHIP RECAP'                                      
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H8,11,C'------'                                                  
         SSPEC H7,18,C'MKT  STA     CMML SEQ'                                   
         SSPEC H8,18,C'---- -----   --------'                                   
                                                                                
         SPROG 14                                                               
         SSPEC H4,15,C'TRAFFIC BUY ACTIVITY'                                    
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H8,11,C'------'                                                  
         SSPEC H7,18,C'MKT  STA   EST'                                          
         SSPEC H8,18,C'---- ----- ---'                                          
                                                                                
         SPROG 15                                                               
         SSPEC H4,15,C'TRAFFIC BUY RECORDS'                                     
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H8,11,C'------'                                                  
         SSPEC H7,18,C'MKT  STA  '                                              
         SSPEC H8,18,C'---- -----'                                              
                                                                                
         SPROG 16                                                               
         SSPEC H4,15,C'TRAFFIC RECORDS'                                         
                                                                                
         SPROG 20                                                               
         SSPEC H4,15,C'DARE ORDER HISTORY'                                      
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H8,11,C'------'                                                  
         SSPEC H7,18,C'MKT  STA   EST ORDER    OLD LINE NEW LINE'               
         SSPEC H8,18,C'---- ----- --- -------- -------- --------'               
                                                                                
         SPROG 21                                                               
         SSPEC H4,15,C'CANADIAN DTM RECORDS'                                    
*                                                                               
         SPROG 22                                                               
         SSPEC H4,15,C'US SBTK RECORDS'                                         
         SSPEC H7,11,C'CLIENT'                                                  
         SSPEC H8,11,C'------'                                                  
         SSPEC H7,18,C'PRD   EST   MARKET  OLD LINE NEW LINE'                   
         SSPEC H8,18,C'---   ---   ------  -------- --------'                   
         SSPEC H7,57,C'SHEET NAME/--LINE PROGRAM'                               
         SSPEC H8,57,C'-------------------------'                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018SPREPSC01 02/18/21'                                      
         END                                                                    
