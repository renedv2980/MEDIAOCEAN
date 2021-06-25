*          DATA SET PPREPPE01  AT LEVEL 039 AS OF 07/18/16                      
*PHASE PPPE01A,+0                                                               
         TITLE 'PPPG01  CLT/PRD/EST     LISTING HEADLINES'                      
PPPE01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ESTIMATES                                                   
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,20,30                                                       
*                                                                               
         PSPEC H1,1,RUN                                                         
         PSPEC H2,1,REQUESTOR                                                   
*                                                                               
         PSPEC H1,95,AGYNAME                                                    
         PSPEC H2,95,AGYADD                                                     
*                                                                               
         PSPEC H4,95,REPORT                                                     
         PSPEC H4,120,PAGE                                                      
*                                                                               
         PSPEC H6,1,MEDIA                                                       
         PSPEC H7,1,CLIENT                                                      
*SPROG 0 -- OLD REPORT                                                          
         SPROG 0                                                                
         PSPEC H8,1,PRODUCT                                                     
*                                                                               
         PSPEC H11,18,C'START - END'                                            
         PSPEC H11,55,C'CHG'                                                    
         PSPEC H11,63,C'PG'                                                     
         PSPEC H11,67,C'PG'                                                     
         PSPEC H11,79,C'MULTI'                                                  
         PSPEC H11,85,C'NO'                                                     
         PSPEC H11,90,C'FISCAL'                                                 
         PSPEC H11,98,C'XFER'                                                   
*                                                                               
         PSPEC H12,11,C'EST'                                                    
         PSPEC H12,21,C'DATES'                                                  
         PSPEC H12,34,C'DESCRIPTION'                                            
         PSPEC H12,55,C'PER'                                                    
         PSPEC H12,59,C'ACC'                                                    
         PSPEC H12,63,C'BRD'                                                    
         PSPEC H12,67,C'EST'                                                    
         PSPEC H12,72,C'EVENT'                                                  
         PSPEC H12,80,C'BRD'                                                    
         PSPEC H12,85,C'BRD'                                                    
         PSPEC H12,89,C'YEAR END'                                               
         PSPEC H12,98,C'EST'                                                    
*                                                                               
*SPROG 10 -- ESTIMAT=0 REPORT: PRODUCT AND PG-BRAND DISPLAYED ONLY              
         SPROG 10                                                               
         PSPEC H3,45,C'PG ESTIMATE RECORDS'                                     
         PSPEC H8,1,C'ESTIMATE'                                                 
         PSPEC H8,10,C'000'                                                     
         PSPEC H12,11,C'PRODUCT'                                                
         PSPEC H12,19,C'PG BRAND'                                               
*                                                                               
*SPROG 20 -- PRODUCT='ZZZ': PG RECORD FIELDS                                    
         SPROG 20                                                               
         PSPEC H3,45,C'PG ESTIMATE RECORDS'                                     
         PSPEC H8,1,C'PRODUCT'                                                  
         PSPEC H8,10,C'ZZZ'                                                     
*                                                                               
         PSPEC H11,15,C'CHG'                                                    
         PSPEC H11,23,C'PG'                                                     
         PSPEC H11,35,C'MULTI'                                                  
         PSPEC H11,41,C'NO'                                                     
         PSPEC H11,46,C'FISCAL'                                                 
         PSPEC H11,54,C'XFER'                                                   
*                                                                               
         PSPEC H12,11,C'EST'                                                    
         PSPEC H12,15,C'PER'                                                    
         PSPEC H12,19,C'ACC'                                                    
         PSPEC H12,23,C'EST'                                                    
         PSPEC H12,28,C'EVENT'                                                  
         PSPEC H12,36,C'BRD'                                                    
         PSPEC H12,41,C'BRD'                                                    
         PSPEC H12,45,C'YEAR END'                                               
         PSPEC H12,54,C'EST'                                                    
*                                                                               
*SPROG 30 -- HEADER ESTIMATE RECORDS FOR 'ZZZ' EST NOT FOUND                    
*                                                                               
         SPROG 30                                                               
         PSPEC H8,1,PRODUCT                                                     
         PSPEC H3,45,C'ESTIMATES WITH MISSING PG EST RECORDS'                   
*                                                                               
         PSPEC H11,18,C'START - END'                                            
         PSPEC H12,11,C'EST'                                                    
         PSPEC H12,21,C'DATES'                                                  
         PSPEC H12,34,C'DESCRIPTION'                                            
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039PPREPPE01 07/18/16'                                      
         END                                                                    
