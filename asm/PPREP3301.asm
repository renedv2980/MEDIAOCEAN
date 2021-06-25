*          DATA SET PPREP3301  AT LEVEL 031 AS OF 08/09/00                      
*PHASE PP3301A                                                                  
         TITLE 'PP3301  PUB USAGE REPORT SPECS'                                 
PP3301   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BUYS                                                        
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
*                                                                               
         PSPEC H9,3,C'CLIENT'                                                   
         PSPEC H10,3,6C'-'                                                      
*                                                                               
         SPROG 0,1,2                                                            
         SPACE                                                                  
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,48,C'PUB USAGE REPORT'                                        
         PSPEC H2,48,16C'-'                                                     
         PSPEC H3,44,PERIOD                                                     
*                                                                               
         PSPEC H1,76,AGYNAME                                                    
         PSPEC H2,76,AGYADD                                                     
         PSPEC H5,76,RUN                                                        
         PSPEC H6,76,REPORT                                                     
         PSPEC H6,101,PAGE                                                      
*                                                                               
         SPROG 0,1                                                              
         SPACE                                                                  
         PSPEC H9,30,C'PUB CODE'                                                
         PSPEC H10,30,8C'-'                                                     
         PSPEC H9,46,C'PUB NAME'                                                
         PSPEC H10,46,8C'-'                                                     
         PSPEC H9,72,C'TOTAL GROSS'                                             
         PSPEC H10,72,11C'-'                                                    
         PSPEC H8,88,C'NO. OF'                                                  
         PSPEC H9,88,C'INSERTS'                                                 
         PSPEC H10,88,7C'-'                                                     
*                                                                               
         SPROG 1                                                                
         SPACE                                                                  
         PSPEC H9,3,C'** AGENCY TOTALS **'                                      
         PSPEC H8,97,C'NO. OF'                                                  
         PSPEC H9,97,C'CLIENTS'                                                 
         PSPEC H10,97,7C'-'                                                     
*                                                                               
         SPROG 2                                                                
         SPACE                                                                  
         PSPEC H9,8,C'INSERTS'                                                  
         PSPEC H10,8,7C'-'                                                      
         PSPEC H9,24,C'PUBS'                                                    
         PSPEC H10,24,4C'-'                                                     
         PSPEC H9,30,C'PCT'                                                     
         PSPEC H10,30,3C'-'                                                     
         PSPEC H9,36,C'CUME'                                                    
         PSPEC H10,36,4C'-'                                                     
         PSPEC H9,42,C'PCT'                                                     
         PSPEC H10,42,3C'-'                                                     
         PSPEC H9,52,C'INS'                                                     
         PSPEC H10,52,3C'-'                                                     
         PSPEC H9,57,C'PCT'                                                     
         PSPEC H10,57,3C'-'                                                     
         PSPEC H9,63,C'CUME'                                                    
         PSPEC H10,63,4C'-'                                                     
         PSPEC H9,69,C'PCT'                                                     
         PSPEC H10,69,3C'-'                                                     
         PSPEC H9,83,C'GROSS'                                                   
         PSPEC H10,83,5C'-'                                                     
         PSPEC H9,91,C'PCT'                                                     
         PSPEC H10,91,3C'-'                                                     
         PSPEC H9,105,C'CUME'                                                   
         PSPEC H10,105,4C'-'                                                    
         PSPEC H9,112,C'PCT'                                                    
         PSPEC H10,112,3C'-'                                                    
*                                                                               
         DC    X'00'                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031PPREP3301 08/09/00'                                      
         END                                                                    
