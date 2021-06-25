*          DATA SET SPREPM501  AT LEVEL 017 AS OF 08/29/00                      
*PHASE SPM501A                                                                  
         TITLE 'SPREPM501-STANDARD INTERFACE TAPE SPECS'                        
         PRINT NOGEN                                                            
SPM501   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,STATION                                                      
         FSPEC GET,MARKET                                                       
         FSPEC OPEN,DEMFILES                                                    
         SPROG 1,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,55,C'SPOTPAK INTERFACE TAPE LISTING'                          
         SSPEC H2,55,30C'-'                                                     
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,51,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H6,1,PRODUCT                                                     
         SSPEC H8,1,ESTIMATE                                                    
         SSPEC H8,100,PAGE                                                      
         SSPEC H8,111,REPORT                                                    
         SPROG 1                                                                
         SSPEC H10,15,C'REC'                                                    
         SSPEC H10,1,C'MEDIA'                                                   
         SSPEC H11,1,C'  EST CLT PRD CDE'                                       
         SSPEC H12,1,C'----- --- --- ---'                                       
         SSPEC H10,36,C'SPOT'                                                   
         SSPEC H11,20,C'MKT STATION DPT  LEN  START   END  SPOTS'               
         SSPEC H12,20,C'--- ------- --- ---- ------ ------ -----'               
         SSPEC H11,65,C'DOLLARS'                                                
         SSPEC H12,65,C'-------'                                                
         SSPEC H10,76,20C'-'                                                    
         SSPEC H10,96,C'DEMOGRAPHICS'                                           
         SSPEC H10,108,20C'-'                                                   
         SSPEC H11,76,C'VALUE CODE'                                             
         SSPEC H12,76,C'----- ----'                                             
         SSPEC H11,90,C'VALUE CODE'                                             
         SSPEC H12,90,C'----- ----'                                             
         SSPEC H11,104,C'VALUE CODE'                                            
         SSPEC H12,104,C'----- ----'                                            
         SSPEC H11,118,C'VALUE CODE'                                            
         SSPEC H12,118,C'----- ----'                                            
         SPROG 2                                                                
         SSPEC H10,15,C'REC'                                                    
         SSPEC H10,42,C'START  START  DAYS'                                     
         SSPEC H10,63,C'START   END  NO OF   COST PER'                          
         SSPEC H11,1,C'MEDIA CLT PRD CDE  MKT STATION DPT  LEN  DATE'           
         SSPEC H12,1,C'----- --- --- ---  --- ------- ---  --- ------'          
         SSPEC H11,49,C' DOW  MTWTFSS  TIME  TIME  SPOTS'                       
         SSPEC H12,49,C'----- ------- ----- -----  -----'                       
         SSPEC H11,82,C'     SPOT'                                              
         SSPEC H12,82,C'-----------'                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPREPM501 08/29/00'                                      
         END                                                                    
