*          DATA SET PPREPS201  AT LEVEL 021 AS OF 06/01/95                      
*PHASE PPS201A,+0,NOAUTO                                                        
         TITLE 'PRINTPAK - MEDIA SCHEDULE REPORT - SPCES'                       
PPS201A  CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         FSPEC READ,BUYS                                                        
         FSPEC GET,DIVISIONS                                                    
         FSPEC GET,REGIONS                                                      
         FSPEC GET,DISTRICTS                                                    
         FSPEC GET,PUBLICATIONS                                                 
         SPACE 2                                                                
         SPROG 0,1,11,21                                                        
         PSPEC H1,1,MEDIA                                                       
         PSPEC H3,1,CLIENT                                                      
         PSPEC H4,1,DIVISION                                                    
         PSPEC H7,1,REQUESTOR                                                   
         PSPEC H3,55,PERIOD                                                     
*                                                                               
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,122,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
         SPROG 0,1,11,21                                                        
         PSPEC H5,1,PRODUCT                                                     
         SPROG 0,1,21                                                           
         PSPEC H6,1,ESTIMATE                                                    
*                                                                               
         SPROG 0                                                                
         PSPEC H1,56,C'SCHEDULE SPREADSHEET'                                    
         PSPEC H2,56,C'--------------------'                                    
*                                                                               
         SPROG 1,11,21                                                          
         PSPEC H1,60,C'ADCODE RECAP'                                            
         PSPEC H2,60,C'------------'                                            
         PSPEC H9,5,C'CLIENT   PRODUCT   ADCODE   COPY NUMBER'                  
         PSPEC H10,5,C'------   -------   ------   -----------'                 
         PSPEC H9,53,C'CAPTION'                                                 
         PSPEC H10,53,C'-------'                                                
         PSPEC H9,84,C'START        END'                                        
         PSPEC H10,84,C'-----        ---'                                       
         PSPEC H9,103,C'INSERTIONS'                                             
         PSPEC H10,103,C'----------'                                            
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021PPREPS201 06/01/95'                                      
         END                                                                    
