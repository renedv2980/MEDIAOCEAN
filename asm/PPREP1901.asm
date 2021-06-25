*          DATA SET PPREP1901  AT LEVEL 011 AS OF 05/04/98                      
*PHASE PP1901A,+0                                                               
         TITLE 'PP1901 -  PRINTPAK CONTRACT UTILIZATION'                        
PP1901   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BUYS                                                        
*                                                                               
         SPROG 0,1,2,3,6,7,8,9,10                                               
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
*                                                                               
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,98,AGYADD                                                     
*                                                                               
         PSPEC H3,55,PERIOD                                                     
         PSPEC H3,1,CLIENT                                                      
*                                                                               
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,119,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
         SPROG 0,1,2,3                                                          
         PSPEC H1,57,C'UTILIZATION REPORT'                                      
         PSPEC H2,57,C'------------------'                                      
         SPROG 6,7,9                                                            
         PSPEC H1,58,C'CONTRACT ANALYSIS'                                       
         PSPEC H2,58,C'-----------------'                                       
         SPROG 1,6                                                              
         PSPEC H7,31,C'CONTRACT'                                                
         PSPEC H7,114,C'RATE'                                                   
*                                                                               
         PSPEC H8,31,C' NUMBER     CONTRACT DATES      LEVEL      PERCEX        
               NT      RATE    DESCRIPTION        EFF. DATE'                    
*                                                                               
         PSPEC H9,31,C'--------  -----------------    --------    -----X        
               --    --------  -----------        ---------'                    
*                                                                               
         SPROG 2,3                                                              
         PSPEC H11,41,C'EST'                                                    
         PSPEC H12,41,C'---'                                                    
         SPROG 2                                                                
         PSPEC H11,46,C'SPACE          UNITS'                                   
         PSPEC H12,46,C'-----          -----'                                   
         PSPEC H10,68,C'  UNIT  '                                               
         PSPEC H11,67,C'RATE/PREM'                                              
         PSPEC H12,67,C'---------'                                              
         SPROG 3                                                                
         PSPEC H11,46,C'DESCRIPTION'                                            
         PSPEC H12,46,C'-----------'                                            
         SPROG 7                                                                
         PSPEC H5,55,C'** INSERTION SUMMARY **'                                 
         PSPEC H6,61,C'(CONTINUED)'                                             
         SPROG 8                                                                
         PSPEC H5,56,C'** POSTING SUMMARY **'                                   
         PSPEC H6,62,C'(CONTINUED)'                                             
         SPROG 9                                                                
         PSPEC H5,55,C'** INSERTION SUMMARY **'                                 
         SPROG 10                                                               
         PSPEC H5,56,C'** POSTING SUMMARY **'                                   
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'01020417090A0B0C0D0F131400'                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011PPREP1901 05/04/98'                                      
         END                                                                    
