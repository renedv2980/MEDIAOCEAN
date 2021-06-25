*          DATA SET SPREP0701  AT LEVEL 021 AS OF 07/17/07                      
*PHASE SP0701A                                                                  
         TITLE 'SP0701 - SPOT UNBILLING'                                        
SP0701   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
*                                                                               
         SPROG 1,2                                                              
*                                                                               
         SSPEC H1,57,C'SPOTPAK UNBILLING'                                       
         SSPEC H2,57,C'-----------------'                                       
*                                                                               
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,98,REPORT                                                     
         SSPEC H5,98,PAGE                                                       
*                                                                               
         SSPEC H2,1,MEDIA                                                       
*                                                                               
         SPROG 1                                                                
*                                                                               
         SSPEC  H9,1,C'INVOICE    PRD EST PERIOD MKT  STATION'                  
         SSPEC H10,1,C'-------    --- --- ------ ---  -------'                  
         SSPEC  H9,68,C'           GROSS             NET'                       
         SSPEC H10,68,C'           -----             ---'                       
*                                                                               
         SSPEC  H8,100,C'        --------- COS2 ---------'                      
         SSPEC  H9,100,C'           GROSS             NET'                      
         SSPEC H10,100,C'           -----             ---'                      
*                                                                               
         SPROG 2                                                                
*                                                                               
         SSPEC  H9,68,C'           GROSS             NET'                       
         SSPEC H10,68,C'           -----             ---'                       
*                                                                               
         SSPEC  H8,100,C'        --------- COS2 ---------'                      
         SSPEC  H9,100,C'           GROSS             NET'                      
         SSPEC H10,100,C'           -----             ---'                      
*                                                                               
         DC    X'00'                                                            
         SPACE 2                                                                
         DC    C'REQLST='                                                       
         DC    CL25'5004AINVOICE NUMBER'                                        
         DC    CL25'5404AMONTH OF SERVICE'                                      
         DC    CL25'6201AOPTION 1'                                              
         DC    X'00'                                                            
*                                                                               
         DC    C'REQ2LST='                                                      
         DC    CL25'2112AGROSS'                                                 
         DC    CL25'3312ADETAILS'                                               
         DC    X'00'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPREP0701 07/17/07'                                      
         END                                                                    
