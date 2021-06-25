*          DATA SET SPREP7U01  AT LEVEL 017 AS OF 07/17/07                      
*PHASE SP7U01A                                                                  
         TITLE 'SP7U01 - NETWORK UNIT UNBILLING'                                
SP7U01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
*                                                                               
         SPROG 1,2                                                              
*                                                                               
         SSPEC H1,57,C'NETPAK UNBILLING'                                        
         SSPEC H2,57,C'----------------'                                        
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
         SSPEC H8,1,C'INVOICE    PRD EST MOS    NETW PKG PROGRM DATE   +        
                    T LOCAL MKST                            GROSS      +        
                      NET          GROSS2'                                      
         SSPEC H9,1,C'-------    --- --- ---    ---- --- ------ ----   +        
                    - ----------                            -----      +        
                      ---          ------'                                      
*                                                                               
         SPROG 2                                                                
*                                                                               
         SSPEC H8,95,C'GROSS             NET          GROSS2'                   
         SSPEC H9,95,C'-----             ---          ------'                   
*                                                                               
         DC    X'00'                                                            
         SPACE 2                                                                
         DC    C'REQLST='                                                       
         DC    CL25'5004AINVOICE NUMBER'                                        
         DC    CL25'5404AMONTH OF SERVICE'                                      
         DC    CL25'6201AOPTION 1'                                              
         DC    X'00'                                                            
                                                                                
         DC    C'REQ2LST='                                                      
         DC    CL25'2112AGROSS'                                                 
         DC    CL25'3312ADETAILS'                                               
         DC    CL25'4501ADETAIL TYPE'                                           
         DC    X'00'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPREP7U01 07/17/07'                                      
         END                                                                    
