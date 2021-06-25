*          DATA SET SPREPK101  AT LEVEL 008 AS OF 02/10/04                      
*PHASE SPK101B                                                                  
         TITLE 'SPK101 - SPOTPAK UNALLOCATION PROGRAM - SPECS'                  
SPK101   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
*                                                                               
         SPROG 0,THRU,5                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
*                                                                               
         SPROG 6                                                                
         SSPEC H1,1,MEDIA                                                       
         SSPEC H5,1,CLIENT                                                      
         SSPEC H6,1,PRODUCT                                                     
*                                                                               
         SPROG 0,THRU,6                                                         
*                                                                               
         SSPEC H3,32,PERIOD                                                     
*                                                                               
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H5,77,PAGE                                                       
         SSPEC H5,87,REPORT                                                     
*                                                                               
         SPROG 0,THRU,5                                                         
         SSPEC H1,35,C'SPOTPAK UNALLOCATION REPORT'                             
         SSPEC H2,35,C'---------------------------'                             
*                                                                               
         SPROG 0,THRU,5                                                         
         SSPEC H8,27,C'UNALLOCATED'                                             
         SSPEC H9,1,C'MKT   STATION  EST-LIN  SPOTS    DOLLARS'                 
         SSPEC H10,1,C'---   -------  -------  ----------------'                
*                                                                               
         SPROG 1,THRU,5                                                         
         SSPEC H1,35,C'SPOTPAK RE-ALLOCATION REPORT'                            
         SSPEC H2,35,C'----------------------------'                            
*                                                                               
         SPROG 1,THRU,5                                                         
         SSPEC H9,42,C'SPOTS    DOLLARS'                                        
         SSPEC H10,42,C'----------------'                                       
*                                                                               
         SPROG 2,THRU,5                                                         
         SSPEC H9,59,C'SPOTS    DOLLARS'                                        
         SSPEC H10,59,C'----------------'                                       
*                                                                               
         SPROG 3,THRU,5                                                         
         SSPEC H9,76,C'SPOTS    DOLLARS'                                        
         SSPEC H10,76,C'----------------'                                       
*                                                                               
         SPROG 4,THRU,5                                                         
         SSPEC H9,93,C'SPOTS    DOLLARS'                                        
         SSPEC H10,93,C'----------------'                                       
*                                                                               
         SPROG 5                                                                
         SSPEC H9,110,C'SPOTS    DOLLARS'                                       
         SSPEC H10,110,C'----------------'                                      
*                                                                               
         SPROG 6                                                                
         SSPEC H1,37,C'PRODUCT CHANGE REPORT'                                   
         SSPEC H2,37,C'---------------------'                                   
*                                                                               
         SSPEC H10,1,C'ESTIMATE'                                                
         SSPEC H11,1,C'--------'                                                
         SSPEC H10,26,C'DAYS     TIMES       PROGRAMMING'                       
         SSPEC H11,26,C'----     -----       -----------'                       
         SSPEC H10,66,C'LEN  WEEK   NEW PRODUCT ALLOC'                          
         SSPEC H11,66,C'---  ----   -----------------'                          
         SSPEC H10,103,C'SPTS  COST/SPOT'                                       
         SSPEC H11,103,C'----  ---------'                                       
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
         DC    C'REQLST='                                                       
         DC    CL25'5003ASECOND PRODUCT'                                        
         DC    CL25'5303ASPOT LENGTH'                                           
         DC    CL25'5603ASECOND SPOT LENGTH'                                    
         DC    CL25'6301APRESERVE CUTINS'                                       
         DC    CL25'6501AINCLUDE PIGGYBACKS'                                    
         DC    CL25'6601APRODUCT CHANGE RPT'                                    
         DC    X'00'                                                            
         DC    C'REQ2LST='                                                      
         DC    CL25'2103ARE-ALLOCATION 1 PRD'                                   
         DC    CL25'2403A............... PRD2'                                  
         DC    CL25'2703A............... LEN'                                   
         DC    CL25'3003A............... LEN2'                                  
         DC    CL25'3301B............... WGT'                                   
         DC    CL25'3403ARE-ALLOCATION 2 PRD'                                   
         DC    CL25'3703A............... PRD2'                                  
         DC    CL25'4003A............... LEN'                                   
         DC    CL25'4303A............... LEN2'                                  
         DC    CL25'4601B............... WGT'                                   
         DC    CL25'4703ARE-ALLOCATION 3 PRD'                                   
         DC    CL25'5003A............... PRD2'                                  
         DC    CL25'5303A............... LEN'                                   
         DC    CL25'5603A............... LEN2'                                  
         DC    CL25'5901B............... WGT'                                   
         DC    CL25'6003ARE-ALLOCATION 4 PRD'                                   
         DC    CL25'6303A............... PRD2'                                  
         DC    CL25'6603A............... LEN'                                   
         DC    CL25'6903A............... LEN2'                                  
         DC    CL25'7201B............... WGT'                                   
         DC    CL25'7303ARE-ALLOCATION 5 PRD'                                   
         DC    CL25'7603A............... PRD2'                                  
         DC    CL25'7901B............... WGT'                                   
         DC    X'00'                                                            
 END                                                                            
