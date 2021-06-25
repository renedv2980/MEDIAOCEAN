*          DATA SET SPREPK201  AT LEVEL 029 AS OF 08/16/00                      
*PHASE SPK201A                                                                  
         TITLE 'SPK201 - SPOTPAK UNALLOCATION PROGRAM - SPECS'                  
SPK201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
*                                                                               
         SPROG 0,THRU,4                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
*                                                                               
         SPROG 5                                                                
         SSPEC H1,1,MEDIA                                                       
         SSPEC H5,1,CLIENT                                                      
         SSPEC H6,1,PRODUCT                                                     
*                                                                               
         SPROG 0,THRU,5                                                         
*                                                                               
         SSPEC H3,32,PERIOD                                                     
*                                                                               
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H5,77,PAGE                                                       
         SSPEC H5,87,REPORT                                                     
*                                                                               
         SPROG 0,THRU,4                                                         
         SSPEC H1,35,C'SPOTPAK UNALLOCATION REPORT'                             
         SSPEC H2,35,C'---------------------------'                             
*                                                                               
         SPROG 0,THRU,4                                                         
         SSPEC H8,27,C'UNALLOCATED'                                             
         SSPEC H9,1,C'MKT   STATION  EST-LIN  SPOTS    DOLLARS'                 
         SSPEC H10,1,C'---   -------  -------  ----------------'                
*                                                                               
         SPROG 1,THRU,4                                                         
         SSPEC H1,35,C'SPOTPAK RE-ALLOCATION REPORT'                            
         SSPEC H2,35,C'----------------------------'                            
*                                                                               
         SPROG 1,THRU,4                                                         
         SSPEC H8,42,C'--PRODUCT XXX --'                                        
         SSPEC H9,42,C'SPOTS    DOLLARS'                                        
         SSPEC H10,42,C'----------------'                                       
*                                                                               
         SPROG 2,THRU,4                                                         
         SSPEC H8,59,C'--PRODUCT XXX --'                                        
         SSPEC H9,59,C'SPOTS    DOLLARS'                                        
         SSPEC H10,59,C'----------------'                                       
*                                                                               
         SPROG 3,THRU,4                                                         
         SSPEC H8,76,C'--PRODUCT XXX --'                                        
         SSPEC H9,76,C'SPOTS    DOLLARS'                                        
         SSPEC H10,76,C'----------------'                                       
*                                                                               
         SPROG 4                                                                
         SSPEC H8,93,C'--PRODUCT XXX --'                                        
         SSPEC H9,93,C'SPOTS    DOLLARS'                                        
         SSPEC H10,93,C'----------------'                                       
*                                                                               
         SPROG 5                                                                
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
         DC    CL25'5003ARE-ALLOCATION 1 PRD'                                   
         DC    CL25'5301B............... WGT'                                   
         DC    CL25'5403ARE-ALLOCATION 2 PRD'                                   
         DC    CL25'5701B............... WGT'                                   
         DC    CL25'5803ARE-ALLOCATION 3 PRD'                                   
         DC    CL25'6101B............... WGT '                                  
         DC    CL25'6203ARE-ALLOCATION 4 PRD'                                   
         DC    CL25'6501B............... WGT '                                  
         DC    CL25'6601APRODUCT CHANGE RPT'                                    
         DC    X'00'                                                            
 END                                                                            
