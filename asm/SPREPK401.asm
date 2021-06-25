*          DATA SET SPREPK401  AT LEVEL 002 AS OF 08/29/00                      
*PHASE SPK401A                                                                  
SPK401   TITLE '- NEW SPOT ALLOCATION PROGRAM - SPEC PHASE'                     
SPK401   CSECT                                                                  
         PRINT NOGEN                                                            
                                                                                
         FSPEC USE,SPK403                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKETS                                                      
                                                                                
         SPROG 10                  RECAP REPORT                                 
                                                                                
         SSPEC H1,49,C'Brand allocation - Summary report'                       
         SSPEC H2,49,C'---------------------------------'                       
                                                                                
         SPROG 20                  WEEKLY ANALYSIS                              
                                                                                
         SSPEC H1,48,C'Brand allocation - Weekly analysis'                      
         SSPEC H2,48,C'----------------------------------'                      
                                                                                
         SPROG 30,35               DETAIL REPORT                                
                                                                                
         SSPEC H1,49,C'Brand allocation - Detail listing'                       
         SSPEC H2,49,C'---------------------------------'                       
                                                                                
         SPROG 40                  PARAM REPORT HEADS                           
                                                                                
         SSPEC H1,54,C'Brand allocation report'                                 
         SSPEC H2,54,C'-----------------------'                                 
                                                                                
         SPROG 80                  CHILD SPOT                                   
                                                                                
         SSPEC H1,47,C'Brand allocation - Child spot report'                    
         SSPEC H2,47,C'------------------------------------'                    
                                                                                
         SPROG 70                  POD CHANGE ANALYSIS                          
                                                                                
         SSPEC H1,52,C'POD change analysis report'                              
         SSPEC H2,52,C'--------------------------'                              
                                                                                
         SPROG 50                  GOAL REPORT                                  
                                                                                
         SSPEC H1,45,C'Brand allocation - Goal balancing report'                
         SSPEC H2,45,C'----------------------------------------'                
                                                                                
         SPROG 37                  PROGRAM/PRODUCT RECAP                        
                                                                                
         SSPEC H1,47,C'Brand allocation - Program/Brand recap'                  
         SSPEC H2,47,C'--------------------------------------'                  
                                                                                
         SPROG 38                  PROGRAM/PRODUCT INDEX RECAP                  
                                                                                
         SSPEC H1,44,C'Brand allocation - Program/Brand index recap'            
         SSPEC H2,44,C'--------------------------------------------'            
                                                                                
         SPROG 39                  BRAND RATING POINT RECAP                     
                                                                                
         SSPEC H1,44,C'Brand allocation - Brand rating point recap'             
         SSPEC H2,44,C'-------------------------------------------'             
                                                                                
         SPROG 0,10,20,30,35,37,38,39,40,50,70,80                               
                                                                                
         SSPEC H3,50,PERIOD                                                     
                                                                                
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,98,REPORT                                                     
         SSPEC H5,98,PAGE                                                       
                                                                                
         SSPEC H1,1,MEDIA                                                       
         SSPEC H3,1,CLIENT                                                      
         SSPEC H4,1,PRODUCT                                                     
         SSPEC H5,1,ESTIMATE                                                    
         SSPEC H6,1,MARKET                                                      
                                                                                
         DC    X'00'                                                            
                                                                                
         DC    C'REQLST='                                                       
                                                                                
         DC    CL25'5901ADaypart filter'                                        
         DC    CL25'6002ASpot length filter'                                    
         DC    CL25'6201AReallocate?'                                           
         DC    CL25'6301ABasis- Demo or $'                                      
         DC    CL25'6401AAllocate excess?'                                      
         DC    CL25'6501ATest only'                                             
         DC    CL25'6701AZero week option'                                      
         DC    X'0000'                                                          
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPK401 08/29/00'                                      
         END                                                                    
