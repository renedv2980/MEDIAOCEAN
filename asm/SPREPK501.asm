*          DATA SET SPREPK501  AT LEVEL 003 AS OF 07/16/06                      
*PHASE SPK501A                                                                  
SPK501   TITLE '- Spec Phase for Network Allocation Program'                    
SPK501   CSECT                                                                  
         PRINT NOGEN                                                            
                                                                                
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC USE,SPK503                                                       
                                                                                
         SPROG 1                   PARAMETER REPORT HEADS                       
                                                                                
         SSPEC H1,54,C'Brand Allocation Report'                                 
         SSPEC H2,54,C'-----------------------'                                 
                                                                                
         SPROG 2                   RECAP REPORT                                 
                                                                                
         SSPEC H1,49,C'Brand Allocation - Summary Report'                       
         SSPEC H2,49,C'---------------------------------'                       
                                                                                
         SPROG 3                   WEEKLY ANALYSIS                              
                                                                                
         SSPEC H1,48,C'Brand Allocation - Weekly Analysis'                      
         SSPEC H2,48,C'----------------------------------'                      
                                                                                
         SPROG 4,5                 DETAIL REPORT                                
                                                                                
         SSPEC H1,49,C'Brand Allocation - Detail Listing'                       
         SSPEC H2,49,C'---------------------------------'                       
                                                                                
         SPROG 6                   PROGRAM/PRODUCT RECAP                        
                                                                                
         SSPEC H1,47,C'Brand Allocation - Program/Brand Recap'                  
         SSPEC H2,47,C'--------------------------------------'                  
                                                                                
         SPROG 7                   GOAL REPORT                                  
                                                                                
         SSPEC H1,45,C'Brand Allocation - Goal Balancing Report'                
         SSPEC H2,45,C'----------------------------------------'                
                                                                                
         SPROG 8                   POD CHANGE ANALYSIS                          
                                                                                
         SSPEC H1,52,C'POD Change Analysis Report'                              
         SSPEC H2,52,C'--------------------------'                              
                                                                                
         SPROG 0,1,2,3,4,5,6,7,8                                                
                                                                                
         SSPEC H3,50,PERIOD                                                     
                                                                                
         SSPEC H1,1,MEDIA                                                       
         SSPEC H3,1,CLIENT                                                      
         SSPEC H4,1,PRODUCT                                                     
         SSPEC H5,1,ESTIMATE                                                    
         SSPEC H6,1,MARKET                                                      
                                                                                
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,98,REPORT                                                     
         SSPEC H5,98,PAGE                                                       
                                                                                
         DC    X'00'                                                            
                                                                                
         DC    C'REQLST='                                                       
                                                                                
         DC    CL25'5901ADaypart filter'                                        
         DC    CL25'6002ASpot length filter'                                    
         DC    CL25'6201AReallocate?'                                           
         DC    CL25'6301ABasis- Demo or $'                                      
         DC    CL25'6401AAllocate excess?'                                      
         DC    CL25'6501ATest only'                                             
         DC    CL25'6701AZero week option'                                      
         DC    X'00'                                                            
                                                                                
         DC    C'REQ2LST='                                                      
                                                                                
         DC    CL25'7802ADaypart filter'                                        
         DC    X'00'                                                            
                                                                                
         DC    X'00'                                                            
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPK501 07/16/06'                                      
         END                                                                    
