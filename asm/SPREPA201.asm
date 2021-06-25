*          DATA SET SPREPA201  AT LEVEL 025 AS OF 08/29/00                      
*PHASE SPA201A                                                                  
         TITLE 'SPA201 - SPOTPAK PRODUCT SUMMARIES - SPECS'                     
SPA201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         SPROG 0,THRU,9                                                         
**                                                                              
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H4,1,PGROUP                                                      
*                                                                               
         SSPEC H3,51,PERIOD                                                     
         SSPEC H4,51,MGROUP                                                     
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         SPROG 1                                                                
*--->    SSPEC H1,55,C'PRODUCT ORDERED SUMMARY'                                 
         SSPEC H1,55,SP#PRO01,23,C                                              
*--->    SSPEC H2,55,C'-----------------------'                                 
         SSPEC H2,55,SP#PRD23,23,C                                              
         SPROG 2                                                                
*--->    SSPEC H1,56,C'PRODUCT PAID SUMMARY'                                    
         SSPEC H1,56,SP#PRO07,20,C                                              
*--->    SSPEC H2,56,C'--------------------'                                    
         SSPEC H2,56,SP#PRD20,20,C                                              
         SPROG 3                                                                
*--->    SSPEC H1,55,C'PRODUCT UNPAID SUMMARY'                                  
         SSPEC H1,55,SP#PRO03,22,C                                              
*--->    SSPEC H2,55,C'----------------------'                                  
         SSPEC H2,55,SP#PRD22,22,C                                              
         SPROG 4                                                                
*--->    SSPEC H1,55,C'PRODUCT BILLED SUMMARY'                                  
         SSPEC H1,55,SP#PRO04,22,C                                              
*--->    SSPEC H2,55,C'----------------------'                                  
         SSPEC H2,55,SP#PRD22,22,C                                              
         SPROG 5                                                                
*--->    SSPEC H1,54,C'PRODUCT BILLABLE SUMMARY'                                
         SSPEC H1,54,SP#PRO05,24,C                                              
*--->    SSPEC H2,54,C'------------------------'                                
         SSPEC H2,54,SP#PRD24,24,C                                              
         SPROG 6                                                                
*--->    SSPEC H1,53,C'PRODUCT COMMISSION SUMMARY'                              
         SSPEC H1,53,SP#PRO06,26,C                                              
*--->    SSPEC H2,53,C'--------------------------'                              
         SSPEC H2,53,SP#PRD26,26,C                                              
         SPROG 7                                                                
*--->    SSPEC H1,55,C'POL UNALLOCATED SUMMARY'                                 
         SSPEC H1,55,SP#POL01,22,C                                              
*--->    SSPEC H2,55,C'-----------------------'                                 
         SSPEC H2,55,SP#PRD22,22,C                                              
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    CL25'5006DPAYMENTS FROM'                                         
         DC    CL25'5606DPAYMENTS TO'                                           
         DC    CL25'6201AREPORT TYPE'                                           
         DC    CL25'6701APRINT COMMENTS'                                        
         DC    CL25'6801ABILLED TODAY ONLY'                                     
         DC    X'00'                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
 END                                                                            
