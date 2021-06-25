*          DATA SET PPREP0901  AT LEVEL 014 AS OF 05/29/90                      
*PHASE PP0901A,+0                                                               
         TITLE 'PP0901 - PRINTPAK REBILLING  SPECS'                             
         PRINT NOGEN                                                            
         FSPEC UPDATE,PRTFILE                                                   
         SPROG 0,5,10,15,20,30                                                  
         PSPEC H1,41,CL24'PRINTPAK ''REBILLING'' RUN'                           
         PSPEC H2,41,C'------------------------'                                
         PSPEC H1,95,AGYNAME                                                    
         PSPEC H2,95,AGYADD                                                     
         PSPEC H4,95,REPORT                                                     
         PSPEC H5,95,RUN                                                        
         PSPEC H9,74,C'GROSS'                                                   
         PSPEC H9,91,C'BILL'                                                    
         PSPEC H9,108,C'NET'                                                    
         PSPEC H9,121,C'ACTUAL'                                                 
         SPROG 10                                                               
         PSPEC H3,48,C'DETAIL BILLING'                                          
         SPROG 5                                                                
         PSPEC H3,48,C'SUMMARY BILLING'                                         
         SPROG 20                                                               
         PSPEC H3,50,C'NEW BILLING'                                             
         SPROG 30                                                               
         PSPEC H3,48,C'REBATE BILLING'                                          
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'01020304050607080C0D131400'                                    
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014PPREP0901 05/29/90'                                      
         END                                                                    
