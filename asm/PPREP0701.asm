*          DATA SET PPREP0701  AT LEVEL 016 AS OF 08/02/07                      
*PHASE PP0701A,+0                                                               
         TITLE 'PP0701 - PRINTPAK UNBILL PPG SPECS'                             
PP0701   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,ACTIVE                                                      
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PRTDIR                                                    
         SPROG 0,5,10,15,20,30                                                  
         PSPEC H1,41,CL24'PRINTPAK ''UNBILLING'' RUN'                           
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
         DC    CL25'5306DBILLING DATE'                                          
         DC    CL25'2804AINVOICE NUMBER'                                        
         DC    CL25'3204AREVERSED INV. NO.'                                     
         DC    CL25'A112AGROSS'                                                 
         DC    CL25'B312AGROSS-DETAILS'                                         
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016PPREP0701 08/02/07'                                      
         END                                                                    
