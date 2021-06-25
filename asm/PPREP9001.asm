*          DATA SET PPREP9001  AT LEVEL 010 AS OF 07/18/16                      
*PHASE PP9001A                                                                  
         TITLE 'PP9001 PRINTPAK TRAIL BALANCE PPG'                              
PP9001   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBLICATIONS                                                 
         RSPEC REQUEST,REPORT                                                   
         RSPEC MAXLINES,55                                                      
         SPROG 0,2,3,4,5,7,8,9                                                  
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H1,60,C'TRIAL BALANCE'                                           
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,56,22C'-'                                                     
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,53,C'FOR THE PERIOD THROUGH'                                  
         PSPEC H3,98,REPORT                                                     
         PSPEC H4,1,CLIENT                                                      
         PSPEC H4,98,RUN                                                        
         PSPEC H4,123,PAGE                                                      
         PSPEC H5,1,PRODUCT                                                     
         PSPEC H5,56,C'CURRENT MONTH OF'                                        
         PSPEC H7,1,C'--------------  * GROSS LESS C/D *  -------------X        
               -- BILLING ACTIVITY JOURNAL ------------------  * NET/NEX        
               T *  ---------------------'                                      
         PSPEC H9,5,C'PRV BILLING     CUR BILLING       REVERSALS  PRD X        
               EST   BILL NO.  T   M/S    RVSD BY  PRV BILLING     CUR X        
               BILLING      REVERSALS'                                          
         PSPEC H10,1,132C'-'                                                    
         SPROG 2,3,4                                                            
         PSPEC H7,1,C'--------------  * GROSS LESS C/D *  -------------X        
               -  DETAIL ACTIVITY JOURNAL  ------------------  * NET/NEX        
               T *  ----------------------'                                     
         PSPEC H9,1,C'PRV BILLING  PRV CLEARNCS CUR BILLING  CUR CLEARNX        
               CS     DATE  -SL   EST         PRV BILLING  PRV CLEARNCSX        
                CUR BILLING  CUR CLEARNCS'                                      
         PSPEC H10,1,131C'-'                                                    
         SPROG 3                                                                
         PSPEC H5,1,40C' '                                                      
         SPROG 4                                                                
         PSPEC H4,1,40C' '                                                      
         PSPEC H5,1,40C' '                                                      
         SPROG 5                                                                
         PSPEC H7,14,C'----  ** GROSS **  ----'                                 
         PSPEC H7,95,C'---  ** NET **  ---'                                     
         SPROG 7,8,9                                                            
         PSPEC H7,1,C'-----------------  ** GROSS **  -----------------X        
               -  DETAIL ACTIVITY JOURNAL  --------------------  ** NETX        
                **  ----------------------'                                     
         PSPEC H9,1,C'PRV BILLING  PRV CLEARNCS CUR BILLING  CUR CLEARNX        
               CS     DATE  -SL   EST         PRV BILLING  PRV CLEARNCSX        
                CUR BILLING  CUR CLEARNCS'                                      
         PSPEC H10,1,131C'-'                                                    
         SPROG 8                                                                
         PSPEC H5,1,40C' '                                                      
         SPROG 9                                                                
         PSPEC H4,1,40C' '                                                      
         PSPEC H5,1,40C' '                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PPREP9001 07/18/16'                                      
         END                                                                    
