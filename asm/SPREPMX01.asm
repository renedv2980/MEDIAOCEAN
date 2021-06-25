*          DATA SET SPREPMX01  AT LEVEL 091 AS OF 05/05/08                      
*PHASE SPMX01A,+0,NOAUTO                                                        
         TITLE 'SPMX01 - BILLING TRANSFER (POSTINGS)'                           
         PRINT NOGEN                                                            
SPMX01   CSECT                                                                  
         SPACE 2                                                                
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
*                                                                               
         SPROG 5                                                                
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,98,REPORT                                                     
         PSPEC H3,122,PAGE                                                      
         PSPEC H1,58,C'BILLING TRANSFER'                                        
         PSPEC H2,58,C'----------------'                                        
*                                                                               
         SPROG 0,1,10,20,30,40,50,60,65,70,75,80,85,90,95                       
         PSPEC H1,1,MEDIA                                                       
         PSPEC H3,1,CLIENT                                                      
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,98,REPORT                                                     
         PSPEC H3,122,PAGE                                                      
         PSPEC H1,58,C'BILLING TRANSFER'                                        
         PSPEC H2,58,C'----------------'                                        
*                                                                               
         SPROG 40,50                                                            
         PSPEC H3,55,C'BILLS NOT TRANSFERRED'                                   
         SPROG 60,65                                                            
         PSPEC H3,58,C'PRODUCT SUMMARY'                                         
         SPROG 70,75                                                            
         PSPEC H3,59,C'CLIENT SUMMARY'                                          
         SPROG 80,85                                                            
         PSPEC H3,56,C'MEDIA OFFICE SUMMARY'                                    
         SPROG 90,95                                                            
         PSPEC H3,54,C'ACCOUNT OFFICE SUMMARY'                                  
*                                                                               
         SPROG 10,20,30,40,50                                                   
         PSPEC H8,1,C'         INV        INV     DUE'                          
         PSPEC H9,1,C'PRD EST  NUM        DATE    DATE    MOS   MOA'            
         PSPEC H10,1,C'--- --- ------     ----- -------- ----- -----'           
         PSPEC H8,47,C'RECEIVABLE     GROSS       NET'                          
         PSPEC H9,47,C'  AMOUNT       MEMO       AMOUNT'                        
         PSPEC H10,47,C'------------ ------------ ------------'                 
         PSPEC H8,86,C'  AGENCY'                                                
         PSPEC H9,86,C'COMMISSION'                                              
         PSPEC H10,86,C'-----------'                                            
*                                                                               
         SPROG 20                 CANADIAN AGENCIES ONLY                        
         PSPEC H8,98,C' GST/PST  AOR/IOR   AC'                                  
         PSPEC H9,98,C' AMOUNT   AMOUNT    OF'                                  
         PSPEC H10,98,C'-------- ---------- --'                                 
*                                 US VERSION                                    
         SPROG 30                                                               
         PSPEC H8,98,C' AOR/IOR   AC'                                           
         PSPEC H9,98,C' AMOUNT    OF'                                           
         PSPEC H10,98,C'---------- --'                                          
*                                                                               
*                                 FOR BILLS IN ERROR                            
*                                 CANADIAN VERSION                              
         SPROG 40                                                               
         PSPEC H8,98,C' GST/PST  AOR/IOR   AC PAGE'                             
         PSPEC H9,98,C' AMOUNT   AMOUNT    OF NUM'                              
         PSPEC H10,98,C'-------- ---------- -- ----'                            
*                                 US VERSION (BILLS IN ERROR)                   
         SPROG 50                                                               
         PSPEC H8,98,C' AOR/IOR   AC PAGE'                                      
         PSPEC H9,98,C' AMOUNT    OF NUM'                                       
         PSPEC H10,98,C'---------- -- ----'                                     
*                                 SUMMARIES                                     
         SPROG 60,65                                                            
         PSPEC H9,33,C'PRODUCT'                                                 
         PSPEC H10,33,C'-------'                                                
*                                                                               
         SPROG 70,75                                                            
         PSPEC H9,34,C'CLIENT'                                                  
         PSPEC H10,34,C'------'                                                 
*                                                                               
         SPROG 80,85                                                            
         PSPEC H9,30,C'MEDIA OFFICE'                                            
         PSPEC H10,30,C'------------'                                           
*                                                                               
         SPROG 90,95                                                            
         PSPEC H9,28,C'ACCOUNT OFFICE'                                          
         PSPEC H10,28,C'--------------'                                         
*                                                                               
         SPROG 60,65,70,75,80,85,90,95                                          
         PSPEC H8,47,C'RECEIVABLE     GROSS       NET'                          
         PSPEC H9,47,C'  AMOUNT       MEMO       AMOUNT'                        
         PSPEC H10,47,C'------------ ------------ ------------'                 
         PSPEC H8,86,C'  AGENCY     AOR/IOR'                                    
         PSPEC H9,86,C'COMMISSION   AMOUNT'                                     
         PSPEC H10,86,C'----------- ----------'                                 
*                                                                               
         SPROG 65,75,85,95                                                      
         PSPEC H8,98,C' GST/PST  AOR/IOR'                                       
         PSPEC H9,98,C' AMOUNT   AMOUNT'                                        
         PSPEC H10,98,C'-------- ----------'                                    
*                                                                               
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    CL25'3203ABILLING TYPE'                                          
         DC    CL25'5006AINTERFACE DATE'                                        
         DC    CL25'5606ADATE'                                                  
         DC    CL25'6301ASTATION TYPE'                                          
         DC    CL25'6401ADATE TYPE'                                             
         DC    CL25'6501AMARK OR UNPOST'                                        
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091SPREPMX01 05/05/08'                                      
         END                                                                    
