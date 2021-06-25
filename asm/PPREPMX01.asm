*          DATA SET PPREPMX01  AT LEVEL 070 AS OF 05/05/08                      
*PHASE PPMX01A,+0,NOAUTO                                                        
         TITLE 'PPMX01 - BILLING TRANSFER (POSTINGS)'                           
         PRINT NOGEN                                                            
PPMX01   CSECT                                                                  
         SPACE 2                                                                
         FSPEC READ,ESTIMATES                                                   
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PRTDIR                                                    
*                                                                               
         SPROG 5                                                                
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,98,REPORT                                                     
         PSPEC H3,122,PAGE                                                      
         PSPEC H4,98,RUN                                                        
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
         PSPEC H4,98,RUN                                                        
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
         SPROG 10,20,30                                                         
         PSPEC H8,1,C'         INV        INV    DUE'                           
         PSPEC H9,1,C'PRD EST  NUM        DATE   DATE    MOS   MOA'             
         PSPEC H10,1,C'--- --- ------     ----- -------- ----- -----'           
         PSPEC H8,47,C'RECEIVABLE     GROSS      NET - CD'                      
         PSPEC H9,47,C'  AMOUNT       MEMO       AMOUNT'                        
         PSPEC H10,47,C'------------ ------------ ------------'                 
         PSPEC H8,86,C'  CD      AGENCY'                                        
         PSPEC H9,86,C'AMOUNT  COMMISSION'                                      
         PSPEC H10,86,C'------- -----------'                                    
*                                                                               
         SPROG 20                 CANADIAN AGENCIES ONLY                        
         PSPEC H8,106,C' GST/PST  AOR/IOR    PROD  AC'                          
         PSPEC H9,106,C' AMOUNT   AMOUNT     JOB   OF'                          
         PSPEC H10,106,C'-------- ---------- ------ --'                         
*                                 US VERSION                                    
         SPROG 30                                                               
         PSPEC H8,106,C' AOR/IOR    PROD  AC'                                   
         PSPEC H9,106,C' AMOUNT     JOB   OF'                                   
         PSPEC H10,106,C'---------- ------ --'                                  
*                                                                               
*                                 FOR BILLS IN ERROR                            
*                                 CANADIAN VERSION                              
         SPROG 40,50                                                            
         PSPEC H8,1,C'         INV        INV    DUE'                           
         PSPEC H9,1,C'PRD EST  NUM        DATE   DATE    MOS   MOA'             
         PSPEC H10,1,C'PAGE--- ------     ----- -------- ----- -----'           
         PSPEC H8,47,C'RECEIVABLE     GROSS      NET - CD'                      
         PSPEC H9,47,C'  AMOUNT       MEMO       AMOUNT'                        
         PSPEC H10,47,C'------------ ------------ ------------'                 
         PSPEC H8,86,C'  CD      AGENCY'                                        
         PSPEC H9,86,C'AMOUNT  COMMISSION'                                      
         PSPEC H10,86,C'------- -----------'                                    
*                                                                               
         SPROG 40                                                               
         PSPEC H8,106,C' GST/PST  AOR/IOR    PROD  AC'                          
         PSPEC H9,106,C' AMOUNT   AMOUNT     JOB   OF'                          
         PSPEC H10,106,C'-------- ---------- ------ --'                         
         SPROG 50                                                               
         PSPEC H8,106,C' AOR/IOR    PROD  AC'                                   
         PSPEC H9,106,C' AMOUNT     JOB   OF'                                   
         PSPEC H10,106,C'---------- ------ --'                                  
*                                                                               
         SPROG 60,65                                                            
         PSPEC H9,33,C'PRODUCT'                                                 
         PSPEC H10,33,C'-------'                                                
         SPROG 70,75                                                            
         PSPEC H9,34,C'CLIENT'                                                  
         PSPEC H10,34,C'------'                                                 
         SPROG 80,85                                                            
         PSPEC H9,30,C'MEDIA OFFICE'                                            
         PSPEC H10,30,C'------------'                                           
         SPROG 90,95                                                            
         PSPEC H9,28,C'ACCOUNT OFFICE'                                          
         PSPEC H10,28,C'--------------'                                         
*                                                                               
         SPROG 60,65,70,75,80,85,90,95                                          
         PSPEC H8,43,C'RECEIVABLE     GROSS     NET - CD'                       
         PSPEC H9,43,C'  AMOUNT       MEMO       AMOUNT'                        
         PSPEC H10,43,C'------------ ------------ ------------'                 
         PSPEC H8,82,C'  CD      AGENCY'                                        
         PSPEC H9,82,C'AMOUNT  COMMISSION'                                      
         PSPEC H10,82,C'------- -----------'                                    
         SPROG 60,70,80,90                                                      
         PSPEC H8,99,C' AOR/IOR'                                                
         PSPEC H9,99,C' AMOUNT'                                                 
         PSPEC H10,99,C'----------'                                             
         SPROG 65,75,85,95                                                      
         PSPEC H8,102,C' GST/PST  AOR/IOR'                                      
         PSPEC H9,102,C' AMOUNT   AMOUNT'                                       
         PSPEC H10,102,C'-------- ----------'                                   
*                                                                               
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    X'0102030407080C0D1400'                                          
         DC    CL25'2703ABILLING TYPE'                                          
         DC    CL25'3006ADATE'                                                  
         DC    CL25'5006AINTERFACE DATE'                                        
         DC    CL25'6401ADATE TYPE'                                             
         DC    CL25'6501AMARK OR UNPOST'                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070PPREPMX01 05/05/08'                                      
         END                                                                    
