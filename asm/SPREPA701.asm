*          DATA SET SPREPA701  AT LEVEL 051 AS OF 08/29/00                      
*PHASE SPA701A                                                                  
         TITLE 'SPREPA701 - MARKET ACTIVITY ANALYSIS'                           
*        PRINT NOGEN                                                            
SPA701   CSECT                                                                  
***********************************************************************         
*                                                                               
* NOTE SPA701 & SPA702 USE SUBCONTROLLER SPA603                                 
*                                                                               
***********************************************************************         
         FSPEC USE,SPA603                                                       
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,44,C'MARKET ACTIVITY ANALYSIS'                                
         SSPEC H1,77,AGYNAME                                                    
*                                                                               
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,44,C'------------------------'                                
         SSPEC H2,77,AGYADD                                                     
*                                                                               
**NOP**  SSPEC H3,1,C'CABLE EXCLUDED'                                           
*                                                                               
         SSPEC H4,1,MGROUP                                                      
         SSPEC H4,77,REPORT                                                     
         SSPEC H4,100,PAGE                                                      
         SSPEC H8,1,C'          ----- MONTHLY DOLLARS ----'                     
         SSPEC H9,1,C'CLT  MON        19XX        19XX INX'                     
         SSPEC H10,1,C'---  ---        ----        ---- ---'                    
         SSPEC H8,39,C'- MONTHLY TRANS -'                                       
         SSPEC H9,39,C'  19XX   19XX INX'                                       
         SSPEC H10,39,C'  ----   ---- ---'                                      
         SSPEC H8,58,C'------- CUME DOLLARS ------'                             
         SSPEC H9,58,C'       19XX        19XX INX'                             
         SSPEC H10,58,C'       ----        ---- ---'                            
         SSPEC H8,87,C'---- CUME TRANS ---'                                     
         SSPEC H9,87,C'   19XX    19XX INX'                                     
         SSPEC H10,87,C'   ----    ---- ---'                                    
*                                                                               
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    CL25'3003NREP NUMBER'                                            
         DC    CL25'3301AREP TYPE'                                              
         DC    CL25'5801AAFFILIATE FILTER'                                      
         DC    CL25'6101ASUMMARIES ONLY'                                        
         DC    CL25'6201ACLIENT DETAIL LEVEL'                                   
         DC    CL25'6401AANALYZE BY OFF'                                        
         DC    CL25'6501ASUPPRESS NET'                                          
         DC    CL25'6601ASUPPRESS CLT'                                          
         DC    CL25'6701ATOTAL OPTION'                                          
         DC    CL25'6801AEXCLUDE CODE'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051SPREPA701 08/29/00'                                      
         END                                                                    
