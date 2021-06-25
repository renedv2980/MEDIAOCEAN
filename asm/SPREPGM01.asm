*          DATA SET SPREPGM01  AT LEVEL 014 AS OF 05/02/06                      
*PHASE SPGM01A                                                                  
         TITLE 'SPGM01 - SPOTPAK GM INTERFACE SPECS'                            
         PRINT NOGEN                                                            
SPGM01   CSECT                                                                  
         SPACE 2                                                                
         FSPEC READ,BILLS                                                       
         FSPEC USE,SPGM03                                                       
         SPACE 2                                                                
         SPROG 0,50                                                             
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,56,C'SPOTPAK GM INTERFACE'                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,56,C'--------------------'                                    
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,98,REPORT                                                     
         SSPEC H4,122,PAGE                                                      
         SSPEC H5,98,RUN                                                        
*                                                                               
         SSPEC  H8,1,C'        BILLNG INVOICE'                                  
         SSPEC  H9,1,C'PRD EST PERIOD NUMBER '                                  
         SSPEC H10,1,C'--- --- ------ ------ '                                  
*                                                                               
         SSPEC  H8,23,C'         BILL'                                          
         SSPEC  H9,23,C'RUN DATE DATE   TYPE'                                   
         SSPEC H10,23,C'-------- ----- ------'                                  
*                                                                               
         SSPEC  H9,47,C'GROSS AMOUNT      NET AMOUNT'                           
         SSPEC H10,47,C'------------      ----------'                           
*                                                                               
         SSPEC  H8,75,C'      AGENCY         ACTUAL'                            
         SSPEC  H9,75,C'    COMMISSION     BILL AMOUNT'                         
         SSPEC H10,75,C'    ----------     -----------'                         
*                                                                               
         SPROG  50                                                              
         SSPEC  H7,110,C'    GST  '                                             
         SSPEC  H8,110,C'  HST/PST'                                             
         SSPEC  H9,110,C'  AMOUNT'                                              
         SSPEC H10,110,C'-----------'                                           
*                                                                               
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPGM01 05/02/06'                                      
         END                                                                    
