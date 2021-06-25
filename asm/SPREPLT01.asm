*          DATA SET SPREPLT01  AT LEVEL 006 AS OF 05/18/05                      
*PHASE SPLT01A                                                                  
         TITLE 'SPLT01 - SPOTPAK LABATTS INTERFACE SPECS'                       
         PRINT NOGEN                                                            
SPLT01   CSECT                                                                  
         SPACE 2                                                                
         FSPEC READ,BILLS                                                       
         FSPEC USE,SPLT03                                                       
         SPACE 2                                                                
         SPROG 0,50                                                             
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,56,C'SPOTPAK LABATTS INTERFACE'                               
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,56,C'------------------------'                                
         SSPEC H3,56,PERIOD                                                     
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
         SSPEC  H8,64,C'NET LESS'                                               
         SSPEC  H9,45,C'GROSS AMOUNT      CD AMOUNT  CASH DISCOUNT'             
         SSPEC H10,45,C'------------      ---------  -------------'             
*                                                                               
         SSPEC  H8,87,C'       AGENCY        ACTUAL'                            
         SSPEC  H9,87,C'     COMMISSION    BILL AMOUNT'                         
         SSPEC H10,87,C'     ----------    -----------'                         
*                                                                               
         SPROG  50                                                              
         SSPEC  H7,120,C'    GST  '                                             
         SSPEC  H8,120,C'  HST/PST'                                             
         SSPEC  H9,120,C'  AMOUNT'                                              
         SSPEC H10,120,C'-----------'                                           
*                                                                               
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPLT01 05/18/05'                                      
         END                                                                    
