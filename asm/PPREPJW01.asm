*          DATA SET PPREPJW01  AT LEVEL 003 AS OF 02/01/11                      
*PHASE PPJW01A,+0                                                               
         TITLE 'PPJW01 - JWT INVOICE LISTING SPECS'                             
PPJW01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         FSPEC READ,BILLS                                                       
         SPACE 2                                                                
         SPROG 0,50                                                             
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,52,C'JWT PRINTPAK INVOICE INTERFACE'                          
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,52,C'------------------------------'                          
         PSPEC H3,56,PERIOD                                                     
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,122,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
         PSPEC  H8,1,C'        BILLNG INVOICE'                                  
         PSPEC  H9,1,C'PRD EST PERIOD NUMBER '                                  
         PSPEC H10,1,C'--- --- ------ ------ '                                  
*                                                                               
         PSPEC  H8,23,C'         BILL'                                          
         PSPEC  H9,23,C'RUN DATE DATE   TYPE'                                   
         PSPEC H10,23,C'-------- ----- ------'                                  
*                                                                               
         PSPEC  H8,64,C'NET LESS'                                               
         PSPEC  H9,45,C'GROSS AMOUNT      CD AMOUNT  CASH DISCOUNT'             
         PSPEC H10,45,C'------------      ---------  -------------'             
*                                                                               
         PSPEC  H8,87,C'       AGENCY        ACTUAL'                            
         PSPEC  H9,87,C'     COMMISSION    BILL AMOUNT'                         
         PSPEC H10,87,C'     ----------    -----------'                         
*                                                                               
         SPROG  50                                                              
         PSPEC  H7,120,C'    GST  '                                             
         PSPEC  H8,120,C'  HST/PST'                                             
         PSPEC  H9,120,C'  AMOUNT'                                              
         PSPEC H10,120,C'-----------'                                           
*                                                                               
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPREPJW01 02/01/11'                                      
         END                                                                    
