*          DATA SET SPREPZ101  AT LEVEL 012 AS OF 08/29/00                      
*PHASE SPZ101A                                                                  
         TITLE 'SPZ101 - SPOTPAK BILLING COMPARRISION'                          
SPZ101   CSECT                                                                  
         PRINT GEN                                                              
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,48,C'B I L L I N G   C O M P A R I S O N'                     
         SSPEC H2,48,C'-----------------------------------'                     
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,CLIENT                                                      
         SSPEC H3,1,PRODUCT                                                     
         SSPEC H4,1,ESTIMATE                                                    
*                                                                               
         SSPEC H1,100,REQUESTOR                                                 
         SSPEC H3,100,PAGE                                                      
         SSPEC H3,111,REPORT                                                    
*                                                                               
    SSPEC H7,1,C'AG MNTH SVC BILL MONTH INVOICE BILL GROSS   BILL NET  X        
                  STATION GROSS STATION NET  GROSS DIF    NET DIF'              
    SSPEC H8,1,C'-- -------- ---------- ------- ------------ ----------X        
               -- ------------- ------------ ------------ ------------'         
*                                                                               
         DC    X'00'                                                            
*                                                                               
 END                                                                            
