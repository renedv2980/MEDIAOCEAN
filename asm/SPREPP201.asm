*          DATA SET SPREPP201  AT LEVEL 013 AS OF 08/29/00                      
*PHASE SPP201A                                                                  
         TITLE 'SPP201 - MASTER MARKET LIST'                                    
SPP201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC OPEN,DEMFILES                                                    
*                                                                               
         SSPEC H1,51,C'MASTER MARKET LISTING'                                   
         SSPEC H2,51,C'---------------------'                                   
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,100,REQUESTOR                                                 
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         DC    X'00'                                                            
 END                                                                            
