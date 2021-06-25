*          DATA SET SPREPPA01  AT LEVEL 015 AS OF 08/29/00                      
*PHASE SPPA01A                                                                  
         TITLE 'SPPA01 - PROGRAM NAME ANALYSIS'                                 
SPPA01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC OPEN,DEMFILES                                                    
*                                                                               
         SSPEC H1,51,C'SPOT PROGRAM NAME ANALYSIS'                              
         SSPEC H2,51,C'--------------------------'                              
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,100,REQUESTOR                                                 
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         DC    X'00'                                                            
 END                                                                            
