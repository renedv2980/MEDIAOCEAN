*          DATA SET SPREPPB01  AT LEVEL 016 AS OF 08/29/00                      
*PHASE SPPB01A                                                                  
         TITLE 'SPPB01 - STATION MASTER LIST'                                   
SPPB01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC OPEN,DEMFILES                                                    
*                                                                               
         SSPEC H1,51,C'STATION/MARKET LIST'                                     
         SSPEC H2,51,C'-------------------'                                     
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,100,REQUESTOR                                                 
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         DC    X'00'                                                            
 END                                                                            
