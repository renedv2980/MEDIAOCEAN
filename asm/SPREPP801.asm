*          DATA SET SPREPP801  AT LEVEL 018 AS OF 08/29/00                      
*PHASE SPP801A                                                                  
         TITLE 'SPP801 - NETWORK PROGRAM DATA EXTRACT- ARB NPA DATA'            
SPP801   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC OPEN,DEMFILES                                                    
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,100,REQUESTOR                                                 
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         DC    X'00'                                                            
 END                                                                            
