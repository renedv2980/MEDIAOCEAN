*          DATA SET NEMED53S   AT LEVEL 009 AS OF 08/10/00                      
*PHASE T31E53A                                                                  
         TITLE 'T31E53 - SPECS FOR PROGRAM LIST'                                
T31E53   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V'                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'PROGRAM LISTING'                                         
         SSPEC H2,52,C'---------------'                                         
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,1,C'NETWORK'                                                  
         SSPEC H5,1,PAGE                                                        
         SSPEC H4,98,NETREP                                                     
         SSPEC H5,98,PERIOD                                                     
         SSPEC H7,1,C'CODE   PROGRAM NAME'                                      
         SSPEC H8,1,C'----   ------------'                                      
         SSPEC H9,1,C'       DAY TIMES'                                         
         SSPEC H10,2,C'      --- -----'                                         
         SSPEC H7,025,C'START DATE  '                                           
         SSPEC H8,025,C'END DATE    '                                           
         SSPEC H9,025,C'SHR/RTG NTI#'                                           
         SSPEC H10,25,C'COST    DYPT'                                           
         SSPEC H7,45,C'-----------------------------VPH------------'            
         SSPEC H8,45,C'  18   18   18   21   21   25   25   35   55'            
         SSPEC H9,45,C'   +   34   49    +   49   49   54   64    +'            
         SSPEC H7,89,C'----------------------- OTHERS WWRK HHWC'                
         SSPEC H8,89,C'   12   15    6    2       V2+  18+  U06'                
         SSPEC H9,89,C'   17   24   11   11     V9-11 1849  U12'                
         SSPEC H10,90,C'                         MOMS 2554  U18'                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009NEMED53S  08/10/00'                                      
         END                                                                    
