*          DATA SET NEMED53    AT LEVEL 005 AS OF 05/18/99                      
*          DATA SET NEMED53    AT LEVEL 008 AS OF 04/29/88                      
*PHASE T31E53A,+0                                                               
         TITLE 'T31E53 - SPECS FOR PROGRAM LIST'                                
T31E53   CSECT                                                                  
         PRINT NOGEN                                                            
         WSPEC H1,1,C'MEDIA     NETWORK T.V'                                    
         WSPEC H2,1,REQUESTOR                                                   
         WSPEC H1,52,C'PROGRAM LISTING'                                         
         WSPEC H2,52,C'---------------'                                         
         WSPEC H1,98,AGYNAME                                                    
         WSPEC H2,98,AGYADD                                                     
         WSPEC H4,1,C'NETWORK'                                                  
         WSPEC H5,1,PAGE                                                        
         WSPEC H4,98,NETREP                                                     
         WSPEC H5,98,PERIOD                                                     
         WSPEC H7,1,C'CODE   PROGRAM NAME'                                      
         WSPEC H8,1,C'----   ------------'                                      
         WSPEC H9,1,C'       DAY TIMES'                                         
         WSPEC H10,2,C'      --- -----'                                         
         WSPEC H7,025,C'START DATE  '                                           
         WSPEC H8,025,C'END DATE    '                                           
         WSPEC H9,025,C'SHR/RTG NTI#'                                           
         WSPEC H10,25,C'COST    DYPT'                                           
         WSPEC H7,45,C'-----------------------------VPH------------'            
         WSPEC H8,45,C'  18   18   18   21   21   25   25   35   55'            
         WSPEC H9,45,C'   +   34   49    +   49   49   54   64    +'            
         WSPEC H7,89,C'----------------------- OTHERS WWRK HHWC -----'          
         WSPEC H8,89,C'   12   15    6    2       V2+  18+  U06 V9-14'          
         WSPEC H9,89,C'   17   24   11   11     V9-11 1849  U12'                
         WSPEC H7,135,C'-----'                                                  
         WSPEC H8,135,C'  45'                                                   
         WSPEC H9,135,C'   +'                                                   
         WSPEC H10,90,C'                         MOMS 2554  U18'                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEMED53   05/18/99'                                      
         END                                                                    
