*          DATA SET SPREPJ101  AT LEVEL 026 AS OF 03/28/01                      
*PHASE SPJ101A                                                                  
         TITLE 'SPJ101 - SPOTPAK BUYER INCENTIVE REPORT - SPECS'                
SPJ101   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC OPEN,DEMFILES                                                    
         FSPEC GET,MARKET                                                       
         SPROG 0,THRU,9                                                         
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
*                                                                               
         SSPEC H3,1,MGROUP                                                      
*                                                                               
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H5,77,PAGE                                                       
         SSPEC H5,88,REPORT                                                     
         SSPEC H6,77,RATING                                                     
         SSPEC H7,77,BOOK                                                       
*                                                                               
         SSPEC H1,44,C'BUYER INCENTIVE REPORT'                                  
         SSPEC H2,44,C'----------------------'                                  
*                                                                               
         SSPEC H3,39,PERIOD                                                     
*                                                                               
         SSPEC H10,01,C'CLIENT'                                                 
         SSPEC H9,24,C'- - - - -  N O N-F D A - - - - -'                        
         SSPEC H10,24,C'PURCHASED   RERATED   DIFF  INDEX'                      
*****                  1234567.9 1234567.9 1234.6 123.45                        
*****                  +23(9)    +33(9)    +43(6) +50(6)                        
         SSPEC H9,58,C'- - - - - - - F D A  - - - - - -'                        
         SSPEC H10,58,C'PURCHASED   RERATED   DIFF  INDEX'                      
*****                  1234567.9 1234567.9 1234.6 123.45                        
*****                  +58(9)    +68(9)    +78(6) +85(6)                        
**NON**  SSPEC H9,93,C'WGHTD'                                                   
         SSPEC H10,93,C'INDEX'                                                  
*                                                                               
         SPROG 2                                                                
         SSPEC H5,46,C'** BUYER RECAP **'                                       
*                                                                               
         SPROG 3                                                                
         SSPEC H5,46,C'** OFFICE RECAP **'                                      
*                                                                               
         DC    X'00'                                                            
 END                                                                            
