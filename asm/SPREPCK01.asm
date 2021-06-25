*          DATA SET SPREPCK01  AT LEVEL 002 AS OF 08/29/00                      
*PHASE SPCK01A                                                                  
         TITLE 'SPCK01 - CCUSA COMPLETED MARKET REPORT'                         
SPCK01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H1,56,C'COKE COMPLETED MARKETS'                                  
         SSPEC H2,56,C'----------------------'                                  
         SSPEC H1,94,AGYNAME                                                    
         SSPEC H2,94,AGYADD                                                     
         SSPEC H4,22,C'        PRODUCT         '                                
         SSPEC H5,22,C'------------------------'                                
         SSPEC H4,48,C'        ESTIMATE        '                                
         SSPEC H5,48,C'------------------------'                                
         SSPEC H4,74,C'           MARKET            '                           
         SSPEC H5,74,C'-----------------------------'                           
         SSPEC H4,105,C'AGENCY'                                                 
         SSPEC H5,105,C'------'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPCK01 08/29/00'                                      
         END                                                                    
