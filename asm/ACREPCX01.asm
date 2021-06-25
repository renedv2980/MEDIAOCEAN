*          DATA SET ACREPCX01  AT LEVEL 005 AS OF 08/16/00                      
*PHASE ACCX01A,*                                                                
         TITLE 'ACCX01 - SPECS FOR COKE EXPENDITURE REPORT'                     
ACCX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         RSPEC MAXLINES,56                                                      
         SPROG 0,1,2,3                                                          
         ASPEC H1,2,RUN,WIDE=198                                                
         ASPEC H1,125,REPORT,WIDE=198                                           
         ASPEC H1,150,PAGE,WIDE=198                                             
         ASPEC H2,125,REQUESTOR,WIDE=198                                        
         ASPEC H5,2,C'PERIOD',WIDE=198                                          
         ASPEC H8,4,C'BOTTLER/BUY TYPE/MEDIA',WIDE=198                          
         ASPEC H8,30,C' CURRENT    Y-T-D    BUDGET    BALANCE ',       X        
               WIDE=198                                                         
         ASPEC H9,30,C'    $         $         $         $    ',       X        
               WIDE=198                                                         
         ASPEC H8,70,C'  JAN     FEB     MAR     APR     MAY     JUN ',X        
               WIDE=198                                                         
         ASPEC H9,70,C'   $       $       $       $       $       $  ',X        
               WIDE=198                                                         
         ASPEC H8,118,C'  JUL     AUG     SEP     OCT     NOV     DEC',X        
               WIDE=198                                                         
         ASPEC H9,118,C'   $       $       $       $       $       $ ',X        
               WIDE=198                                                         
         SPACE 1                                                                
         SPROG 0,1,3                                                            
         ASPEC H4,125,C'AREA',WIDE=198                                          
         SPACE 1                                                                
         SPROG 0,3                                                              
         ASPEC H5,125,C'REGION',WIDE=198                                        
         ASPEC H6,125,C'DISTRICT',WIDE=198                                      
         ASPEC H3,2,C'PRODUCT',WIDE=198                                         
         SPROG 3                                                                
         ASPEC H9,19,C'VEHICLE',WIDE=198                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPCX01 08/16/00'                                      
         END                                                                    
