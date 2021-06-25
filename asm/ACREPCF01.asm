*          DATA SET ACREPCF01  AT LEVEL 001 AS OF 05/31/90                      
*PHASE ACCF01A,+0                                                               
         TITLE 'COST OF FINANCING REPORT'                                       
ACCF01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF MAXLINES,56                                                      
*                                                                               
         ACDEF SPROG,0,1,2                                                      
         ACDEF H1,1,RUN,WIDE=198                                                
         ACDEF H1,139,PAGE,WIDE=198                                             
         ACDEF H2,1,C'COMPANY',WIDE=198                                         
         ACDEF H3,136,C'REPORT ACCF',WIDE=198                                   
         ACDEF H4,136,REQUESTOR,WIDE=198                                        
         ACDEF H5,136,PERIOD,WIDE=198                                           
         ACDEF H1,67,C'COST OF FINANCING REPORT',WIDE=198                       
         ACDEF H2,67,24C'_',WIDE=198                                            
*                                                                               
*        DETAIL REPORT                                                          
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H4,67,C'    (DETAIL VERSION)    ',WIDE=198                       
         ACDEF H8,54,C'BILLING',WIDE=198                                        
         ACDEF H9,54,C'SOURCE ',WIDE=198                                        
         ACDEF H8,69,C'ORIGINAL',WIDE=198                                       
         ACDEF H9,69,C'BILL AMT',WIDE=198                                       
         ACDEF H8,84,C'PAST DUE',WIDE=198                                       
         ACDEF H9,84,C' AMOUNT ',WIDE=198                                       
         ACDEF H8,100,C'CHECK ',WIDE=198                                        
         ACDEF H9,100,C'AMOUNT',WIDE=198                                        
         ACDEF H8,119,C'DETAIL',WIDE=198                                        
         ACDEF H8,137,C' COST OF',WIDE=198                                      
         ACDEF H9,137,C'FINANCING',WIDE=198                                     
         ACDEF H8,152,C' YEAR  ',WIDE=198                                       
         ACDEF H9,152,C'TO DATE',WIDE=198                                       
*                                                                               
*        SUMMARY REPORT W/ AGING COLS                                           
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H8,54,C'BILLING ',WIDE=198                                       
         ACDEF H8,67,C'  A/R   ',WIDE=198                                       
         ACDEF H9,67,C'BALANCE ',WIDE=198                                       
         ACDEF H8,78,C'PAST DUE',WIDE=198                                       
         ACDEF H9,78,C' AMOUNT ',WIDE=198                                       
         ACDEF H9,90,C'%',WIDE=198                                              
         ACDEF H8,99,C'--------------- AGING ---------------',WIDE=198          
         ACDEF H9,100,C'1-10',WIDE=198                                          
         ACDEF H9,111,C'11-30',WIDE=198                                         
         ACDEF H9,123,C'31-60',WIDE=198                                         
         ACDEF H9,133,C'OVER 60',WIDE=198                                       
         ACDEF H8,143,C' COST OF',WIDE=198                                      
         ACDEF H9,143,C'FINANCING',WIDE=198                                     
         ACDEF H8,154,C' YEAR  ',WIDE=198                                       
         ACDEF H9,154,C'TO DATE',WIDE=198                                       
*                                                                               
*        SUMMARY REPORT SUPPRESSING AGING COLS                                  
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H8,54,C'BILLING ',WIDE=198                                       
         ACDEF H8,67,C'  A/R   ',WIDE=198                                       
         ACDEF H9,67,C'BALANCE ',WIDE=198                                       
         ACDEF H8,78,C'PAST DUE',WIDE=198                                       
         ACDEF H9,78,C' AMOUNT ',WIDE=198                                       
         ACDEF H9,90,C'%',WIDE=198                                              
         ACDEF H8,94,C' COST OF',WIDE=198                                       
         ACDEF H9,94,C'FINANCING',WIDE=198                                      
         ACDEF H8,106,C' YEAR  ',WIDE=198                                       
         ACDEF H9,106,C'TO DATE',WIDE=198                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPCF01 05/31/90'                                      
         END                                                                    
