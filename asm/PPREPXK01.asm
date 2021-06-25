*          DATA SET PPREPXK01  AT LEVEL 002 AS OF 05/24/91                      
*          DATA SET SPREPXK01  AT LEVEL 004 AS OF 11/05/85                      
*PHASE PPXK01A,+0                                                               
         TITLE 'PPXK01 - ADVERTISER FILE EXTRACT'                               
PPXK01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H1,57,C'ADVERTISER FILE EXTRACT'                                 
         SSPEC H2,57,C'-----------------------'                                 
         SSPEC H1,94,AGYNAME                                                    
         SSPEC H2,94,AGYADD                                                     
         SSPEC H4,25,C' **** INSERTED **** '                                    
         SSPEC H4,71,C' **** DELETED  **** '                                    
         SSPEC H5,14,C' GROSS       GROSS       NET              '              
         SSPEC H6,14,C' ORDERED     PAID        PAID     COUNT   '              
         SSPEC H7,14,C' ----------  ---------  --------- --------'              
         SSPEC H5,60,C' GROSS       GROSS       NET              '              
         SSPEC H6,60,C' ORDERED     PAID        PAID     COUNT   '              
         SSPEC H7,60,C' ----------  ---------  --------- --------'              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREPXK01 05/24/91'                                      
         END                                                                    
