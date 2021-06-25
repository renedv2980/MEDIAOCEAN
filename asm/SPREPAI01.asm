*          DATA SET SPREPAI01  AT LEVEL 002 AS OF 07/21/16                      
*PHASE SPAI01A,+0                                                               
         TITLE 'SPAI01 - AT&&T INTERFACE HEADLINES'                             
SPAI01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         FSPEC USE,SPAI03                                                       
*                                                                               
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,90                                                          
         SSPEC H1,47,CL27'AT&&T SPOT BILLING INTERFACE'                         
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,47,27C'-'                                                     
         SPROG 20,30,110                                                        
         SSPEC H1,47,CL26'AT&&T NET BILLING INTERFACE'                          
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,47,26C'-'                                                     
         SPROG 0,10,20,30,90,110                                                
         PSPEC H2,95,AGYADD                                                     
         PSPEC H3,95,REPORT                                                     
         PSPEC H3,120,PAGE                                                      
*                                                                               
         PSPEC H6,1,C'                                        '                 
         PSPEC H7,1,C'   MED CLT      CLIENT NAME     PRD EST '                 
         PSPEC H8,1,C'   --- --- -------------------- --- --- '                 
*                                                                               
         PSPEC H6,41,C'  INVOICE    INVOICE     INVOICE     '                   
         PSPEC H7,41,C'  NUMBER      DATE       AMOUNT      '                   
         PSPEC H8,41,C'---------- ---------- -------------- '                   
*                                                                               
         PSPEC H6,78,C'                                 CMA '                   
         PSPEC H7,78,C'           UCOMM E4              DMA '                   
         PSPEC H8,78,C'-------------------------------- ----'                   
*                                                                               
         SPROG 0,20                                                             
         PSPEC H3,05,CL15'ERRORS ON INPUT'                                      
         PSPEC H4,05,15C'-'                                                     
***      SPROG 10,30                                                            
***      PSPEC H3,05,CL21'*FINAL OUTPUT LISTING'                                
***      PSPEC H4,05,21C'-'                                                     
         SPROG 90,110                                                           
         PSPEC  H9,8,C'RECORD TYPE            TOTAL'                            
         PSPEC H10,8,C'-----------            -----'                            
         DC    X'00'                                                            
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPAI01 07/21/16'                                      
         END                                                                    
