*          DATA SET PPREPAI01  AT LEVEL 002 AS OF 10/21/16                      
*PHASE PPAI01A,+0                                                               
         TITLE 'PPAI01 - AT&&T INTERFACE HEADLINES'                             
PPAI01   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
*                                                                               
         SPROG 100                                                              
         PSPEC H1,47,CL28'AT&&T PRINT BILLING INTERFACE'                        
         PSPEC H1,95,AGYNAME                                                    
         PSPEC H2,47,28C'-'                                                     
         PSPEC H2,95,AGYADD                                                     
         PSPEC H3,50,PERIOD                                                     
         PSPEC H3,95,RUN                                                        
         PSPEC H4,95,REPORT                                                     
         PSPEC H4,120,PAGE                                                      
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
         SPROG 1                                                                
         PSPEC H3,05,CL15'ERRORS ON INPUT'                                      
         PSPEC H4,05,15C'-'                                                     
*                                                                               
         SPROG 90                                                               
         PSPEC  H9,8,C'RECORD TYPE            TOTAL'                            
         PSPEC H10,8,C'-----------            -----'                            
         DC    X'00'                                                            
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREPAI01 10/21/16'                                      
         END                                                                    
