*          DATA SET PPREPJX01  AT LEVEL 027 AS OF 07/18/16                      
*PHASE PPJX01A                                                                  
         TITLE 'PPJX01  CLT/PRD/EST     LISTING HEADLINES'                      
PPJX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC READ,ESTIMATES                                                   
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,20,30                                                       
         PSPEC H1,33,C'J WALTER THOMPSON CONVERSION'                            
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H1,76,AGYNAME                                                    
         PSPEC H2,33,31C'-'                                                     
         PSPEC H2,76,AGYADD                                                     
         PSPEC H4,76,RUN                                                        
         PSPEC H5,1,REQUESTOR                                                   
         PSPEC H5,76,REPORT                                                     
         PSPEC H5,101,PAGE                                                      
         PSPEC H6,2,C'CLT'                                                      
         PSPEC H7,2,C'---'                                                      
         PSPEC H6,6,C'PRD'                                                      
         PSPEC H7,6,C'---'                                                      
         PSPEC H6,11,C'OLD BILL NAME'                                           
         PSPEC H7,11,C'-------------'                                           
         PSPEC H6,51,C'NEW BILL NAME'                                           
         PSPEC H7,51,C'-------------'                                           
         DC    X'00'                                                            
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027PPREPJX01 07/18/16'                                      
         END                                                                    
