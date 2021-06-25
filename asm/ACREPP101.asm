*          DATA SET ACREPP101  AT LEVEL 031 AS OF 05/14/09                      
*PHASE ACP101A                                                                  
         TITLE 'SPECS FOR WORK-CODE REPORT'                                     
ACP101   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PTACNV,YES                                                       
*&&US*&& RSPEC MAXLINES,54                                                      
*&&UK                                                                           
         RSPEC MAXLINES,59                                                      
*&&                                                                             
         SPACE 1                                                                
         SPROG 0,1,2,3,4,5,6,7,8                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,57,C'REPORT'                                                  
         ASPEC H2,47,16C'-'                                                     
         ASPEC H1,80,REPORT                                                     
         ASPEC H1,98,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,80,REQUESTOR                                                  
         ASPEC F1,2,REQDETS                                                     
         SPACE 1                                                                
         SPROG 0,2,3,4,5                                                        
         ASPEC H5,2,C'CLIENT'                                                   
         ASPEC H5,80,C'PERIOD'                                                  
         SPACE 1                                                                
         SPROG 0,7                                                              
         ASPEC H8,34,C'----INVOICE----'                                         
         ASPEC H9,34,C'NUMBER     DATE'                                         
         ASPEC H8,54,C' NET         COMMISSION       GROSS '                    
         ASPEC H9,54,C'AMOUNT                        AMOUNT'                    
         ASPEC H8,95,C'------BILL-----'                                         
         ASPEC H9,95,C'NUMBER     DATE'                                         
         SPACE 1                                                                
         SPROG 2,3,4,5,8                                                        
         ASPEC H8,34,C'               '                                         
         ASPEC H9,34,C'               '                                         
         ASPEC H8,69,C'QUARTER TO-    QTD  '                                    
         ASPEC H9,69,C'DATE TOTAL   PERCENT'                                    
         ASPEC H8,90,C'    YEAR TO-      YTD  '                                 
         ASPEC H9,90,C'   DATE TOTAL   PERCENT'                                 
         SPACE 1                                                                
         SPROG 0,5                                                              
         ASPEC H9,2,C'WORK-CODE/SUPPLIER      '                                 
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H9,2,C'WORK-CODE               '                                 
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H9,2,C'SUPPLIER NUMBER AND NAME'                                 
         SPACE 1                                                                
         SPROG 4                                                                
         ASPEC H9,2,C'SUPPLIER/WORK-CODE      '                                 
         SPACE 1                                                                
         SPROG 7,8                                                              
         ASPEC H9,2,C'REPORT RECAP'                                             
         SPACE 1                                                                
         SPROG 1,6                                                              
         ASPEC H5,2,C'MEDIA SUMMARY'                                            
         ASPEC H8,64,68C' '                                                     
         ASPEC H9,64,68C' '                                                     
         ASPEC H8,2,C'MEDIA CODE AND NAME       '                               
         ASPEC H9,2,C'                          '                               
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H8,30,C' NET         COMMISSION       GROSS   '                  
         ASPEC H9,30,C'AMOUNT                        AMOUNT  '                  
         SPACE 1                                                                
         SPROG 6                                                                
         ASPEC H8,30,C'CURRENT      QUARTER TO-     YEAR TO- '                  
         ASPEC H9,30,C' MONTH       DATE TOTAL     DATE TOTAL'                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACREPP101 05/14/09'                                      
         END                                                                    
