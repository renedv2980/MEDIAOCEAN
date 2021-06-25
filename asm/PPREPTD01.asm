*          DATA SET PPREPTD01  AT LEVEL 001 AS OF 11/06/09                      
*PHASE PPTD01A,+0,NOAUTO                                                        
         TITLE 'PPREPTD01 - TAB DELIMITED INTERFACES'                           
PPTD01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BILLS                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,ACTIVE                                                      
         FSPEC GET,PUBS                                                         
*                                                                               
         SPROG 0                                                                
         XSPEC H1,52,C'TAB DELIMITED INTERFACE'                                 
         XSPEC H2,52,C'-----------------------'                                 
*                                                                               
         XSPEC H1,98,AGYNAME                                                    
         XSPEC H2,98,AGYADD                                                     
         XSPEC H4,98,REPORT                                                     
         XSPEC H5,98,PAGE                                                       
*                                                                               
         XSPEC H2,1,MEDIA                                                       
         XSPEC H4,1,CLIENT                                                      
*                                                                               
         XSPEC H6,02,C'INVOICE'                                                 
         XSPEC H7,02,C'NUMBER     RUN DATE INV DATE DUE DATE'                   
         XSPEC H8,02,C'-------    -------- -------- --------'                   
         XSPEC H6,42,C'INSER'                                                   
         XSPEC H7,41,C'-TIONS'                                                  
         XSPEC H8,41,C'------'                                                  
         XSPEC H7,48,C'PUBLICATION'                                             
         XSPEC H8,48,C'-----------'                                             
         XSPEC H7,66,C'   GROSS AMOUNT      NET AMOUNT      CASH DISC.'         
         XSPEC H8,66,C'   ------------      ----------      ----------'         
         XSPEC H7,120,C' AMT. DUE'                                              
         XSPEC H8,120,C' --------'                                              
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001PPREPTD01 11/06/09'                                      
         END                                                                    
