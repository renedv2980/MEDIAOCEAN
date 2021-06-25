*          DATA SET SPREPTD01  AT LEVEL 001 AS OF 11/06/09                      
*          DATA SET SPREPCI01  AT LEVEL 010 AS OF 09/12/05                      
*PHASE SPTD01A,+0,NOAUTO                                                        
         TITLE 'SPREPTD01 - CONTINENTAL AIRLINES INTERFACE'                     
SPTD01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BILLS                                                       
         FSPEC USE,SPTD03                                                       
*                                                                               
         SPROG 0,10                                                             
         XSPEC H1,48,C'TAB DELIMITED INTERFACE'                                 
         XSPEC H2,48,C'-----------------------'                                 
*                                                                               
         XSPEC H1,98,AGYNAME                                                    
         XSPEC H2,98,AGYADD                                                     
         XSPEC H4,98,REPORT                                                     
         XSPEC H5,98,PAGE                                                       
*                                                                               
         SPROG 10                                                               
         XSPEC H2,1,MEDIA                                                       
         XSPEC H4,1,CLIENT                                                      
*                                                                               
         XSPEC H6,02,C'INVOICE'                                                 
         XSPEC H7,02,C'NUMBER     RUN DATE INV DATE DUE DATE'                   
         XSPEC H8,02,C'-------    -------- -------- --------'                   
         XSPEC H6,42,C'SPOTS'                                                   
         XSPEC H7,41,C'/UNITS'                                                  
         XSPEC H8,41,C'------'                                                  
         XSPEC H7,48,C'STATION'                                                 
         XSPEC H8,48,C'-------'                                                 
*                                                                               
         SPROG 0,10                                                             
         XSPEC H7,66,C'   GROSS AMOUNT      NET AMOUNT      CASH DISC.'         
         XSPEC H8,66,C'   ------------      ----------      ----------'         
         XSPEC H7,120,C' AMT. DUE'                                              
         XSPEC H8,120,C' --------'                                              
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPTD01 11/06/09'                                      
         END                                                                    
