*          DATA SET SPREPCI01  AT LEVEL 010 AS OF 09/12/05                      
*PHASE SPCI01A,+0,NOAUTO                                                        
         TITLE 'SPREPCI01 - CONTINENTAL AIRLINES INTERFACE'                     
SPCI01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BILLS                                                       
         FSPEC USE,SPCI03                                                       
*                                                                               
         SPROG 0,10                                                             
         XSPEC H1,46,C'CONTINENTAL BILLING EXTRACT'                             
         XSPEC H2,46,C'---------------------------'                             
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
**PAN#1  DC    CL21'010SPREPCI01 09/12/05'                                      
         END                                                                    
