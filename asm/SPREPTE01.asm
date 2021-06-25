*          DATA SET SPREPTE01  AT LEVEL 017 AS OF 08/29/00                      
*PHASE SPTE01A                                                                  
         TITLE 'SPREPTE01 - TEXACO INTERFACE'                                   
SPTE01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP0003                                                       
*                                                                               
         SPROG 0                                                                
         SSPEC H1,51,C'TEXACO SPOTPAK BILLING EXTRACT'                          
         SSPEC H2,51,C'------------------------------'                          
         SSPEC H3,50,PERIOD                                                     
*                                                                               
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,98,REPORT                                                     
         SSPEC H5,98,PAGE                                                       
*                                                                               
         SSPEC H2,1,MEDIA                                                       
         SSPEC H4,1,CLIENT                                                      
*                                                                               
         SSPEC H6,02,C'INVOICE'                                                 
         SSPEC H7,02,C'NUMBER  RUN DATE INV DATE DUE DATE PRD EST'              
         SSPEC H8,02,C'------- -------- -------- -------- --- ---'              
         SSPEC H6,45,C'BILLNG'                                                  
         SSPEC H7,45,C'PERIOD MKT  STATN '                                      
         SSPEC H8,45,C'------ ---- ------'                                      
         SSPEC H7,65,C'  GROSS AMOUNT      NET AMOUNT '                         
         SSPEC H8,65,C'  ------------      ---------- '                         
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPREPTE01 08/29/00'                                      
         END                                                                    
         SSPEC H6,44,C'BILLNG'                                                  
         SSPEC H7,44,C'PERIOD MKT  STATN '                                      
         SSPEC H8,44,C'------ ---- ------'                                      
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
         END                                                                    
