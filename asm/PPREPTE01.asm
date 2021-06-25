*          DATA SET PPREPTE01  AT LEVEL 005 AS OF 02/08/91                      
*PHASE PPTE01A,+0,NOAUTO                                                        
         TITLE 'PPREPTE01 - TEXACO INTERFACE'                                   
PPTE01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BILLS                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,ACTIVE                                                      
         FSPEC GET,PUBS                                                         
*                                                                               
         SPROG 0                                                                
         PSPEC H1,51,C'TEXACO PRINTPAK BILLING EXTRACT'                         
         PSPEC H2,51,C'-------------------------------'                         
*                                                                               
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H5,98,PAGE                                                       
*                                                                               
         PSPEC H2,1,MEDIA                                                       
         PSPEC H4,1,CLIENT                                                      
*                                                                               
         PSPEC H6,02,C'INVOICE'                                                 
         PSPEC H7,02,C'NUMBER  RUN DATE INV DATE DUE DATE PRD EST'              
         PSPEC H8,02,C'------- -------- -------- -------- --- ---'              
         PSPEC H6,45,C'INSERT'                                                  
         PSPEC H7,45,C' DATE  PUBLICATION'                                      
         PSPEC H8,45,C'------ -----------'                                      
         PSPEC H7,70,C'  GROSS AMOUNT      NET AMOUNT      CASH DISC.'          
         PSPEC H8,70,C'  ------------      ----------      ----------'          
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005PPREPTE01 02/08/91'                                      
         END                                                                    
