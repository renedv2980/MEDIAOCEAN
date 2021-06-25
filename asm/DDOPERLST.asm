*          DATA SET DDOPERLST  AT LEVEL 005 AS OF 01/08/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE OPERLSTA                                                                 
*                                                                               
OPERLIST CSECT                                                                  
*                                                                               
*** COMBINED LIST OF US/UK OPERATORS.                                           
*** SEE DSECT DDOPERLSTD                                                        
*                                                                               
         DC    AL2(OPERLST1-OPERLST0)   "BXLE TABLE"                            
         DC    AL4(OPERLSTX-1)                                                  
*                                                                               
OPERLST0 DC    0C                                                               
         DC    CL8'CHEA',CL30'COLIN HEATH'                                      
OPERLST1 DC    0C                                                               
         DC    CL8'CMAT',CL30'CLIVE MATHIAS'                                    
         DC    CL8'DSHA',CL30'DESMOND SHAW'                                     
         DC    CL8'DTIR',CL30'DOMINGO TIRADO'                                   
         DC    CL8'GACA',CL30'GIOVANNI ACANFORA'                                
         DC    CL8'JDOY',CL30'JONATHAN DOYLE'                                   
         DC    CL8'JGOO',CL30'JULIAN GOODRICH'                                  
         DC    CL8'MALE',CL30'MICHAEL ALEXANDER'                                
         DC    CL8'MHAY',CL30'MASUD HAYATH'                                     
         DC    CL8'PDOU',CL30'PAUL DOUGLAS'                                     
         DC    CL8'RDEF',CL30'RONNIE DEFREITAS'                                 
         DC    CL8'SSCO',CL30'STEVE SCOTT'                                      
         DC    CL8'TFRA',CL30'TREVOR FRANKLIN'                                  
*                                                                               
OPERLSTX DC    X'FFFFFFFF'                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DDOPERLST 01/08/20'                                      
         END                                                                    
