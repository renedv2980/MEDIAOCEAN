*          DATA SET ACINF01    AT LEVEL 002 AS OF 03/05/92                      
*PHASE T60501A,*,NOAUTO                                                         
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE ME'                         
T60501   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE MEDIA    IN ACCOUNTS INFO PROGRAM         
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60501)                                               
         DC    A(FILTABLE-T60501)                                               
         DC    A(PREHEADS-T60501)                                               
         DC    A(PRETABLE-T60501)                                               
         DC    A(HEADINGS-T60501)                                               
         DC    A(DATTABLE-T60501)                                               
         DC    A(KNTRYPNT-T60501)                                               
         DC    A(FNTRYPNT-T60501)                                               
         DC    A(DNTRYPNT-T60501)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'RECORDTYPE'                                                 
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'09'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'MEDIA'                                                      
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DS    0C                                                               
         DC    CL10'ANALYSIS'                                                   
         DC    CL2'AN'                                                          
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AMED-GWS)                                                    
         DC    AL2(ACMDANAL-ACMEDIAD)                                           
         DC    AL1(L'ACMDANAL)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DELETED'                                                    
         DC    CL2'DE'                                                          
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(ACSTATUS-ACKEYD)                                             
         DC    AL1(L'ACSTATUS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
         EJECT                                                                  
*              PRE-HEADING LINE - TO CONTAIN CONSTANT ELEMENTS OF               
*                                 EXPRESSIONS IN FORM X=Y,A=B                   
*                                                                               
*              CL39      C                      SCREEN COLS  2-40               
*              CL39      C                      SCREEN COLS 41-79               
*                                                                               
PREHEADS DC    CL39'                                       '                    
         DC    CL39'                                       '                    
*                                                                               
*              PRE-HEADING DATA TABLE -  9 BYTE ENTRIES                         
*                                                                               
*              CONTENTS AS FOR DISPLAY DATA TABLE BELOW                         
*                                                                               
PRETABLE DC    X'FF'               END OF PRE-HEADING DATA TABLE                
*                                                                               
*              SCREEN HEADINGS - 2 LINES                                        
*                                                                               
*              CL39      C         FIRST LINE - SCREEN COLS  2-40               
*              CL39      C         2ND   LINE -                                 
*              CL39      C         FIRST LINE - SCREEN COLS 41-79               
*              CL39      C         2ND   LINE                                   
*                                                                               
*&&UK                                                                           
HEADINGS DC    CL39'------MEDIA------     COMMISSION      V'                    
         DC    CL39'CODE  DESCRIPTION     ACCOUNT         A'                    
         DC    CL39'AT             ---BILL NUMBERS---  ANLS'                    
         DC    CL39'CCOUNT         FIRST  LAST  RESET  CODE'                    
*&&                                                                             
*&&US                                                                           
HEADINGS DC    CL39'------MEDIA------     COMMISSION'                           
         DC    CL39'CODE  DESCRIPTION     ACCOUNT'                              
         DC    CL39'               ---BILL NUMBERS---  ANLS'                    
         DC    CL39'               FIRST  LAST  RESET  CODE'                    
*&&                                                                             
*                                                                               
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
         SPACE 1                                                                
DATTABLE DC    AL2(AMED-GWS)       MEDIA CODE                                   
         DC    AL2(ACMDCODE-ACMEDIAD)                                           
         DC    AL1(L'ACMDCODE)                                                  
         DC    AL2(0)                                                           
         DC    AL1(3)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AMED-GWS)       MEDIA DESCRIPTION                            
         DC    AL2(ACMDDESC-ACMEDIAD)                                           
         DC    AL1(L'ACMDDESC)                                                  
         DC    AL2(EDITCHAR-GWS)                                                
         DC    AL1(8)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AMED-GWS)       COMMISSION A/C FOR BILLS                     
         DC    AL2(ACMDCOMM-ACMEDIAD+1)                                         
         DC    AL1(L'ACMDCOMM-1)                                                
         DC    AL2(0)                                                           
         DC    AL1(24)                                                          
         DC    AL1(0)                                                           
*                                                                               
*&&UK                                                                           
         DC    AL2(AMED-GWS)       VAT A/C FOR BILLS                            
         DC    AL2(ACMDVTAC-ACMEDIAD+1)                                         
         DC    AL1(L'ACMDVTAC-1)                                                
         DC    AL2(0)                                                           
         DC    AL1(40)                                                          
         DC    AL1(0)                                                           
*&&                                                                             
*                                                                               
         DC    AL2(AMED-GWS)       FIRST BILL NUMBER                            
         DC    AL2(ACMDFBIL-ACMEDIAD)                                           
         DC    AL1(L'ACMDFBIL)                                                  
         DC    AL2(0)                                                           
         DC    AL1(56)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AMED-GWS)       LAST BILL NUMBER                             
         DC    AL2(ACMDLBIL-ACMEDIAD)                                           
         DC    AL1(L'ACMDLBIL)                                                  
         DC    AL2(0)                                                           
         DC    AL1(63)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AMED-GWS)       RESET BILL NUMBER                            
         DC    AL2(ACMDRSET-ACMEDIAD)                                           
         DC    AL1(L'ACMDRSET)                                                  
         DC    AL2(0)                                                           
         DC    AL1(70)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AMED-GWS)                                                    
         DC    AL2(ACMDANAL-ACMEDIAD)                                           
         DC    AL1(L'ACMDANAL)                                                  
         DC    AL2(0)                                                           
         DC    AL1(77)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
KNTRYPNT DS    0H                                                               
FNTRYPNT DS    0H                                                               
DNTRYPNT DS    0H                                                               
*                                                                               
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACINF01   03/05/92'                                      
         END                                                                    
