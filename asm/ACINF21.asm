*          DATA SET ACINF21    AT LEVEL 009 AS OF 03/05/92                      
*PHASE T60515A,*,NOAUTO                                                         
         TITLE 'MEDIA INTERFACE - RECORD TYPE MI'                               
T60515   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE MEDIA    IN ACCOUNTS INFO PROGRAM         
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60515)                                               
         DC    A(FILTABLE-T60515)                                               
         DC    A(PREHEADS-T60515)                                               
         DC    A(PRETABLE-T60515)                                               
         DC    A(HEADINGS-T60515)                                               
         DC    A(DATTABLE-T60515)                                               
         DC    A(KNTRYPNT-T60515)                                               
         DC    A(FNTRYPNT-T60515)                                               
         DC    A(DNTRYPNT-T60515)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'RECORDTYPE'                                                 
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'08'                                                            
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
         DC    CL10'MEDIA CODE'                                                 
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(2)                                                           
         DC    AL1(2)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*                                                                               
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
HEADINGS DC    CL39'MEDIA     DESCRIPTION   COMMISSION ACCT'                    
         DC    CL39'-----     -----------   ---------------'                    
         DC    CL39' '                                                          
         DC    CL39' '                                                          
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
         SPACE 1                                                                
DATTABLE DC    AL2(AKEY-GWS)       MEDIA CODE                                   
         DC    AL2(2)                                                           
         DC    AL1(2)                                                           
         DC    AL2(0)                                                           
         DC    AL1(2)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ACMI-GWS)       MEDIA INTERFACE ELEMENT                      
         DC    AL2(ACMIDESC-ACMID)                                              
         DC    AL1(L'ACMIDESC)                                                  
         DC    AL2(EDITCHAR-GWS)                                                
         DC    AL1(12)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ACMI-GWS)       MEDIA INTERFACE ELEMENT                      
         DC    AL2(ACMICOMM-ACMID)                                              
         DC    AL1(L'ACMICOMM)                                                  
         DC    AL2(EDITCHAR-GWS)                                                
         DC    AL1(26)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
FILTABLE DS    X'00'                                                            
KNTRYPNT DS    0H                                                               
FNTRYPNT DS    0H                                                               
DNTRYPNT DS    0H                                                               
*                                                                               
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACINF21   03/05/92'                                      
         END                                                                    
