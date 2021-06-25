*          DATA SET ACINF20    AT LEVEL 019 AS OF 03/05/92                      
*PHASE T60514A,*,NOAUTO                                                         
         TITLE 'RECEIVABLE STATEMENT LIST - RECORD TYPE RS'                     
T60514   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE MEDIA    IN ACCOUNTS INFO PROGRAM         
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60514)                                               
         DC    A(FILTABLE-T60514)                                               
         DC    A(PREHEADS-T60514)                                               
         DC    A(PRETABLE-T60514)                                               
         DC    A(HEADINGS-T60514)                                               
         DC    A(DATTABLE-T60514)                                               
         DC    A(KNTRYPNT-T60514)                                               
         DC    A(FNTRYPNT-T60514)                                               
         DC    A(DNTRYPNT-T60514)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'RECORDTYPE'                                                 
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'2D'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'SUBCOMPANY'                                                 
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'02'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'FORMAT'                                                     
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(ACCSCDE-ACCSKEY)                                             
         DC    AL1(L'ACCSCDE)                                                   
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
HEADINGS DC    CL39'FORMAT    NAME'                                             
         DC    CL39'--------  -----------------------------'                    
         DC    CL39' '                                                          
         DC    CL39'-------'                                                    
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
         SPACE 1                                                                
DATTABLE DC    AL2(AKEY-GWS)       FORMAT CODE                                  
         DC    AL2(ACCSCDE-ACCSKEY)                                             
         DC    AL1(L'ACCSCDE)                                                   
         DC    AL2(0)                                                           
         DC    AL1(2)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ANAM-GWS)       FORMAT NAME                                  
         DC    AL2(ACNMNAME-ACNAMED)                                            
         DC    AL1(L'ACNMNAME)                                                  
         DC    AL2(EDITNAME-GWS)                                                
         DC    AL1(12)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
FILTABLE DC    X'00'                                                            
KNTRYPNT DS    0H                                                               
FNTRYPNT DS    0H                                                               
DNTRYPNT DS    0H                                                               
*                                                                               
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACINF20   03/05/92'                                      
         END                                                                    
