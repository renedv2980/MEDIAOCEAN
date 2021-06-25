*          DATA SET ACINF02    AT LEVEL 002 AS OF 03/05/92                      
*PHASE T60502A,*,NOAUTO                                                         
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE AN/WO'                      
T60502   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE ANALYSIS IN ACCOUNTS INFO PROGRAM         
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60502)                                               
         DC    A(FILTABLE-T60502)                                               
         DC    A(PREHEADS-T60502)                                               
         DC    A(PRETABLE-T60502)                                               
         DC    A(HEADINGS-T60502)                                               
         DC    A(DATTABLE-T60502)                                               
         DC    A(KNTRYPNT-T60502)                                               
         DC    A(FNTRYPNT-T60502)                                               
         DC    A(DNTRYPNT-T60502)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'RECORDTYPE'                                                 
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'0A'                                                            
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
         DC    CL10'UNIT'                                                       
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'00'                                                            
         DC    AL1(3)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS+1)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ANALYSIS'                                                   
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(4)                                                           
         DC    AL1(2)                                                           
         DC    AL2(SCANBLCK-GWS+2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*                   FILTER TABLE COVERED BY DSECT FILTERSD                      
         SPACE 1                                                                
FILTABLE DC    CL10'DELETED'                                                    
         DC    CL2'DE'                                                          
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(ACSTATUS-ACKEYD)                                             
         DC    AL1(L'ACSTATUS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    CL2'UN'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    CL2'LE'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(3)                                                           
         DC    AL1(1)                                                           
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
HEADINGS DC    CL39'UNIT LEDGER ------ANALYSIS------       '                    
         DC    CL39'CODE  CODE  CODE DESCRIPTION           '                    
         DC    CL39' UNIT LEDGER ------ANALYSIS------      '                    
         DC    CL39' CODE  CODE  CODE DESCRIPTION          '                    
*                                                                               
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
         SPACE 1                                                                
DATTABLE DC    AL2(AKEY-GWS)       UNIT CODE                                    
         DC    AL2(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL1(3)                                                           
         DC    AL1(43)                                                          
*                                                                               
         DC    AL2(AKEY-GWS)       LEDGER CODE                                  
         DC    AL2(3)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL1(9)                                                           
         DC    AL1(49)                                                          
*                                                                               
         DC    AL2(AANA-GWS)       ANALYSIS CODE                                
         DC    AL2(ACANCODE-ACANALD)                                            
         DC    AL1(L'ACANCODE)                                                  
         DC    AL2(0)                                                           
         DC    AL1(15)                                                          
         DC    AL1(55)                                                          
*                                                                               
         DC    AL2(AANA-GWS)       ANALYSIS DESCRIPTION                         
         DC    AL2(ACANDESC-ACANALD)                                            
         DC    AL1(L'ACANDESC)                                                  
         DC    AL2(EDITCHAR-GWS)                                                
         DC    AL1(19)                                                          
         DC    AL1(59)                                                          
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
**PAN#1  DC    CL21'002ACINF02   03/05/92'                                      
         END                                                                    
