*          DATA SET ACINF07    AT LEVEL 010 AS OF 02/11/97                      
*PHASE T60507A,*,NOAUTO                                                         
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE PR'                         
T60507   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE  PRODUCT IN ACCOUNTS INFO PROGRAM         
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60507)                                               
         DC    A(FILTABLE-T60507)                                               
         DC    A(PREHEADS-T60507)                                               
         DC    A(PRETABLE-T60507)                                               
         DC    A(HEADINGS-T60507)                                               
         DC    A(DATTABLE-T60507)                                               
         DC    A(KNTRYPNT-T60507)                                               
         DC    A(FNTRYPNT-T60507)                                               
         DC    A(DNTRYPNT-T60507)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(PRODUNIT-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(PRODLEDG-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'BILLGROUP'                                                  
         DC    CL2'BG'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(APRO-GWS)                                                    
         DC    AL2(ACPRGRUP-ACPROFD)                                            
         DC    AL1(L'ACPRGRUP)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'COSTING'                                                    
         DC    CL2'CO'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(APRO-GWS)                                                    
         DC    AL2(ACPRCOST-ACPROFD)                                            
         DC    AL1(12)                                                          
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'FILTER1'                                                    
         DC    CL2'F1'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'FILTER2'                                                    
         DC    CL2'F2'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'FILTER3'                                                    
         DC    CL2'F3'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(2)                                                           
         DC    AL1(L'ACSTANAL)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'SUBCOMPANY'                                                 
         DC    CL2'SC'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(3)                                                           
         DC    AL1(L'ACSTSUB)                                                   
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ANALYSIS'                                                   
         DC    CL2'AN'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(ACSTCOST-ACSTATD)                                            
         DC    AL1(L'ACSTCOST)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEVEL'                                                      
         DC    CL2'LE'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ALEVEL-GWS)                                                  
         DC    AL2(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEVEL'                                                      
         DC    CL2'LE'                                                          
         DC    CL8'1-2'                                                         
         DC    X'04'                                                            
         DC    AL2(ALEVEL-GWS)                                                  
         DC    AL2(3)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'MEDIA'                                                      
         DC    CL2'ME'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(0)                                                           
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'RECEIVABLE'                                                 
         DC    CL2'RE'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(APRO-GWS)                                                    
         DC    AL2(ACPRRECV-ACPROFD+3)                                          
         DC    AL1(12)                                                          
         DC    AL2(0)                                                           
*                                                                               
*&&US                                                                           
         DC    CL10'SALESANAL'     (STORED IN PROFILE EL BY ROOT)               
         DC    CL2'SA'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(APRO-GWS)                                                    
         DC    AL2(ACPRUNBL-ACPROFD)                                            
         DC    AL1(12)                                                          
         DC    AL2(0)                                                           
*&&                                                                             
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'CLOSED'                                                      
         DC    X'40'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(ACSTSTAT-ACSTATD)                                            
         DC    AL1(L'ACSTSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'LOCKED'                                                      
         DC    X'20'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(ACSTSTAT-ACSTATD)                                            
         DC    AL1(L'ACSTSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
*&&UK                                                                           
         DC    CL10'UFORA'                                                      
         DC    CL2'UA'                                                          
*&&                                                                             
*&&US                                                                           
         DC    CL10'OFFICE'                                                     
         DC    CL2'OF'                                                          
*&&                                                                             
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(APRO-GWS)                                                    
         DC    AL2(ACPROFFC-ACPROFD)                                            
         DC    AL1(L'ACPROFFC)                                                  
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
PREHEADS DC    CL39'UNIT=                                / '                    
         DC    CL39'LEDGER=                                '                    
*                                                                               
*              PRE-HEADING DATA TABLE -  9 BYTE ENTRIES                         
*                                                                               
*              CONTENTS AS FOR DISPLAY DATA TABLE BELOW                         
*                                                                               
PRETABLE DC    AL2(AKEY-GWS)       UNIT CODE                                    
         DC    AL2(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL1(7)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AUNITNAM-GWS)   UNIT NAME                                    
         DC    AL2(0)                                                           
         DC    AL1(29)                                                          
         DC    AL2(0)                                                           
         DC    AL1(9)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AKEY-GWS)       LEDGER CODE                                  
         DC    AL2(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL1(48)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ALEDGNAM-GWS)   LEDGER NAME                                  
         DC    AL2(0)                                                           
         DC    AL1(30)                                                          
         DC    AL2(0)                                                           
         DC    AL1(50)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF PRE-HEADING DATA TABLE                
*                                                                               
*              SCREEN HEADINGS - 2 LINES                                        
*                                                                               
*              CL39      C         FIRST LINE - SCREEN COLS  2-40               
*              CL39      C         2ND   LINE -                                 
*              CL39      C         FIRST LINE - SCREEN COLS 41-79               
*              CL39      C         2ND   LINE                                   
*                                                                               
*&&UK                                                                           
HEADINGS DC    CL39'ACCOUNT CODE BG  RECEIVABLE     COSTING'                    
         DC    CL39'------------ --  ACCOUNT        ACCOUNT'                    
         DC    CL39'        UA'                                                 
         DC    CL39'        --'                                                 
*&&                                                                             
*&&US                                                                           
HEADINGS DC    CL39'ACCOUNT CODE BG   RECEIVBLE     COSTING'                    
         DC    CL39'------------ --   ACCOUNT       ACCOUNT'                    
         DC    CL39'          SALES              OF'                            
         DC    CL39'          ACCOUNT            --'                            
*&&                                                                             
*                                                                               
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
         SPACE 1                                                                
DATTABLE DC    AL2(AKEY-GWS)       ACCOUNT CODE                                 
         DC    AL2(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(0)                                                           
         DC    AL1(2)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(APRO-GWS)       BILL GROUP                                   
         DC    AL2(ACPRGRUP-ACPROFD)                                            
         DC    AL1(L'ACPRGRUP)                                                  
         DC    AL2(0)                                                           
         DC    AL1(15)                                                          
         DC    AL1(0)                                                           
*&&UK                                                                           
*                                                                               
         DC    AL2(APRO-GWS)       RECEIVABLE CODE                              
         DC    AL2(ACPRRECV-ACPROFD+3)                                          
         DC    AL1(7)                                                           
         DC    AL2(0)                                                           
         DC    AL1(19)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(APRO-GWS)       COSTING CODE                                 
         DC    AL2(ACPRCOST-ACPROFD)                                            
         DC    AL1(7)                                                           
         DC    AL2(0)                                                           
         DC    AL1(40)                                                          
         DC    AL1(0)                                                           
*&&                                                                             
*&&US                                                                           
         DC    AL2(APRO-GWS)                                                    
         DC    AL2(ACPRRECV-ACPROFD+3)                                          
         DC    AL1(12)                                                          
         DC    AL2(0)                                                           
         DC    AL1(20)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(APRO-GWS)                                                    
         DC    AL2(ACPRCOST-ACPROFD)                                            
         DC    AL1(12)                                                          
         DC    AL2(0)                                                           
         DC    AL1(34)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(APRO-GWS)                                                    
         DC    AL2(ACPRUNBL-ACPROFD)                                            
         DC    AL1(12)                                                          
         DC    AL2(0)                                                           
         DC    AL1(51)                                                          
         DC    AL1(0)                                                           
*&&                                                                             
*                                                                               
         DC    AL2(APRO-GWS)       UNIT FOR ANALYSIS/OFFICE                     
         DC    AL2(ACPROFFC-ACPROFD)                                            
         DC    AL1(L'ACPROFFC)                                                  
         DC    AL2(0)                                                           
         DC    AL1(70)                                                          
         DC    AL1(0)                                                           
*                                                                               
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
KNTRYPNT DS    0H                                                               
DNTRYPNT DS    0H                                                               
         EJECT                                                                  
FNTRYPNT DS    0D                  GET JOB MEDIA CODE FROM KEY (FILTER)         
         NMOD1 4,**PROD**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60507,RB                                                        
         USING T605TWA,RA                                                       
         LA    R5,SAVEHIER                                                      
         USING ACHEIRD,R5                                                       
         ZIC   R4,ACHRLEVB                                                      
         L     RE,ADRIO                                                         
         LA    RE,3(RE)                                                         
         AR    R4,RE                                                            
         MVC   WORK(1),0(R4)                                                    
         LA    R2,WORK                                                          
*                                                                               
         XIT1  REGS=(R2,R3)                                                     
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACINF07   02/11/97'                                      
         END                                                                    
