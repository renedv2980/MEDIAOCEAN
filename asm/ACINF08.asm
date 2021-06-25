*          DATA SET ACINF08    AT LEVEL 004 AS OF 01/24/97                      
*PHASE T60508A,*,NOAUTO                                                         
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE XP'                         
T60508   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE XP  IN ACCOUNTS INFO PROGRAM              
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60508)                                               
         DC    A(FILTABLE-T60508)                                               
         DC    A(PREHEADS-T60508)                                               
         DC    A(PRETABLE-T60508)                                               
         DC    A(HEADINGS-T60508)                                               
         DC    A(DATTABLE-T60508)                                               
         DC    A(KNTRYPNT-T60508)                                               
         DC    A(FNTRYPNT-T60508)                                               
         DC    A(DNTRYPNT-T60508)                                               
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
FILTABLE DC    CL10'DELETED'                                                    
         DC    CL2'DE'                                                          
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(ACSTATUS-ACKEYD)                                             
         DC    AL1(L'ACSTATUS)                                                  
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
*&&US                                                                           
         DC    CL10'CASHDISCNT'                                                 
         DC    CL2'CD'                                                          
         DC    CL8'NO'                                                          
         DC    C'N'                                                             
         DC    AL2(AXPR-GWS)                                                    
         DC    AL2(ACXPCD-ACXPROFD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*&&                                                                             
*                                                                               
         DC    CL10'DETAIL'                                                     
         DC    CL2'DE'                                                          
         DC    CL8'NO'                                                          
         DC    C'N'                                                             
         DC    AL2(AXPR-GWS)                                                    
         DC    AL2(ACXPDET-ACXPROFD)                                            
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ESTCOMM'                                                    
         DC    CL2'EC'                                                          
         DC    CL8'NO'                                                          
         DC    X'40'                                                            
         DC    AL2(AXPR-GWS)                                                    
         DC    AL2(ACXPST1-ACXPROFD)                                            
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ESTDETAIL'                                                  
         DC    CL2'ED'                                                          
         DC    CL8'YES'                                                         
         DC    C'Y'                                                             
         DC    AL2(AXPR-GWS)                                                    
         DC    AL2(ACXPEDET-ACXPROFD)                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ESTIMATE'                                                   
         DC    CL2'ES'                                                          
         DC    CL8'NO'                                                          
         DC    C'N'                                                             
         DC    AL2(AXPR-GWS)                                                    
         DC    AL2(ACXPEST-ACXPROFD)                                            
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ESTIMATE'                                                   
         DC    CL2'ES'                                                          
         DC    CL8'UNAPPRVD'                                                    
         DC    X'04'                                                            
         DC    AL2(AXPR-GWS)                                                    
         DC    AL2(ACXPST1-ACXPROFD)                                            
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ETA'                                                        
         DC    CL2'ET'                                                          
         DC    CL8'NO'                                                          
         DC    X'80'                                                            
         DC    AL2(AXPR-GWS)                                                    
         DC    AL2(ACXPST1-ACXPROFD)                                            
         DC    AL1(1)                                                           
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
         DC    CL10'JOBS'                                                       
         DC    CL2'JO'                                                          
         DC    CL8'UNAPPRVD'                                                    
         DC    X'02'                                                            
         DC    AL2(AXPR-GWS)                                                    
         DC    AL2(ACXPST1-ACXPROFD)                                            
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
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
*&&US                                                                           
         DC    CL10'PAY'                                                        
         DC    CL2'PA'                                                          
         DC    CL8'NET'                                                         
         DC    C'Y'                                                             
         DC    AL2(AXPR-GWS)                                                    
         DC    AL2(ACXPNET-ACXPROFD)                                            
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*&&                                                                             
*                                                                               
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
         DC    CL10'SUBCOMPANY'                                                 
         DC    CL2'SC'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(3)                                                           
         DC    AL1(L'ACSTSUB)                                                   
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'SUMMARY'                                                    
         DC    CL2'SU'                                                          
         DC    CL8'NO'                                                          
         DC    C'N'                                                             
         DC    AL2(AXPR-GWS)                                                    
         DC    AL2(ACXPSUM-ACXPROFD)                                            
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
HEADINGS DC    CL39'ACCOUNT CODE       OVER   UNDER   PROFI'                    
         DC    CL39'------------      -PCNT- --CASH-- -----'                    
*&&                                                                             
*&&US                                                                           
HEADINGS DC    CL39'ACCOUNT CODE DUE   OVER   UNDER   PROFI'                    
         DC    CL39'------------ DAYS -PCNT- --CASH-- -----'                    
*&&                                                                             
         DC    CL39'LE INFORMATION                         '                    
         DC    CL39'--------------                         '                    
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
*&&US                                                                           
         DC    AL2(AXPR-GWS)       DUE DAYS                                     
         DC    AL2(ACXPDUE-ACXPROFD)                                            
         DC    AL1(L'ACXPDUE)                                                   
         DC    X'FF00'                                                          
         DC    AL1(15)                                                          
         DC    AL1(0)                                                           
*&&                                                                             
*                                                                               
         DC    AL2(AXPR-GWS)       OVER PER CENT                                
         DC    AL2(ACXPOVER-ACXPROFD)                                           
         DC    AL1(L'ACXPOVER)                                                  
         DC    X'FF00'                                                          
         DC    AL1(20)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AXPR-GWS)       UNDER CASH                                   
         DC    AL2(ACXPLOW-ACXPROFD)                                            
         DC    AL1(L'ACXPLOW)                                                   
         DC    X'FF00'                                                          
         DC    AL1(27)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AXPR-GWS)       PROFILE INFORMATION                          
         DC    AL2(ACXPSUM-ACXPROFD)                                            
         DC    AL1(5)                                                           
         DC    X'FF00'                                                          
         DC    AL1(36)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
FNTRYPNT DS    0D                  GET JOB MEDIA CODE FROM KEY (FILTER)         
         NMOD1 0,**XPRO**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60508,RB                                                        
         USING T605TWA,RA                                                       
         LA    R5,SAVEHIER                                                      
         USING ACHEIRD,R5                                                       
         ZIC   R4,ACHRLEVB                                                      
         L     RE,ADRIO                                                         
         LA    RE,3(RE)                                                         
         AR    R4,RE                                                            
         MVC   WORK(1),0(R4)                                                    
         LA    R2,WORK                                                          
         XIT1  REGS=(R2)                                                        
         DROP  R5                                                               
         EJECT                                                                  
DNTRYPNT DS    0D                                                               
         NMOD1 20,**XPRO**                                                      
         USING WRKAREA,RC                                                       
         L     R8,0(R1)                                                         
         USING GWS,R8              R8 = GLOBAL W/S                              
         L     RB,APHASE                                                        
         USING T60508,RB                                                        
         L     R9,AXPR                                                          
         USING ACXPROFD,R9         R9 = EXTRA PROFILE ELEMENT                   
*                                                                               
*                                  WHICH FIELD TO BE EDITTED ?                  
*                                  (R2 CONTAINS FIELD ADDRESS)                  
         LA    R1,ACXPDUE                                                       
         CR    R1,R2                                                            
         BE    T608DUE             -DUE DAYS                                    
         LA    R1,ACXPOVER                                                      
         CR    R1,R2                                                            
         BE    T608OVER            -OVER PER CENT                               
         LA    R1,ACXPLOW                                                       
         CR    R1,R2                                                            
         BE    T608NDER            -UNDER CASH                                  
         DS    0H                  FOR PROFILE INFO MAKE ELEMENT FIXEDL         
         XC    WORK+140(24),WORK+140                                            
         ZIC   R7,1(R9)                                                         
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   WORK+140(0),0(R9)                                                
         LA    R9,WORK+140                                                      
         B     T608PROF                                                         
*                                                                               
T608XIT  LA    R2,WORK+20                                                       
         MVI   DMCB,0              SET FOR NO ERROR                             
         XIT1  REGS=(R2,R4)        RETURN ADDRESS AND LENGTH IN R2 & R3         
*                                  AND NUMBER OF EXTRA LINES IN R4              
         SPACE 2                                                                
*                                  EDIT DUE DAYS - US ONLY                      
T608DUE  EDIT  (P2,ACXPDUE),(3,WORK+20)                                         
         LA    R3,3                                                             
         B     T608XIT                                                          
*                                  EDIT OVER PER CENT                           
T608OVER EDIT  (P3,ACXPOVER),(6,WORK+20),2                                      
         LA    R3,6                                                             
         B     T608XIT                                                          
*                                  EDIT UNDER CASH                              
T608NDER EDIT  (P4,ACXPLOW),(8,WORK+20),2                                       
         LA    R3,8                                                             
         B     T608XIT                                                          
*                                  EDIT PROFILE INFORMATION                     
T608PROF MVI   UNSCANBK,C' '                                                    
         MVC   UNSCANBK+1(159),UNSCANBK                                         
         MVC   WORK+20(110),UNSCANBK                                            
         LA    R1,XPTAB                                                         
         LA    R2,UNSCANBK                                                      
         SR    R4,R4                                                            
         ICM   R3,15,AJOB          IS IT A JOB                                  
         BZ    TP0                 NO - BRANCH                                  
         ICM   R3,15,ASTA          JOB START DATE                               
         BZ    TP0                                                              
         USING ACSTATD,R3                                                       
         MVC   0(5,R2),=C'START'                                                
         GOTO1 VDATCON,DMCB,(1,ACSTBFDT),(8,10(R2))                             
*&&UK*&& OI    10(R2),C'0'                                                      
         LA    R4,1(R4)                                                         
         LA    R2,L'UNSCANBK(R2)                                                
         LA    R1,XPTAB                                                         
         SPACE 1                                                                
TP0      LA    R3,ACXPSUM                                                       
TP1      CLI   0(R1),0                                                          
         BE    TP3                                                              
         CLC   0(1,R3),0(R1)                                                    
         BNE   TP2                                                              
         MVC   0(9,R2),1(R1)       KEYWORD                                      
         MVC   10(3,R2),10(R1)     DISPLAY VALUE                                
         LA    R4,1(R4)                                                         
         LA    R2,L'UNSCANBK(R2)                                                
TP2      LA    R1,L'XPTAB(R1)                                                   
         LA    R3,1(R3)                                                         
         B     TP1                                                              
         SPACE 1                                                                
TP3      LA    R1,XPTAB1           ACXPST1 DETAILS                              
TP4      CLI   0(R1),0                                                          
         BE    TPEND                                                            
         ZIC   R5,0(R1)                                                         
         EX    R5,*+8                                                           
         B     *+8                                                              
         TM    ACXPST1,0                                                        
         BNO   TP5                                                              
         MVC   0(9,R2),1(R1)                                                    
         MVC   10(10,R2),10(R1)                                                 
         LA    R4,1(R4)                                                         
         LA    R2,L'UNSCANBK(R2)                                                
TP5      LA    R1,L'XPTAB1(R1)                                                  
         B     TP4                                                              
         SPACE 1                                                                
TPEND    LTR   R4,R4                                                            
         BZ    TPEND1                                                           
         GOTO1 VUNSCAN,DMCB,((R4),UNSCANBK),(C'C',WORK+20),C',=-='              
         SR    R4,R4               DO WE NEED MORE THAN ONE LINE                
         CLC   WORK+54(46),SPACES                                               
         BE    TPEND0                                                           
         GOTO1 VCHOPPER,DMCB,(80,WORK+20),(34,WRK),3                            
         L     R4,DMCB+8                                                        
         BCTR  R4,0                                                             
         MVC   WORK+20(3*34),WRK                                                
         SPACE 1                                                                
TPEND0   LA    R5,WORK+20          CONVERT HYPHEN TO COMMA                      
         LA    R6,1                                                             
         LA    R7,WORK+100                                                      
         CLI   0(R5),C'-'                                                       
         BNE   *+8                                                              
         MVI   0(R5),C','                                                       
         BXLE  R5,R6,*-12                                                       
         SPACE 1                                                                
TPEND1   LA    R3,34                                                            
         B     T608XIT                                                          
         SPACE 1                                                                
XPTAB    DS    0CL13               SEARCH VALUE(1),KEYWORD(9),VALUE(3)          
         DC    CL13'NSUM      NO'                                               
         DC    CL13'YPAY      NET' US ONLY                                      
         DC    CL13'NDETAIL   NO'                                               
         DC    CL13'YESTDETAILYES'                                              
         DC    CL13'NCD       NO'                                               
         DC    CL13'NESTIMATE NO'                                               
         DC    X'00'                                                            
*                                                                               
XPTAB1   DS    0CL20                                                            
         DC    X'80',CL9'ETA      ',CL10'NO'                                    
         DC    X'40',CL9'ECOMM    ',CL10'NO'                                    
         DC    X'04',CL9'EST      ',CL10'UNAPPRVD'                              
         DC    X'02',CL9'JOBS     ',CL10'UNAPPRVD'                              
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
WRKAREA  DSECT                                                                  
WRK      DS    CL160                                                            
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACINF08   01/24/97'                                      
         END                                                                    
