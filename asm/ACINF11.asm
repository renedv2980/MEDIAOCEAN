*          DATA SET ACINF11    AT LEVEL 008 AS OF 04/09/99                      
*PHASE T6050BA,*,NOAUTO                                                         
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE GE'                         
T60511   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE GENERAL  IN ACCOUNTS INFO PROGRAM         
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60511)                                               
         DC    A(FILTABLE-T60511)                                               
         DC    A(PREHEADS-T60511)                                               
         DC    A(PRETABLE-T60511)                                               
         DC    A(HEADINGS-T60511)                                               
         DC    A(DATTABLE-T60511)                                               
         DC    A(KNTRYPNT-T60511)                                               
         DC    A(FNTRYPNT-T60511)                                               
         DC    A(DNTRYPNT-T60511)                                               
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
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS+1)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS+2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'ACCOUNT'                                                    
         DC    CL2'AC'                                                          
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(0)              INDICATES OVERLAY CALL REQUIRED AT           
         DC    AL2(0)              VALIDATION                                   
         DC    AL1(L'ACGLACC)                                                   
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
         DC    CL10'DELETED'                                                    
         DC    CL2'DE'                                                          
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(ACSTATUS-ACKEYD)                                             
         DC    AL1(L'ACSTATUS)                                                  
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
         DC    CL8'DEPTS'                                                       
         DC    X'10'                                                            
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
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'STAFF'                                                       
         DC    X'80'                                                            
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
HEADINGS DC    CL39'ACCOUNT CODE ------------ACCOUNT NAME--'                    
         DC    CL39'                                       '                    
         DC    CL39'---------- FILT/STAT --GENERAL LEDGER--'                    
         DC    CL39'           123SAPCLD A/C CODE    SOURCE'                    
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
         DC    AL2(ANAM-GWS)       ACCOUNT NAME                                 
         DC    AL2(ACNMNAME-ACNAMED)                                            
         DC    AL1(L'ACNMNAME)                                                  
         DC    AL2(EDITNAME-GWS)                                                
         DC    AL1(15)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       FILTER 1                                     
         DC    AL2(ACSTFILT-ACSTATD)                                            
         DC    AL1(1)                                                           
         DC    AL2(EDITSTOP-GWS)                                                
         DC    AL1(52)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       FILTER 2                                     
         DC    AL2(20)                                                          
         DC    AL1(1)                                                           
         DC    AL2(EDITSTOP-GWS)                                                
         DC    AL1(53)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       ANALYSIS                                     
         DC    AL2(ACSTANAL-ACSTATD)                                            
         DC    AL1(L'ACSTANAL)                                                  
*&&UK*&& DC    AL2(EDITSTOP-GWS)                                                
*&&US*&& DC    X'FF00'                                                          
         DC    AL1(54)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       SUBCOMPANY                                   
         DC    AL2(ACSTSUB-ACSTATD)                                             
         DC    AL1(L'ACSTSUB)                                                   
         DC    AL2(EDITSTOP-GWS)                                                
         DC    AL1(55)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       COSTING GROUP                                
         DC    AL2(ACSTCOST-ACSTATD)                                            
         DC    AL1(L'ACSTCOST)                                                  
         DC    AL2(EDITSTOP-GWS)                                                
         DC    AL1(56)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       STATUS BYTE                                  
         DC    AL2(ACSTSTAT-ACSTATD)                                            
         DC    AL1(L'ACSTSTAT)                                                  
         DC    AL2(EDITSTAT-GWS)                                                
         DC    AL1(57)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AKEY-GWS)       GEN LEDGER A/C CODE AND SOURCE               
         DC    AL2(0)              (DUMMY ENTRY)                                
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
         DC    AL1(62)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
         EJECT                                                                  
KNTRYPNT DS    0D                  UNIT MUST BE 'S' OR 'A'                      
         NMOD1 0,**GENL,CLEAR=YES                                               
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60511,RB                                                        
         CLI   SCANBLCK,C'S'                                                    
         BE    TBXIT                                                            
         CLI   SCANBLCK,C'A'                                                    
         BE    TBXIT                                                            
         MVI   DMCB,1                                                           
         MVI   ERROR,UNITNVAL                                                   
         B     TBXIT                                                            
         EJECT                                                                  
FNTRYPNT DS    0D                  BUILD SPECIAL ACCOUNT FILTER TABLE           
         NMOD1 (LWSX-LWSD),**GENL**,CLEAR=YES                                   
         LR    R9,RC                                                            
         USING LWSD,R9                                                          
         LA    RF,ACFTAB                                                        
         AH    RF,=Y(L'ACFTAB)                                                  
         ST    RF,AGERULE                                                       
*                                                                               
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60511,RB                                                        
         LR    R5,R6                                                            
         USING FTBD,R5                                                          
         LA    R6,ACFTAB                                                        
         USING ACFTBD,R6                                                        
         TM    OPTIONS,ACFILTER                                                 
         BO    F02                                                              
         OI    OPTIONS,ACFILTER    FIRST ONE                                    
         MVI   0(R6),X'FF'                                                      
*                                                                               
F02      CLI   0(R6),X'FF'         FIND NEXT ENTRY SLOT                         
         BE    *+12                                                             
         LA    R6,ACFTBLEN(R6)                                                  
         B     F02                                                              
         L     RF,4(R1)            P2 = A(FILTER VALUE)                         
         MVC   ACFVAL,0(RF)                                                     
         MVC   ACFSIGN,FTBSIGN                                                  
         STC   R7,ACFEXLEN         R7 = LEN MINUS 1 OF FILTER VALUE             
         MVI   ACFTBLEN(R6),X'FF'                                               
         XIT1                                                                   
         DROP  R5,R6                                                            
         EJECT                                                                  
DNTRYPNT DS    0D                                                               
         NMOD1 (LWSX-LWSD),**GENL**                                             
         LR    R9,RC                                                            
         USING LWSD,R9                                                          
         LA    RF,ACFTAB                                                        
         AH    RF,=Y(L'ACFTAB)                                                  
         ST    RF,AGERULE                                                       
*                                                                               
         L     RC,0(R1)                                                         
         USING GWS,RC              RC = GLOBAL W/S                              
         USING T605TWA,RA                                                       
         L     RB,APHASE                                                        
         USING T60511,RB                                                        
         L     R8,ASTA                                                          
         USING ACSTATD,R8                                                       
         LA    R5,ACSTANAL                                                      
         CR    R2,R5                                                            
         BNE   TB0                                                              
         CLI   0(R2),C' '                                                       
         BH    TBXIT                                                            
         MVI   0(R2),C'.'                                                       
         B     TBXIT                                                            
         EJECT                                                                  
*              HANDLE GENERAL LEDGER POSTING INSTRUCTION ELEMENT                
         SPACE 1                                                                
TB0      DS    0H                                                               
         MVC   WORK(16),SPACES                                                  
         CLI   LEDGIND,0           HAS LEDGER RECORD BEEN READ ?                
         BNE   TB5                                                              
*                                                                               
*              READ LEDGER RECORD AND SET UP GEN LEDGER ELEMENTS IN             
*              GLOBAL IN ASCENDING SEQUENCE WITH ELEMENT LENGTH FIELD           
*              RESET TO SIGNIFICANT LENGTH OF ACGLSUB MINUS 1 .                 
*                                                                               
         MVI   LEDGIND,1                                                        
         MVC   KEYB,SPACES                                                      
         L     RE,ADRIO                                                         
         MVC   KEYB(3),0(RE)                                                    
         GOTO1 AREAD                                                            
         BE    TB11                                                             
         SR    R3,R3                                                            
         L     R4,ADRIOB                                                        
         USING ACKEYD,R4                                                        
         LA    R4,ACRECORD                                                      
         L     R8,AGERULE                                                       
         XC    0(3,R8),0(R8)       INITIALIZE CORE TABLE FOR HELLO              
*                                                                               
TB1      CLI   0(R4),0                                                          
         BE    TB3                                                              
         CLI   0(R4),X'15'         GEN LEDGER ELEMENT                           
         BNE   TB2                                                              
         MVC   WORK+50(26),SPACES  FIX ELEMENT LENGTH IN WORK+50                
         IC    R3,1(R4)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK+50(0),0(R4)                                                 
         MVI   WORK+51,26                                                       
         GOTO1 VHELLO,DMCB,(C'P',=C'CORETAB'),AGERULE,WORK+50                   
TB2      IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     TB1                                                              
*                                  RESET ACGLLEN TO SIGNIFICANT LENGTH          
*                                  OF ACGLSUB -1 (ZERO IF ALL SPACES)           
TB3      L     R8,AGERULE                                                       
         LA    R8,2(R8)                                                         
         USING ACGENLD,R8                                                       
         SR    R7,R7                                                            
         IC    R7,ACGLLEN          R7 = ELEMENT LENGTH                          
TB4      CLI   ACGLEL,0                                                         
         BE    TB5                                                              
         LA    R5,9                                                             
         LA    R6,ACGLSUB(R5)                                                   
         CLI   0(R6),C' '                                                       
         BNE   *+8                                                              
         BCT   R5,*-12                                                          
         STC   R5,ACGLLEN                                                       
         AR    R8,R7               BUMP TO NEXT                                 
         B     TB4                                                              
*                                                                               
TB5      L     R8,AGEN             A(A/C REC X'15' ELEMENT) OR ZERO             
         LTR   R8,R8                                                            
         BZ    TB6                                                              
         MVI   WORK+15,C'A'                                                     
         CLI   ACGLLEN,26                                                       
         BE    TB10                                                             
         MVC   ACGLACC+10(4),SPACES                                             
         B     TB10                                                             
*                                                                               
TB6      MVI   WORK+15,C'L'                                                     
         L     R3,ASAVEHIE         LOWEST LEVEL A/C FOR LEDGER ?                
         USING ACHEIRD,R3                                                       
         LA    R4,ACHRLEVB                                                      
         LA    R5,1                                                             
         LR    R6,R5                                                            
         LA    R7,3                                                             
TB6A     CLI   0(R4),0                                                          
         BE    TB6B                                                             
         LA    R4,L'ACHRDESA+1(R4)                                              
         BXLE  R5,R6,TB6A                                                       
TB6B     CH    R5,LEVEL                                                         
         BNE   TB8A                IF NOT SKIP OUT                              
         L     R2,ADRIO                                                         
         CLC   PRODUNIT(2),1(R2)   PRODUCTION UNIT/LEDGER?                      
         BNE   TB7                                                              
         SR    R4,R4               IF SO COMPARISON IS WITH JOB LEVEL           
         IC    R4,ACHRLEVB                                                      
         AR    R2,R4                                                            
         DROP  R3                                                               
TB7      L     RE,AGERULE                                                       
         LH    R4,0(RE)            R4 = TABLE SIZE                              
         SH    R4,=H'3'                                                         
         BNH   TB8A                                                             
         LA    R5,26               R5 = TABLE WIDTH                             
         LNR   R5,R5                                                            
         SR    R6,R6                                                            
TB8      BXH   R4,R5,TB9                                                        
         CLC   ACGLSUB,SPACES      = POST ALL A/C'S TO ACGLACC                  
         BE    TB10                                                             
TB8A     MVI   WORK+15,C' '                                                     
         B     TB11                                                             
COMPOST  CLC   ACGLSUB(0),3(R2)                                                 
*                                                                               
TB9      L     R8,AGERULE                                                       
         LA    R8,2(R8)                                                         
         AR    R8,R4                                                            
         IC    R6,ACGLLEN          SEEKING BEST A/C CODE MATCH                  
         EX    R6,COMPOST                                                       
         BNE   TB8                                                              
TB10     MVC   WORK(L'ACGLACC),ACGLACC                                          
*                                                                               
TB11     SR    R4,R4               APPLY ACCOUNT FILTERS IF ANY                 
         TM    OPTIONS,ACFILTER                                                 
         BZ    TB20                                                             
         LA    R6,ACFTAB                                                        
         USING ACFTBD,R6                                                        
*                                                                               
TB12     CLI   0(R6),X'FF'                                                      
         BE    TB20                                                             
         ZIC   R1,ACFEXLEN                                                      
         MVC   FULL,FBRANCH        IF NEG BRANCH OUT ON EQUAL                   
         CLI   ACFSIGN,C'N'                                                     
         BE    *+8                                                              
         MVI   FULL+1,X'70'        IF POS BRANCH OUT ON NOT EQUAL               
         EX    R1,FCOMPARE                                                      
         EX    R4,FULL                                                          
         LA    R6,ACFTBLEN(R6)                                                  
         B     TB12                                                             
*                                                                               
FCOMPARE CLC   WORK(0),ACFVAL                                                   
FBRANCH  BE    DONTWANT                                                         
DONTWANT BCTR  R4,0                R4 NEGATIVE MEANS DONT WANT RECORD           
*                                                                               
*                                                                               
TB20     LA    R2,WORK             RETURN ADDRESS & LENGTH IN R2 & R3           
         LA    R3,16                                                            
         MVI   DMCB,0              SET FOR NO ERROR                             
TBXIT    XIT1  REGS=(R2,R4)                                                     
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
LWSD     DSECT                                                                  
AGERULE  DS    A                                                                
ACFTAB   DS    XL2000              ACCOUNT FILTER TABLE                         
GERULE   DS    XL2000              GENERAL LEDGER RULES                         
LWSX     EQU   *                                                                
*                                                                               
ACFTBD   DSECT                     DSECT TO COVER ACCOUNT FILTER TABLE          
ACFVAL   DS    CL14                FILTER VALUE                                 
ACFEXLEN DS    CL1                 LENGTH MINUS 1                               
ACFSIGN  DS    CL1                 P=POSITIVE, N=NEGATIVE                       
ACFTBLEN EQU   *-ACFTBD                                                         
*                                                                               
ACFILTER EQU   X'80'               ACCOUNT FILTER IN USE (OPTIONS VAL)          
*                                                                               
*              NESTED INCLUDES FOR ACINFDSECT                                   
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACINF11   04/09/99'                                      
         END                                                                    
